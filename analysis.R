# loading, setting up

set.seed(0503)

doParallel::registerDoParallel()

library(tidymodels) # doesn't load forcats, stringr, readr from tidyverse
library(readr)
library(vip)
library(janitor)

d <- read_csv("data-to-model.csv")

# PREDICTORS
# surveys
# discussion
# assignments

# DEMO/CONTEXT
# course
# gender
# reason
# subject
# semester
# section

# OUTCOME
# time spent
# final_grade
# passing_grade

# data splitting

train_test_split <- initial_split(d_ss) # strata = ""

data_train <- training(train_test_split)
data_test <- testing(train_test_split)

# pre-procesing/feature engineering

sci_rec <- recipe(final_grade ~ ., data = d) %>% 
    add_role(student_id, course_id, new_role = "ID variable") %>% # this can be any string
    step_nzv(all_predictors())
    #step_center(all_numeric_predictors()) %>%
    #step_scale(all_numeric_predictors()) %>% # SD = 1
    #step_dummy(all_nominal_predictors()) %>% 
    #step_impute_knn(all_predictors(), all_outcomes()) # may want to not include this

sci_rec <- sci_rec %>% 
    prep() # trains/estimates parameters

sci_rec %>% 
    summary()

data_train_baked <- bake(sci_rec, data_train)
data_test_baked <- bake(sci_rec, data_test)

# modeling with parsnip - one

rf_mod_one <-
    rand_forest() %>%
    set_engine("ranger") %>%
    set_mode("regression") # or "classification"

rf_wf_one <-
    workflow() %>%
    add_model(rf_mod) %>% 
    add_recipe(sci_rec)

fit_one <- fit(rf_wf_one, data = data_train_baked)
test_preds <- predict(fit_one, new_data = data_test_prepped)
preds <- bind_cols(train = data_test_prepped$final_grade, test = test_preds$.pred)
metrics(preds, truth = train, estimate = test)

# modeling with parsnip - many

rf_mod <- 
    rand_forest(trees = tune(), mtry = tune(), min_n = tune()) %>% 
    set_engine("ranger", importance = "impurity") %>% 
    set_mode("regression") # or "classification"

rf_wf <- 
    workflow() %>%
    add_model(rf_mod) %>%
    add_recipe(sci_rec)

folds <- vfold_cv(data_train_baked, v = 10)

# folds_bs <- rsample::bootstraps(data_train_prepped, times = 10)

my_metrics <- metric_set(rmse, mae, rsq)

tree_res_auto <- rf_wf %>% 
    tune_grid(
        resamples = folds,
        metrics = my_metrics
    )

collect_metrics(tree_res_auto)

show_best(tree_res_auto, n = 10)

rf_grid <- grid_regular(
    mtry(range = c(15, 50)),
    min_n(range = c(3, 20)),
    trees(range = c(100, 1500)),
    levels = 3
)

tree_res <- rf_wf %>% 
    tune_grid(
        resamples = folds, 
        grid = rf_grid,
        metrics = my_metrics
    )

tree_res %>%
    collect_metrics()

tune::show_best(tree_res,
                metric = "rmse")

best_tree <- tree_res %>%
    select_best("rmse")

final_wf <- 
    rf_wf %>% 
    finalize_workflow(best_tree)

final_tree <- 
    final_wf %>%
    fit(data = data_train_baked)

final_tree %>% 
    pull_workflow_fit() %>% 
    vip()

final_wf %>% 
    last_fit(train_test_split)
