library(tidyverse)

gb <- read_csv("gradebook.csv", col_types = cols(last_access_data = col_date())) %>% clean_names()
s <- read_csv("survey.csv") %>% clean_names()
t <- read_csv("trace.csv") %>% clean_names()
disc_final <- read_csv("disc-final.csv") # this is processed because it is not easy to anonymize the text

gb %>% 
    count(student_id) 

gb %>% 
    count(student_id) %>% 
    summarize(mean_n = mean(n))

s %>% 
    count(student_id)

t %>% 
    count(student_id)

s_ss <- s %>% 
    select(student_id:tv) %>% 
    mutate(student_id = as.double(student_id))

t

gb_ss <- gb %>%
    filter(gradebook_type != "T") %>% 
    group_by(student_id, course_id) %>%
    arrange(student_id, item_position) %>% 
    slice(1:20) # first 20 assignments

gb_final <- gb_ss %>% 
    mutate(points_earned = as.double(points_earned)) %>% 
    summarize(total_points_possible = sum(points_possible, na.rm = T),
              total_points_earned = sum(points_earned, na.rm = T)) %>% 
    mutate(percentage_earned = total_points_earned / total_points_possible)

clean_text <- function(htmlString) {
    return(gsub("<.*?>", "", htmlString))
}

find_length <- function(x) {
    stringr::str_split(x, " ") %>% 
        pluck(1) %>% 
        length()
}

disc_ss <- disc %>% 
    mutate(thread_title = stringr::str_replace(thread_title, "10-4", "1-4")) %>% 
    mutate(thread_title = stringr::str_replace(thread_title, "Welcome", "0-0-Welcome")) %>% 
    rename(student_id = user_pk,
           course_id = section) %>% 
    select(student_id, course_id, thread_title, text) %>% 
    mutate(text = clean_text(text)) %>% 
    mutate(n_words = map_dbl(text, find_length))

course_key <- disc_ss %>% 
    mutate(course_subset = stringr::str_sub(course_id, end = 10)) %>% 
    count(course_subset, thread_title) %>% 
    group_by(course_subset) %>% 
    slice(1:3) %>% # first three discussions
    ungroup()

disc_final <- disc_ss %>% 
    semi_join(course_key) %>% 
    group_by(student_id, course_id) %>% 
    summarize(sum_discussion_posts = n(),
              sum_n_words = sum(n_words))

d <- t %>% 
    left_join(s_ss) %>% 
    left_join(gb_final) %>% 
    left_join(disc_final)

d <- d %>% 
    mutate(passing_grade = if_else(final_grade >= .70,
                                   1,
                                   0))
d %>% 
    visdat::vis_dat()

d_ss <- d %>% filter(!is.na(tv))

write_csv(d_ss, "data-to-model.csv")