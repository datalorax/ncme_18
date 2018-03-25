library(tidyverse)
library(glue)

download_file <- function(file, 
  base_url = "http://www.oregon.gov/ode/educator-resources/assessment/TestResults2017/") {
  rio::import(glue("{base_url}{file}"), setclass = "tbl_df", na = c("-", "--", "*"))
}

d <- download_file("pagr_schools_ela_all_1617.xlsx") %>% 
  janitor::clean_names() %>% 
  sundry::rm_empty_rows(number_proficient_level_3_or_4:participation_rate) 

mth <- download_file("pagr_schools_math_all_1617.xlsx")

d <- d %>% 
  select(-number_proficient_level_3_or_4, -percent_proficient_level_3_or_4, -grade_level, -subject) %>% 
  rename(n_l4   = number_level_4,
         pct_l4 = percent_level_4,
         n_l3   = number_level_3,
         pct_l3 = percent_level_3,
         n_l2   = number_level_2,
         pct_l2 = percent_level_2,
         n_l1   = number_level_1,
         pct_l1 = percent_level_1) %>% 
  gather(var, val, n_l4:pct_l1) %>% 
  separate(var, c("stat", "level")) %>% 
  spread(stat, val, convert = TRUE)
d %>% 
  count(student_group, sort = TRUE)
d %>% 
  group_by(school_id, student_group) %>% 
  summarize(tot = sum(n)) %>% 
  group_by(student_group) %>% 
  summarize(min = min(tot, na.rm = TRUE))
  
data <- d %>% 
  filter(student_group == "White" |
         student_group == "Hispanic/Latino" |
         student_group == "Multi-Racial" |
         student_group == "Asian")

bw <- d %>% 
  filter(student_group == "Black/African American" |
         student_group == "White")

bw <- bw %>% 
  group_by(school) %>% 
  count(student_group) %>% 
  ungroup() %>% 
  count(school) %>% 
  filter(n > 1) %>% 
  semi_join(bw, ., by = "school")

bw <- bw %>% 
  group_by(school_id) %>% 
  mutate(level = parse_number(level)) %>% 
  arrange(school_id, level) %>% 
  group_by(school_id, student_group) %>% 
  mutate(cum_pct = cumsum(pct)) %>% 
  ungroup()

