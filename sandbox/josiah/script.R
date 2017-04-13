library(dplyr)
DIR <- '/Users/josiahdavis/Documents/Berkeley/PH252D/data/' # <-- UPDATE AS NEEDED
d <- read.csv(paste0(DIR, 'HR_comma_sep_2.csv'))

# Number of people --------------------------------------
n_tot <- d %>% 
  filter(salary != 'medium') %>% 
  summarize(people = n())
n_tot

# Number of people in each treatment --------------------------------------
n_treat <- d %>% 
  filter(salary != 'medium') %>% 
  group_by(salary) %>% 
  summarize(people = n())
n_treat

# Number of people in each treatment and strata ----------------------------
n_strata <- d %>% 
  filter(salary != 'medium') %>%
  mutate(
    # Create binary variables based on median value of continuous variables
    time_spend_company_b = time_spend_company > 3
    , number_project_b = number_project > 4
    , average_montly_hours_b = average_montly_hours > 200
  ) %>% 
  group_by(
    promotion_last_5years
    , number_project_b
    , Work_accident
    , time_spend_company_b
    , average_montly_hours_b
    , sales
  ) %>%
  summarize(
    sal_h = sum(salary == 'high'), 
    sal_l = sum(salary == 'low'), 
    class_size = n()
  )
n_strata


# Number of people with a positivity violation ------------------------------
n_pos_viol <- n_strata %>% 
  ungroup() %>% 
  filter(sal_h == 0 | sal_l == 0) %>% 
  summarize(
    tot = sum(sal_h) + sum(sal_l),
    tot_pct = (sum(sal_h) + sum(sal_l)) / n_tot[1,1]
  )
n_pos_viol

# Evaluating the Positivity Assumption ------------------------------------

# Question: How many people drop out with different cuts? 


# Which libraries to include in Super Learner? ----------------------------


