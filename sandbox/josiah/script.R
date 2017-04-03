library(dplyr)
DIR <- '/Users/josiahdavis/Documents/Berkeley/PH252D/data/'
d <- read.csv(paste0(DIR, 'HR_comma_sep_2.csv'))
head(d)
summary(d)

# Target Causal Parameter: Does making more money make you less likely to leave your job?
# Do people value money more than they think they do? Not sure how I would answer this yet.
# e.g., satisfaction level is what they tell themselves. leaving is what they do.


# A: salary
# Y: left

# W
# W1: satisfaction_level
# W2: last_evaluation
# W3: number_project
# W4: average_monthly_hours
# W5: time_spend_company
# W6: Work_accident
# W7: promotion_last_5years
# W8: promotion_last_5years

library(dplyr)
DIR <- '/Users/josiahdavis/Documents/Berkeley/PH252D/data/'
# d <- read.csv(paste0(DIR, 'HR_comma_sep_2.csv'))
# head(d)
# summary(d)

