################################################################################
# Setup -----------------------------------------------------------------------#
################################################################################

library(tidyverse)

################################################################################
# Baseline --------------------------------------------------------------------#
################################################################################

# Set parameters --------------------------------------------------------------#

baseline_user_n <- 50000
baseline_visits_n <- 100000
baseline_ctr <- 0.22
baseline_br <- 0.13

# Create User and Visit IDs ---------------------------------------------------#
user_ids <- paste0("u", str_pad(string = seq(1, baseline_user_n), 
                                width = 6, 
                                side = 'left', 
                                pad = '0'))
visit_ids <- paste0("v", str_pad(string = seq(1, baseline_visits_n), 
                                 width = 6, 
                                 side = 'left', 
                                 pad = '0'))

# Simulate days of visit ------------------------------------------------------#
set.seed(1)
simulated_baseline_visit_ids <- sample(user_ids, baseline_visits_n, TRUE)

# Simulate Days of Week -------------------------------------------------------#
days <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")
day_visit_counts <- c(1537, 1721, 1692, 1556, 1378, 1952, 2377)
day_prop <- day_visit_counts / sum(day_visit_counts)
set.seed(1)
simulated_visit_days <- sample(days, baseline_visits_n, replace = TRUE, day_prop)

# Simulate Click Interaction --------------------------------------------------#
click_levels <- c(1, 0)
click_prob <- c(0.22, 0.78)
set.seed(2)
simulated_clicks <- sample(click_levels, baseline_visits_n, replace = TRUE, click_prob)
num_clicks <- sum(simulated_clicks)

# Simulate purchases ------------------------------------------------------------#
purchase_levels <- c(0, 35, 75, 150)
purchase_probs <- c(0.41, 0.3, 0.19, 0.1)

set.seed(3)
visit_click_ids <- visit_ids[as.logical(simulated_clicks)]
visit_purchases <- sample(purchase_levels, sum(num_clicks), replace = TRUE, prob=purchase_probs)
purchase_df <- data.frame(visit_id=visit_click_ids, 
                          purchase=visit_purchases)

# Create Dataframe ------------------------------------------------------------#
simulated_data <- data.frame(visit_id = visit_ids,
                             user_id = simulated_baseline_visit_ids,
                             dow = simulated_visit_days,
                             interaction = simulated_clicks) %>% 
  left_join(., purchase_df, by='visit_id') %>% 
  mutate(purchase = ifelse(is.na(purchase), 0, purchase))
table(simulated_data$purchase) / 100000

# Save ------------------------------------------------------------------------#

data.table::fwrite(simulated_data, "data/baseline/baseline_website_data.csv")

################################################################################
# Scenario 1 ------------------------------------------------------------------#
################################################################################

# Set parameters --------------------------------------------------------------#

baseline_user_n <- 50000
baseline_visits_n <- 100000
baseline_ctr <- 0.23
baseline_br <- 0.13

# Create User and Visit IDs ---------------------------------------------------#
user_ids <- paste0("u", str_pad(string = seq(1, baseline_user_n), 
                                width = 6, 
                                side = 'left', 
                                pad = '0'))
visit_ids <- paste0("v", str_pad(string = seq(1, baseline_visits_n), 
                                 width = 6, 
                                 side = 'left', 
                                 pad = '0'))

# Simulate days of visit ------------------------------------------------------#
set.seed(4)
simulated_baseline_visit_ids <- sample(user_ids, baseline_visits_n, TRUE)

# Simulate Days of Week -------------------------------------------------------#
days <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")
day_visit_counts <- c(1537, 1721, 1692, 1556, 1378, 1952, 2377)
day_prop <- day_visit_counts / sum(day_visit_counts)
set.seed(5)
simulated_visit_days <- sample(days, baseline_visits_n, replace = TRUE, day_prop)

# Simulate Click Interaction --------------------------------------------------#
click_levels <- c(1, 0)
click_prob <- c(0.23, 0.77)
set.seed(6)
simulated_clicks <- sample(click_levels, baseline_visits_n, replace = TRUE, click_prob)
num_clicks <- sum(simulated_clicks)

# Simulate purchases ------------------------------------------------------------#
purchase_levels <- c(0, 35, 75, 150)
purchase_probs <- c(0.41, 0.3, 0.19, 0.1)

set.seed(7)
visit_click_ids <- visit_ids[as.logical(simulated_clicks)]
visit_purchases <- sample(purchase_levels, sum(num_clicks), replace = TRUE, prob=purchase_probs)
purchase_df <- data.frame(visit_id=visit_click_ids, 
                          purchase=visit_purchases)

# Create Dataframe ------------------------------------------------------------#
simulated_data <- data.frame(visit_id = visit_ids,
                             user_id = simulated_baseline_visit_ids,
                             dow = simulated_visit_days,
                             interaction = simulated_clicks) %>% 
  left_join(., purchase_df, by='visit_id') %>% 
  mutate(purchase = ifelse(is.na(purchase), 0, purchase))
table(simulated_data$purchase) / 100000

# Save ------------------------------------------------------------------------#

data.table::fwrite(simulated_data, "data/scenario_1/experiment_1_data.csv")

################################################################################
# Scenario 2 ------------------------------------------------------------------#
################################################################################

# Set parameters --------------------------------------------------------------#

baseline_user_n <- 50000
baseline_visits_n <- 100000
baseline_ctr <- 0.26
baseline_br <- 0.13

# Create User and Visit IDs ---------------------------------------------------#
user_ids <- paste0("u", str_pad(string = seq(1, baseline_user_n), 
                                width = 6, 
                                side = 'left', 
                                pad = '0'))
visit_ids <- paste0("v", str_pad(string = seq(1, baseline_visits_n), 
                                 width = 6, 
                                 side = 'left', 
                                 pad = '0'))

# Simulate days of visit ------------------------------------------------------#
set.seed(8)
simulated_baseline_visit_ids <- sample(user_ids, baseline_visits_n, TRUE)

# Simulate Days of Week -------------------------------------------------------#
days <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")
day_visit_counts <- c(1537, 1721, 1692, 1556, 1378, 1952, 2377)
day_prop <- day_visit_counts / sum(day_visit_counts)
set.seed(9)
simulated_visit_days <- sample(days, baseline_visits_n, replace = TRUE, day_prop)

# Simulate Click Interaction --------------------------------------------------#
click_levels <- c(1, 0)
click_prob <- c(0.25, 0.75)
set.seed(10)
simulated_clicks <- sample(click_levels, baseline_visits_n, replace = TRUE, click_prob)
num_clicks <- sum(simulated_clicks)

# Simulate purchases ------------------------------------------------------------#
purchase_levels <- c(0, 35, 75, 150)
purchase_probs <- c(0.41, 0.3, 0.19, 0.1)

set.seed(11)
visit_click_ids <- visit_ids[as.logical(simulated_clicks)]
visit_purchases <- sample(purchase_levels, sum(num_clicks), replace = TRUE, prob=purchase_probs)
purchase_df <- data.frame(visit_id=visit_click_ids, 
                          purchase=visit_purchases)

# Create Dataframe ------------------------------------------------------------#
simulated_data <- data.frame(visit_id = visit_ids,
                             user_id = simulated_baseline_visit_ids,
                             dow = simulated_visit_days,
                             interaction = simulated_clicks) %>% 
  left_join(., purchase_df, by='visit_id') %>% 
  mutate(purchase = ifelse(is.na(purchase), 0, purchase))
table(simulated_data$purchase) / 100000

# Save ------------------------------------------------------------------------#

data.table::fwrite(simulated_data, "data/scenario_2/experiment_2_data.csv")




