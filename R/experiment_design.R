################################################################################
# Setup -----------------------------------------------------------------------#
################################################################################

library(tidyverse)
library(infer)
library(cowplot)
library(broom)

# Read in data ----------------------------------------------------------------#
# Baseline
set.seed(1)
baseline_data <- data.table::fread("data/baseline/baseline_website_data.csv") %>% 
  mutate(variant = 'baseline',
         interaction = as.character(interaction))
baseline_sample_data <- sample_n(baseline_data, size = 5474, replace = FALSE)

# Get experiment 1 data
set.seed(2)
experiment_population_data <- data.table::fread("data/scenario_1/experiment_1_data.csv") %>% 
  mutate(variant = 'variant_1',
         interaction = as.character(interaction))
experiment_1_sample_data <- sample_n(experiment_population_data, size = 5474, replace = FALSE)
  

# Get experiment 2 data
set.seed(4)
experiment_population_data <- data.table::fread("data/scenario_2/experiment_2_data.csv") %>% 
  mutate(variant = 'variant_2',
         interaction = as.character(interaction))
experiment_2_sample_data <- sample_n(experiment_population_data, size = 5474, replace = FALSE)

# Experiment dataset
experiment_sample <- rbind(baseline_sample_data, experiment_1_sample_data, experiment_2_sample_data)

################################################################################
# QAQC ------------------------------------------------------------------------#
################################################################################

daily_agg <- experiment_sample %>% 
  ungroup(.) %>% 
  mutate(interaction = as.numeric(interaction)) %>% 
  group_by(variant, dow) %>% 
  summarise(visits = n(),
            CTR = mean(interaction),
            n_purchases_day = sum(purchase > 0),
            total_earned = sum(purchase),
            earned_per_visit = mean(purchase)) %>% 
  mutate(dow = as.numeric(factor(dow, levels=c('Monday', 'Tuesday', 'Wednesday', 'Thursday',
                                               'Friday', 'Saturday', 'Sunday')))) %>% 
  arrange(variant, dow) %>% 
  mutate(rolling_purchases = cumsum(n_purchases_day),
         rolling_earned = cumsum(total_earned))

# CTR
png("figures/daily_ctr.png", type='cairo', width = 1600, height = 1200, res = 300)
daily_agg %>% 
  ggplot() +
  geom_bar(aes(x = dow, y = CTR, fill=variant), stat = 'identity', position='dodge') +
  labs(x = "Day of Week",
       y = "Click-Through-Rate",
       fill = 'Website Variant',
       title = "Experiment Click-Through-Rate by Day")
dev.off()

# n_purchases
png("figures/rolling_purchases.png", type='cairo', width = 1600, height = 1200, res = 300)
daily_agg %>% 
  ggplot() +
  geom_point(aes(x = dow, y = rolling_purchases, color=variant)) +
  geom_line(aes(x = dow, y = rolling_purchases, color=variant)) +
  labs(x = "Day of Week",
       y = "Rolling Number of Purchases",
       color = 'Website Variant',
       title = "Experiment Purchases Over Experiment")
dev.off()

# n_earned
png("figures/rolling_earnings.png", type='cairo', width = 1600, height = 1200, res = 300)
daily_agg %>% 
  ggplot() +
  geom_point(aes(x = dow, y = rolling_earned, color=variant)) +
  geom_line(aes(x = dow, y = rolling_earned, color=variant)) +
  labs(x = "Day of Week",
       y = "Rolling Earnings",
       color = 'Website Variant',
       title = "Experiment Earnings Over Experiment")
dev.off()

################################################################################
# Power Analysis --------------------------------------------------------------#
################################################################################

# Set parameters
expected_lift <- 0.02
alpha <- 0.05
beta <- 0.8
needed_sample_size <- 5474 # https://www.stat.ubc.ca/~rollin/stats/ssize/b2.html

h_0_clt <- 0.22
h_a_clt <- 0.24

# Experiment Results ----------------------------------------------------------#
experiment_est <- experiment_sample %>%
  group_by(variant) %>%
  summarize(
    click_rate = sum(as.numeric(interaction)) / n(),
    n = n()
  ) %>%
  mutate(se = sqrt(click_rate * (1 - click_rate) / n))
experiment_est

# 95% CI Baseline -------------------------------------------------------------#
# Use bootstrapping of sample to establish 95% confidence interval of proportion
# Baseline
set.seed(42) # For reproducibility.
baseline_ci <- experiment_sample %>%
  filter(variant == "baseline") %>%
  specify(response = interaction, success = "1") %>%
  generate(reps = 1000, type = "bootstrap") %>%
  calculate(stat = "prop") %>%
  get_ci()
baseline_ci$variant <- "baseline"

# Variant 1
set.seed(42) # For reproducibility.
var1_ci <- experiment_sample %>%
  filter(variant == "variant_1") %>%
  specify(response = interaction, success = "1") %>%
  generate(reps = 1000, type = "bootstrap") %>%
  calculate(stat = "prop") %>%
  get_ci()
var1_ci$variant <- "variant_1"

# Variant 2
set.seed(42) # For reproducibility.
var2_ci <- experiment_sample %>%
  filter(variant == "variant_2") %>%
  specify(response = interaction, success = "1") %>%
  generate(reps = 1000, type = "bootstrap") %>%
  calculate(stat = "prop") %>%
  get_ci()
var2_ci$variant <- "variant_2"

# Combine
cis <- bind_rows(baseline_ci, var1_ci, var2_ci)
cis

click_through_est <- left_join(experiment_est, cis, by='variant')
colnames(click_through_est) <- c("variant", "click_rate", "treatment_size", "se", "lower_ci", "upper_ci")
click_through_est

data.table::fwrite(click_through_est, "data/tables/experiment_summary.csv")

# Plot
options(repr.plot.width = 20, repr.plot.height = 8)

png("figures/proportion_ci_plot.png", type='cairo', width = 1600, height = 1200, res = 300)
CTR_plot <- ggplot(click_through_est, aes(x = variant, y = click_rate)) +
  geom_point(size = 4) +
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), width = 0.1) +
  xlab("Webpage Variant") +
  ylab("Click-Through Rate") +
  theme(text = element_text(size = 20)) +
  ylim(c(0.1, 0.4)) +
  scale_y_continuous(breaks = seq(0, 0.4, by = 0.01)) +
  theme(
    text = element_text(size = 30),
    plot.title = element_text(face = "bold"),
    axis.title = element_text(face = "bold"),
    legend.title = element_text(face = "bold"),
    panel.grid = element_line(color = "#8ccde3", size = 0.75, linetype = 2)
  )
CTR_plot
dev.off()

# Save table
data.table::fwrite(click_through_est, "data/tables/clt_ci_tbl.csv")

################################################################################
# NULL Analysis ---------------------------------------------------------------#
################################################################################

# Experiment 1
set.seed(42) # For reproducibility.
null_distribution_variant_1 <- experiment_sample %>%
  filter(variant != 'variant_2') %>% 
  specify(formula = interaction ~ variant, success = "1") %>%
  hypothesize(null = "independence") %>%
  generate(reps = 1000, type = "permute") %>%
  calculate(stat = "diff in props", order = c('baseline', 'variant_1'))
alpha_threshold_var1 <- quantile(null_distribution_variant_1$stat, 0.95)

# Experiment 2
set.seed(42) # For reproducibility.
null_distribution_variant_2 <- experiment_sample %>%
  filter(variant != 'variant_1') %>% 
  specify(formula = interaction ~ variant, success = "1") %>%
  hypothesize(null = "independence") %>%
  generate(reps = 1000, type = "permute") %>%
  calculate(stat = "diff in props", order = c('baseline', 'variant_2'))
alpha_threshold_var2 <- quantile(null_distribution_variant_2$stat, 0.95)

# Plot
options(repr.plot.width = 8, repr.plot.height = 5) 
h0_dist_var_1 <- null_distribution_variant_1 %>% 
  visualize() +
  theme(text = element_text(size=20)) +
  theme(
    text = element_text(size = 20),
    plot.title = element_text(face = "bold"),
    axis.title = element_text(face = "bold"),
    legend.title = element_text(face = "bold"),
  ) + geom_vline(
    xintercept = alpha_threshold_var1,
    color = "blue", lty = 3, size = 1.5
  ) +
  geom_vline(xintercept = expected_lift, color = "red", size = 1.5)

h0_dist_var_2 <- null_distribution_variant_2 %>% 
  visualize() +
  theme(text = element_text(size=20)) +
  theme(
    text = element_text(size = 20),
    plot.title = element_text(face = "bold"),
    axis.title = element_text(face = "bold"),
    legend.title = element_text(face = "bold"),
  ) + geom_vline(
    xintercept = alpha_threshold_var2,
    color = "blue", lty = 3, size = 1.5
  ) +
  geom_vline(xintercept = expected_lift, color = "red", size = 1.5)

cowplot::plot_grid(h0_dist_var_1, h0_dist_var_2)

# P-Value ---------------------------------------------------------------------#
null_distribution_variant_1 %>%
  get_pvalue(obs_stat = expected_lift, direction = "greater")

null_distribution_variant_2 %>%
  get_pvalue(obs_stat = expected_lift, direction = "greater")

################################################################################
# Experiment 1 Analysis -------------------------------------------------------#
################################################################################

p_baseline_hat <- experiment_sample %>% 
  filter(variant == "baseline") %>% 
  summarize(click_rate = sum(as.numeric(interaction)) / n()) %>% 
  pull()
n_baseline <- experiment_sample %>%
  filter(variant == "baseline") %>%
  nrow()

p_var1_hat <- experiment_sample %>% 
  filter(variant == "variant_1") %>% 
  summarize(click_rate = sum(as.numeric(interaction)) / n()) %>% 
  pull()
n_var1 <- experiment_sample %>%
  filter(variant == "variant_1") %>%
  nrow()

p_hat <- experiment_sample %>%
  filter(variant != 'variant_1') %>% 
  summarise(phat = sum(as.numeric(interaction)) / n()) %>%
  pull()

Z <- (p_var1_hat - p_baseline_hat) / (sqrt(p_hat * (1 - p_hat) * (1 / n_baseline + 1 / n_var1)))

# Simulate Null distribution
Z_norm <- ggplot(data = data.frame(x = c(-4, 4)), aes(x)) +
  stat_function(fun = dnorm, n = 101, args = list(mean = 0, sd = 1)) +
  ylab("") +
  xlab("") +
  theme(text = element_text(size = 20)) +
  geom_vline(xintercept = 0, colour = "red", size = 1.5) +
  labs(x = "", y = "Density") +
  ggtitle("Null Distribution (Standard Normal)")

# Add significance boundary
Z_norm <- Z_norm +
  geom_vline(
    xintercept = c(qnorm(0.95)),
    color = "blue", lty = 2, size = 1.5
  )

# Add our Z
Z_norm <- Z_norm +
  geom_vline(xintercept = Z, color = "darkgreen", size = 1.5)

manual_colors <- c("Z" = "red", "Zstar" = "blue", "Zhat" = "darkgreen")

Z_norm + guides(color = guide_legend(title = "Z", override.aes = list(color = manual_colors)))

png("figures/exp1_znorm.png", type='cairo', width = 1600, height = 1200, res = 300)
Z_norm
dev.off()

p_value <- pnorm(Z, lower.tail = FALSE)
p_value

# With broom
click_summary <- experiment_sample %>% 
  filter(variant != 'variant_2') %>%
  group_by(variant) %>%
  summarize(
    success = sum(as.numeric(interaction)),
    n = n()
  )
click_summary

exp1_prop_test <- prop.test(click_summary$success, 
                            n=click_summary$n, 
                            correct = FALSE, 
                            alternative = 'less')

saveRDS(exp1_prop_test, "data/tables/exp1_prop_test.rds")


# FAIL TO REJECT


################################################################################
# Experiment 2 Analysis -------------------------------------------------------#
################################################################################

p_baseline_hat <- experiment_sample %>% 
  filter(variant == "baseline") %>% 
  summarize(click_rate = sum(as.numeric(interaction)) / n()) %>% 
  pull()
n_baseline <- experiment_sample %>%
  filter(variant == "baseline") %>%
  nrow()

p_var2_hat <- experiment_sample %>% 
  filter(variant == "variant_2") %>% 
  summarize(click_rate = sum(as.numeric(interaction)) / n()) %>% 
  pull()
n_var2 <- experiment_sample %>%
  filter(variant == "variant_2") %>%
  nrow()

p_hat <- experiment_sample %>%
  filter(variant != 'variant_1') %>% 
  summarise(phat = sum(as.numeric(interaction)) / n()) %>%
  pull()

Z <- (p_var2_hat - p_baseline_hat) / (sqrt(p_hat * (1 - p_hat) * (1 / n_baseline + 1 / n_var2)))

# Simulate Null distribution
Z_norm <- ggplot(data = data.frame(x = c(-4, 4)), aes(x)) +
  stat_function(fun = dnorm, n = 101, args = list(mean = 0, sd = 1)) +
  ylab("") +
  xlab("") +
  theme(text = element_text(size = 20)) +
  geom_vline(xintercept = 0, colour = "red", size = 1.5) +
  labs(x = "Quantile", y = "Density") +
  ggtitle("Null Distribution (Standard Normal)")

# Add significance boundary
Z_norm <- Z_norm +
  geom_vline(
    xintercept = c(qnorm(0.95)),
    color = "blue", lty = 2, size = 1.5
  )

# Add our Z
Z_norm <- Z_norm +
  geom_vline(xintercept = Z, color = "darkgreen", size = 1.5)

png("figures/exp2_znorm.png", type='cairo', width = 1600, height = 1200, res = 300)
Z_norm
dev.off()

p_value <- pnorm(Z, lower.tail = FALSE)
p_value

# With broom
click_summary <- experiment_sample %>% 
  filter(variant != 'variant_1') %>%
  group_by(variant) %>%
  summarize(
    success = sum(as.numeric(interaction)),
    n = n()
  )
click_summary

exp2_prop_test <- prop.test(click_summary$success, 
                            n=click_summary$n, 
                            correct = FALSE, 
                            alternative = 'less')
saveRDS(exp2_prop_test, "data/tables/exp2_prop_test.rds")


# REJECT the NULL


################################################################################
# Cost Analysis : CI ----------------------------------------------------------#
################################################################################

# Set parameters
alpha <- 0.05
beta <- 0.8
needed_sample_size <- 5474 # https://www.stat.ubc.ca/~rollin/stats/ssize/b2.html

h_0_diff <- 0

# Experiment Results ----------------------------------------------------------#
experiment_te <- experiment_sample %>%
  group_by(variant) %>%
  summarize(
    earned_per_visit = mean(as.numeric(purchase)),
    se_earned_per_visit = sd(as.numeric(purchase)) / sqrt(n()),
    n_purchases = sum(purchase > 0)
  ) %>% 
  mutate(lower_ci = earned_per_visit - (1.96 * se_earned_per_visit),
         upper_ci = earned_per_visit + (1.96 * se_earned_per_visit))
experiment_te

png("figures/earned_rate_ci_plot.png", type='cairo', width = 1600, height = 1200, res = 300)
te_plot <- ggplot(experiment_te, aes(x = variant, y = earned_per_visit)) +
  geom_point(size = 4) +
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), width = 0.1) +
  xlab("Webpage Variant") +
  ylab("$ Earned Per Visit") +
  theme(text = element_text(size = 12)) +
  #ylim(c(0.1, 0.4)) +
  #scale_y_continuous(breaks = seq(0, 0.4, by = 0.01)) +
  theme(
    text = element_text(size = 15),
    plot.title = element_text(face = "bold"),
    axis.title = element_text(face = "bold"),
    legend.title = element_text(face = "bold"),
    panel.grid = element_line(color = "#8ccde3", size = 0.75, linetype = 2)
  )
te_plot
dev.off()
