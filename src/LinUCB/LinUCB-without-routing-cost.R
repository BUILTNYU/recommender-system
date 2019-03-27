library(dplyr)
library(broom)
library(MASS)
library(ggplot2)
library(purrr)
library(tidyr)
library(knitr)

n <- 10000

set.seed(7)

#simulated dataset

bandit_data <- data_frame(
  visited_american = sample(c(0,1), n, prob = c(0.6, 0.4), replace = T),
  visited_chinese = sample(c(0,1), n, prob = c(0.7, 0.3), replace = T),
  
  arm = sample(c(1:3), n, replace =  T),
  
  american_coef = case_when(arm == 1 ~ .5,
                            arm == 2 ~ .1,
                            arm == 3 ~ .1),
  chinese_coef = case_when(arm == 1 ~ .1,
                           arm == 2 ~ .1,
                           arm == 3 ~ .4),
  arm_baseline = case_when(arm == 1 ~ .1,
                           arm == 2 ~ .2,
                           arm == 3 ~ .1),
  rand_draw = runif(n)
) %>%
  mutate(visit_factor = arm_baseline + american_coef * visited_american + chinese_coef * visited_chinese) %>%
  mutate(visit = ifelse(visit_factor >= rand_draw, 1, 0))

bandit_data %>%
  group_by(arm, visited_american, visited_chinese) %>%
  summarise(ct = n(), reward = sum(visit), mean_clk_rt = mean(visit)) %>%
  group_by(visited_american, visited_chinese) %>%
  filter(mean_clk_rt == max(mean_clk_rt)) %>%
  kable()

alpha = 7

#helper functions
# return the ucb estimates p_t_a 
inside_for_func <- function(inverse_cov_matrix, reward_vector_times_design_matrix, context_vector, alpha){
  theta_hat <- inverse_cov_matrix %*% reward_vector_times_design_matrix
  ucb_estimate <- t(theta_hat) %*% context_vector + 
    alpha * sqrt(t(context_vector) %*% inverse_cov_matrix %*% context_vector)
  return(ucb_estimate)
}

# update the covariate matrix
update_cov_matrix <- function(cov_matrix, context_vector){
  return(cov_matrix + context_vector %*% t(context_vector))
}

# update b_a from above
update_reward_vector_times_design_matrix <- function(reward_vector_times_design_matrix, reward, context_vector){
  return(reward_vector_times_design_matrix + reward * context_vector)
}

#instantiating objects
arms <- c(1:3)
d <- 2
arm_choice <- c()
cov_matrix <- list()
reward_vector_times_design_matrix <- list() 
ucb_estimate <- matrix(0, n, length(arms))

for (t in 1:n){
  context <- bandit_data[t,]
  for (a in arms){
    if(t == 1){
      cov_matrix[[a]] <- diag(d)
      reward_vector_times_design_matrix[[a]] <- rep(0, d)
    }
    inverse_cov_matrix <- ginv(cov_matrix[[a]])
    ucb_estimate[t, a] <- inside_for_func(inverse_cov_matrix, 
                                          as.matrix(reward_vector_times_design_matrix[[a]]), 
                                          as.matrix(c(context$visited_american, context$visited_chinese)), 
                                          alpha)
  }
  trial_arm <- which(ucb_estimate[t,] == max(ucb_estimate[t,]))
  if(length(trial_arm) > 1){
    trial_arm <- sample(trial_arm, 1)
  }
  if(trial_arm == context$arm){
    arm_choice[t] <- trial_arm
  }else{
    arm_choice[t] <- t*10 # need to do this so I can filter out unused observations from bandit dataset
    next
  }
  cov_matrix[[arm_choice[t]]] <- update_cov_matrix(cov_matrix[[arm_choice[t]]], 
                                                   as.matrix(c(context$visited_american, context$visited_chinese)))
  reward_vector_times_design_matrix[[arm_choice[t]]] <- update_reward_vector_times_design_matrix(
    as.matrix(reward_vector_times_design_matrix[[arm_choice[t]]]),
    context$visit,
    as.matrix(c(context$visited_american, context$visited_chinese))
  )
}

bandit_data$arm_choice <- arm_choice

# function to apply to the list columns of the bandit data
lm_fun <- function(data){
  return(tidy(summary(lm(visit ~ 0 + visited_american + visited_chinese, data))))
}

# apply the lm function to each arm's data from the original dataset
bandit_data %>%
  nest(-arm) %>%
  mutate(model = map(data, lm_fun)) %>%
  unnest(model) %>%
  dplyr::select(arm, term, data_estimate = estimate) %>%
  arrange(arm) -> coefficients_from_data

# calculate the coefficients for each of the arms using the bandit data
map_df(arms, function(i) data_frame(arm = i, term = c("visited_american", "visited_chinese"), bandit_estimate = as.vector(ginv(cov_matrix[[i]]) %*% reward_vector_times_design_matrix[[i]]))) -> coefficients_from_bandit

# join them together and see how different they are
coefficients_from_data %>%
  inner_join(coefficients_from_bandit, by = c("arm", "term")) %>%
  mutate(percent_difference = 100*((bandit_estimate - data_estimate)/data_estimate)) -> estimate_data

kable(estimate_data)

#average reward over time
bandit_data %>%
  filter(arm_choice < 10) %>%
  group_by(visited_american, visited_chinese, arm_choice) %>%
  mutate(total_reward = cumsum(visit), trial = c(1:n())) %>%
  mutate(avg_reward = total_reward/trial) %>%
  ggplot(aes(x = trial, y = avg_reward, color = factor(arm), group = factor(arm))) +
  geom_path() +
  facet_wrap(~visited_chinese + visited_american, scales = "free", labeller = "label_both")

