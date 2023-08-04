
library(tidyverse)
library(bbsBayes2)


fit <- readRDS("output/base.rds") # read in the base model fit
fit_cov <- readRDS("output/covariate.rds") # read in the covariate model fit



summ_base <- get_summary(fit) %>%
  mutate(variable_type = stringr::str_extract(variable, "^\\w+"),
         model = "base")

summ_cov <- get_summary(fit_cov) %>%
  mutate(variable_type = stringr::str_extract(variable, "^\\w+"),
         model = "covariate")

summ <- bind_rows(summ_base,summ_cov)

saveRDS(summ,"output/convergence_parameter_summaries.rds")



summ <- readRDS("output/convergence_parameter_summaries.rds")

# convergence -------------------------------------------------------------



rhat_ess_summ <- summ %>%
  group_by(variable_type) %>%
  summarise(n = n(),
            min_ess = min(ess_bulk),
            max_rhat = max(rhat),
            med_ess = median(ess_bulk),
            med_rhat = median(rhat))

ess_fail <- summ %>% filter(ess_bulk < 1000)

ess_fail_summ <- ess_fail %>%
  group_by(variable_type) %>%
  summarise(n = n(),
            min_ess = min(ess_bulk),
            max_rhat = max(rhat))

rhat_fail <- summ %>% filter(rhat > 1.03)

rhat_fail_summ <- rhat_fail %>%
  group_by(variable_type) %>%
  summarise(n = n(),
            min_ess = min(ess_bulk),
            max_rhat = max(rhat))

