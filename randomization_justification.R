
library(dplyr)

n_subjects <- 100

confounder1 <- rnorm(n_subjects)
confounder2 <- rbinom(n_subjects, 1, 0.5)
confounder3 <- runif(n_subjects, 0, 10)
confounder4 <- rexp(n_subjects, rate = 0.1)
confounder5 <- rpois(n_subjects, lambda = 3)

data <- data.frame(confounder1, confounder2, confounder3, confounder4, confounder5)

data$group <- ifelse(runif(n_subjects) > 0.5, 1, 0)

summary_by_group <- data %>%
  group_by(group) %>%
  summarise(across(starts_with("confounder"), mean))

print(summary_by_group)