library(dplyr)
library(ggplot2)
library(tidyr)

# Definir el número de sujetos en el experimento
n_subjects <- 100

# Definir el número de experimentos
n_experiments <- 500

# Función para simular un experimento y obtener el resumen de promedios por grupo
simulate_experiment <- function(n_subjects) {
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
  
  return(summary_by_group)
}

# Realizar múltiples experimentos y almacenar los resultados en una lista
results_list <- lapply(1:n_experiments, function(x) simulate_experiment(n_subjects))

# Combinar todos los resultados en un único dataframe
results <- do.call(rbind, results_list) %>%
  mutate(experiment = rep(1:n_experiments, each = 2))

# Calcular el promedio acumulativo de los factores de confusión en ambos grupos por experimento
cumulative_means <- results %>%
  group_by(group, experiment) %>%
  summarise(across(starts_with("confounder"), mean)) %>%
  group_by(group) %>%
  mutate(across(starts_with("confounder"), cummean))

# Convertir el dataframe de formato ancho a largo
cumulative_means_long <- cumulative_means %>%
  pivot_longer(cols = starts_with("confounder"), names_to = "confounder", values_to = "mean")

# Crear el gráfico de líneas para visualizar la convergencia de los factores de confusión
ggplot(data = cumulative_means_long, aes(x = experiment, y = mean, color = as.factor(group))) +
  geom_line() +
  facet_wrap(~ confounder, scales = "free_y") +
  labs(title = "Convergencia de los factores de confusión a lo largo de múltiples experimentos",
       x = "Número de experimentos",
       y = "Promedio acumulativo",
       color = "Grupo") +
  theme_minimal()