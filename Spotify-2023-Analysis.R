library(ggplot2)
library(dplyr)
library(e1071)

load("spotify-2023-clean.Rdata")
  
## Danceability Distribution
summary(data)
danceability_mean <- summary(data$danceability)[c("Mean")]
danceability_median <- summary(data$danceability)[c("Median")]
danceability_std <- sd(data$danceability)

hist(data$danceability,
     main = "Distribution of Danceability",
     xlab = "Danceability (%)",
     ylab = "Frequency",
     col = "#1DB954",
     border = "white",
     breaks = 20)

cat("Mean:", round(danceability_mean, 2), "\n")
cat("Median:", round(danceability_median, 2), "\n")
cat("Std Dev:", round(danceability_std, 2), "\n")
cat("Skewness:", round(skewness(data$danceability), 2), "\n")


## Energy vs. Valence Relationship
ggplot(data, aes(x = energy, y = valence, color = released_year)) +
  geom_point(alpha = 0.6, size = 2) +
  geom_smooth(method = lm, se = FALSE, color = "black", linewidth = 1.2) +
  scale_color_gradient(low = "#FF6347", high = "#1DB954", name = "Release Year") +
  labs(
    title = "Energy vs. Valence Across Release Years",
    x = "Energy (%)",
    y = "Valence (Positivity %)"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 16),
    legend.position = "right"
  )

cor_energy <- cor.test(data$energy, data$valence)
cat("Correlation between energy and valence:", round(cor_energy$estimate, 3), "\n")
cat("P-value:", format(cor_energy$p.value, scientific = TRUE), "\n")


## BPM Impact on Streaming Success
data <- mutate(data,
  BPM_category = case_when(
    bpm < 100 ~ "Slow",
    bpm <= 120 ~ "Medium",
    bpm > 120 ~ "Fast"
  )
)

data$BPM_category <- factor(data$BPM_category, levels = c("Slow", "Medium", "Fast"))

ggplot(data, aes(x = BPM_category, y = streams, fill = BPM_category)) +
  geom_boxplot(alpha = 0.8, outlier.color = "#FF6347", outlier.size = 2) +
  geom_hline(yintercept = median(data$streams[data$BPM_category=="Slow"]), linetype = "dotted") +
  scale_fill_manual(
    values = c("Slow" = "#1ed760", "Medium" = "#1ed760", "Fast" = "#1ed760")
  ) +
  scale_y_log10(labels = scales::comma) +
  labs(
    title = "Streaming Distribution by BPM Category",
    x = "BPM Category",
    y = "Number of Streams (log scale)"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 16),
    legend.position = "none"
  )

## Audio Feature Correlation Analysis
correlation_matrix <- cor(
  data[, c("danceability", "energy", "valence", "bpm")],
  use = "complete.obs"
)

heatmap(correlation_matrix,
  symm = TRUE,
  main = "Audio Feature Correlation Heatmap",
  col = colorRampPalette(c("white", "#1DB954"))(20),
  margins = c(10, 10)
)

## Temporal Trends: Danceability Over Time
data <- mutate(data,
  release_period = case_when(
    released_year < 2020 ~ "Before 2020",
    released_year >= 2020 ~ "2020 or later"
  )
)

t_test_result <- t.test(danceability ~ release_period, data = data)

boxplot(danceability ~ release_period,
  data = data,
  main = "Danceability: Before vs. After 2020",
  xlab = "Release Period",
  ylab = "Danceability (%)",
  col = c("#FF6347", "#1DB954"),
  border = "black",
  horizontal = FALSE
)

cat("T-test p-value:", format(t_test_result$p.value, scientific = TRUE), "\n")
cat("Mean (Before 2020):", round(t_test_result$estimate[1], 2), "\n")
cat("Mean (2020+):", round(t_test_result$estimate[2], 2), "\n")

## Major vs. Minor Mode Comparison
variables <- c("energy", "danceability", "valence", "acousticness", 
               "speechiness", "liveness", "bpm", "instrumentalness")

my_t_test <- function(variable) {
  t.test(data[[variable]] ~ data$mode)
}

t_test_results <- lapply(variables, my_t_test)
p_values <- sapply(t_test_results, function(test) test$p.value)

results <- data.frame(
  Variable = variables,
  P_Value = round(p_values, 5),
  Bonferroni = round(p.adjust(p_values, method = "bonferroni"), 5),
  BH = round(p.adjust(p_values, method = "BH"), 5)
)


cat("\nSignificant tests (α = 0.05):\n")
cat("Before correction:", sum(results$P_Value < 0.05), "\n")
cat("After Bonferroni:", sum(results$Bonferroni < 0.05), "\n")
cat("After BH:", sum(results$BH < 0.05), "\n")

## Top 100 Songs: Valence Analysis
top_100 <- data %>%
  arrange(desc(streams)) %>%
  slice(1:100)

bootstrap_sample_mean <- function(data) {
  bootstrap_sample <- sample(data, length(data), replace = TRUE)
  return(mean(bootstrap_sample))
}

set.seed(123)
bootstrap_means <- replicate(1000, bootstrap_sample_mean(top_100$valence))
ci <- quantile(bootstrap_means, c(0.025, 0.975))

hist(bootstrap_means,
     main = "Bootstrap Distribution of Mean Valence (Top 100 Songs)",
     xlab = "Mean Valence (%)",
     col = "#1DB954",
     border = "white",
     breaks = 30
)
abline(v = ci, col = "#FF6347", lwd = 2, lty = 2)

cat("95% Bootstrap Confidence Interval:\n")
cat("Lower bound:", round(ci[1], 2), "%\n")
cat("Upper bound:", round(ci[2], 2), "%\n")