# Load necessary package
library(MASS)     # for glm.nb
library(ggplot2)  # optional, for plotting

# Create the data
df <- data.frame(
  NDWI_avg = c(
    0.4973077, 0.3994075, 0.5572792, 0.4099959, 0.4041702,
    0.4572540, 0.4768103, 0.3971894, 0.4413446, 0.3902016,
    0.5263846, 0.4847200, 0.3033548, 0.4845739, 0.4879864,
    0.4725965, 0.4840152, 0.4758581, 0.3276186, 0.2717690
  ),
  rootknot = c(
    241, 16, 253, 119, 250,
    235, 118, 0, 0, 247,
    74, 254, 4, 0, 0,
    67, 231, 2, 4, 0
  )
)

# Fit negative binomial model
model_nb <- glm.nb(rootknot ~ NDWI_avg, data = df)

# View summary of model
summary(model_nb)


# Plot with fitted line (on log scale)
ggplot(df, aes(x = NDWI_avg, y = rootknot)) +
  geom_point() +
  stat_smooth(method = MASS::glm.nb, formula = y ~ x, se = TRUE) +
  labs(title = "Negative Binomial Regression: Rootknot vs NDWI",
       x = "NDWI.avg", y = "Rootknot Count") +
  theme_minimal()

