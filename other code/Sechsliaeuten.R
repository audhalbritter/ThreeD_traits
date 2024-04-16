time <- read_csv("data/boeoegg_burn_duration.csv")
weather <- read_csv("data/zuri_historical_weather.csv")


dat <- weather |>
  filter(Month %in% c(6:8)) |>
  group_by(Year) |>
  summarise(temperature = mean(Temperature)) |>
  left_join(time, by = c("Year" = "year")) |>
  filter(Year > 1964)

fit <- lm(temperature ~ burn_duration_seconds, dat)
summary(fit)
library(broom)
prediction <- augment(fit, interval = "confidence")


ggplot(dat, aes(x = burn_duration_seconds, y = temperature)) +
  geom_point() +
  geom_line(data = prediction, mapping = aes(x = burn_duration_seconds, y = .fitted)) +
  geom_ribbon(data = prediction
              , aes(y = .fitted, ymin = .lower, ymax = .upper),
              alpha = 0.2) +
  stat_cor(label.x = 2000, label.y = 21) +
  labs(x = "Burn duration of the Böögg (s)", y = "Mean summer temperature (°C)") +
  theme_bw()
