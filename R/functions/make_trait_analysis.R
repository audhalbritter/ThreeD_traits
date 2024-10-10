# get propoer N model

run_all_models <- function(n_trait_mean){

  n_trait_mean |>
    group_by(trait_trans, figure_names) |>
    nest() |>
    mutate(model_linear = map(data, ~lm(mean ~ warming * Namount_kg_ha_y + warming * origSiteID + Namount_kg_ha_y * origSiteID, data = .)),
           model_log = map(data, ~lm(mean ~ warming * Nitrogen_log + warming * origSiteID + Nitrogen_log * origSiteID, data = .)),
           model_lognoITV = map(data, ~lm(mean_noitv ~ warming * Nitrogen_log + warming * origSiteID + Nitrogen_log * origSiteID, data = .)),
           model_quad = map(data, ~lm(mean ~ warming * poly(Namount_kg_ha_y, 2) + warming * origSiteID + poly(Namount_kg_ha_y, 2) * origSiteID, data = .)),

           glance_linear = map(.x = model_linear, .f = ~ safely(glance)(.x)$result),
           glance_log = map(.x = model_log, .f = ~ safely(glance)(.x)$result),
           glance_lognoITV = map(.x = model_lognoITV, .f = ~ safely(glance)(.x)$result),
           glance_quad = map(.x = model_quad, .f = ~ safely(glance)(.x)$result)) |>
    # make long table
    pivot_longer(cols = -c(trait_trans, figure_names, data),
                 names_sep = "_",
                 names_to = c(".value", "names")) |>
    #unnest(AIC)
    unnest(glance) |>
    select(trait_trans:adj.r.squared, AIC, deviance)

}


make_prediction <- function(trait_model){

  # make predictions for more data points
  Namount_kg_ha_y = seq(from = 0, to = 150, by = 0.5)
  Nitrogen_log = seq(from = 0, to = 5, by = 0.1)

  trait_model |>
    mutate(result = map(model, tidy),
           anova = map(model, car::Anova),
           anova_tidy = map(anova, tidy),
           # make new data
           newdata = ifelse(names %in% c("log", "lognoITV"),
                            # for log model
                            map(.x = data, .f = ~. |>
                                  distinct(warming, origSiteID) |>
                                  crossing(Nitrogen_log)),
                            # for linear and quadratic models
                            map(.x = data, .f = ~. |>
                                  distinct(warming, origSiteID) |>
                                  crossing(Namount_kg_ha_y) |>
                                  mutate(Nitrogen_log = log(Namount_kg_ha_y + 1)))
                            ),
           # predict with newdata
           prediction = map2(.x = model, .y = newdata, .f = predict, interval = "confidence"))

}




