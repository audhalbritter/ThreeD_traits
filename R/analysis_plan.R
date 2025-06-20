# analysis plan
analysis_plan <- list(

  # slope and aspect
  tar_target(
    name = slope_aspect,
    command = slope_raw |>
      summarise(se_slope = sd(slope)/sqrt(n()),
                slope = mean(slope),
                se_aspect = sd(aspect)/sqrt(n()),
                aspect = mean(aspect))
  ),
  

  ### Run trait model

  # run full model
  tar_target(
    name = full_models,
    command = trait_mean |>
      group_by(trait_trans, figure_names) |>
      nest() |>
      mutate(model_log = map(data, ~lm(mean ~ warming * Nitrogen_log + warming * origSiteID + Nitrogen_log * origSiteID + warming * grazing + grazing * origSiteID, data = .)),
             model_lognoITV = map(data, ~lm(mean_noitv ~ warming * Nitrogen_log + warming * origSiteID + Nitrogen_log * origSiteID + warming * grazing + grazing * origSiteID, data = .)),

             glance_log = map(.x = model_log, .f = ~ safely(glance)(.x)$result),
             glance_lognoITV = map(.x = model_lognoITV, .f = ~ safely(glance)(.x)$result)) |>
      # make long table
      pivot_longer(cols = -c(trait_trans, figure_names, data),
                   names_sep = "_",
                   names_to = c(".value", "names")) |>
      #unnest(AIC)
      unnest(glance) |>
      select(trait_trans:adj.r.squared, AIC, deviance)
  ),

  # make prediction
  tar_target(
    name = trait_prediction,
    command = make_prediction_full(full_models |>
                                     filter(names == "log"))
  ),

  # merge data and make prediction
  tar_target(
    name = trait_output,
    command = trait_prediction |>
      # merge data and prediction
      mutate(output = map2(.x = newdata, .y = prediction, ~ bind_cols(.x, .y))) |>
      select(trait_trans, figure_names, names, output) |>
      unnest(output) |>
      rename(prediction = fit) |>
      mutate(prediction = if_else(trait_trans %in% c("sla_cm2_g", "ldmc"), prediction, exp(prediction)),
             lwr = if_else(trait_trans %in% c("sla_cm2_g", "ldmc"), lwr, exp(lwr)),
             upr = if_else(trait_trans %in% c("sla_cm2_g", "ldmc"), upr, exp(upr)))
  ),



  ### ITV analysis

tar_target(
  name = itv_output,
  command = make_ITV_analysis_origin(trait_mean)
),

tar_target(
  name = variance_part,
  command = make_ITV_proportions_origin(itv_output)
),



  # # check models
  # tar_quarto(name = model_check,
  #            path = "R/model_checking.qmd", quiet = FALSE)



  # MULTIVARIATE TRATI ANALYSIS

  tar_target(
    name = trait_pca,
    command = make_trait_pca(trait_mean)
  )


 )
