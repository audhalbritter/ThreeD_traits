# analysis plan
analysis_plan <- list(

  # SINGLE TRAIT ANALYIS

  # trait analysis (warming and grazing)
  tar_target(
    name = g_trait_models,
    command = g_trait_mean |>
      group_by(trait_trans, figure_names) |>
      nest() |>
      mutate(model = map(data, ~lm(mean ~ warming * grazing + warming * origSiteID + grazing * origSiteID, data = .)),
             result = map(model, tidy),
             anova = map(model, car::Anova),
             anova_tidy = map(anova, tidy),
             prediction = map(model, augment, interval = "confidence"))
  ),

  tar_target(
    name = g_trait_anova,
    command = g_trait_models |>
      unnest(anova_tidy) |>
      select(figure_names, term:p.value) |>
      mutate(term = case_when(str_detect(term, "warming:origSiteID") ~ "WxO",
                              str_detect(term, "warming:grazing") ~ "WxG",
                              str_detect(term, "grazing:origSiteID") ~ "GxO",
                              str_detect(term, "grazing") ~ "G",
                              str_detect(term, "warming") ~ "W",
                              str_detect(term, "origSiteID") ~ "O",
                              TRUE ~ term))
  ),

# trait analysis (warming and nitrogen)
tar_target(
  name = all_n_trait_models,
  command = run_all_models(n_trait_mean)
),

  # only log model
  tar_target(
    name = n_trait_model,
    command = all_n_trait_models |>
      # select log model
      filter(names == "log")
  ),

  # make prediction for nitrogen model
  tar_target(
    name = n_trait_output,
    command = make_prediction(n_trait_model)
  ),

  # results
  tar_target(
    name = n_trait_anova,
    command = n_trait_output |>
      ungroup() |>
      select(figure_names, anova_tidy) |>
      unnest(anova_tidy) |>
      mutate(term = case_when(str_detect(term, "warming:Nitrogen_log") ~ "WxN",
                              str_detect(term, "Nitrogen_log:origSiteID") ~ "NxO",
                              str_detect(term, "warming:origSiteID") ~ "WxO",
                              str_detect(term, "origSiteID") ~ "O",
                              str_detect(term, "Nitrogen_log") ~ "N",
                              str_detect(term, "warming") ~ "W",

                              TRUE ~ term))

  ),

  tar_target(
    name = trait_result,
    command = n_trait_output |>
      ungroup() |>
      unnest(result) |>
      select(figure_names, names, term:p.value)
  ),

#   # check models
#   tar_quarto(name = model_check,
#              path = "R/model_checking.qmd"),


  # MULTIVARIATE TRATI ANALYSIS

  tar_target(
    name = g_trait_pca,
    command = make_trait_pca(g_trait_mean)
  ),

  tar_target(
    name = n_trait_pca,
    command = make_trait_pca(n_trait_mean)
  )
)
