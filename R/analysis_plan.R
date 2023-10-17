# analysis plan
analysis_plan <- list(

  # prep cover
  tar_target(
    name = trait_analysis,
    command = trait_mean |>
      group_by(origSiteID) |>
      nest() |>
      mutate(model = map(data, ~lm(mean ~ warming * grazing * Namount_kg_ha_y, data = .)),
             result = map(model, tidy),
             anova = map(model, car::Anova),
             anova_tidy = map(anova, tidy))
  )

)
