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
             prediction = map(model, augment, interval = "confidence"),

             model_noITV = map(data, ~lm(mean_noitv ~ warming * grazing + warming * origSiteID + grazing * origSiteID, data = .)),
             result_noITV = map(model_noITV, tidy),
             anova_noITV = map(model_noITV, car::Anova),
             anova_tidy_noITV = map(anova_noITV, tidy),
             prediction_noITV = map(model_noITV, augment, interval = "confidence"))
  ),

  tar_target(
    name = g_trait_anova,
    command = bind_rows(
      ITV = g_trait_models |>
        unnest(anova_tidy) |>
        select(figure_names, term:p.value),
      Turnover = g_trait_models |>
        unnest(anova_tidy_noITV) |>
        select(figure_names, term:p.value),
      .id = "process") |>
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
      filter(names %in% c("log", "lognoITV")) |>
      mutate(process = if_else(names == "log", "ITV", "Turnover"))
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
      select(process, figure_names, anova_tidy) |>
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
      select(process, figure_names, names, term:p.value)
  ),

  tar_target(
    name = trait_anovas,
    command = {
      n <- n_trait_anova |>
        filter(process == "ITV") |>
        mutate(sumsq = round(sumsq, 2),
               df = round(df, 2),
               statistic = round(statistic, 2),
               p.value = round(p.value, 3)) |>
        rename(Traits = figure_names, Term = term, "Sum of Square" = sumsq, "F" = statistic, "P" = "p.value")

      g <- g_trait_anova |>

        filter(process == "ITV") |>
        ungroup() |>
        select(-trait_trans) |>
        mutate(sumsq = round(sumsq, 2),
               df = round(df, 2),
               statistic = round(statistic, 2),
               p.value = round(p.value, 3)) |>
        rename(Traits = figure_names, Term = term, "Sum of Square" = sumsq, "F" = statistic, "P" = "p.value")

      bind_cols(n,
                g |>
                  select(Term_g = Term, `Sum of Square_g` = `Sum of Square`, df_g = df, F_g = 'F', P_g = P)) |>
        ungroup() |>
        select(-process) |>
        arrange(Traits)

    }

  ),

  ### ITV analysis

tar_target(
  name = itv_output,
  command = make_ITV_analysis(g_trait_mean, n_trait_mean)
),

tar_target(
  name = itv_output2,
  command = make_ITV2_analysis(g_trait_mean, n_trait_mean)
)


#   # check models
#   tar_quarto(name = model_check,
#              path = "R/model_checking.qmd"),


  # MULTIVARIATE TRATI ANALYSIS

#   tar_target(
#     name = g_trait_pca,
#     command = make_trait_pca(g_trait_mean)
#   ),
#
#   tar_target(
#     name = n_trait_pca,
#     command = make_trait_pca(n_trait_mean)
#   )
 )
