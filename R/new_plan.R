new_plan <- list(

  # trait impute
  tar_target(
    name = trait_impute,
    command = make_trait_impute(community, traits)
  ),

  # trait impute noitv
  tar_target(
    name = trait_null_impute,
    command = make_trait_null_impute(community, traits)
  ),

  # bootstrapping
  tar_target(
    name = trait_mean,
    command = make_bootstrapping(trait_impute, trait_null_impute)
  ),

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

  # merge data and prediction
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

  tar_target(
    name = text,
    command = trait_prediction |>
      ungroup() |>
      select(figure_names, anova_tidy) |>
      unnest(anova_tidy) |>
      mutate(term = case_when(str_detect(term, "warming:Nitrogen_log") ~ "WxN",
                            str_detect(term, "Nitrogen_log:origSiteID") ~ "NxO",
                            str_detect(term, "warming:origSiteID") ~ "WxO",
                            str_detect(term, "warming:grazing") ~ "WxG",
                            str_detect(term, "origSiteID:grazing") ~ "GxO",
                            str_detect(term, "origSiteID") ~ "O",
                            str_detect(term, "Nitrogen_log") ~ "N",
                            str_detect(term, "warming") ~ "W",
                            str_detect(term, "grazing") ~ "G",
                            TRUE ~ term)) |>
    filter(!str_detect(term, "Residuals"),
           p.value <= 0.05) |>
    mutate(x_var = Inf, y_var = -Inf, hjust_var = 1) |>
    ungroup()

  ),

  # make prediction
  tar_target(
    name = trait_WN_figure,
    command = {

      col2 <- warming_palette

      pred <- trait_output |>
      filter(figure_names %in% c("Plant~height~(cm)", "Leaf~dry~mass~(g)", "Leaf~area~(cm^2)"))

      dat  <-  trait_prediction |>
        unnest(data) |>
        filter(figure_names %in% c("Plant~height~(cm)", "Leaf~dry~mass~(g)", "Leaf~area~(cm^2)")) |>
        select(trait_trans:mean) |>
        mutate(mean = exp(mean))


      figure_text <- text |>
        filter(figure_names %in% c("Plant~height~(cm)", "Leaf~dry~mass~(g)", "Leaf~area~(cm^2)")) |>
        filter(term != "GxO") |>
        mutate(vjust_var = case_when(figure_names == "Plant~height~(cm)" & term == "W" ~ -5.6,
                                     figure_names == "Plant~height~(cm)" & term == "N" ~ -4,
                                     figure_names == "Plant~height~(cm)" & term == "O" ~ -2.4,
                                     figure_names == "Plant~height~(cm)" & term == "WxO" ~ -0.8,
                                     term == "W" ~ -7.2,
                                     term == "N" ~ -5.6,
                                     term == "O" ~ -4,
                                     term == "WxO" ~ -2.4,
                                     term == "NxO" ~ -0.8))



      dat |>
        filter(grazing == "Ungrazed") |>
        ggplot(aes(x = Nitrogen_log, y = mean, colour = warming, shape = origSiteID, linetype = origSiteID)) +
        geom_ribbon(data = pred |>
                      filter(grazing == "Ungrazed"),
                    aes(y = prediction, ymin = lwr,
                        ymax = upr,
                        fill = warming),
                    alpha = 0.2,
                    linetype = 0) +
        geom_point() +
        geom_line(data = pred |>
                    filter(grazing == "Ungrazed"),
                  mapping = aes(y = prediction)) +
        # text
        geom_text(data = pred |>
                    filter(grazing == "Ungrazed") |>
                    distinct(figure_names, origSiteID, warming, Nitrogen_log) |>
                    full_join(figure_text,
                              by = c("figure_names")),
                  aes(x = x_var, y = y_var, hjust = hjust_var, vjust = vjust_var, label = term),
                  size = 3, colour = "black") +
        # change labels to real values
        scale_x_continuous(breaks = c(log(1), log(5), log(25), log(150)), labels = c(1, 5, 25, 150), limits = c(log(1), log(230))) +
        scale_colour_manual(name = "", values = c("grey40", col2[2])) +
        scale_fill_manual(name = "", values = c("grey40", col2[2])) +
        scale_shape_manual(name = "", values = c(17, 16), labels = c("Alpine site", "Sub-alpine site")) +
        scale_linetype_manual(name = "", values = c("solid", "dashed"), labels = c("Alpine site", "Sub-alpine site")) +
        labs(x = bquote(Nitrogen~addition~(kg~ha^-1~y^-1)),
             y = "Mean trait value") +
        facet_wrap(~figure_names, scales = "free", labeller = label_parsed) +
        theme_bw() +
        theme(legend.position = "top",
              legend.key.width = unit(1.75, "line"))


    }
  ),


  tar_target(
    name = trait_WG_figure,
    command = {

      col1 <- habitat_palette
      col2 <- warming_palette

      pred <- trait_output |>
        filter(Nitrogen_log == 0) |>
        filter(!figure_names %in% c("Plant~height~(cm)", "Leaf~dry~mass~(g)", "Leaf~area~(cm^2)"))

      dat  <-  trait_prediction |>
        unnest(data) |>
        filter(Nitrogen_log == 0) |>
        filter(!figure_names %in% c("Plant~height~(cm)", "Leaf~dry~mass~(g)", "Leaf~area~(cm^2)")) |>
        select(trait_trans:mean) |>
        mutate(mean = if_else(figure_names == "Leaf~thickness~(mm)", exp(mean), mean)) |>
        mutate(figure_names = factor(figure_names,
                                     levels = c("Leaf~thickness~(mm)", "SLA~(cm^2*g^{-1})", "LDMC~(gg^{-1})")),
               origSiteID = factor(origSiteID, levels = c("Sub-alpine", "Alpine")))


      figure_text <- text |>
        filter(!figure_names %in% c("Plant~height~(cm)", "Leaf~dry~mass~(g)", "Leaf~area~(cm^2)")) |>
        mutate(figure_names = factor(figure_names,
                                     levels = c("Leaf~thickness~(mm)", "SLA~(cm^2*g^{-1})", "LDMC~(gg^{-1})"))) |>
        filter(term != "GxO") |>
        mutate(vjust_var = case_when(figure_names == "Leaf~thickness~(mm)" & term == "W" ~ -0.8,
                                     figure_names == "SLA~(cm^2*g^{-1})" & term == "O" ~ -2.4,
                                     figure_names == "SLA~(cm^2*g^{-1})" & term == "WxO" ~ -0.8,
                                     figure_names == "LDMC~(gg^{-1})" & term == "WxO" ~ -0.8))


      warming <- dat |>
        ggplot(aes(x = origSiteID, y = mean, fill = warming, colour = warming)) +
        geom_violin() +
        # text
        geom_text(data = pred |>
                    distinct(figure_names, origSiteID, warming, grazing) |>
                    left_join(figure_text,
                              by = c("figure_names")),
                  aes(x = x_var, y = y_var, hjust = hjust_var, vjust = vjust_var, label = term),
                  size = 3, colour = "black") +
        scale_fill_manual(name = "", values = c("grey40", col2[2])) +
        scale_colour_manual(name = "", values = c("grey40", col2[2])) +
        labs(y = "Mean trait value", x = "", tag = "a)") +
        facet_wrap(~figure_names , scales = "free_y",
                   labeller = label_parsed) +
        theme_bw() +
        theme(legend.position = "top")




      pred2 <- trait_output |>
        filter(Nitrogen_log == 0) |>
        filter(figure_names %in% c("SLA~(cm^2*g^{-1})", "LDMC~(gg^{-1})"))

      dat2  <-  trait_prediction |>
        unnest(data) |>
        filter(Nitrogen_log == 0) |>
        filter(figure_names %in% c("SLA~(cm^2*g^{-1})", "LDMC~(gg^{-1})")) |>
        select(trait_trans:mean) |>
        mutate(figure_names = factor(figure_names,
                                     levels = c("SLA~(cm^2*g^{-1})", "LDMC~(gg^{-1})")),
               origSiteID = factor(origSiteID, levels = c("Sub-alpine", "Alpine")))


      figure_text2 <- text |>
        filter(figure_names %in% c("SLA~(cm^2*g^{-1})", "LDMC~(gg^{-1})")) |>
        mutate(figure_names = factor(figure_names,
                                     levels = c("SLA~(cm^2*g^{-1})", "LDMC~(gg^{-1})"))) |>
        filter(term != "WxO") |>
        mutate(vjust_var = case_when(figure_names == "SLA~(cm^2*g^{-1})" & term == "O" ~ -2.4,
                                     figure_names == "SLA~(cm^2*g^{-1})" & term == "GxO" ~ -0.8,
                                     figure_names == "LDMC~(gg^{-1})" & term == "GxO" ~ -0.8))


      # Grazing only model
      grazing <- dat2 |>
        ggplot(aes(x = origSiteID, y = mean, fill = grazing, colour = grazing)) +
        geom_violin() +
        # text
        geom_text(data = pred2 |>
                    distinct(figure_names, origSiteID, warming, grazing) |>
                    left_join(figure_text2,
                              by = c("figure_names")),
                  aes(x = x_var, y = y_var, hjust = hjust_var, vjust = vjust_var, label = term),
                  size = 3, colour = "black") +
        scale_fill_manual(name = "", values = c(col1[1], col1[2])) +
        scale_colour_manual(name = "", values = c(col1[1], col1[2])) +
        labs(y = "Mean trait value", x = "", tag = "b)") +
        facet_wrap(~ figure_names, scales = "free_y",
                   labeller = label_parsed) +
        theme_bw() +
        theme(legend.position = "top")

      warming / grazing


    }
  ),


  # MULTIVARIATE TRATI ANALYSIS

  tar_target(
    name = trait_pca,
    command = make_trait_pca(trait_mean)
  ),

  tar_target(
    name = trait_pca_figure,
    command = make_pca_plot(trait_pca, col2 = warming_palette)
  ),

  tar_target(
    name = full_trait_pca_figure,
    command = make_full_pca_plot(trait_pca, col2 = warming_palette)
  )

)

