# figure plan
figure_plan <- list(

  # colour palette
  tar_target(
    name = warming_palette,
    command = wes_palette("GrandBudapest1")
    #c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
    ),

  tar_target(
    name = grazing_palette,
    command = c("seagreen4", "seagreen2")
  ),


  # make figures
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
    mutate(x_var = -Inf, y_var = Inf) |>
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
            mutate(vjust_var = case_when(term == "W" ~ 1.4,
                                 term == "N" ~ 3,
                                 term == "O" ~ 4.6,
                                 term == "WxO" ~ 6.2,
                                 term == "NxO" ~ 7.8),
           hjust_var = case_when(term == "W" ~ -0.2,
                                 term == "N" ~ -0.3,
                                 term == "O" ~ -0.3,
                                 term == "WxO" ~ -0.1,
                                 term == "NxO" ~ -0.1))

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
        scale_x_continuous(breaks = c(0, 1.79, 2.4, 3.93, 5.02), 
                           labels = c(0, 5, 10, 50, 150) 
                          #limits = c(log(1), log(230))
                          ) +
        # add some space at the top
        scale_y_continuous(expand = expansion(mult = c(0, 0.25))) +
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

      col1 <- grazing_palette
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
                                     levels = c("Leaf~thickness~(mm)", "SLA~(cm^2*g^{-1})", "LDMC~(gg^{-1})")))


      figure_text <- text |>
        filter(!figure_names %in% c("Plant~height~(cm)", "Leaf~dry~mass~(g)", "Leaf~area~(cm^2)")) |>
        mutate(figure_names = factor(figure_names,
                                     levels = c("Leaf~thickness~(mm)", "SLA~(cm^2*g^{-1})", "LDMC~(gg^{-1})"))) |>
        filter(term != "GxO") |>
        mutate(vjust_var = case_when(figure_names == "Leaf~thickness~(mm)" & term == "W" ~ 1.4,
                                     figure_names == "SLA~(cm^2*g^{-1})" & term == "O" ~ 1.4,
                                     figure_names == "SLA~(cm^2*g^{-1})" & term == "WxO" ~ 3,
                                     figure_names == "LDMC~(gg^{-1})" & term == "WxO" ~ 1.4),
               hjust_var = case_when(term == "W" ~ -0.2,
                                 term == "O" ~ -0.3,
                                 term == "WxO" ~ -0.1,
                                 term == "GxO" ~ -0.1))


      warming <- dat |>
        ggplot(aes(x = origSiteID, y = mean, fill = warming, colour = warming)) +
        geom_violin(draw_quantiles = c(0.5), colour = "grey20") +
        # text
        geom_text(data = pred |>
                    distinct(figure_names, origSiteID, warming, grazing) |>
                    full_join(figure_text,
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
                                     levels = c("SLA~(cm^2*g^{-1})", "LDMC~(gg^{-1})")))


      figure_text2 <- text |>
        filter(figure_names %in% c("SLA~(cm^2*g^{-1})", "LDMC~(gg^{-1})")) |>
        mutate(figure_names = factor(figure_names,
                                     levels = c("SLA~(cm^2*g^{-1})", "LDMC~(gg^{-1})"))) |>
        filter(term != "WxO") |>
        mutate(vjust_var = case_when(figure_names == "SLA~(cm^2*g^{-1})" & term == "O" ~ 1.4,
                                     figure_names == "SLA~(cm^2*g^{-1})" & term == "GxO" ~ 3,
                                     figure_names == "LDMC~(gg^{-1})" & term == "GxO" ~ 1.4),
               hjust_var = case_when(term == "O" ~ -0.3,
                                     term == "GxO" ~ -0.1))


      # Grazing only model
      grazing <- dat2 |>
        ggplot(aes(x = origSiteID, y = mean, fill = grazing, colour = grazing)) +
        geom_violin(draw_quantiles = c(0.5), colour = "grey20") +
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

  # ITV figure
  tar_target(
    name = itv_figure,
    command = make_ITV_plot_origin(variance_part)
  ),

  # multivariate trait figure
  tar_target(
    name = trait_pca_figure,
    command = make_pca_plot(trait_pca, col2 = warming_palette)
  ),

  tar_target(
    name = full_trait_pca_figure,
    command = make_full_pca_plot(trait_pca, col2 = warming_palette)
  )

)
