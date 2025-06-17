# Trait figures

make_nitrogen_figure <- function(n_trait_output, n_trait_anova, col2){

  # Nitrogen and warming model
  pred_n = n_trait_output |>
    # merge data and prediction
    mutate(output = map2(.x = newdata, .y = prediction, ~ bind_cols(.x, .y))) |>
    select(process, trait_trans, figure_names, names, output) |>
    unnest(output) |>
    rename(prediction = fit) |>
    filter(figure_names %in% c("Plant~height~(cm)", "Leaf~dry~mass~(g)", "Leaf~area~(cm^2)", "Leaf~thickness~(mm)")) |>
    mutate(prediction = exp(prediction),
           lwr = exp(lwr),
           upr = exp(upr)) |>
    filter(process == "ITV")

  dat = n_trait_output |>
    filter(figure_names %in% c("Plant~height~(cm)", "Leaf~dry~mass~(g)", "Leaf~area~(cm^2)", "Leaf~thickness~(mm)")) |>
    unnest(data) |>
    select(process, trait_trans:mean) |>
    mutate(mean = exp(mean)) |>
    filter(process == "ITV")

  text_n = n_trait_anova |>
    filter(!str_detect(term, "Residuals"),
           p.value <= 0.05) |>
    mutate(x_var = Inf, y_var = -Inf, hjust_var = 1) |>
    filter(figure_names %in% c("Plant~height~(cm)", "Leaf~dry~mass~(g)", "Leaf~area~(cm^2)", "Leaf~thickness~(mm)")) |>
    ungroup() |>
    mutate(vjust_var = case_when(figure_names == "Plant~height~(cm)" & term == "N" ~ -2.6,
                                 figure_names == "Plant~height~(cm)" & term == "WxO" ~ -0.8,
                                 figure_names %in% c("Plant~height~(cm)", "Leaf~dry~mass~(g)") & term == "W" ~ -5.6,
                                 figure_names == "Leaf~area~(cm^2)" & term == "W" ~ -7.2,
                                 term == "O" ~ -4,
                                 term == "N" ~ -5.6,
                                 figure_names %in% c("Leaf~dry~mass~(g)", "Leaf~area~(cm^2)") & term == "WxO" ~ -2.6,
                                 term == "NxO" ~ -0.8,
                                 figure_names ==  "Leaf~thickness~(mm)" & term == "W" ~ -0.8)) |>
    filter(process == "ITV")


  ggplot(dat, aes(x = Nitrogen_log, y = mean, colour = warming, shape = origSiteID, linetype = origSiteID)) +
    geom_ribbon(data = pred_n,
                aes(y = prediction, ymin = lwr,
                      ymax = upr,
                      fill = warming),
                alpha = 0.2,
                linetype = 0) +
    geom_point() +
    geom_line(data = pred_n,
              mapping = aes(y = prediction)) +
    # text
    geom_text(data = pred_n |>
                distinct(process, figure_names, origSiteID, warming, Nitrogen_log) |>
                full_join(text_n |>
                            filter(vjust_var == -0.8),
                          by = c("figure_names", "process")),
              aes(x = x_var, y = y_var, hjust = hjust_var, vjust = vjust_var, label = term),
              size = 3, colour = "black") +
    geom_text(data = pred_n |>
                distinct(process, figure_names, origSiteID, warming, Nitrogen_log) |>
                full_join(text_n |>
                            filter(vjust_var == -2.6),
                          by = c("figure_names", "process")),
              aes(x = x_var, y = y_var, hjust = hjust_var, vjust = vjust_var, label = term),
              size = 3, colour = "black") +
    geom_text(data = pred_n |>
                distinct(process, figure_names, origSiteID, warming, Nitrogen_log) |>
                full_join(text_n |>
                            filter(vjust_var == -4),
                          by = c("figure_names", "process")),
              aes(x = x_var, y = y_var, hjust = hjust_var, vjust = vjust_var, label = term),
              size = 3, colour = "black") +
    geom_text(data = pred_n |>
                distinct(process, figure_names, origSiteID, warming, Nitrogen_log) |>
                full_join(text_n |>
                            filter(vjust_var == -5.6,
                                   term == "W"),
                          by = c("figure_names", "process")),
              aes(x = x_var, y = y_var, hjust = hjust_var, vjust = vjust_var, label = term),
              size = 3, colour = "black") +
      geom_text(data = pred_n |>
                  distinct(process, figure_names, origSiteID, warming, Nitrogen_log) |>
                  full_join(text_n |>
                              filter(vjust_var == -5.6,
                                     term == "N"),
                            by = c("figure_names", "process")),
                aes(x = x_var, y = y_var, hjust = hjust_var, vjust = vjust_var, label = term),
                size = 3, colour = "black") +
    geom_text(data = pred_n |>
                distinct(process, figure_names, origSiteID, warming, Nitrogen_log) |>
                full_join(text_n |>
                            filter(vjust_var == -7.2),
                          by = c("figure_names", "process")),
              aes(x = x_var, y = y_var, hjust = hjust_var, vjust = vjust_var, label = term),
              size = 3, colour = "black") +
    # change labels to real values
    scale_x_continuous(breaks = c(log(1), log(5), log(25), log(150)), labels = c(1, 5, 25, 150)) +
    scale_colour_manual(name = "", values = c("grey40", col2[2])) +
    scale_fill_manual(name = "", values = c("grey40", col2[2])) +
    scale_shape_manual(name = "", values = c(17, 16), labels = c("Alpine site", "Sub-alpine site")) +
    scale_linetype_manual(name = "", values = c("solid", "dashed"), labels = c("Alpine site", "Sub-alpine site")) +
    labs(x = bquote(log(Nitrogen)~kg~ha^-1~y^-1),
         y = "Mean trait value") +
    facet_wrap(~ figure_names, scales = "free", labeller = label_parsed) +
    theme_bw() +
    theme(legend.position = "top",
          legend.key.width = unit(1.75, "line"))

}


# Warming only model
make_grazing_figure <- function(g_trait_models, g_trait_anova, col1, col2){

  pred <- bind_rows(ITV = g_trait_models |>
              unnest(prediction) |>
              select(trait_trans, figure_names, mean:.std.resid),

            Turnover = g_trait_models |>
              unnest(prediction_noITV) |>
              select(trait_trans, figure_names, mean = mean_noitv, warming:.std.resid),
            .id = "process") |>
    filter(trait_trans %in% c("sla_cm2_g", "ldmc")) |>
    mutate(figure_names = factor(figure_names,
                          levels = c("SLA~(cm^2*g^{-1})", "LDMC~(gg^{-1})")),
    origSiteID = factor(origSiteID, levels = c("Sub-alpine", "Alpine"))) |>
    filter(process == "ITV")


  text = g_trait_anova |>
    filter(!str_detect(term, "Residuals"),
           p.value <= 0.05) |>
    filter(trait_trans %in% c("sla_cm2_g", "ldmc")) |>
    mutate(x_var = Inf, y_var = -Inf, hjust_var = 1) |>
    mutate(figure_names = factor(figure_names,
                                levels = c("SLA~(cm^2*g^{-1})", "LDMC~(gg^{-1})"))) |>
    filter(process == "ITV")


  warming <- pred |>
    #filter(grazing == "Ungrazed") |>
    ggplot(aes(x = origSiteID, y = mean, fill = warming)) +
    geom_boxplot() +
    # text
    geom_text(data = pred |>
                distinct(process, figure_names, origSiteID, warming, grazing) |>
                left_join(text |>
                            ungroup() |>
                            filter(term == "O"),
                          by = c("figure_names", "trait_trans", "process")),
              aes(x = x_var, y = y_var, hjust = hjust_var, vjust = -2.4, label = term),
              size = 3, colour = "black") +
    geom_text(data = pred |>
                distinct(process, figure_names, origSiteID, warming, grazing) |>
                left_join(text |>
                            ungroup() |>
                            filter(str_detect(term, "W")),
                          by = c("figure_names", "trait_trans", "process")),
              aes(x = x_var, y = y_var, hjust = hjust_var, vjust = -0.8, label = term),
              size = 3, colour = "black") +
    scale_fill_manual(name = "", values = c("grey40", col2[2])) +
    labs(y = "Mean trait value", x = "", tag = "a)") +
    facet_wrap(~figure_names , scales = "free_y",
               labeller = label_parsed) +
    theme_bw() +
    theme(legend.position = "top")


  # Grazing only model
  grazing <- pred |>
    #filter(grazing == "Ambient") |>
    ggplot(aes(x = origSiteID, y = mean, fill = grazing)) +
    geom_boxplot() +
    # text
    geom_text(data = pred |>
                distinct(process, figure_names, origSiteID, warming, grazing) |>
                left_join(text |>
                            ungroup() |>
                            filter(term == "O"),
                          by = c("figure_names", "trait_trans", "process")),
              aes(x = x_var, y = y_var, hjust = hjust_var, vjust = -2.4, label = term),
              size = 3, colour = "black") +
    geom_text(data = pred |>
                distinct(process, figure_names, origSiteID, warming, grazing) |>
                left_join(text |>
                            ungroup() |>
                            filter(str_detect(term, "G")),
                          by = c("figure_names", "trait_trans", "process")),
              aes(x = x_var, y = y_var, hjust = hjust_var, vjust = -0.8, label = term),
              size = 3, colour = "black") +
    scale_fill_manual(name = "", values = c(col1[1], col1[2])) +
    labs(y = "Mean trait value", x = "", tag = "b)") +
    facet_wrap(~ figure_names, scales = "free_y",
               labeller = label_parsed) +
    theme_bw() +
    theme(legend.position = "top")

  warming / grazing


  }





# APPENDIX FIGURE
make_GxW_figure <- function(g_trait_models, g_trait_anova, col1){

  # Grazing and warming model
  pred = g_trait_models |>
    unnest(prediction) |>
    select(trait_trans, figure_names, mean:.std.resid) |>
    filter(trait_trans %in% c("sla_cm2_g", "ldmc")) |>
    mutate(figure_names = factor(figure_names,
                                 levels = c("SLA~(cm^2*g^{-1})", "LDMC~(gg^{-1})")),
           origSiteID = factor(origSiteID, levels = c("Sub-alpine", "Alpine")))

  text = g_trait_anova |>
    filter(!str_detect(term, "Residuals"),
           p.value <= 0.05) |>
    filter(trait_trans %in% c("sla_cm2_g", "ldmc")) |>
    mutate(x_var = Inf, y_var = -Inf, hjust_var = 1) |>
    mutate(figure_names = factor(figure_names,
                                 levels = c("SLA~(cm^2*g^{-1})", "LDMC~(gg^{-1})")))

  ggplot(pred, aes(x = grazing, y = mean, fill = warming)) +
    geom_boxplot() +
    # text
    geom_text(data = pred |>
                distinct(figure_names, origSiteID, warming, grazing) |>
                left_join(text |>
                            ungroup() |>
                            filter(!str_detect(term, "G|W")),
                          by = c("figure_names")),
              aes(x = x_var, y = y_var, hjust = hjust_var, vjust = -4, label = term),
              size = 3, colour = "black") +
    geom_text(data = pred |>
                distinct(figure_names, origSiteID, warming, grazing) |>
                left_join(text |>
                            ungroup() |>
                            filter(str_detect(term, "W")),
                          by = c("figure_names")),
              aes(x = x_var, y = y_var, hjust = hjust_var, vjust = -2.4, label = term),
              size = 3, colour = "black") +
    geom_text(data = pred |>
                distinct(figure_names, origSiteID, warming, grazing) |>
                left_join(text |>
                            ungroup() |>
                            filter(str_detect(term, "G")),
                          by = c("figure_names")),
              aes(x = x_var, y = y_var, hjust = hjust_var, vjust = -0.8, label = term),
              size = 3, colour = "black") +
    scale_fill_manual(name = "", values = c("grey40", col2[2])) +
    labs(y = "Mean trait value", x = "", tag = "b)") +
    facet_grid(figure_names ~ origSiteID, scales = "free_y",
               labeller = label_parsed) +
    theme_bw() +
    theme(legend.position = "top")

}


make_single_trait_appendix_figure <- function(g_trait_models, g_trait_anova, col2){

  pred = g_trait_models |>
    unnest(prediction) |>
    select(trait_trans, figure_names, mean:.std.resid) |>
    mutate(figure_names = factor(figure_names,
                                 levels = c("Plant~height~(cm)", "Leaf~dry~mass~(g)", "Leaf~area~(cm^2)", "Leaf~thickness~(mm)", "SLA~(cm^2*g^{-1})", "LDMC~(gg^{-1})")),
           origSiteID = factor(origSiteID, levels = c("Sub-alpine", "Alpine")))

  text = g_trait_anova |>
    filter(!str_detect(term, "Residuals"),
           p.value <= 0.05) |>
    mutate(x_var = Inf, y_var = -Inf, hjust_var = 1) |>
    mutate(figure_names = factor(figure_names,
                                 levels = c("Plant~height~(cm)", "Leaf~dry~mass~(g)", "Leaf~area~(cm^2)", "Leaf~thickness~(mm)", "SLA~(cm^2*g^{-1})", "LDMC~(gg^{-1})")))


  ggplot(pred, aes(x = warming, y = mean, fill = warming)) +
    geom_boxplot() +
    # text
    geom_text(data = pred |>
                distinct(figure_names, origSiteID, warming, grazing) |>
                left_join(text |>
                            ungroup() |>
                            filter(term == "O"),
                          by = c("figure_names")),
              aes(x = x_var, y = y_var, hjust = hjust_var, vjust = -4, label = term),
              size = 3, colour = "black") +
    geom_text(data = pred |>
                distinct(figure_names, origSiteID, warming, grazing) |>
                left_join(text |>
                            ungroup() |>
                            filter(term == "W"),
                          by = c("figure_names")),
              aes(x = x_var, y = y_var, hjust = hjust_var, vjust = -2.4, label = term),
              size = 3, colour = "black") +
    geom_text(data = pred |>
                distinct(figure_names, origSiteID, warming, grazing) |>
                left_join(text |>
                            ungroup() |>
                            filter(term == "WxO"),
                          by = c("figure_names")),
              aes(x = x_var, y = y_var, hjust = hjust_var, vjust = -0.8, label = term),
              size = 3, colour = "black") +
    scale_fill_manual(name = "", values = c("grey40", col2[2])) +
    labs(y = "Mean trait value", x = "") +
    facet_grid(figure_names ~ origSiteID, scales = "free_y",
               labeller = label_parsed) +
    theme_bw() +
    theme(legend.position = "top")

}



# w <- text |>
#   filter(!figure_names %in% c("Height~(cm)", "Dry~mass~(g)", "Area~(cm^2)")) |>
#   ungroup() |>
#   filter(!(figure_names == "SLA~(cm^2*g^{-1})" & term %in% c("W", "O")))
#
# warming <- dat |>
#   filter(!figure_names %in% c("Height~(cm)", "Dry~mass~(g)", "Area~(cm^2)")) |>
#   ggplot(aes(x = warming, y = mean, fill = origSiteID)) +
#   geom_boxplot() +
#   # text
#   # geom_text(data = pred |>
#   #             filter(figure_names %in% c("Height~(cm)", "Dry~mass~(g)", "Area~(cm^2)")) |>
#   #             distinct(figure_names, origSiteID, warming, Nitrogen_log) |>
#   #             left_join(w |> group_by(figure_names) |>
#   #                         slice(3),
#   #                       by = c("figure_names")),
#   #           aes(x = x_var, y = y_var, hjust = hjust_var, vjust = -4, label = term),
#   #           size = 3, colour = "black") +
#   geom_text(data = pred |>
#               filter(!figure_names %in% c("Height~(cm)", "Dry~mass~(g)", "Area~(cm^2)")) |>
#               distinct(figure_names, origSiteID, warming, Nitrogen_log) |>
#               left_join(w |> group_by(figure_names) |>
#                           slice(2),
#                         by = c("figure_names")),
#             aes(x = x_var, y = y_var, hjust = hjust_var, vjust = -2.4, label = term),
#             size = 3, colour = "black") +
#   geom_text(data = pred |>
#               filter(!figure_names %in% c("Height~(cm)", "Dry~mass~(g)", "Area~(cm^2)")) |>
#               distinct(figure_names, origSiteID, warming, Nitrogen_log) |>
#               left_join(w |> group_by(figure_names) |>
#                           slice(1),
#                         by = c("figure_names")),
#             aes(x = x_var, y = y_var, hjust = hjust_var, vjust = -0.8, label = term),
#             size = 3, colour = "black") +
#   scale_fill_manual(name = "", values = c(col_palette[3], col_palette[4])) +
#   labs(x = "",
#        y = "Mean trait value",
#        tag = "b)") +
#   facet_wrap(~ figure_names, scales = "free", labeller = label_parsed) +
#   theme_bw()
