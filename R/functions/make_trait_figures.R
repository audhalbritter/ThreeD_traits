# Trait figures

make_single_trait_figure <- function(g_trait_models, g_trait_anova, n_trait_output, n_trait_anova, col1, col2){

  # Grazing and warming model
  pred = g_trait_models |>
    unnest(prediction) |>
    select(trait_trans, figure_names, mean:.std.resid) |>
    filter(trait_trans %in% c("leaf_thickness_mm_log", "sla_cm2_g", "ldmc")) |>
    mutate(figure_names = factor(figure_names,
                                 levels = c("Thickness~(mm)", "SLA~(cm^2*g^{-1})", "LDMC")))

  text = g_trait_anova |>
    filter(!str_detect(term, "Residuals"),
           p.value <= 0.05,
           str_detect(term, "x")) |>
    filter(trait_trans %in% c("leaf_thickness_mm_log", "sla_cm2_g", "ldmc")) |>
    mutate(x_var = Inf, y_var = -Inf, hjust_var = 1) |>
    mutate(figure_names = factor(figure_names,
                                 levels = c("Thickness~(mm)", "SLA~(cm^2*g^{-1})", "LDMC")))

  grazing <- ggplot(pred, aes(x = warming, y = mean, fill = origSiteID)) +
    geom_boxplot() +
    # text
    geom_text(data = pred |>
                distinct(figure_names, origSiteID, warming, grazing) |>
                left_join(text |>
                            ungroup() |>
                            filter(!str_detect(term, "G")) |>
                            group_by(figure_names) |>
                            slice(3),
                          by = c("figure_names")),
              aes(x = x_var, y = y_var, hjust = hjust_var, vjust = -4, label = term),
              size = 3, colour = "black") +
    geom_text(data = pred |>
                distinct(figure_names, origSiteID, warming, grazing) |>
                left_join(text |>
                            ungroup() |>
                            group_by(figure_names) |>
                            slice(2),
                          by = c("figure_names")),
              aes(x = x_var, y = y_var, hjust = hjust_var, vjust = -2.4, label = term),
              size = 3, colour = "black") +
    geom_text(data = pred |>
                distinct(figure_names, origSiteID, warming, grazing) |>
                left_join(text |>
                            ungroup() |>
                            group_by(figure_names) |>
                            slice(1),
                          by = c("figure_names")),
              aes(x = x_var, y = y_var, hjust = hjust_var, vjust = -0.8, label = term),
              size = 3, colour = "black") +
    scale_fill_manual(name = "", values = c(col1[2], col1[1])) +
    labs(y = "Mean trait value", x = "", tag = "b)") +
    facet_grid(figure_names ~ grazing, scales = "free_y",
                labeller = label_parsed) +
    theme_bw()


  # Nitrogen and warming model
  pred = n_trait_output |>
    # merge data and prediction
    mutate(output = map2(.x = newdata, .y = prediction, ~ bind_cols(.x, .y))) |>
    select(trait_trans, figure_names, names, output) |>
    unnest(output) |>
    rename(prediction = fit) |>
    mutate(figure_names = factor(figure_names,
                                 levels = c("Height~(cm)", "Dry~mass~(g)", "Area~(cm^2)", "Thickness~(mm)", "SLA~(cm^2*g^{-1})", "LDMC")))

  dat = n_trait_output |>
    unnest(data) |>
    select(trait_trans:mean) |>
    mutate(figure_names = factor(figure_names,
                                 levels = c("Height~(cm)", "Dry~mass~(g)", "Area~(cm^2)", "Thickness~(mm)", "SLA~(cm^2*g^{-1})", "LDMC")))

  text = n_trait_anova |>
    filter(!str_detect(term, "Residuals"),
           p.value <= 0.05) |>
    mutate(x_var = Inf, y_var = -Inf, hjust_var = 1) |>
    mutate(figure_names = factor(figure_names,
                                 levels = c("Height~(cm)", "Dry~mass~(g)", "Area~(cm^2)", "Thickness~(mm)", "SLA~(cm^2*g^{-1})", "LDMC")))

  n <- text |>
    filter(figure_names %in% c("Height~(cm)", "Dry~mass~(g)", "Area~(cm^2)")) |>
    ungroup() |>
    filter(str_detect(term, "x"))

  nitrogen <- dat |>
    filter(figure_names %in% c("Height~(cm)", "Dry~mass~(g)", "Area~(cm^2)")) |>
    ggplot(aes(x = Nitrogen_log, y = mean, colour = warming, shape = origSiteID, linetype = origSiteID)) +
    geom_ribbon(data = pred |>
                  filter(figure_names %in% c("Height~(cm)", "Dry~mass~(g)", "Area~(cm^2)"))
                , aes(y = prediction, ymin = lwr,
                      ymax = upr,
                      fill = warming),
                alpha = 0.2,
                linetype = 0) +
    geom_point() +
    geom_line(data = pred |> filter(figure_names %in% c("Height~(cm)", "Dry~mass~(g)", "Area~(cm^2)")),
              mapping = aes(y = prediction)) +
    # text
    geom_text(data = pred |>
                filter(figure_names %in% c("Height~(cm)", "Dry~mass~(g)", "Area~(cm^2)")) |>
                distinct(figure_names, origSiteID, warming, Nitrogen_log) |>
                left_join(n |> group_by(figure_names) |>
                            slice(2),
                          by = c("figure_names")),
              aes(x = x_var, y = y_var, hjust = hjust_var, vjust = -2.4, label = term),
              size = 3, colour = "black") +
    geom_text(data = pred |>
                filter(figure_names %in% c("Height~(cm)", "Dry~mass~(g)", "Area~(cm^2)")) |>
                distinct(figure_names, origSiteID, warming, Nitrogen_log) |>
                left_join(n |> group_by(figure_names) |>
                            slice(1),
                          by = c("figure_names")),
              aes(x = x_var, y = y_var, hjust = hjust_var, vjust = -0.8, label = term),
              size = 3, colour = "black") +
    # change labels to real values
    scale_x_continuous(breaks = c(log(1), log(5), log(25), log(150)), labels = c(1, 5, 25, 150)) +
    scale_colour_manual(name = "", values = c("grey40", col2[2])) +
    scale_fill_manual(name = "", values = c("grey40", col2[2])) +
    scale_shape_manual(name = "", values = c(17, 16)) +
    scale_linetype_manual(name = "", values = c("solid", "dashed")) +
    labs(x = bquote(log(Nitrogen)~kg~ha^-1~y^-1),
         y = "Mean trait value",
         tag = "a)") +
    facet_wrap(~ figure_names, scales = "free", labeller = label_parsed) +
    theme_bw()

  (nitrogen / grazing) + plot_layout(heights = c(1.2, 3), guides = 'collect', axes = "collect")


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
