# figure plan
figure_plan <- list(

  # colour palette
  tar_target(
    name = warming_palette,
    command = wes_palette("GrandBudapest1")
    #c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
    ),

  tar_target(
    name = habitat_palette,
    command = c("#E69F00", "#56B4E9")
  ),

  # trait means
  tar_target(
    name = nitrogen_figure,
    command = make_nitrogen_figure(n_trait_output, n_trait_anova, col2 = warming_palette)
      ),

  tar_target(
    name = grazing_figure,
    command = make_grazing_figure(g_trait_models, g_trait_anova, col1 = habitat_palette, col2 = warming_palette)
  ),

  tar_target(
    name = itv_figure,
    command = make_ITV_plot_origin(variance_part)
  )

  # multivariate trait pca
  # tar_target(
  #   name = trait_pca_figure,
  #   command = make_pca_plot(g_trait_pca, n_trait_pca, col_palette)
  # )

)
