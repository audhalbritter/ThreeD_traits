# figure plan
figure_plan <- list(

  # colour palette
  tar_target(
    name = col_palette,
    command = wes_palette("GrandBudapest1")
  ),

  # trait means
  tar_target(
    name = g_trait_figure,
    command = make_g_trait_figure(g_trait_models, g_trait_anova, col_palette)
      ),

  tar_target(
    name = n_trait_figure,
    command = make_n_trait_figure(n_trait_output, n_trait_anova, col_palette)
  ),

  # multivariate trait pca
  tar_target(
    name = trait_pca_figure,
    command = make_pca_plot(g_trait_pca, n_trait_pca, col_palette)
  )

)
