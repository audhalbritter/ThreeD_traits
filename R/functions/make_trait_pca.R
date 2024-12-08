## TRAITS (PCA)

make_trait_pca <- function(trait_mean){

  set.seed(32)

  # make wide trait table
  cwm_fat <- trait_mean %>%
    # remove nutrient ratio traits
    filter(!trait_trans %in% c("CN_ratio", "NP_ratio")) |>
    select(trait_trans, turfID, origSiteID, warming, grazing, Namount_kg_ha_y, Nitrogen_log, mean) %>%
    #select(Gradient:mean, Mean_elevation, GS, SoilMoisture, SoilTemperature) %>%
    pivot_wider(names_from = "trait_trans", values_from = "mean") %>%
    ungroup()

  # environemntal variables
  # env <- cwm_fat |>
  #   select(SoilMoisture, SoilTemperature)

  pca_output <- cwm_fat %>%
    select(-(turfID:Nitrogen_log)) %>%
    rda(scale = TRUE, center = TRUE)

  # envfit
  # env_fit <- envfit(pca_output, env)
  # env_out <- scores(env_fit, "vectors") |>
  #   as_tibble() |>
  #   mutate(label = c("Moisture", "Temperature"),
  #          class = "Environment",
  #          figure_names = c("Moisture", "Temperature"))

  pca_sites <- bind_cols(
    cwm_fat %>%
      select(turfID:Nitrogen_log),
    fortify(pca_output, display = "sites")
  )

  # arrows
  pca_traits <- fortify(pca_output, display = "species") %>%
    mutate(trait_trans = label) %>%
    fancy_trait_name_dictionary() #|>
    # add environmental variables
    #bind_rows(env_out) |>
    # mutate(class = as.character(class),
    #        class = factor(class, levels = c("Size", "Leaf economics", "Isotopes", "Environment")))

  # permutation test
  #trait_mean <- g_trait_mean
  raw <- cwm_fat |> select(-(turfID:Nitrogen_log))
  # meta data
  meta <- cwm_fat|> select(turfID:Nitrogen_log) |>
    mutate(origSiteID = factor(origSiteID))

  # adonis test
  adonis_result <- adonis2(raw ~ warming * Nitrogen_log + warming * origSiteID + Nitrogen_log * origSiteID + warming * grazing + grazing * origSiteID, data = meta, permutations = 999, method = "euclidean", by = "terms")

  # if(meta %>% distinct(grazing) %>% count() == 2){
  #   adonis_result <- adonis2(raw ~ warming * grazing + warming * origSiteID + grazing * origSiteID, data = meta, permutations = 999, method = "euclidean")
  # } else {
  #   adonis_result <- adonis2(raw ~ warming * Nitrogen_log + warming * origSiteID + Nitrogen_log * origSiteID, data = meta, permutations = 999, method = "euclidean")
  # }

  # rda
  rda_result <- rda(raw ~ warming * Nitrogen_log + warming * origSiteID + Nitrogen_log * origSiteID + warming * grazing + grazing * origSiteID, data = meta, scale = TRUE, center = TRUE)
  #anova(rda, permutations = 9999, by = "term")

  # RsquareAdj(rda)
  # anova(rda, permutations=9999, by = "term")
  # anova.cca(rda, step = 1000)
  # anova.cca(rda, step = 1000, by = "term")

  outputList <- list(pca_sites, pca_traits, pca_output, adonis_result, rda_result)

}

# ordiplot(g.rda, scaling = 2, type = "text")
# fortify(g.rda)


make_pca_plot <- function(trait_pca, col2){

  e_B1 <- eigenvals(trait_pca[[3]])/sum(eigenvals(trait_pca[[3]]))

  trait_pca[[1]] |>
    mutate(site_graze = paste(origSiteID, grazing, sep = "_"),
           site_graze = case_match(site_graze,
                                   "Alpine_Grazed" ~ "Alpine grazed",
                                   "Alpine_Ungrazed" ~ "Alpine ungrazed",
                                   "Sub-alpine_Grazed" ~ "Sub-alpine grazed",
                                   "Sub-alpine_Ungrazed" ~ "Sub-alpine ungrazed",
                                   .default = site_graze)) |>
    ggplot(aes(x = PC1, y = PC2, colour = warming, linetype = origSiteID, fill = warming)) +
    geom_convexhull(alpha = 0.2) +
    geom_point(aes(shape = site_graze), size = 2) +
    geom_segment(data = trait_pca[[2]],
                 aes(x = 0, y = 0, xend = PC1, yend = PC2),
                 arrow = arrow(length = unit(0.2, "cm")),
                 inherit.aes = FALSE, colour = "grey60") +
    geom_text(data = trait_pca[[2]] |>
                mutate(figure_names = case_match(figure_names,
                                                 "Plant~height~(cm)" ~ "Height~(cm)",
                                                 "Leaf~dry~mass~(g)" ~ "Dry~mass~(g)",
                                                 "Leaf~area~(cm^2)" ~ "Area~(cm^2)",
                                                 "Leaf~thickness~(mm)" ~ "Thickness~(mm)",
                                                 .default = figure_names),
                       PC2 = case_when(label == "leaf_area_cm2_log" ~ -0.2,
                                       label == "leaf~dry~mass~(g)" ~ -0.1,
                                       label == "sla_cm2_g" ~ -1.1,
                                       TRUE ~ PC2),
                       PC1 = case_when(label == "leaf_thickness_mm_log" ~ 0.7,
                                       TRUE ~ PC1)),
              aes(x = PC1 + 0.3, y = PC2 + 0.2, label = figure_names),
              size = 3,
              inherit.aes = FALSE,
              show.legend = FALSE, parse = TRUE) +
    coord_equal() +
    scale_fill_manual(name = "Warming", values = c("grey40", col2[2])) +
    scale_colour_manual(name = "Warming", values = c("grey40", col2[2])) +
    scale_shape_manual(name = "Origin and grazing", values = c(17, 2, 16, 1)) +
    scale_linetype_manual(name = "Origin and grazing", values = c("solid", "dashed", "solid", "dashed")) +
    labs(x = glue("PCA1 ({round(e_B1[1] * 100, 1)}%)"),
         y = glue("PCA2 ({round(e_B1[2] * 100, 1)}%)")) +
    theme_bw()

}


make_full_pca_plot <- function(trait_pca, col2){

  e_B1 <- eigenvals(trait_pca[[3]])/sum(eigenvals(trait_pca[[3]]))

  trait_pca[[1]] |>
    mutate(site_graze = paste(origSiteID, grazing, sep = "_"),
           site_graze = case_match(site_graze,
                                   "Alpine_Grazed" ~ "Alpine grazed",
                                   "Alpine_Ungrazed" ~ "Alpine ungrazed",
                                   "Sub-alpine_Grazed" ~ "Sub-alpine grazed",
                                   "Sub-alpine_Ungrazed" ~ "Sub-alpine ungrazed",
                                   .default = site_graze)) |>
    ggplot(aes(x = PC1, y = PC2, colour = warming, linetype = origSiteID, fill = warming)) +
    geom_convexhull(alpha = 0.3) +
    geom_point(aes(shape = site_graze, size = Namount_kg_ha_y)) +
    geom_segment(data = trait_pca[[2]],
                 aes(x = 0, y = 0, xend = PC1, yend = PC2),
                 arrow = arrow(length = unit(0.2, "cm")),
                 inherit.aes = FALSE, colour = "grey60") +
    geom_text(data = trait_pca[[2]] |>
                mutate(figure_names = case_match(figure_names,
                                                 "Plant~height~(cm)" ~ "Height~(cm)",
                                                 "Leaf~dry~mass~(g)" ~ "Dry~mass~(g)",
                                                 "Leaf~area~(cm^2)" ~ "Area~(cm^2)",
                                                 "Leaf~thickness~(mm)" ~ "Thickness~(mm)",
                                                 .default = figure_names),
                       PC2 = case_when(label == "leaf_area_cm2_log" ~ -0.2,
                                       label == "leaf~dry~mass~(g)" ~ -0.1,
                                       label == "sla_cm2_g" ~ -1.1,
                                       TRUE ~ PC2),
                       PC1 = case_when(label == "leaf_thickness_mm_log" ~ 0.7,
                                       TRUE ~ PC1)),
              aes(x = PC1 + 0.3, y = PC2 + 0.2, label = figure_names),
              size = 3,
              inherit.aes = FALSE,
              show.legend = FALSE, parse = TRUE) +
    coord_equal() +
    guides(size = guide_legend(title = "Nitrogen addition")) +
    scale_fill_manual(name = "Warming", values = c("grey40", col2[2])) +
    scale_colour_manual(name = "Warming", values = c("grey40", col2[2])) +
    scale_shape_manual(name = "Origin and grazing", values = c(17, 2, 16, 1)) +
    scale_linetype_manual(name = "Origin and grazing", values = c("solid", "dashed", "solid", "dashed")) +
    labs(x = glue("PCA1 ({round(e_B1[1] * 100, 1)}%)"),
         y = glue("PCA2 ({round(e_B1[2] * 100, 1)}%)")) +
    theme_bw()

}





make_pca_plot2 <- function(g_trait_pca, n_trait_pca, col2){

  trait_pca <- g_trait_pca

  # both gradients
  e_B1 <- eigenvals(trait_pca[[3]])/sum(eigenvals(trait_pca[[3]]))

  pcg1 <- trait_pca[[1]] %>%
    mutate(site_grazing = paste0(substr(origSiteID, 1, 1), "_", substr(grazing, 1, 1)),
           site_grazing = factor(site_grazing, levels = c("A_C", "A_N", "S_C", "S_N"))) |>
    ggplot(aes(x = PC1, y = PC2, colour = warming, shape = site_grazing)) +
    geom_point(size = 2) +
    coord_equal() +
    #stat_ellipse(aes(colour = Mean_elevation)) +
    scale_colour_manual(name = "", values = c(col2, col2[2])) +
    scale_shape_manual(name = "Site and origin", values = c(17, 2, 16, 1),
                       labels = c("Alpine exclosure", "Alpine grazing", "Sub-alpine exclosure", "Sub-alpine grazing")) +
    #scale_colour_viridis_c(end = 0.8, option = "inferno", direction = -1, name = "Elevation m a.s.l.", limits = c(range[1], range[2])) +
    labs(x = glue("PCA1 ({round(e_B1[1] * 100, 1)}%)"),
         y = glue("PCA2 ({round(e_B1[2] * 100, 1)}%)"),
         tag = "a)") +
    theme_bw()

  arrowsg1 <- trait_pca[[1]] %>%
    ggplot(aes(x = PC1, y = PC2)) +
    geom_segment(data = trait_pca[[2]],
                 aes(x = 0, y = 0, xend = PC1, yend = PC2),
                 arrow = arrow(length = unit(0.2, "cm")),
                 inherit.aes = FALSE) +
    geom_text(data = trait_pca[[2]] |>
                mutate(PC2 = case_when(label == "leaf_area_cm2_log" ~ 0,
                                       label == "dry_mass_g_log" ~ -0.12,
                                       label == "plant_height_cm_log" ~ -0.3,
                                       TRUE ~ PC2)),
              aes(x = PC1+0.3, y = PC2, label = figure_names),
              size = 2.5,
              inherit.aes = FALSE,
              show.legend = FALSE, parse = TRUE) +
    coord_equal() +
    labs(x = "PCA1", y = "PCA2",
         tag = "c)") +
    scale_x_continuous(expand = c(.2, 0))  +
    #scale_linetype_manual(name = "", values = c("solid", "dashed", "solid", "solid")) +
    #scale_colour_manual(name = "", values = c("black", "grey40", "grey70", "cornflowerblue")) +
    theme_bw()



  trait_pca <- n_trait_pca

  # both gradients
  e_B2 <- eigenvals(trait_pca[[3]])/sum(eigenvals(trait_pca[[3]]))

  pcan2 <- trait_pca[[1]] %>%
    mutate(site_warming = paste0(substr(origSiteID, 1, 1), "_", substr(warming, 1, 1)),
           site_warming = factor(site_warming, levels = c("A_A", "A_W", "S_A", "S_W"))) |>
    ggplot(aes(x = PC1, y = PC2, colour = warming, shape = origSiteID, alpha = Nitrogen_log)) +
    geom_point(size = 2) +
    coord_equal() +
    scale_shape_manual(name = "", values = c(17, 16)) +
    scale_colour_manual(name = "", values = c("grey40", col2[2])) +
    scale_alpha(name = "log(nitrogen)", range = c(0.3, 1)) +
    #scale_colour_viridis_c(end = 0.8, option = "inferno", direction = -1) +
    labs(x = glue("PCA1 ({round(e_B2[1] * 100, 1)}%)"),
         y = glue("PCA2 ({round(e_B2[2] * 100, 1)}%)"),
         tag = "b)") +
    theme_bw()

  arrowsn2 <- trait_pca[[1]] %>%
    ggplot(aes(x = PC1, y = PC2)) +
    geom_segment(data = trait_pca[[2]],
                 aes(x = 0, y = 0, xend = PC1, yend = PC2),
                 arrow = arrow(length = unit(0.2, "cm")),
                 inherit.aes = FALSE) +
    geom_text(data = trait_pca[[2]] |>
                mutate(PC2 = case_when(label == "leaf_area_cm2_log" ~ 0.05,
                                       label == "dry_mass_g_log" ~ 0.2,
                                       label == "plant_height_cm_log" ~ -0.15,
                                       TRUE ~ PC2)),
              aes(x = PC1+0.3, y = PC2, label = figure_names),
              size = 2.5,
              inherit.aes = FALSE,
              show.legend = FALSE, parse = TRUE) +
    coord_equal() +
    labs(x = "PCA1", y = "PCA2",
         tag = "d)") +
    scale_x_continuous(expand = c(.2, 0)) +
    theme_bw()

  (pcg1 + pcan2) / (arrowsg1 + arrowsn2) + plot_layout(guides = "collect")

}
