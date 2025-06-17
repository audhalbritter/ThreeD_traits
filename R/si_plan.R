# Supporting Information plan
si_plan <- list(


  # CHECK TRAIT CORRELATIONS

   tar_target(
    name = trait_correlation_plot,
    command = {

      # make correlation table of all traits
      corr_table_g <- g_trait_mean %>%
        fancy_trait_name_dictionary(.) |>
        ungroup() |>
        select(origSiteID:mean, trait_fancy, -trait_trans) |>
        pivot_wider(names_from = trait_fancy, values_from = mean)  |>
        select(`Height cm`:`SLA cm2/g`) %>%
        cor(.)

      g_plot <- ggcorrplot(corr_table_g,
                 lab = TRUE,
                 ggtheme = ggplot2::theme_bw(),
                 colors = c("#6D9EC1", "white", "#E46726"),
                 title = "Warming x grazing")

      # make correlation table of all traits
      corr_table_n <- n_trait_mean %>%
        fancy_trait_name_dictionary(.) |>
        ungroup() |>
        select(origSiteID:mean, trait_fancy, -trait_trans) |>
        pivot_wider(names_from = trait_fancy, values_from = mean)  |>
        select(`Height cm`:`SLA cm2/g`) %>%
        cor(.)

      n_plot <- ggcorrplot(corr_table_n,
                 lab = TRUE,
                 ggtheme = ggplot2::theme_bw(),
                 colors = c("#6D9EC1", "white", "#E46726"),
                 title = "Warming x nitrogen")

      n_plot + g_plot + plot_layout(guides = "collect")

    }
  ),


  # CHECK TRAIT IMPUTATION COVERAGE
  # trait coverage plot (check how much of the community has been sampled)
  # tar_target(
  #   name = trait_coverage,
  #   command = fortify(trait_impute) |>
  #     ungroup() |>
  #     complete(.id, level, trait_trans, fill = list(s = 0)) |>
  #     filter(level == "turfID") |>
  #     group_by(siteID, treatment_comm, trait_trans) |>
  #     # prob = 0.25 gives 75% of the plots
  #     # also run prob = 0.5 for 50% of the plots
  #     summarise(q = quantile(s, prob = 0.25))
  #
  # ),

  # trait imputation plot

  tar_target(
    name = trait_names,
    command = c(
      "plant_height_log_cm" = "Height cm",
      "dry_mass_g_log" = "Dry mass g",
      "leaf_area_log_cm2" = "Area cm2",
      "leaf_thickness_log_mm" = "Thickness mm",
      "ldmc" = "LDMC",
      "sla_cm2_g" = "SLA cm2/g")
  ),

  tar_target(
    name = imputation_plot_g,
    command = {

      #check trait coverage
      wg_trait_impute %>%
        autoplot(., other_col_how = "ignore") +
        scale_fill_manual(labels = c("turfID", "blockID", "siteID", "global"),
                          values = c("#56B4E9", "#009E73", "#E69F00", "#D55E00")) +
        scale_y_continuous(breaks = c(0, 0.5, 1)) +
        # scale_x_discrete(breaks = c("B_1_D", "B_2_D", "B_3_D", "B_4_D", "B_5_D", "C_1_D", "C_2_D", "C_3_D", "C_4_D", "C_5_D", "C_6_D", "C_7_D"),
        #                  labels = c("1", "2", "3", "4", "5", "1", "2", "3", "4", "5", "6", "7")) +

        facet_wrap(~ trait_trans, labeller = labeller(trait_trans = trait_names)) +
        labs(x = "Treatments") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 90))

    }
  ),

  tar_target(
    name = imputation_plot_n,
    command = {

      ids <- wn_trait_impute %>%
        mutate(id = paste(origSiteID, origBlockID, turfID, sep = "_"),
               name = paste(origSiteID, warming, Namount_kg_ha_y, grazing, sep = "_")) |>
        ungroup() |>
        arrange(origSiteID) |>
        distinct(id)

      #check trait coverage
      wn_trait_impute %>%
        autoplot(., other_col_how = "ignore") +
        scale_fill_manual(labels = c("turfID", "blockID", "siteID", "global"),
                          values = c("#56B4E9", "#009E73", "#E69F00", "#D55E00")) +
        scale_y_continuous(breaks = c(0, 0.5, 1)) +
        #scale_x_discrete(labels = c("A W0C" = "Alpine_1_3 WN1C 85")) +
        facet_wrap(~ trait_trans, labeller = labeller(trait_trans = trait_names)) +
        labs(x = "Treatments") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 90))

    }
  ),

  tar_target(
    name = species_list,
    command = traits |>
      distinct(siteID, species) |>
      tidylog::filter(!str_detect(species, "Unknown"),
                      !species %in% c("Carex rupestris", "Carex rupestris cf",
                                      "Carex norvegica cf", "Carex sp")) |>
      left_join(community |>
                  mutate(siteID = case_when(destSiteID == "Joa" ~ "Joasete",
                                            destSiteID == "Vik" ~ "Vikesland",
                                            destSiteID == "Lia" ~ "Liahovden")) |>
                  distinct(siteID, species, cover, functional_group) |>
                  group_by(siteID, species, functional_group)|>
                  summarise(cover = mean(cover)),
                by = c("siteID", "species")) |>
      # prettify and order factors
      mutate(siteID = recode(siteID,
                             "Liahovden" = "Alpine",
                             "Joasete" = "Sub-alpine",
                             "Vikesland" = "Boreal"),
             siteID = factor(siteID, levels = c("Alpine", "Sub-alpine", "Boreal"))) |>
      # fix NA's in functional group
      mutate(functional_group = case_when(species %in% c("Carex norvegica", "Carex atrata", "Carex pilulifera", "Poa alpina", "Deschampsia cespitosa") ~ "graminoid",
                                          species %in% c("Viola tricolor", "Antennaria dioica", "Pyrola norvegica", "Antennaria alpina") ~ "forb",
                                          species == "Salix herbacea" ~ "shrub",
                                          TRUE ~ functional_group),
             functional_group = stringr::str_to_title(functional_group),
             functional_group = factor(functional_group, levels = c("Graminoid", "Forb", "Legume", "Shrub", "Pteridophyte"))) |>
      mutate(cover = round(cover, 1)) |>
      filter(!is.na(cover)) |>
      pivot_wider(names_from = siteID, values_from = cover) |>
      arrange(functional_group, species) |>
      select("Functional Group" = functional_group, Species = species, Boreal, `Sub-alpine`, Alpine)

  ),

  tar_target(
    name = species_list_out,
    command = species_list |>
      gt() |>
    tab_options(
      table.font.size = 12,
      data_row.padding = gt::px(1)
    ) |>
    cols_align(
      align = c("left"),
      columns = "Functional Group"
    )
  )

)
