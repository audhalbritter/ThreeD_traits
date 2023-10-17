# Supporting Information plan
si_plan <- list(


  # CHECK TRAIT CORRELATIONS

  tar_target(
    name = trait_correlation_plot,
    command = {

      # make correlation table of all traits
      corr_table <- trait_mean %>%
        fancy_trait_name_dictionary(.) |>
        ungroup() |>
        select(siteID:mean, trait_fancy, -trait_trans) |>
        pivot_wider(names_from = trait_fancy, values_from = mean)  |>
        select(`Height cm`:`SLA cm2/g`) %>%
        cor(.)

      ggcorrplot(corr_table,
                 lab = TRUE,
                 ggtheme = ggplot2::theme_bw(),
                 colors = c("#6D9EC1", "white", "#E46726"))

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
    name = imputation_plot,
    command = {

      trait_names <- c(
        "plant_height_log_cm" = "Height cm",
        "dry_mass_g_log" = "Dry mass g",
        "leaf_area_log_cm2" = "Area cm2",
        "leaf_thickness_log_mm" = "Thickness mm",
        "ldmc" = "LDMC",
        "sla_cm2_g" = "SLA cm2/g")

      #check trait coverage
      imputation_plot <- trait_impute %>%
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
  )

)
