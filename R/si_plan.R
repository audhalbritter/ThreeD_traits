# Supporting Information plan
si_plan <- list(


  # CHECK TRAIT CORRELATIONS

   tar_target(
    name = trait_correlation_plot,
    command = {

      # make correlation table of all traits
      corr_table_g <- trait_mean %>%
        fancy_trait_name_dictionary(.) |>
        ungroup() |>
        select(origSiteID:mean, trait_fancy, -trait_trans) |>
        pivot_wider(names_from = trait_fancy, values_from = mean)  |>
        select(`Height cm`:`SLA cm2/g`) %>%
        cor(.)

      ggcorrplot(corr_table_g,
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
    name = imputation_plot,
    command = {

      dd <- trait_impute %>% 
        ungroup() |> 
        distinct(origSiteID, origBlockID, turfID, warming, grazing) |> 
        arrange(origSiteID, origBlockID, turfID) |> 
        mutate(ID = paste0(origSiteID, "_ ", origBlockID, "_", turfID),
               ID2 = paste0(origSiteID, "-", substr(warming, 1,1), "-", substr(grazing, 1,1))) |> 
        select(ID, ID2)

      # print list of treatments
      #cat(paste0('c("', paste(dd$ID2, collapse = '", "'), '")'))

      #check trait coverage
      trait_impute %>% 
        autoplot(., other_col_how = "ignore") +
        scale_fill_manual(labels = c("turfID", "blockID", "siteID", "global"),
                          values = c("#56B4E9", "#009E73", "#E69F00", "#D55E00")) +
        scale_y_continuous(breaks = c(0, 0.5, 1)) +
        # scale_x_discrete(breaks = c("Alpine_ 1_3 WN1C 85", "Alpine_ 1_4 AN1C 4", "Alpine_ 1_7 AN1N 7", "Alpine_ 1_8 WN1N 87", "Alpine_ 4_27 AN3C 27", "Alpine_ 4_29 WN3C 106", "Alpine_ 4_31 AN3N 31", "Alpine_ 4_32 WN3N 112", "Alpine_ 10_74 WN2C 155", "Alpine_ 10_77 AN2C 77", "Alpine_ 10_79 AN2N 79", "Alpine_ 10_80 WN2N 159", "Sub-alpine_ 1_81 AN1C 81", "Sub-alpine_ 1_85 WN1C 162", "Sub-alpine_ 1_87 WN1N 164", "Sub-alpine_ 1_88 AN1N 88", "Sub-alpine_ 4_105 WN3C 173", "Sub-alpine_ 4_109 AN3C 109", "Sub-alpine_ 4_111 AN3N 111", "Sub-alpine_ 4_112 WN3N 176", "Sub-alpine_ 10_156 AN2C 156", "Sub-alpine_ 10_158 WN2C 199", "Sub-alpine_ 10_159 WN2N 200", "Sub-alpine_ 10_160 AN2N 160"),
        #                  labels = c("Alpine-W-0-U", "Alpine-A-0-U", "Alpine-A-0-G", "Alpine-W-0-G", "Alpine-A-0-U", "Alpine-W-0-U", "Alpine-A-0-G", "Alpine-W-0-G", "Alpine-W-0-U", "Alpine-A-0-U", "Alpine-A-0-G", "Alpine-W-0-G", "Sub-alpine-A-0-U", "Sub-alpine-W-0-U", "Sub-alpine-W-0-G", "Sub-alpine-A-0-G", "Sub-alpine-W-0-U", "Sub-alpine-A-0-U", "Sub-alpine-A-0-G", "Sub-alpine-W-0-G", "Sub-alpine-A-0-U", "Sub-alpine-W-0-U", "Sub-alpine-W-0-G", "Sub-alpine-A-0-G")) +

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
  ),

  # climate figure
  tar_target(
    name = climate_figure,
    command = daily_temp |>
      mutate(month = month(date),
             year = year(date)) |>
      filter(month %in% c(5, 6, 7, 8, 9),
             year %in% c(2020, 2021, 2022),
             # only controls
             Nlevel %in% c(1, 2, 3),
             grazing == "Control") %>%
      ggplot(aes(x = as.factor(year), y = value, fill = warming)) +
      geom_violin(draw_quantiles = 0.5) +
      scale_fill_manual(name = "", values = c("grey40", warming_palette[2])) +
      labs(x = "Year",
           y = "Soil moisture (propotion) - Temperature (°C)") +
      facet_grid(variable ~ origSiteID, scales = "free_y") +
      theme_bw()
  )

)
