# data curation plan
tranformation_plan <- list(

  # climate data
  tar_target(
    name = daily_temp,
    command = climate_raw %>%
      mutate(date = dmy(format(date_time, "%d.%b.%Y"))) |>
      # daily temperature
      group_by(date, destSiteID, destBlockID, destPlotID, origSiteID, origBlockID, warming, Nlevel, grazing, Namount_kg_ha_y) %>%
      summarise(air_temperature = mean(air_temperature, na.rm = TRUE),
                ground_temperature = mean(ground_temperature, na.rm = TRUE),
                soil_temperature = mean(soil_temperature, na.rm = TRUE),
                soilmoisture = mean(soilmoisture, na.rm = TRUE)) |>
      pivot_longer(cols = c(air_temperature:soilmoisture), names_to = "variable", values_to = "value") |>
      filter(!is.na(value)) |>
      # prettify
      mutate(warming = recode(warming, A = "Ambient", W = "Warming"),
             grazing = recode(grazing, C = "Control", M = "Medium", I = "Intensive"),
             grazing = factor(grazing, levels = c("Control", "Medium", "Intensive")),
             origSiteID = recode(origSiteID, Joa = "Sub-alpine", Lia = "Alpine"),
             variable = recode(variable, air_temperature = "air", ground_temperature = "ground", soil_temperature = "soil"))

  ),

  # summer temp 2020-2022
  tar_target(
    name = summer_T,
    command = daily_temp |>
      mutate(month = month(date),
             year = year(date)) |>
      filter(month %in% c(5, 6, 7, 8, 9),
             year %in% c(2020, 2021, 2022),
             # only controls
             Nlevel %in% c(1, 2, 3),
             grazing == "Control") %>%
      group_by(variable, warming, origSiteID) %>%
      summarise(mean = mean(value),
                se = sd(value)/sqrt(n())) |>
      pivot_wider(names_from = warming, values_from = c(mean, se)) |>
      mutate(diff = round((mean_Warming - mean_Ambient), 2),
             se_diff = round((sqrt(se_Warming^2+se_Ambient^2)), 3))
  ),


  # prep cover
  tar_target(
    name = community,
    command = clean_community(cover_raw, metaTurfID, sp_list)
  ),


  # prep traits
  tar_target(
    name = traits,
    command = clean_traits(trait_raw)

  ),


  # BOOTSTRAPPING
  # make trait impute
  # warming x grazing data
  tar_target(
    name = wg_trait_impute,
    command = make_trait_impute(community |>
                                  filter(Namount_kg_ha_y == 0),
                                traits |>
                                  filter(Namount_kg_ha_y == 0))

  ),

  # warming x grazing data without ITV
  tar_target(
    name = wg_trait_null_impute,
    command = make_trait_null_impute(community |>
                                  filter(Namount_kg_ha_y == 0),
                                traits |>
                                  filter(Namount_kg_ha_y == 0))

  ),

  # warming x N
  tar_target(
    name = wn_trait_impute,
    command = make_trait_impute(community |>
                                  filter(grazing == "Ungrazed"),
                                traits |>
                                  filter(grazing == "Ungrazed"))

  ),

  # warming x N witout ITV
  tar_target(
    name = wn_trait_null_impute,
    command = make_trait_null_impute(community |>
                                  filter(grazing == "Ungrazed"),
                                traits |>
                                  filter(grazing == "Ungrazed"))

  ),

  # bootstrap and summarise
  # tar_target(
  #   name = g_trait_mean,
  #   command = make_bootstrapping(wg_trait_impute)
  # ),

  tar_target(
    name = g_trait_mean,
    command = make_bootstrapping(wg_trait_impute, wg_trait_null_impute)
  ),

  # tar_target(
  #   name = n_trait_mean,
  #   command = make_bootstrapping(wn_trait_impute)
  # ),

  tar_target(
    name = n_trait_mean,
    command = make_bootstrapping(wn_trait_impute, wn_trait_null_impute)
  )

  # multivariate functional diversity
  # tar_target(
  #   name = multivariate_traits,
  #   command = make_multi_trait_impute(community |>
  #                                       filter(Namount_kg_ha_y == 0),
  #                                     traits |>
  #                                       filter(Namount_kg_ha_y == 0))
  # )

)
