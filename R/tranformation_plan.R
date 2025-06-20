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


  ### TRAIT IMPUTSTION AND BOOTSTRAPPING

  # trait impute
  tar_target(
    name = trait_impute,
    command = make_trait_impute(community, traits)
  ),

  # trait imputation coverage
  tar_target(
    name = trait_coverage,
    command = fortify_filled_trait(trait_impute) |>
      ungroup() |>
      #filter(Trait == "CN_ratio") |>
      complete(.id, level, trait_trans, fill = list(s = 0)) |>
      filter(level == "turfID") |>
      group_by(trait_trans) |>
      # prob = 0.25 gives 75% of the plots
      # also run prob = 0.5 for 50% of the plots
      summarise(q = quantile(s, prob = 0.25))
  ),

  # trait impute noitv
  tar_target(
    name = trait_null_impute,
    command = make_trait_null_impute(community, traits)
  ),

  # bootstrapping
  tar_target(
    name = trait_mean,
    command = make_bootstrapping(trait_impute, trait_null_impute)
  )


)
