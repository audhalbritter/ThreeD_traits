# data curation plan
tranformation_plan <- list(

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


  tar_target(
    name = wn_trait_impute,
    command = make_trait_impute(community |>
                                  filter(grazing == "Control"),
                                traits |>
                                  filter(grazing == "Control"))

  ),

  # bootstrap and summarise
  tar_target(
    name = g_trait_mean,
    command = make_bootstrapping(wg_trait_impute)
  ),

  tar_target(
    name = n_trait_mean,
    command = make_bootstrapping(wn_trait_impute)
  )

)
