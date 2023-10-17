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
  tar_target(
    name = trait_impute,
    command = make_trait_impute(community, traits)

  ),

  # bootstrap and summarise
  tar_target(
    name = trait_mean,
    command = make_bootstrapping(trait_impute)

  )

)
