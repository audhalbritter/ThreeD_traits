#### TRAIT BOOTSTRAPPING ####


make_trait_impute <- function(community, traits){

  #prepare community data
  comm <- community

  #prepare trait data
  trait <- traits |>
    select(siteID, blockID, turfID, warming, grazing, Nlevel, Namount_kg_ha_y, treatment, species, trait_trans, value_trans, origSiteID, destSiteID)

  #set seed for bootstrapping repeatability
  set.seed(2525)
  trait_imp <- trait_fill(comm = comm,
                          traits = trait,
                          scale_hierarchy = c("origSiteID", "blockID", "turfID"),
                          taxon_col = "species",
                          trait_col = "trait_trans",
                          value_col = "value_trans",
                          abundance_col = "cover",
                          treatment_col = c("treatment"),
                          treatment_level = c("origSiteID"),
                          other_col = c("destSiteID", "Nlevel", "warming", "grazing", "Namount_kg_ha_y", "Nitrogen_log"),
                          min_n_in_sample = 2)


  return(trait_imp)
}


make_trait_null_impute <- function(community, traits){
  #prepare community data
  comm <- community

  #prepare trait data
  trait <- traits |>
    select(siteID, blockID, turfID, warming, grazing, Nlevel, Namount_kg_ha_y, treatment, species, trait_trans, value_trans, origSiteID, destSiteID)

  #prepare trait data without intraspecific variation
  trait.null <- trait %>%
    select(siteID, blockID, turfID, warming, grazing, Nlevel, Namount_kg_ha_y, treatment, species, trait_trans, value_trans) %>%
    group_by(species, trait_trans) %>%
    summarise(value_trans = mean(value_trans)) %>%
    right_join(trait, by = c("species", "trait_trans")) %>%
    select(-value_trans.y, "value_trans" = value_trans.x)

  #set seed for bootstrapping reproducibility
  set.seed(2626)
  trait_imp_null <- trait_fill(comm = comm,
                               traits = trait.null,
                               scale_hierarchy = c("origSiteID", "blockID", "turfID"),
                               taxon_col = "species",
                               trait_col = "trait_trans",
                               value_col = "value_trans",
                               abundance_col = "cover",
                               treatment_col = c("treatment"),
                               treatment_level = c("origSiteID"),
                               other_col = c("destSiteID", "Nlevel", "warming", "grazing", "Namount_kg_ha_y", "Nitrogen_log"),
                               min_n_in_sample = 2)

  return(trait_imp_null)

}



#do the bootstrapping
# make_bootstrapping <- function(trait_imp){
#
#   CWM <- trait_np_bootstrap(trait_imp, nrep = 100, sample_size = 200)
#
#   trait_mean <- trait_summarise_boot_moments(CWM) |>
#     ungroup() |>
#     select(-global, -n) %>%
#     fancy_trait_name_dictionary(.) |>
#     # order traits
#     mutate(trait_trans = factor(trait_trans, levels = c("plant_height_cm_log", "dry_mass_g_log", "leaf_area_cm2_log", "leaf_thickness_mm_log", "ldmc", "sla_cm2_g")))
#
#   return(trait_mean)
#
# }

#do the bootstrapping
make_bootstrapping <- function(trait_imp, trait_imp_null){

  # trait_imp <- wg_trait_impute
  # trait_imp_null <- wg_trait_null_impute
  CWM <- trait_np_bootstrap(trait_imp, nrep = 100, sample_size = 200)
  CWM_notiv <- trait_np_bootstrap(trait_imp_null, nrep = 100, sample_size = 200)

  CWM_mean <- trait_summarise_boot_moments(CWM) %>%
    ungroup() |>
    select(global, origSiteID:mean, var, skew, kurt, -n)

  CWM_notiv_mean <- trait_summarise_boot_moments(CWM_notiv) %>%
    ungroup() |>
    select(origSiteID:mean, -n) %>%
    rename("mean_noitv" = "mean")

  traitMean <- CWM_mean %>%
    left_join(CWM_notiv_mean, by = c("origSiteID", "blockID", "turfID", "treatment_comm", "destSiteID", "Nlevel", "warming", "grazing", "Namount_kg_ha_y", "Nitrogen_log", "trait_trans"))

  #prepare bootstrapped trait data for analyses
  traitMean <- traitMean %>%
    fancy_trait_name_dictionary(.) |>
    ungroup() %>%
    mutate(trait_trans = factor(trait_trans, levels = c("plant_height_cm_log", "dry_mass_g_log", "leaf_area_cm2_log", "leaf_thickness_mm_log", "ldmc", "sla_cm2_g")))

  return(traitMean)

}



### multivariate traits

make_multi_trait_impute <- function(community, traits){

  write_csv(community, file = "community.csv")
  write_csv(traits, file = "traits.csv")

  community <- read_csv("community.csv")
  traits <- read_csv("traits.csv")
  #prepare community data
  comm <- community |>
    filter(turfID %in% c("81 AN1C 81", "85 WN1C 162", "87 WN1N 164")) |>
    filter(Namount_kg_ha_y == 0) |>
    select(-functional_group, -remark, -origBlockID, -origPlotID, -destBlockID, -destPlotID)

  #prepare trait data
  trait <- traits |>
    filter(turfID %in% c("81 AN1C 81", "85 WN1C 162", "87 WN1N 164")) |>
    filter(Namount_kg_ha_y == 0) |>

    select(ID, siteID, blockID, turfID, warming, grazing, Nlevel, Namount_kg_ha_y, treatment, species, trait_trans, value_trans, origSiteID, destSiteID)

  #set seed for bootstrapping repeatability
  set.seed(2525)

  multivariate_traits <- trait_fill(
    comm = comm,
    traits = trait,
    scale_hierarchy = c("origSiteID", "blockID", "turfID"),
    taxon_col = "species",
    value_col = "value_trans",
    trait_col = "trait_trans",
    abundance_col = "cover",
    treatment_col = c("treatment"),
    treatment_level = c("origSiteID"),
    other_col = c("destSiteID", "Nlevel", "warming", "grazing", "Namount_kg_ha_y", "Nitrogen_log"),
    complete_only = TRUE,
    leaf_id = "ID"
  )

}

# multivariate bootstrapping
make_multi_boot <- function(multivariate_traits){

  boot_multi <- trait_multivariate_bootstrap(
    filled_traits = multivariate_traits,
    nrep = 5, # number of reps is set low for demo purposes
    sample_size = 200,   # problem here with higher numbers (default 200)
    id = "ID",
    fun = function(x) {
      dbFD(
        x = x,
        calc.FRic = FALSE,
        calc.FDiv = FALSE,
        calc.CWM = FALSE,
        stand.x = FALSE,
        scale.RaoQ = FALSE
      )
    }
  )

}

# multivariate_traits |>
#   filter(turfID == "3 WN1C 85") |>
#   ggplot(aes(x = treatment_comm, y = value_trans)) +
#   geom_point() +
#   facet_wrap(~ trait_trans, scales = "free")

# raoq_est <- boot_multi |>
#   mutate(result = map(result, as.data.frame)) |>
#   unnest(result)

#
# multivariate_traits |>
#   pivot_wider(names_from = trait_trans, values_from = value_trans) |>
#   select()



# comm <- community |>
#   filter(turfID %in% c("81 AN1C 81", "85 WN1C 162", "87 WN1N 164")) |>
#   filter(species %in% c("Achillea millefolium", "Rumex acetosa", "Stellaria graminea")) |>
#   filter(Namount_kg_ha_y == 0) |>
#   select(origSiteID, blockID, turfID, warming, grazing, treatment, species, cover)
#
# #prepare trait data
# trait <- traits |>
#   filter(turfID %in% c("81 AN1C 81", "85 WN1C 162", "87 WN1N 164")) |>
#   filter(species %in% c("Achillea millefolium", "Rumex acetosa", "Stellaria graminea")) |>
#   filter(Namount_kg_ha_y == 0) |>
#   select(ID, origSiteID, blockID, turfID, warming, grazing, treatment, species, trait_trans, value_trans)
#
# set.seed(2024)
# trait_imp <- trait_fill(comm = comm,
#                         traits = trait,
#                         scale_hierarchy = c("origSiteID", "blockID", "turfID"),
#                         taxon_col = "species",
#                         trait_col = "trait_trans",
#                         value_col = "value_trans",
#                         abundance_col = "cover",
#                         treatment_col = c("treatment"),
#                         treatment_level = c("origSiteID"),
#                         other_col = c("warming", "grazing"),
#                         min_n_in_sample = 2)
