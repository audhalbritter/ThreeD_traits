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
                          scale_hierarchy = c("siteID", "blockID", "turfID"),
                          taxon_col = "species",
                          trait_col = "trait_trans",
                          value_col = "value_trans",
                          abundance_col = "cover",
                          treatment_col = c("treatment"),
                          treatment_level = c("siteID"),
                          other_col = c("origSiteID", "origBlockID", "destSiteID", "destBlockID", "Nlevel", "warming", "grazing", "Namount_kg_ha_y"),
                          min_n_in_sample = 2)


  return(trait_imp)
}


#do the bootstrapping
make_bootstrapping <- function(trait_imp){

  CWM <- trait_np_bootstrap(trait_imp, nrep = 100, sample_size = 200)

  trait_mean <- trait_summarise_boot_moments(CWM) |>
    ungroup() |>
    select(-global, -n)

  return(trait_mean)

}
