# clean data

clean_community <- function(cover_raw, metaTurfID, sp_list){

  cover_raw |>
    # filter for 2022 and trait data treatments
    filter(year == 2022,
           grazing %in% c("C", "N"),
           Nlevel %in% c(1, 2, 3, 6, 7, 8, 10)) |>
    # group species that are uncertain
    # e.g. Antennaria dioica, Ant alpina and Ant sp
    mutate(species = case_when(str_detect(species, "Antennaria") ~ "Antennaria sp",
                               species == "Carex capillaris cf" ~ "Carex capillaris",
                               species == "Carex leporina cf" ~ "Carex leporina",
                               species == "Carex nigra cf" ~ "Carex nigra",
                               species == "Carex vaginata cf" ~ "Carex vaginata",
                               species == "Epilobium anagallidifolium cf" ~ "Epilobium anagallidifolium cf",
                               species == "Erigeron uniflorus cf" ~ "Erigeron uniflorus",
                               species == "Euphrasia wetsteinii cf" ~ "Euphrasia wetsteinii",
                               str_detect(species, "Luzula") ~ "Luzula sp",
                               str_detect(species, "Pyrola") ~ "Pyrola sp",
                               TRUE ~ species)) |>
    # Remove Carex rupestris and norvegica cf, Carex sp, because they are very uncertain
    filter(!str_detect(species, "Unknown"),
           !species %in% c("Carex rupestris", "Carex rupestris cf",
                           "Carex norvegica cf", "Carex sp")) |>

    # log transform Nitrogen
    mutate(Nitrogen_log = log(Namount_kg_ha_y + 1)) |>

    # prettify and order factors
    mutate(origSiteID = recode(origSiteID, "Lia" = "Alpine", "Joa" = "Sub-alpine"),
           origSiteID = factor(origSiteID, levels = c("Alpine", "Sub-alpine")),

           # make new variable for 3 treatments (AC0 is the control)
           treatment = paste0(warming, grazing, Namount_kg_ha_y),
           treatment = factor(treatment, levels = c("AC0", "WC0",
                                                    "AC5", "WC5",
                                                    "AC10", "WC10",
                                                    "AC50", "WC50",
                                                    "AC150", "WC150",
                                                    "AN0", "WN0")),

           grazing = recode(grazing, "C" = "Control", "N" = "Natural"),
           warming = recode(warming, "A" = "Ambient", "W" = "Warming"),

           # make same as traits
           siteID = recode(destSiteID, "Lia" = "Liahovden", "Joa" = "Joasete", "Vik" = "Vikesland"),
           blockID = destBlockID) |>

    # take average cover of 3 control plots (probably not wanted, because we have traits from 2 controls?)
    # group_by(year, date, origSiteID, destSiteID, warming, grazing, Namount_kg_ha_y, Nitrogen_log, grazing_num, species) |>
    # summarise(cover = mean(cover)) |>

    # add taxon information
    left_join(sp_list |>
                mutate(species = paste(genus, species, sep = " ")),
              by = "species") |>
    # fix NA's in functional group
    mutate(functional_group = case_when(species == "Carex nigra" ~ "graminoid",
                                        species %in% c("Oxytropa laponica", "Galium verum", "Veronica officinalis", "Erigeron uniflorus", "Epilobium anagallidifolium") ~ "forb",
                                        TRUE ~ functional_group)) |>
    ungroup() |>
    select(-date, -recorder, -scribe, -file, -genus, -family)

}


clean_traits <- function(trait_raw){

  trait_raw |>
    # remove gradient plots
    filter(!siteID == "Hogsete",
           !(siteID == "Vikesland" & warming == "A")) |>
    select(-gradient) |>

    # remove missing treatment (3 leaves)
    filter(!is.na(grazing)) |>

    # remove wet mass, correlated with dry mass
    filter(trait != "wet_mass_g") |>
    # log transform size traits
    mutate(
      value_trans = if_else(
        trait %in% c(
          "plant_height_cm",
          "dry_mass_g",
          "leaf_area_cm2",
          "leaf_thickness_mm"
        ),
        true = suppressWarnings(log(value)),# suppress warnings from log(-value) in isotopes (these are calculated but not kept)
        false = value
      ),
      trait_trans = recode(
        trait,
        "plant_height_cm" = "plant_height_cm_log",
        "dry_mass_g" = "dry_mass_g_log",
        "leaf_area_cm2" = "leaf_area_cm2_log",
        "leaf_thickness_mm" = "leaf_thickness_mm_log"
      )) |>
    # order traits
    mutate(trait_trans = factor(trait_trans, levels = c("plant_height_log_cm", "dry_mass_g_log", "leaf_area_log_cm2", "leaf_thickness_log_mm", "ldmc", "sla_cm2_g"))) |>

    # prettify and order factors
    mutate(origSiteID = recode(origSiteID, "Lia" = "Alpine", "Joa" = "Sub-alpine"),
           origSiteID = factor(origSiteID, levels = c("Alpine", "Sub-alpine")),

           # make new variable for 3 treatments (AC0 is the control)
           treatment = paste0(warming, grazing, Namount_kg_ha_y),
           treatment = factor(treatment, levels = c("AC0", "WC0",
                                                    "AC5", "WC5",
                                                    "AC10", "WC10",
                                                    "AC50", "WC50",
                                                    "AC150", "WC150",
                                                    "AN0", "WN0")),

           grazing = recode(grazing, "C" = "Control", "N" = "Natural"),
           grazing = factor(grazing, levels = c("Control", "Natural")),

           warming = recode(warming, "A" = "Ambient", "W" = "Warming"),
           warming = factor(warming, levels = c("Ambient", "Warming")),

           Namount_kg_ha_y = as.character(Namount_kg_ha_y),
           Namount_kg_ha_y = factor(Namount_kg_ha_y, levels = c("0", "5", "10", "50", "150")),

           blockID = as.numeric(blockID))

}
