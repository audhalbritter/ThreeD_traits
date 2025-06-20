### species gain and losses

cover_change <- function(cover_raw){

  cover_total <- cover_raw |>
    # filter for 2019 and 2022 and trait data treatments
    filter(year %in% c(2019, 2022),
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
    tidylog::filter(!str_detect(species, "Unknown"),
           !species %in% c("Carex rupestris", "Carex rupestris cf",
                           "Carex norvegica cf", "Carex sp")) |>

    # log transform Nitrogen
    mutate(Nitrogen_log = log(Namount_kg_ha_y + 1)) |>

    # prettify and order factors
    mutate(origSiteID = recode(origSiteID, "Lia" = "Alpine", "Joa" = "Sub-alpine"),
           #origSiteID = factor(origSiteID, levels = c("Alpine", "Sub-alpine")),

           # make new variable for 3 treatments (AC0 is the control)
           treatment = paste0(warming, grazing, Namount_kg_ha_y),
           treatment = factor(treatment, levels = c("AC0", "WC0",
                                                    "AC5", "WC5",
                                                    "AC10", "WC10",
                                                    "AC50", "WC50",
                                                    "AC150", "WC150",
                                                    "AN0", "WN0")),

           grazing = recode(grazing, "C" = "Ungrazed", "N" = "Grazed"),
           grazing = factor(grazing, levels = c("Ungrazed", "Grazed")),
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
    select(-date, -recorder, -scribe, -file, -genus, -family) |>

    # remove treatments with no trait data
    filter(!is.na(treatment))
  
}


### Winners and losers

get_winners_and_losers <- function(cover_total){

  #first and last year transplant comm by treatment
  #get first_transplant (year 1)
  first_transplant = cover_total |>
    filter(year == 2019)  |>
    select(origSiteID, destSiteID, destBlockID, turfID, warming, Nlevel, Namount_kg_ha_y, Nitrogen_log, grazing, year, species, cover, functional_group)

  # get last_transplant (last year)
  last_transplant = cover_total |>
    filter(year == 2022)  |>
    select(origSiteID, destSiteID, destBlockID, turfID, warming, Nlevel, Namount_kg_ha_y, Nitrogen_log, grazing, year, species, cover, functional_group)

  #losers = first - last year
  losers = anti_join(first_transplant, last_transplant, by = c("origSiteID", "destSiteID", "destBlockID", "turfID", "warming", "Nlevel", "Namount_kg_ha_y", "Nitrogen_log", "grazing", "species", "functional_group"))

  #winners = last - first year
  winners = anti_join(last_transplant, first_transplant, by = c("origSiteID", "destSiteID", "destBlockID", "turfID", "warming", "Nlevel", "Namount_kg_ha_y", "Nitrogen_log", "grazing", "species", "functional_group"))

  # combine winner and losers
  winn_loos <- bind_rows(colonization = winners,
                          extinction = losers,
                         .id = "status") |>
    select(-cover)


}


make_gain_loss_figure <- function(winn_loos){

  dat <- winn_loos |> 
  filter(functional_group %in% c("forb", "graminoid")) |> 
  group_by(status, origSiteID, warming, grazing, Nlevel, Nitrogen_log, Namount_kg_ha_y, functional_group) |> 
  summarise(n = n()) |> 
  ungroup() |> 
  group_by(status, origSiteID, warming, grazing, Nitrogen_log, Namount_kg_ha_y, functional_group) |> 
  summarise(n = mean(n)) |> 
  ungroup() |> 
  mutate(n = if_else(status == "extinction", n * -1, n),
         WN = paste0(substr(warming, 1, 1), "-", Namount_kg_ha_y, "-", substr(grazing, 1, 1)),
        WN = factor(WN, levels = c("A-0-G", "W-0-G", "A-0-U", "W-0-U", "A-5-U", "W-5-U", "A-10-U", "W-10-U", "A-50-U", "W-50-U", "A-150-U", "W-150-U")))
    
    
  WG <- dat |> 
    filter(Namount_kg_ha_y == 0) |> 
    ggplot(aes(x = warming, y = n, fill = functional_group)) +
    geom_col() +
    geom_hline(yintercept = 0, colour = "grey70") +
    scale_fill_manual(name = "", values = c("thistle", "mediumseagreen")) +
    labs(x = "", y = "# of extinctions/colonizations") +
    facet_grid(origSiteID ~ grazing) +
    theme_bw()

  
  WN <- dat |> 
    filter(grazing == "Ungrazed") |> 
    ggplot(aes(x = as.factor(Namount_kg_ha_y), y = n, fill = functional_group)) +
    geom_col() +
    geom_hline(yintercept = 0, colour = "grey70") +
    scale_fill_manual(name = "", values = c("thistle", "mediumseagreen")) +
    labs(x = "Nitrogen addition (kg N ha y)", y = "# of extinctions/colonizations") +
    facet_grid(origSiteID ~ warming) +
    theme_bw()

  gain_loss_figure <- WG + WN + plot_layout(guides = "collect") & theme(legend.position = "top")

}

