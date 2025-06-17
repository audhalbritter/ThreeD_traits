library(tidyverse)
library(tidylog) 
library(openxlsx)
library(FD)
library(traitstrap) # install from GitHub, installing from Cran gave me errors

# code following https://rdrr.io/github/richardjtelford/traitstrap/f/vignettes/traitstrap-workflow.Rmd

traits <- read.csv("d:/Epperlein/Desktop/PFTC//PFTC6_ThreeD_clean_leaf_traits_2022.csv")
abund <- read.csv("d:/Epperlein/Desktop/PFTC/Three-D_clean_cover_2019-2022.csv")

abund <- abund %>% 
  filter(year == "2022") %>% 
  select(-c(date, recorder, scribe, remark, file)) 

plot.info <- traits %>% 
  filter(!is.na(turfID)) %>% 
  select(-c(species,trait, value, comment, problem,
            flag, individual_nr))


traits_df <- traits %>%           
  select(ID, turfID, blockID, species, individual_nr, origSiteID, destSiteID, trait, value) %>% 
  filter(!is.na(turfID)) %>% 
  mutate(destPlotID = str_extract(turfID, "(?<=\\s)[^\\s]+$")) %>% 
  rename(destBlockID=blockID) %>% 
  filter(species != "Viola tricolor") %>%    # in other plots
  group_by(turfID, species, trait) %>% 
  ungroup() %>% 
  mutate(Genus = word(species, 1))

trait_species <- unique(traits_df$species)

# viola tricolor measured but not in cover data

# correcting some species names to match names in traits df
abund_df <- abund %>% 
  filter(!is.na(turfID)) %>%      # get rid of gradient
  mutate(species = case_when(
    species == "Carex pilulifera cf" ~ "Carex pilulifera",
    species == "Carex norvegica cf" ~  "Carex norvegica",
    species == "Salix herbaceae" ~ "Salix herbacea",
    species == "Carex atrata cf" ~  "Carex atrata",
    species == "Pyrola sp" ~  "Pyrola norvegica",
    T ~ species)) %>% 
  filter(species %in% trait_species) %>% 
  filter(!(turfID== "68 AN9I 68" & species=="Luzula spicata" & cover=="3")) %>% 
  mutate(cover = case_when(
    turfID== "68 AN9I 68" & species=="Luzula spicata" ~ as.integer(2),  # one duplicate with luzula cover being 1 and 3 in same plot --> took the mean
    T ~ cover)) %>% 
  select(destSiteID, destSiteID, destPlotID, destBlockID, turfID, species, cover) %>% 
  distinct() %>% 
  mutate(Genus = word(species, 1))

 
traits_df$destPlotID <- as.integer(traits_df$destPlotID)

multivariate_traits <- trait_fill(
  comm = abund_df, 
  traits = traits_df,
  scale_hierarchy = c("destSiteID", "destBlockID","destPlotID"),   #starting with the highest level
  taxon_col = "species",
  value_col = "value",
  trait_col = "trait",
  abundance_col = "cover",
  complete_only = T,           # need to be true otherwise trait_multi_bootstr won't work
  leaf_id = "ID"
)


# default is 100 for n rep and 200 for sample size but that gives an error
# with nrep and sample_size both set at 5, I only get warnings
# need to increase numbers and see where error comes from

boot_multi <- trait_multivariate_bootstrap(
  filled_traits = multivariate_traits,
  nrep = 5, # number of reps is set low for demo purposes
  sample_size = 5,   # problem here with higher numbers (default 200)               
  id = "ID",
  fun = function(x) {
    dbFD(
      x = x,
      calc.FRic = TRUE,
      calc.FDiv = FALSE,
      calc.CWM = FALSE,
      stand.x = FALSE,
      scale.RaoQ = FALSE
      )
  }
)

# extracting data from list columns
raoq_est <- boot_multi %>% 
  mutate(result = map(result, as.data.frame)) %>% 
  unnest(result)

names(raoq_est)

ggplot(data = raoq_est, mapping = aes(x = FRic, fill = destSiteID)) +
  geom_density(alpha = 0.5) #+
  #xlim(c(0, 100))

  
# join plot info
full <- raoq_est %>% 
  right_join(plot.info, relationship = "many-to-many") %>% 
  distinct() %>% 
  mutate(location = case_when(
    destSiteID == "Vik" ~ "subalpine",       # I think I might not have this correct yet
    destSiteID == "Joa" ~ "alpine",
    destSiteID == "Lia" ~ "alpine"
  ))
  
names(full)


ggplot(data = full, mapping = aes(warming, FRic)) +
  geom_point() +
  facet_wrap(~location)
  #geom_boxplot()


