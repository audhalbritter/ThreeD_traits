# Note to get targets to work the remote path in the download plan needs to be updated to

# tar_target(
#   name = trait_download,
#   command =  get_file(node = "fcbw4",
#                       file = "PFTC6_ThreeD_clean_leaf_traits_2022.csv",
#                       path = "data",
#                       remote_path = "i. trait_data"),
#   format = "file"
# ),

##### Libraries
library(glmmTMB)
library(DHARMa)
library(sjPlot)
library(MuMIn) # for AICc function
library(tidyverse)

# Data read in from Aud's scripts...

## Modelling experiments
### Preparing data -------

# With the community weighted means
trait_mean

# trait mean is wrong because the trait name cleaning is broken?
unique(trait_raw$trait)
unique(trait_mean$trait)
unique(trait_mean$trait_trans)
# AE - I can't remember what the exact problem was with this now... but I just ran the code again below

# Just run it myself?

clean_community_data <-  cover_raw |>
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

  # add Namount variable
  left_join(metaTurfID)  |>
  #distinct(Nlevel, Namount_kg_ha_y), by = "Nlevel")  |>
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


# Checking grazing stuff

table(trait_raw$grazing)
trait_raw %>%
  select(siteID,warming,grazing,Namount_kg_ha_y) %>%
  unique()

trait_raw |>
  # remove incline data
  filter(siteID =="Liahovden" | siteID =="Joasete" | (siteID =="Vikesland" & warming =="W")) %>%
  select(siteID,warming,grazing,Namount_kg_ha_y) %>%
  unique()


## Adapting other code
Traits_clean <- trait_raw |>
  # remove incline data and gradient plots e.g. Hogsete and ambient plots in Vikesland
  filter(siteID =="Liahovden" | siteID =="Joasete" | (siteID =="Vikesland" & warming =="W")) |>
  select(-gradient) |>

  # remove missing treatment (3 leaves)
  filter(!is.na(grazing)) |>

  # remove wet mass, correlated with dry mass
  filter(trait != "wet_mass_g") |>
  # order traits
  #mutate(trait_trans = factor(trait_trans, levels = c("plant_height_log_cm", "dry_mass_g_log", "leaf_area_log_cm2", "leaf_thickness_log_mm", "ldmc", "sla_cm2_g"))) |>

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

# Get the bootstrapped CWMs

#prepare trait data
Traits_clean2 <- Traits_clean |>
  select(siteID, blockID, turfID, warming, grazing, Nlevel, Namount_kg_ha_y, trait, value,treatment, species, origSiteID, destSiteID)


Traits_clean2 %>%
  select(siteID,warming,grazing,Namount_kg_ha_y) %>%
  unique()

#set seed for bootstrapping repeatability
set.seed(2525)
trait_imp <- trait_fill(comm = clean_community_data,
                        traits = Traits_clean2,
                        scale_hierarchy = c("siteID", "blockID", "turfID"),
                        taxon_col = "species",
                        trait_col = "trait",
                        value_col = "value",
                        abundance_col = "cover",
                        treatment_col = c("treatment"),
                        treatment_level = c("siteID"),
                        other_col = c("origSiteID", "origBlockID", "destSiteID", "destBlockID", "Nlevel", "warming", "grazing", "Namount_kg_ha_y"),
                        min_n_in_sample = 2)




#do the bootstrapping
CWM <- trait_np_bootstrap(trait_imp, nrep = 100, sample_size = 200)

trait_mean <- trait_summarise_boot_moments(CWM) |>
  ungroup() |>
  select(-global, -n)



# Plot out bootstrapped CWMs
trait_mean %>%
  mutate(Nitrogen_log = log(Namount_kg_ha_y + 1)) |>
  filter(origSiteID == "Alpine") |> ungroup() |>
  ggplot(aes(x = Nitrogen_log, y = mean, colour = warming, linetype = grazing)) +
  geom_point() +
  geom_smooth(method = "lm") +
  scale_colour_manual(values = c("grey30", "#FD6467")) +
  labs(x = bquote(log(Nitrogen)~kg~ha^-1~y^-1),
       y = "Mean trait value", title = "Alpine") +
  facet_wrap(~ trait, scales = "free") +
  theme_bw()

# Ok things work

# Variance and skewness are also possible to investigate

trait_mean %>%
  mutate(Nitrogen_log = log(Namount_kg_ha_y + 1)) |>
  filter(origSiteID == "Alpine") |> ungroup() |>
  ggplot(aes(x = Nitrogen_log, y = skew, colour = warming, linetype = grazing)) +
  geom_point() +
  geom_smooth(method = "lm") +
  scale_colour_manual(values = c("grey30", "#FD6467")) +
  labs(x = bquote(log(Nitrogen)~kg~ha^-1~y^-1),
       y = "Mean trait value", title = "Alpine") +
  facet_wrap(~ trait, scales = "free") +
  theme_bw()



## Add a log Nitrogen amount
trait_mean <- trait_mean %>%
  mutate(Nitrogen_log = log(Namount_kg_ha_y + 1))
trait_mean$Nitrogen_log


## is the grazing treatment stuff correct?
trait_mean %>%
  select(siteID,warming,grazing,Namount_kg_ha_y) %>%
  unique()
# hmm the cwm function adds a level for each one?

## Maybe subset the CWM data for just the interactions we actually had

# Stick the interactions together
Traits_clean3 <- Traits_clean2 %>%
  unite(Site_block_treatment, c(siteID, blockID,turfID,treatment), sep = "_", remove = FALSE)


trait_mean2 <- trait_mean %>%
  unite(Site_block_treatment, c(siteID, blockID,turfID,treatment_comm), sep = "_", remove = FALSE)

# Are they different?
setdiff(unique(Traits_clean3$Site_block_treatment), unique(trait_mean2$Site_block_treatment))
# ok maybe a couple of rogue ones due to NAs?
setdiff(unique(trait_mean2$Site_block_treatment), unique(Traits_clean3$Site_block_treatment))
# so the treatment_comm has NAs for all the weird natural grazing ones we didnt do - easy fix = remove these

trait_mean2 <- trait_mean %>%
  filter(!is.na(treatment_comm))
## This is the data to use in the CWM analyses



#### Modelling -------

# Our variables are...
# -  Nitrogen
# - Warming (transplant)
# - Grazing
# Had this note...
# We cannot do a 3 way interaction because we only have grazing at the 0 N level.
# We need to run 2 models: w x g and w x n
trait_mean2 %>%
       filter(grazing =="Natural")



### Potential levels for mixed models?

ggplot(trait_mean,aes(x=siteID,y=blockID))+
  geom_point()+
  facet_wrap(~turfID)
# so turf isnt a level but site and block are? maybe
hist(trait_mean$blockID)

ggplot(trait_mean,aes(x=siteID,y=blockID))+
  geom_point()
# Spatial component? Probably not necessary and not sure we have all the info needed

### Warming vs grazing model
# One for each trait?
unique(trait_mean$trait)
# Plant height model
# Dry Mass
# LDMC
# Leaf Area
# Leaf Thickness
# SLA


# So could do separate models for warming alpine and subalpine?
# Or include site origin as an interaction
# How do other transplant experiments do it?


# I guess there's also space for looking at groups of species
# So this would be beyond the basic CWM for each plot and would have CWM per group?
# But that might mask the abundance changes with them separate

### Correlations between CWMs
library(GGally)

trait_mean2_wide <- trait_mean2 %>%
  select(siteID,blockID,turfID,trait,mean) %>%
  pivot_wider(names_from =trait, values_from =mean)
ggpairs(trait_mean2_wide, columns = 4:9)

### Height warming vs grazing GLM ----

height_df_no_N <- trait_mean2 %>%
  filter(trait=="plant_height_cm") %>%
  filter(Namount_kg_ha_y ==0)

# This is removing all the nitrogen treatments

# Warming and grazing interaction
GW_height_M0 <- glm(mean~warming*grazing,
                    data=height_df_no_N,
                    family=gaussian)
summary(GW_height_M0)
plot(simulateResiduals(GW_height_M0))

height_df_no_N %>% group_by(origSiteID) %>%
  tally()

plot_model(GW_height_M0,type="eff",terms = c("warming [all]"),show.data=T, jitter = T)
plot_model(GW_height_M0)

# Warming * grazing * siteorigin interaction

GW_height_M1 <- glm(mean~warming*grazing*origSiteID,
                    data=height_df_no_N,
                    family=gaussian)
summary(GW_height_M1)
plot(simulateResiduals(GW_height_M1))

plot_model(GW_height_M1,type="eff",terms = c("warming [all]"),show.data=T, jitter = T)
plot_model(GW_height_M1,type="eff",terms = c("origSiteID [all]"),show.data=T, jitter = T)
plot_model(GW_height_M1)
AICc(GW_height_M0,GW_height_M1) # AICc does make a difference

# this is better

# Now without 3 way interaction?

GW_height_M2 <- glm(mean~warming*grazing+origSiteID,
                    data=height_df_no_N,
                    family=gaussian)
summary(GW_height_M2)
plot(simulateResiduals(GW_height_M2))

plot_model(GW_height_M2,type="eff",terms = c("warming [all]"),show.data=T, jitter = T)
plot_model(GW_height_M2,type="eff",terms = c("grazing [all]"),show.data=T, jitter = T)
plot_model(GW_height_M2,type="eff",terms = c("origSiteID [all]"),show.data=T, jitter = T)
plot_model(GW_height_M2)
AICc(GW_height_M2,GW_height_M1)
# better without the interaction

# interaction of grazing removed

GW_height_M3 <- glm(mean~warming+grazing+origSiteID,
                    data=height_df_no_N,
                    family=gaussian)
summary(GW_height_M3)
plot(simulateResiduals(GW_height_M3))

plot_model(GW_height_M3,type="eff",terms = c("warming [all]"),show.data=T, jitter = T)
plot_model(GW_height_M3,type="eff",terms = c("grazing [all]"),show.data=T, jitter = T)
plot_model(GW_height_M3,type="eff",terms = c("origSiteID [all]"),show.data=T, jitter = T)
plot_model(GW_height_M2)
AICc(GW_height_M1,GW_height_M2,GW_height_M3)

# This is better with no interaction at all but maybe we want to keep the interaction
# as that's what we care about, even if it is unimportant

# Warming x site origin interaction

GW_height_M4 <- glm(mean~warming*origSiteID+grazing,
                    data=height_df_no_N,
                    family=gaussian)
summary(GW_height_M4)
plot(simulateResiduals(GW_height_M4))

AICc(GW_height_M1,GW_height_M2,GW_height_M3,GW_height_M4)
# Best one is still no interaction at all



#### Height model with warming x nitrogen - removing natural grazing -----

height_df_no_graze <- trait_mean2 %>%
  filter(trait=="plant_height_cm") %>%
  filter(grazing =="Control")

# Warming X log nitrogen interaction
WN_height_M1 <- glm(mean~warming*Nitrogen_log,
                    data=height_df_no_graze,
                    family=gaussian)
summary(WN_height_M1)
plot(simulateResiduals(WN_height_M1)) # qq plot a bit s shaped
# interaction N.S.

plot_model(WN_height_M1,type="eff",terms = c("warming [all]"),show.data=T)
plot_model(WN_height_M1)

## With origSiteID interaction

WN_height_M2 <- glm(mean~warming*Nitrogen_log*origSiteID,
                    data=height_df_no_graze,
                    family=gaussian)
summary(WN_height_M2)
plot(simulateResiduals(WN_height_M2)) # residuals are odd...
AICc(WN_height_M1,WN_height_M2)

plot_model(WN_height_M2,type="eff",terms = c("warming [all]"),show.data=T, jitter=T)
plot_model(WN_height_M2,type="eff",terms = c("origSiteID [all]"),show.data=T, jitter=T)
plot_model(WN_height_M2)
# This looks interesting

# Without interaction
WN_height_M3 <- glm(mean~warming*Nitrogen_log+origSiteID,
                    data=height_df_no_graze,
                    family=gaussian)
summary(WN_height_M3)
plot(simulateResiduals(WN_height_M3)) # AIC better without 3 way interaction
# more importantly = residuals aren't as crazy
AICc(WN_height_M3,WN_height_M2)

## Have to try without log nitrogen too

WN_height_M4 <- glm(mean~warming*Namount_kg_ha_y +origSiteID,
                    data=height_df_no_graze,
                    family=gaussian)
summary(WN_height_M4)
plot(simulateResiduals(WN_height_M4))
AICc(WN_height_M4,WN_height_M3)

plot_model(WN_height_M4,type="eff",terms = c("warming [all]"),show.data=T, jitter=T)
plot_model(WN_height_M4,type="eff",terms = c("origSiteID [all]"),show.data=T, jitter=T)
plot_model(WN_height_M4,type="eff",terms = c("Namount_kg_ha_y [all]"),show.data=T, jitter=T)
plot_model(WN_height_M4)

# AICc is worse
# This may be a useful link
# https://stats.stackexchange.com/questions/298/in-linear-regression-when-is-it-appropriate-to-use-the-log-of-an-independent-va

### Testing the other interactions

WN_height_M5 <- glm(mean~warming+Nitrogen_log*origSiteID,
                    data=height_df_no_graze,
                    family=gaussian)
summary(WN_height_M5)
plot(simulateResiduals(WN_height_M5)) # strange residuals and qqplot
AICc(WN_height_M5,WN_height_M3) # AIC is better but residuals are worse

plot_model(WN_height_M5,type="eff",terms = c("warming [all]"),show.data=T, jitter=T)
plot_model(WN_height_M5,type="eff",terms = c("origSiteID [all]"),show.data=T, jitter=T)
plot_model(WN_height_M5)

## try with warming x origin site too

WN_height_M6 <- glm(mean~warming+Nitrogen_log*origSiteID+warming:origSiteID,
                    data=height_df_no_graze,
                    family=gaussian)
summary(WN_height_M6)
plot(simulateResiduals(WN_height_M6))
AICc(WN_height_M3,WN_height_M6)

# It is not better - pretty bad residuals

## What about quadratic Nitrogen?
WN_height_M7 <- glm(mean~warming*poly(Nitrogen_log,2)+origSiteID,
                    data=height_df_no_graze,
                    family=gaussian)
summary(WN_height_M7)
plot(simulateResiduals(WN_height_M7))
AICc(WN_height_M3,WN_height_M7)

plot_model(WN_height_M7,type="eff",terms = c("warming [all]"),show.data=T, jitter=T)
plot_model(WN_height_M7,type="eff",terms = c("origSiteID [all]"),show.data=T, jitter=T)
plot_model(WN_height_M7,type="eff",terms = c("Nitrogen_log [all]", "warming"),show.data=T, jitter=T)
plot_model(WN_height_M7)

# Quadratic looks a bit forced with so few points
# and it doesn't look like it really fits the residuals - adds a curve in

### Warming x Nitrogen interaction

WN_height_M8 <- glm(mean~warming+Nitrogen_log*origSiteID+warming:Nitrogen_log,
                    data=height_df_no_graze,
                    family=gaussian)
summary(WN_height_M8)
plot(simulateResiduals(WN_height_M8))
AICc(WN_height_M3,WN_height_M8)
# model residuals are odd again

### No interaction at all?

# Without interaction
WN_height_M9 <- glm(mean~warming+Nitrogen_log+origSiteID,
                    data=height_df_no_graze,
                    family=gaussian)
summary(WN_height_M9)
plot(simulateResiduals(WN_height_M9))
# residuals and everything looks better with no interaction?
AICc(WN_height_M3,WN_height_M9)
plot_model(WN_height_M9,type="eff",terms = c("warming [all]"),show.data=T, jitter=T)
plot_model(WN_height_M9,type="eff",terms = c("origSiteID [all]"),show.data=T, jitter=T)
plot_model(WN_height_M9,type="eff",terms = c("Nitrogen_log [all]"),show.data=T, jitter=F)
plot_model(WN_height_M9)

# But the model doesn't look right without an interaction of nitrogen and site ID in it?

## Force interaction to be kept, just check interactions
# of site origin with warming and nitrogen

WN_height_M10 <- glm(mean~warming*Nitrogen_log+origSiteID+
                       warming:origSiteID,
                     data=height_df_no_graze,
                     family=gaussian)
summary(WN_height_M10)
plot(simulateResiduals(WN_height_M10))
# residuals and everything looks better with no interaction?
AICc(WN_height_M3,WN_height_M9,WN_height_M10)

## No evidence it is better

WN_height_M11 <- glm(mean~warming*Nitrogen_log+origSiteID+
                       Nitrogen_log:origSiteID,
                     data=height_df_no_graze,
                     family=gaussian)
summary(WN_height_M11)
plot(simulateResiduals(WN_height_M11))
# Not better, residuals and everything looks better with no interaction?
AICc(WN_height_M3,WN_height_M9,WN_height_M11)



#### Leaf Area - warming x grazing ------

LeafArea_df_no_N <- trait_mean2 %>%
  filter(trait=="leaf_area_cm2") %>%
  filter(Namount_kg_ha_y ==0)

hist(LeafArea_df_no_N$mean)

# Warming and grazing interaction

GW_leafarea_M0 <- glm(mean~warming*grazing,
                      data=LeafArea_df_no_N,
                      family=gaussian)
summary(GW_drymass_M0)
plot(simulateResiduals(GW_drymass_M0))

# Gamma is worse - tested it

# Warming * grazing * siteorigin interaction

GW_leafarea_M1 <- glm(mean~warming*grazing+origSiteID,
                      data=LeafArea_df_no_N,
                      family=gaussian)
summary(GW_leafarea_M1)
plot(simulateResiduals(GW_leafarea_M1))

plot_model(GW_leafarea_M1,type="eff",terms = c("warming [all]"),show.data=T, jitter = T)
plot_model(GW_leafarea_M1,type="eff",terms = c("origSiteID [all]"),show.data=T, jitter = T)
plot_model(GW_leafarea_M1)
AICc(GW_height_M0,GW_leafarea_M1)

# this is better
# Now with 3 way interaction?

GW_leafarea_M2 <- glm(mean~warming*grazing*origSiteID,
                      data=LeafArea_df_no_N,
                      family=gaussian)
summary(GW_leafarea_M2)
plot(simulateResiduals(GW_leafarea_M2))

plot_model(GW_leafarea_M2,type="eff",terms = c("warming [all]"),show.data=T, jitter = T)
plot_model(GW_leafarea_M2,type="eff",terms = c("origSiteID [all]"),show.data=T, jitter = T)
plot_model(GW_leafarea_M2)
AICc(GW_leafarea_M1,GW_leafarea_M2)
# Interaction is worse

## No interaction

GW_leafarea_M3 <- glm(mean~warming+grazing+origSiteID,
                      data=LeafArea_df_no_N,
                      family=gaussian)
summary(GW_leafarea_M3)
plot(simulateResiduals(GW_leafarea_M3))

plot_model(GW_leafarea_M3,type="eff",terms = c("warming [all]"),show.data=T, jitter = T)
plot_model(GW_leafarea_M3,type="eff",terms = c("origSiteID [all]"),show.data=T, jitter = T)
plot_model(GW_leafarea_M3)
AICc(GW_leafarea_M3,GW_leafarea_M1)

# Simpler = better > 2 AICc away

### Interaction of warming and siteID

GW_leafarea_M4 <- glm(mean~warming*origSiteID+grazing,
                      data=LeafArea_df_no_N,
                      family=gaussian)
summary(GW_leafarea_M4)
plot(simulateResiduals(GW_leafarea_M4))

AICc(GW_leafarea_M3,GW_leafarea_M1,GW_leafarea_M4)
# not better

### Interaction of grazing and siteID

GW_leafarea_M5 <- glm(mean~warming+origSiteID*grazing,
                      data=LeafArea_df_no_N,
                      family=gaussian)
summary(GW_leafarea_M5)
plot(simulateResiduals(GW_leafarea_M5))

AICc(GW_leafarea_M3,GW_leafarea_M1,GW_leafarea_M5)
# not better

## SO best = no interaction


## Leaf Area -  warming x nitrogen - removing natural grazing ----

LeafArea_df_no_graze <- trait_mean2 %>%
  filter(trait=="leaf_area_cm2") %>%
  filter(grazing =="Control")

## Warming x nitrogen interaction

WN_leafarea_M1 <- glm(mean~warming*Nitrogen_log,
                      data=LeafArea_df_no_graze,
                      family=gaussian)
summary(WN_leafarea_M1)
plot(simulateResiduals(WN_leafarea_M1))
# interaction NS

plot_model(WN_leafarea_M1,type="eff",terms = c("warming [all]"),show.data=T,jitter=T)
plot_model(WN_leafarea_M1,type="eff",terms = c("Nitrogen_log [all]"),show.data=T)
plot_model(WN_leafarea_M1,type="eff",terms = c("Nitrogen_log [all]", "warming [all]"),show.data=T)
plot_model(WN_leafarea_M1)

## Can we look at quadratic term for Nitrogen

WN_leafarea_M2 <- glm(mean~warming*poly(Nitrogen_log,2),
                      data=LeafArea_df_no_graze,
                      family=gaussian)
summary(WN_leafarea_M2)
plot(simulateResiduals(WN_leafarea_M2))
# interaction NS
AICc(WN_leafarea_M1,WN_leafarea_M2) # this model is worse

## With origSiteID interaction

WN_leafarea_M3 <- glm(mean~warming*Nitrogen_log*origSiteID,
                      data=LeafArea_df_no_graze,
                      family=gaussian)
summary(WN_leafarea_M3)
plot(simulateResiduals(WN_leafarea_M3))
AICc(WN_leafarea_M1,WN_leafarea_M3)
# Model is better with origSiteID in it

plot_model(WN_leafarea_M3,type="eff",terms = c("origSiteID [all]"),show.data=T,jitter=T)
plot_model(WN_leafarea_M3,type="eff",terms = c("Nitrogen_log [all]"),show.data=T)
plot_model(WN_leafarea_M3,type="eff",terms = c("warming [all]"),show.data=T,jitter=T)
plot_model(WN_leafarea_M3)

## Model without interaction with origin site ID

WN_leafarea_M4 <- glm(mean~warming*Nitrogen_log+origSiteID,
                      data=LeafArea_df_no_graze,
                      family=gaussian)
summary(WN_leafarea_M4)
plot(simulateResiduals(WN_leafarea_M4))
AICc(WN_leafarea_M4,WN_leafarea_M3)

plot_model(WN_leafarea_M4,type="eff",terms = c("origSiteID [all]"),show.data=T,jitter=T)
plot_model(WN_leafarea_M4,type="eff",terms = c("Nitrogen_log [all]"),show.data=T)
plot_model(WN_leafarea_M4,type="eff",terms = c("Nitrogen_log [all]","warming"),show.data=T)
plot_model(WN_leafarea_M4,type="eff",terms = c("warming [all]"),show.data=T,jitter=T)
plot_model(WN_leafarea_M4)
# Model is better without three way interaction


## Have to try without log nitrogen too

WN_leafarea_M5 <- glm(mean~warming*Namount_kg_ha_y +origSiteID,
                      data=LeafArea_df_no_graze,
                      family=gaussian)
summary(WN_leafarea_M5)
plot(simulateResiduals(WN_leafarea_M5))
AICc(WN_leafarea_M4,WN_leafarea_M5)
# This is worse without log nitrogen

# No interaction?

WN_leafarea_M6 <- glm(mean~warming+Nitrogen_log+origSiteID,
                      data=LeafArea_df_no_graze,
                      family=gaussian)
summary(WN_leafarea_M6)
plot(simulateResiduals(WN_leafarea_M6))
AICc(WN_leafarea_M4,WN_leafarea_M3,WN_leafarea_M6)
# this is similar in terms of AICc but the residuals show we need an interaction

# Subset of interactions

WN_leafarea_M7 <- glm(mean~warming*Nitrogen_log+origSiteID+origSiteID:Nitrogen_log,
                      data=LeafArea_df_no_graze,
                      family=gaussian)
summary(WN_leafarea_M7)
plot(simulateResiduals(WN_leafarea_M7))
AICc(WN_leafarea_M4,WN_leafarea_M3,WN_leafarea_M7)
# similar but the residuals are odd


### Looking at interaction of nitrogen and site origin

WN_leafarea_M8 <- glm(mean~warming+Nitrogen_log*origSiteID,
                      data=LeafArea_df_no_graze,
                      family=gaussian)
summary(WN_leafarea_M8)
plot(simulateResiduals(WN_leafarea_M8))
AICc(WN_leafarea_M4,WN_leafarea_M3,WN_leafarea_M8)
# worse?

# Subset of interactions
WN_leafarea_M9 <- glm(mean~warming*Nitrogen_log+origSiteID+origSiteID:warming,
                      data=LeafArea_df_no_graze,
                      family=gaussian)
summary(WN_leafarea_M9)
plot(simulateResiduals(WN_leafarea_M9))
AICc(WN_leafarea_M4,WN_leafarea_M3,WN_leafarea_M8,WN_leafarea_M9)
# very similar to the model 4 but with an non-significant interaction


## Just check if a quadratic effect for nitrogen is needed in the best model

WN_leafarea_M10 <- glm(mean~warming*poly(Nitrogen_log,2)+origSiteID,
                      data=LeafArea_df_no_graze,
                      family=gaussian)
summary(WN_leafarea_M10)
plot(simulateResiduals(WN_leafarea_M10))
AICc(WN_leafarea_M4,WN_leafarea_M10)
# this is worse

# Best model just seems to be warming x nitrogen + origSiteID


#### Dry Mass - warming x grazing model ----

drymass_df_no_N <- trait_mean2 %>%
  filter(trait=="dry_mass_g") %>%
  filter(Namount_kg_ha_y ==0)

hist(drymass_df_no_N$mean)

traits %>%
  filter(trait=="dry_mass_g") %>%
  ggplot(aes(x=value))+
  geom_histogram()


# Warming and grazing interaction

GW_drymass_M0 <- glm(mean~warming*grazing,
                     data=drymass_df_no_N,
                     family=gaussian)
summary(GW_drymass_M0)
plot(simulateResiduals(GW_drymass_M0))

# Would a gamma distirbution be better?

GW_drymass_M0.2 <- glm(mean~warming*grazing,
                       data=drymass_df_no_N,
                       family=Gamma)
summary(GW_drymass_M0.2)
plot(simulateResiduals(GW_drymass_M0.2))
AICc(GW_drymass_M0,GW_drymass_M0.2)
# hmm it might be - models after this are gaussian but probably could be gamma


# Warming * grazing * siteorigin interaction

GW_drymass_M1 <- glm(mean~warming*grazing+origSiteID,
                     data=drymass_df_no_N,
                     family=gaussian)
summary(GW_drymass_M1)
plot(simulateResiduals(GW_drymass_M1))

plot_model(GW_drymass_M1,type="eff",terms = c("warming [all]"),show.data=T, jitter = F)
plot_model(GW_drymass_M1,type="eff",terms = c("origSiteID [all]"),show.data=T, jitter = F)
plot_model(GW_drymass_M1)
AICc(GW_drymass_M0,GW_drymass_M1)

# this is better in terms of AIC but residuals are poor
# Now without 3 way interaction?

GW_drymass_M2 <- glm(mean~warming*grazing*origSiteID,
                     data=drymass_df_no_N,
                     family=gaussian)
summary(GW_drymass_M2)
plot(simulateResiduals(GW_drymass_M2))

# doesn't look good!

GW_drymass_M3 <- glm(mean~warming+grazing+origSiteID,
                     data=drymass_df_no_N,
                     family=gaussian)
summary(GW_drymass_M3)
plot(simulateResiduals(GW_drymass_M3))

# looks pretty bad here too

GW_drymass_M4 <- glm(mean~warming*origSiteID+grazing,
                     data=drymass_df_no_N,
                     family=gaussian)
summary(GW_drymass_M4)
plot(simulateResiduals(GW_drymass_M4))
# also has problems

## Check the gamma versions of the best model

GW_drymass_M5 <- glm(mean~warming*grazing+origSiteID,
                     data=drymass_df_no_N,
                     family=Gamma)
summary(GW_drymass_M5)
plot(simulateResiduals(GW_drymass_M5))
# this is not good

# Best model = GW_drymass_M1, some issues


## Dry Mass warming x nitrogen - removing natural grazing -----

dry_mass_df_no_graze <- trait_mean2 %>%
  filter(trait=="dry_mass_g") %>%
  filter(grazing =="Control")

# Warming x Nitrogen
WN_dry_mass_M1 <- glm(mean~warming*Nitrogen_log,
                      data=dry_mass_df_no_graze,
                      family=gaussian)
summary(WN_dry_mass_M1)
plot(simulateResiduals(WN_dry_mass_M1))
# interaction not significant

## Can we look at quadratic term for Nitrogen

WN_dry_mass_M2 <- glm(mean~warming*poly(Nitrogen_log,2),
                      data=dry_mass_df_no_graze,
                      family=gaussian)
summary(WN_dry_mass_M2)
plot(simulateResiduals(WN_dry_mass_M2))
# Residuals look better - not much to see

plot_model(WN_dry_mass_M2,type="eff",terms = c("warming [all]"),show.data=T)
plot_model(WN_dry_mass_M2,type="eff",terms = c("Nitrogen_log [all]"),show.data=T)
plot_model(WN_dry_mass_M2,type="eff",terms = c("Nitrogen_log [all]", "warming [all]"),show.data=T)
plot_model(WN_dry_mass_M2)
# hmm very small effects (because not standardised?) and nothing significant


## With origSiteID interaction

WN_dry_mass_M3 <- glm(mean~warming*Nitrogen_log*origSiteID,
                      data=dry_mass_df_no_graze,
                      family=gaussian)
summary(WN_dry_mass_M3)
plot(simulateResiduals(WN_dry_mass_M3))
AICc(WN_height_M1,WN_height_M2,WN_dry_mass_M3) # definitely worth including site ID

plot_model(WN_dry_mass_M3,type="eff",terms = c("origSiteID [all]"),show.data=T)
plot_model(WN_dry_mass_M3,type="eff",terms = c("Nitrogen_log [all]"),show.data=T)
plot_model(WN_dry_mass_M3)
# This looks like its got problems, maybe its just because the dry mass is so small?

# Remove three way interaction

WN_dry_mass_M4 <- glm(mean~warming*Nitrogen_log+origSiteID,
                      data=dry_mass_df_no_graze,
                      family=gaussian)
summary(WN_dry_mass_M4)
plot(simulateResiduals(WN_dry_mass_M4))
AICc(WN_dry_mass_M4,WN_dry_mass_M3)

plot_model(WN_dry_mass_M4,type="eff",terms = c("origSiteID [all]"),show.data=T)
plot_model(WN_dry_mass_M4,type="eff",terms = c("Nitrogen_log [all]"),show.data=T)
plot_model(WN_dry_mass_M4,type="eff",terms = c("Nitrogen_log [all]","warming"),show.data=T)
plot_model(WN_dry_mass_M4,type="eff",terms = c("warming [all]"),show.data=T)
plot_model(WN_dry_mass_M4)
# Not bad but some of the residuals are a  bit out

# another option
WN_dry_mass_M5 <- glm(mean~warming*Nitrogen_log+origSiteID+origSiteID:Nitrogen_log,
                      data=dry_mass_df_no_graze,
                      family=gaussian)
summary(WN_dry_mass_M5)
plot(simulateResiduals(WN_dry_mass_M5))
AICc(WN_dry_mass_M4,WN_dry_mass_M3,WN_dry_mass_M5)

# best so far?
plot_model(WN_dry_mass_M5,type="eff",terms = c("origSiteID [all]"),show.data=T)
plot_model(WN_dry_mass_M5,type="eff",terms = c("Nitrogen_log [all]"),show.data=T)
plot_model(WN_dry_mass_M5,type="eff",terms = c("Nitrogen_log [all]","warming"),show.data=T)
plot_model(WN_dry_mass_M5,type="eff",terms = c("Nitrogen_log [all]","origSiteID"),show.data=T)
plot_model(WN_dry_mass_M5,type="eff",terms = c("warming [all]"),show.data=T)
plot_model(WN_dry_mass_M5)


## Other option
WN_dry_mass_M6 <- glm(mean~warming+Nitrogen_log*origSiteID,
                      data=dry_mass_df_no_graze,
                      family=gaussian)
summary(WN_dry_mass_M6)
plot(simulateResiduals(WN_dry_mass_M6)) # this doesnt look good
AICc(WN_dry_mass_M4,WN_dry_mass_M3,WN_dry_mass_M6)

## Other option
WN_dry_mass_M7 <- glm(mean~warming*Nitrogen_log+origSiteID+origSiteID:warming,
                      data=dry_mass_df_no_graze,
                      family=gaussian)
summary(WN_dry_mass_M7)
plot(simulateResiduals(WN_dry_mass_M7))
AICc(WN_dry_mass_M4,WN_dry_mass_M7,WN_dry_mass_M6)

## What about quadratic nitrogen again?

WN_dry_mass_M8 <- glm(mean~warming*poly(Nitrogen_log)+origSiteID,
                      data=dry_mass_df_no_graze,
                      family=gaussian)
summary(WN_dry_mass_M8)
plot(simulateResiduals(WN_dry_mass_M8))
AICc(WN_dry_mass_M4,WN_dry_mass_M3,WN_dry_mass_M8)
# not an improvement

## What about non-logged nitrogen
WN_dry_mass_M9 <- glm(mean~warming*Namount_kg_ha_y+origSiteID,
                      data=dry_mass_df_no_graze,
                      family=gaussian)
summary(WN_dry_mass_M9)
plot(simulateResiduals(WN_dry_mass_M9))
AICc(WN_dry_mass_M4,WN_dry_mass_M9)
# very similar


#### Leaf thickness - Warming x Grazing ------

# Celeste's Models

leafthick_df_no_N <- trait_mean2 %>%
  filter(trait=="leaf_thickness_mm") %>%
  filter(Namount_kg_ha_y ==0)

hist(leafthick_df_no_N$mean)

traits %>%
  filter(trait=="leaf_thickness_mm") %>%
  ggplot(aes(x=value))+
  geom_histogram()

# Warming and grazing interaction

GW_leafthick_M0 <- glm(mean~warming*grazing,
                       data=leafthick_df_no_N,
                       family=gaussian)
summary(GW_leafthick_M0)
plot(simulateResiduals(GW_leafthick_M0))

plot_model(GW_leafthick_M0,type="eff",terms = c("warming [all]"),show.data=T, jitter = T)
plot_model(GW_leafthick_M0,type="eff",terms = c("grazing [all]"),show.data=T, jitter = T)
plot_model(GW_leafthick_M0)

# Warming * grazing * siteorigin interaction
GW_leafthick_M1 <- glm(mean~warming*grazing*origSiteID,
                       data=leafthick_df_no_N,
                       family=gaussian)
summary(GW_leafthick_M1)
plot(simulateResiduals(GW_leafthick_M1))

plot_model(GW_leafthick_M1,type="eff",terms = c("warming [all]"),show.data=T, jitter = T)
plot_model(GW_leafthick_M1,type="eff",terms = c("origSiteID [all]"),show.data=T, jitter = T)
plot_model(GW_leafthick_M0,type="eff",terms = c("grazing [all]"),show.data=T, jitter = T)
plot_model(GW_leafthick_M1)
AICc(GW_leafthick_M0,GW_leafthick_M1) # M0 is actually better

# Now without 3 way interaction

GW_leafthick_M2 <- glm(mean~warming*grazing+origSiteID,
                       data=leafthick_df_no_N,
                       family=gaussian)
summary(GW_leafthick_M2)
plot(simulateResiduals(GW_leafthick_M2))

plot_model(GW_leafthick_M2,type="eff",terms = c("warming [all]"),show.data=T, jitter = T)
plot_model(GW_leafthick_M2,type="eff",terms = c("grazing [all]"),show.data=T, jitter = T)
plot_model(GW_leafthick_M2,type="eff",terms = c("origSiteID [all]"),show.data=T, jitter = T)
plot_model(GW_leafthick_M2)
AICc(GW_leafthick_M2,GW_leafthick_M1,GW_leafthick_M0)
# better but residuals are off due to quadratic effect?

# Now without the interaction with grazing

GW_leafthick_M3 <- glm(mean~warming+grazing+origSiteID,
                       data=leafthick_df_no_N,
                       family=gaussian)
summary(GW_leafthick_M3)
plot(simulateResiduals(GW_leafthick_M3))

plot_model(GW_leafthick_M3,type="eff",terms = c("warming [all]"),show.data=T, jitter = T)
plot_model(GW_leafthick_M3,type="eff",terms = c("grazing [all]"),show.data=T, jitter = T)
plot_model(GW_leafthick_M3,type="eff",terms = c("origSiteID [all]"),show.data=T, jitter = T)
plot_model(GW_leafthick_M3)
AICc(GW_leafthick_M1,GW_leafthick_M2,GW_leafthick_M3) #M3 is best - no interaction


### Checking evidence of interaction, given the curve in the residuals

GW_leafthick_M4 <- glm(mean~warming+grazing+origSiteID+warming:origSiteID,
                       data=leafthick_df_no_N,
                       family=gaussian)
summary(GW_leafthick_M4)
plot(simulateResiduals(GW_leafthick_M4))

plot_model(GW_leafthick_M4,type="eff",terms = c("warming [all]"),show.data=T, jitter = T)
plot_model(GW_leafthick_M4,type="eff",terms = c("grazing [all]"),show.data=T, jitter = T)
plot_model(GW_leafthick_M4,type="eff",terms = c("origSiteID [all]"),show.data=T, jitter = T)
plot_model(GW_leafthick_M4)
AICc(GW_leafthick_M1,GW_leafthick_M2,GW_leafthick_M3,GW_leafthick_M4) #M4 is similar in terms of AICc but
# residuals are a lot better so = best model so far

## Look at grazing interaction too

GW_leafthick_M5 <- glm(mean~warming+grazing+origSiteID+grazing:origSiteID,
                       data=leafthick_df_no_N,
                       family=gaussian)
summary(GW_leafthick_M5)
plot(simulateResiduals(GW_leafthick_M5))
AICc(GW_leafthick_M5,GW_leafthick_M4)
# worse


## Leaf Thickness -  warming*nitrogen ----

leafthick_df_no_graze <- trait_mean2 %>%
  filter(trait=="leaf_thickness_mm") %>%
  filter(grazing =="Control")

hist(leafthick_df_no_graze$mean)

# Warming x nitrogen interaction
WN_leafthick_M1 <- glm(mean~warming*Nitrogen_log,
                       data=leafthick_df_no_graze,
                       family=gaussian)
summary(WN_leafthick_M1)
plot(simulateResiduals(WN_leafthick_M1))
# interaction N.S.

# nothing significant
plot_model(WN_leafthick_M1,type="eff",terms = c("warming [all]"),show.data=T, jitter = T)
plot_model(WN_leafthick_M1)

## With origSiteID interaction
WN_leafthick_M2 <- glm(mean~warming*Nitrogen_log*origSiteID,
                       data=leafthick_df_no_graze,
                       family=gaussian)
summary(WN_leafthick_M2)
plot(simulateResiduals(WN_leafthick_M2))
AICc(WN_leafthick_M1,WN_leafthick_M2) # M1 is better

# Without any interaction

WN_leafthick_M3 <- glm(mean~warming+Nitrogen_log+origSiteID,
                       data=leafthick_df_no_graze,
                       family=gaussian)
summary(WN_leafthick_M3)
plot(simulateResiduals(WN_leafthick_M3))
AICc(WN_leafthick_M3,WN_leafthick_M2,WN_leafthick_M1) # M3 and M1 are similar

plot_model(WN_leafthick_M3,type="eff",terms = c("warming [all]"),show.data=T, jitter=F)


## try with warming x origin site too
WN_leafthick_M4 <- glm(mean~warming*origSiteID+Nitrogen_log,
                       data=leafthick_df_no_graze,
                       family=gaussian)
summary(WN_leafthick_M4)
plot(simulateResiduals(WN_leafthick_M4))
AICc(WN_leafthick_M3,WN_leafthick_M4) #M3/M1 still better

### Trying warming x nitrogen interaction with site id
WN_leafthick_M5 <- glm(mean~warming*Nitrogen_log+origSiteID,
                       data=leafthick_df_no_graze,
                       family=gaussian)
summary(WN_leafthick_M5)
plot(simulateResiduals(WN_leafthick_M5))
AICc(WN_leafthick_M3,WN_leafthick_M5) #M3/M1 still better

### Warming x Nitrogen interaction
WN_leafthick_M6 <- glm(mean~warming+Nitrogen_log*origSiteID,
                       data=leafthick_df_no_graze,
                       family=gaussian)
summary(WN_leafthick_M6)
plot(simulateResiduals(WN_leafthick_M6))
AICc(WN_leafthick_M3,WN_leafthick_M6) # these residuals are strange


### try other interaction
WN_leafthick_M7 <- glm(mean~warming*Nitrogen_log+origSiteID+
                         origSiteID:warming,
                       data=leafthick_df_no_graze,
                       family=gaussian)
summary(WN_leafthick_M7)
plot(simulateResiduals(WN_leafthick_M7))
AICc(WN_leafthick_M3,WN_leafthick_M7)

### try other interaction
WN_leafthick_M8 <- glm(mean~warming*Nitrogen_log+origSiteID+
                         origSiteID:Nitrogen_log,
                       data=leafthick_df_no_graze,
                       family=gaussian)
summary(WN_leafthick_M8)
plot(simulateResiduals(WN_leafthick_M8))
AICc(WN_leafthick_M3,WN_leafthick_M8)

## Is quadratic nitrogen important?

WN_leafthick_M9 <- glm(mean~warming+poly(Nitrogen_log,2)+origSiteID,
    family = gaussian,
    data=leafthick_df_no_graze)
summary(WN_leafthick_M9)
plot(simulateResiduals(WN_leafthick_M9))
AICc(WN_leafthick_M3,WN_leafthick_M9)
# not better
WN_leafthick_M10 <- glm(mean~warming*poly(Nitrogen_log,2)+origSiteID,
                       family = gaussian,
                       data=leafthick_df_no_graze)
summary(WN_leafthick_M10)
plot(simulateResiduals(WN_leafthick_M10))
AICc(WN_leafthick_M3,WN_leafthick_M10)
# not better


## M3 remains the best thus far for leaf thickness
# M1 is similar

# Checking random effects for grazing
# Marginal model
WN_leafthick_glm <- glm(mean~warming*origSiteID+grazing,
                        family = gaussian,
                        data=leafthick_df_no_N)
summary(WN_leafthick_glm)

#R andom intercept
library(glmmTMB)

WN_leafthick_glmm <- glmmTMB(mean~warming*origSiteID+grazing
                            +(1|siteID),
                            family = gaussian,
                            data=leafthick_df_no_N)
summary(WN_leafthick_glmm)
# variance taken up by siteID is tiny - because of inclusion of origsiteID in models

# Random slope
WN_leafthick_glmm2 <- glmmTMB(mean~warming*origSiteID+grazing
                             +(0+siteID|blockID),
                             family = gaussian,
                             data=leafthick_df_no_N)
summary(WN_leafthick_glmm2)

AICc(WN_leafthick_glm, WN_leafthick_glmm, WN_leafthick_glmm2)

# Okay - random effects make the models worse. Celeste also checked origSiteID as random effect and models without is better.

# Check different nitrogen variables:
# Nitrogen as log
WN_leafthick_nlog <- glm(mean~warming*origSiteID+Nitrogen_log,
                         data=leafthick_df_no_graze,
                         family=gaussian)
summary(WN_leafthick_nlog)

#Nitrogen as normal
WN_leafthick_n <- glm(mean~warming*origSiteID+Namount_kg_ha_y,
                      data=leafthick_df_no_graze,
                      family=gaussian)
summary(WN_leafthick_n)

#Nitrogen as level
# the levels don't make sense - let's try and fix it
leafthick_df_no_graze <- leafthick_df_no_graze %>%
  mutate(Ncat = as.factor(Namount_kg_ha_y))
# model
WN_leafthick_l <- glm(mean~warming*origSiteID+Ncat,
                      data=leafthick_df_no_graze,
                      family=gaussian)
summary(WN_leafthick_l, corr = F)

# last try: group into 3 levels, 0-10; 50; 150
leafthick_df_no_graze <- leafthick_df_no_graze %>%
  mutate(Ncats = cut(Namount_kg_ha_y, breaks=c(-Inf, 10, 50, 150),
                     labels=c("low","middle","high")))

# model
WN_leafthick_l2 <- glm(mean~warming*origSiteID+Ncats,
                       data=leafthick_df_no_graze,
                       family=gaussian)
summary(WN_leafthick_l2, corr = F)

AICc(WN_leafthick_nlog, WN_leafthick_n, WN_leafthick_l, WN_leafthick_l2)

# Okay - log nitrogen or normal still remains the best

### List of best models
# Here is the best model and one keeping the interaction: - probably dont want the 3 way interactions now
# Best model - Grazing
GW_leafthick_B <- glm(mean~warming*origSiteID+grazing,
                      data=leafthick_df_no_N,
                      family=gaussian)
summary(GW_leafthick_B)
plot(simulateResiduals(GW_leafthick_B))

plot_model(GW_leafthick_B,type="eff",terms = c("warming [all]"),show.data=T, jitter = T)
plot_model(GW_leafthick_B,type="eff",terms = c("grazing [all]"),show.data=T, jitter = T)
plot_model(GW_leafthick_B,type="eff",terms = c("origSiteID [all]"),show.data=T, jitter = T)
plot_model(GW_leafthick_B)

# Keeping interaction
GW_leafthick_I <- glm(mean~warming*origSiteID*grazing,
                      data=leafthick_df_no_N,
                      family=gaussian)
summary(GW_leafthick_I)
plot(simulateResiduals(GW_leafthick_I))

plot_model(GW_leafthick_I,type="eff",terms = c("warming [all]"),show.data=T, jitter = T)
plot_model(GW_leafthick_I,type="eff",terms = c("grazing [all]"),show.data=T, jitter = T)
plot_model(GW_leafthick_I,type="eff",terms = c("origSiteID [all]"),show.data=T, jitter = T)
plot_model(GW_leafthick_I)

AICc(GW_leafthick_B, GW_leafthick_I)

# Best model - nitrogen
WN_leafthick_B <- glm(mean~warming+Nitrogen_log+origSiteID,
                      data=leafthick_df_no_graze,
                      family=gaussian)
summary(WN_leafthick_B)
plot(simulateResiduals(WN_leafthick_B))

plot_model(WN_leafthick_B,type="eff",terms = c("warming [all]"),show.data=T, jitter = T)
plot_model(WN_leafthick_B,type="eff",terms = c("Nitrogen_log [all]"),show.data=T, jitter = T)
plot_model(WN_leafthick_B,type="eff",terms = c("origSiteID [all]"),show.data=T, jitter = T)
plot_model(WN_leafthick_B)

# Keeping interaction
WN_leafthick_I <- glm(mean~warming*origSiteID*Nitrogen_log,
                      data=leafthick_df_no_graze,
                      family=gaussian)
summary(WN_leafthick_I)
plot(simulateResiduals(WN_leafthick_I))

plot_model(WN_leafthick_I,type="eff",terms = c("warming [all]"),show.data=T, jitter = T)
plot_model(WN_leafthick_I,type="eff",terms = c("Nitrogen_log [all]"),show.data=T, jitter = T)
plot_model(WN_leafthick_I,type="eff",terms = c("origSiteID [all]"),show.data=T, jitter = T)
plot_model(WN_leafthick_I)

AICc(WN_leafthick_B, WN_leafthick_I)

#### Specific leaf area (SLA) - Grazing x Warming ------

# First looking at grazing and warming - without nitrogen

sla_df_no_N <- trait_mean2 %>%
  filter(trait=="sla_cm2_g") %>%
  filter(Namount_kg_ha_y ==0)

hist(sla_df_no_N$mean)

traits %>%
  filter(trait=="sla_cm2_g") %>%
  ggplot(aes(x=value))+
  geom_histogram()

# Warming and grazing interaction

GW_sla_M0 <- glm(mean~warming*grazing,
                 data=sla_df_no_N,
                 family=gaussian)
summary(GW_sla_M0)
plot(simulateResiduals(GW_sla_M0)) # interaction N.S.

plot_model(GW_sla_M0,type="eff",terms = c("warming [all]"),show.data=T, jitter = T)
plot_model(GW_sla_M0,type="eff",terms = c("grazing [all]"),show.data=T, jitter = T)
plot_model(GW_sla_M0)

# Warming * grazing * siteorigin interaction
GW_sla_M1 <- glm(mean~warming*origSiteID*grazing,
                 data=sla_df_no_N,
                 family=gaussian)
summary(GW_sla_M1)
plot(simulateResiduals(GW_sla_M1)) # OrigSiteIDxwarming interaction is Significant

plot_model(GW_sla_M1,type="eff",terms = c("warming [all]"),show.data=T, jitter = T)
plot_model(GW_sla_M1,type="eff",terms = c("origSiteID [all]"),show.data=T, jitter = T)
plot_model(GW_sla_M1,type="eff",terms = c("grazing [all]"),show.data=T, jitter = T)
plot_model(GW_sla_M1)
AICc(GW_sla_M0,GW_sla_M1) # M1 is best

# Now without 3 way interaction

GW_sla_M2 <- glm(mean~warming*origSiteID+grazing,
                 data=sla_df_no_N,
                 family=gaussian)
summary(GW_sla_M2)
plot(simulateResiduals(GW_sla_M2))

plot_model(GW_sla_M2,type="eff",terms = c("warming [all]"),show.data=T, jitter = T)
plot_model(GW_sla_M2,type="eff",terms = c("grazing [all]"),show.data=T, jitter = T)
plot_model(GW_sla_M2,type="eff",terms = c("origSiteID [all]"),show.data=T, jitter = T)
plot_model(GW_sla_M2)
AICc(GW_sla_M2,GW_sla_M1) #M2 is better

# Now tesing grazing x origin interaction

GW_sla_M3 <- glm(mean~warming+grazing*origSiteID,
                 data=sla_df_no_N,
                 family=gaussian)
summary(GW_sla_M3)
plot(simulateResiduals(GW_sla_M3))
AICc(GW_sla_M3,GW_sla_M2)
# worse

# Now with warming x grazing

GW_sla_M4 <- glm(mean~warming*grazing+origSiteID,
                 data=sla_df_no_N,
                 family=gaussian)
summary(GW_sla_M4)
plot(simulateResiduals(GW_sla_M4))

AICc(GW_sla_M4,GW_sla_M2) #M2 is better

# Check with no interactions
GW_sla_M5 <- glm(mean~warming+grazing+origSiteID,
                 data=sla_df_no_N,
                 family=gaussian)
summary(GW_sla_M5)
plot(simulateResiduals(GW_sla_M5))

plot_model(GW_sla_M5,type="eff",terms = c("warming [all]"),show.data=T, jitter = T)
plot_model(GW_sla_M5,type="eff",terms = c("grazing [all]"),show.data=T, jitter = T)
plot_model(GW_sla_M5,type="eff",terms = c("origSiteID [all]"),show.data=T, jitter = T)
plot_model(GW_sla_M5)
AICc(GW_sla_M5,GW_sla_M2) #M2 is still better - here the interactions are defs important

## SLA - warming and nitrogen ----

sla_df_no_graze <- trait_mean2 %>%
  filter(trait=="sla_cm2_g") %>%
  filter(grazing =="Control")

hist(sla_df_no_graze$mean)

# Warming x nitrogen interaction

WN_sla_M1 <- glm(mean~warming*Nitrogen_log,
                 data=sla_df_no_graze,
                 family=gaussian)
summary(WN_sla_M1)
plot(simulateResiduals(WN_sla_M1))
# interaction N.S.


## With three-way interaction

WN_sla_M2 <- glm(mean~warming*Nitrogen_log*origSiteID,
                 data=sla_df_no_graze,
                 family=gaussian)
summary(WN_sla_M2)
plot(simulateResiduals(WN_sla_M2))
AICc(WN_sla_M1,WN_sla_M2) #M2 is better

plot_model(WN_sla_M2,type="eff",terms = c("warming [all]"),show.data=T, jitter=T)
plot_model(WN_sla_M2,type="eff",terms = c("origSiteID [all]"),show.data=T, jitter=T)
plot_model(WN_sla_M2,type="eff",terms = c("warming [all]", "origSiteID"),show.data=T, jitter=T)
plot_model(WN_sla_M2)

# Without interaction

WN_sla_M3 <- glm(mean~warming+Nitrogen_log+origSiteID,
                 data=sla_df_no_graze,
                 family=gaussian)
summary(WN_sla_M3)
plot(simulateResiduals(WN_sla_M3))
AICc(WN_sla_M3,WN_sla_M2) #M3 has better AIC but the residuals suggest a quadratic effect?

# Try some 2 way interactions

WN_sla_M4 <- glm(mean~warming*Nitrogen_log+origSiteID,
                 data=sla_df_no_graze,
                 family=gaussian)
summary(WN_sla_M4)
plot(simulateResiduals(WN_sla_M4))
AICc(WN_sla_M2,WN_sla_M4) # very similar - residuals a bit out



WN_sla_M5 <- glm(mean~warming+Nitrogen_log*origSiteID,
                 data=sla_df_no_graze,
                 family=gaussian)
summary(WN_sla_M5)
plot(simulateResiduals(WN_sla_M5))
AICc(WN_sla_M2,WN_sla_M5) # Similar, residuals out



WN_sla_M6 <- glm(mean~warming*origSiteID+Nitrogen_log,
                 data=sla_df_no_graze,
                 family=gaussian)
summary(WN_sla_M6)
plot(simulateResiduals(WN_sla_M6))
AICc(WN_sla_M2,WN_sla_M6) #M6 is better
# Interaction between warming and originsite

plot_model(WN_sla_M6,type="eff",terms = c("warming [all]"),show.data=T, jitter=F)
plot_model(WN_sla_M6,type="eff",terms = c("origSiteID [all]"),show.data=T, jitter=F)
plot_model(WN_sla_M6,type="eff",terms = c("warming [all]", "origSiteID"),show.data=T, jitter=F)
plot_model(WN_sla_M6) # M6 is the best model

#M6 is the best model

# Check random effects:
# Convergence problems
#Marginal model
# GW_sla_glm <- glm(mean~warming*origSiteID*grazing,
#                   family = gaussian,
#                   data=sla_df_no_N)
# summary(GW_sla_glm)
#
# #Random intercept
# GW_sla_glmm <- glmmTMB(mean~warming*origSiteID*grazing
#                       +(1|siteID),
#                       family = gaussian,
#                       data=sla_df_no_N)
# summary(GW_sla_glmm)
#
# #Random slope
# GW_sla_glmm2 <- glmmTMB(mean~warming*origSiteID*grazing
#                        +(0+siteID|blockID),
#                        family = gaussian,
#                        data=sla_df_no_N)
# summary(GW_sla_glmm2)
#
# AICc(GW_sla_glm, GW_sla_glmm2)

# CM: Interesting - here, site as the random effect seem to improve the model quite a bit. This is confusing. How can site be important for some models but not for others?? I do not know what this means. Do we now need to check random effects for all models?

# Check different nitrogen variables
#Nitrogen as log

GW_sla_nlog <- glm(mean~warming*origSiteID+Nitrogen_log,
                   data=sla_df_no_graze,
                   family=gaussian)
summary(GW_sla_nlog)

#Nitrogen as normal

GW_sla_n <- glm(mean~warming*origSiteID+Namount_kg_ha_y,
                data=sla_df_no_graze,
                family=gaussian)
summary(GW_sla_n)

#Nitrogen as level
# the levels don't make sense - let's try and fix it
sla_df_no_graze <- sla_df_no_graze %>%
  mutate(Ncat = as.factor(Namount_kg_ha_y))
# model
GW_sla_l <- glm(mean~warming*origSiteID+Ncat,
                data=sla_df_no_graze,
                family=gaussian)
summary(GW_sla_l)

# last try: group into 3 levels, 0-10; 50; 150
sla_df_no_graze <- sla_df_no_graze %>%
  mutate(Ncats = cut(Namount_kg_ha_y, breaks=c(-Inf, 10, 50, 150),
                     labels=c("low","middle","high")))

# model
GW_sla_l2 <- glm(mean~warming*origSiteID+Ncats,
                 data=sla_df_no_graze,
                 family=gaussian)
summary(GW_sla_l2)

AICc(GW_sla_nlog, GW_sla_n, GW_sla_l, GW_sla_l2)

# Here normal nitrogen is the best - interestingly, it seems that nitrogen only has an effect at the most extreme level (150)


### List of best model choices - probably don't want these three-way interactions
# The best model and a separate one keeping the interaction
# Best model - Grazing
GW_sla_B <- glm(mean~warming*origSiteID*grazing,
                data=sla_df_no_N,
                family=gaussian)
summary(GW_sla_B)
plot(simulateResiduals(GW_sla_B))

plot_model(GW_sla_B,type="eff",terms = c("warming [all]"),show.data=T, jitter = T)
plot_model(GW_sla_B,type="eff",terms = c("grazing [all]"),show.data=T, jitter = T)
plot_model(GW_sla_B,type="eff",terms = c("origSiteID [all]"),show.data=T, jitter = T)
plot_model(GW_sla_B)

# Here the best model includes the interactions
# Best model - Nitrogen
WN_sla_B <- glm(mean~warming*origSiteID+Nitrogen_log,
                data=sla_df_no_graze,
                family=gaussian)
summary(WN_sla_B)
plot(simulateResiduals(WN_sla_B))

plot_model(WN_sla_B,type="eff",terms = c("warming [all]"),show.data=T, jitter = T)
plot_model(WN_sla_B,type="eff",terms = c("Nitrogen_log"),show.data=T, jitter = T)
plot_model(WN_sla_B,type="eff",terms = c("origSiteID [all]"),show.data=T, jitter = T)

plot_model(WN_sla_B,type="eff",terms = c("origSiteID [all]","warming"),
           show.data=T, jitter = F)+
  theme_classic()+
  labs(x="Site Origin",y="SLA CWM", col = "Treatment",
       title = "SLA warming x site origin model")

plot_model(WN_sla_B)

# Keeping interaction
WN_sla_I <- glm(mean~warming*origSiteID*Nitrogen_log,
                data=sla_df_no_graze,
                family=gaussian)
summary(WN_sla_I)
plot(simulateResiduals(WN_sla_B))

plot_model(WN_sla_I,type="eff",terms = c("warming [all]"),show.data=T, jitter = T)
plot_model(WN_sla_I,type="eff",terms = c("Nitrogen_log"),show.data=T, jitter = T)
plot_model(WN_sla_I,type="eff",terms = c("origSiteID [all]"),show.data=T, jitter = T)
plot_model(WN_sla_I)

AICc(WN_sla_B, WN_sla_I)


### LDMC - Grazing x Warming -----
# Dickson's analyses

# First looking at grazing and warming - without nitrogen

ldmc_df_no_N <- trait_mean2 %>%
  filter(trait=="ldmc") %>%
  filter(Namount_kg_ha_y ==0)

hist(ldmc_df_no_N$mean)

traits %>%
  filter(trait=="ldmc") %>%
  ggplot(aes(x=value))+
  geom_histogram()

# Warming and grazing interaction

GW_ldmc_M0 <- glm(mean~warming*grazing,
                  data=ldmc_df_no_N,
                  family=gaussian)
summary(GW_ldmc_M0)
plot(simulateResiduals(GW_ldmc_M0)) # interaction N.S.


# Warming * grazing * siteorigin interaction

GW_ldmc_M1 <- glm(mean~warming*grazing*origSiteID,
                  data=ldmc_df_no_N,
                  family=gaussian)
summary(GW_ldmc_M1)
plot(simulateResiduals(GW_ldmc_M1)) # OrigSiteIDxwarming interaction is Significant

plot_model(GW_ldmc_M1,type="eff",terms = c("warming [all]"),show.data=T, jitter = F)
plot_model(GW_ldmc_M1,type="eff",terms = c("origSiteID [all]"),show.data=T, jitter = F)
plot_model(GW_ldmc_M1,type="eff",terms = c("grazing [all]"),show.data=T, jitter = F)
plot_model(GW_ldmc_M1,type="eff",terms = c("origSiteID [all]", "warming"),show.data=T, jitter = F)
plot_model(GW_ldmc_M1)
AICc(GW_ldmc_M0,GW_ldmc_M1) # M1 is best with AIC, with AICc its not that different

# Now without 3 way interaction

GW_ldmc_M2 <- glm(mean~warming*grazing+origSiteID,
                  data=ldmc_df_no_N,
                  family=gaussian)
summary(GW_ldmc_M2)
plot(simulateResiduals(GW_ldmc_M2))

plot_model(GW_ldmc_M2,type="eff",terms = c("warming [all]"),show.data=T, jitter = T)
plot_model(GW_ldmc_M2,type="eff",terms = c("grazing [all]"),show.data=T, jitter = T)
plot_model(GW_ldmc_M2,type="eff",terms = c("origSiteID [all]"),show.data=T, jitter = T)
plot_model(GW_ldmc_M2)
AICc(GW_ldmc_M2,GW_ldmc_M1) #M1 is better
# residuals don't look good - interaction is NB

# Now with grazing x site origin interaction

GW_ldmc_M3 <- glm(mean~warming+grazing*origSiteID,
                  data=ldmc_df_no_N,
                  family=gaussian)
summary(GW_ldmc_M3)
plot(simulateResiduals(GW_ldmc_M3))

AICc(GW_ldmc_M3,GW_ldmc_M1) # M3 is better


# Now warming site origin interaction

GW_ldmc_M4 <- glm(mean~warming*origSiteID+grazing,
                  data=ldmc_df_no_N,
                  family=gaussian)
summary(GW_ldmc_M4)
plot(simulateResiduals(GW_ldmc_M4))

plot_model(GW_ldmc_M4,type="eff",terms = c("warming [all]"),show.data=T, jitter = F)
plot_model(GW_ldmc_M4,type="eff",terms = c("grazing [all]"),show.data=T, jitter = F)
plot_model(GW_ldmc_M4,type="eff",terms = c("origSiteID [all]"),show.data=T, jitter = F)
plot_model(GW_ldmc_M4,type="eff",terms = c("warming [all]", "origSiteID"),show.data=T, jitter = F)
plot_model(GW_ldmc_M4)
AICc(GW_ldmc_M4,GW_ldmc_M3) # M4 is very similar
# no real difference
# so how to choose - model averaging?



## LDMC  warming x nitrogen - without grazing-----

ldmc_df_no_graze <- trait_mean2 %>%
  filter(trait=="ldmc") %>%
  filter(grazing =="Control")


hist(ldmc_df_no_graze$mean)

# Warming x nitrogen interaction

WN_ldmc_M1 <- glm(mean~warming*Nitrogen_log,
                  data=ldmc_df_no_graze,
                  family=gaussian)
summary(WN_ldmc_M1)
plot(simulateResiduals(WN_ldmc_M1))
# interaction N.S.

plot_model(WN_ldmc_M1,type="eff",terms = c("warming [all]"),show.data=T, jitter = F)
plot_model(WN_ldmc_M1,type="eff",terms = c("Nitrogen_log [all]"),show.data=T, jitter = F)
plot_model(WN_ldmc_M1)

## With origSiteID interaction

WN_ldmc_M2 <- glm(mean~warming*Nitrogen_log*origSiteID,
                  data=ldmc_df_no_graze,
                  family=gaussian)
summary(WN_ldmc_M2)
plot(simulateResiduals(WN_ldmc_M2))
AICc(WN_ldmc_M1,WN_ldmc_M2) #M1 is better

plot_model(WN_ldmc_M2,type="eff",terms = c("warming [all]"),show.data=T, jitter=T)
plot_model(WN_ldmc_M2,type="eff",terms = c("origSiteID [all]"),show.data=T, jitter=T)
plot_model(WN_ldmc_M2,type="eff",terms = c("warming [all]", "origSiteID"),show.data=T, jitter=T)
plot_model(WN_ldmc_M2)

# Without interaction

WN_ldmc_M3 <- glm(mean~warming+Nitrogen_log+origSiteID,
                  data=ldmc_df_no_graze,
                  family=gaussian)
summary(WN_ldmc_M3)
plot(simulateResiduals(WN_ldmc_M3)) # worse without the interaction
AICc(WN_ldmc_M3,WN_ldmc_M1) #M3 is very similar

# Try some 2 way interactions - nitrogen x site origin

WN_ldmc_M4 <- glm(mean~warming+Nitrogen_log*origSiteID,
                  data=ldmc_df_no_graze,
                  family=gaussian)
summary(WN_ldmc_M4)
plot(simulateResiduals(WN_ldmc_M4))
AICc(WN_ldmc_M1,WN_ldmc_M4) #M1 is better

### Warming x nitrogen
WN_ldmc_M5 <- glm(mean~warming*Nitrogen_log+origSiteID,
                  data=ldmc_df_no_graze,
                  family=gaussian)
summary(WN_ldmc_M5)
plot(simulateResiduals(WN_ldmc_M5))
AICc(WN_ldmc_M1,WN_ldmc_M5) #M1 is better

# Warming X Site

WN_ldmc_M6 <- glm(mean~warming*origSiteID+Nitrogen_log,
                  data=ldmc_df_no_graze,
                  family=gaussian)
summary(WN_ldmc_M6)
plot(simulateResiduals(WN_ldmc_M6))
AICc(WN_ldmc_M1,WN_ldmc_M6) #M6 is better
# Interaction between warming and originsite is significant
plot_model(WN_ldmc_M6,type="eff",terms = c("origSiteID [all]", "warming"),show.data=T, jitter=F)+
  theme_classic()+
  labs(title="LDMC warming x site_origin Model",
       col="Treatment",
       y = "LDMC CWM",
       x= "Site Origin")
plot_model(WN_ldmc_M6,type="eff",terms = c("warming [all]", "origSiteID"),show.data=T, jitter=F)


# Try nitrogen without log
WN_ldmc_M7 <- glm(mean~warming*origSiteID+Namount_kg_ha_y,
                  data=ldmc_df_no_graze,
                  family=gaussian)
summary(WN_ldmc_M7)
plot(simulateResiduals(WN_ldmc_M7))
AICc(WN_ldmc_M6,WN_ldmc_M7)
# m7 is very similar to m6

plot_model(WN_ldmc_M7,type="eff",terms = c("warming [all]"),show.data=T, jitter=T)
plot_model(WN_ldmc_M7,type="eff",terms = c("origSiteID [all]"),show.data=T, jitter=T)
plot_model(WN_ldmc_M7,type="eff",terms = c("Namount_kg_ha_y [all]", "origSiteID"),show.data=T, jitter=T)
plot_model(WN_ldmc_M7)


### Quadratic Nitrogen

WN_ldmc_M8 <- glm(mean~warming*origSiteID+poly(Nitrogen_log,2),
                  data=ldmc_df_no_graze,
                  family=gaussian)
summary(WN_ldmc_M8)
plot(simulateResiduals(WN_ldmc_M8))
AICc(WN_ldmc_M6,WN_ldmc_M8)
# worse


## just testing a couple more combos

WN_ldmc_M9 <- glm(mean~warming*Nitrogen_log+origSiteID+
                     origSiteID:Nitrogen_log,
                   data=ldmc_df_no_graze,
                   family=gaussian)
summary(WN_ldmc_M9)
plot(simulateResiduals(WN_ldmc_M9))
AICc(WN_ldmc_M6,WN_ldmc_M9)


WN_ldmc_M10 <- glm(mean~warming*Nitrogen_log+origSiteID+
                     origSiteID:warming,
                   data=ldmc_df_no_graze,
                   family=gaussian)
summary(WN_ldmc_M10)
plot(simulateResiduals(WN_ldmc_M10))
AICc(WN_ldmc_M6,WN_ldmc_M10)

## Model 6 is the best model


#### Next steps:
# Nice graphs
# Supplementary tables
# Functional Diversity and analyses




