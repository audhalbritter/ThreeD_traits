##  A quick look at separate models for alpine/subalpine

# only 12 plots for each alpine and sub-alpine
# Kept the interaction between variables as that was what we were interested in
# so didn't bother with model selection

### Height - Warming x Grazing - alpine ----

height_df_no_N_alp <- height_df_no_N %>%
  filter(origSiteID =="Alpine")

GW_alp_height_m1 <- glm(mean~warming*grazing,
                    data=height_df_no_N_alp,
                    family=gaussian)
summary(GW_alp_height_m1)
plot(simulateResiduals(GW_alp_height_m1))

plot_model(GW_alp_height_m1,type="eff",terms = c("warming [all]"),show.data=T, jitter = T)
plot_model(GW_alp_height_m1)


### Height - Warming x Grazing - subalpine ----

height_df_no_N_sub <- height_df_no_N %>%
  filter(origSiteID =="Sub-alpine")

GW_sub_height_m1 <- glm(mean~warming*grazing,
                        data=height_df_no_N_sub,
                        family=gaussian)
summary(GW_sub_height_m1)
plot(simulateResiduals(GW_sub_height_m1))

plot_model(GW_sub_height_m1,type="eff",terms = c("warming [all]"),show.data=T, jitter = F)
plot_model(GW_sub_height_m1)

### Height - Nitrogen x Grazing - alpine ----

height_df_no_graze_alp <- trait_mean2 %>%
  filter(trait=="plant_height_cm") %>%
  filter(grazing =="Control") %>%
  filter(origSiteID =="Alpine")

WN_height_alp_m1 <- glm(mean~warming*Nitrogen_log,
                    data=height_df_no_graze_alp,
                    family=gaussian)
summary(WN_height_alp_m1)
plot(simulateResiduals(WN_height_alp_m1))
# residuals are so odd

plot_model(WN_height_alp_m1,type="eff",terms = c("warming [all]"),show.data=T)
plot_model(WN_height_alp_m1)

### Height - Nitrogen x Grazing - sub-alpine ----

height_df_no_graze_sub <- trait_mean2 %>%
  filter(trait=="plant_height_cm") %>%
  filter(grazing =="Control") %>%
  filter(origSiteID =="Sub-alpine")

WN_height_sub_m1 <- glm(mean~warming*Nitrogen_log,
                        data=height_df_no_graze_sub,
                        family=gaussian)
summary(WN_height_sub_m1)
plot(simulateResiduals(WN_height_sub_m1))
# residuals are still weird

# so no effect at all here

### Leaf Area - Warming x Grazing - Alpine ----

LeafArea_df_no_N_alp <- trait_mean2 %>%
  filter(trait=="leaf_area_cm2") %>%
  filter(Namount_kg_ha_y ==0) %>%
  filter(origSiteID =="Alpine")


# Warming and grazing interaction
GW_leafarea_alp_M1 <- glm(mean~warming*grazing,
                      data=LeafArea_df_no_N_alp,
                      family=gaussian)
summary(GW_leafarea_alp_M1)
plot(simulateResiduals(GW_leafarea_alp_M1))
plot_model(GW_leafarea_alp_M1,type="eff",terms = c("warming [all]"),show.data=T)

### Leaf Area - Warming x Grazing - Alpine ----

LeafArea_df_no_N_alp <- trait_mean2 %>%
  filter(trait=="leaf_area_cm2") %>%
  filter(Namount_kg_ha_y ==0) %>%
  filter(origSiteID =="Alpine")


# Warming and grazing interaction
GW_leafarea_alp_M1 <- glm(mean~warming*grazing,
                          data=LeafArea_df_no_N_alp,
                          family=gaussian)
summary(GW_leafarea_alp_M1)
plot(simulateResiduals(GW_leafarea_alp_M1))
plot_model(GW_leafarea_alp_M1,type="eff",terms = c("warming [all]"),show.data=T)

### Leaf Area - Warming x Grazing - sub-Alpine ----

LeafArea_df_no_N_sub <- trait_mean2 %>%
  filter(trait=="leaf_area_cm2") %>%
  filter(Namount_kg_ha_y ==0) %>%
  filter(origSiteID =="Sub-alpine")


# Warming and grazing interaction
GW_leafarea_sub_M1 <- glm(mean~warming*grazing,
                          data=LeafArea_df_no_N_sub,
                          family=gaussian)
summary(GW_leafarea_sub_M1)
plot(simulateResiduals(GW_leafarea_sub_M1))
plot_model(GW_leafarea_sub_M1,type="eff",terms = c("warming [all]"),show.data=T)
# no effects

### Leaf Area - Warming x Nitrogen - Alpine ----
## So warming x nitrogen - removing natural grazing --

LeafArea_df_no_graze_Alp <- trait_mean2 %>%
  filter(trait=="leaf_area_cm2") %>%
  filter(grazing =="Control") %>%
  filter(origSiteID =="Alpine")

WN_leafarea_alp_M1 <- glm(mean~warming*Nitrogen_log,
                      data=LeafArea_df_no_graze_Alp,
                      family=gaussian)
summary(WN_leafarea_alp_M1)
plot(simulateResiduals(WN_leafarea_alp_M1))
plot_model(WN_leafarea_alp_M1,type="eff",terms = c("warming [all]"),show.data=T)


### Leaf Area - Warming x Nitrogen - sub-alpine ----
## So warming x nitrogen - removing natural grazing --

LeafArea_df_no_graze_sub <- trait_mean2 %>%
  filter(trait=="leaf_area_cm2") %>%
  filter(grazing =="Control") %>%
  filter(origSiteID =="Sub-alpine")

WN_leafarea_sub_M1 <- glm(mean~warming*Nitrogen_log,
                          data=LeafArea_df_no_graze_sub,
                          family=gaussian)
summary(WN_leafarea_sub_M1)
plot(simulateResiduals(WN_leafarea_sub_M1))


### DRY Mass - warming x grazing alpine ----

drymass_df_no_N_alp <- trait_mean2 %>%
  filter(trait=="dry_mass_g") %>%
  filter(Namount_kg_ha_y ==0) %>%
  filter(origSiteID =="Alpine")

# Warming and grazing interaction
GW_drymass_alp_M1 <- glm(mean~warming*grazing,
                     data=drymass_df_no_N_alp,
                     family=gaussian)
summary(GW_drymass_alp_M1)
plot(simulateResiduals(GW_drymass_alp_M1))
plot_model(GW_drymass_alp_M1,type="eff",terms = c("warming [all]"),show.data=T)


### DRY Mass - warming x grazing sub-alpine ----

drymass_df_no_N_sub <- trait_mean2 %>%
  filter(trait=="dry_mass_g") %>%
  filter(Namount_kg_ha_y ==0) %>%
  filter(origSiteID =="Sub-alpine")


# Warming and grazing interaction
GW_drymass_sub_M1 <- glm(mean~warming*grazing,
                         data=drymass_df_no_N_sub,
                         family=gaussian)
summary(GW_drymass_sub_M1)
plot(simulateResiduals(GW_drymass_sub_M1))
plot_model(GW_drymass_sub_M1,type="eff",terms = c("warming [all]"),show.data=T)
# residuals not good

## Dry Mass - warming x nitrogen - Alpine ----

dry_mass_df_no_graze_alp <- trait_mean2 %>%
  filter(trait=="dry_mass_g") %>%
  filter(grazing =="Control") %>%
  filter(origSiteID =="Alpine")

WN_dry_mass_Alp_M1 <- glm(mean~warming*Nitrogen_log,
                      data=dry_mass_df_no_graze_alp,
                      family=gaussian)
summary(WN_dry_mass_Alp_M1)
plot(simulateResiduals(WN_dry_mass_Alp_M1))
plot_model(WN_dry_mass_Alp_M1,type="eff",terms = c("warming [all]"),show.data=T)

# bad residuals

## Dry Mass - warming x nitrogen - sub-alpine ----

dry_mass_df_no_graze_sub <- trait_mean2 %>%
  filter(trait=="dry_mass_g") %>%
  filter(grazing =="Control") %>%
  filter(origSiteID =="Sub-alpine")

WN_dry_mass_sub_M1 <- glm(mean~warming*Nitrogen_log,
                          data=dry_mass_df_no_graze_sub,
                          family=gaussian)
summary(WN_dry_mass_sub_M1)
plot(simulateResiduals(WN_dry_mass_sub_M1))


### The other models quickly...

## Leaf thickness - grazing x warming - alpine----

leafthick_df_no_N_alp <- leafthick_df_no_N %>%
  filter(origSiteID =="Alpine")

# Warming and grazing interaction
GW_leafthick_M1_alp <- glm(mean~warming*grazing,
                       data=leafthick_df_no_N_alp,
                       family=gaussian)
summary(GW_leafthick_M1_alp)
plot(simulateResiduals(GW_leafthick_M1_alp))

## Leaf thickness - grazing x warming - sub-alpine-----

leafthick_df_no_N_sub <- leafthick_df_no_N %>%
  filter(origSiteID =="Sub-alpine")

# Warming and grazing interaction
GW_leafthick_M1_sub <- glm(mean~warming*grazing,
                           data=leafthick_df_no_N_sub,
                           family=gaussian)
summary(GW_leafthick_M1_sub)
plot(simulateResiduals(GW_leafthick_M1_sub))

## SLA- grazing x warming - alpine-----

sla_df_no_N_Alp <- sla_df_no_N %>%
  filter(origSiteID =="Alpine")

# Warming and grazing interaction
GW_sla_M1_alp <- glm(mean~warming*grazing,
                           data=sla_df_no_N_Alp,
                           family=gaussian)
summary(GW_sla_M1_alp)
plot(simulateResiduals(GW_sla_M1_alp))

## SLA - grazing x warming - sub-alpine----

sla_df_no_N_sub <- sla_df_no_N %>%
  filter(origSiteID =="Sub-alpine")

# Warming and grazing interaction
GW_sla_M1_sub <- glm(mean~warming*grazing,
                     data=sla_df_no_N_sub,
                     family=gaussian)
summary(GW_sla_M1_sub)
plot(simulateResiduals(GW_sla_M1_sub))

## LDMC - grazing x warming - alpine----

ldmc_df_no_N_alp <- ldmc_df_no_N %>%
  filter(origSiteID =="Alpine")

GW_ldmc_M1_alp <- glm(mean~warming*grazing,
                  data=ldmc_df_no_N_alp,
                  family=gaussian)
summary(GW_ldmc_M1_alp)
plot(simulateResiduals(GW_ldmc_M1_alp))

## LDMC - grazing x warming - sub-alpine----

ldmc_df_no_N_sub <- ldmc_df_no_N %>%
  filter(origSiteID =="Sub-alpine")

GW_ldmc_M1_sub <- glm(mean~warming*grazing,
                      data=ldmc_df_no_N_sub,
                      family=gaussian)
summary(GW_ldmc_M1_sub)
plot(simulateResiduals(GW_ldmc_M1_sub))

## Now warming and nitrogen

## Leaf thickness - Nitrogen x warming - alpine----

leafthick_df_no_graze_alp <- leafthick_df_no_N %>%
  filter(origSiteID =="Alpine")

# Warming and grazing interaction
WN_leafthick_M1_alp <- glm(mean~warming*grazing,
                           data=leafthick_df_no_graze_alp,
                           family=gaussian)
summary(WN_leafthick_M1_alp)
plot(simulateResiduals(WN_leafthick_M1_alp))

## Leaf thickness - Nitrogen x warming - sub-alpine----

leafthick_df_no_graze_sub <- leafthick_df_no_N %>%
  filter(origSiteID =="Sub-alpine")

# Warming and grazing interaction
WN_leafthick_M1_sub <- glm(mean~warming*grazing,
                           data=leafthick_df_no_graze_sub,
                           family=gaussian)
summary(WN_leafthick_M1_sub)
plot(simulateResiduals(WN_leafthick_M1_sub))


## SLA- Nitrogen x warming - alpine----

sla_df_no_graze_Alp <- sla_df_no_graze %>%
  filter(origSiteID =="Alpine")

# Warming and grazing interaction
WN_sla_M1_alp <- glm(mean~warming*Nitrogen_log,
                     data=sla_df_no_graze_Alp,
                     family=gaussian)
summary(WN_sla_M1_alp)
plot(simulateResiduals(WN_sla_M1_alp))

## SLA - Nitrogen x warming - sub-alpine----

sla_df_no_graze_sub <- sla_df_no_graze %>%
  filter(origSiteID =="Sub-alpine")

# Warming and grazing interaction
WN_sla_M1_sub <- glm(mean~warming*Nitrogen_log,
                     data=sla_df_no_graze_sub,
                     family=gaussian)
summary(WN_sla_M1_sub)
plot(simulateResiduals(WN_sla_M1_sub))

## LDMC - Nitrogen x warming - alpine----

ldmc_df_no_graze_alp <- ldmc_df_no_graze %>%
  filter(origSiteID =="Alpine")

WN_ldmc_M1_alp <- glm(mean~warming*Nitrogen_log,
                  data=ldmc_df_no_graze_alp,
                  family=gaussian)
summary(WN_ldmc_M1_alp)
plot(simulateResiduals(WN_ldmc_M1_alp))

## LDMC - Nitrogen x warming - sub-alpine----

ldmc_df_no_graze_sub <- ldmc_df_no_graze %>%
  filter(origSiteID =="Sub-alpine")

WN_ldmc_M1_sub <- glm(mean~warming*Nitrogen_log,
                      data=ldmc_df_no_graze_sub,
                      family=gaussian)
summary(WN_ldmc_M1_sub)
plot(simulateResiduals(WN_ldmc_M1_sub))

