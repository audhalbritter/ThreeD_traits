#rename traits to fancy names for figures

fancy_trait_name_dictionary <- function(dat){

  dat <- dat %>%
    mutate(trait_fancy = recode(trait_trans,
                                plant_height_cm_log = "Height cm",
                                dry_mass_g_log = "Dry mass g",
                                leaf_area_cm2_log = "Area cm2",
                                leaf_thickness_mm_log = "Thickness mm",
                                ldmc = "LDMC",
                                sla_cm2_g = "SLA cm2/g",
                                N_percent = "N %",
                                NP_ratio = "NP",
                                P_percent = "P %",
                                C_percent = "C %",
                                CN_ratio = "CN",
                                dC13_permil = "δC13 ‰",
                                dN15_permil = "δN15 ‰")) |>
    mutate(figure_names = case_match(trait_trans,
                                     "plant_height_cm_log" ~ "Plant~height~(cm)",
                                     "dry_mass_g_log" ~ "Leaf~dry~mass~(g)",
                                     "leaf_area_cm2_log" ~ "Leaf~area~(cm^2)",
                                     "leaf_thickness_mm_log" ~ "Leaf~thickness~(mm)",
                                     "ldmc" ~ "LDMC",
                                     "sla_cm2_g" ~ "SLA~(cm^2*g^{-1})")) |>
    mutate(figure_names = factor(figure_names,
                                 levels = c("Plant~height~(cm)", "Leaf~dry~mass~(g)", "Leaf~area~(cm^2)", "Leaf~thickness~(mm)", "SLA~(cm^2*g^{-1})", "LDMC")))
                                     # "C_percent" ~ "C~('%')",
                                     # "N_percent" ~ "N~('%')",
                                     # "CN_ratio" ~ "CN",
                                     # "P_percent" ~ "P~('%')",
                                     # "NP_ratio" ~ "NP",
                                     # "dC13_permil" ~ "δ^{13}~C~'(‰)'",
                                     # "dN15_permil" ~ "δ^{15}~N~'(‰)'"))
    # # add class
    # mutate(class = case_when(trait_trans %in% c("Plant_Height_cm_log", "Dry_Mass_g_log", "Leaf_Area_cm2_log", "Thickness_mm_log", "Shoot_Length_cm_log", "Shoot_Length_Green_cm_log") ~ "Size",
    #                          trait_trans %in% c("dC13_permil", "dN15_permil") ~ "Isotopes",
    #                          TRUE ~ "Leaf economics"),
    #        class = factor(class, levels = c("Size", "Leaf economics", "Isotopes")))

  return(dat)
}
