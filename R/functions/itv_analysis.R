### ITV ANALYSIS ###

# total <- specif.avg <- mean (local mean)
# turnover <- const.avg <- mean_noitv (global mean)
# intraspecific <- specific.avg - const.avg


# Across all treatments
make_ITV_analysis <- function(trait_mean){

  trait_long <- trait_mean |>
    select(-var, -skew, -kurt) |>
    rename(specific_mean = mean, fixed_mean = mean_noitv) |>
    mutate(itv_mean = specific_mean - fixed_mean)  |>
    # make long table
    pivot_longer(cols = c(specific_mean, fixed_mean, itv_mean), names_to = "mean", values_to = "value")

  itv_output <- trait_long %>%
    mutate(nitrogen_fct = factor(Namount_kg_ha_y)) |>
    group_by(class, trait_trans, mean) %>%
    nest() %>%
    # anova for each mean (3x)
    mutate(estimate = map(data, ~{
      mod = aov(value ~ warming * nitrogen_fct + warming * origSiteID + nitrogen_fct * origSiteID + warming * grazing + grazing * origSiteID, data = .x)
      # output tidy results
      estimates = tidy(mod)
    })) %>%
    unnest(estimate)

}


make_ITV_proportions <- function(itv_output){

  variance_part <- itv_output %>%
    # select important columns: sumsq = SS
    select(class, trait_trans, mean, term, ss = sumsq, p = p.value) %>%
    # make wide table
    pivot_wider(names_from = mean, values_from = c(ss, p)) %>%
    # rename columns
    rename("total_ss" = ss_specific_mean, "turnover_ss" = ss_fixed_mean, "intraspecific_ss" = ss_itv_mean,
           total_pval = p_specific_mean, turnover_pval = p_fixed_mean, intraspecific_pval = p_itv_mean) %>%
    # calculate covariation SS
    mutate(covariation_ss = total_ss - turnover_ss - intraspecific_ss,
           # sum of variation ss
           sum_ss = sum(total_ss)) |>

    # calculate proportion explained variation (divide ss by sum_ss)
    mutate(total_prop = total_ss/sum_ss,
           turnover_prop = turnover_ss/sum_ss,
           intraspecific_prop = intraspecific_ss/sum_ss,
           covariation_prop = covariation_ss/sum_ss) %>%
    select(-sum_ss) |>

    # make long table
    pivot_longer(cols = c(total_ss:covariation_prop), names_to = c("process", "variable"), names_sep = "_",values_to = "value") |>
    mutate(variable = recode(variable, "ss" = "sumsq", "pval" = "pvalue", "prop" = "proportion")) |>
    pivot_wider(names_from = variable, values_from = value) |>

    # prettify
    ungroup() %>%
    fancy_trait_name_dictionary(.) |>
    mutate(term = case_when(term == "warming" ~ "W",
                              term == "nitrogen_fct" ~ "N",
                              term == "origSiteID" ~ "O",
                              term == "grazing" ~ "G",
                              term == "warming:nitrogen_fct" ~ "WxN",
                              term == "warming:origSiteID" ~ "WxO",
                              term == "nitrogen_fct:origSiteID" ~ "NxO",
                              term == "warming:grazing" ~ "WxG",
                              term == "origSiteID:grazing" ~ "GxO",
                              TRUE ~ term),
             term = factor(term, levels = c("W", "N", "G", "O", "WxN", "WxG", "WxO", "NxO", "GxO", "Residuals")))

}



make_ITV_plot <- function(variance_part){

# get total variation
  total <- variance_part |>
    filter(process %in% c("intraspecific", "turnover")) |>
    group_by(class, trait_trans, trait_fancy, figure_names, process) |>
    summarise(proportion = sum(proportion)) |>
    mutate(term = "Total") |>
    ggplot(aes(x = figure_names, y = proportion)) +
    geom_col(aes(fill = process)) +
    geom_point(data = variance_part |>
                 filter(process %in% c("total")) |>
                 group_by(class, trait_trans, trait_fancy, figure_names, process) |>
                 summarise(proportion = sum(proportion)) |>
                 mutate(term = "Total"),
               aes(x = figure_names, y = proportion),
               colour = "black", size = 0.5) +
    coord_flip() +
    labs(x = "",
         y = "Proportion variance explained",
         tag = "a)") +
    scale_fill_manual(values = c("#005BBB", "#FFD500")) +
    scale_x_discrete(labels = parse(text = levels(variance_part$figure_names))) +
    facet_wrap(~ term, labeller = label_parsed) +
    theme_bw() +
    theme(legend.position = "top")

  traits <- variance_part |>
    filter(process %in% c("intraspecific", "turnover")) |>
    #bind_rows(total) |>
    mutate(term = factor(term, levels = c("W", "N", "G", "O", "WxN", "WxO", "NxO", "WxG", "GxO", "Residuals"))) |>
    ggplot(aes(x = figure_names, y = proportion)) +
    geom_col(aes(fill = process)) +
    geom_point(data = variance_part |>
                 filter(process %in% c("total")),
               aes(x = figure_names, y = proportion),
               colour = "black", size = 0.5) +
    coord_flip() +
    labs(x = "",
         y = "Proportion variance explained",
         tag = "b)") +
    scale_fill_manual(values = c("#005BBB", "#FFD500")) +
    scale_x_discrete(labels = parse(text = levels(variance_part$figure_names))) +
    facet_wrap(~ term, labeller = label_parsed, ncol = 3) +
    theme_bw() +
    theme(legend.position = "top")

  layout <- "
  AA
  BB
  BB
  BB
"
  total/traits + plot_layout(design = layout, guides = 'collect') & theme(legend.position = "top")

  }


# By origin and for each treatment
make_ITV_analysis_origin <- function(trait_mean){

  trait_long <- trait_mean |>
    select(-var, -skew, -kurt) |>
    rename(specific_mean = mean, fixed_mean = mean_noitv) |>
    mutate(itv_mean = specific_mean - fixed_mean)  |>
    # make long table
    pivot_longer(cols = c(specific_mean, fixed_mean, itv_mean), names_to = "mean", values_to = "value")

  itv_output <- trait_long %>%
    mutate(nitrogen_fct = factor(Namount_kg_ha_y)) |>
    group_by(class, trait_trans, mean, origSiteID) %>%
    nest() %>%
    # anova for each mean (3x)
    mutate(estimate = map(data, ~{
      mod = aov(value ~ warming * nitrogen_fct + warming * grazing, data = .x)
      # output tidy results
      estimates = tidy(mod)
    })) %>%
    unnest(estimate)

}


make_ITV_proportions_origin <- function(itv_output){

  variance_part <- itv_output %>%
    # select important columns: sumsq = SS
    select(class, trait_trans, mean, term, ss = sumsq, p = p.value) %>%
    # make wide table
    pivot_wider(names_from = mean, values_from = c(ss, p)) %>%
    # rename columns
    # rename columns
    rename("total_ss" = ss_specific_mean, "turnover_ss" = ss_fixed_mean, "intraspecific_ss" = ss_itv_mean,
           total_pval = p_specific_mean, turnover_pval = p_fixed_mean, intraspecific_pval = p_itv_mean) %>%
    # calculate covariation SS
    mutate(covariation_ss = total_ss - turnover_ss - intraspecific_ss,
           # sum of variation ss
           sum_ss = sum(total_ss)) |>

    # calculate proportion explained variation (divide ss by sum_ss)
    mutate(total_prop = total_ss/sum_ss,
           turnover_prop = turnover_ss/sum_ss,
           intraspecific_prop = intraspecific_ss/sum_ss,
           covariation_prop = covariation_ss/sum_ss) %>%
    select(-sum_ss) |>

    # make long table
    pivot_longer(cols = c(total_ss:covariation_prop), names_to = c("process", "variable"), names_sep = "_",values_to = "value") |>
    mutate(variable = recode(variable, "ss" = "sumsq", "pval" = "pvalue", "prop" = "proportion")) |>
    pivot_wider(names_from = variable, values_from = value) |>

    # prettify
    ungroup() %>%
    fancy_trait_name_dictionary(.) |>
    mutate(term = case_when(term == "warming" ~ "W",
                            term == "nitrogen_fct" ~ "N",
                            term == "grazing" ~ "G",
                            term == "warming:nitrogen_fct" ~ "WxN",
                            term == "warming:grazing" ~ "WxG",
                            TRUE ~ term),
           term = factor(term, levels = c("W", "N", "G", "WxN", "WxG", "Residuals")))

}



make_ITV_plot_origin <- function(variance_part){

  total <- variance_part |>
    filter(process %in% c("intraspecific", "turnover")) |>
    group_by(origSiteID, class, trait_trans, trait_fancy, figure_names, process) |>
    summarise(proportion = sum(proportion)) |>
    mutate(term = "Total") |>
    ggplot(aes(x = figure_names, y = proportion)) +
    geom_col(aes(fill = process)) +
    geom_point(data = variance_part |>
                 filter(process %in% c("total")) |>
                 group_by(class, origSiteID, trait_trans, trait_fancy, figure_names, process) |>
                 summarise(proportion = sum(proportion)) |>
                 mutate(term = "Total"),
               aes(x = figure_names, y = proportion),
               colour = "black", size = 0.5) +
    coord_flip() +
    labs(x = "",
         y = "Proportion variance explained",
         tag = "a)") +
    scale_fill_manual(values = c("#005BBB", "#FFD500")) +
    scale_x_discrete(labels = parse(text = levels(variance_part$figure_names))) +
    facet_wrap(~ origSiteID, labeller = label_parsed) +
    theme_bw() +
    theme(legend.position = "top")

  traits <- variance_part |>
    filter(process %in% c("intraspecific", "turnover")) |>
    #bind_rows(total) |>
    mutate(term = factor(term, levels = c("W", "N", "G", "WxN", "WxG", "Residuals"))) |>
    ggplot(aes(x = figure_names, y = proportion)) +
    geom_col(aes(fill = process)) +
    geom_point(data = variance_part |>
                 filter(process %in% c("total")),
               aes(x = figure_names, y = proportion),
               colour = "black", size = 0.5) +
    coord_flip() +
    labs(x = "",
         y = "Proportion variance explained",
         tag = "b)") +
    scale_fill_manual(values = c("#005BBB", "#FFD500")) +
    scale_x_discrete(labels = parse(text = levels(variance_part$figure_names))) +
    scale_y_continuous(breaks = c(0, 0.3, 0.6)) +
    facet_grid(origSiteID ~ term, labeller = label_parsed) +
    theme_bw() +
    theme(legend.position = "top")

  layout <- "
  AA
  BB
  BB
  BB
"
  total / traits + plot_layout(design = layout, guides = 'collect') & theme(legend.position = "top")

}



# make_ITV_analysis <- function(trait_mean){
#
#   trait_long <- trait_mean |>
#     select(-var, -skew, -kurt) |>
#     rename(specific_mean = mean, fixed_mean = mean_noitv) |>
#     mutate(itv_mean = specific_mean - fixed_mean)  |>
#     # make long table
#     pivot_longer(cols = c(specific_mean, fixed_mean, itv_mean), names_to = "mean", values_to = "value")
#
#   itv_output <- trait_long %>%
#     group_by(class, trait_trans, mean) %>%
#     nest() %>%
#     # anova for each mean (3x)
#     mutate(estimate = map(data, ~{
#       mod <- aov(value ~ 1, data =  .x)
#       # output tidy results
#       estimates = tidy(mod)
#     })) %>%
#     unnest(estimate)
#
#   return(itv_output)
#
# }


# make_ITV_plot <- function(itv_output){
#
#   variance_part <- itv_output %>%
#     # select important columns: sumsq = SS
#     select(class, trait_trans, mean, term, sumsq) %>%
#     # make wide table
#     pivot_wider(names_from = mean, values_from = sumsq) %>%
#     # rename columns
#     rename("total_ss" = specific_mean, "turnover_ss" = fixed_mean, "intraspecific_ss" = itv_mean) %>%
#     # calculate covariation SS
#     mutate(covariation_ss = total_ss- turnover_ss - intraspecific_ss) |>
#
#     # calculate proportion explained variation (divide ss by total_ss)
#     mutate(total_p = total_ss/total_ss,
#            turnover_p = turnover_ss/total_ss,
#            intraspecific_p = intraspecific_ss/total_ss,
#            covariation_p = covariation_ss/total_ss) %>%
#
#     # make long table
#     pivot_longer(cols = c(total_ss:covariation_p), names_to = c("process", "variable"), names_sep = "_",values_to = "value") |>
#     mutate(variable = recode(variable, "ss" = "sumsq", "p" = "proportion")) |>
#     pivot_wider(names_from = variable, values_from = value)
#
#   Group_plot <- fancy_trait_name_dictionary(variance_part) %>%
#     # filter for processes (turnover and ITV) we are interested in and standardize to 1
#     filter(process %in% c("turnover", "intraspecific")) |>
#     group_by(driver, class, trait_trans) |>
#     mutate(sum = sum(proportion),
#            proportion_standardized = proportion / sum) |>
#     ungroup() |>
#     group_by(driver, class, process) |>
#     summarise(proportion_standardized = mean(proportion_standardized)) |>
#     mutate(process = recode(process, intraspecific = "ITV"),
#            var = "Total") |>
#     mutate(new = paste(class, driver, sep = "_"),
#            new = factor(new, levels = c("Size_WxG", "Size_WxN", "Leaf economics_WxG", "Leaf economics_WxN", "Isotopes_WxG", "Isotopes_WxN"))) |>
#     # make turnover negative
#     mutate(proportion_standardized = if_else(process == "turnover", -1*proportion_standardized, proportion_standardized)) |>
#     ggplot(aes(x = class, y = proportion_standardized, fill = process)) +
#     geom_col() +
#     geom_hline(yintercept = 0, colour = "grey", linetype = "dashed") +
#     scale_x_discrete(limits = rev) +
#     coord_flip() +
#     scale_fill_manual(name = "Process", values = c("#005BBB", "#FFD500")) +
#     scale_alpha_manual(values = c(1, 0.5)) +
#     lims(y = c(-1, 1)) +
#     labs(x = "", y = "Relative contribution",
#          tag = "(a)") +
#     facet_wrap( ~ driver, scales = "free_x") +
#     theme_bw() +
#     theme(text = element_text(size = 18),
#           panel.spacing = unit(1, "cm"))
#
#
#   ITV_plot <- fancy_trait_name_dictionary(variance_part) %>%
#     # remove class part
#     #mutate(figure_names = str_remove(figure_names, "Size~-~|LES~-~|I~-~")) |>
#     #mutate(figure_names = factor(figure_names, levels = c("Height~(cm)", "Dry~mass~(g)", "Area~(cm^2)", "Thickness~(mm)", "SLA~(cm^2*g^{-1})", "LDMC"))) |>
#
#     # filter for processes (turnover and ITV) we are interested in and standardize to 1
#     filter(process %in% c("turnover", "intraspecific")) |>
#     group_by(driver, trait_trans) |>
#     mutate(sum = sum(proportion),
#            proportion_standardized = proportion / sum) |>
#     ungroup() |>
#     mutate(process = recode(process, intraspecific = "ITV")) |>
#     # make turnover negative
#     mutate(proportion_standardized = if_else(process == "turnover", -1*proportion_standardized, proportion_standardized)) |>
#     ggplot(aes(x = figure_names, y = proportion_standardized, fill = process)) +
#     geom_col() +
#     geom_hline(yintercept = 0, colour = "grey", linetype = "dashed") +
#     scale_x_discrete(limits = rev, labels = ggplot2:::parse_safe) +
#     coord_flip() +
#     scale_fill_manual(name = "Process", values = c("#005BBB", "#FFD500")) +
#     scale_alpha_manual(values = c(1, 0.5)) +
#     lims(y = c(-1, 1)) +
#     labs(x = "", y = "Relative contribution",
#          tag = "(b)") +
#     facet_grid( ~ driver, scales = "free", space = "free_y") +
#     theme_bw() +
#     theme(text = element_text(size = 18),
#           panel.spacing = unit(0.3, "cm"),
#           strip.text = element_blank())
#
#   Group_plot / ITV_plot + plot_layout(guides = 'collect', heights = c(1, 4)) & theme(legend.position = 'top')
#
# }
