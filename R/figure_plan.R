# figure plan
figure_plan <- list(

  # trait means
  tar_target(
    name = trait_figure_alpine,
    command = fancy_trait_name_dictionary(trait_mean) |>
      mutate(Nitrogen_log = log(Namount_kg_ha_y + 1)) |>
      filter(origSiteID == "Alpine") |> ungroup() |>
      ggplot(aes(x = Nitrogen_log, y = mean, colour = warming, linetype = grazing)) +
      geom_point() +
      geom_smooth(method = "lm") +
      scale_colour_manual(values = c("grey30", "#FD6467")) +
      labs(x = bquote(log(Nitrogen)~kg~ha^-1~y^-1),
           y = "Mean trait value", title = "Alpine") +
      facet_wrap(~ trait_fancy, scales = "free") +
      theme_bw()
  ),

  tar_target(
    name = trait_figure_subalpine,
    command = fancy_trait_name_dictionary(trait_mean) |>
      mutate(Nitrogen_log = log(Namount_kg_ha_y + 1)) |>
      filter(origSiteID == "Sub-alpine") |> ungroup() |>
      ggplot(aes(x = Nitrogen_log, y = mean, colour = warming, linetype = grazing)) +
      geom_point() +
      geom_smooth(method = "lm") +
      scale_colour_manual(values = c("grey30", "#FD6467")) +
      labs(x = bquote(log(Nitrogen)~kg~ha^-1~y^-1),
           y = "Mean trait value", title = "Sub-alpine") +
      facet_wrap(~ trait_fancy, scales = "free") +
      theme_bw()
  )

)
