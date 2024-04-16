# output

tar_load(trait_figure)

ggsave(filename = "other code/output/trait_figure2.png", plot = trait_figure, dpi = 300, width = 8, height = 10)


tar_load(trait_pca_figure)
ggsave(filename = "other code/output/pca.png", plot = trait_pca_figure, dpi = 300, width = 8, height = 6)


tar_load(n_trait_anova)

t <- n_trait_anova |>
  mutate(sumsq = round(sumsq, 2),
         df = round(df, 2),
         statistic = round(statistic, 2),
         p.value = round(p.value, 3)) |>
  rename(Traits = figure_names, Term = term, "Sum of Square" = sumsq, "F" = statistic, "P" = "p.value")

tab1 <- t |>
  filter(Traits %in% c("Height~(cm)", "Dry~mass~(g)", "Area~(cm^2)")) |>
  group_by(Traits) |>
  gt() |>
  tab_options(
    table.font.size = 12,
    data_row.padding = gt::px(1)
  ) |>
  tab_style(
    style = list(
      cell_text(weight = "bold")
    ),
    locations = cells_body(
      columns = c(Term, "Sum of Square", df, "F", P),
      rows = P <= 0.05
    )
  ) |>
  cols_align(
    align = c("left"),
    columns = Term
  )

tab2 <- t |>
  filter(!Traits %in% c("Height~(cm)", "Dry~mass~(g)", "Area~(cm^2)")) |>
  group_by(Traits) |>
  gt() |>
  tab_options(
    table.font.size = 12,
    data_row.padding = gt::px(1)
  ) |>
  tab_style(
    style = list(
      cell_text(weight = "bold")
    ),
    locations = cells_body(
      columns = c(Term, "Sum of Square", df, "F", P),
      rows = P <= 0.05
    )
  )  |>
  cols_align(
    align = c("left"),
    columns = Term
  )

listed_tables <- list(tab1, tab2)

my_tables <- gt_two_column_layout(listed_tables)


gt_two_column_layout(listed_tables <- list(tab1, tab2), output = "save",
                     filename = "other code/output/n_trait_stats.png",
                     vwidth = 680, vheight = 400)



tar_load(g_trait_anova)

gt <- g_trait_anova |>
  ungroup() |>
  select(-trait_trans) |>
  mutate(sumsq = round(sumsq, 2),
         df = round(df, 2),
         statistic = round(statistic, 2),
         p.value = round(p.value, 3)) |>
  rename(Traits = figure_names, Term = term, "Sum of Square" = sumsq, "F" = statistic, "P" = "p.value")

tabg1 <- gt |>
  filter(Traits %in% c("Height~(cm)", "Dry~mass~(g)", "Area~(cm^2)")) |>
  group_by(Traits) |>
  gt() |>
  tab_options(
    table.font.size = 12,
    data_row.padding = gt::px(1)
  ) |>
  tab_style(
    style = list(
      cell_text(weight = "bold")
    ),
    locations = cells_body(
      columns = c(Term, "Sum of Square", df, "F", P),
      rows = P <= 0.05
    )
  ) |>
  cols_align(
    align = c("left"),
    columns = Term
  )

tabg2 <- gt |>
  filter(!Traits %in% c("Height~(cm)", "Dry~mass~(g)", "Area~(cm^2)")) |>
  group_by(Traits) |>
  gt() |>
  tab_options(
    table.font.size = 12,
    data_row.padding = gt::px(1)
  ) |>
  tab_style(
    style = list(
      cell_text(weight = "bold")
    ),
    locations = cells_body(
      columns = c(Term, "Sum of Square", df, "F", P),
      rows = P <= 0.05
    )
  ) |>
  cols_align(
    align = c("left"),
    columns = Term
  )


gt_two_column_layout(listed_tables <- list(tabg1, tabg2), output = "save",
                     filename = "other code/output/g_trait_stats.png",
                     vwidth = 680, vheight = 400)


g_trait_pca[[4]] |> tidy()
n_trait_pca[[4]] |> tidy()
