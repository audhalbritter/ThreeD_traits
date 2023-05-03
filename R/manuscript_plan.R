# Manuscript plan
manuscript_plan <- list(

  # # bibliography
  # tar_target(
  #   name = bibliography,
  #   command = "manuscript/bibliography.bib",
  #   format = "file"
  # ),
  #
  # # add packages to bibliography
  # tar_target(
  #   name = biblio2,
  #   command = rjt.misc::package_citations(
  #     packages = c("targets", "tidyverse", "rmarkdown"),
  #     old_bib = bibliography,
  #     new_bib = "manuscript/bibliography2.bib"),
  #   format = "file"
  # ),

  #render manuscript
  # tar_render(name = ms,
  #            path = "Manuscript/Manuscript.Rmd")

  # Results
  tar_render(name = results,
             path = "manuscript/results.qmd")


)
