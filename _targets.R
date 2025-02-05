library("targets")
library("tarchetypes")

tar_option_set(packages = c("dataDownloader", "tidyverse", "performance", "broom", "dataDocumentation", "traitstrap", "ggcorrplot", "vegan", "ggvegan", "glue", "patchwork", "wesanderson", "gt", "ggConvexHull"))

# source other scripts
tar_source()

#Combine target plans
combined_plan <- c(
  download_plan,
  tranformation_plan,
  analysis_plan,
  figure_plan,
  new_plan,
  #manuscript_plan
  si_plan
  #si_figure_plan
)
