library("targets")
source("libraries.R")

# make the targets that are out of date
# looks for a file called "_targets.R" in the working directory
tar_make()
tar_load_everything()
# view pipeline and show which targets are out of date
tar_visnetwork()

fs::file_show("manuscript/manuscript.pdf")#display pdf
