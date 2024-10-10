library("targets")
source("other code/libraries.R")

# make the targets that are out of date
# looks for a file called "_targets.R" in the working directory
tar_make()
tar_load_everything()
# view pipeline and show which targets are out of date
tar_visnetwork()

fs::file_show("manuscript/manuscript.pdf")#display pdf

# ggsave("n_fig.png", nitrogen_figure, dpi = 300, height = 8, width = 6)
# ggsave("g_fig.png", grazing_figure, dpi = 300, height = 10, width = 6)
# ggsave("itv.png", itv_figure, dpi = 300, height = 6, width = 6)
# ggsave("itv2.png", itv2_figure, dpi = 300, height = 8, width = 8)
