library("targets")
source("other code/libraries.R")

# make the targets that are out of date
# looks for a file called "_targets.R" in the working directory
tar_make()
tar_load_everything()
# view pipeline and show which targets are out of date
tar_visnetwork()

fs::file_show("manuscript/manuscript.pdf")#display pdf

ggsave("output/Figure2_WN_traits.png", trait_WN_figure, dpi = 300, height = 3, width = 7)
ggsave("output/Figure3_WG_traits.png", trait_WG_figure, dpi = 300, height = 6, width = 6)
ggsave("output/Figure4_pca.png", trait_pca_figure, dpi = 300, height = 6, width = 6)
ggsave("output/Figure5_itv.png", itv_figure, dpi = 300, height = 8, width = 6)


ggsave("output/FigureSIX_pca.png", full_trait_pca_figure, dpi = 300, height = 6, width = 6)
ggsave("output/FigureSI_Y_itv.png", itv_origin_output, dpi = 300, height = 6, width = 6)
