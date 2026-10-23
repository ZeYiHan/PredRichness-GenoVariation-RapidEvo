# Load packages/functions
suppressPackageStartupMessages(source('packages.R'))

tar_source()

list(
  pop_data_targets,
  pop_dynamics_targets,
  pop_variation_targets,
  bc_data_targets,
  bc_heatmap_targets,
  bc_diversity_targets,
  tar_quarto(explore, "reports/explore.qmd", quiet = FALSE)
)
