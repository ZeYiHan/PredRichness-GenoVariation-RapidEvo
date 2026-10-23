# Part II - Prey Evolutionary Outcomes
# processed barcode data wrangle
bc_data_targets <- tar_plan(
  tar_file(bc_shepherd_folder, "data/processed/Shepherd_paired_end"),
  tar_file(jar_ids_file, "data/raw/Jar_identification.csv"),
  jar_ids = read_csv(jar_ids_file, show_col_types = FALSE),
  bc_data_merged = merge_bc_data(bc_shepherd_folder, jar_ids),
  bc_data_cleaned = clean_bc_data(bc_data_merged),
  bc_data_aggregate = aggregate_bc_data(bc_data_cleaned)
)

# plot delta frequency changes'
bc_heatmap_targets <- tar_plan(
  hmp_dlt_gv10 = heatmap_top_bc(filter(bc_data_cleaned, GV == 10), dlt_freq, rank_dlt_freq),
  hmp_dlt_gv100 = heatmap_top_bc(filter(bc_data_cleaned, GV == 100), dlt_freq, rank_dlt_freq),
  hmp_dlt_gv1000 = heatmap_top_bc(filter(bc_data_cleaned, GV == 1000), dlt_freq, rank_dlt_freq)
)

# prey population mutant diversity and loss
bc_diversity_targets <-  tar_plan(
  fig_bc_perc_loss = plot_bc_loss(bc_data_aggregate),
  fig_bc_shannon = plot_bc_shann(bc_data_aggregate),
  mod_bc_perc_loss = fit_mod_bc_loss(bc_data_aggregate),
  anova_bc_loss = car::Anova(mod_bc_perc_loss),
  mod_bc_shann = fit_mod_bc_shann(bc_data_aggregate),
  anova_bc_shann = car::Anova(mod_bc_perc_loss)
)

# Clustering analysis on genotypic frequency outcome on day 21
bc_nmds_targets <- tar_plan(
  tar_map(
    values = tibble(gv = c(10, 100, 1000)),
    names  = gv,
    tar_target(bc_cluster, prep_cluster_mx(filter(bc_data_cleaned, GV == gv), freq_d21))
  )
)