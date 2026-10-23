# Part I - Prey Population Dynamics 
# population dynamics data wrangle
pop_data_targets <- tar_plan(
  tar_file(pop_data_folder, "data/raw/PR_GV_flowcytomerty_raw"),
  pop_data_merged = merge_pop_csv(pop_data_folder),
  pop_aggregate = clean_aggregate_pop(pop_data_merged)
)

# population dynamics time seires
pop_dynamics_targets <- tar_plan(
  fig_pop_dynamics = plot_pop_dynamics(pop_aggregate),
  dyn_data = prep_dyn_data(pop_aggregate),
  mod_dyn_autocorr_check = check_dyn_autocorr(dyn_data),
  mod_dyn_pr_gv = mod_dyn_autocorr_check$model,
  anova_pr_gv = car::Anova(mod_dyn_pr_gv),
  mod_dyn_pred_int = fit_dyn_pred_int(dyn_data),
  anova_pred_int = car::Anova(mod_dyn_pred_int)
)

pop_variation_targets <- tar_plan(
  pop_sd = summarise_pop_sd(pop_aggregate),
  fig_sd_richness = plot_sd_richness(pop_sd),
  fig_sd_gv = plot_sd_gv(pop_sd),
  mod_sd = fit_mod_sd(pop_sd),
  anova_sd = car::Anova(mod_sd)
)