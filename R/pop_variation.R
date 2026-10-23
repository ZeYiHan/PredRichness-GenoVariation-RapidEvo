# Summary data
summarise_pop_sd <- function(pop_aggregate) {
  pop_aggregate |>
    filter(Day > 0) |>
    droplevels()|>
    group_by(Day_f, GV_f, PredNum_f) |>
    summarise(
      mean_d  = mean(Dens),
      sd_d    = sd(Dens),
      mean_ln = mean(lnDens),
      sd_ln   = sd(lnDens),
      .groups = "drop"
    )
}

# Figures 
plot_sd_richness <- function(pop_sd) {
  ggplot(pop_sd, aes(factor(PredNum_f), sd_ln)) +
    geom_boxplot(width = 0.6, outlier.shape = NA) +
    geom_jitter(aes(color = factor(PredNum_f)),
                position = position_jitter(width = 0.2, seed = 1),
                shape = 16, size = 2) +
    scale_color_manual(values = pn_col, guide = "none") +
    facet_grid(~ Day_f, labeller = as_labeller(\(v) paste("Day", v))) +
    labs(x = "Number of predator species", y = "SD in ln(density)") +
    theme_preddiv()
}

plot_sd_gv <- function(pop_sd) {
  ggplot(pop_sd, aes(factor(GV_f), sd_ln)) +
    geom_boxplot(width = 0.5, outlier.shape = NA) +
    geom_jitter(aes(color = factor(GV_f)),
                position = position_jitter(width = 0.2, seed = 1),
                shape = 16, size = 2) +
    scale_color_manual(values = gv_col, guide = "none") +
    facet_grid(~ Day_f, labeller = as_labeller(\(v) paste("Day", v))) +
    labs(x = "Genetic variation", y = "SD in ln(density)") +
    theme_preddiv()
}

fit_mod_sd <- function(pop_sd) {
  glm(sd_ln ~ GV_f *Day_f +  PredNum_f * Day_f,
      family = Gamma(link = "log"),
      data = pop_sd)
}