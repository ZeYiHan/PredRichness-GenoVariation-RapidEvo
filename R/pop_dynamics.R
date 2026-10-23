# --------------------------------------------------------------------
# plot full Chlamydomonas reinhardtii population dynamics 
# --------------------------------------------------------------------
plot_pop_dynamics <- function(pop_aggregate, dodge = 4) {
  pd <- position_dodge(width = dodge)

  ggplot(pop_aggregate,
              aes(x = Day, y = lnDens, color = PredTrt, group = PredTrt)) +
    geom_point(position = pd, alpha = 0.9) + 
    stat_summary(fun = mean, geom = "line",  position = pd, linewidth = 0.6) +
    stat_summary(fun = mean, geom = "point", position = pd, size = 3, alpha = 0.5) +
    stat_summary(fun.data = mean_se, geom = "errorbar",
                          position = pd, width = 3, linewidth = 0.6) +
    scale_color_manual(values = trt_col, labels = trt_lab, name = "Predator treatment") +
    facet_grid(GV ~ PredNum) +
    labs(x = "Day", y = "ln(density) cells/mL") +
    theme_preddiv()
}


# --------------------------------------------------------------------
# Linear mix models for population dynamics
# --------------------------------------------------------------------
# We perform hypothesis testing for our set question: 
#    - do predator diversity and prey genotypic diveristy interactively affect prey population dynamics?
#    - do predator presence interactively, instead of additively, affect prey population dynamics? 

# prep data for model
prep_dyn_data <- function(pop_aggregate) {
  filter(pop_aggregate, Day > 0) |>
    droplevels() |>
  mutate(Time = as.integer(Day_f)) 
}

# 2. Hypothesis testing 1 - PR GV interactive effects
# Fixed effects: predator richness as factor, genotypic variation as factor, Day as factor
# unique predator composition treatments are included in random effects

# Temporal autocorrelation check 
check_dyn_autocorr <- function(dyn_data) {
  fixed <- lnDens ~ PredNum_f * GV_f * Day_f
  ctrl  <- lmeControl(opt = "optim", maxIter = 200, msMaxIter = 200)

  fit <- function(cor = NULL) {
    lme(fixed, data = dyn_data, random = ~ 1 | PredTrt/Jar,
        correlation = cor, control = ctrl, method = "REML")
  }
# check different autoregression structure
  fits <- list(
    AR0 = fit(),
    AR1 = fit(corAR1(form = ~ Time | PredTrt/Jar)),
    AR2 = fit(corARMA(form = ~ Time | PredTrt/Jar, p = 2))
  )

  aic <- data.frame(model = names(fits),
             AIC = vapply(fits, AIC, numeric(1))) |>
          mutate(dAIC = AIC - min(AIC))
  
  best_mod <- names(fits)[which.min(aic$AIC)]

  list(aic = aic, best_mod = best_mod, model = fits[[best_mod]])
}
# Model with AR 1 has lowest AIC. It will be the final model for hypothesis testing PR, GV effects


# Hypothesis testing 2 - Predator presence interactively effects
fit_dyn_pred_int <- function(dyn_data){
  lme(lnDens ~ GV_f * Day_f * Tetra * Glauc * Pcaud,random = ~ 1 |Jar,
    correlation = corAR1(form = ~ Time |Jar),data = dyn_data, method = "REML")
}
