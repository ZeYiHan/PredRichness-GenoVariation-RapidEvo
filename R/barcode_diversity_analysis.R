#  fig_bc_perc_loss = plot_bc_loss(bc_data_aggregate),
# fig_bc_shannon = plot_bc_shann(bc_data_aggregate),
#   mod_bc_perc_loss = fit_mod_loss(bc_data_aggregate),
#   mod_bc_shann = fit_mod_shann(bc_data_aggregate)

plot_bc_loss <- function(bc_data_aggregate){
  ggplot(bc_data_aggregate) + 
    geom_boxplot(aes(x=PredNum_f, y=perctLoss))+
    geom_jitter(aes(x=PredNum_f, y=perctLoss,color=PredTrt),
      position=position_jitterdodge(jitter.width = 0.4, dodge.width = 0.4))+
    scale_color_manual(values = trt_col, labels = trt_lab, name = "Predator treatment") +
    ylab("Precentage Genotypic Loss")+
    xlab("Predator Richness")+
    facet_grid(~ GV_f, scales = "free")+
    theme_preddiv()
}
  
plot_bc_shann <- function(bc_data_aggregate){
  ggplot(bc_data_aggregate) + 
    geom_boxplot(aes(x=PredNum_f, y=Shannon))+
    geom_jitter(aes(x=PredNum_f, y=Shannon,color=PredTrt),
      position=position_jitterdodge(jitter.width = 0.4, dodge.width = 0.4))+
    scale_color_manual(values = trt_col, labels = trt_lab, name = "Predator treatment") +
    ylab("C. reinhardtii Shannon Diverisity")+
    xlab("Predator Richness")+
    facet_grid(~ GV_f, scales = "free")+
    theme_preddiv()
}

# linear model test PR and GV effects on genotypic loss and diversity
fit_mod_bc_loss <- function(bc_data_aggregate){
  lm(richness ~ PredNum_f * GV_f, data = bc_data_aggregate)
}

fit_mod_bc_shann <- function(bc_data_aggregate){
  lm(richness ~ PredNum_f * GV_f, data = bc_data_aggregate)
}