trt_col <- c(Control = "#b2baf8", 
             Glauc = "#8FB89A", 
             Tetra = "#bedc96", 
             P.caud = "#9bd9d3",
             TG = "#ad9898", 
             GP = "#f9ba80", 
             TP = "#D4A0A0", 
             TGP = "#8abfe6")

trt_lab <- c(Control = "Control",
             Glauc   = "*Glaucoma* sp.",
             Tetra   = "*T. pyriformis*",
             P.caud  = "*P. caudatum*",
             TG = "TG", GP = "GP", TP = "TP", TGP = "TGP")
trt_levels <- c("Control", "Tetra", "Glauc", "P.caud", "TG", "GP", "TP", "TGP")

pn_col  <- c(`0` = "#b2baf8", `1` = "#c8d49c", `2` = "#e59f90", `3` = "#8abfe6")
gv_col  <- c(`10` = "grey", `100` = "#8ecbf8", `1000` = "#fdc470")


theme_preddiv <- function(base_size = 11) {
  theme_bw(base_size = base_size) +
    theme(
      panel.border = element_blank(),
      panel.grid = element_blank(),
      axis.line = element_line(colour = "black"),
      legend.text = element_markdown(),
      strip.background = element_rect(colour = "white", fill = "#f0f0f0"),
      plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
      axis.ticks.length = unit(-0.15, "cm"),
      axis.text.x = element_text(margin = margin(t = 0.3, unit = "cm")),
      axis.text.y = element_text(margin = margin(r = 0.3, unit = "cm"))
    )
}

save_plot <- function(plot, path, width = 8, height = 6, dpi = 300) {
  ggsave(path, plot, width = width, height = height, dpi = dpi, create.dir = TRUE)
  path
}