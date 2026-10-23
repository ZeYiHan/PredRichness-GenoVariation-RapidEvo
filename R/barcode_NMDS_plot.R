prep_cluster_mx <- function(bc_data, freq_col) {
  wide <- bc_data |>
    ungroup() |>
    select(JarID, PredNum, PredTrt, barcode, {{ freq_col }}) |>
    pivot_wider(names_from = barcode, values_from = {{ freq_col }}, values_fill = 0)

  trt_info <- select(wide, JarID, PredNum, PredTrt)

  mx <- wide |>
    select(-c(JarID, PredNum, PredTrt)) |>
    as.matrix()
  mx <- mx[, apply(mx, 2, var) > 0, drop = FALSE]
  rownames(mx) <- trt_info$JarID

  list(mx = mx, trt_info = trt_info)
}

plot_bc_NMDS <- function(nmds, trt_info, title = NULL, pt_col = trt_col,
                         stress_pos = c(-1, -0.9)) {
  pred_num <- factor(trt_info$PredNum, levels = names(pn_col))

  vegan::ordiplot(nmds, type = "n")
  points(nmds, "species", pch = 3, col = "#d3d3d3", cex = 0.5)
  vegan::ordihull(nmds, groups = pred_num, draw = "polygon",
                  border = "white", col = pn_col, label = FALSE)
  points(nmds, "sites",
         pch = c(16, 17, 18, 8)[as.integer(pred_num)],
         col = pt_col[as.character(trt_info$PredTrt)],
         cex = 1.6)
  text(stress_pos[1], stress_pos[2], sprintf("Stress: %.3f", nmds$stress))
  title(title)
}