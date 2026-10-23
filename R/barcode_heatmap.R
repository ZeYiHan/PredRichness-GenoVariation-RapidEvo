heatmap_top_bc <- function(bc_data, freq_col, rank_col, n_top = 10) {
  mx <- prep_pheatmap_matrix(bc_data, {{ freq_col }}, {{ rank_col }}, n_top)
  plot_bc_pheatmap(mx, prep_pheatmap_anno(bc_data), make_diverging_palette(mx))
}