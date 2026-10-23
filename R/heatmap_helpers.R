# define color Palette function that custom to each plot. White as 0, negative and positive have different colors
make_diverging_palette <- function(data, n_neg = 100, n_pos = 100) {
  data <- as.matrix(data)
  mode(data) <- "numeric"

  mat_vals <- as.numeric(data)
  mat_vals <- mat_vals[!is.na(mat_vals)]

  min_val <- min(mat_vals)
  max_val <- max(mat_vals)

  neg_colors <- colorRampPalette(c('#88CEEF', 'white'))(n_neg)
  neg_breaks <- seq(min_val, 0, length.out = n_neg + 1)

  pos_colors <- colorRampPalette(c('white', '#fdb31a', '#d65c58'))(n_pos) 
  pos_breaks <- seq(0, max_val, length.out = n_pos + 1)

  list(
    colors = c(neg_colors, pos_colors),
    breaks = c(neg_breaks, pos_breaks[-1])
  )
}

prep_pheatmap_matrix <- function(bc_data, freq_col, rank_col, n_top = 10) {
  top_bc <- bc_data |>
    filter({{ rank_col }} <= n_top) |>
    pull(barcode) |>
    unique()

  bc_data |>
    filter(barcode %in% top_bc) |>
    select(barcode, JarID, {{ freq_col }}) |>
    pivot_wider(names_from = JarID, values_from = {{ freq_col }}) |>
    column_to_rownames("barcode") |>
    as.matrix()
}

prep_pheatmap_anno <- function(bc_data){
  bc_data |>
    distinct(JarID, PredNum, PredTrt) |>
    mutate(PredTrt  = factor(PredTrt, levels = trt_levels),
           PredRich = factor(PredNum, levels = names(pn_col))) |>
    select(JarID, PredTrt, PredRich) |>
    column_to_rownames("JarID")
}

plot_bc_pheatmap <- function(pheatmap_matrix, anno_col, color_palette,
                             cluster_cols = FALSE, cluster_rows = TRUE) {
  pheatmap::pheatmap(
    pheatmap_matrix,
    cluster_cols = cluster_cols,
    cluster_rows = cluster_rows,
    border_color = NA,
    treeheight_row = 0,
    annotation_col = anno_col,
    annotation_colors = list(PredTrt = trt_col[trt_levels], PredRich = pn_col),
    angle_col = 45,
    show_rownames = FALSE,
    drop_levels = FALSE,
    color = color_palette$colors,
    breaks = color_palette$breaks
  )
}