# Ordered estimates above an aligned panel of the choices that produced them.
# Separate estimand panels keep their own vertical scales.

curve_colours <- c("Published baseline" = "#eb6834", "Reported in original" = "#2a78d6",
                   "Additional combination" = "#8c8b86")
curve_shapes <- c("Published baseline" = 18, "Reported in original" = 17, "Additional combination" = 16)

order_results <- function(manifest, results) {
  results$facet <- if (length(manifest$menu$sample_modes) > 1L)
    paste0(results$panel, " (", results$sample_mode, " sample)") else results$panel
  panels <- unique(results$facet[order(match(results$panel, unique(results$panel)), results$sample_mode)])
  results$facet <- factor(results$facet, levels = panels)
  results <- results[order(results$facet, results$estimate, results$spec_id), ]
  results$rank <- stats::ave(results$estimate, results$facet, FUN = seq_along)
  results$role <- ifelse(results$preferred, "Published baseline",
                         ifelse(results$original, "Reported in original", "Additional combination"))
  results$role <- factor(results$role, levels = names(curve_colours))
  results
}

role_labels <- function(manifest) {
  labels <- names(curve_colours)
  if (manifest$menu$analysis_kind == "synthetic_demo") labels[1L] <- "Preferred specification"
  stats::setNames(labels, names(curve_colours))
}

choice_panel_data <- function(manifest, results) {
  menu <- manifest$menu
  dims <- Filter(function(d) !identical(d$name, menu$panel_by), menu_dimensions(menu))
  if (length(menu$sample_modes) > 1L) dims <- c(dims, list(list(name = "sample_mode", label = "Sample",
    options = lapply(menu$sample_modes, function(m) list(id = m, label = paste(m, "observations"))))))
  rows <- lapply(dims, function(d) {
    labels <- vapply(d$options, function(o) o$label, "")
    ids <- vapply(d$options, function(o) o$id, "")
    do.call(rbind, lapply(seq_along(ids), function(k) data.frame(
      facet = results$facet, rank = results$rank, role = results$role,
      dimension = if (is.null(d$label)) d$name else d$label,
      option = labels[k], chosen = results[[d$name]] == ids[k], stringsAsFactors = FALSE)))
  })
  out <- do.call(rbind, rows)
  out$dimension <- factor(out$dimension, levels = unique(out$dimension))
  out$option <- factor(out$option, levels = rev(unique(out$option)))
  out
}

plot_curve <- function(manifest, results, directory, file_stem = "specification_curve") {
  validation <- validate_results(manifest, results)
  abort_if(!validation$passed, "Plotting is blocked because specifications failed.")
  menu <- manifest$menu
  results <- order_results(manifest, results)
  n_max <- max(table(results$facet))
  point_size <- if (n_max > 150) 0.9 else if (n_max > 60) 1.4 else 2.2
  baseline <- results[results$preferred, c("facet", "estimate")]
  y_label <- if (is.null(menu$estimate_label)) "Estimate" else menu$estimate_label
  theme <- ggplot2::theme_minimal(base_size = 9) +
    ggplot2::theme(panel.grid.minor = ggplot2::element_blank(),
                   panel.grid.major.x = ggplot2::element_blank(),
                   strip.text = ggplot2::element_text(face = "bold", hjust = 0),
                   axis.text.x = ggplot2::element_blank(), axis.ticks.x = ggplot2::element_blank())
  present <- levels(droplevels(results$role))
  labels <- role_labels(manifest)[present]
  top <- ggplot2::ggplot(results, ggplot2::aes(rank, estimate)) +
    ggplot2::geom_hline(yintercept = 0, colour = "grey55", linewidth = 0.35) +
    ggplot2::geom_hline(data = baseline, ggplot2::aes(yintercept = estimate),
                        colour = curve_colours[["Published baseline"]], linewidth = 0.3, linetype = "22") +
    ggplot2::geom_linerange(ggplot2::aes(ymin = ci_lower, ymax = ci_upper), colour = "grey72",
                            linewidth = if (n_max > 150) 0.25 else 0.45) +
    ggplot2::geom_point(ggplot2::aes(colour = role, shape = role, size = role)) +
    ggplot2::scale_colour_manual(values = curve_colours[present], breaks = present, labels = labels) +
    ggplot2::scale_shape_manual(values = curve_shapes[present], breaks = present, labels = labels) +
    ggplot2::scale_size_manual(values = c("Published baseline" = point_size * 1.9,
                                          "Reported in original" = point_size * 1.15,
                                          "Additional combination" = point_size)[present], guide = "none") +
    ggplot2::facet_wrap(~facet, nrow = 1L, scales = "free") +
    ggplot2::labs(x = NULL, y = y_label, colour = NULL, shape = NULL) +
    theme + ggplot2::theme(legend.position = "top", legend.justification = "left",
                           legend.margin = ggplot2::margin(0, 0, 0, 0))
  choices <- choice_panel_data(manifest, results)
  bottom <- ggplot2::ggplot(choices, ggplot2::aes(rank, option)) +
    ggplot2::geom_point(data = choices[!choices$chosen, ], colour = "grey90", size = point_size * 0.55) +
    ggplot2::geom_point(data = choices[choices$chosen, ], colour = "#1f2a33", size = point_size * 0.7) +
    ggplot2::facet_grid(dimension ~ facet, scales = "free", space = "free_y") +
    ggplot2::labs(x = "Specifications ranked by estimate", y = NULL) +
    theme + ggplot2::theme(strip.text.x = ggplot2::element_blank(),
                           strip.text.y = ggplot2::element_text(angle = 0, face = "bold", hjust = 0),
                           panel.grid.major.y = ggplot2::element_line(colour = "grey96"))
  n_options <- nrow(unique(choices[, c("dimension", "option")]))
  figure <- patchwork::wrap_plots(top, bottom, ncol = 1L, heights = c(1.15, max(0.6, n_options * 0.085)))
  size <- menu$figure
  width <- if (is.null(size$width)) 7 else size$width
  height <- if (is.null(size$height)) 6 else size$height
  dir.create(directory, recursive = TRUE, showWarnings = FALSE)
  ggplot2::ggsave(file.path(directory, paste0(file_stem, ".png")), figure, width = width, height = height, dpi = 300)
  ggplot2::ggsave(file.path(directory, paste0(file_stem, ".pdf")), figure, width = width, height = height)
  summaries <- summarise_results(manifest, results)
  utils::write.csv(summaries, file.path(directory, "descriptive_summary.csv"), row.names = FALSE)
  table <- c("\\begin{tabular}{llrrrr}", "\\hline",
             "Panel & Sample & Specifications & Minimum & Median & Maximum \\\\", "\\hline",
             vapply(seq_len(nrow(summaries)), function(i) sprintf("%s & %s & %d & %.4f & %.4f & %.4f \\\\",
               summaries$panel[i], summaries$sample_mode[i], summaries$specifications[i],
               summaries$minimum_estimate[i], summaries$median_estimate[i], summaries$maximum_estimate[i]), ""),
             "\\hline", "\\end{tabular}")
  writeLines(table, file.path(directory, "descriptive_summary.tex"))
  invisible(figure)
}
