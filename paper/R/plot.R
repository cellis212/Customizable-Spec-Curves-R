plot_curve <- function(manifest, results, directory) {
  validation <- validate_results(manifest, results)
  abort_if(!validation$passed, "Plotting is blocked because specifications failed.")
  results <- results[order(results$sample_mode, results$estimate, results$spec_id), ]
  results$rank <- ave(results$estimate, results$sample_mode, FUN = seq_along)
  results$reference <- ifelse(results$preferred, "Reference", "Alternative")
  subtitle <- if (manifest$menu$analysis_kind == "synthetic_demo")
    "Generated data only; pointwise cluster-robust intervals; no joint inference" else
    "Pointwise cluster-robust intervals; no joint inference"
  top <- ggplot2::ggplot(results, ggplot2::aes(rank, estimate)) +
    ggplot2::geom_hline(yintercept = 0, colour = "grey70", linewidth = 0.4) +
    ggplot2::geom_linerange(ggplot2::aes(ymin = ci_lower, ymax = ci_upper), colour = "#506879") +
    ggplot2::geom_point(ggplot2::aes(colour = reference, shape = reference), size = 2.5) +
    ggplot2::scale_colour_manual(values = c(Alternative = "#506879", Reference = "#A54133")) +
    ggplot2::scale_shape_manual(values = c(Alternative = 16, Reference = 18)) +
    ggplot2::facet_grid(. ~ sample_mode) +
    ggplot2::scale_x_continuous(breaks = seq_len(max(results$rank))) +
    ggplot2::labs(title = manifest$menu$title, subtitle = subtitle,
                  x = NULL, y = "Focal coefficient", colour = NULL, shape = NULL) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(legend.position = "bottom", panel.grid.minor = ggplot2::element_blank())
  decisions <- do.call(rbind, lapply(names(manifest$menu$control_blocks), function(block) {
    cbind(results[, c("spec_id", "sample_mode", "rank")],
          decision = block, chosen = results$control_block == block)
  }))
  decisions$decision <- factor(decisions$decision, levels = rev(names(manifest$menu$control_blocks)))
  bottom <- ggplot2::ggplot(decisions, ggplot2::aes(rank, decision)) +
    ggplot2::geom_point(ggplot2::aes(alpha = chosen), colour = "#243D4D", size = 2.5) +
    ggplot2::scale_alpha_manual(values = c(`FALSE` = 0.12, `TRUE` = 1), guide = "none") +
    ggplot2::facet_grid(. ~ sample_mode) +
    ggplot2::scale_x_continuous(breaks = seq_len(max(results$rank))) +
    ggplot2::labs(x = "Specification rank within sample panel", y = "Control block") +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(panel.grid.minor = ggplot2::element_blank())
  figure <- patchwork::wrap_plots(top, bottom, heights = c(2, 1))
  dir.create(directory, recursive = TRUE, showWarnings = FALSE)
  ggplot2::ggsave(file.path(directory, "specification_curve.png"), figure, width = 9, height = 6, dpi = 300)
  ggplot2::ggsave(file.path(directory, "specification_curve.pdf"), figure, width = 9, height = 6)
  summaries <- do.call(rbind, lapply(split(results, results$sample_mode), function(rows) {
    data.frame(sample_mode = rows$sample_mode[1L], specifications = nrow(rows),
      median_coefficient = stats::median(rows$estimate), minimum_coefficient = min(rows$estimate),
      maximum_coefficient = max(rows$estimate), fraction_positive = mean(rows$estimate > 0),
      fraction_pointwise_significant = mean(rows$p_value < 1 - manifest$menu$confidence_level),
      interpretation = "Descriptive within the specified set; not a hypothesis probability",
      stringsAsFactors = FALSE)
  }))
  utils::write.csv(summaries, file.path(directory, "descriptive_summary.csv"), row.names = FALSE)
  table <- c("\\begin{tabular}{lrrr}", "\\hline",
             "Sample & Specifications & Minimum & Maximum \\\\", "\\hline",
             vapply(seq_len(nrow(summaries)), function(i) sprintf("%s & %d & %.4f & %.4f \\\\",
               summaries$sample_mode[i], summaries$specifications[i],
               summaries$minimum_coefficient[i], summaries$maximum_coefficient[i]), ""),
             "\\hline", "\\end{tabular}")
  writeLines(table, file.path(directory, "descriptive_summary.tex"))
  invisible(figure)
}
