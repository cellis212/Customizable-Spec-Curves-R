#!/usr/bin/env Rscript
# Publication figures from validated runs. This script only reads frozen
# manifests and their validated results; it estimates nothing.
#
# Usage: Rscript paper/figures/make_figures.R ESTIMATES_DIR OUTPUT_DIR
# ESTIMATES_DIR/<id>/manifest-<version>.json and run-<version>/ for each
# application in paper/config/applications.json.

args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 2L) stop("Usage: make_figures.R ESTIMATES_DIR OUTPUT_DIR")
script <- sub("^--file=", "", commandArgs()[grepl("^--file=", commandArgs())][1L])
root <- normalizePath(file.path(dirname(script), "../.."), mustWork = TRUE)
if (dir.exists(file.path(root, "renv/library")) && requireNamespace("renv", quietly = TRUE))
  renv::load(project = root, quiet = TRUE)
for (file in c("curve.R", "plot.R")) source(file.path(root, "paper/R", file))

load_run <- function(run, manifest_path) {
  manifest <- jsonlite::read_json(manifest_path, simplifyVector = TRUE)
  manifest$menu <- jsonlite::read_json(manifest_path, simplifyVector = FALSE)$menu
  manifest$manifest_hash <- file_sha256(manifest_path)
  results <- utils::read.csv(file.path(run, "results.csv"), stringsAsFactors = FALSE)
  validation <- validate_results(manifest, results)
  stopifnot(validation$passed, validation$baseline_reproduction == "passed")
  list(manifest = manifest, results = results)
}

# Elsevier requires embedded fonts. R's pdf device never embeds them, so
# round-trip through PostScript with Ghostscript, which embeds subsets.
save_figure <- function(figure, path, width, height) {
  ggplot2::ggsave(paste0(path, ".pdf"), figure, width = width, height = height, device = grDevices::pdf)
  if (nzchar(Sys.which("gs"))) {
    postscript <- tempfile(fileext = ".ps")
    embedded <- tempfile(fileext = ".pdf")
    gs <- function(arguments) if (system2("gs", c("-q", "-dNOPAUSE", "-dBATCH", arguments)) != 0L)
      stop("Ghostscript font embedding failed")
    gs(c("-sDEVICE=ps2write", paste0("-sOutputFile=", postscript), shQuote(paste0(path, ".pdf"))))
    gs(c("-sDEVICE=pdfwrite", "-dPDFSETTINGS=/prepress", "-dEmbedAllFonts=true",
         paste0("-sOutputFile=", embedded), postscript))
    file.copy(embedded, paste0(path, ".pdf"), overwrite = TRUE)
  } else warning("Ghostscript not found; PDF fonts are not embedded.")
  ggplot2::ggsave(paste0(path, ".png"), figure, width = width, height = height, dpi = 300)
}

publication_curve <- function(run, panels, path, width, height, heights, base_size = 9.5, y_label = NULL,
                              wrap = 26, first_stage = FALSE, weak_f = 10) {
  manifest <- run$manifest
  menu <- manifest$menu
  results <- order_results(manifest, run$results[run$results$panel %in% panels, , drop = FALSE])
  results$facet <- factor(as.character(results$facet), levels = panels)
  # Mark the published specification in every panel: same choices on every
  # dimension except the one that defines panels.
  preferred <- run$results[run$results$preferred, ]
  same <- rep(TRUE, nrow(results))
  for (d in setdiff(dimension_names(menu), menu$panel_by)) same <- same & results[[d]] == preferred[[d]]
  roles <- c("Published specification", "Reported or described in article", "Other specification")
  results$role <- factor(ifelse(same, roles[1L], ifelse(results$original, roles[2L], roles[3L])), levels = roles)
  present <- roles[roles %in% results$role]
  colours <- stats::setNames(c("#eb6834", "#2a78d6", "#5f5e5a"), roles)
  shapes <- stats::setNames(c(23, 24, 21), roles)
  excludes_zero <- results$ci_lower > 0 | results$ci_upper < 0
  results$fill <- ifelse(excludes_zero, colours[as.character(results$role)], "white")
  # IV: weakly identified specifications (effective F below the threshold) are faded.
  results$alpha <- if (first_stage) ifelse(results$first_stage_f < weak_f, 0.3, 1) else 1
  n_max <- max(table(results$facet))
  size <- if (n_max > 60) 1.25 else 2.3
  results$size <- size * c(2.1, 1.6, 1)[as.integer(results$role)]
  results <- results[order(-as.integer(results$role)), ]  # published specification drawn last
  baseline <- results[results$preferred, c("facet", "estimate")]
  base_theme <- ggplot2::theme_minimal(base_size = base_size) +
    ggplot2::theme(panel.grid.minor = ggplot2::element_blank(), panel.grid.major.x = ggplot2::element_blank(),
                   strip.text = ggplot2::element_text(face = "bold", hjust = 0, size = base_size),
                   axis.text.x = ggplot2::element_blank(), axis.ticks.x = ggplot2::element_blank(),
                   axis.text = ggplot2::element_text(size = base_size * 0.85, colour = "grey20"),
                   plot.margin = ggplot2::margin(2, 4, 2, 2))
  top <- ggplot2::ggplot(results, ggplot2::aes(rank, estimate)) +
    ggplot2::geom_hline(yintercept = 0, colour = "grey45", linewidth = 0.35) +
    ggplot2::geom_linerange(ggplot2::aes(ymin = ci_lower, ymax = ci_upper), colour = "grey80",
                            linewidth = if (n_max > 60) 0.3 else 0.5) +
    ggplot2::geom_hline(data = baseline, ggplot2::aes(yintercept = estimate), colour = colours[[1L]],
                        linewidth = 0.4, linetype = "22") +
    ggplot2::geom_point(ggplot2::aes(shape = role, colour = role, fill = I(fill), size = I(size), alpha = I(alpha)),
                        stroke = 0.45) +
    ggplot2::scale_colour_manual(values = colours, name = "Filled markers: 95% interval excludes zero", drop = TRUE) +
    ggplot2::scale_shape_manual(values = shapes, name = "Filled markers: 95% interval excludes zero", drop = TRUE) +
    ggplot2::facet_wrap(~facet, nrow = 1L, scales = "free", labeller = ggplot2::label_wrap_gen(width = wrap)) +
    ggplot2::labs(x = NULL, y = if (is.null(y_label)) menu$estimate_label else y_label) +
    ggplot2::guides(colour = ggplot2::guide_legend(nrow = 2L, byrow = TRUE, override.aes = list(
      fill = unname(colours[present]), size = c(3.2, 2.8, 2.2)[match(present, roles)])),
                    shape = ggplot2::guide_legend(nrow = 2L)) +
    base_theme + ggplot2::theme(legend.position = "top", legend.justification = "left", legend.location = "plot",
                                legend.title.position = "top",
                                legend.title = ggplot2::element_text(size = base_size * 0.85, colour = "grey30"),
                                legend.margin = ggplot2::margin(0, 0, 0, 0),
                                legend.text = ggplot2::element_text(size = base_size * 0.9))
  choices <- choice_panel_data(manifest, results)
  bottom <- ggplot2::ggplot(choices, ggplot2::aes(rank, option)) +
    ggplot2::geom_point(data = choices[!choices$chosen, ], colour = "grey90", size = size * 0.55) +
    ggplot2::geom_point(data = choices[choices$chosen, ], colour = "#1f2a33", size = size * 0.6) +
    ggplot2::facet_grid(dimension ~ facet, scales = "free", space = "free_y") +
    ggplot2::labs(x = "Specifications ranked by estimate", y = NULL) +
    base_theme + ggplot2::theme(strip.text.x = ggplot2::element_blank(),
                                strip.text.y = ggplot2::element_text(angle = 0, face = "bold", hjust = 0,
                                                                     size = base_size * 0.85),
                                panel.grid.major.y = ggplot2::element_line(colour = "grey96"))
  plots <- list(top)
  if (first_stage) {
    strength <- ggplot2::ggplot(results, ggplot2::aes(rank, first_stage_f)) +
      ggplot2::geom_hline(yintercept = weak_f, colour = "grey45", linewidth = 0.35, linetype = "22") +
      ggplot2::geom_point(ggplot2::aes(colour = role), size = size * 0.55, show.legend = FALSE) +
      ggplot2::scale_colour_manual(values = colours) +
      ggplot2::scale_y_log10() +
      ggplot2::facet_wrap(~facet, nrow = 1L, scales = "free_x") +
      ggplot2::labs(x = NULL, y = sprintf("Effective F\n(dashed: %g)", weak_f)) +
      base_theme + ggplot2::theme(strip.text = ggplot2::element_blank())
    plots <- c(plots, list(strength))
  }
  figure <- patchwork::wrap_plots(c(plots, list(bottom)), ncol = 1L, heights = heights)
  save_figure(figure, path, width, height)
  invisible(results)
}

# Regression discontinuity: estimates against the bandwidth, for each polynomial
# and kernel, with the global published specification marked.
bandwidth_plot <- function(run, path, width, height, base_size = 9) {
  r <- run$results
  r <- r[r$donut == "none" & r$threshold == "pooled" & r$spending %in% c("capital", "instruction"), ]
  r$window <- factor(ifelse(r$bandwidth == "global", "All measures", sub("^h([0-9]+)_.*$", "Within \\1 points", r$bandwidth)),
                     levels = c("All measures", "Within 20 points", "Within 10 points", "Within 5 points"))
  r$kernel <- factor(ifelse(grepl("_tri$", r$bandwidth), "Triangular", "Uniform"), levels = c("Uniform", "Triangular"))
  labels <- c(linear = "Linear", linear_sides = "Linear, separate slopes", quadratic = "Quadratic", cubic = "Cubic (published)")
  r$polynomial <- factor(labels[r$polynomial], levels = labels)
  r$panel <- factor(r$panel, levels = unique(r$panel[order(r$spending != "capital")]))
  palette <- stats::setNames(c("#2a78d6", "#1b9e77", "#7a4fa3", "#eb6834"), labels)
  # Explicit horizontal positions: within each bandwidth, one slot per kernel and polynomial.
  r <- r[order(r$window, r$kernel, r$polynomial), ]
  slot <- stats::ave(seq_len(nrow(r)), r$panel, r$window, FUN = seq_along)
  slots <- stats::ave(seq_len(nrow(r)), r$panel, r$window, FUN = length)
  r$x <- as.integer(r$window) + (slot - (slots + 1) / 2) * 0.09
  figure <- ggplot2::ggplot(r, ggplot2::aes(x, estimate, colour = polynomial, shape = kernel)) +
    ggplot2::geom_hline(yintercept = 0, colour = "grey45", linewidth = 0.35) +
    ggplot2::geom_linerange(ggplot2::aes(ymin = ci_lower, ymax = ci_upper), linewidth = 0.45) +
    ggplot2::geom_point(size = 1.9, fill = "white", stroke = 0.6) +
    ggplot2::geom_point(data = r[r$preferred, ], shape = 23, size = 3.4, colour = "black", fill = "#eb6834",
                        inherit.aes = FALSE, ggplot2::aes(x, estimate)) +
    ggplot2::scale_x_continuous(breaks = seq_along(levels(r$window)), labels = levels(r$window)) +
    ggplot2::scale_colour_manual(values = palette, name = "Polynomial in vote share") +
    ggplot2::scale_shape_manual(values = c(Uniform = 16, Triangular = 21), name = "Kernel") +
    ggplot2::facet_wrap(~panel, ncol = 1L, scales = "free_y") +
    ggplot2::labs(x = "Bandwidth (vote margin of the measure)", y = run$manifest$menu$estimate_label) +
    ggplot2::theme_minimal(base_size = base_size) +
    ggplot2::theme(panel.grid.minor = ggplot2::element_blank(), panel.grid.major.x = ggplot2::element_blank(),
                   strip.text = ggplot2::element_text(face = "bold", hjust = 0), legend.position = "top",
                   legend.justification = "left", legend.box = "vertical", legend.margin = ggplot2::margin(0, 0, 0, 0))
  save_figure(figure, path, width, height)
  invisible(r)
}

# Structural (GMM) curve: converged specifications only (failed rows are counted
# in the caption and kept in the results), the focal parameter on a pseudo-log
# scale (linear near zero, so intervals that cross zero are drawn in full),
# filled markers where the J test does not reject at 5%, crosses at a parameter
# bound, the J-test p-value for each specification, and the choices.
gmm_curve <- function(run, path, width, height, heights, base_size = 8, j_level = 0.05) {
  manifest <- run$manifest
  ok <- run$results[run$results$status == "ok", , drop = FALSE]
  results <- order_results(manifest, ok)
  roles <- c("Published specification", "Reported in article", "Other specification")
  results$role <- factor(ifelse(results$preferred, roles[1L], ifelse(results$original, roles[2L], roles[3L])), levels = roles)
  colours <- stats::setNames(c("#eb6834", "#2a78d6", "#5f5e5a"), roles)
  shapes <- stats::setNames(c(23, 24, 21), roles)
  tested <- is.finite(results$j_p_value)
  results$fill <- ifelse(tested & results$j_p_value >= j_level, colours[as.character(results$role)],
                         ifelse(tested, "white", "grey85"))
  results$size <- 1.1 * c(2.6, 1.8, 1)[as.integer(results$role)]
  results <- results[order(-as.integer(results$role)), ]
  base_theme <- ggplot2::theme_minimal(base_size = base_size) +
    ggplot2::theme(panel.grid.minor = ggplot2::element_blank(), panel.grid.major.x = ggplot2::element_blank(),
                   axis.text.x = ggplot2::element_blank(), axis.ticks.x = ggplot2::element_blank(),
                   axis.text = ggplot2::element_text(size = base_size * 0.85, colour = "grey20"),
                   plot.margin = ggplot2::margin(2, 4, 2, 2))
  bounds <- results[nzchar(results$bound_hits), , drop = FALSE]
  top <- ggplot2::ggplot(results, ggplot2::aes(rank, estimate)) +
    ggplot2::geom_hline(yintercept = 0, colour = "grey45", linewidth = 0.3) +
    ggplot2::geom_linerange(ggplot2::aes(ymin = ci_lower, ymax = ci_upper), colour = "grey85", linewidth = 0.2) +
    ggplot2::geom_hline(yintercept = results$estimate[results$preferred], colour = colours[[1L]], linewidth = 0.4,
                        linetype = "22") +
    ggplot2::geom_point(ggplot2::aes(shape = role, colour = role, fill = I(fill), size = I(size)), stroke = 0.4) +
    ggplot2::geom_point(data = bounds, ggplot2::aes(rank, estimate), shape = 4, size = 1.2, colour = "black",
                        stroke = 0.35, inherit.aes = FALSE) +
    ggplot2::scale_y_continuous(transform = scales::transform_pseudo_log(sigma = 0.5, base = 10),
                                breaks = c(-300, -30, -3, 0, 1, 3, 10, 30, 100, 300)) +
    ggplot2::scale_colour_manual(values = colours, name = "Filled: J test does not reject at 5% (grey: no J test); x: at a bound",
                                 drop = TRUE) +
    ggplot2::scale_shape_manual(values = shapes, name = "Filled: J test does not reject at 5% (grey: no J test); x: at a bound",
                                drop = TRUE) +
    ggplot2::labs(x = NULL, y = quote(atop("Risk aversion" ~ gamma, "(pseudo-log scale)"))) +
    ggplot2::guides(colour = ggplot2::guide_legend(nrow = 1L, override.aes = list(fill = unname(colours), size = c(3, 2.6, 2))),
                    shape = ggplot2::guide_legend(nrow = 1L)) +
    base_theme + ggplot2::theme(legend.position = "top", legend.justification = "left", legend.location = "plot",
                                legend.title.position = "top", legend.margin = ggplot2::margin(0, 0, 0, 0),
                                legend.title = ggplot2::element_text(size = base_size * 0.85, colour = "grey30"),
                                legend.text = ggplot2::element_text(size = base_size * 0.9))
  j_panel <- ggplot2::ggplot(results[tested, ], ggplot2::aes(rank, pmax(j_p_value, 1e-16))) +
    ggplot2::geom_hline(yintercept = j_level, colour = "grey45", linewidth = 0.35, linetype = "22") +
    ggplot2::geom_point(ggplot2::aes(colour = role), size = 0.55, show.legend = FALSE) +
    ggplot2::scale_colour_manual(values = colours) +
    ggplot2::scale_y_log10(limits = c(1e-16, 1), breaks = c(1e-15, 1e-10, 1e-5, 1), labels = c("1e-15", "1e-10", "1e-5", "1")) +
    ggplot2::scale_x_continuous(limits = range(results$rank)) +
    ggplot2::labs(x = NULL, y = sprintf("J-test p-value\n(dashed: %g)", j_level)) + base_theme
  choices <- choice_panel_data(manifest, results)
  bottom <- ggplot2::ggplot(choices, ggplot2::aes(rank, option)) +
    ggplot2::geom_point(data = choices[!choices$chosen, ], colour = "grey92", size = 0.5) +
    ggplot2::geom_point(data = choices[choices$chosen, ], colour = "#1f2a33", size = 0.55) +
    ggplot2::facet_grid(dimension ~ ., scales = "free", space = "free_y") +
    ggplot2::labs(x = "Converged specifications ranked by estimate", y = NULL) +
    base_theme + ggplot2::theme(strip.text.y = ggplot2::element_text(angle = 0, face = "bold", hjust = 0, size = base_size * 0.85),
                                panel.grid.major.y = ggplot2::element_line(colour = "grey96"))
  figure <- patchwork::wrap_plots(list(top, j_panel, bottom), ncol = 1L, heights = heights)
  save_figure(figure, path, width, height)
  invisible(results)
}

application_paths <- function(root, estimates, id) {
  apps <- jsonlite::read_json(file.path(root, "paper/config/applications.json"))$applications
  app <- Filter(function(a) identical(a$id, id), apps)[[1L]]
  list(run = file.path(estimates, id, paste0("run-", app$manifest_version)),
       manifest = file.path(estimates, id, paste0("manifest-", app$manifest_version, ".json")))
}
load_application <- function(id) {
  p <- application_paths(root, args[1L], id)
  load_run(p$run, p$manifest)
}

out <- args[2L]
dir.create(out, recursive = TRUE, showWarnings = FALSE)
bll <- load_application("bll2010")
cz <- load_application("cz_am")
bliq <- load_application("cz_bliq")
btmr <- load_application("btmr2021")
# Sized to the elsarticle text block (about 5.3 inches) so fonts print at their nominal size.
publication_curve(bll, "Log Gini", file.path(out, "Figure_1"), width = 5.4, height = 5.6, heights = c(1.25, 1.4))
publication_curve(cz, unique(cz$results$panel), file.path(out, "Figure_2"), width = 5.4, height = 5.2,
                  heights = c(1.1, 1.2), wrap = 16)
publication_curve(bliq, unique(bliq$results$panel), file.path(out, "Figure_3"), width = 5.4, height = 5.0,
                  heights = c(1.1, 1.1), wrap = 16)
publication_curve(btmr, "All specifications", file.path(out, "Figure_4"), width = 5.4, height = 5.8,
                  heights = c(1.2, 1.5))
publication_curve(bll, c("Log Gini", "Logistic Gini", "Log Theil"), file.path(out, "Figure_S1"),
                  width = 6.5, height = 6.4, heights = c(1.2, 1.45), base_size = 8,
                  y_label = "Effect of deregulation (log or log-odds points)")
# Supplementary Material: design-specific examples.
fi <- load_application("fi2015")
cfr <- load_application("cfr2010")
bp <- load_application("bp2005")
publication_curve(fi, c("Commercial banks", "Placebo lenders"), file.path(out, "Figure_S2"), width = 6.5, height = 7.2,
                  heights = c(1.1, 1.7), base_size = 8, wrap = 30)
publication_curve(cfr, c("Capital outlays per pupil", "Total expenditure per pupil", "Instructional spending per pupil (placebo)"),
                  file.path(out, "Figure_S3"), width = 6.5, height = 7.0, heights = c(1.1, 1.6), base_size = 8, wrap = 15)
bandwidth_plot(cfr, file.path(out, "Figure_S4"), width = 6.5, height = 6.2, base_size = 8.5)
publication_curve(bp, "Rural branches per capita", file.path(out, "Figure_S5"), width = 6.5, height = 8.6,
                  heights = c(1.1, 0.45, 2.4), base_size = 8, first_stage = TRUE)
yogo <- load_application("yogo2006")
gmm_curve(yogo, file.path(out, "Figure_S6"), width = 6.5, height = 8.6, heights = c(1.2, 0.45, 2.6))
cat("Figures written to", normalizePath(out), "\n")
