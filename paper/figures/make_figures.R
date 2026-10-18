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

publication_curve <- function(run, panels, path, width, height, heights, base_size = 9.5, y_label = NULL,
                              wrap = 26) {
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
    ggplot2::geom_point(ggplot2::aes(shape = role, colour = role, fill = I(fill), size = I(size)), stroke = 0.45) +
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
  figure <- patchwork::wrap_plots(top, bottom, ncol = 1L, heights = heights)
  ggplot2::ggsave(paste0(path, ".pdf"), figure, width = width, height = height, device = grDevices::pdf)
  # Elsevier requires embedded fonts. R's pdf device never embeds them, so
  # round-trip through PostScript with Ghostscript, which embeds subsets.
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
cat("Figures written to", normalizePath(out), "\n")
