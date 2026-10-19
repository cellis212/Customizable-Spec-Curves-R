# Checks for the HTML explorer (paper/figures/make_explorer.R): it writes one
# self-contained page with every frozen specification, keeps failed rows and
# their messages, escapes every value, and refuses incomplete or mismatched results.
args <- commandArgs()
script <- sub("^--file=", "", args[grepl("^--file=", args)][1L])
root <- normalizePath(file.path(dirname(script), "../.."))
if (dir.exists(file.path(root, "renv/library")) && requireNamespace("renv", quietly = TRUE))
  renv::load(project = root, quiet = TRUE)
for (file in c("curve.R", "demo.R")) source(file.path(root, "paper/R", file))

checks <- 0L
check <- function(condition, message) {
  checks <<- checks + 1L
  if (!isTRUE(condition)) stop(message, call. = FALSE)
}
build <- function(output, ...) {
  arguments <- shQuote(c(file.path(root, "paper/figures/make_explorer.R"), output, ...))
  out <- suppressWarnings(system2(file.path(R.home("bin"), "Rscript"), arguments, stdout = TRUE, stderr = TRUE))
  list(status = if (is.null(attr(out, "status"))) 0L else attr(out, "status"), output = paste(out, collapse = "\n"))
}

directory <- tempfile("spec-curve-explorer-test-")
dir.create(run <- file.path(directory, "run"), recursive = TRUE)
menu <- read_menu(file.path(root, "paper/config/demo.json"))
data <- make_demo_data(menu$seed)
manifest_path <- file.path(directory, "manifest.json")
manifest <- freeze_manifest(menu, data, root, manifest_path)
results <- run_manifest(manifest, data)
# A failed row whose message tries to close the page's script element.
message <- 'Focal coefficient is unidentified </script><b>x</b> & "quoted"'
results$status[2L] <- "failed"
results$error[2L] <- message
results[2L, c("estimate", "std_error", "ci_lower", "ci_upper")] <- NA
utils::write.csv(results, file.path(run, "results.csv"), row.names = FALSE, na = "")

page <- file.path(directory, "explorer.html")
built <- build(page, manifest_path, run, "--title", "Test <appendix>")
check(built$status == 0L && file.exists(page), paste("The explorer should build:", built$output))
html <- paste(readLines(page, encoding = "UTF-8", warn = FALSE), collapse = "\n")
# Matching up to the first "<" proves that no raw "<" sits inside the data.
open <- '<script type="application/json" id="explorer-data">'
block <- regmatches(html, regexpr(paste0(open, "[^<]*</script>"), html, fixed = FALSE))
check(length(block) == 1L, "Data must sit in one script element with no raw '<' inside it.")
page_data <- jsonlite::fromJSON(sub("</script>$", "", substring(block, nchar(open) + 1L)), simplifyVector = FALSE)
app <- page_data$applications[[1L]]
check(setequal(unlist(app$data$spec_id), manifest$grid$spec_id), "Every frozen specification must be in the page.")
check(identical(app$data$status[[2L]], "failed") && identical(app$data$error[[2L]], message),
      "A failed row must keep its status and exact message.")
check(is.null(app$data$estimate[[2L]]) && is.numeric(app$data$estimate[[1L]]), "Missing estimates must stay missing.")
check(identical(app$manifest$sha256, manifest$manifest_hash), "The page must record the manifest hash.")
check(identical(app$menu$analysis_kind, "synthetic_demo"), "The synthetic label must reach the page.")
check(grepl("<title>Test &lt;appendix&gt;</title>", html, fixed = TRUE), "The title must be escaped.")
check(!grepl("(src|href)=\"(https?:)?//|@import|url\\(", html), "The page must not load external resources.")
check(!grepl(directory, html, fixed = TRUE), "The page must not contain local paths.")

dir.create(short <- file.path(directory, "short"))
utils::write.csv(results[-3L, ], file.path(short, "results.csv"), row.names = FALSE, na = "")
refused <- build(file.path(directory, "short.html"), manifest_path, short)
check(refused$status != 0L && grepl("lacks 1 frozen specification", refused$output),
      "Results missing a frozen row must stop the explorer.")

dir.create(other <- file.path(directory, "other"))
results$manifest_hash <- strrep("0", 64L)
utils::write.csv(results, file.path(other, "results.csv"), row.names = FALSE, na = "")
mismatch <- build(file.path(directory, "other.html"), manifest_path, other)
check(mismatch$status != 0L && grepl("manifest hash differs", mismatch$output),
      "Results estimated from another manifest must stop the explorer.")

cat("Explorer checks passed:", checks, "\n")
