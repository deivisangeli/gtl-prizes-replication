# ==============================================================================
# run_all.R — Master script for "The Missing Nobels" replication package
#
# Usage: Rscript run_all.R
#
# Sources all code/*.R scripts in numeric order. Each runs in an isolated
# environment to prevent variable leakage between scripts.
# ==============================================================================

cat("=== The Missing Nobels — Replication Package ===\n\n")

# Ensure we are in the repo root
if (!file.exists("_helpers.R")) {
  stop("run_all.R must be executed from the repository root directory.")
}

# Create output directories if they don't exist
dir.create("output/tables",  recursive = TRUE, showWarnings = FALSE)
dir.create("output/figures", recursive = TRUE, showWarnings = FALSE)

scripts <- list.files("code", pattern = "^\\d+_.*\\.R$", full.names = TRUE)
scripts <- sort(scripts)

cat(sprintf("Found %d scripts to run.\n\n", length(scripts)))

t0 <- Sys.time()

for (s in scripts) {
  cat(sprintf("Running %s ... ", basename(s)))
  t1 <- Sys.time()
  tryCatch({
    source(s, local = new.env(parent = globalenv()))
    elapsed <- round(difftime(Sys.time(), t1, units = "secs"), 1)
    cat(sprintf("done (%.1fs)\n", elapsed))
  }, error = function(e) {
    cat(sprintf("FAILED\n  Error: %s\n", conditionMessage(e)))
  })
}

total <- round(difftime(Sys.time(), t0, units = "secs"), 1)
cat(sprintf("\n=== All scripts completed in %.1f seconds ===\n", total))
