# ==============================================================================
# tests/test_outputs.R — Verify all replication outputs exist and pass sanity checks
#
# Usage: Rscript tests/test_outputs.R   (from repo root)
# ==============================================================================

cat("=== Running output verification tests ===\n\n")

pass <- 0
fail <- 0

check <- function(desc, condition) {
  if (condition) {
    cat(sprintf("  PASS: %s\n", desc))
    pass <<- pass + 1
  } else {
    cat(sprintf("  FAIL: %s\n", desc))
    fail <<- fail + 1
  }
}

# ---------- 1. Existence checks ----------
cat("--- File existence checks ---\n")

expected_tables <- c(
  "output/tables/selectedPrizes.tex",
  "output/tables/summaryStats.tex",
  "output/tables/selectedECPrizes.tex",
  "output/tables/corr.tex",
  "output/tables/prizesByField.tex",
  "output/tables/prizeRankRobustnessTable.tex"
)

expected_figures <- c(
  "output/figures/cum_prize_time.png",
  "output/figures/scatter_time_views.png",
  "output/figures/scatter_moneyprize_time_linear_fit.png",
  "output/figures/scatter_money_views.png",
  "output/figures/moneyPerWinner_prizeLevel.png",
  "output/figures/moneyPerWinner_winnerLevel.png",
  "output/figures/prizesDensityByPhD_finest.pdf",
  "output/figures/prizesDensityByVSacademics_finest.pdf",
  "output/figures/prizesDensityByVS_finest_T1.pdf",
  "output/figures/prizesDensityByVS_finest_T12.pdf",
  "output/figures/prizesDensityFunding.pdf"
)

for (f in c(expected_tables, expected_figures)) {
  check(sprintf("File exists: %s", f), file.exists(f))
}

# ---------- 2. Content sanity checks ----------
cat("\n--- Content sanity checks ---\n")

# selectedPrizes.tex should contain 99 data rows
if (file.exists("output/tables/selectedPrizes.tex")) {
  lines <- readLines("output/tables/selectedPrizes.tex")
  # Count lines that contain "&" (data rows in a LaTeX table)
  data_lines <- grep("&.*&.*&.*&", lines, value = TRUE)
  # Exclude header lines
  data_lines <- data_lines[!grepl("Award Name|Rank|Rating|Field|Tier", data_lines)]
  check("selectedPrizes.tex has ~99 data rows", length(data_lines) >= 95 && length(data_lines) <= 105)
}

# selectedECPrizes.tex should contain 68 data rows
if (file.exists("output/tables/selectedECPrizes.tex")) {
  lines <- readLines("output/tables/selectedECPrizes.tex")
  data_lines <- grep("&.*&", lines, value = TRUE)
  data_lines <- data_lines[!grepl("Award Name|Tier|Field", data_lines)]
  check("selectedECPrizes.tex has ~68 data rows", length(data_lines) >= 64 && length(data_lines) <= 72)
}

# summaryStats.tex should have 8 variables
if (file.exists("output/tables/summaryStats.tex")) {
  lines <- readLines("output/tables/summaryStats.tex")
  data_lines <- grep("&.*&.*&.*&", lines, value = TRUE)
  data_lines <- data_lines[!grepl("Variable|Median|Mean", data_lines)]
  check("summaryStats.tex has 8 variable rows", length(data_lines) >= 7 && length(data_lines) <= 9)
}

# corr.tex should be a 5x5 correlation table
if (file.exists("output/tables/corr.tex")) {
  content <- paste(readLines("output/tables/corr.tex"), collapse = "\n")
  check("corr.tex mentions Survey Rating", grepl("Survey Rating", content))
  check("corr.tex mentions News Mentions", grepl("News Mentions", content))
}

# ---------- 3. Diff tests against expected/ ----------
cat("\n--- Diff tests against expected/ ---\n")

expected_dir <- "expected"
if (dir.exists(expected_dir)) {
  ref_files <- list.files(expected_dir, pattern = "\\.tex$", full.names = TRUE)
  for (ref in ref_files) {
    fname <- basename(ref)
    generated <- file.path("output/tables", fname)
    if (file.exists(generated)) {
      # Filter out comment/timestamp lines before comparing
      strip_comments <- function(x) x[!grepl("^\\s*%", x)]
      ref_content <- strip_comments(readLines(ref))
      gen_content <- strip_comments(readLines(generated))
      matches <- identical(ref_content, gen_content)
      check(sprintf("Diff test: %s matches expected", fname), matches)
    } else {
      check(sprintf("Diff test: %s (generated file missing)", fname), FALSE)
    }
  }
} else {
  cat("  SKIP: expected/ directory not found (run once to populate)\n")
}

# ---------- Summary ----------
cat(sprintf("\n=== Results: %d passed, %d failed ===\n", pass, fail))

if (fail > 0) {
  quit(status = 1)
}
