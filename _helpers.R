# ==============================================================================
# Shared configuration for "The Missing Nobels" replication package
# ==============================================================================

# Packages
pacman::p_load(
  tidyverse, readxl, ggplot2, dplyr, tidyr, scales,
  ggrepel, ggforce, ggbreak,
  stargazer, openxlsx, estimatr,
  sandwich, lmtest, Hmisc, xtable, psych,
  LaplacesDemon
)

# Paths (relative to repo root)
data_dir   <- "data"
output_dir <- "output"
table_dir  <- file.path(output_dir, "tables")
figure_dir <- file.path(output_dir, "figures")

# Broad-area color mapping (used across all density plots)
color_map <- c(
  "Math & Phys. sci" = "#1A237E",
  "Applied sci"      = "#8B4577",
  "Life & Health"    = "#00A5A5",
  "Social sci"       = "#E7298A",
  "Other"            = "gray50"
)

# Broad-area assignment function
assign_broad_area <- function(field) {
  dplyr::case_when(
    field %in% c("Mathematics","Physics & Astronomy",
                 "Chemistry & Chemical Eng.") ~ "Math & Phys. sci",
    field %in% c("Computer Science","Computer & Electrical Eng.",
                 "Mechanical & Aerospace Eng.",
                 "Civil & Env. Eng.","Biomedical Eng.",
                 "Other Engineering",
                 "Geosciences","Climate & Ocean Sci.",
                 "Agriculture & Food Sci.") ~ "Applied sci",
    field %in% c("Molecular Bio & Genetics",
                 "Microbiology & Immunology","Neuroscience",
                 "Ecology & Evolution",
                 "Public Health",
                 "Medicine",
                 "Dentistry & Allied Health") ~ "Life & Health",
    field %in% c("Psychology","Economics","Business",
                 "Political Science & Sociology","Anthropology",
                 "Other Social Sci.") ~ "Social sci",
    TRUE ~ "Other"
  )
}

# Default transparency for density plots
alpha <- 0.4

# Shared density plotting function (used by scripts 07-11)
make_density_plot <- function(results, denom_label, denom_unit, total_size) {
  results <- results[order(-results$density), ]
  results$fieldSize_pct <- results$size / sum(results$size) * 100
  results$AccFieldSize <- cumsum(results$fieldSize_pct)
  results$lagAccFieldSize <- dplyr::lag(results$AccFieldSize)
  results$lagAccFieldSize[1] <- 0
  results$broadArea <- assign_broad_area(results$field)
  results$midX <- (results$lagAccFieldSize + results$AccFieldSize) / 2

  yMax <- max(results$density) * 1.08
  avgDensity <- weighted.mean(results$density, w = results$fieldSize_pct)
  labelBuffer <- yMax * 0.04

  tallThreshold <- avgDensity * 2
  results$labelInside <- results$density > tallThreshold
  results$labelY <- ifelse(results$labelInside,
                           results$density - labelBuffer,
                           results$density + labelBuffer)

  legStep <- yMax * 0.05
  legBase <- yMax * 0.70
  refBase <- legBase - legStep * 2

  ref_width <- 100 * denom_unit / total_size

  ggplot() +
    geom_rect(data = results,
      aes(xmin = lagAccFieldSize, xmax = AccFieldSize,
          ymin = 0, ymax = density, fill = broadArea),
      alpha = alpha, color = NA) +
    geom_segment(data = results,
      aes(x = lagAccFieldSize, xend = AccFieldSize,
          y = density, yend = density, color = broadArea),
      linewidth = 0.8) +
    geom_segment(data = results,
      aes(x = lagAccFieldSize, xend = lagAccFieldSize,
          y = 0, yend = density, color = broadArea),
      linewidth = 0.4) +
    geom_segment(data = results,
      aes(x = AccFieldSize, xend = AccFieldSize, y = 0, yend = density),
      linetype = "dashed", color = "gray80", linewidth = 0.3) +
    geom_text(data = results[!results$labelInside, ],
      aes(x = midX, y = labelY, label = field),
      size = 2.3, angle = 90, hjust = 0, color = "black") +
    geom_text(data = results[results$labelInside, ],
      aes(x = midX, y = labelY, label = field),
      size = 2.3, angle = 90, hjust = 1, color = "black") +
    geom_segment(data = results,
      aes(x = lagAccFieldSize, xend = lagAccFieldSize,
          y = -yMax*0.01, yend = -yMax*0.02),
      color = "black", linewidth = 0.1) +
    geom_segment(data = results,
      aes(x = AccFieldSize, xend = AccFieldSize,
          y = -yMax*0.01, yend = -yMax*0.02),
      color = "black", linewidth = 0.1) +
    geom_segment(data = results,
      aes(x = lagAccFieldSize, xend = AccFieldSize,
          y = -yMax*0.02, yend = -yMax*0.02),
      color = "black", linewidth = 0.3) +
    geom_text(data = results,
      aes(x = midX, y = -yMax*0.04,
          label = paste0(round(fieldSize_pct, 0), "%")),
      size = 2.3, angle = 90) +
    geom_hline(yintercept = avgDensity, linetype = "dashed",
               color = "gray50", linewidth = 0.3) +
    geom_text(aes(x = 97, y = avgDensity, label = "Avg."),
      hjust = 1, vjust = -0.5, size = 2.5, color = "gray50") +
    geom_rect(aes(xmin = 60, xmax = 60 + ref_width,
                  ymin = refBase, ymax = refBase + legStep),
      fill = "black", alpha = 0.1, color = "grey") +
    geom_text(aes(x = 60 + ref_width + 1,
                  y = refBase + legStep / 2,
                  label = paste0("= 1 recognition per ", denom_label)),
      color = "black", size = 2.5, hjust = 0, vjust = 0.5) +
    theme_minimal() +
    ylab(paste0("Award density (yearly recognitions/", denom_label, ")")) +
    xlab(paste0("Field size (% of ", denom_label, ")")) +
    scale_x_continuous(breaks = c(0, 100), labels = c("0%", "100%")) +
    scale_y_continuous(labels = scales::label_number(decimal.mark = ".")) +
    theme(
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      axis.text.x = element_text(face = "bold", size = 10, margin = margin(t = -10)),
      axis.text.y = element_text(face = "bold", size = 9),
      axis.text = element_text(size = 11),
      axis.title = element_text(size = 11)
    ) +
    scale_fill_manual(values = color_map, aesthetics = c("fill", "color")) +
    # Manual legend
    annotate("rect", xmin = 80, xmax = 84,
      ymin = legBase + legStep * 3, ymax = legBase + legStep * 4,
      fill = "#1A237E", alpha = alpha) +
    annotate("rect", xmin = 80, xmax = 84,
      ymin = legBase + legStep * 2, ymax = legBase + legStep * 3,
      fill = "#8B4577", alpha = alpha) +
    annotate("rect", xmin = 80, xmax = 84,
      ymin = legBase + legStep * 1, ymax = legBase + legStep * 2,
      fill = "#00A5A5", alpha = alpha) +
    annotate("rect", xmin = 80, xmax = 84,
      ymin = legBase, ymax = legBase + legStep * 1,
      fill = "#E7298A", alpha = alpha) +
    annotate("segment", x = 84,
      y = legBase + legStep * 3, yend = legBase + legStep * 4,
      color = "#1A237E", linewidth = 1) +
    annotate("segment", x = 84,
      y = legBase + legStep * 2, yend = legBase + legStep * 3,
      color = "#8B4577", linewidth = 1) +
    annotate("segment", x = 84,
      y = legBase + legStep * 1, yend = legBase + legStep * 2,
      color = "#00A5A5", linewidth = 1) +
    annotate("segment", x = 84,
      y = legBase, yend = legBase + legStep * 1,
      color = "#E7298A", linewidth = 1) +
    annotate("text", x = 85, y = legBase + legStep * 3.5,
      label = "Math & Phys. sci", hjust = 0, size = 2.5) +
    annotate("text", x = 85, y = legBase + legStep * 2.5,
      label = "Applied sci", hjust = 0, size = 2.5) +
    annotate("text", x = 85, y = legBase + legStep * 1.5,
      label = "Life & Health", hjust = 0, size = 2.5) +
    annotate("text", x = 85, y = legBase + legStep * 0.5,
      label = "Social sci", hjust = 0, size = 2.5) +
    annotate("text", x = 85, y = legBase + legStep * 4.3,
      label = "Broad fields", hjust = 0, size = 2.8, fontface = "bold") +
    coord_cartesian(ylim = c(-yMax*0.06, yMax)) +
    theme(legend.position = "none")
}
