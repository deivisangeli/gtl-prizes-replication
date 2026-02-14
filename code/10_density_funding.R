# ==============================================================================
# 10_density_funding.R
# Generates: Figure S5 (prizesDensityFunding.pdf)
#
# Extracted from archive/densityByField.R — the funding density section.
# Uses the old field-assignment approach (plotFundingField from cleanPrizeList)
# with manual winner-share allocation for multidisciplinary prizes.
# ==============================================================================
source("_helpers.R")

# Load and prepare prize list
prizeList <- read_excel(file.path(data_dir, "cleanPrizeList.xlsx")) %>%
  filter(!is.na(`Award Name`), `Award Name` != "Max Planck Research Award") %>%
  filter(plotFinestField != "Humanities")

prizeList <- prizeList[order(prizeList$pcaRank), ]
prizeList$cumWinners <- cumsum(prizeList$`Yearly Winners`)
totalWinners <- sum(prizeList$`Yearly Winners`)
prizeList <- prizeList[order(prizeList$`Award Name`), ]

prizeList <- prizeList %>% select(`Award Name`, pcaRank, `Yearly Winners`,
                                  `plotField`, plotFinestField, plotFundingField)

prizeList$plotFundingMultifield <- 0
prizeList$plotFundingMultifield[prizeList$plotFundingField %in% c("Multidisciplinary", "Environment")] <- 1

################################################################################
# Load budget data
################################################################################

federalRnD <- readxl::read_excel(file.path(data_dir, "2022budgetByField.xlsx"))
federalRnD <- federalRnD %>% filter(plotFundingField != "NA") %>%
  filter(!(plotFundingField %in% c("Humanities", "Other non-science")))

federalRnD$plotFundingField <- paste0("rnd", federalRnD$plotFundingField)
federalRnD <- federalRnD %>% select(plotFundingField, researchBudget2022, fundingPlotOrder)
federalRnD <- federalRnD %>% group_by(plotFundingField) %>%
  summarise(researchBudget2022 = sum(researchBudget2022, na.rm = TRUE),
            fundingPlotOrder = first(fundingPlotOrder))
federalRnD <- federalRnD[order(federalRnD$fundingPlotOrder), ]
federalRnD$rndFieldSize <- federalRnD$researchBudget2022 / sum(federalRnD$researchBudget2022)

forCbind <- federalRnD %>% select(-researchBudget2022, -fundingPlotOrder)
fundingFields <- forCbind$plotFundingField
forCbind$plotFundingField <- paste0(federalRnD$plotFundingField, " size")
forCbind <- forCbind %>% spread(plotFundingField, rndFieldSize)
prizeList <- cbind(prizeList, forCbind)

# Initialize winnerShare columns
for (field in fundingFields) {
  prizeList[paste0(field, " winnerShare")] <- 0
  prizeList[, paste0(field, " winnerShare")][paste0("rnd", prizeList$plotFundingField) == field & prizeList$plotFundingMultifield == 0] <- 1
}

################################################################################
# Manual field assignments for multidisciplinary prizes
################################################################################

# Frontiers of Knowledge Award in Basic Sciences → Physical Sciences only
prizeList$`rndPhysical Sciences winnerShare`[prizeList$`Award Name` == "Frontiers of Knowledge Award in Basic Sciences"] <- 1

# Kavli Prize in Neuroscience → proportional across Phys.Sci, Life, CS&Eng
totalKavli <- sum(prizeList$`rndPhysical Sciences size`[1], prizeList$`rndLife Sciences & Medicine size`[1],
                  prizeList$`rndCS & Engineering size`[1])
prizeList$`rndPhysical Sciences winnerShare`[prizeList$`Award Name` == "Kavli Prize in Neuroscience"] <- prizeList$`rndPhysical Sciences size`[1] / totalKavli
prizeList$`rndLife Sciences & Medicine winnerShare`[prizeList$`Award Name` == "Kavli Prize in Neuroscience"] <- prizeList$`rndLife Sciences & Medicine size`[1] / totalKavli
prizeList$`rndCS & Engineering winnerShare`[prizeList$`Award Name` == "Kavli Prize in Neuroscience"] <- prizeList$`rndCS & Engineering size`[1] / totalKavli

# Kyoto Prize in Basic Sciences → 1/4 math, 1/4 life, 1/2 phys.sci
prizeList$`rndMath winnerShare`[prizeList$`Award Name` == "Kyoto Prize in Basic Sciences"] <- 1/4
prizeList$`rndLife Sciences & Medicine winnerShare`[prizeList$`Award Name` == "Kyoto Prize in Basic Sciences"] <- 1/4
prizeList$`rndPhysical Sciences winnerShare`[prizeList$`Award Name` == "Kyoto Prize in Basic Sciences"] <- 1/2

# Balzan Prizes, Templeton Prize, Max Planck-Humboldt → proportional all fields
for (field in fundingFields) {
  prizeList[, paste0(field, " winnerShare")][prizeList$`Award Name` == "Templeton Prize"] <- prizeList[, paste0(field, " size")][1]
  prizeList[, paste0(field, " winnerShare")][prizeList$`Award Name` == "Max Planck-Humboldt Research Award"] <- prizeList[, paste0(field, " size")][1]
  prizeList[, paste0(field, " winnerShare")][prizeList$`Award Name` == "Balzan Prizes"] <- prizeList[, paste0(field, " size")][1]
}

# Environmental prizes → proportional all fields
env_prizes <- c("Blue Planet Prize", "Bower Award and Prize for Achievement in Science",
                "Eni Award", "Frontiers of Knowledge Award in Ecology and Conservation Biology",
                "Heineken Prize for Environmental Sciences", "Stockholm Water Prize",
                "Tyler Prize for Environmental Achievement", "Volvo Environment Prize")
for (field in fundingFields) {
  for (prize_name in env_prizes) {
    prizeList[, paste0(field, " winnerShare")][prizeList$`Award Name` == prize_name] <- prizeList[, paste0(field, " size")][1]
  }
}

# Science-wide prizes → proportional across fields up to Life Sciences & Medicine
life_idx <- grep("Life Sciences & Medicine", fundingFields)
for (field in fundingFields[1:life_idx]) {
  share <- prizeList[, paste0(field, " size")][1] / sum(prizeList[1, paste0(fundingFields[1:life_idx], " size")])
  for (prize_name in c("Albert Einstein World Award of Science", "King Faisal International Prize in Science",
                        "Harvey Prize", "Copley Medal", "Japan Prize")) {
    prizeList[, paste0(field, " winnerShare")][prizeList$`Award Name` == prize_name] <- share
  }
}

# Compute yearly winners per funding field
for (field in fundingFields) {
  prizeList[paste0(field, " YearlyWinners")] <-
    prizeList[paste0(field, " winnerShare")] * prizeList$`Yearly Winners`
}

################################################################################
# Compute funding field stats
################################################################################

yearlyWinnersByFundingField <- data.frame(plotFundingField = fundingFields)

for (row in 1:nrow(yearlyWinnersByFundingField)) {
  yearlyWinnersByFundingField[row, "fieldSize"] <- federalRnD[federalRnD$plotFundingField == fundingFields[row], "rndFieldSize"]
  yearlyWinnersByFundingField[row, "yearlyWinners"] <-
    sum(prizeList[, paste0(fundingFields[row], " YearlyWinners")])
  yearlyWinnersByFundingField[row, "winnersPerBillionUSD"] <-
    yearlyWinnersByFundingField[row, "yearlyWinners"] / (sum(federalRnD[federalRnD$plotFundingField == fundingFields[row], "researchBudget2022"]) / 10^6)
}

fundingFieldStats <- yearlyWinnersByFundingField
fundingFieldStats$fieldSize <- 100 * fundingFieldStats$fieldSize
fundingFieldStats$plotPosition <- federalRnD[federalRnD$plotFundingField %in% fundingFieldStats$plotFundingField, "fundingPlotOrder", ][[1]]

fundingFieldStats <- fundingFieldStats[order(-fundingFieldStats$winnersPerBillionUSD,
                                              fundingFieldStats$plotPosition), ]

# Add accumulated field sizes
fundingFieldStats$AccFieldSize <- cumsum(fundingFieldStats$fieldSize)
fundingFieldStats$lagAccFieldSize <- dplyr::lag(fundingFieldStats$AccFieldSize)
fundingFieldStats$lagAccFieldSize[1] <- 0

# Remove rnd prefix for display
fundingFieldStats$plotFundingField <- gsub("rnd", "", fundingFieldStats$plotFundingField)
fundingFieldStats$plotFundingField[fundingFieldStats$plotFundingField == "Life Sciences & Medicine"] <- "Life & Health Sciences"

# Create ribbon data for step plot
create_ribbon_data_funding <- function(data, var) {
  result <- data.frame()
  for (i in 1:(nrow(data) - 1)) {
    result <- rbind(result, data.frame(
      x = c(data$lagAccFieldSize[i], data$lagAccFieldSize[i + 1]),
      y = c(data[i, var], data[i, var]),
      plotFundingField = data$plotFundingField[i]
    ))
  }
  last_row <- nrow(data)
  result <- rbind(result, data.frame(
    x = c(data$lagAccFieldSize[last_row], data$AccFieldSize[last_row]),
    y = data[last_row, var],
    plotFundingField = data$plotFundingField[last_row]
  ))
  return(result)
}

funding_ribbon_data <- create_ribbon_data_funding(fundingFieldStats, "winnersPerBillionUSD")

custom_colors_funding <- c(
  "#8B4577",  # Ag. & natural resources
  "#E7298A",  # Business and Econ
  "#8B4577",  # CS & Engineering
  "#00A5A5",  # Life Sciences & Medicine
  "#1A237E",  # Math
  "#E7298A",  # Other Social Sciences
  "#1A237E",  # Physical Sciences
  "#E0E0E2"   # Psychology
)

################################################################################
# Plot
################################################################################

maxHeight <- round(max(fundingFieldStats$winnersPerBillionUSD), 0)
yMax <- max(fundingFieldStats$winnersPerBillionUSD) * 1.08
avgDensity <- weighted.mean(fundingFieldStats$winnersPerBillionUSD, fundingFieldStats$fieldSize)
tallThreshold <- avgDensity * 2
labelBuffer <- yMax * 0.04

fundingFieldStats$labelInside <- fundingFieldStats$winnersPerBillionUSD > tallThreshold
fundingFieldStats$manualOffset <- ifelse(fundingFieldStats$labelInside, -labelBuffer, labelBuffer)
fundingFieldStats$labelHjust <- ifelse(fundingFieldStats$labelInside, 1, 0)

legStep <- yMax * 0.05
legBase <- yMax * 0.70

ggplot() +
  geom_ribbon(data = funding_ribbon_data,
              aes(x = x, ymin = 0, ymax = y, fill = plotFundingField),
              alpha = alpha) +
  geom_line(data = funding_ribbon_data,
            aes(x = x, y = y, color = plotFundingField), size = 1) +
  geom_segment(data = fundingFieldStats,
               aes(x = AccFieldSize, xend = AccFieldSize,
                   y = 0, yend = winnersPerBillionUSD),
               linetype = "dashed", color = "gray80", size = 0.3) +
  geom_text(data = fundingFieldStats[!fundingFieldStats$labelInside, ],
    aes(x = (lagAccFieldSize + AccFieldSize) / 2,
        y = winnersPerBillionUSD + manualOffset,
        label = plotFundingField),
    size = 3.5, angle = 90, hjust = 0, color = "black") +
  geom_text(data = fundingFieldStats[fundingFieldStats$labelInside, ],
    aes(x = (lagAccFieldSize + AccFieldSize) / 2,
        y = winnersPerBillionUSD + manualOffset,
        label = plotFundingField),
    size = 3.5, angle = 90, hjust = 1, color = "black") +
  geom_segment(data = fundingFieldStats,
               aes(x = lagAccFieldSize, xend = lagAccFieldSize,
                   y = -0.2, yend = -0.3), color = "black", size = 0.1) +
  geom_segment(data = fundingFieldStats,
               aes(x = AccFieldSize, xend = AccFieldSize,
                   y = -0.2, yend = -0.3), color = "black", size = 0.1) +
  geom_segment(data = fundingFieldStats,
               aes(x = lagAccFieldSize, xend = AccFieldSize,
                   y = -0.3, yend = -0.3), color = "black", size = 0.3) +
  geom_text(data = fundingFieldStats,
            aes(x = (lagAccFieldSize + AccFieldSize) / 2,
                y = -0.13 * maxHeight, angle = 90,
                label = paste0(round(fieldSize, 0), "%")),
            size = 3.3) +
  theme_minimal() +
  ylab("Award density (yearly recognitions/Billion USD R&D)") +
  xlab("Field size (% of R&D budget)") +
  scale_x_continuous(breaks = c(0, 100), labels = c("0%", "100%")) +
  scale_y_continuous(breaks = c(0, 2.5, 5, 7.5, 10, 12.5)) +
  coord_cartesian(ylim = c(-yMax * 0.14, yMax)) +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.text.x = element_text(face = "bold", size = 10, margin = margin(t = -10)),
        axis.text.y = element_text(face = "bold", size = 9, margin = margin(t = -10)),
        axis.text = element_text(size = 13),
        axis.title = element_text(size = 13)) +
  scale_fill_manual(values = custom_colors_funding, aesthetics = c("fill", "color")) +
  annotate("rect", xmin = 80, xmax = 85,
    ymin = legBase + legStep * 3, ymax = legBase + legStep * 4,
    fill = "#1A237E", alpha = alpha) +
  annotate("rect", xmin = 80, xmax = 85,
    ymin = legBase + legStep * 2, ymax = legBase + legStep * 3,
    fill = "#8B4577", alpha = alpha) +
  annotate("rect", xmin = 80, xmax = 85,
    ymin = legBase + legStep * 1, ymax = legBase + legStep * 2,
    fill = "#00A5A5", alpha = alpha) +
  annotate("rect", xmin = 80, xmax = 85,
    ymin = legBase, ymax = legBase + legStep * 1,
    fill = "#E7298A", alpha = alpha) +
  annotate("segment", x = 85,
    y = legBase + legStep * 3, yend = legBase + legStep * 4,
    color = "#1A237E", size = 1) +
  annotate("segment", x = 85,
    y = legBase + legStep * 2, yend = legBase + legStep * 3,
    color = "#8B4577", size = 1) +
  annotate("segment", x = 85,
    y = legBase + legStep * 1, yend = legBase + legStep * 2,
    color = "#00A5A5", size = 1) +
  annotate("segment", x = 85,
    y = legBase, yend = legBase + legStep * 1,
    color = "#E7298A", size = 1) +
  annotate("text", x = 86, y = legBase + legStep * 3.5,
           label = "Math & Phys. sci", hjust = 0, size = 3) +
  annotate("text", x = 86, y = legBase + legStep * 2.5,
           label = "Applied sci", hjust = 0, size = 3) +
  annotate("text", x = 86, y = legBase + legStep * 1.5,
           label = "Life & Health", hjust = 0, size = 3) +
  annotate("text", x = 86, y = legBase + legStep * 0.5,
           label = "Social sci", hjust = 0, size = 3) +
  annotate("text", x = 86, y = legBase + legStep * 4.3,
           label = "Broad fields", hjust = 0, size = 3.3, fontface = "bold") +
  geom_hline(yintercept = avgDensity, linetype = "dashed", color = "gray50", size = 0.3) +
  geom_text(aes(x = 97, y = avgDensity, label = "Avg."),
            hjust = 1, vjust = -0.5, size = 3, color = "gray50") +
  theme(legend.position = "none")

ggsave(file.path(figure_dir, "prizesDensityFunding.pdf"), width = 9, height = 4.7)
