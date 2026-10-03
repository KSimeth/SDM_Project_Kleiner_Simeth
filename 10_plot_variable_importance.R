# 10 - Random Forest: variable importance - Europe vs USA
# ================================================================#

# 10.0 -  directory ####
#------------------------------------------------#
# Kora
#setwd("C:/Users/koras/OneDrive/Dokumente/Unikram/Physische_Geographie/SDM/Project/data")

# Lilly
#setwd("C:/Users/phi/Documents/Uni/Master/SoSe2026/SDM/Project/data")


# 10.1 Packages
packages <- c("dplyr", "ggplot2")

for (pkg in packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) install.packages(pkg)
  library(pkg, character.only = TRUE)
}



# 10.2 Load Random Forest importance results
europe <- read.csv(
  "maxent/evaluation/random_forest_variable_importance.csv"
) %>%
  mutate(
    Region = "Europe"
  )

usa <- read.csv(
  "usa/maxent/evaluation/random_forest_variable_importance.csv"
) %>%
  mutate(
    Region = "USA"
  )

# 10.3 Combine regions
importance_results <- bind_rows(
  europe,
  usa
)

# 10.4 Readable labels
importance_results <- importance_results %>%
  mutate(
    metric = recode(
      metric,
      AUC = "AUC",
      Boyce = "Boyce Index",
      overlap_percent = "Percent overlap",
      overprediction_percent = "Overprediction",
      underprediction_percent = "Underprediction"
    ),
    variable = recode(
      variable,
      background_number = "Background points",
      buffer = "Buffer size",
      weighting = "Spatial weighting",
      range_size = "Range size"
    ),
    Region = factor(
      Region,
      levels = c("USA", "Europe")
    )
  )

# 10.5 Check results
cat("\n====================================================\n")
cat("COMBINED RANDOM FOREST VARIABLE IMPORTANCE\n")
cat("====================================================\n")

print(importance_results)

# 10.6 Plot - Europe vs USA
p <- ggplot(
  importance_results,
  aes(
    x = variable,
    y = IncMSE,
    fill = Region
  )
) +
  geom_col(
    position = position_dodge(width = 0.8),
    width = 0.7
  ) +
  geom_text(
    aes(
      label = paste0(round(IncMSE, 1), "%")
    ),
    position = position_dodge(width = 0.8),
    vjust = -0.3,
    size = 3
  ) +
  facet_wrap(
    ~ metric,
    scales = "fixed"
  ) +
  scale_y_continuous(
    limits = c(-20, 100),
    breaks = seq(-20, 100, 20),
    labels = function(x) paste0(x, "%")
  ) +
  labs(
    x = NULL,
    y = "% increase in mean squared error",
    fill = "Region"
  ) +
  theme_classic() +
  theme(
    axis.text.x = element_text(
      angle = 45,
      hjust = 1
    ),
    legend.position = "top"
  )

print(p)

# 10.7 Save plot
plot_dir <- "maxent/evaluation/region_comparison/plots"

dir.create(
  plot_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

ggsave(
  file.path(
    plot_dir,
    "Random_forest_variable_importance_Europe_vs_USA.png"
  ),
  p,
  width = 8,
  height = 6,
  dpi = 300
)

# 10.8 Save combined results
write.csv(
  importance_results,
  "maxent/evaluation/region_comparison/random_forest_variable_importance_Europe_vs_USA.csv",
  row.names = FALSE
)

# 10.9 Final output
cat("\n====================================================\n")
cat("COMBINED RANDOM FOREST ANALYSIS COMPLETED\n")
cat("====================================================\n")
cat("Regions: Europe, USA\n")
cat("Metrics:", length(unique(importance_results$metric)), "\n")
cat("\nFiles saved:\n")
cat("- random_forest_variable_importance_Europe_vs_USA.csv\n")
cat("- Random_forest_variable_importance_Europe_vs_USA.png\n")
cat("\n====================================================\n")