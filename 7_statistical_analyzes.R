# 7 - Compare MaxEnt model performance between Europe and USA

#----------------------------------------------------------------#
# Compare MaxEnt model performance between Europe and USA
# across identical virtual species and background sampling strategies.
# Model results are compared between regions for matching combinations
# of species, background number, buffer size, spatial weighting and
# replicate.
#----------------------------------------------------------------#

library(dplyr)
library(tidyr)
library(ggplot2)

# 7.0 - Change here for Europe/USA extant and Kora/Lilly directory ####
#------------------------------------------------#

#Europe Kora
#setwd("C:/Users/koras/OneDrive/Dokumente/Unikram/Physische_Geographie/SDM/Project/data")

#USA Kora
#setwd("C:/Users/koras/OneDrive/Dokumente/Unikram/Physische_Geographie/SDM/Project/data/usa")

#Europe Lilly
#setwd("C:/Users/phi/Documents/Uni/Master/SoSe2026/SDM/Project/data")

#USA Lilly
#setwd("C:/Users/phi/Documents/Uni/Master/SoSe2026/SDM/Project/data/usa")

# 7.1 - Define input and output files ####
europe_file <- "maxent/evaluation/maxent_evaluation_results.csv"
usa_file <- "usa/maxent/evaluation/maxent_evaluation_results.csv"
output_dir <- "maxent/evaluation/region_comparison"
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

# 7.2 - Load and prepare data ####
europe <- read.csv(europe_file, stringsAsFactors = FALSE)
usa <- read.csv(usa_file, stringsAsFactors = FALSE)

europe$region <- "Europe"
usa$region <- "USA"

all_results <- bind_rows(europe, usa) %>%
  mutate(
    species = factor(species),
    background_number = factor(
      background_number,
      levels = c("5000", "1xocc", "5xocc")
    ),
    buffer = factor(
      buffer,
      levels = c("100km", "200km", "adaptive")
    ),
    weighting = factor(
      weighting,
      levels = c("0", "0.5", "0.75")
    ),
    replicate = factor(replicate),
    region = factor(
      region,
      levels = c("Europe", "USA")
    )
  )

metrics <- c(
  "AUC",
  "Boyce",
  "overlap_percent",
  "overprediction_percent"
)

key_variables <- c(
  "species",
  "background_number",
  "buffer",
  "weighting",
  "replicate"
)

# 7.3 - Overall mean results ####
overall_summary <- all_results %>%
  summarise(
    AUC_mean = mean(AUC, na.rm = TRUE),
    AUC_SD = sd(AUC, na.rm = TRUE),
    Boyce_mean = mean(Boyce, na.rm = TRUE),
    Boyce_SD = sd(Boyce, na.rm = TRUE),
    Overlap_mean = mean(overlap_percent, na.rm = TRUE),
    Overlap_SD = sd(overlap_percent, na.rm = TRUE),
    Overprediction_mean = mean(overprediction_percent, na.rm = TRUE),
    Overprediction_SD = sd(overprediction_percent, na.rm = TRUE)
  )

overall_summary

# 7.4 - Mean results by region ####
mean_europe <- europe %>%
  summarise(
    AUC = mean(AUC, na.rm = TRUE),
    Boyce = mean(Boyce, na.rm = TRUE),
    Overlap = mean(overlap_percent, na.rm = TRUE),
    Overprediction = mean(overprediction_percent, na.rm = TRUE)
  )

mean_usa <- usa %>%
  summarise(
    AUC = mean(AUC, na.rm = TRUE),
    Boyce = mean(Boyce, na.rm = TRUE),
    Overlap = mean(overlap_percent, na.rm = TRUE),
    Overprediction = mean(overprediction_percent, na.rm = TRUE)
  )

mean_europe
mean_usa

# 7.5 - Mean results by species and region ####
species_means <- bind_rows(
  europe %>% mutate(Region = "Europe"),
  usa %>% mutate(Region = "USA")
) %>%
  group_by(Region, species) %>%
  summarise(
    AUC = mean(AUC, na.rm = TRUE),
    Boyce = mean(Boyce, na.rm = TRUE),
    overlap_percent = mean(overlap_percent, na.rm = TRUE),
    overprediction_percent = mean(overprediction_percent, na.rm = TRUE),
    .groups = "drop"
  )

species_means

# 7.6 - Create paired dataset ####
paired_results <- all_results %>%
  select(
    all_of(key_variables),
    region,
    all_of(metrics)
  ) %>%
  pivot_wider(
    names_from = region,
    values_from = all_of(metrics),
    names_sep = "_"
  )

nrow(paired_results)

# 7.7 - Overall paired t-tests ####
significance_results <- lapply(
  metrics,
  function(metric) {
    test <- t.test(
      paired_results[[paste0(metric, "_USA")]],
      paired_results[[paste0(metric, "_Europe")]],
      paired = TRUE
    )
    
    data.frame(
      metric = metric,
      mean_difference_USA_minus_Europe =
        mean(
          paired_results[[paste0(metric, "_USA")]] -
            paired_results[[paste0(metric, "_Europe")]],
          na.rm = TRUE
        ),
      t = unname(test$statistic),
      df = unname(test$parameter),
      p_value = test$p.value
    )
  }
) %>%
  bind_rows() %>%
  mutate(
    p_adjusted = p.adjust(p_value, method = "BH"),
    significant = p_adjusted < 0.05
  )

significance_results

write.csv(
  paired_results,
  file.path(output_dir, "paired_Europe_USA_results.csv"),
  row.names = FALSE
)

write.csv(
  significance_results,
  file.path(output_dir, "regional_significance_tests.csv"),
  row.names = FALSE
)

# 7.8 - Calculate USA−Europe differences ####
difference_results <- paired_results %>%
  mutate(
    AUC_diff = AUC_USA - AUC_Europe,
    Boyce_diff = Boyce_USA - Boyce_Europe,
    overlap_diff = overlap_percent_USA - overlap_percent_Europe,
    overprediction_diff =
      overprediction_percent_USA - overprediction_percent_Europe
  )

# 7.9 - Visual comparison by background number ####
background_plot_data <- difference_results %>%
  select(
    species,
    background_number,
    buffer,
    weighting,
    replicate,
    AUC_Europe,
    AUC_USA,
    Boyce_Europe,
    Boyce_USA,
    overlap_percent_Europe,
    overlap_percent_USA,
    overprediction_percent_Europe,
    overprediction_percent_USA
  ) %>%
  pivot_longer(
    cols = c(
      AUC_Europe,
      AUC_USA,
      Boyce_Europe,
      Boyce_USA,
      overlap_percent_Europe,
      overlap_percent_USA,
      overprediction_percent_Europe,
      overprediction_percent_USA
    ),
    names_to = c("metric", "region"),
    names_pattern =
      "(AUC|Boyce|overlap_percent|overprediction_percent)_(Europe|USA)",
    values_to = "value"
  ) %>%
  mutate(
    metric = recode(
      metric,
      AUC = "AUC",
      Boyce = "Boyce",
      overlap_percent = "Overlap",
      overprediction_percent = "Overprediction"
    ),
    metric = factor(
      metric,
      levels = c("AUC", "Boyce", "Overlap", "Overprediction")
    ),
    region = factor(
      region,
      levels = c("USA", "Europe")
    )
  )

plot_background_number <- ggplot(
  background_plot_data,
  aes(
    x = background_number,
    y = value,
    fill = region
  )
) +
  geom_boxplot(
    position = position_dodge(width = 0.8),
    width = 0.7,
    outlier.alpha = 0.3
  ) +
  facet_wrap(
    ~ metric,
    scales = "free_y",
    ncol = 2
  ) +
  labs(
    x = "Background number",
    y = "Model performance",
    fill = "Region"
  ) +
  theme_classic() +
  theme(
    legend.position = "top"
  )

plot_background_number

ggsave(
  file.path(
    output_dir,
    "region_comparison_background_number.png"
  ),
  plot_background_number,
  width = 9,
  height = 7,
  dpi = 300
)

# 7.10 - Visual comparison by buffer ####
plot_buffer <- ggplot(
  background_plot_data,
  aes(
    x = buffer,
    y = value,
    fill = region
  )
) +
  geom_boxplot(
    position = position_dodge(width = 0.8),
    width = 0.7,
    outlier.alpha = 0.3
  ) +
  facet_wrap(
    ~ metric,
    scales = "free_y",
    ncol = 2
  ) +
  labs(
    x = "Buffer",
    y = "Model performance",
    fill = "Region"
  ) +
  theme_classic() +
  theme(
    legend.position = "top"
  )

plot_buffer

ggsave(
  file.path(
    output_dir,
    "region_comparison_buffer.png"
  ),
  plot_buffer,
  width = 9,
  height = 7,
  dpi = 300
)

# 7.11 - Visual comparison by spatial weighting ####
plot_weighting <- ggplot(
  background_plot_data,
  aes(
    x = weighting,
    y = value,
    fill = region
  )
) +
  geom_boxplot(
    position = position_dodge(width = 0.8),
    width = 0.7,
    outlier.alpha = 0.3
  ) +
  facet_wrap(
    ~ metric,
    scales = "free_y",
    ncol = 2
  ) +
  labs(
    x = "Spatial weighting",
    y = "Model performance",
    fill = "Region"
  ) +
  theme_classic() +
  theme(
    legend.position = "top"
  )

plot_weighting

ggsave(
  file.path(
    output_dir,
    "region_comparison_spatial_weighting.png"
  ),
  plot_weighting,
  width = 9,
  height = 7,
  dpi = 300
)

# 7.12 - Test whether regional differences depend on background factors ####
metrics_diff <- c(
  "AUC_diff",
  "Boyce_diff",
  "overlap_diff",
  "overprediction_diff"
)

background_factors <- c(
  "background_number",
  "buffer",
  "weighting"
)

background_tests <- expand.grid(
  metric = metrics_diff,
  factor = background_factors,
  stringsAsFactors = FALSE
)

background_tests <- lapply(
  seq_len(nrow(background_tests)),
  function(i) {
    metric <- background_tests$metric[i]
    factor_name <- background_tests$factor[i]
    
    model <- aov(
      as.formula(
        paste(metric, "~", factor_name)
      ),
      data = difference_results
    )
    
    result <- summary(model)[[1]]
    
    data.frame(
      metric = metric,
      factor = factor_name,
      F = result[1, "F value"],
      p_value = result[1, "Pr(>F)"]
    )
  }
) %>%
  bind_rows() %>%
  mutate(
    p_adjusted = p.adjust(
      p_value,
      method = "BH"
    ),
    significant = p_adjusted < 0.05
  )

background_tests

write.csv(
  background_tests,
  file.path(
    output_dir,
    "background_factor_tests.csv"
  ),
  row.names = FALSE
)

# 7.13 - Pairwise comparison for significant Boyce effect ####
boyce_test <- background_tests %>%
  filter(
    metric == "Boyce_diff",
    factor == "background_number"
  )

if (boyce_test$p_adjusted < 0.05) {
  
  boyce_background <- aov(
    Boyce_diff ~ background_number,
    data = difference_results
  )
  
  boyce_tukey <- TukeyHSD(
    boyce_background
  )
  
  boyce_tukey
  
  boyce_summary <- difference_results %>%
    group_by(background_number) %>%
    summarise(
      mean_diff = mean(Boyce_diff, na.rm = TRUE),
      sd_diff = sd(Boyce_diff, na.rm = TRUE),
      n = n(),
      .groups = "drop"
    )
  
  boyce_summary
  
  write.csv(
    boyce_summary,
    file.path(
      output_dir,
      "boyce_background_number_summary.csv"
    ),
    row.names = FALSE
  )
}

# 7.14 - Species-specific regional tests ####
species_tests <- lapply(
  metrics,
  function(metric) {
    lapply(
      levels(paired_results$species),
      function(sp) {
        data <- paired_results %>%
          filter(species == sp)
        
        test <- t.test(
          data[[paste0(metric, "_USA")]],
          data[[paste0(metric, "_Europe")]],
          paired = TRUE
        )
        
        data.frame(
          species = sp,
          metric = metric,
          mean_difference_USA_minus_Europe =
            mean(
              data[[paste0(metric, "_USA")]] -
                data[[paste0(metric, "_Europe")]],
              na.rm = TRUE
            ),
          t = unname(test$statistic),
          df = unname(test$parameter),
          p_value = test$p.value
        )
      }
    ) %>%
      bind_rows()
  }
) %>%
  bind_rows() %>%
  group_by(metric) %>%
  mutate(
    p_adjusted = p.adjust(
      p_value,
      method = "BH"
    ),
    significant = p_adjusted < 0.05
  ) %>%
  ungroup()

species_tests

write.csv(
  species_tests,
  file.path(
    output_dir,
    "species_regional_significance_tests.csv"
  ),
  row.names = FALSE
)
