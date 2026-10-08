# 7 - Compare MaxEnt model performance between Europe and USA

#----------------------------------------------------------------#

# Compare MaxEnt model performance between Europe and USA

# across identical virtual species and background sampling strategies.

# Model results are compared between regions for matching combinations

# of species, background number, buffer size, spatial weighting and

# replicate.

#----------------------------------------------------------------#

# 7.0 - Load packages and working directory

library(dplyr)
library(tidyr)
library(ggplot2)

setwd("D:/SDM/Project/data")

# 7.1 - Define input and output files

europe_file <- "maxent/evaluation/maxent_evaluation_results.csv"
usa_file <- "usa/maxent/evaluation/maxent_evaluation_results.csv"
output_dir <- "maxent/evaluation/region_comparison"
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

# 7.2 - Load and prepare data

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

# 7.6 - Create paired dataset

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

# 7.7 - Overall paired t-tests

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
      mean_usa = mean(
        paired_results[[paste0(metric, "_USA")]],
        na.rm = TRUE
      ),
      mean_europe = mean(
        paired_results[[paste0(metric, "_Europe")]],
        na.rm = TRUE
      ),
      mean_difference_USA_minus_Europe = mean(
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
    p_adjusted = p.adjust(
      p_value,
      method = "BH"
    ),
    significant = p_adjusted < 0.05
  )

significance_results

# 7.8 - Calculate USA−Europe differences

difference_results <- paired_results %>%
  mutate(
    AUC_diff = AUC_USA - AUC_Europe,
    Boyce_diff = Boyce_USA - Boyce_Europe,
    overlap_diff = overlap_percent_USA - overlap_percent_Europe,
    overprediction_diff =
      overprediction_percent_USA - overprediction_percent_Europe
  )

# 7.9 - Visual comparison by background number

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
      levels = c(
        "AUC",
        "Boyce",
        "Overlap",
        "Overprediction"
      )
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

# 7.10 - Visual comparison by buffer

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

# 7.11 - Visual comparison by spatial weighting

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

# 7.12 - Test overall effects of background sampling strategies

background_factors <- c(
  "background_number",
  "buffer",
  "weighting"
)

overall_combinations <- expand.grid(
  metric = metrics,
  factor = background_factors,
  stringsAsFactors = FALSE
)

overall_tests <- lapply(
  seq_len(nrow(overall_combinations)),
  function(i) {
    metric <- overall_combinations$metric[i]
    factor_name <- overall_combinations$factor[i]
    model_formula <- as.formula(
      paste(metric, "~ region +", factor_name)
    )
    model <- aov(
      model_formula,
      data = all_results
    )
    result <- summary(model)[[1]]
    factor_row <- grep(
      paste0("^", factor_name),
      rownames(result)
    )
    data.frame(
      metric = metric,
      factor = factor_name,
      F = result[factor_row, "F value"],
      p_value = result[factor_row, "Pr(>F)"]
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

overall_tests

write.csv(
  overall_tests,
  file.path(
    output_dir,
    "overall_background_effect_tests.csv"
  ),
  row.names = FALSE
)

# 7.13 - Tukey HSD tests for significant overall effects

# 7.13.1 - Tukey HSD test for overall background number effect

boyce_background_model <- aov(
  Boyce ~ background_number,
  data = all_results
)

tukey_boyce_background_overall <- TukeyHSD(
  boyce_background_model,
  "background_number"
)$background_number

tukey_boyce_background_overall

# 7.13.2 - Tukey HSD test for spatial weighting

AUC_weighting_model <- aov(
  AUC ~ weighting,
  data = all_results
)

tukey_AUC_weighting_overall <- TukeyHSD(
  AUC_weighting_model,
  "weighting"
)$weighting

tukey_AUC_weighting_overall

Boyce_weighting_model <- aov(
  Boyce ~ weighting,
  data = all_results
)

tukey_Boyce_weighting_overall <- TukeyHSD(
  Boyce_weighting_model,
  "weighting"
)$weighting

tukey_Boyce_weighting_overall

# 7.14 - Test whether regional differences depend on background factors

interaction_combinations <- expand.grid(
  metric = metrics,
  factor = background_factors,
  stringsAsFactors = FALSE
)

interaction_tests <- lapply(
  seq_len(nrow(interaction_combinations)),
  function(i) {
    metric <- interaction_combinations$metric[i]
    factor_name <- interaction_combinations$factor[i]
    model_formula <- as.formula(
      paste(metric, "~ region *", factor_name)
    )
    model <- aov(
      model_formula,
      data = all_results
    )
    result <- summary(model)[[1]]
    interaction_name <- paste0(
      "region:",
      factor_name
    )
    interaction_row <- which(
      rownames(result) == interaction_name
    )
    data.frame(
      metric = metric,
      factor = factor_name,
      F = result[interaction_row, "F value"],
      p_value = result[interaction_row, "Pr(>F)"]
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

interaction_tests

write.csv(
  interaction_tests,
  file.path(
    output_dir,
    "region_background_interaction_tests.csv"
  ),
  row.names = FALSE
)

# 7.14.1 - Regional Tukey HSD test for background number

tukey_results <- lapply(
  levels(all_results$region),
  function(reg) {
    region_data <- all_results %>%
      filter(region == reg)
    boyce_model <- aov(
      Boyce ~ background_number,
      data = region_data
    )
    tukey <- TukeyHSD(
      boyce_model,
      "background_number"
    )$background_number
    data.frame(
      region = reg,
      comparison = rownames(tukey),
      diff = tukey[, "diff"],
      lwr = tukey[, "lwr"],
      upr = tukey[, "upr"],
      p_adjusted = tukey[, "p adj"]
    )
  }
) %>%
  bind_rows()

tukey_results

# 7.15 - Species-specific regional tests

species_tests <- lapply(
  metrics,
  function(metric) {
    species_results <- lapply(
      levels(paired_results$species),
      function(sp) {
        species_data <- paired_results %>%
          filter(species == sp)
        test <- t.test(
          species_data[[paste0(metric, "_USA")]],
          species_data[[paste0(metric, "_Europe")]],
          paired = TRUE
        )
        data.frame(
          species = sp,
          metric = metric,
          mean_difference_USA_minus_Europe = mean(
            species_data[[paste0(metric, "_USA")]] -
              species_data[[paste0(metric, "_Europe")]],
            na.rm = TRUE
          ),
          t = unname(test$statistic),
          df = unname(test$parameter),
          p_value = test$p.value
        )
      }
    )
    bind_rows(species_results)
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

species_tests

# 7.16 - Plot species-specific regional differences

species_plot <- paired_results %>%
  select(
    species,
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
    cols = -species,
    names_to = c("metric", "Region"),
    names_pattern = "(.*)_(Europe|USA)$",
    values_to = "value"
  ) %>%
  mutate(
    metric = factor(
      metric,
      levels = c(
        "AUC",
        "Boyce",
        "overlap_percent",
        "overprediction_percent"
      ),
      labels = c(
        "AUC",
        "Boyce Index",
        "Spatial overlap (%)",
        "Overprediction (%)"
      )
    ),
    Region = factor(
      Region,
      levels = c("USA", "Europe")
    )
  )

species_regional_plot <- ggplot(
  species_plot,
  aes(
    x = species,
    y = value,
    fill = Region
  )
) +
  geom_boxplot(
    position = position_dodge(width = 0.75),
    width = 0.65,
    outlier.shape = NA
  ) +
  facet_wrap(
    ~ metric,
    scales = "free_y",
    ncol = 1
  ) +
  labs(
    x = NULL,
    y = "Model performance",
    fill = "Region"
  ) +
  theme_classic() +
  theme(
    axis.text.x = element_text(
      angle = 20,
      hjust = 1
    )
  )

ggsave(
  file.path(
    output_dir,
    "species_regional_differences.png"
  ),
  species_regional_plot,
  width = 7,
  height = 6,
  dpi = 300
)

