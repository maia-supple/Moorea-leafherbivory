# Load necessary libraries
library(tidyverse)

# --- Richness Analysis by Functional Feeding Group ---
average_richness_funcfeed <- damage_richness %>%
  group_by(group, Func_feed_group) %>%
  summarise(avg_richness = mean(richness, na.rm = TRUE))

# --- Two-way ANOVA: Richness ~ Group * Functional Feeding Group ---
anova_funcfeed <- aov(richness ~ group * Func_feed_group, data = damage_richness)
summary(anova_funcfeed)
TukeyHSD(anova_funcfeed)

# --- Richness by Individual Herbivory Types ---
herbivory_types <- c(
  "hole feeding", "margin feeding", "skeletonization",
  "surface feeding", "piercing and sucking", "oviposition",
  "galling", "mining", "fungal"
)

richness_results <- list()

for (type in herbivory_types) {
  filtered <- leafdatawscore %>%
    filter(herbivory_type == type) %>%
    group_by(site, damage_type, group) %>%
    summarise(count = n(), .groups = "drop") %>%
    group_by(site, group) %>%
    summarise(richness = n(), NRI_group = first(group), .groups = "drop")
  
  model <- aov(richness ~ NRI_group, data = filtered)
  print(paste("---", type, "---"))
  print(summary(model))
  print(TukeyHSD(model))
  
  richness_results[[type]] <- list(anova = model, tukey = TukeyHSD(model))
}

# --- Herbivory Frequency ANOVA ---
leafdata_funcfeed <- leafdatawscore %>%
  filter(!is.na(Func_feed_group), !is.na(group), !is.na(leaf_ID)) %>%
  mutate(Func_feed_group = gsub("ovipositon", "oviposition", Func_feed_group)) %>%
  filter(!grepl("incertae sedia", Func_feed_group)) %>%
  group_by(site, species, group, Func_feed_group, leaf_ID) %>%
  summarise(detected = 1, .groups = "drop") %>%
  group_by(site, species, group, Func_feed_group) %>%
  summarise(leaves_with_damage = n_distinct(leaf_ID), .groups = "drop") %>%
  mutate(proportion = leaves_with_damage / 50)

leafdata_funcfeed$group <- factor(leafdata_funcfeed$group, levels = c("N", "R", "I"))
leafdata_funcfeed$Func_feed_group <- as.factor(leafdata_funcfeed$Func_feed_group)

anova_model <- aov(proportion ~ group * Func_feed_group, data = leafdata_funcfeed)
summary(anova_model)
TukeyHSD(anova_model)

# Individual ANOVAs per herbivory type
unique_ffg <- unique(leafdata_funcfeed$Func_feed_group)
ffg_results <- list()

for (type in unique_ffg) {
  df_sub <- filter(leafdata_funcfeed, Func_feed_group == type)
  model <- aov(proportion ~ group, data = df_sub)
  print(paste("---", type, "---"))
  print(summary(model))
  tukey <- TukeyHSD(model)
  print(tukey)
  ffg_results[[type]] <- list(anova = model, tukey = tukey)
}
