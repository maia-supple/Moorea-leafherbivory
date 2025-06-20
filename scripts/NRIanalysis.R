# NRIanalysis.R

# Load libraries
library(dplyr)
library(tidyr)
library(vegan)

# 1. Herbivory Index Data Preparation & ANOVA ---------------------------------

# Remove fungal herbivory data
herbonlydata <- subset(leafdatawscore, herbivory_type != "fungal")

# Sum herbivory index by leaf
herbindexdata <- herbonlydata %>%
  group_by(leaf_ID, species, site, group) %>%
  summarise(total_damage = sum(herbivory_index, na.rm = TRUE), .groups = "drop")

# Average by site, species, group
herbindex_avg <- herbindexdata %>%
  group_by(site, species, group) %>%
  summarise(avg_total_damage = mean(total_damage, na.rm = TRUE), .groups = "drop")

# ANOVA on avg total damage by group
anova_herbindex <- aov(avg_total_damage ~ group, data = herbindex_avg)
summary(anova_herbindex)
tukey_herbindex <- TukeyHSD(anova_herbindex)

# 2. Herbivory Frequency Analysis ---------------------------------------------

# Summarize total herbivory index by leaf
herb_data <- leafdatawscore %>%
  group_by(leaf_ID, site, species, group) %>%
  summarise(total_HI = sum(herbivory_index, na.rm = TRUE), .groups = "drop")

# Proportion non-zero herbivory by group, species, site
herb_data_plot <- herb_data %>%
  filter(!is.na(total_HI)) %>%
  group_by(group, species, site) %>%
  summarise(proportion_non_zero = sum(total_HI > 0.01) / n(), .groups = "drop")

# ANOVA on proportion non-zero
anova_herb_freq <- aov(proportion_non_zero ~ group, data = herb_data_plot)
summary(anova_herb_freq)
tukey_herb_freq <- TukeyHSD(anova_herb_freq)

# 3. Shannon Diversity --------------------------------------------------------

# Calculate damage type counts per site
leafdatashannon <- leafdatawscore %>% mutate(damage_type = as.factor(damage_type))

# Construct damage type matrix per site
damage_matrix <- leafdatashannon %>%
  group_by(site, damage_type) %>%
  summarise(count = n(), .groups = "drop") %>%
  pivot_wider(names_from = damage_type, values_from = count, values_fill = 0)

# Compute Shannon diversity per site
shannon_results <- damage_matrix %>%
  rowwise() %>%
  mutate(ShannonIndex = diversity(c_across(-site), index = "shannon")) %>%
  ungroup() %>%
  select(site, ShannonIndex)

# Merge group and species info (assuming unique site-group-species mapping)
site_info <- leafdatawscore %>%
  select(site, group, species) %>%
  distinct()

shannon_by_site <- left_join(shannon_results, site_info, by = "site")

# ANOVA on Shannon by group
anova_shannon <- aov(ShannonIndex ~ group, data = shannon_by_site)
summary(anova_shannon)
tukey_shannon <- TukeyHSD(anova_shannon)

# 4. Damage Type Richness -----------------------------------------------------

damage_richness <- leafdatawscore %>%
  filter(!is.na(Func_feed_group) & Func_feed_group != "NA") %>%
  group_by(species, group, site, Func_feed_group) %>%
  summarise(richness = n_distinct(damage_type), .groups = "drop")

average_richness <- damage_richness %>%
  group_by(group, species, site) %>%
  summarise(avg_richness = mean(richness, na.rm = TRUE), .groups = "drop")

anova_richness <- aov(avg_richness ~ group, data = average_richness)
summary(anova_richness)
tukey_richness <- TukeyHSD(anova_richness)

# 5. Specificity Score Richness -------------------------------------------------

richness_data_specificity <- leafdatawscore %>%
  filter(!is.na(new_score) & new_score != "N/A") %>%
  group_by(new_score, group, species, site) %>%
  summarise(richness = n_distinct(damage_type), .groups = "drop") %>%
  mutate(group = factor(group, levels = c("N", "R", "I")))

# ANOVA full model with interaction
anova_specificity_full <- aov(richness ~ new_score * group, data = richness_data_specificity)
summary(anova_specificity_full)
tukey_specificity_full <- TukeyHSD(anova_specificity_full)

# Run ANOVA within specificity scores 1, 2, and 3
for(score in c("1", "2", "3")){
  cat("ANOVA for specificity score:", score, "\n")
  subset_data <- subset(richness_data_specificity, new_score == score)
  anova_res <- aov(richness ~ group, data = subset_data)
  print(summary(anova_res))
  print(TukeyHSD(anova_res))
}

# 6. Specificity Score Frequency Proportions -----------------------------------

specificity_proportions_df <- leafdatawscore %>%
  group_by(site, group, species, new_score) %>%
  summarise(Count = n(), .groups = "drop") %>%
  group_by(site) %>%
  mutate(Proportion = Count / sum(Count)) %>%
  ungroup()

anova_spec_prop <- aov(Proportion ~ group * new_score, data = specificity_proportions_df)
summary(anova_spec_prop)
tukey_spec_prop <- TukeyHSD(anova_spec_prop)

# ANOVAs per specificity score within proportions
for(score in c("1", "2", "3")){
  cat("ANOVA on proportions for specificity score:", score, "\n")
  p_data <- subset(specificity_proportions_df, new_score == score)
  anova_res <- aov(Proportion ~ group, data = p_data)
  print(summary(anova_res))
  print(TukeyHSD(anova_res))
}

