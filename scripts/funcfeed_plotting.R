# Load required libraries
library(tidyverse)
library(RColorBrewer)
library(ggplot2)

# --- Boxplot of Richness by Functional Feeding Group ---
Richness_funcfeed <- ggplot(damage_richness, aes(x = group, y = richness, fill = group)) +
  geom_boxplot() +
  geom_jitter(aes(color = group), size = 2, alpha = 0.7, position = position_jitter(width = 0.1, height = 0)) +
  facet_wrap(~ Func_feed_group, scales = "free_y") +
  labs(
    title = "Comparison of Richness Across Functional Feeding Groups",
    x = "NRI Group",
    y = "Richness"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    strip.text = element_text(size = 12, face = "bold")
  )
ggsave("Richness_funcfeed.pdf", Richness_funcfeed, width = 10, height = 8)

# --- Herbivory Proportion Boxplot ---
herbivory_plot <- ggplot(leafdata_funcfeed, aes(x = group, y = proportion, fill = group)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(width = 0.2, alpha = 0.7, size = 0.5, aes(color = group)) +
  facet_wrap(~Func_feed_group, scales = "free_y") +
  scale_fill_manual(values = c("N" = "blue", "R" = "yellow", "I" = "red")) +
  scale_color_manual(values = c("N" = "blue", "R" = "orange", "I" = "red")) +
  labs(
    title = "Proportion of Leaves Affected by Each Herbivory Type (out of 50)",
    x = "Biogeographic Group (NRI)",
    y = "Proportion of Leaves Affected"
  ) +
  theme_minimal(base_size = 13) +
  theme(strip.text = element_text(face = "bold"), legend.position = "none")

ggsave("herbivory_proportion_boxplot.pdf", herbivory_plot, width = 12, height = 8)

# --- Ecto/Endo/Stylo Bar Plot ---
categories <- c(
  "margin feeding" = "Ectophytic", "hole feeding" = "Ectophytic", "surface feeding" = "Ectophytic",
  "skeletonization" = "Ectophytic", "oviposition" = "Stylophytic", "ovipositon" = "Stylophytic",
  "piercing and sucking" = "Stylophytic", "mining" = "Endophytic", "galling" = "Endophytic",
  "fungal" = "Fungal", "incertae sedia" = "Stylophytic"
)

leafdata_EESF <- leafdatawscore %>%
  mutate(category = recode(Func_feed_group, !!!categories)) %>%
  filter(!is.na(category))

damage_species <- leafdata_EESF %>%
  distinct(damage_type, species, category) %>%
  group_by(damage_type, category) %>%
  summarise(n_species = n(), .groups = "drop")

damage_type_counts <- damage_species %>%
  group_by(n_species, category) %>%
  summarise(count = n(), .groups = "drop")

damage_type_counts$category <- factor(damage_type_counts$category, 
                                      levels = c("Endophytic", "Stylophytic", "Ectophytic", "Fungal"))

bar_plot <- ggplot(damage_type_counts, aes(x = factor(n_species), y = count, fill = category)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(
    x = "Number of Host Species per Damage Type",
    y = "Number of Damage Types",
    title = "How Many Damage Types Are Specialists vs. Generalists?",
    fill = "Feeding Category"
  ) +
  scale_fill_brewer(palette = "Set1") +
  theme_minimal()

ggsave("damage_type_generalists.pdf", bar_plot, width = 10, height = 6)

# --- Violin Plot of Score by Herbivory Type ---
leafdatawscore$new_score <- as.numeric(leafdatawscore$new_score)

violin_plot <- ggplot(leafdatawscore, aes(x = Func_feed_group, y = new_score, fill = Func_feed_group)) +
  geom_violin(trim = FALSE, alpha = 0.6) +
  geom_boxplot(width = 0.1, color = "black", alpha = 0.3, outlier.shape = NA) +
  geom_point(alpha = 0.5, color = "black", size = 1) +
  labs(
    title = "Violin Plot of Herbivory Type and Distribution of Scores",
    x = "Herbivory Type",
    y = "Score"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none")

ggsave("violin_plot.pdf", plot = violin_plot, width = 10, height = 6, dpi = 300)
