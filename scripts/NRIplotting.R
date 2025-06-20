# NRIplotting.R

library(ggplot2)
library(dplyr)
library(tidyr)

# Load your analysis results or raw data (if necessary)
# source("analysis.R") or load saved objects

# 1. Herbivory Index Boxplot ---------------------------------------------------

species_order <- c("H. tiliaceus", "M. collina", "B. asiatica", "N. forsteri", 
                   "S. malaccense", "H. rosa-sinensis", "M. citrofolia", "D. erecta", 
                   "M. calvescens", "S. cumini", "L. camara", "S. campanulata")

herbindex_avg$species <- factor(herbindex_avg$species, levels = species_order)
herbindex_avg <- herbindex_avg %>%
  mutate(species = replace_na(species, "S. cumini"))

herbindex_boxplot <- ggplot(herbindex_avg, aes(x = species, y = avg_total_damage, fill = group)) +
  geom_boxplot() +
  geom_jitter(aes(color = group), width = 0.1, size = 0.5) +
  theme_minimal() +
  labs(
    title = "Average Total Herbivory Damage Per Site",
    x = "Species",
    y = "Total Damage (%)",
    fill = "Group"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("herbindex_boxplot.pdf", herbindex_boxplot, width = 10, height = 6, dpi = 300)

# 2. Herbivory Frequency Boxplot ------------------------------------------------

herb_data_plot$species <- factor(herb_data_plot$species, levels = species_order)

herb_percent_plot_bar <- ggplot(herb_data_plot, aes(x = species, y = proportion_non_zero, fill = group)) +
  geom_boxplot(position = position_dodge(width = 0.9), width = 0.7) +
  labs(x = "Species", y = "Proportion of Non-Zero Herbivory", fill = "Group") +
  theme_minimal() +
  theme(axis.text = element_text(size = 9), axis.title = element_text(size = 12)) +
  coord_flip() +
  geom_jitter(size = 0.7)

ggsave("herb_percent_plot_bar.pdf", herb_percent_plot_bar, width = 5.76, height = 5, units = "in")

# 3. Shannon Diversity Plot ----------------------------------------------------

shannon_by_site$species <- factor(shannon_by_site$species, levels = species_order)

shannon_plots <- ggplot(shannon_by_site, aes(x = species, y = ShannonIndex, fill = group)) +
  geom_boxplot(position = position_dodge(width = 0.9), width = 0.7) +
  labs(x = "Species", y = "Shannon Diversity Index", fill = "Group") +
  theme_minimal() +
  theme(axis.text = element_text(size = 9), axis.title = element_text(size = 12)) +
  coord_flip() +
  geom_jitter(size = 0.5)

ggsave("shannon_plot.pdf", shannon_plots, width = 5.76, height = 5, units = "in")

# 4. Damage Type Richness Plot -------------------------------------------------

damage_richness <- damage_richness %>%
  mutate(
    species = factor(species, levels = species_order),
    group = factor(group, levels = c("N", "R", "I"))
  )

plot1 <- ggplot(damage_richness, aes(x = species, y = richness, fill = group)) +
  geom_boxplot(alpha = 0.6) +
  geom_jitter(aes(color = group), size = 2, alpha = 0.7,
              position = position_jitter(width = 0.1, height = 0)) +
  labs(title = "Damage Type Richness by Plant Group",
       x = "Plant Species", y = "Number of Unique Damage Types") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set2")

ggsave("damage_type_richness_nojitter.pdf", plot1, width = 6, height = 4)

# 5. Rarefaction Curve Plot ----------------------------------------------------

# Assuming obs_all dataframe is created in analysis script for rarefaction

rarefaction <- obs_all %>%
  ggplot(aes(x = N, y = S, group = type)) +
  geom_ribbon(aes(ymin = lower2.5, ymax = higher97.5, fill = type), alpha = 0.5) +
  geom_line(aes(color = type)) +
  labs(x = "No. of leaves", y = "Observed damage richness", fill = "Group") +
  theme(
    panel.background = element_blank(),
    panel.grid.major = element_line(color = "gray", size = 0.5),
    panel.grid.minor = element_blank(),
    axis.text = element_text(size = 14),
    axis.title = element_text(size = 14),
    title = element_text(size = 14)
  )

ggsave("rarefaction.png", rarefaction, width = 5.76, height = 6, units = "in")

# 6. Specificity Score Richness Plot --------------------------------------------

specificity_plot <- ggplot(richness_data_specificity, aes(x = group, y = richness, fill = group)) +
  geom_boxplot() +
  geom_jitter(aes(color = group), size = 2, alpha = 0.7, position = position_jitter(width = 0.1)) +
  facet_wrap(~ new_score, scales = "free_y") +
  labs(x = "NRI Group", y = "Richness of Damage Types") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("specificity_richness.pdf", specificity_plot, width = 6, height = 4)

# 7. Specificity Score Frequency Plot -------------------------------------------

specificty_plot <- ggplot(specificity_proportions_df, aes(x = new_score, y = Proportion, fill = group)) +
  geom_boxplot() +
  geom_jitter(size = 1) +
  labs(x = "Specificity Score", y = "Proportion of Leaves Affected", fill = "Group") +
  scale_fill_manual(values = c("N" = "#7392de", "R" = "#869948", "I" = "#e0632d", "NA" = "grey")) +
  theme_minimal() +
  theme(axis.text = element_text(size = 9), axis.title = element_text(size = 10)) +
  coord_flip()

ggsave("specificity_plot_freq.pdf", specificty_plot, width = 6, height = 4)
