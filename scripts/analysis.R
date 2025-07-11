##data set up
#load data set
library(readr)
library(tidyverse)
DATA_COLLECTION_Moorea_project_Sheet1 <- read_csv("data/raw/DATA COLLECTION_Moorea project - Sheet1.csv")
View(DATA_COLLECTION_Moorea_project_Sheet1)

DATA_COLLECTION_Moorea_project_Sheet3 <- read_csv("data/raw/DATA COLLECTION_Moorea project - Sheet3.csv")
View(DATA_COLLECTION_Moorea_project_Sheet3)

#name data sets
leafdata <- DATA_COLLECTION_Moorea_project_Sheet1
scoredata <- DATA_COLLECTION_Moorea_project_Sheet3

#combine data sets
leafdatawscore <- leafdata %>%
  left_join(scoredata %>% select(damage_type, new_score), by = "damage_type")

#make NA's undamaged
leafdatawscore <- leafdatawscore %>%
  mutate(
    herbivory_type = ifelse(is.na(Func_feed_group), "Undamaged", Func_feed_group),
    damage_type = ifelse(is.na(damage_type), "Undamaged", damage_type),
    score = ifelse(is.na(new_score), "Undamaged", new_score)
  )

##herbivory index code
# Load necessary libraries
library(dplyr)
library(ggplot2)
library(tidyr)

# Remove fungal herbivory data
# Subset the dataset to exclude rows where herbivory_type is "fungal"
herbonlydata <- subset(leafdatawscore, herbivory_type != "fungal")

# Tabulate total herbivory
# Group by leaf, species, site, and group, then calculate the sum of percent damage for each group
herbindexdata <- herbonlydata %>%
  group_by(leaf_ID, species, site, group) %>%
  summarise(total_damage = sum(herbivory_index, na.rm = TRUE))

# Calculate average total damage by site, species, and group
herbindex_avg <- herbindexdata %>%
  group_by(site, species, group) %>%
  summarise(avg_total_damage = mean(total_damage, na.rm = TRUE))

# Perform ANOVA to check if there is a significant difference in damage across groups
anova_result <- aov(avg_total_damage ~ group, data = herbindex_avg)

# Apply Tukey's Honest Significant Difference (HSD) test to compare group means
TukeyHSD(anova_result)

# Display ANOVA summary to see p-values and other details
summary(anova_result)

# Create boxplot to visualize total damage across species, color-coded by group
# Specify the species order for consistent appearance in the plot
species_order <- c("H. tiliaceus", "M. collina", "B. asiatica", "N. forsteri", 
                   "S. malaccense", "H. rosa-sinensis", "M. citrofolia", "D. erecta", 
                   "M. calvescens", "S. cumini", "L. camara", "S. campanulata")

# Ensure species column is treated as a factor with the specified order
herbindex_avg$species <- factor(herbindex_avg$species, levels = species_order)

# Replace NA values in species column with "S. cumini" (default species)
herbindex_avg <- herbindex_avg %>%
  mutate(species = replace_na(species, "S. cumini"))

# Plot boxplot for average total damage across species
herbindex_boxplot <- ggplot(herbindex_avg, aes(x = species, y = avg_total_damage, fill = group)) +
  geom_boxplot() +
  geom_jitter(aes(color = group), width = 0.1, size = 0.5) +  # Add jitter for better visibility of individual points
  theme_minimal() + 
  labs(
    title = "Average Total Damage Per Site",
    x = "Species",
    y = "Total Damage (%)",
    fill = "Group"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for readability

# Save the plot as a PDF file
ggsave("herbindex_boxplot.pdf", plot = herbindex_boxplot, width = 10, height = 6, dpi = 300)

##Herbivory Frequency code
# Load necessary libraries
library(tidyverse)
library(dplyr)
library(ggplot2)

# Summarize herbivory data: total herbivory index (HI) by leaf, site, species, and group
herb_data <- leafdatawscore %>% 
  group_by(leaf_ID, site, species, group) %>% 
  summarise(total_HI = sum(herbivory_index), .groups = "drop")

# Filter and summarize proportion of non-zero herbivory per group, species, and site
herb_data_plot <- herb_data %>%
  filter(!is.na(total_HI)) %>%
  group_by(group, species, site) %>%
  summarise(
    proportion_non_zero = sum(total_HI > 0.01) / n(),  # Proportion of non-zero herbivory
    .groups = "drop"
  )

# Order the species factor levels for consistent plotting
herb_data_plot$species <- factor(herb_data_plot$species, 
                                 levels = c("B. asiatica", "H. tiliaceus", "N. forsteri", "M. collina", 
                                            "D. erecta", "S. malaccense", "H. rosa-sinensis", "M. citrofolia", 
                                            "L. camara", "S. campanulata", "M. calvescens", "S. cumini"))

# Create the boxplot for herbivory proportion by species, color-coded by group
herb_percent_plot_bar <- ggplot(herb_data_plot, aes(x = species, y = proportion_non_zero, fill = group)) +
  geom_boxplot(position = position_dodge(width = 0.9), width = 0.7) +  # Boxplot with dodged bars
  labs(x = "Species", y = "Proportion of Non-Zero Herbivory", fill = "Group") +
  theme_minimal() +  # Use minimal theme for the plot
  theme(axis.text = element_text(size = 9), axis.title = element_text(size = 12)) +  # Adjust text size
  coord_flip() +  # Flip coordinates for horizontal boxes
  geom_jitter(size = 0.7)  # Add jitter to visualize individual points

# Display the plot
print(herb_percent_plot_bar)

#standard deviation
herb_data_summary <- herb_data_plot %>%
  group_by(species, group) %>%
  summarise(
    mean_prop = mean(proportion_non_zero, na.rm = TRUE),
    sd_prop = sd(proportion_non_zero, na.rm = TRUE),
    .groups = "drop"
  )


# Save the plot as a Pdf file
ggsave("herb_percent_plot_bar.df", plot = herb_percent_plot_bar, width = 5.76, height = 5, units = "in")

#standard deviation 
herbindex_summary <- herbindex_avg %>%
  group_by(species, group) %>%
  summarise(
    mean_damage = mean(avg_total_damage, na.rm = TRUE),
    sd_damage = sd(avg_total_damage, na.rm = TRUE),
    .groups = "drop"
  )
# ANOVA
# Filter and prepare data for ANOVA
anova_herb_data <- herb_data %>%
  filter(!is.na(total_HI)) %>%  # Remove rows with NA values in total_HI
  group_by(site, species, group) %>%
  summarise(
    proportion_non_zero = sum(total_HI > 0.01) / n(),  # Proportion of non-zero herbivory
    .groups = "drop"
  )

# Perform ANOVA to check if group affects herbivory proportions
herb_anova <- aov(proportion_non_zero ~ group, data = anova_herb_data)

# Display the ANOVA results
summary(herb_anova)

# Perform Tukey's HSD post-hoc test for pairwise comparisons
tukey_herb <- TukeyHSD(herb_anova)

# Print the Tukey post-hoc test results
print(tukey_herb)

##Shannon Diversity Code 
# Load necessary libraries
library(vegan)
library(dplyr)
library(ggplot)

# Convert "damage_type" to a factor to count occurrences
leafdatashannon <- leafdatawscore %>%
  mutate(damage_type = as.factor(damage_type))

# Calculate Shannon Diversity for each site
shannon_results <- leafdatashannon %>%
  group_by(site) %>%
  summarize(ShannonIndex = diversity(table(damage_type), index = "shannon"))

# Merge Shannon Diversity results with the original data by site
diversity_data <- leafdatashannon %>% 
  group_by(site)

# Join the Shannon diversity index with the diversity_data
diversity_dat <- left_join(diversity_data, shannon_results, by = "site")

# Calculate mean Shannon diversity for each site
shannon_by_site <- diversity_dat %>%
  group_by(site) %>%
  summarize(MeanShannon = mean(ShannonIndex, na.rm = TRUE)) %>%
  left_join(leafdata %>% select(site, group,species) %>% distinct(), by = "site")

# Run ANOVA comparing average Shannon across N, R, and I
shannon_site_anova <- aov(MeanShannon ~ group, data = shannon_by_site)
summary(shannon_site_anova)

# Tukey HSD post-hoc test
tukey_site <- TukeyHSD(shannon_site_anova)
print(tukey_site)

# Ensure 'species' is a factor with a defined order for consistent plotting
shannon_by_site$species <- factor(shannon_by_site$species, 
                                    levels = c("B. asiatica", "H. tiliaceus", "N. forsteri", "M. collina", 
                                               "D. erecta", "S. malaccense", "H. rosa-sinensis", "M. citrofolia", 
                                               "L. camara", "S. campanulata", "M. calvescens", "S. cumini"))

# Create the boxplot for Shannon Diversity Index by species and group
shannon_plots <- ggplot(shannon_by_site, aes(x = species, y = MeanShannon, fill = group)) +
  geom_boxplot(position = position_dodge(width = 0.9), width = 0.7) +  # Boxplot with dodged bars
  labs(x = "Species", y = "Shannon Diversity Index", fill = "Group") +  # Axis labels
  theme_minimal() +  # Minimal theme for the plot
  theme(axis.text = element_text(size = 9), axis.title = element_text(size = 12)) +  # Customize text size
  coord_flip() +  # Flip coordinates for horizontal boxes
  geom_jitter(size = 0.5)  # Add jitter to visualize individual points

#standard deviation 
shannon_summary <- shannon_by_site %>%
  group_by(species, group) %>%
  summarise(
    mean_shannon = mean(MeanShannon, na.rm = TRUE),
    sd_shannon = sd(MeanShannon, na.rm = TRUE),
    .groups = "drop"
  )

# Save the plot as a PDF file
ggsave("shannon_plot.pdf", plot = shannon_plots, width = 5.76, height = 5, units = "in")

##Damage Type Richness NRI 
library(ggplot2)
library(dplyr)

# Filter out undamaged leaves (damage_type == "NA")
damage_richness <- leafdatawscore %>%
  filter(Func_feed_group != "NA") %>%
  group_by(species, group, site, Func_feed_group) %>%
  summarise(richness = n_distinct(damage_type), .groups = "drop")

# Average richness by NRI group
average_richness <- damage_richness %>%
  group_by(group, species, site) %>%
  summarise(avg_richness = mean(richness, na.rm = TRUE))

# Run ANOVA
anova_result <- aov(avg_richness ~ group, data = average_richness)

# View summary of ANOVA
summary(anova_result)
TukeyHSD(anova_result)

# Reorder species for plotting
species_order <- c("H. tiliaceus", "M. collina", "B. asiatica", "N. forsteri", 
                   "S. malaccense", "H. rosa-sinensis", "M. citrofolia", "D. erecta", 
                   "M. calvescens", "S. cumini", "L. camara", "S. campanulata")

damage_richness <- damage_richness %>%
  mutate(
    species = factor(species, levels = species_order),
    group = factor(group, levels = c("N", "R", "I")),
    richness = as.integer(richness)
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

#standard deviation 
richness_summary <- damage_richness %>%
  group_by(species, group) %>%
  summarise(
    mean_richness = mean(richness, na.rm = TRUE),
    sd_richness = sd(richness, na.rm = TRUE),
    .groups = "drop"
  )

#rarefaction 
library(vegan)
library(ggplot2)
library(tidyverse)

# read in leaf data
leaf <- leafdatawscore

# damage types per group to check
leaf %>% group_by(group, damage_type) %>% count() %>% ungroup(damage_type) %>% count()

# convert each row to a single leaf with damage types across columns, observed damage types noted as a "1"
leaf_wide <- leaf %>% 
  pivot_wider(
    id_cols = c("leaf_ID", "group"),
    names_from = "damage_type",
    values_from = "damage_type",
    values_fn = function(x) sum(!is.na(x)), 
    values_fill = 0
  )

# If you're not sure whether an NA column was created, safely remove it like this:
leaf_wide <- leaf_wide %>% select(where(~!all(is.na(.))))


# subset for each group
leaf_wide_I <- leaf_wide %>% filter(group == "I")
leaf_wide_N <- leaf_wide %>% filter(group == "N")
leaf_wide_R <- leaf_wide %>% filter(group == "R") 

# run species accumulation on our data
accum_I <- poolaccum(leaf_wide_I[,3:84])
accum_N <- poolaccum(leaf_wide_N[,3:84])
accum_R <- poolaccum(leaf_wide_R[,3:84])

# extract observed species richness estimates and create new dataframes
obs_N <- data.frame(summary(accum_N)$S, check.names = FALSE)
colnames(obs_N) <- c("N", "S", "lower2.5", "higher97.5", "std")
head(obs_N) # just to see what we are working with
obs_I <- data.frame(summary(accum_I)$S, check.names = FALSE)
colnames(obs_I) <- c("N", "S", "lower2.5", "higher97.5", "std")
obs_R <- data.frame(summary(accum_R)$S, check.names = FALSE)
colnames(obs_R) <- c("N", "S", "lower2.5", "higher97.5", "std")

# combine dataframes for each group
obs_N <- obs_N %>% mutate(type = "N")
obs_I <- obs_I %>% mutate(type = "I")
obs_all <- obs_R %>% mutate(type = "R") %>% bind_rows(obs_N, obs_I)

# plot all together to see curves in a single plot, colour-coded by plant group
rarefaction<- obs_all %>%
  ggplot(data = ., aes(x = N,
                       y = S, group = type)) +
  # Add confidence intervals
  geom_ribbon(aes(ymin = lower2.5,
                  ymax = higher97.5, fill = type),
              alpha = 0.5) +
  # Add observed richness line 
  geom_line(aes(colour = type)) +
  labs(x = "No. of leaves",
       y = "Observed damage richness", fill = "Group") +
  theme(
    panel.background = element_blank(),  # Remove background color
    panel.grid.major = element_line(color = "gray", size = 0.5),  # Adjust grid lines
    panel.grid.minor = element_blank(),  # Remove minor grid lines
    axis.text = element_text(size = 14),
    axis.title = element_text(size = 14),
    title = element_text(size = 14)
  )
ggsave("rarefaction.png", plot = rarefaction, width = 5.76, height = 6, units = "in")

##Specificty Score Richness

# Clean and summarize
richness_data_specificity <- leafdatawscore %>%
  filter(!is.na(new_score) & new_score != "N/A") %>%
  group_by(new_score, group, species, site) %>%
  summarise(richness = n_distinct(damage_type), .groups = "drop") %>%
  mutate(group = factor(group, levels = c("N", "R", "I")))

# Plot
specificity_plot <- ggplot(richness_data_specificity, aes(x = group, y = richness, fill = group)) +
  geom_boxplot() +
  geom_jitter(aes(color = group), size = 2, alpha = 0.7,
              position = position_jitter(width = 0.1, height = 0)) +
  facet_wrap(~ new_score, scales = "free_y") +
  labs(x = "NRI Group", y = "Richness of Damage Types") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("specificity_richness.pdf", specificity_plot, width = 6, height = 4)

# Subset the data to only include rows where the score category is "1"
score1_data <- subset(richness_data_specificity, new_score == "1")

# Perform a one-way ANOVA to test if richness differs between groups within "score_1"
anova_score <- aov(richness ~ group, data = score1_data)

# Show the summary (ANOVA table) of the results for "score_1"
summary(anova_score)

# Perform Tukey's Honestly Significant Difference (HSD) post-hoc test for "score_1"
TukeyHSD(anova_score)


# Repeat the same steps for "score_2"
score2_data <- subset(richness_data_specificity, new_score == "2")
anova_score <- aov(richness ~ group, data = score2_data)
summary(anova_score)
TukeyHSD(anova_score)


# Repeat the same steps for "score_3"
score3_data <- subset(richness_data_specificity, new_score == "3")
anova_score <- aov(richness ~ group, data = score3_data)
summary(anova_score)
TukeyHSD(anova_score)

# Perform a two-way ANOVA to test for the effects of both score category, group, 
# and their interaction on richness across the entire dataset
anova_full <- aov(richness ~ new_score * group, data = richness_data_specificity)

# Show the summary (ANOVA table) of the full model
summary(anova_full)

# Perform Tukey's HSD post-hoc test for the full model
TukeyHSD(anova_full)

## Calculate specificity score frequency proportions for each site/group/species/score
specificity_proportions_df <- leafdatawscore %>%
  group_by(site, group, species, new_score) %>%        # Group data by site, group, species, and specificity score
  summarise(Count = n(), .groups = "drop") %>%          # Count how many times each combination occurs
  group_by(site) %>%                                    # Group by site again
  mutate(Proportion = Count / sum(Count)) %>%           # Calculate the proportion of each group within a site
  ungroup()                                             # Remove grouping to flatten the data

## Plot specificity proportions by score
specificity_plot <- ggplot(specificity_proportions_df, aes(x = new_score, y = Proportion, fill = group)) +
  geom_boxplot() +                                      # Add a boxplot to show distribution
  geom_jitter(size = 1) +                               # Add points (jittered) to show individual data
  labs(x = "Specificity Score", y = "Proportion of Leaves Affected", fill = "Group") + # Axis labels
  scale_fill_manual(values = c(                        # Manually set colors for groups
    "N" = "#7392de",   # Native
    "R" = "#869948",   # Rare
    "I" = "#e0632d",   # Invasive
    "NA" = "grey"      # Not Available
  )) +
  theme_minimal() +                                     # Use minimal clean theme
  theme(axis.text = element_text(size = 9),             # Customize text size
        axis.title = element_text(size = 10)) +
  coord_flip()                                          # Flip axes for easier readability

## Display the plot
specificity_plot

## Save the plot as a PDF
ggsave("specificity_plot_freq.pdf", plot = specificty_plot, width = 6, height = 4, dpi = 300)

## --- Statistical Analysis ---

## Overall ANOVA: Does Proportion differ by group, specificity score, or their interaction?
anova_model <- aov(Proportion ~ group * new_score, data = specificity_proportions_df)
summary(anova_model)              # View the ANOVA table
TukeyHSD(anova_model)              # Post-hoc Tukey test to see pairwise differences

## --- ANOVA within each specificity score separately ---

## Subset for specificity score 1
p1_data <- subset(specificity_proportions_df, new_score == "1")
anova_result1 <- aov(Proportion ~ group, data = p1_data)
summary(anova_result1)
TukeyHSD(anova_result1)

## Subset for specificity score 2
p2_data <- subset(specificity_proportions_df, new_score == "2")
anova_result2 <- aov(Proportion ~ group, data = p2_data)
summary(anova_result2)
TukeyHSD(anova_result2)

## Subset for specificity score 3
p3_data <- subset(specificity_proportions_df, new_score == "3")
anova_result3 <- aov(Proportion ~ group, data = p3_data)
summary(anova_result3)
TukeyHSD(anova_result3)

## Subset for NA specificity scores(NA=undamaged)
pNA_data <- subset(specificity_proportions_df, is.na(new_score))

## Run ANOVA on Proportion ~ group for the NA score subset
anova_result_NA <- aov(Proportion ~ group, data = pNA_data)

## View the ANOVA summary
summary(anova_result_NA)

## Perform Tukey's HSD post-hoc test
TukeyHSD(anova_result_NA)

##Richness vs Functional Feeding Group 
# --- Calculate average richness for each group and functional feeding group ---
average_richness_funcfeed <- damage_richness %>%
  group_by(group, Func_feed_group) %>%
  summarise(avg_richness = mean(richness, na.rm = TRUE))

# --- Create a boxplot to visualize richness across functional feeding groups ---
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

# --- Save plot to file ---
ggsave("Richness_funcfeed.pdf", Richness_funcfeed, width = 10, height = 8)

# --- Two-way ANOVA: richness ~ NRI group * functional feeding group ---
anova_funcfeed <- aov(richness ~ group * Func_feed_group, data = damage_richness)

# --- View the ANOVA summary table ---
summary(anova_funcfeed)

# --- Perform Tukey's HSD post-hoc test ---
TukeyHSD(anova_funcfeed)

# -----------------------------------------------
# --- Functional Feeding Group Specific Analyses ---
# -----------------------------------------------

# Repeat the following for each herbivory type (hole feeding, margin feeding, etc.)

# 1. Hole Feeding Richness Analysis
hole_feeding_richness <- leafdatawscore %>%
  filter(herbivory_type == "hole feeding") %>%
  group_by(site, damage_type, group) %>%
  summarise(count = n(), .groups = "drop")

site_richness <- hole_feeding_richness %>%
  group_by(site, group) %>%
  summarise(
    hole_feeding_richness = n(),
    NRI_group = first(group),
    .groups = "drop"
  )

anova_result <- aov(hole_feeding_richness ~ NRI_group, data = site_richness)
summary(anova_result)
TukeyHSD(anova_result)

# 2. Margin Feeding Richness Analysis
margin_feeding_richness <- leafdatawscore %>%
  filter(herbivory_type == "margin feeding") %>%
  group_by(site, damage_type, group) %>%
  summarise(count = n(), .groups = "drop")

site_richness <- margin_feeding_richness %>%
  group_by(site, group) %>%
  summarise(
    margin_feeding_richness = n(),
    NRI_group = first(group),
    .groups = "drop"
  )

anova_result <- aov(margin_feeding_richness ~ NRI_group, data = site_richness)
summary(anova_result)
TukeyHSD(anova_result)

# 3. Skeletonization Richness Analysis
skeletonization_richness <- leafdatawscore %>%
  filter(herbivory_type == "skeletonization") %>%
  group_by(site, damage_type, group) %>%
  summarise(count = n(), .groups = "drop")

site_richness <- skeletonization_richness %>%
  group_by(site, group) %>%
  summarise(
    skeletonization_richness = n(),
    NRI_group = first(group),
    .groups = "drop"
  )

anova_result <- aov(skeletonization_richness ~ NRI_group, data = site_richness)
summary(anova_result)
TukeyHSD(anova_result)

# 4. Surface Feeding Richness Analysis
surface_feeding_richness <- leafdatawscore %>%
  filter(herbivory_type == "surface feeding") %>%
  group_by(site, damage_type, group) %>%
  summarise(count = n(), .groups = "drop")

site_richness <- surface_feeding_richness %>%
  group_by(site, group) %>%
  summarise(
    surface_feeding_richness = n(),
    NRI_group = first(group),
    .groups = "drop"
  )

anova_result <- aov(surface_feeding_richness ~ NRI_group, data = site_richness)
summary(anova_result)
TukeyHSD(anova_result)

# 5. Piercing and Sucking Richness Analysis
PS_richness <- leafdatawscore %>%
  filter(herbivory_type == "piercing and sucking") %>%
  group_by(site, damage_type, group) %>%
  summarise(count = n(), .groups = "drop")

site_richness <- PS_richness %>%
  group_by(site, group) %>%
  summarise(
    PS_richness = n(),
    NRI_group = first(group),
    .groups = "drop"
  )

anova_result <- aov(PS_richness ~ NRI_group, data = site_richness)
summary(anova_result)
TukeyHSD(anova_result)

# 6. Oviposition Richness Analysis
oviposition_richness <- leafdatawscore %>%
  filter(herbivory_type == "oviposition") %>%
  group_by(site, damage_type, group) %>%
  summarise(count = n(), .groups = "drop")

site_richness <- oviposition_richness %>%
  group_by(site, group) %>%
  summarise(
    oviposition_richness = n(),
    NRI_group = first(group),
    .groups = "drop"
  )

anova_result <- aov(oviposition_richness ~ NRI_group, data = site_richness)
summary(anova_result)
TukeyHSD(anova_result)

# 7. Galling Richness Analysis
galling_richness <- leafdatawscore %>%
  filter(herbivory_type == "galling") %>%
  group_by(site, damage_type, group) %>%
  summarise(count = n(), .groups = "drop")

site_richness <- galling_richness %>%
  group_by(site, group) %>%
  summarise(
    galling_richness = n(),
    NRI_group = first(group),
    .groups = "drop"
  )

anova_result <- aov(galling_richness ~ NRI_group, data = site_richness)
summary(anova_result)
TukeyHSD(anova_result)

# 8. Mining Richness Analysis
mining_richness <- leafdatawscore %>%
  filter(herbivory_type == "mining") %>%
  group_by(site, damage_type, group) %>%
  summarise(count = n(), .groups = "drop")

site_richness <- mining_richness %>%
  group_by(site, group) %>%
  summarise(
    mining_richness = n(),
    NRI_group = first(group),
    .groups = "drop"
  )

anova_result <- aov(mining_richness ~ NRI_group, data = site_richness)
summary(anova_result)
TukeyHSD(anova_result)

# 9. Fungal Richness Analysis 
fungal_richness <- leafdatawscore %>%
  filter(herbivory_type == "fungal") %>%
  group_by(site, damage_type, group) %>%
  summarise(count = n(), .groups = "drop")

site_richness <- fungal_richness %>%
  group_by(site, group) %>%
  summarise(
    fungal_richness = n(),
    NRI_group = first(group),
    .groups = "drop"
  )

anova_result <- aov(fungal_richness ~ NRI_group, data = site_richness)
summary(anova_result)
TukeyHSD(anova_result)
##Herbivory frequency by FFGs

# Load libraries
library(tidyverse)
# -----------------------------------------
# Step 1: Filter and preprocess data
# -----------------------------------------

# Remove missing data and fix herbivory_type
leafdata_cleaned <- leafdatawscore %>%
  filter(!is.na(Func_feed_group), !is.na(group), !is.na(leaf_ID)) %>%
  mutate(
    Func_feed_group = gsub("ovipositon", "oviposition", Func_feed_group)
  ) %>%
  filter(!grepl("incertae sedia", Func_feed_group))

# -----------------------------------------
# Step 2: Count unique leaf IDs per herbivory type
# -----------------------------------------

# For each Func_feed_group, site, species, and group:
# Count how many unique leaves had that damage (count leaf only once per type)
leafdata_funcfeed <- leafdata_cleaned %>%
  group_by(site, species, group, Func_feed_group, leaf_ID) %>%
  summarise(detected = 1, .groups = "drop") %>%  # only mark that it was present on that leaf
  group_by(site, species, group, Func_feed_group) %>%
  summarise(leaves_with_damage = n_distinct(leaf_ID), .groups = "drop") %>%
  mutate(proportion = leaves_with_damage / 50)  # assuming each site-species-group had 50 leaves sampled

# Reorder groups
leafdata_funcfeed$group <- factor(leafdata_funcfeed$group, levels = c("N", "R", "I"))

# -----------------------------------------
# Step 3: Create faceted boxplot
# -----------------------------------------

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

print(herbivory_plot)

# Save as PDF
ggsave("herbivory_proportion_boxplot.pdf", herbivory_plot, width = 12, height = 8)

# -----------------------------------------
# Step 4: Two-way ANOVA: group * herbivory_type
# -----------------------------------------

# Ensure factors are set
leafdata_funcfeed$Func_feed_group <- as.factor(leafdata_funcfeed$Func_feed_group)
leafdata_funcfeed$group <- factor(leafdata_funcfeed$group, levels = c("N", "R", "I"))

# Two-way ANOVA
anova_model <- aov(proportion ~ group * Func_feed_group, data = leafdata_funcfeed)
summary(anova_model)
TukeyHSD(anova_model)
options(max.print = 10000) 
# -----------------------------------------
# Step 5: Individual ANOVAs per herbivory type
# -----------------------------------------

# Loop through herbivory types
Func_feed_group <- unique(leafdata_funcfeed$Func_feed_group)

anova_results <- list()
tukey_results <- list()

for (type in Func_feed_group) {
  cat("\n--- Func_feed_group:", type, "---\n")
  df_sub <- filter(leafdata_funcfeed, Func_feed_group == type)
  model <- aov(proportion ~ group, data = df_sub)
  print(summary(model))
  anova_results[[type]] <- summary(model)
  
  tukey <- TukeyHSD(model)
  print(tukey)
  tukey_results[[type]] <- tukey
}

##ecto/endo/stylo
library(tidyverse)
library(RColorBrewer)

# Define functional categories
categories <- c(
  "margin feeding" = "Ectophytic",
  "hole feeding" = "Ectophytic",
  "surface feeding" = "Ectophytic",
  "skeletonization" = "Ectophytic",
  "oviposition" = "Stylophytic",
  "ovipositon" =  "Stylophytic",
  "piercing and sucking" = "Stylophytic",
  "mining" = "Endophytic",
  "galling" = "Endophytic",
  "fungal" = "Fungal",
  "incertae sedia" = "Stylophytic"
)

# Add category info to leafdata
leafdata_EESF <- leafdatawscore %>%
  mutate(category = recode(Func_feed_group, !!!categories)) %>%
  filter(!is.na(category))

# 2. For each damage_type, count how many distinct species it appears on
damage_species <- leafdata_EESF %>%
  distinct(damage_type, species, category) %>%    # one row per dt–sp–cat
  group_by(damage_type, category) %>%
  summarise(n_species = n(), .groups = "drop")

# 3. Now tally how many damage_types occur on exactly 1, 2, 3, … species, by category
damage_type_counts <- damage_species %>%
  group_by(n_species, category) %>%
  summarise(count = n(), .groups = "drop")

# Order categories
damage_type_counts$category <- factor(damage_type_counts$category, 
                                      levels = c("Endophytic", "Stylophytic", "Ectophytic", "Fungal"))

# Plot
ggplot(damage_type_counts, aes(x = factor(n_species), y = count, fill = category)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(
    x     = "Number of Host Species per Damage Type",
    y     = "Number of Damage Types",
    title = "How Many Damage Types Are Specialists vs. Generalists?",
    fill  = "Feeding Category"
  ) +
  scale_fill_brewer(palette = "Set1") +
  theme_minimal()


#Violin Plot
# Load required libraries for data manipulation and plotting
library(tidyverse)  # Includes ggplot2, dplyr, readr, etc.
library(ggplot2)    # Specifically for plotting

# Ensure 'new_score' is numeric to allow meaningful plotting on the y-axis
leafdatawscore$new_score <- as.numeric(leafdatawscore$new_score)

# Create a violin plot to visualize the distribution of 'new_score' across herbivory types
violin_plot <- ggplot(leafdatawscore, aes(x = Func_feed_group, y = new_score, fill = Func_feed_group)) +
  geom_violin(trim = FALSE, alpha = 0.6) +  # Violin plot to show distribution; keep full range (no trimming)
  geom_boxplot(width = 0.1, color = "black", alpha = 0.3, outlier.shape = NA) +  # Overlay boxplot inside violins, hide outliers
  geom_point(alpha = 0.5, color = "black", size = 1) +  # Add individual data points for additional transparency
  labs(
    title = "Violin Plot of Herbivory Type and Distribution of Scores",  # Plot title
    x = "Herbivory Type",   # x-axis label
    y = "Score"             # y-axis label
  ) +
  theme_minimal() +  # Clean, minimal background
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels for readability
    legend.position = "none"  # Remove legend (fill is redundant with x-axis labels)
  )

# Display the plot in the RStudio viewer or plotting window
print(violin_plot)

# Save the plot to a high-resolution PDF file for publication or sharing
ggsave("violin_plot.pdf", plot = last_plot(), width = 10, height = 6, dpi = 300)

##Confamilial PAIRS

#DT Richness
average_richness
# Filter data for confamilial groups
df_confamilial_1 <- average_richness %>% filter(species %in% c('D. erecta', 'L. camara'))
df_confamilial_2 <- average_richness %>% filter(species %in% c('H. tiliaceus', 'H. rosa-sinensis'))
df_confamilial_3 <- average_richness %>% filter(species %in% c('N. forsteri', 'M. citrofolia'))
df_confamilial_4 <- average_richness %>% filter(species %in% c('S. cumini', 'M. collina', 'S. malaccense'))

# Run t-test for DT richness between D. erecta and L. camara
t_test_result_1 <- t.test(avg_richness ~ species, data = df_confamilial_1)
# Print the results
print(t_test_result_1)

# Run t-test for DT richness between H. tiliaceus and H. rosa-sinensis
t_test_result_2 <- t.test(avg_richness ~ species, data = df_confamilial_2)
# Print the results
print(t_test_result_2)

# Run t-test for DT richness between N. forsteri and M. citrifolia
t_test_result_3 <- t.test(avg_richness ~ species, data = df_confamilial_3)
# Print the results
print(t_test_result_3)

# Run ANOVA for DT richness across multiple species (for example, for trio)
anova_result_1 <- aov(avg_richness ~ species, data = df_confamilial_4)

# Print the results
summary(anova_result_1)
# Perform Tukey HSD test for multiple comparisons
tukey_result_1 <- TukeyHSD(anova_result_1)

#rarefaction
library(vegan)
library(ggplot2)
library(tidyverse)

# read in leaf data
leaf <- leafdatawscore

# damage types per group to check
leaf %>% group_by(species, damage_type) %>% count() %>% ungroup(damage_type) %>% count()

leaf_wide <- leaf %>%
  # 1) remove any rows with missing damage_type
  filter(!is.na(damage_type)) %>%
  
  # 2) pivot so each damage_type becomes a column, counting presences per leaf
  pivot_wider(
    id_cols      = c(leaf_ID, species),
    names_from   = damage_type,
    values_from  = damage_type,
    values_fn    = ~sum(!is.na(.)),  # count non‐NA entries (i.e. presence)
    values_fill  = 0
  ) %>%
  
  # 3) convert counts >1 to a simple presence/absence (1/0)
  mutate(across(-c(leaf_ID, species), ~ if_else(. > 0, 1L, 0L)))

#subset for each group
leaf_wide_collina <- leaf_wide %>% filter(species == "M. collina")
leaf_wide_malay <- leaf_wide %>% filter(species == "S. malaccense")
leaf_wide_cumini <- leaf_wide %>% filter(species == "S. cumini") 

# run species accumulation on our data
accum_cumini <- poolaccum(leaf_wide_cumini[,3:84])
accum_collina <- poolaccum(leaf_wide_collina[,3:84])
accum_malay <- poolaccum(leaf_wide_malay[,3:84])

# extract observed species richness estimates and create new dataframes
obs_collina <- data.frame(summary(accum_collina)$S, check.names = FALSE)
colnames(obs_collina) <- c("N", "S", "lower2.5", "higher97.5", "std")
head(obs_collina) # just to see what we are working with
obs_cumini <- data.frame(summary(accum_cumini)$S, check.names = FALSE)
colnames(obs_cumini) <- c("N", "S", "lower2.5", "higher97.5", "std")
obs_malay <- data.frame(summary(accum_malay)$S, check.names = FALSE)
colnames(obs_malay) <- c("N", "S", "lower2.5", "higher97.5", "std")

# combine dataframes for each group
obs_collina <- obs_collina %>% mutate(type = "M. collina")
obs_cumini <- obs_cumini %>% mutate(type = "S. cumini")
obs_all <- obs_malay %>% mutate(type = "S. malaccense") %>% bind_rows(obs_collina, obs_cumini)

# plot all together to see curves in a single plot, colour-coded by plant group
rarefaction_3<- obs_all %>%
  ggplot(data = ., aes(x = N,
                       y = S, group = type)) +
  # Add confidence intervals
  geom_ribbon(aes(ymin = lower2.5,
                  ymax = higher97.5, fill = type),
              alpha = 0.5) +
  theme_bw()+
  # Add observed richness line 
  geom_line(aes(colour = type)) +
  theme(legend.position = "none")+
  labs(x = "No. of leaves",
       y = "Observed damage richness") +
  scale_fill_manual(values = c("#A30262", "#D162A4", "#FF9A56")) 
ggsave("rarefaction_3.png", plot = rarefaction_3, width = 13, height = 4, units = "in")

#Noni and Forsteri
#subset for each group
leaf_wide_noni <- leaf_wide %>% filter(species == "M. citrofolia ")
leaf_wide_forsteri <- leaf_wide %>% filter(species == "N. forsteri")

# run species accumulation on our data
accum_noni <- poolaccum(leaf_wide_noni[,3:84])
accum_forsteri <- poolaccum(leaf_wide_forsteri[,3:84])


# extract observed species richness estimates and create new dataframes
obs_noni<- data.frame(summary(accum_noni)$S, check.names = FALSE)
colnames(obs_noni) <- c("N", "S", "lower2.5", "higher97.5", "std")
head(obs_noni) # just to see what we are working with
obs_forsteri <- data.frame(summary(accum_forsteri)$S, check.names = FALSE)
colnames(obs_forsteri) <- c("N", "S", "lower2.5", "higher97.5", "std")

# combine dataframes for each group
obs_noni <- obs_noni %>% mutate(type = "M. citrifolia")
obs_forsteri <- obs_forsteri %>% mutate(type = "N. forsteri")
 %>% bind_rows(obs_noni, obs_forsteri)

str(leaf_wide_noni[, 3:84])

# plot all together to see curves in a single plot, colour-coded by plant group
rarefaction_nonfor<- obs_all %>%
  ggplot(data = ., aes(x = N,
                       y = S, group = type)) +
  # Add confidence intervals
  geom_ribbon(aes(ymin = lower2.5,
                  ymax = higher97.5, fill = type),
              alpha = 0.5) +
  theme_bw()+
  # Add observed richness line 
  geom_line(aes(colour = type)) +
  theme(legend.position = "none")+
  labs(x = "No. of leaves",
       y = "Observed damage richness") 
ggsave("rarefaction_nonfor.png", plot = rarefaction_nonfor, width = 13, height = 4, units = "in")

#seahib and hibrosa
#subset for each group
leaf_wide_seahib <- leaf_wide %>% filter(species == "H. tiliaceus")
leaf_wide_hibrosa <- leaf_wide %>% filter(species == "H. rosa-sinensis")

# run species accumulation on our data
accum_seahib <- poolaccum(leaf_wide_seahib[,3:84])
accum_hibrosa <- poolaccum(leaf_wide_hibrosa[,3:84])


# extract observed species richness estimates and create new dataframes
obs_seahib<- data.frame(summary(accum_seahib)$S, check.names = FALSE)
colnames(obs_seahib) <- c("N", "S", "lower2.5", "higher97.5", "std")
obs_hibrosa <- data.frame(summary(accum_hibrosa)$S, check.names = FALSE)
colnames(obs_hibrosa) <- c("N", "S", "lower2.5", "higher97.5", "std")

# combine dataframes for each group
obs_seahib <- obs_seahib %>% mutate(type = "H. tiliaceus")
obs_hibrosa <- obs_hibrosa %>% mutate(type = "H. rosa-sinensis") %>% bind_rows(obs_seahib, obs_hibrosa)
obs_all <- obs_seahib %>% mutate(type = "H. tiliaceus") %>% bind_rows(obs_hibrosa)

# plot all together to see curves in a single plot, colour-coded by plant group
rarefaction_seahibrosa<- obs_all %>%
  ggplot(data = ., aes(x = N,
                       y = S, group = type)) +
  # Add confidence intervals
  geom_ribbon(aes(ymin = lower2.5,
                  ymax = higher97.5, fill = type),
              alpha = 0.5) +
  theme_bw()+
  # Add observed richness line 
  geom_line(aes(colour = type)) +
  theme(legend.position = "none")+
  labs(x = "No. of leaves",
       y = "Observed damage richness") 
ggsave("rarefaction_seahibrosa.pdf", plot = rarefaction_seahibrosa, width = 13, height = 4, units = "in")

#lantana and duranta 
#subset for each group
leaf_wide_cam <- leaf_wide %>% filter(species == "L. camara")
leaf_wide_dur <- leaf_wide %>% filter(species == "D. erecta")

# run species accumulation on our data
accum_cam <- poolaccum(leaf_wide_cam[,3:84])
accum_dur <- poolaccum(leaf_wide_dur[,3:84])


# extract observed species richness estimates and create new dataframes
obs_cam<- data.frame(summary(accum_cam)$S, check.names = FALSE)
colnames(obs_cam) <- c("N", "S", "lower2.5", "higher97.5", "std")
obs_dur <- data.frame(summary(accum_dur)$S, check.names = FALSE)
colnames(obs_dur) <- c("N", "S", "lower2.5", "higher97.5", "std")


# combine dataframes for each group
obs_cams <- obs_cam %>% mutate(type = "L. camara")
obs_durs <- obs_dur %>% mutate(type = "D. erecta") %>% bind_rows(obs_cam, obs_dur)
obs_allcamdur <- obs_cams %>% mutate(type = "L. camara") %>% bind_rows(obs_durs)

# plot all together to see curves in a single plot, colour-coded by plant group
rarefaction_camdur<- obs_allcamdur %>%
  ggplot(data = ., aes(x = N,
                       y = S, group = type)) +
  # Add confidence intervals
  geom_ribbon(aes(ymin = lower2.5,
                  ymax = higher97.5, fill = type),
              alpha = 0.5) +
  theme_bw()+
  # Add observed richness line 
  geom_line(aes(colour = type)) +
  theme(legend.position = "none")+
  labs(x = "No. of leaves",
       y = "Observed damage richness") 
ggsave("rarefaction_camdur.pdf", plot = rarefaction_camdur, width = 13, height = 4, units = "in")

#Herbivory frequency 
herb_data_plot

herb_confamilial_1 <- herb_data_plot %>% filter(species %in% c('D. erecta', 'L. camara'))
herb_confamilial_2 <- herb_data_plot %>% filter(species %in% c('H. tiliaceus', 'H. rosa-sinensis'))
herb_confamilial_3 <- herb_data_plot %>% filter(species %in% c('N. forsteri', 'M. citrofolia'))
herb_confamilial_4 <- herb_data_plot %>% filter(species %in% c('S. cumini', 'M. collina', 'S. malaccense'))

# t-test for each pair
t_test_1 <- t.test(proportion_non_zero ~ species, data = herb_confamilial_1 )
t_test_2 <- t.test(proportion_non_zero ~ species, data = herb_confamilial_2 )
t_test_3 <- t.test(proportion_non_zero ~ species, data = herb_confamilial_3 )

# Print results
print(t_test_1)
print(t_test_2)
print(t_test_3)

# Run ANOVA for  richness across multiple species (for example, for trio)
anova_result_4 <- aov(proportion_non_zero ~ species, data = herb_confamilial_4)

# Print the results
summary(anova_result_4)
# Perform Tukey HSD test for multiple comparisons
tukey_result_4 <- TukeyHSD(anova_result_4)

#herbivory frequency by FFG

herb_data_FGG <- leafdatawscore %>% 
  group_by(leaf_ID, site, species, group, Func_feed_group) %>% 
  summarise(total_HI = sum(herbivory_index), .groups = "drop")

herb_data_FGG <- herb_data_FGG %>%
  filter(!is.na(total_HI)) %>%
  group_by(group, species, site, Func_feed_group) %>%
  summarise(
    proportion_non_zero = sum(total_HI > 0.01) / n(),  # Proportion of non-zero herbivory
    .groups = "drop"
  )

# Step 2: Plot
library(tidyverse)

# Helper function to make a boxplot per pair
plot_confamilial_pair <- function(herb_data_FGG, species_pair, group_label) {
  herb_data_FGG %>%
    filter(species %in% species_pair) %>%
    ggplot(aes(x = species, y = proportion_non_zero, fill = species)) +
    geom_boxplot(outlier.shape = NA, alpha = 0.7) +
    geom_jitter(aes(color = species), width = 0.2, size = 1.5, alpha = 0.6) +
    facet_wrap(~Func_feed_group, scales = "free_y") +
    labs(
      title = paste("Functional Feeding Groups for", group_label),
      x = "Species",
      y = "Proportion Non-Zero"
    ) +
    theme_minimal(base_size = 12) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    guides(fill = "none", color = "none")
}

# Generate plots for each pair
p1 <- plot_confamilial_pair(herb_data_FGG, c("D. erecta", "L. camara"), "D. erecta vs L. camara")
p2 <- plot_confamilial_pair(herb_data_FGG, c("H. tiliaceus", "H. rosa-sinensis"), "H. tiliaceus vs H. rosa-sinensis")
p3 <- plot_confamilial_pair(herb_data_FGG, c("N. forsteri", "M. citrofolia"), "N. forsteri vs M. citrofolia")
p4 <- plot_confamilial_pair(herb_data_FGG, c("S. cumini", "M. collina", "S. malaccense"), "S. cumini / M. collina / S. malaccense")

# View each plot one at a time, or use patchwork to combine
p1  # You can run each individually
p2
p3
p4

# Load required library
library(dplyr)

#--------------------------------------------
# Function to perform pairwise t-tests
#--------------------------------------------
# This function compares the mean proportion of non-zero herbivory observations 
# between two plant species for each functional feeding group using Welch's t-test.
# It returns the t-statistic and p-value for each group.

run_pairwise_ttests <- function(herb_data_FGG, species_pair, pair_label) {
  herb_data_FGG %>%
    filter(species %in% species_pair) %>%       # Subset data to the two species being compared
    filter(!is.na(Func_feed_group)) %>%         # Remove rows with NA in the feeding group
    group_by(Func_feed_group) %>%
    summarise(
      t_statistic = tryCatch(
        t.test(proportion_non_zero ~ species, var.equal = FALSE)$statistic, # Welch’s t-test
        error = function(e) NA
      ),
      p_value = tryCatch(
        t.test(proportion_non_zero ~ species, var.equal = FALSE)$p.value,
        error = function(e) NA
      ),
      .groups = "drop"
    ) %>%
    mutate(pair = pair_label) %>%               # Add a label for the species pair
    select(pair, Func_feed_group, t_statistic, p_value)
}

#--------------------------------------------
# Run pairwise t-tests for three species pairs
#--------------------------------------------
ttest_1 <- run_pairwise_ttests(herb_data_FGG, c("D. erecta", "L. camara"), "D. erecta vs L. camara")
ttest_2 <- run_pairwise_ttests(herb_data_FGG, c("H. tiliaceus", "H. rosa-sinensis"), "H. tiliaceus vs H. rosa-sinensis")
ttest_3 <- run_pairwise_ttests(herb_data_FGG, c("N. forsteri", "M. citrofolia"), "N. forsteri vs M. citrofolia")

# Combine all pairwise t-test results into a single table
all_ttests <- bind_rows(ttest_1, ttest_2, ttest_3)

# View all t-test results (increase n to print all rows)
print(all_ttests, n = 50)

#--------------------------------------------
# Function to perform one-way ANOVA across three species
#--------------------------------------------

run_anova_three_way <- function(herb_data_FGG, species_list) {
  herb_data_FGG %>%
    filter(species %in% species_list) %>%           # Subset to the three species of interest
    filter(!is.na(Func_feed_group)) %>%             # Remove rows with NA in the feeding group
    group_by(Func_feed_group) %>%
    summarise(
      F_statistic = tryCatch(
        summary(aov(proportion_non_zero ~ species))[[1]][["F value"]][1], # Extract F statistic
        error = function(e) NA
      ),
      p_value = tryCatch(
        summary(aov(proportion_non_zero ~ species))[[1]][["Pr(>F)"]][1], # Extract p-value
        error = function(e) NA
      ),
      .groups = "drop"
    ) %>%
    mutate(comparison = "S. cumini vs M. collina vs S. malaccense") %>%   # Label the comparison
    select(comparison, Func_feed_group, F_statistic, p_value)
}

#--------------------------------------------
# Run one-way ANOVA across three species
#--------------------------------------------
anova_results <- run_anova_three_way(herb_data_FGG, c("S. cumini", "M. collina", "S. malaccense"))
anova_results

#host specificity
# Filter data for the species pair: *L. camara* and *D. erecta*
specificity_camdur <- specificity_proportions_df %>%
  filter(species %in% c("L. camara", "D. erecta"))

# Plot specificity score vs. proportion of leaves affected
specificty_plot_camdur <- ggplot(specificity_camdur, aes(x = new_score, y = Proportion, fill = species)) +
  geom_boxplot() +                             # Boxplot to show distribution
  geom_jitter(size = 1) +                      # Adds individual data points for clarity
  labs(
    x = "Specificity score", 
    y = "Proportion of Leaves Affected", 
    fill = "Group"
  ) +
  theme_minimal() +
  theme(
    axis.text = element_text(size = 9), 
    axis.title = element_text(size = 10)
  )

# Filter for species pair: *N. forsteri* and *M. citrofolia*
specificity_nonfor <- specificity_proportions_df %>%
  filter(species %in% c("N. forsteri","M. citrofolia"))

# Set factor levels to control plot ordering
specificity_nonfor$species <- factor(specificity_nonfor$species, 
                                     levels = c("N. forsteri","M. citrofolia"))

# Create plot
specificty_plot_nonfor <- ggplot(specificity_nonfor, aes(x = new_score, y = Proportion, fill = species)) +
  geom_boxplot() +  
  geom_jitter(size = 1) +
  labs(
    x = "Specificity score", 
    y = "Proportion of Leaves Affected", 
    fill = "Group"
  ) +
  theme_minimal() +
  theme(axis.text = element_text(size = 9), axis.title = element_text(size = 10)) 

ggsave("specificty_plot_nonfor.pdf", plot = specificty_plot_nonfor, width = 6, height = 4, dpi = 300)


# Save figure as a high-resolution PDF
ggsave("specificty_plot_camdur.pdf", plot = specificty_plot_camdur, width = 6, height = 4, dpi = 300)

# Filter for species pair: *H. tiliaceus* and *H. rosa-sinensis*
specificity_roshib <- specificity_proportions_df %>%
  filter(species %in% c("H. tiliaceus", "H. rosa-sinensis" ))

specificity_roshib$species <- factor(specificity_roshib$species, 
                                     levels = c("H. tiliaceus", "H. rosa-sinensis"))

specificty_plot_roshib <- ggplot(specificity_roshib, aes(x = new_score, y = Proportion, fill = species)) +
  geom_boxplot() +  
  geom_jitter(size = 1) +
  labs(
    x = "Specificity score", 
    y = "Proportion of Leaves Affected", 
    fill = "Group"
  ) +
  theme_minimal() +
  theme(axis.text = element_text(size = 9), axis.title = element_text(size = 10)) 

ggsave("specificty_plot_roshib.pdf", plot = specificty_plot_roshib, width = 6, height = 4, dpi = 300)

# Filter for the three-way comparison: *M. collina*, *S. malaccense*, and *S. cumini*
specificity_3pair <- specificity_proportions_df %>%
  filter(species %in% c("M. collina","S. malaccense", "S. cumini"))

specificity_3pair$species <- factor(specificity_3pair$species, 
                                    levels = c("M. collina","S. malaccense", "S. cumini"))

specificty_plot_3pair <- ggplot(specificity_3pair, aes(x = new_score, y = Proportion, fill = species)) +
  geom_boxplot() +  
  geom_jitter(size = 1) +
  labs(
    x = "Specificity score", 
    y = "Proportion of Leaves Affected", 
    fill = "Group"
  ) +
  theme_minimal() +
  theme(axis.text = element_text(size = 9), axis.title = element_text(size = 10)) 

ggsave("specificty_plot_3pair.pdf", plot = specificty_plot_3pair, width = 6, height = 4, dpi = 300)

library(dplyr)
library(dplyr)

# Filter for score 1 only
score1_df <- specificity_proportions_df %>%
  filter(new_score == "1")

# Function to run t-test for a given pair
run_score1_ttest <- function(df, species_pair, label) {
  filtered <- df %>%
    filter(species %in% species_pair)
  
  t_test <- t.test(Proportion ~ species, data = filtered)
  
  data.frame(
    pair = label,
    t_statistic = t_test$statistic,
    p_value = t_test$p.value
  )
}

# Run for all three confamilial pairs
t1 <- run_score1_ttest(score1_df, c("L. camara", "D. erecta"), "L. camara vs D. erecta")
t2 <- run_score1_ttest(score1_df, c("N. forsteri", "M. citrofolia"), "N. forsteri vs M. citrofolia")
t3 <- run_score1_ttest(score1_df, c("H. tiliaceus", "H. rosa-sinensis"), "H. tiliaceus vs H. rosa-sinensis")

# Combine results
all_ttests_score1 <- bind_rows(t1, t2, t3)

print(all_ttests_score1)

# Filter for score 1 in the 3-species group
anova_df <- score1_df %>%
  filter(species %in% c("M. collina", "S. malaccense", "S. cumini")) %>%
  mutate(species = factor(species))

# Run ANOVA
anova_res <- aov(Proportion ~ species, data = anova_df)
summary(anova_res)

# Post-hoc test
TukeyHSD(anova_res)

# Filter for score 2 only
score2_df <- specificity_proportions_df %>%
  filter(new_score == "2")

# Function to run t-test for a given pair
run_score2_ttest <- function(df, species_pair, label) {
  filtered <- df %>%
    filter(species %in% species_pair)
  
  t_test <- t.test(Proportion ~ species, data = filtered)
  
  data.frame(
    pair = label,
    t_statistic = t_test$statistic,
    p_value = t_test$p.value
  )
}

# Run for all three confamilial pairs
t12 <- run_score2_ttest(score2_df, c("L. camara", "D. erecta"), "L. camara vs D. erecta")
t22 <- run_score2_ttest(score2_df, c("N. forsteri", "M. citrofolia"), "N. forsteri vs M. citrofolia")
t32 <- run_score2_ttest(score2_df, c("H. tiliaceus", "H. rosa-sinensis"), "H. tiliaceus vs H. rosa-sinensis")

# Combine results
all_ttests_score2 <- bind_rows(t12, t22, t32)

print(all_ttests_score2)

# Filter for score 2 in the 3-species group
anova_df <- score2_df %>%
  filter(species %in% c("M. collina", "S. malaccense", "S. cumini")) %>%
  mutate(species = factor(species))

# Run ANOVA
anova_res2 <- aov(Proportion ~ species, data = anova_df)
summary(anova_res2)

# Post-hoc test
TukeyHSD(anova_res2)

# Filter for score 3 only
score3_df <- specificity_proportions_df %>%
  filter(new_score == "3")

# Function to run t-test for a given pair
run_score3_ttest <- function(df, species_pair, label) {
  filtered <- df %>%
    filter(species %in% species_pair)
  
  t_test <- t.test(Proportion ~ species, data = filtered)
  
  data.frame(
    pair = label,
    t_statistic = t_test$statistic,
    p_value = t_test$p.value
  )
}

# Run for all three confamilial pairs
t13 <- run_score3_ttest(score3_df, c("L. camara", "D. erecta"), "L. camara vs D. erecta")
t23 <- run_score3_ttest(score3_df, c("N. forsteri", "M. citrofolia"), "N. forsteri vs M. citrofolia")
t33 <- run_score3_ttest(score3_df, c("H. tiliaceus", "H. rosa-sinensis"), "H. tiliaceus vs H. rosa-sinensis")

# Combine results
all_ttests_score3 <- bind_rows(t13, t23, t33)

print(all_ttests_score3)

# Filter for score 3 in the 3-species group
anova_df <- score3_df %>%
  filter(species %in% c("M. collina", "S. malaccense", "S. cumini")) %>%
  mutate(species = factor(species))

# Run ANOVA
anova_res3 <- aov(Proportion ~ species, data = anova_df)
summary(anova_res3)

# Post-hoc test
TukeyHSD(anova_res3)

#--------------------------------------------
# 0) Load libraries
#--------------------------------------------
library(dplyr)    # data manipulation
library(purrr)    # functional mapping
library(tibble)   # tibble() constructor

#--------------------------------------------
# 1) Define your species pairs & labels
#--------------------------------------------
species_pairs <- list(
  c("L. camara",    "D. erecta"      ),
  c("N. forsteri",  "M. citrofolia"  ),
  c("H. tiliaceus", "H. rosa-sinensis")
)
pair_labels <- c(
  "L. camara vs D. erecta",
  "N. forsteri vs M. citrofolia",
  "H. tiliaceus vs H. rosa-sinensis"
)

#--------------------------------------------
# 2) Subset to undamaged leaves only
#--------------------------------------------
undamaged_df <- specificity_proportions_df %>%
  filter(is.na(new_score)) %>%  # keep only undamaged
  drop_na(Proportion)           # ensure Proportion column is present

#--------------------------------------------
# 3) Safe t‑test function for undamaged proportions
#--------------------------------------------
run_undamaged_ttest <- function(df, species_pair, label) {
  # a) Filter to the two species
  sub <- df %>% filter(species %in% species_pair)
  
  # b) Count n per species
  counts <- sub %>% count(species)
  message("[Undamaged | ", label, "] counts: ",
          paste0(counts$species, "=", counts$n, collapse = ", "))
  
  # c) If either species has fewer than 2 obs, return NA
  if (nrow(counts) < 2 || any(counts$n < 2)) {
    warning(" → Insufficient undamaged data for ", label, "; returning NA")
    return(tibble(
      pair        = label,
      t_statistic = NA_real_,
      p_value     = NA_real_
    ))
  }
  
  # d) Run Welch’s t‑test
  tt <- t.test(Proportion ~ species, data = sub, var.equal = FALSE)
  tibble(
    pair        = label,
    t_statistic = unname(tt$statistic),
    p_value     = tt$p.value
  )
}

#--------------------------------------------
# 4) Run all undamaged-pair t‑tests
#--------------------------------------------
undamaged_tests <- map2_df(
  species_pairs, pair_labels,
  ~ run_undamaged_ttest(undamaged_df,  .x, .y)
)

print(undamaged_tests)

# Filter for undamaged in the 3-species group
anova_df <- undamaged_df %>%
  filter(species %in% c("M. collina", "S. malaccense", "S. cumini")) %>%
  mutate(species = factor(species))

# Run ANOVA
anova_res3 <- aov(Proportion ~ species, data = anova_df)
summary(anova_res3)

# Post-hoc test
TukeyHSD(anova_res3)
