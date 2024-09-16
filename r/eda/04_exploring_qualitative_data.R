# Load necessary libraries
library(tidyverse)
library(ggplot2)
library(vcd)  # For mosaic plots
library(bayesplot)

# Set the bayesplot default theme
theme_set(bayesplot::theme_default())

# Set a seed for reproducibility
set.seed(sum(utf8ToInt("frequency_distributions")))

# Load the data
dat <- read.csv("../psicometria/data/penguins.csv")

# Remove rows with missing values
df <- na.omit(dat)

# Create a contingency table
contingency_table <- table(df$island, df$species)

# Print the contingency table
print(contingency_table)

# Bar plot for total number of penguins per island
ggplot(df, aes(x = island)) +
  geom_bar(alpha = 0.5) +
  ggtitle("Numero totale di pinguini per isola") +
  xlab("Isola") +
  ylab("Numero di pinguini")

# Bar plot for total number of penguins per species
ggplot(df, aes(x = species)) +
  geom_bar(alpha = 0.5) +
  ggtitle("Numero totale di pinguini per specie") +
  xlab("Specie") +
  ylab("Numero di pinguini")

# Stacked bar plot for number of penguins by species and island
ggplot(df, aes(x = island, fill = species)) +
  geom_bar(position = "stack") +
  ggtitle("Numero di pinguini per specie e isola") +
  xlab("Isola") +
  ylab("Numero di pinguini") +
  labs(fill = "Specie")

# Stacked bar plot for proportion of penguins by species and island
ggplot(df, aes(x = island, fill = species)) +
  geom_bar(position = "fill") +
  ggtitle("Proporzione di pinguini per specie e isola") +
  xlab("Isola") +
  ylab("Proporzione") +
  labs(fill = "Specie")

# Mosaic plot
mosaic(~ species + island, data = df, main = "Mosaic Plot of Species and Island")

# Calculate and display row proportions
row_proportions <- df %>%
  dplyr::count(island, species) %>%
  dplyr::group_by(island) %>%
  dplyr::mutate(proportion = n / sum(n)) %>%
  tidyr::pivot_wider(names_from = species, values_from = proportion, values_fill = 0) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(across(where(is.numeric), ~ round(., 3))) %>%
  dplyr::mutate(Total = rowSums(dplyr::select(., -island)))

print(row_proportions)

# Calculate and display column proportions
column_proportions <- df %>%
  dplyr::count(island, species) %>%
  dplyr::group_by(species) %>%
  dplyr::mutate(proportion = n / sum(n)) %>%
  tidyr::pivot_wider(names_from = island, values_from = proportion, values_fill = 0) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(across(where(is.numeric), ~ round(., 3))) %>%
  dplyr::mutate(Total = rowSums(dplyr::select(., -species)))

print(column_proportions)

# Bar plot for gender distribution by species
ggplot(df, aes(x = species, fill = sex)) +
  geom_bar(position = "dodge") +
  ggtitle("Distribuzione del genere per specie") +
  xlab("Specie") +
  ylab("Conteggio")

# Violin plot for body mass distribution by species and sex
ggplot(df, aes(x = species, y = body_mass_g, fill = sex)) +
  geom_violin(position = position_dodge(width = 0.9), alpha = 0.5) +
  geom_boxplot(position = position_dodge(width = 0.9), width = 0.2, alpha = 0.8) +
  ggtitle("Distribuzione di Body Mass in funzione di specie e genere") +
  xlab("Specie") +
  ylab("Body Mass (g)") +
  labs(fill = "Genere")




