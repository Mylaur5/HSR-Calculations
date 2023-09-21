# DMGFactor = (1 + FlatAtk/BaseAtk + Atk%)*(1 + CR*CD)




librarian::shelf(magrittr, vroom, ggpubr, tidyverse)
library(ggplot2)
library(dplyr)
# Load data
character_data <- vroom("data/character_data.tsv") %>%
  rename(character_name = colnames(.[1]))
light_cone_data <- vroom("data/light_cone_data.tsv") %>%
  rename(lc_name = colnames(.[1]))

base_atk <- character_data[1, ]$ATK + light_cone_data[1, ]$ATK
skill_multiplier <- 100
base_damage <- base_atk * skill_multiplier / 100

calculate_AverageCrit <- function(CRITRate, CRITDamage) {
  CRITRate <-
    pmax(0, pmin(100, CRITRate)) # Clamp CRITRate between 0% and 100%
  AverageCrit <- 1 + (CRITRate / 100) * (CRITDamage / 100)
  return(AverageCrit)
}

# Define a function to calculate damage based on factors
calculate_damage <-
  function(base_atk,
           skill_multiplier = 100,
           atk_percent = 0,
           crit_rate = 0,
           crit_damage = 0,
           dmg_multiplier = 0) {
    total_atk <- base_atk * (1 + atk_percent / 100)
    base_dmg <- total_atk * skill_multiplier / 100
    dmg_multiplier <- 1 + dmg_multiplier / 100
    average_crit <- calculate_AverageCrit(crit_rate, crit_damage)

    # Calculate damage using the formula
    damage <-
      base_dmg * average_crit * dmg_multiplier

    return(damage)
  }

calculate_damage(
  base_atk,
  skill_multiplier = 100,
  atk_percent = 0,
  crit_rate = 0,
  crit_damage = 0,
  dmg_multiplier = 0
) %>% print() -> dmg_calculated

relative_dmg <- ((dmg_calculated - base_damage) / base_damage) * 100


# Calculate relative damage increase as a percentage
df <- tibble(
  base_atk,
  skill_multiplier,
  base_damage,
)
df <- df %>%
  dplyr::mutate(relative_dmg = relative_dmg)

relative_damage_increase <- function(...) {
  ((calculate_damage(...) - base_damage) / base_damage) * 100
}

# Create a ggplot base with updated Y-axis scale and labels
base_plot <- ggplot(df) +
  xlim(0, 2 * base_atk) +
  labs(
    title = "Relative Damage Increase vs Base Damage",
    x = "Base Damage",
    y = "Relative Damage Increase (%)"
  ) +
  theme_pubclean()

# Define a function to add geom_function and geom_point layers
geom_function_base <- function(
    skill_multiplier = 100,
    atk_percent = 0,
    crit_rate = 0,
    crit_damage = 0,
    dmg_multiplier = 0,
    colour,
    data = df,
    legend_label = "Base") {
  geom_func <- geom_function(
    fun = relative_damage_increase,
    args = list(
      skill_multiplier = skill_multiplier,
      atk_percent = atk_percent,
      crit_rate = crit_rate,
      crit_damage = crit_damage,
      dmg_multiplier = dmg_multiplier
    ),
    aes(color = {{ legend_label }}),
    show.legend = TRUE,
  )
  geom_pt <- geom_point(
    data = data,
    aes(x = base_damage, y = relative_damage_increase(
      base_atk = base_atk,
      skill_multiplier = skill_multiplier,
      atk_percent = atk_percent,
      crit_rate = crit_rate,
      crit_damage = crit_damage,
      dmg_multiplier = dmg_multiplier
    ),
    colour = {{ legend_label }}),
    size = 1,
    show.legend = TRUE
  )

  return(list(geom_func, geom_pt))
}

# Add the geom_function layers to the base plot
final_plot <- base_plot +
  geom_function_base(data = df, colour = "black", legend_label = "Base") +
  geom_function_base(atk_percent = 100, colour = "red", legend_label = "ATK") +
  geom_function_base(crit_rate = 70, crit_damage = 100, colour = "orange", legend_label = "CR") +
  geom_function_base(crit_rate = 50, crit_damage = 150, colour = "purple", legend_label = "CD") +
  geom_function_base(dmg_multiplier = 60, colour = "darkgreen", legend_label = "DMG") +
  scale_color_manual(
    name = "Legend",
    values = c(
      "Base" = "black",
      "ATK" = "red",
      "CR" = "orange",
      "CD" = "purple",
      "DMG" = "darkgreen"
    )
  )

# Display the final plot
print(final_plot)


