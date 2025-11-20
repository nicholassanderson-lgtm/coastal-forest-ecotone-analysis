## -----------------------------------------------------------------------------------------------------------------------------------------------------------
pkg <- c("readxl", "vegan", "permute", "ggplot2", "boot", "dplyr", "car", "rstatix", "lmerTest", "emmeans", "dplyr", "tidyr", "ggtext", "viridis")

new_pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
if (length(new_pkg)) install.packages(new_pkg)
lapply(pkg, library, character.only = TRUE)


## -----------------------------------------------------------------------------------------------------------------------------------------------------------
# Code for the blocked permanova
file_path <- "C:/Users/Nicho/OneDrive/Desktop/TreeCompCap.xlsx"
tree_data <- read_excel(file_path, sheet = 1) 

species_cols <- c("Black gum", "Red Maple", "White oak", "Scarlet oak",
                  "Northern Red Oak", "Sassafras", "Pitch Pine",
                  "Black Cherry", "Black Oak", "Post Oak",
                  "Pignut Hickory", "Eastern red cedar", "American holly")

community_matrix <- tree_data[, species_cols]
env_data <- tree_data[, c("Location", "Plot")]

env_data$Plot <- as.factor(env_data$Plot)
env_data$Location <- as.factor(env_data$Location)

set.seed(42) 

d_bray <- vegdist(community_matrix, method = "bray")
perm_design <- how(blocks = env_data$Location)

permanova_result <- adonis2(
  community_matrix ~ Plot,
  data = env_data,
  permutations = 999,
  method = "bray",
  strata = env_data$Location 
)

print(permanova_result)

permanova_R2 <- permanova_result$R2[1]
permanova_p <- permanova_result$`Pr(>F)`[1]

disp_plot <- betadisper(d_bray, env_data$Plot)

print(anova(disp_plot))

permdisp_result <- permutest(disp_plot, permutations = 999)
print(permdisp_result)

permdisp_p <- permdisp_result$tab[1, "Pr(>F)"]
permdisp_F <- permdisp_result$tab[1, "F"]


## -----------------------------------------------------------------------------------------------------------------------------------------------------------
disp_df <- data.frame(distance = disp_plot$distances, Plot = env_data$Plot)
plot_colors <- c("1" = "#E69F00", "2" = "#56B4E9", "3" = "#009E73")

ggplot(disp_df, aes(x = Plot, y = distance, fill = Plot)) +
  geom_boxplot(alpha = 0.8, outlier.shape = NA) +
  geom_jitter(width = 0.15, alpha = 0.6, color = "black") +
  scale_fill_manual(values = plot_colors) +
  labs(
    x = "Plot Type (1=Edge, 2=Transition, 3=Interior)",
    y = "Distance to Group Centroid (Bray-Curtis)",
    title = "Homogeneity of Dispersion (PERMDISP)"
  ) +
  theme_bw(base_size = 12) +
  annotate("text", x = 2, y = max(disp_df$distance) - 0.05,
           label = paste0("PERMDISP F = ", round(permdisp_F, 3), ", p = ", round(permdisp_p, 3)), size = 4, fontface = "bold")

plot(disp_plot, 
     ellipse = TRUE, 
     col = plot_colors[disp_plot$group],
     main = "Multivariate Dispersion by Plot (PCoA Ordination)",
     sub = paste("Permutation p-value:", round(permdisp_p, 4)))

get_R2_nested <- function(data, indices) {
  d <- data[indices, ]
  sp <- d[, species_cols]
  env <- d[, c("Plot", "Location")]
  env[] <- lapply(env, factor)
  
  res <- adonis2(sp ~ Plot,
                 data = env,
                 permutations = 999, 
                 method = "bray",
                 strata = env$Location)
  return(res$R2[1])
}

set.seed(42)
boot_results_nested <- boot(tree_data, statistic = get_R2_nested, R = 3000)

boot.ci(boot_results_nested, type = "perc")


## -----------------------------------------------------------------------------------------------------------------------------------------------------------
plots <- levels(env_data$Plot)
comparison_pairs <- combn(plots, 2)
num_comparisons <- ncol(comparison_pairs)
pairwise_results_list <- list() 

for (i in 1:num_comparisons) {
  p1 <- comparison_pairs[1, i]
  p2 <- comparison_pairs[2, i]
  
  subset_indices <- env_data$Plot %in% c(p1, p2)
  community_sub <- community_matrix[subset_indices, ]
  env_sub <- env_data[subset_indices, ]
  
  env_sub <- droplevels(env_sub) 
  
  perm_result <- adonis2(
    community_sub ~ Plot,
    data = env_sub,
    method = "bray",
    permutations = 999,
    strata = env_sub$Location
  )
  
  
  unadj_p <- perm_result$`Pr(>F)`[1]
  adj_p <- min(unadj_p * num_comparisons, 1) 
  
  pairwise_results_list[[i]] <- data.frame(
    Comparison = paste(p1, "vs.", p2),
    Df = perm_result$Df[1],
    SumOfSqs = perm_result$SumOfSqs[1],
    R2 = perm_result$R2[1],
    Pseudo_F = perm_result$F[1],
    P_Value_Unadjusted = unadj_p,
    P_Value_Adjusted = adj_p
  )
}

pairwise_table <- do.call(rbind, pairwise_results_list) 

print(pairwise_table)


## -----------------------------------------------------------------------------------------------------------------------------------------------------------
#ordinance plots 
permanova_recalc <- adonis2(
  community_matrix ~ Plot,
  data = env_data,
  method = "bray",
  permutations = 999,
  strata = env_data$Location
)
permanova_R2_plot <- permanova_recalc$R2[1]
permanova_p_plot <- permanova_recalc$`Pr(>F)`[1]

set.seed(42)

nmds_result <- metaMDS(community_matrix, distance = "bray", k = 2, trymax = 100)

print(paste("NMDS Stress Value:", round(nmds_result$stress, 3)))

nmds_scores <- as.data.frame(scores(nmds_result, display = "sites"))
nmds_data <- cbind(nmds_scores, env_data)

ggplot(nmds_data, aes(x = NMDS1, y = NMDS2)) +
  geom_point(aes(color = Location, shape = Plot), size = 4, alpha = 0.8) +
  geom_polygon(aes(fill = Location, group = Location), alpha = 0.2, show.legend = FALSE) +
  scale_color_brewer(palette = "Set2") +
  scale_fill_brewer(palette = "Set2") +
  scale_shape_manual(
    name = "Plot",
    values = c(16, 17, 15), 
    labels = c("1 (Edge)", "2 (Transition)", "3 (Interior)")
  ) +
  labs(
    subtitle = paste("Stress:", round(nmds_result$stress, 3)),
    caption = paste0(
      "PERMANOVA Plot Effect: <i>R</i><sup>2</sup> = ", round(permanova_R2_plot, 5),
      ", <i>p</i> = ", round(permanova_p_plot, 3)
    )
  ) +
  theme_bw(base_size = 14) +
  theme(
    plot.caption = element_markdown(),
    plot.title = element_text(hjust = 0.5, face = "bold"),
    legend.title = element_text(face = "bold")
  )


## -----------------------------------------------------------------------------------------------------------------------------------------------------------
location_order <- c(
  "Gardiner County Park", 
  "Seatuck", 
  "Ludlow Creek",
  "Wertheim", 
  "Haven Point", 
  "Pine Neck", 
  "Shinnecock Reservation"
)

nmds_data$Location <- factor(nmds_data$Location, levels = location_order)
nmds_data$LocationOrder <- as.numeric(nmds_data$Location)

ggplot(nmds_data, aes(x = NMDS1, y = NMDS2)) +
  stat_ellipse(aes(color = Location, group = Location),
               linetype = "dashed", linewidth = 0.8, alpha = 0.5) +
  geom_polygon(
    data = nmds_data %>% group_by(Location) %>% slice(chull(NMDS1, NMDS2)),
    aes(x = NMDS1, y = NMDS2, fill = Location, group = Location),
    alpha = 0.08, color = "grey60", linewidth = 0.2, show.legend = FALSE
  ) +
  geom_point(aes(color = Location, shape = Plot), size = 5, alpha = 0.9) +
  scale_color_viridis_d(option = "plasma", direction = -1,
                        name = "Location (West → East)",
                        breaks = location_order, labels = location_order) +
  scale_fill_viridis_d(option = "plasma", direction = -1,
                       name = "Location (West → East)",
                       breaks = location_order, labels = location_order) +

  scale_shape_manual(
    name = "Plot Type",
    values = c(16, 17, 15), 
    labels = c("Edge", "Transition", "Interior")
  ) +

  labs(
    title = "",
    x = "NMDS1",
    y = "NMDS2",
    caption = paste0(
      "<i>PERMANOVA</i>: <i>R</i><sup>2</sup> = ", round(permanova_R2_plot, 3),
      ", <i>p</i> = ", round(permanova_p_plot, 3),
      "; <i>Stress</i> = ", round(nmds_result$stress, 3)
    )
  ) +

  theme_bw(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 16),
    plot.caption = element_markdown(hjust = 0, size = 11),
    legend.title = element_text(face = "plain"),
    legend.key.height = unit(1, "lines"),
    legend.position = "right",
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8)
  )


## -----------------------------------------------------------------------------------------------------------------------------------------------------------
species_env_data <- bind_cols(env_data, community_matrix)

species_abundance_by_plot <- species_env_data %>%
  group_by(Plot) %>%
  summarise(across(all_of(species_cols), \(x) sum(x, na.rm = TRUE)), .groups = "drop") %>%
  mutate(Total_N = rowSums(across(all_of(species_cols))))

relative_abundance_table <- species_abundance_by_plot %>%
  mutate(across(all_of(species_cols), ~ .x / Total_N, .names = "Rel_{.col}"))

final_summary_table <- species_abundance_by_plot %>%
  pivot_longer(
    cols = all_of(species_cols),
    names_to = "Species",
    values_to = "Abundance"
  ) %>%
  left_join(
    relative_abundance_table %>%
      pivot_longer(
        cols = starts_with("Rel_"),
        names_to = "Species",
        values_to = "Relative_Abundance"
      ) %>%
      mutate(Species = sub("^Rel_", "", Species)) %>%
      dplyr::select(Plot, Species, Relative_Abundance),
    by = c("Plot", "Species")
  ) %>%
  arrange(Plot, desc(Abundance))

print(final_summary_table, n = 39)


## -----------------------------------------------------------------------------------------------------------------------------------------------------------
dbh_path <- "C:/Users/Nicho/OneDrive/Desktop/DBH.xlsx"
density_path <- "C:/Users/Nicho/OneDrive/Desktop/Density.xlsx"
ba_path <- "C:/Users/Nicho/OneDrive/Desktop/BA.xlsx"

dbh_df <- read_excel(dbh_path) %>%
  mutate(
    Plot = factor(Plot, levels = c(1, 2, 3),
                  labels = c("Edge", "Transition", "Interior")),
    Location = as.factor(Location)
  ) %>%
  group_by(Location, Plot) %>%
  summarise(mean_DBH = mean(TreeSizes, na.rm = TRUE), .groups = "drop")

density_df <- read_excel(density_path) %>%
  mutate(
    Plot = factor(Plot, levels = c(1, 2, 3),
                  labels = c("Edge", "Transition", "Interior")),
    Location = as.factor(Location)
  ) %>%
  group_by(Location, Plot) %>%
  summarise(mean_density = mean(density, na.rm = TRUE), .groups = "drop")

ba_df <- read_excel(ba_path) %>%
  rename(BasalArea_acre = `Basal Area per acre`) %>%   
  mutate(
    Plot = factor(Plot, levels = c(1, 2, 3),
                  labels = c("Edge", "Transition", "Interior")),
    Location = as.factor(Location)
  ) %>%
  group_by(Location, Plot) %>%
  summarise(mean_BA = mean(BasalArea_acre, na.rm = TRUE), .groups = "drop")

combined_df <- dbh_df %>%
  left_join(density_df, by = c("Location", "Plot")) %>%
  left_join(ba_df, by = c("Location", "Plot"))

print(combined_df)


## -----------------------------------------------------------------------------------------------------------------------------------------------------------
location_order <- c(
  "Gardiner County Park", 
  "Seatuck", 
  "Ludlow Creek",
  "Wertheim", 
  "Haven Point", 
  "Pine Neck", 
  "Shinnecock Reservation"
)

combined_df$Location <- factor(combined_df$Location, levels = location_order)
plot_shapes <- c("Edge" = 16, "Transition" = 17, "Interior" = 15)

combined_df$mean_density_ha <- combined_df$mean_density / 0.404686
combined_df$mean_BA_ha <- combined_df$mean_BA / 0.404686

ggplot(combined_df, aes(x = mean_DBH, y = mean_density_ha)) +
  geom_point(
    aes(color = Location, shape = Plot, size = mean_BA_ha),
    alpha = 0.9, stroke = 0.8
  ) +

  scale_color_viridis_d(
    option = "plasma",
    direction = -1,
    name = "Location (West → East)",
    breaks = location_order,
    labels = location_order
  ) +

  scale_shape_manual(
    name = "Plot Type",
    values = plot_shapes
  ) +

  scale_size_continuous(
    range = c(2.5, 7.5), 
    breaks = c(100, 200, 300, 400, 500),
    name = expression(Basal~Area~(m^2~ha^-1))
  ) +

  labs(
    x = "Mean Tree Size (DBH, cm)",
    y = expression(Tree~Density~(stems~ha^-1))
  ) +

  guides(
    shape = guide_legend(
      order = 1,
      override.aes = list(size = 4),
      title.position = "top",
      title.hjust = 0.5
    ),
    color = guide_legend(
      order = 2,
      override.aes = list(size = 4),
      title.position = "top",
      title.hjust = 0.5
    ),
    size = guide_legend(
      order = 3,
      title.position = "top",
      title.hjust = 0.5,
      override.aes = list(alpha = 0.9)
    )
  ) +

  theme_bw(base_size = 14) +
  theme(
    legend.position = "right",
    legend.box = "vertical",
    legend.box.just = "left",
    legend.margin = margin(2, 6, 2, 6),
    legend.spacing.y = unit(0.4, "lines"),      # tighter legend stack
    legend.title = element_text(face = "bold", size = 11.5),
    legend.text = element_text(size = 10.5),
    legend.key.height = unit(0.7, "lines"),
    legend.key.width = unit(0.7, "lines"),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8),
    axis.title = element_text(size = 13, face = "plain"),
    axis.text = element_text(size = 12)
  )

