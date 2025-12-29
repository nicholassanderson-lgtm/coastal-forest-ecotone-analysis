## -----------------------------------------------------------------------------------------------------------------------------------------------------------
library(ggplot2)
library(dplyr)
library(tidyr)
library(patchwork)
install.packages("ggforce")   # only once


metric_colors <- c(
  "Structural Size" = "#1b9e77",
  "Density" = "#7570b3",  
  "Composition" = "#e7298a"  
)

plot_empirical_and_ratchet_fixed <- function() {
  
  empirical_raw <- tibble::tribble(
    ~Metric,           ~Edge,   ~Transition, ~Interior,
    "Structural Size",  11.8,     14.8,        15.8,
    "Density",          33.6,     43.4,        28.4,
    "Composition",      0.30,     0.15,        0.00
  )

  empirical_df <- empirical_raw |>
    pivot_longer(-Metric, names_to="Zone", values_to="Value") |>
    group_by(Metric) |>
    mutate(Relative = (Value - min(Value)) / (max(Value) - min(Value))) |>
    ungroup() |>
    mutate(Zone = factor(Zone, levels=c("Edge","Transition","Interior")))

 p_emp <- ggplot(empirical_df, aes(x=Zone, y=Relative, color=Metric, group=Metric)) +
    geom_line(size=1.3) +
    geom_point(size=3) +
    scale_color_manual(values=metric_colors) +
    labs(title="",
      x="Spatial Zone (Edge → Interior)",
      y="Relative Value (0–1)"
    ) + guides(color = guide_legend(title = NULL)) + 
    theme_bw(base_size=14) +
    theme(legend.position = "bottom",     
      plot.title = element_text(face="bold", hjust=0.5))

    x <- seq(0, 1, length.out=100)
  ratchet_df <- data.frame(
    Position = x,
    Structural_Size = 1.0 - 0.9 * (1 - x)^2,
    Density = 1.0 - 0.6 * (1 - x)^1.5,
    Composition = 1.0 - 0.8 * (1 - x)^1.1
  ) |>
    rename(`Structural Size` = Structural_Size) |>  
    pivot_longer(-Position, names_to="Metric", values_to="Value")

  p_ratchet <- ggplot(ratchet_df, aes(x=Position, y=Value, color=Metric)) +
    geom_line(size=1.3) +
    scale_color_manual(values=metric_colors) +
    labs(title="",
      x="Gradient (Edge → Interior)",
      y="Relative Value (0–1)"
    ) + guides(color = guide_legend(title = NULL)) + 
    theme_bw(base_size=14) +
    theme( legend.position = "bottom",
      plot.title = element_text(face="bold", hjust=0.5))

  list(empirical_plot=p_emp, ratchet_plot=p_ratchet)
}

plots <- plot_empirical_and_ratchet_fixed()

plots$ratchet_plot 
plots$empirical_plot


## -----------------------------------------------------------------------------------------------------------------------------------------------------------
library(ggplot2)
library(dplyr)
library(ggforce)

set.seed(42)

circle_r <- 1.25
point_r_mult <- 0.92

col_fac  <- "#D55E00" 
col_facu <- "#009E73" 

make_concept_patch <- function(cx, cy, n, mean_dbh, sd_dbh, p_fac) {
  tibble(
    angle = runif(n, 0, 2*pi),
    r     = sqrt(runif(n, 0, 1)) * (circle_r * point_r_mult),
    size  = pmin(pmax(rnorm(n, mean_dbh, sd_dbh), 2), 30),
    composition = ifelse(runif(n) < p_fac, col_fac, col_facu)
  ) %>%
    mutate(x = cx + r * cos(angle), y = cy + r * sin(angle))
}

dens_edge_con <- 15
dens_tran_con <- 30
dens_int_con  <- 45

dbh_edge_con <- 10
dbh_tran_con <- 14
dbh_int_con  <- 18
dbh_sd_con   <- 2

p_fac_edge <- 0.80
p_fac_tran <- 0.50
p_fac_int  <- 0.20

patch_edge_con <- make_concept_patch(1, 0, dens_edge_con, dbh_edge_con, dbh_sd_con, p_fac_edge) %>%
  mutate(Zone = "Edge", cx = 1, cy = 0)
patch_tran_con <- make_concept_patch(4, 0, dens_tran_con, dbh_tran_con, dbh_sd_con, p_fac_tran) %>%
  mutate(Zone = "Transition", cx = 4, cy = 0)
patch_int_con  <- make_concept_patch(7, 0, dens_int_con,  dbh_int_con,  dbh_sd_con, p_fac_int) %>%
  mutate(Zone = "Interior", cx = 7, cy = 0)

all_concept <- bind_rows(patch_edge_con, patch_tran_con, patch_int_con)

circle_df_con <- tibble(
  Zone = c("Edge","Transition","Interior"),
  cx   = c(1,4,7),
  cy   = 0,
  r    = circle_r
)

p_conceptual <- ggplot() +
  geom_circle(
    data = circle_df_con,
    aes(x0 = cx, y0 = cy, r = r),
    size = 1, color = "grey30", fill = "white"
  ) +
  geom_point(
    data = all_concept,
    aes(x = x, y = y, size = size, fill = composition),
    shape = 21, color = "black", alpha = 0.9
  ) +
  geom_text(
    data = circle_df_con,
    aes(x = cx, y = cy + (circle_r * 1.15), label = Zone),
    fontface = "bold", size = 6
  ) +
  scale_fill_identity(guide = "none") +
  scale_size_continuous(range = c(2, 8), guide = "none") +
  coord_equal() +
  theme_void(base_size = 14)

p_conceptual


## -----------------------------------------------------------------------------------------------------------------------------------------------------------
library(ggplot2)
library(dplyr)
library(ggforce)

set.seed(42)

circle_r <- 1.25      
point_r_mult <- 0.92   

prop_fac_edge <- 69/109
prop_fac_tran <- 78/131
prop_fac_int  <- 28/91

dbh_means <- c(Edge = 11.8, Transition = 14.8, Interior = 15.8)

density_vals <- c(Edge = 682, Transition = 875, Interior = 570)
max_circles <- 45
n_edge <- max(1, round(max_circles * (density_vals["Edge"] / max(density_vals))))
n_tran <- max_circles
n_int  <- max(1, round(max_circles * (density_vals["Interior"] / max(density_vals))))

col_fac  <- "#D55E00" 
col_facu <- "#009E73" 

make_empirical_patch <- function(cx, cy, n, mean_dbh, prop_fac) {
  n_fac <- round(n * prop_fac)
  comp_vec <- c(rep(col_fac, n_fac), rep(col_facu, n - n_fac))
  comp_vec <- sample(comp_vec, n) 

  size_raw <- rnorm(n, mean_dbh, 2)
  size_constrained <- pmin(pmax(size_raw, 2), 30)

  tibble(
    angle = runif(n, 0, 2*pi),
    r     = sqrt(runif(n, 0, 1)) * (circle_r * point_r_mult),
    size  = size_constrained,
    composition = comp_vec
  ) %>%
    mutate(x = cx + r * cos(angle), y = cy + r * sin(angle))
}

all_emp <- bind_rows(
  make_empirical_patch(1, 0, n_edge, dbh_means["Edge"],       prop_fac_edge) %>% mutate(Zone = "Edge"),
  make_empirical_patch(4, 0, n_tran, dbh_means["Transition"], prop_fac_tran) %>% mutate(Zone = "Transition"),
  make_empirical_patch(7, 0, n_int,  dbh_means["Interior"],   prop_fac_int)  %>% mutate(Zone = "Interior")
)

circle_df_emp <- tibble(cx = c(1,4,7), cy = 0, r = circle_r)

p_empirical <- ggplot() +
  geom_circle(
    data = circle_df_emp,
    aes(x0 = cx, y0 = cy, r = r),
    size = 1, color = "grey30", fill = "white"
  ) +
  geom_point(
    data = all_emp,
    aes(x = x, y = y, size = size, fill = composition),
    shape = 21, color = "black", alpha = 0.9
  ) +
  scale_fill_identity(
    guide  = "legend",
    breaks = c(col_facu, col_fac),
    labels = c("FACU/UPL species", "FAC species")
  ) +
  scale_size_continuous(range = c(2, 8), guide = "none") +
  coord_equal() +
  theme_void() +
  theme(
    legend.position = "bottom",
    legend.box      = "horizontal",
    legend.title    = element_blank(),
    legend.text     = element_text(size = 10)
  ) + guides(
    fill = guide_legend(
      override.aes = list(size = 4)
    )
  )

p_empirical


