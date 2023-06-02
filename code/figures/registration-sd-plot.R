# Packages: ---------------------------------------------------------------
library(data.table) # CRAN v1.14.2
library(ggplot2)    # CRAN v3.4.0
library(tikzDevice) # CRAN v0.12.3.1
source(here::here("code", "functions", "theme_gunning.R"))

# Plot Settings: ----------------------------------------------------------
plots_path <- here::here("outputs", "plots")
theme_gunning()
theme_update(strip.text = element_text(size = 9),
             text = element_text(size = 9),
             panel.grid.minor = element_blank(),
             axis.title = element_text(size = 9),
             legend.text = element_text(size = 9, hjust = 0.5),
             legend.title = element_text(size = 9, hjust = 0.5),
             plot.title = element_text(hjust = 0.5, size = 11))
doc_width_cm <- 16
doc_width_inches <- doc_width_cm *  0.3937
options(scipen = 999)



# -------------------------------------------------------------------------
plot_sds_dt_lng <- readRDS(file = here::here("outputs", "data", "registration_plot_sds_dt_lng.rds"))
plot_sds_dt_lng[,plane_of_motion := factor(plane_of_motion,
                                           levels = c("fle", "abd", "rot"),
                                           labels = c("\\emph{flexion}", "\\emph{abduction}", "\\emph{rotation}"))]
plot_sds_dt_lng[, registered := factor(registered,
                                     levels = c("unreg", "reg"),
                                     labels = c("Before Registration",
                                                "After Registration"))]

reg_plot <- ggplot(plot_sds_dt_lng) +
  aes(x = t_grid, y = sd, group = registered, color = registered) +
  facet_wrap(location ~ plane_of_motion, ncol = 3, scales = "free_y") +
  geom_line() +
  labs(x = "Normalised Time ($\\%$ of Stride)", y = "Standard Deviation") +
  scale_color_hue(name = "", h = c(5, 250)) +
  theme(legend.position = "bottom")

tikz(file.path(plots_path, "pointwise-sd-registration.tex"),
     width = 0.8 * doc_width_inches, 
     height = 0.9 *  doc_width_inches)
reg_plot
dev.off()
