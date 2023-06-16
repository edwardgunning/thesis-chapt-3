library(ggplot2)    # CRAN v3.4.0 
library(tikzDevice) # CRAN v0.12.3.1
library(data.table) # CRAN v1.14.2

# Plot Settings: ----------------------------------------------------------
source(here::here("code", "functions", "theme_gunning.R"))
theme_gunning()
doc_width_cm <- 16
doc_width_inches <- doc_width_cm *  0.3937
options(scipen = 999)

# -------------------------------------------------------------------------
plots_path <- here::here("outputs", "plots")


# Read in results: --------------------------------------------------------
results_list <- readRDS(
  file = here::here("outputs", "data", "lm-reg-speed-simulation-settings.rds")
)

time_df <- results_list$time_df
time_df_lng <- melt.data.table(data.table(time_df), id.vars = c("M", "sim_rep"), measure.vars = c("time_1", "time_2"))
time_df_lng[, minute := value / 60]
time_df_lng[, variable := factor(variable,
                                 levels = c("time_2", "time_1"),
                                 labels = c("\\texttt{landmarkreg()}", "\\texttt{landmark\\_reg\\_block()}"))]
lmreg_plot <- ggplot(data = time_df_lng) +
  aes(x = M, colour = variable, group = interaction(M, variable)) +
  geom_boxplot(aes(y = value), outlier.size = 0.5) +
  # scale_y_log10(breaks = c(
  #   0.0001,
  #   0.001,
  #   0.01,
  #   0.1,
  #   1
  # ), labels = paste(c(
  #   0.0001,
  #   0.001,
  #   0.01,
  #   0.1,
  #   1
  # ))) +
  scale_y_log10() +
  scale_x_log10(breaks = c(
    1,
    10,
    100#,
    # 1000,
    # 10000
  ), labels = paste(c(
    1,
    10,
    100#,
    # 1000,
    # 10000
  ))) +
  labs(y = "Time (seconds)", x = "Number of Strides in Dataset", color = "Algorithm:") +
  theme(legend.position = "bottom")

lmreg_plot

tikz(file.path(plots_path, "lmreg-speed-plot.tex"),
     width = 0.55 * doc_width_inches, 
     height = 0.575 *  doc_width_inches)
lmreg_plot
dev.off()
