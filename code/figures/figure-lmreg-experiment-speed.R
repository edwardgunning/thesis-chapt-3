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
time_dt <- data.table(time_df)
time_dt[, ratio := time_2/time_1]



time_df_lng <- melt.data.table(data.table(time_df), id.vars = c("M", "sim_rep"), measure.vars = c("time_1", "time_2"))
time_df_lng[, minute := value / 60]
time_df_lng[, variable := factor(variable,
                                 levels = c("time_2", "time_1"),
                                 labels = c("\\texttt{fda::landmarkreg()}", "\\texttt{landmark\\_reg\\_block()}"))]


lm(value ~ M, data = time_df_lng[variable=="\\texttt{fda::landmarkreg()}"])
lm(value ~ M, data = time_df_lng[variable=="\\texttt{landmark\\_reg\\_block()}"])

p1 <- ggplot(data = time_df_lng) +
  aes(x = M, colour = variable, group = interaction(M, variable)) +
  geom_boxplot(aes(y=value), width =20, outlier.size = 0.75) +
  theme(legend.position = c(0.35, 0.925),
        legend.background = element_blank(),
        legend.title = element_blank()) +
  labs(x = "Number of Strides in Dataset",
       y = "Computation Time (seconds)") +
  scale_x_continuous(breaks = unique(time_df$M))
p1


p2 <- ggplot(data = time_dt) +
  aes(x = factor(M), y = ratio, group = M) +
  geom_boxplot(outlier.size = 0.9) +
  labs(x = "Number of Strides in Dataset",
       y = "Ratio of Computation Times")
p2


(combined_plot <- ggpubr::ggarrange(p1, p2))

tikz(file.path(plots_path, "lmreg-speed-plot.tex"),
     width = 1 * doc_width_inches, 
     height = 0.5 *  doc_width_inches)
combined_plot
dev.off()





