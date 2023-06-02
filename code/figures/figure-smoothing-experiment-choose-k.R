# Packages and Functions: -------------------------------------------------
library(ggplot2)    # CRAN v3.4.0
library(data.table) # CRAN v1.14.2
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

plot_mses_dt <- fread(file = here::here("outputs", "data", "plot_mses_dt_choosing_k.csv"))
plot_mses_dt[,plane_of_motion := factor(plane_of_motion,
                                        levels = c("fle", "abd", "rot"),
                                        labels = c("\\emph{flexion}", "\\emph{abduction}", "\\emph{rotation}"))]

plot_mses_dt[, location := factor(location, levels = c("Hip", "Knee", "Ankle"))]


# "P_4213" & side == "right" & stride_num == 83?
plot_mses_dt_trimmed <- plot_mses_dt[!((subject_id == "P_4230" & side == "left" & stride_num == 63) |
                                         (subject_id == "P_4117" & side == "right" & stride_num == 63) |
                                         (subject_id == "P_4121" & side == "left" & stride_num %in% c(70, 71))|
                                         (subject_id == "P_4121" & side == "right" & stride_num %in% c(70)) |
                                         (subject_id == "P_4122" & side == "left" & stride_num == 67) |
                                         (subject_id == "P_4217" & side == "left" & stride_num %in% c(3:5, 40:41, 60)) |
                                         (subject_id == "P_4217" & side == "right" & stride_num %in% c(3:6, 40:41, 62:63)))] # |



boxplot_choose_k <- ggplot(data = plot_mses_dt_trimmed[quantity == "Angles" & location %in% c("Hip", "Knee", "Ankle")]) +
  geom_vline(xintercept = c(60, 80, 100), col = "grey", lty = "dashed")+
  aes(x = n_basis,
      y = rmse,
      colour = factor(n_basis),
      group = n_basis) +
  geom_vline(xintercept = 80, color = "darkgrey") +
  geom_boxplot(outlier.size = 0.1) +
  facet_wrap(location ~ plane_of_motion,
             scales = "free_y",
             ncol = 3) +
  labs(y = "Root Mean Squared Error (RMSE)",
       x = "Number of Basis Functions ($K$)",
       color = "$K$:")  +
  theme(legend.position = "bottom") +
  guides(colour = guide_legend(nrow = 2))

boxplot_choose_k
tikz(file.path(plots_path, "choose-k-boxplots.tex"),
     width = 0.8 * doc_width_inches, 
     height = 0.9 *  doc_width_inches)
boxplot_choose_k
dev.off()

plot_mses_dt_trimmed[location=="Ankle" & plane_of_motion == "rot" & quantity == "Angles" & n_basis==55][rev(order(mse))][1:10]
weird_stride <- unlist(risc1_dt[subject_id == "P_4117" & side == "right" & stride_num == 63 & location == "Ankle" & quantity == "Angles" &
                                  plane_of_motion == "fle", data_0:data_197])

