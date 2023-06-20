library(data.table) # CRAN v1.14.2
library(ggplot2)    # CRAN v3.4.0
library(tikzDevice)
source(here::here("code/functions/theme_gunning.R"))
theme_gunning()
plots_path <- here::here("outputs", "plots")
# Some settings for the plots: --------------------------------------------
theme_gunning() # use default theme
theme_update(
  axis.text = element_text(size = 10),
  axis.title = element_text(size = 10),
  strip.text = element_text(size = 10),
  plot.title = element_text(size = 11),
  legend.text = element_text(size = 10),
  legend.title = element_text(size = 10)
)
# rough guide for sizing of plot outputs:
doc_width_cm <- 16
doc_width_inches <- doc_width_cm *  0.3937


current_repo_path <- here::here(
  "outputs",
  "data",
  "risc1-registered-basis-coef.rds"
)

basis_coef_current <- readRDS(current_repo_path)


basis_coef_subset <- basis_coef_current[subject_id %in% c("P_4001", "P_4002"),
                   .(subject_id = subject_id[1],
                     side = side[1], 
                     peak_kfa_time = peak_kfa_time[1], 
                     stride_length_frames = stride_length_frames[1], long_time = long_time[1]),
                   by = trial_id]

plot_dt_lng <- melt.data.table(data = basis_coef_subset, 
                               id.vars = c("trial_id", "subject_id", "side", "long_time"),
                               measure.vars = c("stride_length_frames", "peak_kfa_time"),
                               variable.factor = FALSE, value.factor = FALSE)

plot_dt_lng[, participant_id := stringr::str_remove(subject_id, "P_400")]
plot_dt_lng[, variable := factor(variable, levels = c("peak_kfa_time", "stride_length_frames"),
                                 labels = c("\\textbf{(a)} Peak KFA Timing ($\\%$ of Stride)", "\\textbf{(b)} Stride Length (Frames)"))]
plot_dt_lng[, side := factor(side, levels = c("left", "right"), labels = c("Left", "Right"))]
# warning -- stride length frames is changed from integer to double, that's ok!
p<-ggplot(data = plot_dt_lng) +
  aes(x = long_time, y = value, colour = participant_id,
      group = interaction(subject_id, side),
      shape = side,
      linetype = side) +
  facet_wrap(~ variable, scales = "free_y") +
  geom_line() +
  geom_point() +
  xlab("Longitudinal Time ($T$)") +
  scale_color_manual(values = c("red3", "#619CFF"), name = "Participant ID:") +
  scale_linetype_manual(values = c(1, 4), name = "Side:") +
  scale_shape_manual(values = c(20, 17),  name = "Side:") +
  scale_x_continuous(breaks = c(0, 0.25, 0.5, 0.75, 1), labels = paste(c(0, 0.25, 0.5, 0.75, 1))) +
  theme(legend.position = "bottom",
        legend.title = element_text(face = "bold"),
        axis.title.y = element_blank())
p
tikz(file.path(plots_path, "phase-plot.tex"),
     width = 1.2 * doc_width_inches, 
     height = 0.675 * doc_width_inches,
     standAlone = TRUE)
print(p)
dev.off()

tinytex::lualatex(file.path(plots_path, "phase-plot.tex"))

