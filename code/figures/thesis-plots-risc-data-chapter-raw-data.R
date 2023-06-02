# Analysis/ sampling originally in file: code/full-data/rough-work-smoothing.R

# Set up -----------------------------------------------------------------
library(data.table) # CRAN v1.14.0 
library(ggplot2)    # CRAN v3.3.5
library(tikzDevice) # CRAN v0.12.3.1
library(ggpubr)     # CRAN v0.4.0
library(ggnewscale) # CRAN v0.4.8
library(fda)        # CRAN v5.5.1
source(file = here::here("code", 
                         "functions",
                         "functions-helper-smoothing.R"))


# -------------------------------------------------------------------------
plots_path <- here::here("outputs", "plots")

# -------------------------------------------------------------------------
doc_width_cm <- 16
doc_width_inches <- doc_width_cm *  0.3937
options(scipen = 999)
# -------------------------------------------------------------------------

risc1_dt <- fread(file = here::here("outputs",
                                           "data",
                                           "risc1_intermediate_copy_01.csv"))



# ------------------------------------------------------------------------#
# ------------------------------------------------------------------------#
subset_dt <- risc1_dt[
  subject_id == "P_4001" & side == "right" &
    quantity == "Angles" &
    plane_of_motion == "fle" &
    location %in% c("Hip", "Knee", "Ankle") 
]
subset_dt <- subset_dt[, c("stride_num", "location", paste0("data_", 0:197))]
subset_dt_lng <- melt.data.table(data = subset_dt,
                                 id.vars = c("location", "stride_num"), 
                                 measure.vars = paste0("data_", 0:197),
                                 variable.name = "frame",
                                 value.name = "angle", 
                                 variable.factor = FALSE,
                                 value.factor = FALSE,
                                 na.rm = TRUE,
                                 verbose = TRUE)
subset_dt_lng[, frame := as.numeric(stringr::str_remove(frame, "data_"))]
subset_dt_lng
setorderv(subset_dt_lng, c("location", "stride_num", "frame"))
head(subset_dt_lng)
subset_dt_lng[, frame_new := 0:(.N-1), by = location]



sample_hz <- 200
second_per_frame <- 1/sample_hz
subset_dt_lng[, seconds := second_per_frame * frame_new]
segment_times <- unique(subset_dt_lng[frame == 0, seconds])

theme_set(theme_bw())
theme_update(strip.text = element_text(size = 9),
             text = element_text(size = 9),
             panel.grid.minor = element_blank(),
             axis.title = element_text(size = 9),
             legend.text = element_text(size = 9, hjust = 0.5),
             axis.text = element_text(size = 9),
             legend.position = "none",
             legend.title = element_text(size = 9, hjust = 0.5),
             plot.title = element_text(hjust = 0.5, size = 11))
p <- ggplot(data = subset_dt_lng) +
  aes(x = seconds, y = angle, colour = factor(stride_num)) +
  facet_wrap(~ location, nrow = 3, ncol = 1, "free_y") +
  #geom_line() +
  geom_point(col = "black", size = 0.075)+
  labs(colour = "Stride Number:") +
  #geom_vline(xintercept = segment_times, col = "grey", lty = 2) +
  labs(y = "Angle ($^{\\circ}$)", x = "Time (seconds)") +
  scale_x_continuous(expand = c(0.025, 0.025))
p




tikz(file.path(plots_path, "raw-minutes-data.tex"),
     width = 1 * doc_width_inches, 
     height = 0.6 *  doc_width_inches)
p
dev.off()



# Now, zoomed in on first 10 strides: -------------------------------------

# ------------------------------------------------------------------------#
subset_dt <- risc1_dt[
  subject_id == "P_4001" & side == "right" &
    quantity == "Angles" &
    plane_of_motion == "fle" &
    stride_num %in% 1:10 &
    location %in% c("Hip", "Knee", "Ankle") 
]
subset_dt <- subset_dt[, c("stride_num", "location", paste0("data_", 0:197))]
subset_dt_lng <- melt.data.table(data = subset_dt,
                                 id.vars = c("location", "stride_num"), 
                                 measure.vars = paste0("data_", 0:197),
                                 variable.name = "frame",
                                 value.name = "angle", 
                                 variable.factor = FALSE,
                                 value.factor = FALSE,
                                 na.rm = TRUE,
                                 verbose = TRUE)
subset_dt_lng[, frame := as.numeric(stringr::str_remove(frame, "data_"))]
subset_dt_lng
setorderv(subset_dt_lng, c("location", "stride_num", "frame"))
head(subset_dt_lng)
subset_dt_lng[, frame_new := 0:(.N-1), by = location]



sample_hz <- 200
second_per_frame <- 1/sample_hz
# millisecond_per_frame <- 1000 * second_per_frame
subset_dt_lng[, seconds := second_per_frame * frame_new]
segment_times <- unique(subset_dt_lng[frame == 0, seconds])

theme_set(theme_bw())
theme_update(strip.text = element_text(size = 9),
             text = element_text(size = 9),
             panel.grid.minor = element_blank(),
             axis.title = element_text(size = 9),
             legend.text = element_text(size = 9, hjust = 0.5),
             axis.text = element_text(size = 9),
             legend.position = "none",
             legend.title = element_text(size = 9, hjust = 0.5),
             plot.title = element_text(hjust = 0.5, size = 11))
ptop <- ggplot(data = subset_dt_lng) +
  aes(x = seconds, y = angle, colour = factor(stride_num)) +
  facet_wrap(~ location, nrow = 3, ncol = 1, "free_y") +
  geom_point(col = "black", size = 0.075)+
  labs(colour = "Stride Number:") +
  geom_vline(xintercept = segment_times, col = "darkgrey", lty = 2, linewidth = 0.75) +
  labs(y = "Angle ($^{\\circ}$)", x = "Time (seconds)") +
  scale_x_continuous(expand = c(0.025, 0.025))

pbottom <-  ggplot(data = subset_dt_lng) +
  aes(x = second_per_frame * frame, y = angle, group = stride_num) +
  facet_wrap(~ location, ncol = 3, nrow=1, "free_y") +
  geom_line(linewidth = 0.25) +
  geom_point(col = "black", size = 0.075)+
  labs(colour = "Stride Number:") +
  labs(y = "Angle ($^{\\circ}$)", x = "Time (seconds)") +
  scale_x_continuous(expand = c(0.025, 0.025))

combined_plot <- ggarrange(ptop, pbottom, ncol = 1, nrow = 2, heights = c(0.6, 0.35), labels = c(
  "\\textbf{(a)}", "\\textbf{(b)}"
), label.x = c(0.01, 0.01), label.y = c(0.98, 0.98), font.label = list(size = 9.5))
combined_plot
tikz(file.path(plots_path, "segmented-data.tex"),
     width = 1 * doc_width_inches, 
     height = 0.95 *  doc_width_inches)
combined_plot
dev.off()

