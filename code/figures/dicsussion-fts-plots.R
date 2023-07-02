library(data.table) # CRAN v1.14.2
library(ggplot2)    # CRAN v3.4.0
library(fda)        # CRAN v5.5.1
library(tikzDevice) # CRAN v0.12.3.1
library(ggpubr)
source(here::here("code/functions/theme_gunning.R"))
source(here::here("code/functions/functions-helper-smoothing.R"))
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


basis_coef_subset <- basis_coef_current[subject_id == "P_4211" & side == "left" & location == "Knee" & plane_of_motion == "fle"]

bspl80 <- fda::create.bspline.basis(
  rangeval = c(0, 100),
  nbasis = 80,
  norder = 4)

basis_coef_subset[, paste0("eval_", 0:100) := {
  coef_mat <- coef_to_mat(.SD)
  fd_obj <- fd(coef = coef_mat, basisobj = bspl80)
  get_list_of_rows(eval.fd(0:100, fdobj = fd_obj))
}, .SDcols = paste0("lm_coef_", 1:80)]


basis_coef_subset <- basis_coef_subset[, c("stride_num", paste0("eval_", 0:100))]
basis_coef_subset_lng <- melt.data.table(data = basis_coef_subset, 
                                         id.vars = "stride_num",
                                         measure.vars = c(paste0("eval_", 0:100)),
                                         variable.name = "t",
                                         value.name = "angle",
                                         variable.factor = FALSE,
                                         value.factor = FALSE)
basis_coef_subset_lng[, t := as.numeric(stringr::str_remove(t, "eval_"))]

rainbow_plot <- ggplot(data = basis_coef_subset_lng) +
  aes(x = t, y = angle, group = stride_num, colour = stride_num) +
  geom_line() +
  scale_color_gradientn(colours = rainbow(10)) +
  labs(colour = "Stride Number:", 
       y ="Angle ($^{\\circ}$)",
       x = "Normalised Time ($\\%$ of Stride)",
       title = "Rainbow Plot") +
  scale_x_continuous(expand = c(0.02,0.02)) +
  theme(legend.position = "bottom")



image_plot <- ggplot(data = basis_coef_subset_lng) +
  aes(x = t, y = stride_num, fill = angle) +
  geom_tile() +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_fill_viridis_c(option = "A", direction = -1, breaks = c(0, 25, 50, 75, 100),
                       labels = c(0, 25, 50, 75, 100)) +
  theme(panel.border = element_rect(colour = "lightgrey"),
        axis.ticks = element_line(colour = "lightgrey")) +
  labs(y = "Stride Number", 
       fill ="Angle ($^{\\circ}$):",
       x = "Normalised Time ($\\%$ of Stride)",
       title = "Image Plot") +
  theme(legend.position = "bottom")
  
ggarrange(rainbow_plot, image_plot)

tikz(file.path(plots_path, "fts-plots.tex"),
     width = 1.2 * doc_width_inches, 
     height = 0.675 * doc_width_inches,
     standAlone = TRUE)
ggarrange(rainbow_plot, image_plot)
dev.off()

tinytex::lualatex(file.path(plots_path, "fts-plots.tex"))
