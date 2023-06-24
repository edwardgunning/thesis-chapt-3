library(data.table) # CRAN v1.14.2
library(ggplot2)    # CRAN v3.3.5    
library(tikzDevice) # CRAN v0.12.3.1
library(fda)        # CRAN v5.5.1
library(ggplot2)    # CRAN v3.3.5

data_path <- here::here(
  "outputs",
  "data",
  "risc1-registered-basis-coef.rds"
)
plots_path <- here::here("outputs", "plots")
source(here::here("code",
                  "functions", 
                  "functions-helper-smoothing.R"))

theme_set(theme_bw())
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
risc_1_basis_coef_reg <- readRDS(file = data_path)

risc_1_basis_coef_knee_fle <- risc_1_basis_coef_reg[location=="Knee" & 
                                                      plane_of_motion == "fle"]

trial_ids <- risc_1_basis_coef_knee_fle$trial_id
# [1] "P_4063_L_stride28" "P_4061_R_stride50" "P_4297_L_stride57" "P_4181_L_stride42"
# [5] "P_4262_R_stride1"
# Take a random sample of functions for plot:
set.seed(2023)
(trial_ids_rand_sample <- sample(trial_ids, size = 5))

# Define basis to combine with stored coefficients
bspl80 <- fda::create.bspline.basis(
  rangeval = c(0, 100),
  nbasis = 80,
  norder = 4)

fd_object <- risc_1_basis_coef_knee_fle[
  trial_id %in% trial_ids_rand_sample,
  {
    fd(coef_to_mat(.SD), bspl80)
  },
  .SDcols = paste0("coef_", 1:80)]

fd_eval_mat <- eval.fd(evalarg = 0:100, fdobj = fd_object)

fd_dt <- data.table(t = 0:100,fd_eval_mat)
fd_dt_lng <- melt.data.table(data = fd_dt, 
                             id.vars = "t",
                             variable.name = "obs", 
                             variable.factor = FALSE,
                             value.name = "angle",
                             value.factor = FALSE,
                             measure.vars = paste0("fun_rep", 1:5))

peak_time_dt <- data.table(
  obs = paste0("fun_rep", 1:5),
  t = risc_1_basis_coef_knee_fle[trial_id %in% trial_ids_rand_sample, peak_kfa_time]
)

peak_time_dt$angle <- eval.fd(evalarg = matrix(peak_time_dt$t, nrow =1, ncol = 5),
                              fdobj = fd_object)[1, ]


peak_kfa_plot <- ggplot(fd_dt_lng) +
  aes(x = t, y = angle, group = obs, colour = obs) +
  geom_vline(xintercept = mean(risc_1_basis_coef_reg$peak_kfa_time), lty = 2, col = "darkgrey") +
  geom_line() +
  geom_point(data = peak_time_dt, size = 1.25) +
  theme(legend.position = "none")+
  labs(y ="Angle ($^{\\circ}$)", x = "Normalised Time ($\\%$ of Stride)")

tikz(file.path(plots_path, "peak-kfa-plot.tex"),
     width = 0.45 * doc_width_inches, 
     height = 0.4 *  doc_width_inches,
     standAlone = TRUE)
peak_kfa_plot
dev.off()

tinytex::lualatex(file.path(plots_path, "peak-kfa-plot.tex"))

