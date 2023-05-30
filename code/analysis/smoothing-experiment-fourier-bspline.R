# Source necessary files --------------------------------------------------
library(data.table) # CRAN v1.14.2
library(ggplot2)    # CRAN v3.4.0
library(tikzDevice) # CRAN v0.12.3.1
source(file = here::here("code",
                         "functions",
                         "functions-helper-smoothing.R"))
source(file = here::here("code",
                         "functions",
                         "functions-get-mse.R"))
source(here::here("code", "functions", "theme_gunning.R"))
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

# Import Data: ------------------------------------------------------------
risc1_dt <- fread(file = here::here("outputs",
                                    "data",
                                    "risc1_intermediate_copy_01.csv"))

screen_index <- readRDS(file = here::here("outputs",
                                          "data",
                                          "screen_index_intermediate_copy_01.rds"))





# Set random seed ---------------------------------------------------------
set.seed(1996)


# Generate a data set of unique strides to sample from --------------------
strides_subject_dt <- risc1_dt[
  !screen_index, # (make sure these strides are not in screen index by 
  on = .(trial_id = observation), # using an anti-join)
  .(subject_id, trial_id)]



# Sample a single stride for each subject ---------------------------------
strides_sample_dt <- strides_subject_dt[,
                                        .(chosen_stride = trial_id[sample(1:.N, size = 1)]),
                                        by = subject_id]


# From this data set, again just sample a small number of subjects --------
sample_dt <- risc1_dt[strides_sample_dt[sample(1:.N, size = .N)],
                      on = .(subject_id, trial_id = chosen_stride),
                      nomatch = 0]


# Smooth this data sample using different number of basis functions -------
# and calculate MSEs 

# first, count number of non-na data values (i.e. curve length)
# (can do this in blocks of strides using by argument
# as all curves from same stride will be same length - speed up!)
sample_dt[, non_na_vals := count_non_na_values(.SD[1, ]),
          .SDcols = data_0:data_197,
          by = .(subject_id,
                 side,
                 stride_num)]

sample_dt[,
          paste0("bspline_nbasis",
                 seq(from = 51, to = 111, by = 10)) :=
            get_smoothing_mses_from_dt(
              sub_dt = .SD[, 1:non_na_vals[1]],
              n_basis_from = 51,
              n_basis_to = 111,
              n_basis_by = 10),
          .SDcols = data_0:data_197,
          by = .(subject_id,
                 side,
                 stride_num,
                 non_na_vals)]

sample_dt[,
          paste0("fourier_nbasis",
                 seq(from = 51, to = 111, by = 10)) :=
            get_smoothing_mses_fourier_from_dt(
              sub_dt = .SD[, 1:non_na_vals[1]],
              n_basis_from = 51,
              n_basis_to = 111,
              n_basis_by = 10),
          .SDcols = data_0:data_197,
          by = .(subject_id,
                 side,
                 stride_num,
                 non_na_vals)]



# create vector containing names of columns to to work with 
col_names_select <- c("subject_id",
                      "side",
                      "stride_num",
                      "plane_of_motion",
                      "quantity",
                      "location",
                      "non_na_vals",
                      paste0("bspline_nbasis",
                             seq(from = 51, to = 111, by = 10)),
                      paste0("fourier_nbasis",
                             seq(from = 51, to = 111, by = 10)))

plot_mses_fourier_bspline_dt <- melt(data = sample_dt[, ..col_names_select],
                                     id.vars = c("subject_id",
                                                 "side",
                                                 "stride_num",
                                                 "plane_of_motion",
                                                 "quantity",
                                                 "location",
                                                 "non_na_vals"), 
                                     variable.name = "type_nbasis",
                                     value.name = "mse", 
                                     variable.factor = FALSE)

plot_mses_fourier_bspline_dt[, `:=`(basis_type = stringr::str_extract(type_nbasis, "(fourier|bspline)(?=_nbasis\\d{1,3})"),
                                    nbasis = as.numeric(stringr::str_extract(type_nbasis, "(?<=(fourier|bspline)_nbasis)\\d{1,3}")))
]

plot_mses_fourier_bspline_dt[, type_nbasis := NULL]
plot_mses_fourier_bspline_dt[, rmse := sqrt(mse)]
plot_mses_fourier_bspline_dt[, mse := NULL]
plot_mses_fourier_bspline_dt[, basis_type := factor(basis_type,
                                                    levels = c("bspline", "fourier"),
                                                    labels = c("B-Spline", "Fourier"))]
setorder(plot_mses_fourier_bspline_dt, - basis_type)
plot_mses_fourier_bspline_dt[,plane_of_motion := factor(plane_of_motion,
                                                              levels = c("fle", "abd", "rot"),
                                                              labels = c("\\emph{flexion}", "\\emph{abduction}", "\\emph{rotation}"))]



options(scipen = 999)
boxplots <- ggplot(data = plot_mses_fourier_bspline_dt[quantity == "Angles" & location %in% c("Hip", "Knee", "Ankle")]) +
  aes(x = nbasis, y = rmse, group = interaction(basis_type, nbasis),
      color = basis_type) +
  facet_wrap(factor(location, levels = c("Hip", "Knee", "Ankle")) ~ plane_of_motion, ncol = 3, scales = "free_y") +
  labs(y = "Root Mean Squared Error (RMSE)",
       x = "Number of Basis Functions ($K$)") +
  geom_boxplot(outlier.size = 0.1) +
  scale_y_log10() +
  scale_color_hue(name = "Basis Type:", h = c(180, 300)) +
  scale_x_continuous(breaks = seq(51, 111, by = 10)) +
  theme(legend.position = "bottom",
        legend.title = element_text(vjust = 0.5))

boxplots
tikz(file.path(plots_path, "fourier-vs-bspline-boxplots.tex"),
     width = 0.8 * doc_width_inches, 
     height = 0.9 *  doc_width_inches)
boxplots
dev.off()



# Quick demo of where approximation error comes from:
library(fda)        # CRAN v5.5.1
y <- unlist(sample_dt[location=="Hip" & quantity=="Angles" & plane_of_motion == "rot"][1, data_0:data_133])
t_grid <- seq(0, 100, length.out = length(y))
plot(t_grid, y, type = "l")
fourier_basis <- create.fourier.basis(rangeval = c(0, 100), nbasis = 55)
bspline_basis <- create.bspline.basis(rangeval = c(0, 100), nbasis = 55, norder = 4)
smooth_fourier <- smooth.basis(argvals = t_grid, y = y, fdParobj = fdPar(fdobj = fourier_basis, Lfdobj = 0, lambda = 0))
smooth_bspline <- smooth.basis(argvals = t_grid, y = y, fdParobj = fdPar(fdobj = bspline_basis, Lfdobj = 0, lambda = 0))
plot(smooth_fourier$fd - smooth_bspline$fd)
plot(eval.fd(t_grid, smooth_fourier$fd)[,1] - y, type = "l")
lines(eval.fd(t_grid, smooth_bspline$fd)[,1] - y, col = 2)
# Especially at endpoints...


