# -------------------------------------------------------------------------
# This script contains the smoothing experiment for speed contained 
# in Chapter 3.3.3 of the Thesis.
# -------------------------------------------------------------------------

# Get latest version of risc1 data set
library(data.table) # CRAN v1.14.2
library(ggplot2)
library(tikzDevice)
## Get custom smoothing functions -----------------------------------------
source(here::here("code",
                  "functions",
                  "functions-helper-smoothing.R"))
source(here::here("code",
                  "functions",
                  "function-smooth-from-dt.R"))
source(here::here("code",
                  "functions",
                  "theme_gunning.R"))

theme_gunning()
# -------------------------------------------------------------------------
plots_path <- here::here("outputs", "plots")

# -------------------------------------------------------------------------
doc_width_cm <- 16
doc_width_inches <- doc_width_cm *  0.3937
options(scipen = 999)


risc1_dt <- fread(here::here("outputs", "data", "risc1_intermediate_copy_02.csv"))
# Take subset
# Angles - hip, knee, ankle
risc1_dt_subset <- risc1_dt[quantity == "Angles"]

# How large is it?
risc1_dt_subset[, .N]
risc1_dt_subset[, uniqueN(trial_id)]



                    # [1] 442071

## Set up basis -----------------------------------------------------------

bspl80 <- fda::create.bspline.basis(
  rangeval = c(0, 100),
  nbasis = 80,
  norder = 4)


## Do smoothing + time normalisation in one step --------------------------


# Count non na values in first row... i.e. get stride length
risc1_dt_subset[, stride_length_frames := count_non_na_values(.SD[1, ]),
                .SDcols = `data_0`:`data_197`,
                by = trial_id]



N_sim <- 50
M <- 10^seq(0, 4)
time_df <- expand.grid(sim_rep = seq_len(50), M = M) 
time_df$time_1 <- time_df$time_2 <- NA
simulation_seeds <- vector("list", length = nrow(time_df))
for(i in seq_len(nrow(time_df))) {
  print(paste0("Iteration", i))
  simulation_seeds[[i]] <- .Random.seed
  sample_dataset <- risc1_dt_subset[trial_id %in% sample(unique(trial_id), size = time_df[i, "M"], replace = FALSE)]
  # Experiment:
  dt_1 <- copy(sample_dataset)
  dt_2 <- copy(sample_dataset)
  
  stopifnot(identical(dt_1, dt_2))
  
  stopifnot(sample_dataset[, uniqueN(trial_id)] == time_df[i, "M"])
  time_df[i, "time_1"] <- system.time(dt_1[, 
                                        paste("coef", 1:80, sep = "_") := {
                                          # Smooth the data with basis expansion, return coefs
                                          curve_length <- stride_length_frames[1]
                                          raw_dt <- .SD[, 1:curve_length]
                                          smooth_from_dt(raw_dt = raw_dt,
                                                         supplied_basis = bspl80,
                                                         lambda = 0)},
                                        .SDcols = `data_0`:`data_197`,
                                        by = trial_id])["elapsed"]
  
  time_df[i, "time_2"] <- system.time(dt_2[, 
                                           paste("coef", 1:80, sep = "_") := {
                                             # Smooth the data with basis expansion, return coefs
                                             curve_length <- stride_length_frames
                                             raw_dt <- .SD[, 1:curve_length]
                                             smooth_from_dt(raw_dt = raw_dt,
                                                            supplied_basis = bspl80,
                                                            lambda = 0)},
                                           .SDcols = `data_0`:`data_197`,
                                           by = stride_length_frames])["elapsed"]
}

library(ggplot2)    # CRAN v3.4.0 
options(scipen = 999)


time_df_lng <- melt.data.table(data.table(time_df), id.vars = c("M", "sim_rep"), measure.vars = c("time_1", "time_2"))
time_df_lng[, minute := value / 60]
time_df_lng[, variable := factor(variable,
                                 levels = c("time_1", "time_2"),
                                 labels = c("By Stride", "By Block"))]
smoothing_plot <- ggplot(data = time_df_lng) +
  aes(x = M, colour = variable, group = interaction(M, variable)) +
  geom_boxplot(aes(y = minute), outlier.size = 0.5) +
  scale_y_log10(breaks = c(
    0.0001,
    0.001,
    0.01,
    0.1,
    1
  ), labels = paste(c(
    0.0001,
    0.001,
    0.01,
    0.1,
    1
  ))) +
  scale_x_log10(breaks = c(
    1,
    10,
    100,
    1000,
    10000
  ), labels = paste(c(
    1,
    10,
    100,
    1000,
    10000
  ))) +
  labs(y = "Time (minutes)", x = "Number of Strides in Dataset", color = "Computation:") +
  theme(legend.position = "bottom")


tikz(file.path(plots_path, "smoothing-plot.tex"),
                       width = 0.55 * doc_width_inches, 
                       height = 0.575 *  doc_width_inches)
smoothing_plot
dev.off()

saveRDS(object = list(session_info = sessionInfo(),
                      simulation_seeds = simulation_seeds,
                      time_df),
        file = here::here("outputs", "data", "speed-simulation-settings.rds"))

