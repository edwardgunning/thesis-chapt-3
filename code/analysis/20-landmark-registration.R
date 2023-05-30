# ------------------------------------------------------------------------#
# Implement landmark registration on the risc1 data set.
# ------------------------------------------------------------------------#

# Packages and functions --------------------------------------------------
library(data.table) # CRAN v1.14.0

# Helper functions for landmark registration:
source(file = here::here(
  "code",
  "functions",
  "functions-helper-landmark-registration.R"
))

# Custom functions for landmark registration:
source(file = here::here(
  "code",
  "functions",
  "functions-landmark-registration.R"
))

# Functions to turn columns/ rows of matrix into elements of a list
source(file = here::here(
  "code",
  "functions",
  "functions-helper-smoothing.R"
))

# Data --------------------------------------------------------------------

# path to data:
data_path <- here::here(
  "outputs",
  "data",
  "risc1-cleaned-basis-coef.rds"
  )
# read it in:
risc1_basis_coef <- readRDS(file = data_path)

# define basis to combine with stored coefficients
bspl80 <- fda::create.bspline.basis(
  rangeval = c(0, 100),
  nbasis = 80,
  norder = 4)

# Grid search for peak knee flexion angle ---------------------------------

# Search for peak kfa for each side and save it as a variable.
# use custom function get_peak_kfa().

# (idea: could speed this up by just searching second half of gait cycle)
risc1_basis_coef[, 
                 peak_kfa_time := {
  # Create an fda::fd objectfor each stride only for 
  # the knee flexion angle.
  coef_mat <- as.matrix(t(.SD[plane_of_motion == "fle" & location == "Knee"]))
  
  fd_obj <- fda::fd(coef = coef_mat, basisobj = bspl80)
  
  # Return the time of the peak knee flexion angle
  # 1001 evaluation points on [0, 100] means steps of 0.1
  # this will be very useful later
  get_peak_kfa(kfa_fd = fd_obj, n_eval_points = 1001)$timing
  
  },
  .SDcols = paste0("coef_", 1:80),
  by = trial_id]




# Some exploratory analysis of the timings ------------------------------

# Compute the average and the standard deviation of timings for each
# subject, then compare these between inury groups

peak_kfa_time_summary <- risc1_basis_coef[, # list to return:
                                          .(st_dev = sd(peak_kfa_time),
                                            mean = mean(peak_kfa_time)),
                                          by = .(subject_id, 
                                                 retrospective_injury_status,
                                                 prospectively_injured_12_mths,
                                                 injured_knee_1_yr)]

# Check what these look like
par(mfrow = c(2, 1))
boxplot(data = peak_kfa_time_summary,
        mean ~ prospectively_injured_12_mths)

boxplot(data = peak_kfa_time_summary,
        st_dev ~ prospectively_injured_12_mths)

boxplot(data = peak_kfa_time_summary,
        mean ~ retrospective_injury_status)

boxplot(data = peak_kfa_time_summary,
        st_dev ~ retrospective_injury_status)

# no clear patterns..



# Landmark registration ---------------------------------------------------

# The fda::lanmdarkreg() function computes a warping function for each
# replicate function, and registration involves smoothing each function
# separately. This is obviously inefficient for euch a large data set,
# especially when sets of 9 curves share the same landmark. Furthermore,
# because of the grid search we have done on step sizes of 0.1, there only
# a limited number of unique peak timings. 

# Number of unique timings in the data
risc1_basis_coef[, .N]
risc1_basis_coef[, peak_kfa_time := round(peak_kfa_time, 1)]
risc1_basis_coef[, uniqueN(peak_kfa_time)]
# only 137 unique landmark timings...

# Therefore, we can register blocks. For each unique 
# landmark timing, we can create just warping function
# and perform the registration smoothing part for all
# curves with that landmark. this can be done very easily using
# the landmark time...

# need overall mean (target) to be computed first
(overall_peak_kfa_mean <- risc1_basis_coef[, mean(peak_kfa_time)])

# use custom functions to do the registration;
system.time(
  risc1_basis_coef[,  paste0("lm_coef_", 1:80) := {
    
    # create an fd object from stored coefficients
    fd_obj <- fda::fd(coef = as.matrix(t(.SD)),
                 basisobj = bspl80)
    
    # use custom block landmark registration function
    # grab coefficients of fda object.
    coef <- landmark_reg_block(fd_obj = fd_obj,
                               lower = 0,
                               upper = 100,
                               n_eval = 101,
                               lambda_y = 10^-8,
                               lambda_w = 10^-10,
                               landmark = peak_kfa_time,
                               target = overall_peak_kfa_mean)$coef
    
    # return in a list, each coefficient (1:80), as an element
    # so that they are stored as variables in the dt
    get_list_of_rows(coef)
    
  },
  .SDcols = paste0("coef_", 1:80),
  by = .(peak_kfa_time)]
  )

# user  system elapsed 
# 16.687   2.061  18.173 

save_rds_path <- here::here(
  "outputs",
  "data",
  "risc1-registered-basis-coef.rds"
)


saveRDS(object = risc1_basis_coef, file = save_rds_path)

# Exploratory plots, before and after registration ------------------------

# reg_plots_path <- here::here("outputs",
#                              "full-data",
#                              "registration-plots")
# 
# stopifnot(file.exists(reg_plots_path))

# Make and save plots to compare standard deviation curves before and 
# after registration.

# Overall

# png(filename = file.path(reg_plots_path, "overall-sd-curves.png"),
#     height = 1000, width = 1000)
par(mfrow = c(3, 3), cex = 1)
risc1_basis_coef[, {
  
  unreg <- fda::fd(coef = as.matrix(t(.SD[, paste0("coef_", 1:80)])), basisobj = bspl80)
  reg <- fda::fd(coef = as.matrix(t(.SD[, paste0("lm_coef_", 1:80)])), basisobj = bspl80)
  
  fda::plot.fd(fda::sd.fd(unreg),
               ylab = "Standard Deviation",
               xlab = "Normalised Time (% of Gait Cycle)")
  title(paste(location, plane_of_motion))
  lines(fda::sd.fd(reg), col = 2)
  abline(v = overall_peak_kfa_mean, col = "darkgrey", lty = 2)
  },
  by = .(plane_of_motion, location)]
dev.off()

plot_sds_dt <- risc1_basis_coef[, {
  unreg <- fda::fd(coef = as.matrix(t(.SD[, paste0("coef_", 1:80)])), basisobj = bspl80)
  reg <- fda::fd(coef = as.matrix(t(.SD[, paste0("lm_coef_", 1:80)])), basisobj = bspl80)
  as.list(c(fda::eval.fd(0:100, fda::sd.fd(unreg)), fda::eval.fd(0:100, fda::sd.fd(reg))))
},
by = .(plane_of_motion, location)]


names(plot_sds_dt)[-c(1:2)] <- paste0(rep(c("unreg_", "reg_"), each = 101), rep(0:100, times = 2))
plot_sds_dt_lng <- melt.data.table(data = plot_sds_dt,
                                   measure.vars = paste0(rep(c("unreg_", "reg_"), each = 101), rep(0:100, times = 2)),
                                   id.vars = c("plane_of_motion", "location"),
                                   variable.factor = FALSE, 
                                   value.name = "sd",
                                   value.factor = FALSE)
plot_sds_dt_lng[, registered := stringr::str_extract(string = variable, pattern = "(unreg|reg)")]
plot_sds_dt_lng[, t_grid := stringr::str_extract(variable, pattern = "(?<=(unreg|reg)_)\\d{1,3}")]
plot_sds_dt_lng[, variable := NULL]
plot_sds_dt_lng[, t_grid := as.numeric(t_grid)]

saveRDS(object = plot_sds_dt_lng, file = here::here("outputs", "data", "registration_plot_sds_dt_lng.rds"))


png(filename = file.path("outputs", "plots", "registration-effect-on-overall-mean-curves.png"),
    height = 1000, width = 1000)
par(mfrow = c(3, 3), cex = 1.25)
risc1_basis_coef[, {
  
  unreg <- fda::fd(coef = as.matrix(t(.SD[, paste0("coef_", 1:80)])), basisobj = bspl80)
  reg <- fda::fd(coef = as.matrix(t(.SD[, paste0("lm_coef_", 1:80)])), basisobj = bspl80)
  
  fda::plot.fd(fda::mean.fd(unreg),
               ylab = "Sample Mean Function",
               xlab = "Normalised Time (% of Gait Cycle)")
  title(paste(location, plane_of_motion))
  lines(fda::mean.fd(reg), col = 2)
  #abline(v = overall_peak_kfa_mean, col = "darkgrey", lty = 2)
},
by = .(plane_of_motion, location)]
dev.off()
