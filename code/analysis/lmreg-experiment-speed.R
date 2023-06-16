# ------------------------------------------------------------------------#
# Implement landmark registration on the risc1 data set.
# ------------------------------------------------------------------------#

# Packages and functions --------------------------------------------------
library(data.table)    # CRAN v1.14.0
library(fda)           # CRAN v5.5.1
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

source(file = here::here(
  "code",
  "functions",
  "landmark-reg-updated.R"
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


N_sim <- 50
M <- c(1, seq(50, 250, by = 50))
time_df <- expand.grid(sim_rep = seq_len(N_sim), M = M) 
time_df$time_1 <- time_df$time_2  <- NA
N_sim_total <- nrow(time_df)
simulation_seeds <- vector("list", length = nrow(time_df))
set.seed(1)
#basis for lmreg:
W_basis_simple <- fda::create.bspline.basis(rangeval = c(0, 100),
                                             nbasis = 4,
                                             norder = 3,
                                             breaks = c(0, overall_peak_kfa_mean, 100))
W_fdpar_simple <- fdPar(fdobj = W_basis_simple, Lfdobj = 1, lambda = 10^-12)

for(i in seq_len(N_sim_total)) {
  print(paste("Iteration", i, "of", N_sim_total))
  simulation_seeds[[i]] <- .Random.seed
  sample_trials <- sample(x = risc1_basis_coef$trial_id, size = time_df[i, "M"])
  sample_dt <- risc1_basis_coef[trial_id %in% sample_trials]
  # use custom functions to do the registration;
  time_df[i, "time_1"] <- system.time(
    sample_dt[,
              paste0("lm_coef_", 1:80) := {
                       
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
  )["elapsed"]
  
  
  time_df[i, "time_2"] <- system.time(
    sample_dt[,  paste0("lm_coef_", 1:80) := {
      
                       # create an fd object from stored coefficients
      fd_obj <- fda::fd(coef = as.matrix(t(.SD)),
                        basisobj = bspl80)
      
      
      coef <- landmarkreg(fdobj = fd_obj,
                  ximarks = peak_kfa_time,
                  x0marks =  overall_peak_kfa_mean, 
                  WfdPar = W_fdpar_simple)$regfd$coefs
      get_list_of_rows(coef)
      
    },
    .SDcols = paste0("coef_", 1:80)])["elapsed"]
  }


saveRDS(object = list(session_info = sessionInfo(),
                      simulation_seeds = simulation_seeds,
                      time_df = time_df),
        file = here::here("outputs", "data", "lm-reg-speed-simulation-settings.rds"))

