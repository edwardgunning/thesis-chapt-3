# ------------------------------------------------------------------------#
# Purpose:
# Script which runs the flipped curves detection on the knee abduction
# angles.

# Uses flipped L-2 distance (ccustom functions I wrote) to detect the
# flipped curves.
# ------------------------------------------------------------------------#



# Get helper functions ----------------------------------------------------

source(here::here("code",
                  "full-data",
                  "functions",
                  "functions-helper-smoothing.R"))
source(here::here("code",
                  "full-data",
                  "functions",
                  "function-smooth-from-dt.R"))
source(here::here("code",
                  "full-data",
                  "functions",
                  "functions-flipped-curves.R"))


# Create subset of data to check ------------------------------------------

sample_to_check <- risc1_dt[quantity == "Angles" &
    location == "Knee" &
    plane_of_motion == "abd"][
  !screen_index,
  on = .(trial_id = observation)
  ]


# Transform this data set to functions ------------------------------------

bspl80 <- fda::create.bspline.basis(
  rangeval = c(0, 100),
  nbasis = 80,
  norder = 4)

sample_to_check[, non_na_vals := apply(.SD, MARGIN = 1, count_non_na_values),
                .SDcols = `data_0`:`data_197`]

system.time(
  sample_to_check[,
                  paste0("coef", 1:80) := smooth_from_dt(
                    raw_dt = .SD[, 1:non_na_vals],
                    supplied_basis = bspl80,
                    lambda = 0),
                  .SDcols = `data_0`:`data_197`,
                  by = seq_len(length.out = nrow(sample_to_check))])



sample_to_check[, paste0("data_", 0:197) := NULL]




# Apply algorithm to check if flipped -------------------------------------

# Ignore P_4092

check_flipped_dt <- sample_to_check[
  subject_id != "P_4092",
  .(detect = compute_flipped_L2_from_dt(.SD, basis_obj = bspl80)[["detect"]]),
  .SDcols = c("side", paste0("coef", 1:80)),
  by = .(subject_id)]
# Could also compute "how flipped" they are.

# flipped_size_dt <- sample_to_check[subject_id %in% check_flipped_dt[detect == "yes", subject_id],
#                                     lapply(compute_flipped_L2_from_dt(.SD, basis_obj = bspl80)[["result"]],
#                                            round, 2),
#                                     .SDcols = c("side", paste0("coef", 1:80)),
#                                     by = .(subject_id)][order(L2/flipped_L2)]

# make plot from original data:
flagged_subset_of_risc1_dt <- risc1_dt[
  subject_id %in% check_flipped_dt[detect == "yes", subject_id] &
    quantity == "Angles" &
    location == "Knee" & plane_of_motion == "abd"]

flagged_rds_path <- here::here(
  "outputs",
  "full-data",
  "RDS-objects",
  "flagged_subset_of_risc1_dt.RDS"
)

saveRDS(object = flagged_subset_of_risc1_dt, file = flagged_rds_path)


flagged_subset_of_risc1_dt[,
         matplot(x = 5 * (0:197),
                 y = as.matrix(t(.SD)),
                 type = "l",
                 xlab = "time (milliseconds)",
                 ylab = "Angle (deg)",
                 col = as.numeric(side),
                 main = paste(subject_id, side)),
         .SDcols = `data_0`:`data_197`,
         by = .(subject_id, side)]


# Extra - Find Change Point -----------------------------------------------
# identify where curve shape changes for flipped P_4243.
special_case <- flagged_subset_of_risc1_dt[
  subject_id == "P_4243" & side == "left"]

special_case[, matplot(x = 5 * (0:197),
                      y = as.matrix(t(.SD)),
                      type = "n",
                      xlab = "time (milliseconds)",
                      ylab = "Angle (deg)",
                      col = as.numeric(side)),
             .SDcols = `data_0`:`data_197`]

# par(ask = T)
# for (i in seq_len(nrow(special_case))) {
#   print(special_case[i, stride_num])
#   special_case[i, plot(x = 5 * (0:197),
#                         y = as.matrix(t(.SD)),
#                         type = "l", add = T),
#                .SDcols = `data_0`:`data_197`]
#   }
# 
# par(ask = F)
