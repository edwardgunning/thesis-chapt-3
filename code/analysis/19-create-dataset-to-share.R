# -------------------------------------------------------------------------
# Create version of risc1 data set for exploratory analysis
# -------------------------------------------------------------------------

# Take subset
# Angles - hip, knee, ankle
risc1_dt_subset <- risc1_dt[quantity == "Angles" &
                              location %in% c("Hip",
                                              "Knee",
                                              "Ankle")]
# How large is it?
risc1_dt_subset[, .N]
# [1] 442071


# Do smoothing ------------------------------------------------------------

## Get custom smoothing functions -----------------------------------------
source(here::here("code",
                  "functions",
                  "functions-helper-smoothing.R"))
source(here::here("code",
                  "functions",
                  "function-smooth-from-dt.R"))


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

# Now there are only a small number of unique stride lengths.
# Therefore, we can do our least squares in "blocks".
# Each block is all strides of the same length.
# Gives a speed up of x20 compared with using each trial as a block,
# which I had been using previously:
risc1_dt_subset[, paste("coef", 1:80, sep = "_") := {
  # Smooth the data with basis expansion, return coefs
  curve_length <- stride_length_frames
  raw_dt <- .SD[, 1:curve_length]
  smooth_from_dt(raw_dt = raw_dt,
                 supplied_basis = bspl80,
                 lambda = 0)
  },
  .SDcols = `data_0`:`data_197`,
  by = stride_length_frames]

# remove raw data observations and variables we don't need
# don't remove stride length (frames) because it may be useful...
risc1_dt_subset[, c("quantity",
                    "condition4",
                    paste("data", 0:197, sep = "_")) := NULL]

# Combine with injury data ------------------------------------------------

# read in injury data:
retro_injury_data_rds_path <- here::here(
  "outputs",
  "data",
  "cleaned-injury-and-demographics-retrospective.rds"
)


retro_injury_data <- readRDS(
  file = retro_injury_data_rds_path
)

# join injury data to functional data:
# we do not take subjects who are not in the risc1 data but in the  
# retro injury data set, thus set all.x = TRUE, but all.y = FALSE
risc1_subset_with_injury <- merge.data.table(x = risc1_dt_subset,
      y = retro_injury_data,
      all.x = TRUE,
      all.y = FALSE,
      by = "subject_id")

stopifnot(risc1_subset_with_injury[, .N] == risc1_dt_subset[, .N])



# Re-order data columns: --------------------------------------------------
col_order_vec <- union(c("trial_id", 
                   "subject_id",
                   "side", 
                   "stride_num",
                   "location",
                   "plane_of_motion",
                   "self_selected_speed_kmph",
                   "stride_length_frames",
                   "long_time",
                   "stride_length_frames",
                   "dominance",
                   "treadmill"),
                   colnames(retro_injury_data))

col_order_vec <- c(col_order_vec,
                   paste("coef", 1:80, sep = "_"))

setcolorder(x = risc1_subset_with_injury,
            neworder = col_order_vec)




# Make a discretised version ----------------------------------------------
# so that instead of basis coefficients, it is function values
# on a grid
risc1_basis_coef <- copy(risc1_subset_with_injury)
risc1_discrete <- copy(risc1_subset_with_injury)

risc1_discrete[, paste("time", 0:100, sep = "_") :=
                 {
                   coef_matrix <- as.matrix(t(.SD))
                   fd_obj <- fda::fd(coef = coef_matrix,
                                     basisobj = bspl80)
                   eval_matrix <- fda::eval.fd(evalarg = 0:100,
                                               fdobj = fd_obj)
                   get_list_of_rows(eval_matrix)
                 },
               .SDcols = paste("coef", 1:80, sep = "_")]

risc1_discrete[, paste("coef", 1:80, sep = "_") := NULL]

# View(risc1_discrete)
# View(risc1_basis_coef)



# Save data sets ----------------------------------------------------------


## For sharing (write to google drive) ------------------------------------

# Save both versions to both RDS and csv formats:

# In sharing google drive folder:
sharing_data_folder <- here::here("outputs", "data")

stopifnot(file.exists(sharing_data_folder))

fwrite(risc1_discrete,
       file = file.path(
         sharing_data_folder,
         "discrete.csv"
       ))

fwrite(risc1_basis_coef,
       file = file.path(
         sharing_data_folder,
         "basis-coef.csv"
       ))




## And for my own use (on machine) ----------------------------------------
saveRDS(object = risc1_basis_coef, 
        file = here::here(
          sharing_data_folder,
          "risc1-cleaned-basis-coef.rds"))
