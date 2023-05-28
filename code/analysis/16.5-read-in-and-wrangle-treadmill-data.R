library(data.table) # CRAN v1.14.2 

treadmill_and_dominance_data_path <- here::here(
    "data",
    "treadmill-and-leg-dominance.xlsx"
  )
  
  # Get the column names from the data as is

col_types <- rep("text", 3)
col_names <- c("subject_id", "dominance", "treadmill")
na_vec <- c("999")
treadmill_and_dominance_data <- readxl::read_xlsx(
  path = treadmill_and_dominance_data_path,
  col_types = col_types,
  col_names = col_names,
  na = na_vec,
  skip = 2)


treadmill_and_dominance_dt <- as.data.table(treadmill_and_dominance_data)


# Do all IDs have the form P_ 4 digits?
# (check what % of IDs we have in correct format)
treadmill_and_dominance_dt[,
                  paste(100 * mean(stringr::str_detect(subject_id, "P_\\d{4}")),
                        "%")]
# no.

# How many do not?
treadmill_and_dominance_dt[, sum(!stringr::str_detect(subject_id, "P_\\d{4}"))]
# 4

# Look at these:
treadmill_and_dominance_dt[!stringr::str_detect(subject_id, "P_\\d{4}")]

# They are all p_ 4 digits instead of P_?
treadmill_and_dominance_dt[!stringr::str_detect(subject_id, "P_\\d{4}"),
                  all(stringr::str_detect(subject_id, "p_\\d{4}"))]
# yes.

# For these rows, replace p with P:
# using look ahead assertion ('?=') to be safe.
treadmill_and_dominance_dt[!stringr::str_detect(subject_id, "P_\\d{4}"),
                  subject_id := stringr::str_replace(
                    subject_id, "p(?=_\\d{4})", replacement = "P")]

treadmill_and_dominance_dt[, dominance := factor(dominance,
  levels = c("R", "L"), labels = c("right", "left")
)]

treadmill_and_dominance_dt[, treadmill := factor(
  treadmill,
  levels = c("1", "2"), labels = c("old", "new")
)]





# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
treadmill_path_to_write <- here::here(
  "data",
  "cleaned-treadmill-and-leg-dominance.csv"
)

fwrite(treadmill_and_dominance_dt,
       file = treadmill_path_to_write)


# write to rds ------------------------------------------------------------

treadmill_path_for_rds <- here::here(
  "outputs",
  "data",
  "cleaned-treadmill-and-leg-dominance.RDS"
)

saveRDS(object = treadmill_and_dominance_dt,
        file = treadmill_path_for_rds)


