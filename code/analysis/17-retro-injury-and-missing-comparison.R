# -------------------------------------------------------------------------
# Quick visual check of whether retrospective injury status is linked
# to missingness.
# -------------------------------------------------------------------------
require(data.table)

retro_injury_data_rds_path <- here::here(
  "outputs",
  "data",
  "cleaned-injury-and-demographics-retrospective.RDS"
)

retro_injury_data <- readRDS(
  file = retro_injury_data_rds_path
)


# risc1_dt <- fread(file = here::here("outputs",
#                                     "data",
#                                     "risc1_intermediate_copy_01.csv"))
# screen_index <- readRDS(file = here::here("outputs",
#                                         "data",
#                                         "screen_index_intermediate_copy_01.rds"))

# Data Wrangling ----------------------------------------------------------



## Create a data set of strides being removed -----------------------------

# create a data set that counts a subject's total strides
risc1_strides_dt <- risc1_dt[,
                             .(total_strides = uniqueN(trial_id)),
                             by = subject_id]

screen_index_strides_dt <- screen_index[
  observation %in% risc1_dt[, unique(trial_id)],
  .(n_remove_strides = uniqueN(observation)),
  by = subject_id]

# Left Join two data sets
missing_strides_dt <- merge(
  x = risc1_strides_dt,
  y = screen_index_strides_dt,
  all.x = T)

# All the missing values from left join are actually 0's
missing_strides_dt[is.na(n_remove_strides),
                   n_remove_strides := 0]



## Join with retrospective injury -----------------------------------------

retro_injury_and_strides_dt <- missing_strides_dt[
  retro_injury_data,
  on = .(subject_id),
  nomatch = 0]

stopifnot(
  retro_injury_and_strides_dt[, .N] ==
  missing_strides_dt[, .N])

retro_injury_and_strides_dt[
  , prop_of_strides_removed := n_remove_strides / total_strides]




# Visualisation -----------------------------------------------------------

par(mfrow = c(3, 2))
retro_injury_and_strides_dt[, {
  hist(prop_of_strides_removed,
       main = unlist(.BY),
       breaks = seq(0, 1, by = 0.1))
  "done"},
  by = retrospective_injury_status]

# looks ok...
