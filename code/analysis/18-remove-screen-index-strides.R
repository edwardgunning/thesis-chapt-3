# Make table to compare subjects ------------------------------------------
# in the risc1 data, screen index and injury data:
# good to do before we remove based on screen index...
risc1_dt_binary <- risc1_dt[, .(subject_id = unique(subject_id))]
risc1_dt_binary[, risc1 := TRUE]

screen_index_binary <- screen_index[, .(subject_id = unique(subject_id))]
screen_index_binary[, screen_index := TRUE]

cross_check_dt <- merge.data.table(x = risc1_dt_binary,
                                   y = screen_index_binary,
                                   by = "subject_id",
                                   all = TRUE)

# read in injury data:
retro_injury_data_rds_path <- here::here(
  "outputs",
  "data",
  "cleaned-injury-and-demographics-retrospective.RDS"
)

retro_injury_data <- readRDS(
  file = retro_injury_data_rds_path
)

retro_injury_binary <- retro_injury_data[, .(subject_id = unique(subject_id))]
retro_injury_binary[, retro_injury := TRUE]

cross_check_dt <- merge.data.table(
  x = cross_check_dt,
  y = retro_injury_binary, 
  by = "subject_id", 
  all = TRUE)


cross_check_dt[is.na(risc1), risc1 := FALSE]
cross_check_dt[is.na(screen_index), screen_index := FALSE]
cross_check_dt[is.na(retro_injury), retro_injury := FALSE]

# now, some subjects are just in the risc1, actually with no information
# so need
retro_injury_binary_2 <- retro_injury_data[
  , .(all_info_missing = all(unlist(lapply(.SD, is.na)))), by = .(subject_id)]

retro_injury_binary_2[, retro_injury_non_na := !all_info_missing]
retro_injury_binary_2[, all_info_missing := NULL]

cross_check_dt <- merge.data.table(
  x = cross_check_dt,
  y = retro_injury_binary_2, 
  by = "subject_id", 
  all = TRUE)
# this shouldn't have added any new participants that weren't there,
# but check..
stopifnot(all(!is.na(cross_check_dt)))

# and save this for use later when discussing with Shane...
cross_check_dt_path <- here::here("outputs", "tables", "cross-check-subjects-list.csv")
fwrite(cross_check_dt, file = cross_check_dt_path)

# also write the subjects we need to ask Shane about into a csv
# we know P_4066 has a problem with labelling...
cross_check_interest_dt_path <- here::here("data", "cross-check-subjects-of-interest.csv")
fwrite(cross_check_dt[retro_injury_non_na == TRUE & risc1 == FALSE &
                        !(subject_id %in% c("P_4066", "P_4060", "P_4210", "P_4234"))], 
       file = cross_check_interest_dt_path)

# Remove participant completely from risc1 if they have > 70% miss --------

# From Shane and Kieran:

# "In terms of a threshold of stride missingness,
# his thoughts were to remove the 70%+ participants."


strides_dt_risc1 <- risc1_dt[,
                             .(total_strides = uniqueN(trial_id)),
                             by = subject_id]

strides_dt_screen_index <- screen_index[,
                                        .(remove_strides = uniqueN(observation)),
                                        by = subject_id]

strides_remove_dt <- merge(x = strides_dt_risc1,
      y = strides_dt_screen_index,
      all.x = TRUE,
      all.y = FALSE,
      by = "subject_id")

strides_remove_dt[is.na(remove_strides),
                  remove_strides := 0]

strides_remove_dt[, pc_remove := 100 * (remove_strides/ total_strides)]

# Find subjects with > 70% of strides to remove:
subject_ids_70pc <- strides_remove_dt[pc_remove > 70,
                  subject_id]
subject_ids_70pc
# [1] P_4153 P_4159

# How many rows from risc1 data is this?
risc1_dt[subject_id %in% subject_ids_70pc,
         .N]
# [1] 11664

(n_removed_70pc_subjects <- risc1_dt[subject_id %in% subject_ids_70pc,
                                    uniqueN(interaction(stride_num, subject_id))])
# [1] 162


# ok... drop these two participants.
risc1_dt <- risc1_dt[!(subject_id %in% subject_ids_70pc)]

risc1_dt[, .N]






# Otherwise, remove a stride if it is in Screen Index --------------------

# How many strides will be removed?
(n_removed_screen_index <- risc1_dt[trial_id %in% screen_index[, unique(observation)],
         uniqueN(trial_id)])


# Show what anti-join does
# identical(risc1_dt[!screen_index, on = .(trial_id = observation)],
#           risc1_dt[!(trial_id %in% screen_index[, unique(observation)])])


# check that the number of strides being removed 
stopifnot(
(risc1_dt[, .N] - risc1_dt[!screen_index,
         on = .(trial_id = observation), .N])/ 36 ==
  risc1_dt[trial_id %in% screen_index[, unique(observation)],
           uniqueN(trial_id)])
  

risc1_dt <- risc1_dt[!screen_index,
                     on = .(trial_id = observation)]


# Save an intermediate copy of the data:
fwrite(risc1_dt, file = here::here("outputs",
                                   "data",
                                   "risc1_intermediate_copy_02.csv"))


