#-------------------------------------------------------------------------#
# Before we go to look at removing frame numbers from trial_id
# to create a stride stride variable we will need to check that there's a
# one to one mapping between the trial_id before and after frame number
# is removed.
#-------------------------------------------------------------------------#

# Identify the problem ----------------------------------------------------
# I have written a function to remove the frame number from the
# observation variables.
source(here::here("code",
                  "functions",
                  "function-remove-frame-num.R"))

# create new trial_id variable with frame_number removed
risc1_dt[, trial_id_fn_rem := remove_frame_num(obs = trial_id)]

# Now, the number of unique values for both trial_id and
# trial_id_fn_rem should be the same
risc1_dt[, .(unique_trial_id = uniqueN(trial_id),
             unique_trial_id_fn_rem = uniqueN(trial_id_fn_rem))]

# Stop if the number of unique trial IDs does not match the
# number of unique shortened trial IDs
stopifnot(risc1_dt[, uniqueN(trial_id) == uniqueN(trial_id_fn_rem)])

risc1_dt[, trial_id_fn_rem := NULL]


# Old,  before P_4066 was removed from the data  --------------------------

# un-comment below to see analysis of P_4066 data

#    unique_trial_id unique_trial_id_fn_rem
# 1:           51316                  51267

# # less unique values values when frame number is removed.... something
# # not right
# 
# # Let's check:
# risc1_dt[, .(n_unique_strides = uniqueN(trial_id)),
#          by = trial_id_fn_rem][n_unique_strides != 1]
# 
# # ok, is this just from 1 subject?
# risc1_dt[, .(n_unique_strides = uniqueN(trial_id)),
#          by = trial_id_fn_rem][
#            n_unique_strides != 1,
#            unique(stringr::str_extract(trial_id_fn_rem, "P_\\d+"))]
# # [1] "P_4066"
# # yes
# 
# # Create data table to show the problem -----------------------------------
# # For each shortened trial ID, calculate the unique (and number of unique)
# # trial IDs. Then, filter for where there is more than one shortened trial
# # ID 
# show_trial_id_prob_dt <- risc1_dt[, .(n_unique_strides = uniqueN(trial_id),
#              unique_strides = list(unique(trial_id))),
#          by = .(`shortened trial_id` = trial_id_fn_rem)][
#            n_unique_strides > 1
#            ]
# 
# risc1_dt[, .(n_unique_strides = uniqueN(trial_id),
#              unique_strides = list(unique(trial_id))),
#          by = .(`shortened trial_id` = trial_id_fn_rem)][
#            n_unique_strides > 1
#            ]
# 
# show_trial_id_prob_dt[, n_unique_strides := NULL]
# 
# show_trial_id_prob_dt[, `:=`(`original trial_ids` = unique_strides,
#                              unique_strides = NULL)]
# 
# head(show_trial_id_prob_dt)
# 
# 
# # Remove objects not needed -----------------------------------------------
# 
# rm(show_trial_id_prob_dt)
