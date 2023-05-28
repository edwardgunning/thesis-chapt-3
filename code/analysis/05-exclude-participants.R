#-------------------------------------------------------------------------#
# The screen_index data contains information on a small number of
# participants who's entire data should excluded from the risc1 data set
# the purpose of this script is to identify these subjects from
# the screen_index file and use this information to drop them from
# the risc1 data set.

# Additional temporary removal of observations from subject 4066
#-------------------------------------------------------------------------#


# Helper functions --------------------------------------------------------
source(here::here("code",
                  "functions",
                  "function-trial-to-subject-id.R"))

# Packages ----------------------------------------------------------------

# # data.table should be loaded, but if it is not, then load it.
# if (!("data.table" %in% .packages())) library(data.table)


# Find participants who are to be fully removed from the data set ---------

# We are looking to get the subject IDs.

# From Shane, in email:
# "If a participant's code is appended with '_alldata',
# this means that all of that participant's trials should be excluded."

# On looking through the screen_index data set, I noticed that
# there was IDs appended with 'All_Data', rather than 'alldata'
# Therefore, we do a more thorough check.

# Look for 'All', 'all', 'Data' and 'data':
screen_index[observation %like% "Data"]
screen_index[observation %like% "data"]
screen_index[observation %like% "all"]
screen_index[observation %like% "All"]
# It looks like three subjects that mathc this

# Combine this into a single search.
screen_index[
  tolower(observation) %like% "all" & tolower(observation) %like% "data"
  ]

# These should also be the only rows with NA values for condition
# (for other rows, condition will correspond to a specific trial)
# let's do a check:
check_dts <- identical(# check if data.tables are identical
  screen_index[is.na(condition)],
  screen_index[
    tolower(observation) %like% "all" & tolower(observation) %like% "data"
    ]
  )
stopifnot(check_dts)
rm(check_dts)

# Get the subject IDs with data.table commands.
# i: Filter rows where observation contains all_data or All_Data
# j: Get the subject IDs by grabbing everything before 2nd underscore
# in the observation variable

# regex used: P_ then any number of digits with \\d+

subject_id_exclude <- screen_index[
  observation %like% "alldata" | observation %like% "All_Data",
  stringr::str_extract(observation, "P_\\d+")
]

subject_id_exclude



# Remove all rows from risc1 data set for chosen subjects -----------------

# Check how many rows we will remove for each subject:
# i: Filter if the subject ID (using function we have written)
# j: .N computes the number of rows (quickly)
# by:  Do this for each subject ID

drop_table <- risc1_dt[
  trial_to_subject_id(trial_id) %chin% subject_id_exclude,
  .N,
  by = .(subject_id = trial_to_subject_id(trial_id))]

# subject_id    N
# 1:     P_4060 5976
# 2:     P_4210 5400
# 3:     P_4234 5580

fwrite(x = drop_table,
       file = here::here(
         "outputs",
         "tables",
         "participants-excluded.csv")
       )

# We are removing about 5500-6000 rows per-subject,
# We need to check if this is normal.

# Do this computation for all subjects in the risc1 data set:
risc1_dt[, .N, # Count rows
         by = .(subject_id = trial_to_subject_id(trial_id))]

# 5000 does not look out of the ordinary.
# Let's get a better overview
risc1_dt[, .N, by = trial_to_subject_id(trial_id)][, summary(N)]
risc1_dt[, .N, by = trial_to_subject_id(trial_id)][
  , hist(N, main = "Number of Rows per Subject ID")]

abline(v = 5500)
# Number of rows we are computing for each subject does not seem unusual.

##
# Proceed but come back to here if we can think of better checks to do!
##


# Count number of rows before:
n_before <- risc1_dt[, .N]
n_before
# count number of rows to be dropped:
n_to_subtract <- risc1_dt[
  trial_to_subject_id(trial_id) %chin% subject_id_exclude,
  .N]
n_to_subtract
# [1] 16956
# ^ we will drop this many rows.


# Remove rows corresponding to selected participants:
risc1_dt <- risc1_dt[
  !(trial_to_subject_id(trial_id) %chin% subject_id_exclude)
  ]


# Check that it has subtracted the correct amount of rows:
stopifnot((n_before - n_to_subtract) == risc1_dt[, .N])

rm(n_before)
rm(n_to_subtract)

# New number of rows:
risc1_dt[, .N]


# Drop the 'alldata' appended rows from screen_index ----------------------

# specific = only contains specific strides/ angles
n_before <- screen_index[, .N]
screen_index <- screen_index[
  !(observation %like% "alldata" | observation %like% "All_Data")
]

# must check that this dropped three rows
# i.e. the number of entries in the vector of subject IDs to exclude.
stopifnot((n_before - screen_index[, .N]) == length(subject_id_exclude))
rm(n_before)

# now, how many rows in screen_index
screen_index[, .N]

# Done

# update to code.
# there are also other observations from these subjects
# in the screen index data set.
n_others_to_drop <- screen_index[
  trial_to_subject_id(observation) %chin% subject_id_exclude, .N]
n_others_to_drop
# [1] 581
# ^ 581 other rows in screen index corresponding to these participants

# Look at these by participant.
screen_index[trial_to_subject_id(observation) %chin% subject_id_exclude,
             .N,
             by = .(subject_id = trial_to_subject_id(observation))]
# subject_id   N
# 1:     P_4060 374
# 2:     P_4210  66
# 3:     P_4234 141

n_before_others_drop <- screen_index[, .N]
n_before_others_drop

screen_index <- screen_index[
  !(trial_to_subject_id(observation) %chin% subject_id_exclude)]

stopifnot(screen_index[, .N] == n_before_others_drop - n_others_to_drop)

rm(n_before_others_drop)
rm(n_others_to_drop)

screen_index[, .N]

# TEMPORARY: Remove P_4066 while Shane investigates -----------------------
# Update (31st Jan 2021 - This will be permanent)
# this subject had a problem with duplicate strides and differing
# frame numbers.

# check how many observations are being removed:
(P_4066_N_remove <- risc1_dt[trial_to_subject_id(trial_id) == "P_4066", .N])
(risc1_dt_N_before <- risc1_dt[, .N])

# remove all rows for this subject
risc1_dt <- risc1_dt[
  trial_to_subject_id(trial_id) != "P_4066"
  ]

stopifnot(risc1_dt[, .N] == risc1_dt_N_before - P_4066_N_remove) 

# Check if we need to modify screen index dataset 
# (i.e., throw error if this participant is included)
stopifnot(!screen_index[, any(observation %like% "P_4066")])

# remove temporary variables created in this step
rm(list = c("drop_table", "subject_id_exclude", "risc1_dt_N_before", "P_4066_N_remove"))
