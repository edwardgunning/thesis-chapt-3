#-------------------------------------------------------------------------#
# Before we go to look at removing frame numbers from trial_id
# to create a stride stride variable we will look at how many observations
# per stride variable. This is where there can be a source of error
#-------------------------------------------------------------------------#

# Do all trials correspond to the same number of observations.
risc1_dt[, .N, by = trial_id][, unique(N)]
# ...no.
# some subjects 36 and other have 24

investigate_trial_obs_dt <- copy(risc1_dt)
# See which conditions, if any, are missing
# use chained expression, first compute number of obs by trial_id
investigate_trial_obs_dt[, n_obs_trial := .(.N), by = trial_id][
  n_obs_trial == 24, unique(condition4)]

# no "CentreOfMass" or "Thorax" in condition4

# formally check this:
investigate_trial_obs_dt[, n_obs_trial := .(.N), by = trial_id][
  n_obs_trial == 24, # filter those with trials with 24 observations
  any(condition4 %like% "CentreOfMass" | condition4 %like% "Thorax")
  ]

# Create a data frame of observation where there are 24 obs per trial:
trials_24_obs_dt <- investigate_trial_obs_dt[n_obs_trial == 24]
# Don't need this variable anymore:
trials_24_obs_dt[, n_obs_trial := NULL]

# How many subjects does this correspond to?
trials_24_obs_dt[,
                 subject_id := .(stringr::str_extract(trial_id, "P_\\d+"))
                 ][, unique(subject_id)]
# [1] "P_4217" "P_4257"

# Get these subject IDs
subject_ids_24_obs <- trials_24_obs_dt[, unique(subject_id)]

# Lets get a more in-depth look.
# How many trial_ids have 24 observations per trial
trials_24_obs_dt[, .(unique_trials = uniqueN(trial_id)), by = subject_id]
# subject_id unique_trials
# <char>         <int>
#   1:     P_4217           157
# 2:     P_4257           177


# Do all their trial IDs have 24 observations?
# basically a check of whether there are any trials 
# for these subjects with other than 24 obs
investigate_trial_obs_dt[
  stringr::str_extract(trial_id, "P_\\d+")
  %chin%
  subject_ids_24_obs &
    n_obs_trial != 24,
  nrow(.SD) == 0
  ]
#[1] TRUE
#... Yes!

# Do these subjects have any "CentreOfMass" or "Thorax" variables?
investigate_trial_obs_dt[
  stringr::str_extract(trial_id, "P_\\d+") %chin% subject_ids_24_obs,
  any(condition4 %like% "CentreOfMass" | condition4 %like% "Thorax")
]

# Remove temporary variables.
rm(investigate_trial_obs_dt, trials_24_obs_dt, subject_ids_24_obs)
