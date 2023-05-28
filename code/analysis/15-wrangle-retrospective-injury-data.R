# ------------------------------------------------------------------------#
# First step at data cleaning - change variables to the correct format
# Quickly look for very unusual/ odd values. (better checks later)
# ------------------------------------------------------------------------#

# Preliminaries - packages and scripts ------------------------------------
library(data.table) # CRAN v1.14.0

# Convert to data table
retro_injury_data <- as.data.table(
  retro_injury_data
)

# Look at data
# View(retro_injury_data)

# Subject ID --------------------------------------------------------------

# Do all IDs have the form P_ 4 digits?
# (check what % of IDs we have in correct format)
retro_injury_data[,
            paste(100 * mean(stringr::str_detect(subject_id, "P_\\d{4}")),
                  "%")]
# no.

# How many do not?
retro_injury_data[, sum(!stringr::str_detect(subject_id, "P_\\d{4}"))]
# 4

# Look at these:
retro_injury_data[!stringr::str_detect(subject_id, "P_\\d{4}")]

# They are all p_ 4 digits instead of P_?
retro_injury_data[!stringr::str_detect(subject_id, "P_\\d{4}"),
            all(stringr::str_detect(subject_id, "p_\\d{4}"))]
# yes.

# For these rows, replace p with P:
# using look ahead assertion ('?=') to be safe.
retro_injury_data[!stringr::str_detect(subject_id, "P_\\d{4}"),
            subject_id := stringr::str_replace(
              subject_id, "p(?=_\\d{4})", replacement = "P")]

# Done
# throw error if all subject IDs are not in correct format
stopifnot(
  retro_injury_data[, all(stringr::str_detect(subject_id, "P_\\d{4}"))])


# and if there are any duplicates
stopifnot(
  retro_injury_data[, !any(duplicated(subject_id))]
)


# Age ---------------------------------------------------------------------

# numeric variable. # in years (seems ok)
retro_injury_data[, summary(age)]
# all values within acceptable range (18-64).
# 14 missing values


# Sex ---------------------------------------------------------------------

retro_injury_data[, summary(as.factor(sex))]
# 1    2    NA's 
# 183  118   14 

# Factor variable.
# coding: male is 1 & female is 2
# deta
retro_injury_data[, sex := factor(sex,
                            levels = c(1, 2),
                            labels = c("male", "female"))]

retro_injury_data[, summary(sex)]
#  male female   NA's 
#   183    118     14 




# Runner Category ---------------------------------------------------------

# some notes from correspindence with Sarah
# via email on 2021-09-14 provided the following definition:
# (note: another amended data set recieved via email on 2021-09-20)

# A novice runner was defined as a person who has not been running on a
# regular basis during the last year and has completed ≤10km
# total training volume per week during the preceding year
# (Nielsen et al., 2013).
# A recreational runner was defined as a person who runsa minimum of 10km
# per week, for at least six months prior to inclusion in the study
# (Saragiotto et al., 2014).

# coding from spreadsheet
# 1 novice, 2 recreational

retro_injury_data[, summary(as.factor(runner_category))]

retro_injury_data[, runner_category := factor(
  runner_category,
  levels = c(1, 2),
  labels = c("novice", "recreational")
)]

# View(retro_injury_data )


# Weight ------------------------------------------------------------------

# Weight in KG.
retro_injury_data[, summary(weight_kg)]
# all within an expected range, 15 NAs


# Height ------------------------------------------------------------------

retro_injury_data[, summary(height_cm)]
retro_injury_data[, summary(height_m)]
# all within an expected range, 15 NAs

# BMI ---------------------------------------------------------------------

retro_injury_data[, summary(bmi_kgm)]
# all within an expected range, 15 NAs


# Retrospective injury status ---------------------------------------------

# Categorical Variable:
# From sheet:
# 0 = never injured, 1 = injured<1 year, 2 = Injured 1-2 years, 3= Injured>2 years

retro_injury_data[, summary(factor(retrospective_injury_status))]

# 0    1    2    3 NA's 
# 53  124   54   69   15 

# Define levels and labels for turning to factor:

retro_labels_vec <- c(
  "never_injured", # 0
  "injured_greater_than_2_yr", # 3
  "injured_1_to_2_yr", # 2
  "injured_less_than_1_yr") # 1

retro_levels_vec <- c(0, 3, 2, 1)

# Recode as an ordered factor
retro_injury_data[,
                  retrospective_injury_status := factor(
                    retrospective_injury_status,
                    levels = retro_levels_vec,
                    labels = retro_labels_vec,
                    ordered = TRUE
                  )]

retro_injury_data[,
                  summary(retrospective_injury_status)]


# Prospectively Injured 12 Months -----------------------------------------

# Categorical variable
# Assuming factor levels from last data set:

# 1 injured
# 0 not injured

retro_injury_data[, summary(
  factor(prospectively_injured_12_mths))]

retro_injury_data[, prospectively_injured_12_mths := factor(
  prospectively_injured_12_mths,
  levels = c(0, 1),
  labels = c("not_injured", "injured"))]

retro_injury_data[,
                  summary(prospectively_injured_12_mths)]



# Number of injuries in past year -----------------------------------------

retro_injury_data[, summary(num_retrospective_injuries_1_yr)]
# Between 0 and 4, seems reasonable



# Number of injuries in past 2 years --------------------------------------

retro_injury_data[, summary(num_retrospective_injuries_2_yr)]
# Between 0 and 7, seems reasonable


# Months since most recent injury -----------------------------------------

retro_injury_data[, summary(months_since_most_recent_injury)]

# 121 NAs - a lot expected but need to check these later
# between 2 and 31 months seems reasonable






# Injured area in last year -----------------------------------------------

# Three different values for each injury location:
# From Sarah 22/10/2021
# "So for those columns:
# 0- injured, but not with that injury in the time frame
# 
# 1- injured with that injury in the time frame
# 
# 3- not injured within the time frame"
#
# For me, I think that these 3s and 0s should both be no's
# Because the injured in this time frame can just be grabbed from
# previous columns.

# This makes the variable simpler:
# Was this location injured in past year?
# 0 - no, 1 - yes.
# info on whether they were injured at all in this time frame can be
# found in previous columns.


injured_1_yr_colnames <- paste("injured",
                               injury_locations,
                               "1_yr", sep = "_")

retro_injury_data[,
                  purrr::map(.SD,
                             table),
                  .SDcols = injured_1_yr_colnames]
retro_injury_data[,
                  summary(.SD),
                  .SDcols = injured_1_yr_colnames]

# Write custom function to recode each of the columns
refactor_injury_location <- function(x){
  stopifnot(is.numeric(x)) # must be numeric
  # throw error if we try to re-run on already factor
  stopifnot(!is.factor(x)) 
  factor(x,
         levels = c(0, 1, 3),
         labels = c("no", "yes", "no"))
}

# test function
testthat::expect_equal(refactor_injury_location(c(0, 1, 3, NA)),
                       factor(c("no", "yes", "no", NA)))

# apply it to data
retro_injury_data[,
                  (injured_1_yr_colnames) := purrr::map(
                    .SD,
                    refactor_injury_location),
                  .SDcols = injured_1_yr_colnames]

retro_injury_data[,
                  summary(.SD),
                  .SDcols = injured_1_yr_colnames]


# Injured Area 2 ----------------------------------------------------------

# Same idea as for 1 year

injured_2_yr_colnames <- paste("injured",
                               injury_locations,
                               "2_yr", sep = "_")

retro_injury_data[,
                  purrr::map(.SD,
                             table),
                  .SDcols = injured_2_yr_colnames]



# apply same custom function as above to data
retro_injury_data[,
                  (injured_2_yr_colnames) := purrr::map(
                    .SD,
                    refactor_injury_location),
                  .SDcols = injured_2_yr_colnames]

retro_injury_data[,
                  summary(.SD),
                  .SDcols = injured_2_yr_colnames]


# Again, look at data.
# View(retro_injury_data)



# Add simplified retrospective injury category ----------------------------

# Based on advice from Andrew at meeting 27/10/2021

# Write custom function to do this:
simplify_injury_status <- function(x){
  stopifnot(is.factor(x))
  fcase(
    x == "never_injured", "no",
    x %in% c(
      "injured_less_than_1_yr",
      "injured_1_to_2_yr",
      "injured_greater_than_2_yr"),
    "yes"
    )
}


# Test:
testthat::expect_equal(
  simplify_injury_status(
    as.factor(c("injured_less_than_1_yr", 
                "never_injured",
                "injured_1_to_2_yr",
                "injured_greater_than_2_yr",
                NA))
  ),
  c("yes", "no", "yes", "yes", NA)
)


# And create simplified binary variable:
retro_injury_data[, simplified_retrospective_injury_status :=
                    simplify_injury_status(retrospective_injury_status)]




# Update: Add self selected speed variable --------------------------------
# on 10/12/2021
# Add self-selected speed variable


## Read in data ------------------------------------------------------------
path_to_speed <- here::here("data", "self-selected-speed.xlsx")
col_names_speed <- c("subject_id", "self_selected_speed_kmph")
col_types_speed <- c("text", "numeric")
skip_n_speed <- 2
na_strings_speed <- c("999")
self_selected_speed_tbl <- readxl::read_xlsx(path = path_to_speed,
                                         col_names = col_names_speed,
                                         skip = skip_n_speed,
                                         na = na_strings_speed)

self_selected_speed_dt <- as.data.table(self_selected_speed_tbl)



## Clean up data ----------------------------------------------------------
# Do usual checks on data
# Do all IDs have the form P_ 4 digits?
# (check what % of IDs we have in correct format)
self_selected_speed_dt[,
                  paste(100 * mean(stringr::str_detect(subject_id, "P_\\d{4}")),
                        "%")]
# no.

# How many do not?
self_selected_speed_dt[, sum(!stringr::str_detect(subject_id, "P_\\d{4}"))]
# 4

# Look at these:
self_selected_speed_dt[!stringr::str_detect(subject_id, "P_\\d{4}")]

# They are all p_ 4 digits instead of P_?
self_selected_speed_dt[!stringr::str_detect(subject_id, "P_\\d{4}"),
                  all(stringr::str_detect(subject_id, "p_\\d{4}"))]
# yes.

# For these rows, replace p with P:
# using look ahead assertion ('?=') to be safe.
self_selected_speed_dt[!stringr::str_detect(subject_id, "P_\\d{4}"),
                  subject_id := stringr::str_replace(
                    subject_id, "p(?=_\\d{4})", replacement = "P")]





## Join it to the main retrospective injury data --------------------------
# Look at the 
retro_injury_data[
  subject_id %in% self_selected_speed_dt[is.na(self_selected_speed_kmph), subject_id]]
# note that P-4154 seems to be non-missing on most other variables ...(?)

# # Now, check NAs:
# are these NA for self selected speed the NAs that are in the data set...?
# retro_injury_data[subject_id %in% self_selected_speed_dt[is.na(running_speed), subject_id],
#                   all(purrr::map_lgl(.x = .SD, .f = function(x) all(is.na(x)))),
#                   .SDcols = age:simplified_retrospective_injury_status]

stopifnot(nrow(self_selected_speed_dt) == nrow(retro_injury_data))
# check that all subject IDs are the same
stopifnot(
    fsetequal(
    self_selected_speed_dt[, .(subject_id)],
    retro_injury_data[, .(subject_id)]
    )
  )

# This means we can inner join safely..
retro_injury_data <- merge.data.table(
  x = retro_injury_data, 
  y = self_selected_speed_dt,
  by = "subject_id",
  all = TRUE)

stopifnot(nrow(retro_injury_data) == nrow(self_selected_speed_dt))

# get a summary of self-selected speed variable:
retro_injury_data[, summary(self_selected_speed_kmph)]
#     Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#    5.00   10.00   11.00   11.01   12.00   16.00       6 
# quite a wide range... 5 to 16, but all values believable
retro_injury_data[, hist(self_selected_speed_kmph)]
retro_injury_data
