# ------------------------------------------------------------------------#
# Script to do "cross checks" on the data.
# These are things we know should definitely be right, but are still worth
# checking. Should help find data errors
# ------------------------------------------------------------------------#

# Height variables check --------------------------------------------------

# Check that all missing heigh (m) are also missing for height (cm)
# & vice-versa
retro_injury_data[is.na(height_m),
                  all(is.na(height_cm))]

retro_injury_data[is.na(height_cm),
                  all(is.na(height_m))]

# Now, check that the two are consistent
# Visually
retro_injury_data[, plot(height_m, height_cm)]
# Their ratio should be 100
retro_injury_data[!is.na(height_m),
                  all(height_cm/ height_m - 100 < 10^-6)]




# BMI variable check ------------------------------------------------------

# Count the number of NAs for height, weight and BMI
retro_injury_data[, purrr::map(.x = .SD,
                               .f = function(x) {
                                 sum(is.na(x))
                               }),
                  .SDcols = c("height_m",
                              "height_cm",
                              "bmi_kgm")]

# Ok, same number of NAs for all.
# check these are same rows
retro_injury_data[is.na(height_m), 
                  all(
                    is.na(height_cm) & is.na(bmi_kgm)
                    )
                  ]
# Ok...


# manually calculate BMI
# then consistent with height and weights
# custom function for calculation:
bmi_calc <- function(weight_kg, height_m) {
  (weight_kg / (height_m^2))
}

retro_injury_data[,
                  manual_bmi_kgm := bmi_calc(weight_kg,
                                             height_m)]





# check that it produced same number of NAs
retro_injury_data[, purrr::map(.x = .SD,
                               .f = function(x) {
                                 sum(is.na(x))
                               }),
                  .SDcols = c("height_m",
                              "height_cm",
                              "bmi_kgm",
                              "manual_bmi_kgm")]

# Visually check
retro_injury_data[, plot(bmi_kgm, manual_bmi_kgm)]
# And check fully:
stopifnot(
  retro_injury_data[!is.na(bmi_kgm),
                    all(bmi_kgm - manual_bmi_kgm < 10^-6)]
)

retro_injury_data[, manual_bmi_kgm := NULL]

# Number of injuries ------------------------------------------------------

# number of injuries in past year should be <= number of injuries in 
# past 2 years

# Check about NAs. These should be the same for both years.
retro_injury_data[, purrr::map(.x = .SD,
                               .f = function(x) {
                                 sum(is.na(x))
                               }),
                  .SDcols = c("num_retrospective_injuries_1_yr",
                              "num_retrospective_injuries_2_yr")]

retro_injury_data[is.na(num_retrospective_injuries_1_yr),
                  all(is.na(num_retrospective_injuries_2_yr))]

# check that number of injuries in past year should be <= number of injuries in 
# past 2 years
retro_injury_data[!is.na(num_retrospective_injuries_1_yr),
                  all(
                    num_retrospective_injuries_1_yr <=
                      num_retrospective_injuries_2_yr
                    )]


# Number of injuries should be an integer ---------------------------------

retro_injury_data[!is.na(num_retrospective_injuries_1_yr),
                  purrr::map(.x = .SD,
                             .f = function(x) {all(x == round(x))}),
                  .SDcols = c("num_retrospective_injuries_1_yr",
                              "num_retrospective_injuries_2_yr")]


# Injury location checks --------------------------------------------------


## Injury 1 year  ---------------------------------------------------------


# if they are marked as having an injury at a location in the past year
# they should be down as being injured in past year
injury_locations <- c("calf",
                      "heel",
                      "hip",
                      "buttock",
                      "knee",
                      "back_of_thigh",
                      "front_of_thigh",
                      "ankle",
                      "shin",
                      "foot",
                      "lower_back",
                      "itb", # IT band
                      "toes",
                      "inner_thigh",
                      "sij" # sacroiliac joint (SI Joint? Check this)
)

injured_1_yr_colnames <- paste("injured",
                               injury_locations,
                               "1_yr", sep = "_")

# create dt for test:

# test_1_col:
# flagges true if subject has a yes for any injury lcoation in past year

# test_2_col:
# flags true if retrospective_injury_status says they have been injured in past year

# the test columns should be the same!

test_1_yr_dt <- retro_injury_data[,
                  .(test_1_col = apply(X = .SD, 
                                       MARGIN = 1,
                                       FUN = function(x) {any(x == "yes")}),
                    test_2_col =
                      retrospective_injury_status == "injured_less_than_1_yr"),
                  .SDcols = injured_1_yr_colnames]

# Just check that the NAs are coming from the same place
stopifnot(test_1_yr_dt[, sum(is.na(test_1_col))] ==
            test_1_yr_dt[, sum(is.na(test_2_col))])

stopifnot(test_1_yr_dt[is.na(test_1_col),
                       all(is.na(test_2_col))])

# Now do formal test 
test_1_yr_dt[!is.na(test_1_col),
             all(test_1_col == test_2_col)]



## Injury 2 year  ---------------------------------------------------------
# Same idea for injury 2 years
# but
# test 2 column will need to look for either
# "injured_less_than_1_yr" OR
# injured_1_to_2_yr in the 
# retrospective_injury_status column
injured_2_yr_colnames <- paste("injured",
                               injury_locations,
                               "2_yr", sep = "_")

test_2_yr_dt <- retro_injury_data[,
                                  .(subject_id,
                                    test_1_col = apply(X = .SD, 
                                                       MARGIN = 1,
                                                       FUN = function(x) {any(x == "yes")}),
                                    test_2_col =
                                      retrospective_injury_status  == "injured_less_than_1_yr" |
                                      retrospective_injury_status  == "injured_1_to_2_yr"),
                                  .SDcols = injured_2_yr_colnames]


# Just check that the NAs are coming from the same place
stopifnot(test_2_yr_dt[, sum(is.na(test_1_col))] ==
            test_2_yr_dt[, sum(is.na(test_2_col))])
stopifnot(test_2_yr_dt[is.na(test_1_col),
                       all(is.na(test_2_col))])

# Now do formal test 
test_2_yr_dt[!is.na(test_1_col),
             all(test_1_col == test_2_col)]

# Oh, fails.... lets check what breaks it
test_2_yr_dt[!is.na(test_1_col) & !(test_1_col == test_2_col),]
fail_id <- test_2_yr_dt[!is.na(test_1_col) & !(test_1_col == test_2_col),
                        subject_id]
fail_id
# [1] "P_4259"





# Look at relationship between injured 1 year and injured 2 years ---------

# Write code to check if a location is injured within the last year
# the that is also marked as injured for the last 2 years
# loop through pairs of location variables

years_check <- purrr::map2(.x = injured_1_yr_colnames, # location injured 1 year
            .y = injured_2_yr_colnames, # location injured 2 years
            .f = function(x, y){
              retro_injury_data[get(x) == "yes", # if injured in past year
                                all(get(x) == get(y))] # also injured in past 2 years
            })

names(years_check) <- injury_locations
problem_ind <- which(years_check == F) 
# again, problems with knee!

retro_injury_data[injured_knee_1_yr =="yes" & injured_knee_2_yr == "no"]

# ^ again... same problem, same ID as above.

# correspondence from Sarah confirms that this is an error in the data,
# and needs to be manually edited (email 22/10/2021)

# Double check that this is subject P_4259 (mentioned in email):
stopifnot(
  retro_injury_data[injured_knee_1_yr =="yes" & injured_knee_2_yr == "no",
                  subject_id == "P_4259"]
)

# and manually replace 
retro_injury_data[subject_id == "P_4259",
                  injured_knee_2_yr := "yes"]

# now re-run check
stopifnot(
  all(
purrr::map2_lgl(.x = injured_1_yr_colnames, # location injured 1 year
            .y = injured_2_yr_colnames, # location injured 2 years
            .f = function(x, y){
              retro_injury_data[get(x) == "yes", # if injured in past year
                                all(get(x) == get(y))] # also injured in past 2 years
            })
)
)

# All good.

# re-run check from section just above..
test_2_yr_dt <- retro_injury_data[,
                                  .(subject_id,
                                    test_1_col = apply(X = .SD, 
                                                       MARGIN = 1,
                                                       FUN = function(x) {any(x == "yes")}),
                                    test_2_col = retrospective_injury_status  == "injured_less_than_1_yr" |
                                      retrospective_injury_status  == "injured_1_to_2_yr"),
                                  .SDcols = injured_2_yr_colnames]

stopifnot(
test_2_yr_dt[!is.na(test_1_col),
             all(test_1_col == test_2_col)]
)


# Months since most recent injury and injury status -----------------------

# Months since injury should be:
# 2 to 12 months if injured past year
# 12-24 months if injured
# NA if injured greater than 2 years or never injured

retro_injury_data[!is.na(retrospective_injury_status) &
                    retrospective_injury_status != "never_injured",
                  .(min = min(months_since_most_recent_injury,
                              na.rm = TRUE),
                    max = max(months_since_most_recent_injury,
                              na.rm = TRUE)),
                  by = retrospective_injury_status]


# Check formally:
retro_injury_data[retrospective_injury_status == "injured_less_than_1_yr",
                    any(!months_since_most_recent_injury %between% c(2, 12))]
# ok..

retro_injury_data[retrospective_injury_status == "injured_1_to_2_yr",
                    any(!(months_since_most_recent_injury %between% c(12, 24)))]

# fails check... look:
retro_injury_data[retrospective_injury_status == "injured_1_to_2_yr" &
                    !(months_since_most_recent_injury %between% c(12, 24))]
# P_4145

retro_injury_data[retrospective_injury_status == "injured_greater_than_2_yr",
                    any(!months_since_most_recent_injury >= 24)]

retro_injury_data[retrospective_injury_status == "injured_greater_than_2_yr" & 
                    !months_since_most_recent_injury >= 24]

# ^ Flag the above two with Sarah.

# Flagged with Sarah, and response was:

# Apologies, I should have omitted that column as the category of
# retrospective injury column is the most accurate- some people had
# reported when they had the injury and were vague as to when this
# happened (start of the month/end of the month) so we had to ring them
# up and enquire to get the most accurate info- that is what is included
# in the '1-2 years, <1 year, >2 year, never injured column).

# Could drop this for now (maybe use again later):
retro_injury_data[, months_since_most_recent_injury := NULL]



# Cross-check self-selected speed -----------------------------------------

# retro_injury_data[, hist(self_selected_speed_kmph)]
# must be > 0 obviously if its a speed
stopifnot(
  all(
    retro_injury_data[!is.na(self_selected_speed_kmph), 
                      self_selected_speed_kmph > 0]
    ))


# Crosscheck with previous Prospective data -------------------------------

# This section can be ignored by others running this script
# As it's cross checking with other scripts/ data files
# you can skip to writing to csv.

# source(here::here(
#   "code",
#   "full-data",
#   "11-injury-data-import-and-clean.R"
# ))

injury_data <- readRDS(here::here(
  "outputs",
  "data",
  "cleaned-injury-and-demographics-data.RDS"
))


## Check runner categories ------------------------------------------------
test_dt_1 <- injury_data[
  ,
  .(subject_id,
    age_1 = age,
    sex_1 = sex,
    weight_1 = weight_kg,
    height_1 = height_m,
    bmi_1 = bmi_kgm,
    pros_1 = prospectively_injured_12_mths,
    run_cat_1 = runner_category_2,
    speed_1 = self_selected_speed_kmph)
]

test_dt_2 <- retro_injury_data[
  ,
  .(subject_id,
    age_2 = age,
    sex_2 = sex,
    weight_2 = weight_kg,
    height_2 = height_m,
    bmi_2 = bmi_kgm,
    pros_2 = prospectively_injured_12_mths,
    run_cat_2 = runner_category,
    speed_2 = self_selected_speed_kmph)
]


test_dt_1[, .N]
test_dt_2[, .N]

test_dt_combined <- test_dt_1[test_dt_2,
          on = .(subject_id),
          nomatch = 0]

# check that join has not dropped any rows of prospective data

stopifnot(
  test_dt_combined[, .N] == test_dt_1[, .N]
)

# Check factor variables
test_dt_combined[, all(sex_1 == sex_2,
                     pros_1 == pros_2,
                     run_cat_1 == run_cat_2)]

# Check numeric variables

test_dt_combined_num <- test_dt_combined[,
                 .(age_diff = age_1 - age_2,
                   weight_diff = weight_1 - weight_2,
                   height_diff = height_1 - height_2,
                   bmi_diff = bmi_1 - bmi_2,
                   speed_df = speed_1 - speed_2)]

stopifnot(
  all(test_dt_combined_num < 10^-10)
)

# all looks good.



# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
# additional -- add treadmill and leg-dominance variable...

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
  "outputs",
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


# -------------------------------------------------------------------------
# -------------------------------------------------------------------------

# -------------------------------------------------------------------------
# combine treadmill and retro injury data
# check that subject ID as same levels for inneer join:
stopifnot(nrow(retro_injury_data) == nrow(treadmill_and_dominance_dt))
stopifnot(unique(retro_injury_data$subject_id) == unique(treadmill_and_dominance_dt$subject_id))

retro_injury_data_updated <- merge.data.table(x = retro_injury_data, 
                                              y = treadmill_and_dominance_dt, 
                                              by = "subject_id", 
                                              all = TRUE)

stopifnot(retro_injury_data_updated[, .N] == retro_injury_data[, .N])
stopifnot(setequal(retro_injury_data$subject_id, retro_injury_data_updated$subject_id))

# write to csv ------------------------------------------------------------
path_to_write <- here::here(
  "outputs",
  "data",
  "cleaned-injury-and-demographics-retrospective.csv"
)

fwrite(retro_injury_data_updated,
       file = path_to_write)


# write to rds ------------------------------------------------------------

path_for_rds <- here::here(
  "outputs",
  "data",
  "cleaned-injury-and-demographics-retrospective.rds"
)

saveRDS(object = retro_injury_data_updated,
        file = path_for_rds)

