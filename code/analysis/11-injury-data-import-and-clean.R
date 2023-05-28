# ------------------------------------------------------------------------#
# Purpose:
# Read in the injury and demographics data. Perform data wrangling.
# Generate a quick summary of the data set and then save it.
# ------------------------------------------------------------------------#

require(data.table)

# Set up for data import --------------------------------------------------
injury_data_path <- here::here(
  "data",
  "injury-and-demographics-data.xlsx")

# Comments on columns that can be dropped based on meeting
# Shane and Sarah on 2021-09-14.

col_names_vec <- c("subject_id",
                   "prospectively_excluded_notes", # can be dropped
                   "exclusion_code", # can be dropped
                   "prospectively_included_12_mths", # can be dropped
                   "age",
                   "sex",
                   "runner_category", # can be dropped
                   "runner_category_recreational", # can be dropped
                   "weight_kg",
                   "height_m",
                   "bmi_kgm",
                   "treadmill",
                   "self_selected_speed",
                   "prospectively_injured_12_mths",
                   "injured_side")

col_types_vec <- c("text",
                   "skip", # Prospectively excluded notes
                   "skip", # Exclusion code
                   "skip", # Prospectively included 12 months
                   "numeric",
                   "numeric",
                   "skip", # Runner Category 1
                   "skip", # Runner Category 2
                   "numeric",
                   "numeric",
                   "numeric",
                   "numeric",
                   "numeric",
                   "numeric",
                   "text")


# Read in data and convert to data table ----------------------------------
injury_data <- readxl::read_xlsx(
  path = injury_data_path,
  skip = 1,
  col_names = col_names_vec,
  col_types = col_types_vec)

# convert to data table
injury_data <- as.data.table(injury_data)

# check dimensions
dim(injury_data)


# Subject ID --------------------------------------------------------------

# Do all IDs have the form P_ 4 digits?
# (check what % of IDs we have in correct format)
injury_data[,
            paste(100 * mean(stringr::str_detect(subject_id, "P_\\d{4}")),
                  "%")]
# no.

# How many do not?
injury_data[, sum(!stringr::str_detect(subject_id, "P_\\d{4}"))]
# 4

# Look at these:
injury_data[!stringr::str_detect(subject_id, "P_\\d{4}")]

# They are all p_ 4 digits instead of P_?
injury_data[!stringr::str_detect(subject_id, "P_\\d{4}"),
            all(stringr::str_detect(subject_id, "p_\\d{4}"))]
# yes.

# For these rows, replace p with P:
# using look ahead assertion ('?=') to be safe.
injury_data[!stringr::str_detect(subject_id, "P_\\d{4}"),
            subject_id := stringr::str_replace(
              subject_id, "p(?=_\\d{4})", replacement = "P")]

# Done
# throw error if all subject IDs are not in correct format
stopifnot(
  injury_data[, all(stringr::str_detect(subject_id, "P_\\d{4}"))])


# and if there are any duplicates
stopifnot(
  injury_data[, !any(duplicated(subject_id))]
)


# Age ---------------------------------------------------------------------
# numeric variable. # in years (seems ok)
injury_data[, summary(age)]
# all values within acceptable range (18-64).

# And no missing values:
injury_data[, any(is.na(age))]



# Sex ---------------------------------------------------------------------

# Factor variable.
# coding: male is 0 & female is 1
# confirmed at meeting 2021-09-14
injury_data[, sex := factor(sex,
                            levels = c(0, 1),
                            labels = c("male", "female"))]

injury_data[, .N, by = sex][, pc := paste0(100 * (N / sum(N)), "%")][]

# sex   N
# 1:   0 160
# 2:   1  96


# Category of Runner ------------------------------------------------------

# Sarah provided amended data set of runner categories
# via email on 2021-09-14 along with the following definition:
# (note: another amended data set recieved via email on 2021-09-20)

# A novice runner was defined as a person who has not been running on a
# regular basis during the last year and has completed ≤10km
# total training volume per week during the preceding year
# (Nielsen et al., 2013).
# A recreational runner was defined as a person who runsa minimum of 10km
# per week, for at least six months prior to inclusion in the study
# (Saragiotto et al., 2014).
# I would use the second column for analysis,
# as the competitive runners were all also recreational,
# but they also competed in events.

# So we read in the amended categories:
amended_categories_path <- here::here(
  "data",
  "runner-categories-amended.xlsx")

amended_categories_names <- c(
  "subject_id",
  "runner_category_1",
  "runner_category_2"
  )


# subject ID is text, categories are integers
amended_categories_types <- c("text",
                              rep("numeric", 2))

# Read in data
amended_categories_df <- readxl::read_xlsx(path = amended_categories_path,
                                           col_names = amended_categories_names,
                                           col_types = amended_categories_types,
                                           skip = 2, sheet = 1)
# convert to data table
amended_categories_dt <- as.data.table(amended_categories_df)

# from email 2021-09-20 the missing subject ID in row 256 is
# subject P_4244
amended_categories_dt[is.na(subject_id), subject_id := "P_4244"]

# Do the same processing on subject ID columns as before
# Do all IDs have the form P_ 4 digits?
amended_categories_dt[, mean(stringr::str_detect(subject_id, "P_\\d{4}"))]
# no.

# How many do not?
amended_categories_dt[, sum(!stringr::str_detect(subject_id, "P_\\d{4}"))]
# 4

# View these:
amended_categories_dt[!stringr::str_detect(subject_id, "P_\\d{4}")]

# They are all p_ 4 digits instead of P_?
amended_categories_dt[!stringr::str_detect(subject_id, "P_\\d{4}"),
            all(stringr::str_detect(subject_id, "p_\\d{4}"))]
# yes.

# For these rows, replace p with P:
# using look ahead assertion ('?=') to be safe.
amended_categories_dt[!stringr::str_detect(subject_id, "P_\\d{4}"),
            subject_id := stringr::str_replace(
              subject_id, "p(?=_\\d{4})", replacement = "P")]

# Done
# now, all subject IDs are in correct format?
amended_categories_dt[, all(stringr::str_detect(subject_id, "P_\\d{4}"))]


# Now, check that that the subject IDs are the same in both data sets
stopifnot(
  setequal(injury_data$subject_id,
           amended_categories_dt$subject_id)
  )

# inner-join these amended categories to the injury data set
injury_data <- injury_data[amended_categories_dt,
                           on = "subject_id",
                           nomatch = 0]

# Code as factors based on info in Excel file:

# Runner Category 1:
# 1- novice, 2- recreational, 3- competitive

# Runner Category 2:
# 1- novice, 2- recreational, 2- competitive

injury_data[, `:=`(
  runner_category_1 = factor(
    runner_category_1,
    levels = c(1, 2, 3),
    labels = c("novice", "recreational", "competitive")),
  runner_category_2 = factor(
    runner_category_2,
    levels = c(1, 2),
    labels = c("novice", "recreational"))
  )]



# Quick test if the levels for runner_category_1
# are what we would expect by manually
# re-coding all competitive runners to be recreational.

test_copy_dt <- copy(injury_data)
test_copy_dt[, test_runner_category_2 := factor(
  runner_category_1,
  levels = c("novice", "recreational", "competitive"),
  labels = c("novice", "recreational", "recreational"))]

stopifnot(
  all(test_copy_dt$runner_category_2 == test_copy_dt$test_runner_category_2)
)

stopifnot(
  all.equal.factor(test_copy_dt$runner_category_2,
                 test_copy_dt$test_runner_category_2))

stopifnot(
  identical(test_copy_dt$runner_category_2,
          test_copy_dt$test_runner_category_2)
  )

rm(test_copy_dt)

# Weight ------------------------------------------------------------------

# Weight in KG.
injury_data[, summary(weight_kg)]

# oh ok... there is a "666" value (max)
# presumably this is NA
injury_data[weight_kg == 666]
# Subject P_4257

# from meeting 2021-09-14, Sarah:
# This subject's weight was coded as 66 because it was deemed an outlier
# the actual weight value supplied was 132kg

injury_data[subject_id == "P_4257", weight_kg := 132]
# Summarise data again:
injury_data[, summary(weight_kg)]


# Height ------------------------------------------------------------------

# height in metres
injury_data[, summary(height_m)]

# all within believable range (I think)


# BMI ---------------------------------------------------------------------

# look at bmi values
injury_data[, summary(bmi_kgm)]
injury_data[, hist(bmi_kgm)]

# manually check that BMI  consistent with height and weights:
bmi_calc <- function(weight_kg, height_m) {
  (weight_kg / (height_m^2))
}

bmi_test <- copy(injury_data)
bmi_test[, bmi_test := bmi_calc(weight_kg = weight_kg, height_m = height_m)]
plot(bmi_kgm ~ bmi_test,
     xlab = "BMI Calculated from Height and Weight Variables",
     ylab = "BMI Supplied in Data",
     data = bmi_test, pch = 16)

stopifnot(
  all(round(
    bmi_test$bmi_test - bmi_test$bmi_kgm,
    digits = 8) == 0)
)

rm(bmi_test)


# Treadmill ---------------------------------------------------------------
# old and new treadmills
# 1 old
# 2 new
# confirmed at meeting 2021-09-14
injury_data[, treadmill := factor(treadmill,
                                  levels = c(1, 2),
                                  labels = c("old", "new"))]

injury_data[, .N, by = treadmill]


# Self - selected speed -----------------------------------------------------

# from meeting meeting 2021-09-14
# unit of measurement of self selected speed is kmph
injury_data[, summary(self_selected_speed)]
injury_data[, hist(self_selected_speed)]
setnames(
  injury_data,
  "self_selected_speed",
  "self_selected_speed_kmph")

comment_speed <- "Even though this variable is measured in kmph, it should be reported in metres"
comment(injury_data$self_selected_speed_kmph) <- comment_speed

# Prospectively injured 12 months -----------------------------------------

# 1 injured
# 0 not injured
injury_data[, prospectively_injured_12_mths := factor(
  prospectively_injured_12_mths,
  levels = c(0, 1),
  labels = c("not_injured", "injured"))]

injury_data[, .N,
            by = prospectively_injured_12_mths]


# Injured Side -------------------------------------------------------------

# injured side - dominant side, non dominant side or bilateral injury
# coded 0 for missing if they didn't become injured.

injury_data[, .N, by =  injured_side]
injury_data[, injured_side := factor(injured_side, levels = c("Bilateral",
                                                              "ND",
                                                              "D",
                                                              "0"),
                                     labels = c("bilateral",
                                                "non_dominant",
                                                "dominant",
                                                "0"))]

injury_data[injured_side == "0", injured_side := NA]

# Check all these values for injured are non-missing
injury_data[prospectively_injured_12_mths == "injured",
            all(!is.na(injured_side))]
# And all these values for non-injured are non-missing
injury_data[prospectively_injured_12_mths == "not_injured",
            all(is.na(injured_side))]





# Summary -----------------------------------------------------------------

inspectdf::show_plot(inspectdf::inspect_cat(
  injury_data[, -c("subject_id")]
),
col_palette = 0,
label_thresh = 0.01,
label_color = c("white", "black"))


skimr::skim(injury_data)


# Save as an RDS file -----------------------------------------------------

# Save cleaned data as RDS
save_rds_path <- here::here(
  "outputs",
  "data",
  "cleaned-injury-and-demographics-data.rds"
)
save_csv_path <- here::here(
  "outputs",
  "data",
  "cleaned-injury-and-demographics-data.csv"
)

saveRDS(object = injury_data,
        file = save_rds_path)
fwrite(x = injury_data, file = save_csv_path)


rm(list = c("amended_categories_df", "amended_categories_dt",
  "amended_categories_names", "amended_categories_path",
  "amended_categories_types", "bmi_calc", 
  "col_names_vec", "col_types_vec",
  "comment_speed", "injury_data",
  "injury_data_path", "save_csv_path", "save_rds_path"))
