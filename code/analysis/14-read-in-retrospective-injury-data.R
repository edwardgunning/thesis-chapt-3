# ------------------------------------------------------------------------#
# Goal: Read in the retrospective injury data
# ------------------------------------------------------------------------#

# Path to the data:
retro_injury_data_path <- here::here(
  "data",
  "injury-and-demographics-retrospective.xlsx"
)

# Get the column names from the data as is
# (we will need to fix these)
original_names_vec <- names(readxl::read_xlsx(
  path = retro_injury_data_path, n_max = 0))

# Look at them:
original_names_vec

# Define the column types for the data:
col_types_vec <- c("text", # Subject ID
                   "skip", # Notes for exclusion and o/w blank
                   rep("numeric", 12), # 12 variables either numeric or numbers representing categories
                   "skip", # Empty column as header
                   rep("numeric", 15),
                   "skip", # Empty column as header
                   rep("numeric", 15))

# Column names

# Injury location will be repeated twice
# (for whether they became injured at 1 or 2 years)
# define it here first
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

# create vector of clumn types for the data
col_names_vec <- c("subject_id", # Subject ID
                   NA, # Notes for exclusion and o/w blank
                   "age", # Age
                   "sex", # Sex (coded as 1 and 2)
                   "runner_category", # Category of runner (coded as 1 and 2)
                   "weight_kg", # Weight (kg)
                   "height_cm", # Height (cm)
                   "height_m", # Height (m)
                   "bmi_kgm", # BMI (kg/m^2),
                   "retrospective_injury_status", # Retrospective injury dummy coded w/ 4 levels (0-3)
                   "prospectively_injured_12_mths", # Prospective injury dummy coded w/ 2 levels
                   "num_retrospective_injuries_1_yr", # Number of retrospective injuries in past year
                   "num_retrospective_injuries_2_yr", # Number of retrospective injuries in past 2 years
                   "months_since_most_recent_injury", # Months since most recent time of injury
                   NA, # Empty column as header
                   paste("injured",
                         injury_locations,
                         "1_yr", sep = "_"),
                   NA, # Empty column as header
                   paste("injured",
                         injury_locations,
                         "2_yr", sep = "_")
                   )


# Check that original names, new names and column types make sense
check_variables_df <- data.frame(old = original_names_vec,
           new = col_names_vec,
           type = col_types_vec)

# logical check that those with NA as column names are the ones being skipped
stopifnot(is.na(
  check_variables_df$new[check_variables_df$type == "skip"]
  )
  )

# Some extra parameters for reading in
na_strings_vec <- c("666",
                    "999",
                    "555") # Character vector of strings to interpret as missing values.
skip_n <- 2 # Minimum number of rows to skip before reading anything

# Read in data:
retro_injury_data <- readxl::read_xlsx(
  path = retro_injury_data_path,
  sheet =  1,
  col_names = col_names_vec,
  col_types = col_types_vec,
  skip = skip_n,
  na = na_strings_vec
)
