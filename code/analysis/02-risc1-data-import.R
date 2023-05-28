#-------------------------------------------------------------------------#
# Import the .csv file that has been downloaded containing risc1 data
#-------------------------------------------------------------------------#

# Using data.table::fread()
# CRAN v1.14.0

# Import data set from stored .csv file -----------------------------------

# First create list specifying which columns should be read in as
# character or numeric:

# - all the columns containing kinematic data should be numeric
# - all of the "information" columns should be character (for a start):

numeric_var_names <- paste("data", 0:197, sep = "_")

char_var_names <- c(paste0("condition", 1:4), "trial_id")

col_classes_list <- list(numeric = numeric_var_names, # kinematic
                         character =  char_var_names) # information

# Assuming that data has been downloaded into data folder:
risc1_full_data_path <- here::here("data", "risc1_full_data.csv")

# Read the .csv and time it:
system.time(risc1_dt <- data.table::fread(
  file = risc1_full_data_path,
  na.strings = c("NaN"), # treat NaN values as NA (missing values)
  colClasses = col_classes_list) # use list to specify classes ids
  )
# user system elapsed
# 15.364 8.424 15.209
# very quick


# Remove temporary variables created from environment ---------------------
rm(list = c("numeric_var_names",
            "char_var_names",
            "col_classes_list",
            "risc1_full_data_path"))
