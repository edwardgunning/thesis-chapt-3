# Set up ------------------------------------------------------------------
library(data.table) # CRAN v1.14.0 

# And adjust printing settings:
# Plus nice setup options for printing (like tibble):
options(datatable.print.class = TRUE,
        datatable.print.keys = TRUE)

# And variable for set up code paths
code_folder_path <- here::here("code", "analysis")

# ------------------------------------------------------------------------#
# Part 1
# Initial read in and clean of risc1 kinematic data.
# ------------------------------------------------------------------------#

# (Optional) --------------------------------------------------------------
# Script to download data from google drive into "data" folder in project

# (commented out for now)

# source(file = file.path(code_folder_path))

# Read in risc1 data ------------------------------------------------------
# This is a large file and can take a small bit of time
# usually < 20 seconds.
source(file = file.path(code_folder_path, "02-risc1-data-import.R"))

# Read in Screen Index Data -----------------------------------------------
source(file = file.path(code_folder_path, "03-screen-index-data-import.R"))

# Very basic checks and cleaning of both datasets -------------------------
source(file = file.path(code_folder_path, "04-initial-data-clean.R"))

# Remove participants whose entire data are invalid -----------------------
# These are participants whose data should be removed completely
# this will also write a csv file to the outputs folder
# of the participant IDs and number of rows being removed
source(file = file.path(code_folder_path, "05-exclude-participants.R"))

# Script that investigates screen index dataset more closely --------------
# This script doesn't actually modify the dataset but there are some checks
# in it so we will run it anyway.
source(file = file.path(code_folder_path, "06-second-data-clean.R"))

# Script that investigates number of observations per trial ---------------
# Checks number of rows for each stride for each subject
# Again, doesn't modify the data set but does some checks so 
# compute
source(file = file.path(code_folder_path,
                        "07-investigate-observations-per-trial.R"))

# Script that checks whether each stride is unique ------------------------
# Does this by matching frame numbers (which indicate the start of a new
# trial) with stride ID
# Again, doesn't modify data set but does checks and was used to find 
# P_4066 (removed in script 5) who had duplicates
source(file = file.path(code_folder_path,
                        "08-investigate-removing-frame-number.R"))


# Script to remove frame number variables in screen index -----------------
# the frame number discrepancies in the screen index dataset are
# simply a result of manual input, and can be ignored
# However, it is still worthwhile to save them
# This script removes them from the screen index dataset but also
# writes to output folder a csv containing any discrepancies.
source(file = file.path(code_folder_path,
                        "09-screen-index-frame-numbers.R"))

# Clean variables ---------------------------------------------------------
# This script uses custom functions to perform
# important parts of the data cleaning
# Using custom functions, its splits larger string variables
# into sensibly named smaller variables.
# then, it converts/ checks the class of the variables.
source(file = file.path(code_folder_path,
                        "10-third-data-clean.R"))

# Trying to detect "flipped" curves using L2 distance ---------------------
# use custom functions to detect flipped curves. This piece of work
# was done to identify potential problems with the data.
# As of 31/01/2021 - These results were communicated to Shane but not
# followed up.

# (commented out as it takes time to run)
# source(file = file.path(code_folder_path,
#                         "13-run-flipped-curves-screen.R"))

# ------------------------------------------------------------------------#
# Part 2
# Initial read in and clean injury data sets
# ------------------------------------------------------------------------#


# Read in prospective only injury data  -----------------------------------
# We only ended up using this data for cross checking more comprehensive
# injury data (i.e., prospective and retrospective) that arrived
source(file = file.path(code_folder_path,
                        "11-injury-data-import-and-clean.R"))


# Examine relationship between missing data and injury --------------------
# Through visualisation. In the process, this script also saves
# an rds object containing data on misssingness and injury in the outputs
# folder
# (note: this generates a lot of plots!)
# (commented because it doesn't need to be ran to prepare data)
# source(file = file.path(
#   code_folder_path,
#   "12-injury-data-and-missing-comparison.R"))


# Now, read in retrospective injury dataset ------------------------------
source(file = file.path(code_folder_path,
                        "14-read-in-retrospective-injury-data.R"))

# Clean and amend this dataset --------------------------------------------
source(file = file.path(code_folder_path,
                        "15-wrangle-retrospective-injury-data.R"))

# Cross check the contents of this dataset --------------------------------
# these are all the "sense checks" that I could think of
# (will produce some plots)
source(
  file = file.path(code_folder_path,
                   "16-cross-check-retrospective-injury-data.R"))

# Cross check the retrospective injury data with any missingness ----------
source(
  file = file.path(code_folder_path,
                   "17-retro-injury-and-missing-comparison.R"))

# ------------------------------------------------------------------------#
# Part 3
# More cleaning + creating the dataset to share
# ------------------------------------------------------------------------#

# Remove flagged strides from risc1 dataset -------------------------------
# if a stride is flagged in screen index dataset, remove from risc1
# also creates a csv in outputs file of some subjects who are of interest
# beacuase they appear in either the injury or screen index datasets
# but not the risc1.
# Although, for now, we didn't manage to follow these up with shane

# also removes a subject where >70% of strides are missing 
# (on advice of Shane and Kieran).
source(
  file = file.path(code_folder_path,
                   "18-remove-screen-index-strides.R"))


# Create dataset to share ------------------------------------------------

# Time normalise the data and convert to functions
# Store basis coefficients and evaluations of the data
# Combine with injury data
# write and save these to rds objects and csv files
# on google drive for use with group
# (also store copy in the data folder)
source(file = file.path(code_folder_path, "19-create-dataset-to-share.R"))

