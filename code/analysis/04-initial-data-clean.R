#-------------------------------------------------------------------------#
# A short, initial tidy of the risc1 and screen_index data.
#-------------------------------------------------------------------------#

## Basic Checks -----------------------------------------------------------

# should return risc1_dt data set
stopifnot(exists("risc1_dt"))

# should be a data.table
stopifnot(is.data.table(risc1_dt))

# check dimensions of the data set
dim(risc1_dt)
# [1] 1860324     203
#... big!

##  Data Cleaning --------------------------------------------------------

# Get rid of columns that tell us nothing (only have one unique value):
if (risc1_dt[, uniqueN(condition1)] == 1) risc1_dt[, condition1 := NULL]

if (risc1_dt[, uniqueN(condition2)] == 1) risc1_dt[, condition2 := NULL]

if (risc1_dt[, uniqueN(condition3)] == 1) risc1_dt[, condition3 := NULL]

dim(risc1_dt)
# 1860324     200
# removed 3 columns and 0 rows as expected.
# now, time to remove the observations for screen_index..


# Initial Data Cleaning: Screen Index Data --------------------------------

## Basic Checks -----------------------------------------------------------
# should return screen_index data set
stopifnot(exists("screen_index"))

# should be a data.table
stopifnot(is.data.table(screen_index))

dim(screen_index)
# [1] 4981    4

##  Data Cleaning --------------------------------------------------------
# Try to simplify this data to what we need to exclude observations

# change all variable names to lower case
# this is good practice
colnames(screen_index) <- tolower(colnames(screen_index))

head(screen_index, n = 3)

# condition variable contains:
# time_normalised.Baseline_Run_SelfSelected.stride.KneeVelocity_rot

# We can remove "time_normalised.Baseline_Run_SelfSelected.stride."
screen_index[, condition :=  stringr::str_remove(# stringr, CRAN v1.4.0
  condition,
  pattern = "time_normalised.Baseline_Run_SelfSelected.stride.")]

# Now, look again:
head(screen_index, n = 3)
#  "KneeVelocity_rot"
# This is better!

# Remove columns 3 and 4 (not needed),
# these were Shane's own note:
screen_index[, c("method", "reason") := NULL]

# Check again:
screen_index
# Much better now!
