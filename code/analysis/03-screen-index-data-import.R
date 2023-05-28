#-------------------------------------------------------------------------#
# Import the Excel file that contains the observations in the data to be
# excluded
# Returns it as a data.table for compatability with risc1 data
#-------------------------------------------------------------------------#

# Using readxl CRAN v1.3.1
# and  data.table CRAN v1.14.0

# Read in file ------------------------------------------------------------
screen_index <- readxl::read_excel(
  path = here::here("data", "screen_index.xlsx"),
  sheet = "ExcludeIndex") # it is in one of many sheets, be careful.

# Convert to data.table ---------------------------------------------------
# because we will be using it with risc1_dt
screen_index <- data.table::as.data.table(screen_index)
