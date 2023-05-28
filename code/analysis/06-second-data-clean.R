#-------------------------------------------------------------------------#
# Second look at the data points to issues with "foot" angles
#-------------------------------------------------------------------------#

# Preliminary check of screen index data ----------------------------------
dim(screen_index)

# 1) Look at condition variable -------------------------------------------

# Are all the conditions in the screen index also in the risc1 data set?
all(
  screen_index[, unique(condition)] %chin% risc1_dt[, unique(condition4)]
  )
# [1] FALSE

# OK... need to look further:
# look at observations where condition isn't in the risc1 data set
screen_index[!(condition %chin% risc1_dt[, unique(condition4)])]

# ...OK - these mention FOOT angles!
# How many observations?
screen_index[!(condition %chin% risc1_dt[, unique(condition4)]), .N]
# [1] 19 (formerly 35)

# Check: Are these all of the entries which mention "foot"?
stopifnot(
  screen_index[tolower(condition) %like% "foot"]
  ==
    screen_index[!(condition %chin% risc1_dt[, unique(condition4)])]
  )
# yes, all of these conditions mention foot

# We will not remove these observations from
# the screen index data set.
# this is based on Shane's advice. Even though we don't
# have foot angles in the current data set
# problems with the foot angle on this stride indicate
# there might be a problem elsewhere with this stride.
# Therefore, leave these as strides to remove in
# screen index data set.
