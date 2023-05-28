#-------------------------------------------------------------------------#
# This script does important parts of the data cleaning
# Using custom functions, its splits larger string variables
# into sensibly named smaller variables.
# then, it converts/ checks the class of the variables.
#-------------------------------------------------------------------------#

# Load functions:
source(here::here("code",
                  "functions",
                  "functions-clean-trial_id.R"))

source(here::here("code",
                  "functions",
                  "functions-clean-condition.R"))

# Remove frame numbers ----------------------------------------------------
# from trial_id and observation variables
# use custom function:
# ----------------------------
# experimental:
risc1_dt[, frame_num := get_frame_num(trial_id)]
# ----------------------------


risc1_dt[, trial_id := remove_frame_num(trial_id)]


screen_index[, observation := remove_frame_num(observation)]


# Convert long strings to smaller variables -------------------------------
# use custom functions:
risc1_dt[, `:=`(
  subject_id = get_subject_id(trial_id),
  side = get_side(trial_id),
  stride_num = get_stride_num(trial_id),
  location = get_location(condition4),
  quantity = get_quantity(condition4),
  plane_of_motion = get_plane_of_motion(condition4)
)]

screen_index[, `:=`(
  subject_id = get_subject_id(observation),
  side = get_side(observation),
  stride_num = get_stride_num(observation),
  location = get_location(condition),
  quantity = get_quantity(condition),
  plane_of_motion = get_plane_of_motion(condition)
)]


# create longitudinal time variable for each subject.
risc1_dt[, long_time := (frame_num - min(frame_num)) / max(frame_num),
         by = subject_id]
# Inspect data new columns ------------------------------------------------
risc1_dt[, skimr::skim(.SD),
         .SDcols = c("subject_id", "side", "stride_num",
                     "location", "quantity", "plane_of_motion", "frame_num", "long_time")]

screen_index[, skimr::skim(.SD),
         .SDcols = c("subject_id", "side", "stride_num",
                     "location", "quantity", "plane_of_motion")]


# Manage column classes ---------------------------------------------------

risc1_dt[, `:=`(
  subject_id = factor(subject_id),
  side = factor(side, levels = c("left", "right")),
  location = factor(location,
                    levels = c("Thorax", "Pelvis", "Hip",
                               "Knee", "Ankle", "CentreOfMass")),
  quantity = factor(quantity, levels = c("Angles", "Velocity")),
  plane_of_motion = factor(plane_of_motion,
                           levels = c("fle", "abd", "rot", "x", "y", "z"))
)]

screen_index[, `:=`(
  subject_id = factor(subject_id),
  side = factor(side, levels = c("left", "right")),
  location = factor(location,
                    levels = c("Thorax", "Pelvis", "Hip",
                               "Knee", "Ankle", "Foot")),
  quantity = factor(quantity, levels = c("Angles", "Velocity")),
  plane_of_motion = factor(plane_of_motion,
                           levels = c("fle", "abd", "rot", "x", "y", "z"))
)]


# Inspect again -----------------------------------------------------------

risc1_dt[, skimr::skim(.SD),
         .SDcols = c("subject_id", "side", "stride_num",
                     "location", "quantity", "plane_of_motion", "frame_num", "long_time")]

risc1_dt[, head(.SD),
         .SDcols = c("subject_id", "side", "stride_num",
                     "location", "quantity", "plane_of_motion", "frame_num", "long_time")]



screen_index[, skimr::skim(.SD),
         .SDcols = c("subject_id", "side", "stride_num",
                     "location", "quantity", "plane_of_motion")]

screen_index[, head(.SD),
         .SDcols = c("subject_id", "side", "stride_num",
                     "location", "quantity", "plane_of_motion")]

# check dimensions
dim(risc1_dt)
dim(screen_index)

# Save an intermediate copy of the data:
fwrite(risc1_dt, file = here::here("outputs",
                                   "data",
                                   "risc1_intermediate_copy_01.R"))

# Save an intermediate copy of the data:
fwrite(screen_index, file = here::here("outputs",
                                   "data",
                                   "screen_index_intermediate_copy_01.R"))

# finished



