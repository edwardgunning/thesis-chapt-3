# ------------------------------------------------------------------------#
# Functions for cleaning condition variables
# ------------------------------------------------------------------------#


# Check -------------------------------------------------------------------


## Function ---------------------------------------------------------------


check_condition <- function(condition){
  # Function to check that the string being passed satisfies what we would expect 
  # for a condition variable using regex
  
  # this helper function is used within other functions
  
  stopifnot(is.character(condition))
  
  stopifnot(stringr::str_detect(string = condition,
                                pattern = "[A-z][A-Za-z]+((Angles)|(Velocity))_((rot)|(abd)|(fle)|x|y|z)"))
  
  # Checks:
  # [A-z][a-z]+
  # Capitalized word(s) (e.g., Knee or Centre of Mass)
  
  #((Angles)|(Velocity))
  # Angles OR Velocity
  
  # _ 
  # underscore
  
  # ((rot)|(abd)|(fle)|x|y|z)
  
  # one of:
  # rot, abd, fle, x, y, z
}



## Test -------------------------------------------------------------------

test_conditions <- c("ThoraxAngles_fle", "ThoraxAngles_abd", "ThoraxAngles_rot", 
                     "ThoraxVelocity_fle", "ThoraxVelocity_abd", "ThoraxVelocity_rot", 
                     "PelvisAngles_fle", "PelvisAngles_abd", "PelvisAngles_rot", "PelvisVelocity_fle", 
                     "PelvisVelocity_abd", "PelvisVelocity_rot", "HipAngles_fle", 
                     "HipAngles_abd", "HipAngles_rot", "HipVelocity_fle", "HipVelocity_abd", 
                     "HipVelocity_rot", "KneeAngles_fle", "KneeAngles_abd", "KneeAngles_rot", 
                     "KneeVelocity_fle", "KneeVelocity_abd", "KneeVelocity_rot", "AnkleAngles_fle", 
                     "AnkleAngles_abd", "AnkleAngles_rot", "AnkleVelocity_fle", "AnkleVelocity_abd", 
                     "AnkleVelocity_rot", "CentreOfMassAngles_x", "CentreOfMassAngles_y", 
                     "CentreOfMassAngles_z", "CentreOfMassVelocity_x", "CentreOfMassVelocity_y", 
                     "CentreOfMassVelocity_z")

testthat::expect_null(check_condition(condition = test_conditions))

# Location ----------------------------------------------------------------

## Function ---------------------------------------------------------------

get_location <- function(condition){
  
  # function to retrieve location (e.g., Hip, Thorax...) from longer string
  
  check_condition(condition)
  stringr::str_extract(condition,
                       "[A-z][A-Za-z]+(?=((Angles)|(Velocity))_((rot)|(abd)|(fle)|x|y|z))")
  
  # uses same idea as check with positive look-ahead assertion.
  # i.e. Extract caitalised words using [A-z][A-Za-z]
  # if rest of string after matches as specified
}



## Test -------------------------------------------------------------------

test_results <- c("Thorax", "Thorax", "Thorax", "Thorax", "Thorax", "Thorax", 
                  "Pelvis", "Pelvis", "Pelvis", "Pelvis", "Pelvis", "Pelvis", "Hip", 
                  "Hip", "Hip", "Hip", "Hip", "Hip", "Knee", "Knee", "Knee", "Knee", 
                  "Knee", "Knee", "Ankle", "Ankle", "Ankle", "Ankle", "Ankle", 
                  "Ankle", "CentreOfMass", "CentreOfMass", "CentreOfMass", "CentreOfMass", 
                  "CentreOfMass", "CentreOfMass")

testthat::expect_equal(object = get_location(test_conditions), expected = test_results)






# Quantity ----------------------------------------------------------------


## Function ---------------------------------------------------------------

get_quantity <- function(condition){
  
  # function to retrieve quantity (Angles or Velocity) from longer string
  check_condition(condition)
  
  stringr::str_extract(string = condition, pattern = "(Angles)|(Velocity)")
}

## Test -------------------------------------------------------------------

test_results <- c("Angles", "Angles", "Angles", "Velocity", "Velocity", "Velocity", 
                  "Angles", "Angles", "Angles", "Velocity", "Velocity", "Velocity", 
                  "Angles", "Angles", "Angles", "Velocity", "Velocity", "Velocity", 
                  "Angles", "Angles", "Angles", "Velocity", "Velocity", "Velocity", 
                  "Angles", "Angles", "Angles", "Velocity", "Velocity", "Velocity", 
                  "Angles", "Angles", "Angles", "Velocity", "Velocity", "Velocity")

testthat::expect_equal(object = get_quantity(test_conditions), expected = test_results)



# Plane of  Motion --------------------------------------------------------

## Function ---------------------------------------------------------------

get_plane_of_motion <- function(condition){
  # function to retrieve pane of motion (fle, abd, rot, x, y, z) from longer string
  check_condition(condition)
  
  stringr::str_extract(string = condition,
                    pattern = "(?<=[A-z][A-Za-z]{2,11}((Angles)|(Velocity))_)((rot)|(abd)|(fle)|x|y|z)")
  
  # here we use a positive look-behind assertion.
  # if we match: [A-z][A-Za-z]{2,11}((Angles)|(Velocity))_ 
  # then everything after it is extracted: ((rot)|(abd)|(fle)|x|y|z)
  # note we had to specify a maximum and minium length for the patern 
  # so removed [A-Za-z]+ (any number of upper or lower place characters)
  # and replaced with [A-Za-z]{2,11} between 2 and 11 characters
  # because the look behind assertion requires a BOUNDED charcater length
  # {2, 11} means between 2 and 11.
  # 2 and 11 were chosen empirically i.e. with
  # range(nchar(get_location(test_conditions)) - 1)
}



## Test -------------------------------------------------------------------
test_results <- c("fle", "abd", "rot", "fle", "abd", "rot", "fle", "abd", "rot", 
                  "fle", "abd", "rot", "fle", "abd", "rot", "fle", "abd", "rot", 
                  "fle", "abd", "rot", "fle", "abd", "rot", "fle", "abd", "rot", 
                  "fle", "abd", "rot", "x", "y", "z", "x", "y", "z")

testthat::expect_equal(object = get_plane_of_motion(test_conditions),
                      expected = test_results)



# Removing testing objects ------------------------------------------------

rm(test_results)
rm(test_conditions)
