# Functions for working with shortened trial IDs


# Trial IDs to use for testing --------------------------------------------
test_trial_id <- c("P_4082_L_stride84", "P_4096_L_stride12",
                   "P_4170_R_stride72", "P_4298_R_stride65",
                   "P_4239_L_stride15")


# Checking function -------------------------------------------------------

## Function ---------------------------------------------------------------
check_trial_id <- function(trial_id){
  # Function to check that the shortened trial ID is as we would expect
  stopifnot(is.character(trial_id))
  stopifnot(stringr::str_detect(trial_id,
                                pattern = "P_\\d{4}_[LR]_stride\\d{1,3}"))
  
  # Matching Explanation:
  # P_  matches P_
  # \\d{4} matches 4 digits
  # _stride matches _stride
  # \\d{1,3} matches 1 to 3 digits
}

## Test -------------------------------------------------------------------
testthat::expect_null(object = check_trial_id(test_trial_id))



# Subject ID --------------------------------------------------------------


## Function ---------------------------------------------------------------
get_subject_id <- function(trial_id){
  # check
  check_trial_id(trial_id)
  
  stringr::str_extract(string = trial_id,
                       pattern = "P_\\d{4}(?=_[LR]_stride\\d{1,3})")
  # positive look-ahead assertion.
  # Grab P_ and 4 digits if it is preceded by
  # _[LR]_stride\\d{1,3})
}


## Test -------------------------------------------------------------------
testthat::expect_equal(get_subject_id(test_trial_id),
                       c("P_4082", "P_4096", "P_4170", "P_4298", "P_4239"))



# Side --------------------------------------------------------------------


## Function ---------------------------------------------------------------
get_side <- function(trial_id){
  # perform check:
  check_trial_id(trial_id)
  # positive look-ahead assertion.
  # Grab subject ID and side:
  # e.g., "P_4082_L" 
  pos_la <- stringr::str_extract(trial_id, "P_\\d{4}_[LR](?=_stride\\d{1,3})")
  # then, postivie look-behind assertion
  # after subject ID and _, grab either L or R
  pos_lb <- stringr::str_extract(pos_la, "(?<=P_\\d{4}_)[LR]")
  # Finally, it is safer to use "left" an "right" rather than
  # L and R. use data table fcase which is like dplyr::case_when
  data.table::fcase(pos_lb == "R", "right",
                    pos_lb == "L", "left")
}


## Test --------------------------------------------------------------------
testthat::expect_equal(get_side(test_trial_id),
                       c("left", "left", "right", "right", "left"))


# Stride Number -----------------------------------------------------------


## Function ---------------------------------------------------------------
get_stride_num <- function(trial_id){
  check_trial_id(trial_id)
  # positive look-behind assertion to match the digits coming
  # after "stride_"
  stride_num_char <- stringr::str_extract(trial_id,
                                          pattern = "(?<=P_\\d{4}_[LR]_stride)\\d{1,3}")
  # these will be a character, return integer
  as.integer(stride_num_char)
}


## Test --------------------------------------------------------------------
testthat::expect_equal(get_stride_num(test_trial_id),
                       c(84, 12, 72, 65, 15))
# other test where stride nums are > 100 
testthat::expect_equal(get_stride_num("P_4082_L_stride122"),
                       122)
testthat::expect_equal(get_stride_num("P_4021_R_stride1"),
                       1)


# Remove test trial IDs ---------------------------------------------------
rm(test_trial_id)