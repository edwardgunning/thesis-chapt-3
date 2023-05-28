trial_to_subject_id <- function(obs, shortened = FALSE){
  #`@description
  #' `trial_to_subject_id` gets the subject_id
  #' from observation or trial_id variables
  #'
  #'
  #'@param obs a character vector containing observation name
  #' e.g., "P_4001_L_stride600_fn882" (function checks for this)
  #' can also work on shortened trial id
  #' (i.e. with frame number removed)
  #' by setting shortened = TRUE
  stopifnot(is.character(obs))
  
  if(!shortened){
    # check that it has the correct format
    stopifnot(
      stringr::str_detect(
        string = obs,
        pattern = "P_\\d{4}_[LR]_stride\\d{1,3}_fn\\d+")
    )
  }
  
  if(shortened){
    # check that it has the correct format
    stopifnot(
      stringr::str_detect(
        string = obs,
        pattern = "P_\\d{4}_[LR]_stride\\d{1,3}")
    )
  }
  
  if(!shortened){
    # check that it has the correct format
    return(
      stringr::str_extract(
        string = obs,
        pattern = "P_\\d{4}(?=_[LR]_stride\\d{1,3}_fn\\d+)")
    )
  }
  
  if(shortened){
    # check that it has the correct format
    return(
      stringr::str_extract(
        string = obs,
        pattern = "P_\\d{4}(?=_[LR]_stride\\d{1,3})")
    )
  }
  # positive look-ahead assertion.
  # Grab P_ and 4 digits if it is preceded by
  # _[LR]_stride\\d{1,3})
  
  # alternative: 
  # stringr::str_remove(string = obs, pattern = "_[LR]*_stride\\d+_fn\\d+")
}

# do tests:
testthat::expect_equal(trial_to_subject_id(obs = "P_4001_L_stride600_fn882"),
                       "P_4001")

testthat::expect_equal(trial_to_subject_id(obs = "P_4001_R_stride5_fn500"),
                       "P_4001")

testthat::expect_equal(trial_to_subject_id(obs = "P_4200_R_stride50_fn500"),
                       "P_4200")

testthat::expect_equal(trial_to_subject_id(obs = "P_1000_R_stride555_fn100000"),
                       "P_1000")


