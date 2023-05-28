remove_frame_num <- function(obs){
  
  #`@description
  #' `remove_stride_num` removes frame number
  #' from observation or trial_id variables
  #'
  #'
  #'@param obs a character vector containing observation name
  #' e.g., "P_4001_L_stride600_fn882"
  #' 
  stopifnot(is.character(obs))
  
  # check that it has the correct format
  stopifnot(
    stringr::str_detect(
      string = obs,
      pattern = "P_\\d+_[LR]*_stride\\d+_fn\\d+")
    )
  
  # then extract what we need:
  stringr::str_extract(string = obs,
              pattern = "P_\\d+_[LR]*_stride\\d+")
  # extract:
  # P_
  # \\d+ means one or more digits
  # _
  # [LR] means L or R
  # _stride
  # \\d+ means one or more digits
  
  # alternative: 
  # stringr::str_remove(string = obs, pattern = "_fn\\d+")
}

# do tests:
testthat::expect_equal(remove_frame_num(obs = "P_4001_L_stride600_fn882"),
                       "P_4001_L_stride600")

testthat::expect_equal(remove_frame_num(obs = "P_4001_R_stride5_fn5000"),
                       "P_4001_R_stride5")

testthat::expect_equal(remove_frame_num(obs = "P_4200_R_stride50_fn5000"),
                       "P_4200_R_stride50")

testthat::expect_equal(remove_frame_num(obs = "P_1000000_R_stride5555555_fn100000"),
                       "P_1000000_R_stride5555555")



# New function 5th April 2022 ---------------------------------------------

# To GET frame number

# Also, if we wanted to get frame number,
# the following function works and uses a
# positive look-behind assertion

get_frame_num <- function(obs){
  
  stopifnot(is.character(obs))
  # Matches the frame number only if it sees the
  # string P_\\d{4}_[LR]_stride\\d{1,3}_fn before
  fn_char <- stringr::str_extract(
    string = obs,
    pattern = "(?<=P_\\d{4}_[LR]_stride\\d{1,3}_fn)\\d+")
  
  fn_num <- as.numeric(fn_char)
}

# do tests:
testthat::expect_equal(get_frame_num(obs = "P_4001_L_stride600_fn882"),
                       882)

testthat::expect_equal(get_frame_num(obs = "P_4001_R_stride5_fn5000"),
                       5000)

testthat::expect_equal(get_frame_num(obs = "P_4200_R_stride50_fn5000"),
                       5000)

testthat::expect_equal(get_frame_num(obs = "P_1000_R_stride555_fn100000"),
                       100000)


