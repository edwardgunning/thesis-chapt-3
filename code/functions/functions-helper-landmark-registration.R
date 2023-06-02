# ------------------------------------------------------------------------#
# Functions to help with landmark registration
# They pick out the peak kfa from an FDA object containing the kfa
# ------------------------------------------------------------------------#


# Function: Get middle maximum --------------------------------------------

# for an object x the which.max function only returns the first element  
# satisfying x == max(x). This can be misleading.

# This function instead of which.min that picks middle element if there are  
# multiple adjacent maximums


## Function ---------------------------------------------------------------

which_max_midval <- function(x, tol = 0){
  
  stopifnot(is.numeric(x))
  stopifnot(is.vector(x))
  stopifnot(tol >= 0)
  
  if(tol == 0){
    all_max <- which(x == max(x))
  } else{
    all_max <- which(abs(x - max(x)) <= tol)
  }
  
  if(length(all_max) == 0){stop("No match found, try adjusting tolerance")}
  if(length(all_max) == 1){
    return(all_max)
  }
  if(!all(diff(all_max) == 1)) {
    stop(
      "Multiple matches of x == max(x) found are NOT adjacent, does not make sense to choose middle."
      )
  } else {
    warning("Multiple adjacent matches found, choosing middle")
    stopifnot(all(diff(all_max) == 1))
    return(floor(median(all_max)))
  }
}



## Test ------------------------------------------------------------------
testthat::expect_warning(
  test_val <- which_max_midval(rep(c(1, 2), each = 3))
  )

testthat::expect_equal(test_val, 5)

testthat::expect_equal(which_max_midval(1:10), 10)

testthat::expect_equal(which_max_midval(10:1), 1)

testthat::expect_error(which_max_midval(rep(1:2, times = 5)))




# Function Get Peak KFA ---------------------------------------------------


## Function ---------------------------------------------------------------


# function to pick out peak knee flexion angle from an fda object.
get_peak_kfa <- function(kfa_fd, n_eval_points = 501){
  
  # kfa_fd 
  # a univariate fda object containing a single knee flexion angle curve
  
  # check if its an fd object
  stopifnot(fda::is.fd(kfa_fd))
  
  # check it is univariate
  stopifnot(length(dim(kfa_fd$coef)) == 2)
  # with one column
  
  stopifnot(ncol(kfa_fd$coef) == 1)
  
  # get range of domain
  dom_range <- kfa_fd$basis$rangeval
  
  # create fine grid of equally spaced points to evaluate the data on
  eval_seq <- seq(from = dom_range[1],
                  to = dom_range[2],
                  length.out = n_eval_points)
  
  # evaluate the curves on this grid
  kfa_eval <- c(fda::eval.fd(evalarg = eval_seq,
                      fdobj = kfa_fd))
  
  # find which is the maximum value of the curve applied across columns (curves)
  landmark_index <- which_max_midval(x = kfa_eval)
  
  stopifnot(length(landmark_index) == 1) 
  
  landmark_timing <- eval_seq[landmark_index]
  
  # bonus - also return the maximum values
  landmark_value <- max(kfa_eval)
  stopifnot(length(landmark_value) == 1)
  
  
  # check that maximum knee angle is in second half of gait cycle
  mid_point <- mean(dom_range)
  stopifnot(landmark_timing > mid_point)
  
  # return 
  return(list(timing = landmark_timing,
              value = landmark_value))
  
}



## Test -------------------------------------------------------------------

# Create an FD object containing a sin wave on [0, 2pi]
# we know from calculus that the max is at 2pi/3

# test this
test_basis_1 <- fda::create.fourier.basis(rangeval = c(0, 2*pi),
                     nbasis = 3)
test_coef_1 <- c(0, -1, 0)
test_fd_1 <- fda::fd(test_coef_1, test_basis_1)

testthat::expect_equal(
  object = get_peak_kfa(kfa_fd = test_fd_1)$timing,
  expected = (3*pi)/2,
  tolerance = 10^-4)

# test that it throws error if raw values rather than fd object is supplied.
testthat::expect_error(
  object = get_peak_kfa(
    eval.fd(
    evalarg = seq(0, 2 * pi, length.out = 100), fdobj = test_fd_1
    )
  )
  )

# This time it is const + sin(x) + cos(x)
# this time max is at 7pi/4
# (https://www.symbolab.com/solver/calculus-function-extreme-points-calculator/extreme%20points%20f%5Cleft(x%5Cright)%3D-sin%5Cleft(x%5Cright)%2Bcos%5Cleft(x%5Cright)?or=input)
test_coef_2 <- c(1, -1, 1)
test_fd_2 <- fda::fd(test_coef_2, test_basis_1)
# need to take more points to avoid warning of multiple matches
# and adjust tolerance
testthat::expect_equal(
  object = get_peak_kfa(kfa_fd = test_fd_2,
                        n_eval_points = 1000)$timing,
  expected = (7*pi)/4,
  tolerance = 10^-2)


rm(list = c("test_basis_1", "test_coef_1",
            "test_coef_2", "test_fd_1", 
            "test_fd_2", "test_val"))

