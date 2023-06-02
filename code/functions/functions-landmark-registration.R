# ------------------------------------------------------------------------#
# Functions to help with the landmark registration of the risc1 data.
# ------------------------------------------------------------------------#

# Create warping function for one landmark --------------------------------

## Function ---------------------------------------------------------------

create_warping_function_1_lm <- function(target,
                                         landmark,
                                         lower,
                                         upper,
                                         lambda_w) {
  
  # Helper function to create simple warping functions
  # to be used when registering data to a single landmark.
  
  # The basis used is a quadratic b-spline basis with 3 knots:
  # - one at the start of the domain
  # - placed at the target landmark
  # - one at the end of the domain
  
  # Penalised least squares is used to fit the curve, (penalise 1st derivative)
  # and monotonicity is checked after the warping function is created
  # (the monotone smoothing technique is not needed for these simple functions)
  
  # routine checks
  stopifnot(lambda_w >= 0)
  stopifnot(upper > lower)
  stopifnot(target > lower & target < upper)
  stopifnot(landmark > lower & landmark < upper)
  
  # Create x and y values for smoothing:
  
  # - target value on the abscissa
  x_val <- c(lower, target, upper)
  # - landmark value on the ordinal
  y_val <- c(lower, landmark, upper)
  
  # create simple basis for constructing the warping functions
  wbasisLM <- fda::create.bspline.basis(rangeval = c(lower, upper),
                                        nbasis = 4,
                                        norder = 3,
                                        breaks = x_val)
  
  # initialize coefficients for smoothing
  # (not sure if this needs to be done,
  # think I took it from Ramsay, Hooker & Graves (2009)
  Wfd0   <- fda::fd(coef = matrix(data = 0,
                                  nrow = wbasisLM$nbasis,
                                  ncol = 1),
                    basisobj = wbasisLM)
  # set up fdPar object 
  WfdPar <- fda::fdPar(fdobj = Wfd0,
                       Lfdobj = 1, # penalise 1st derivative
                       lambda = lambda_w) # penalty
  
  # return fda object
  fda::smooth.basis(argvals = x_val,
                    y = y_val, 
                    fdParobj = WfdPar)$fd
}



## Test -------------------------------------------------------------------

# First test, most simple.
# Landmark and targer are same value; warping functions should be
# the identity function y = x.
test_1_fd <- create_warping_function_1_lm(target = 50,
                                         landmark = 50,
                                         lower = 0,
                                         upper = 100,
                                         lambda_w = 1e-10) # minimal smoothing

# test that if we evaluate at points 0,1,...,100 we should get 
# these points back out
testthat::expect_equal(
  round(c(fda::eval.fd(evalarg = 0:100, fdobj = test_1_fd))),
  0:100)

# re-do this test for another value of landmark and target (again equal)
test_1_fd <- create_warping_function_1_lm(target = 25,
                                          landmark = 25,
                                          lower = 0,
                                          upper = 100,
                                          lambda_w = 1e-10) # minimal smoothing

testthat::expect_equal(
  round(c(fda::eval.fd(evalarg = 0:100, fdobj = test_1_fd))),
  0:100)

# better test of this function is to check that it works to 
# create a function that maps the target to the landmark.

# the outputted function should have the (x, y) pair (target, landmark)

test_2_fd <- create_warping_function_1_lm(target = 50,
                                          landmark = 60, # note difference
                                          lower = 0,
                                          upper = 100,
                                          lambda_w = 1e-10) # minimal smoothing

testthat::expect_equal(
  c(round(fda::eval.fd(evalarg = 50, fdobj = test_2_fd), 3)), # evaluate function at 50
  60 # should return 60
)

# re-do with target and landmark switched
test_2_fd <- create_warping_function_1_lm(target = 60,
                                          landmark = 50, # note difference
                                          lower = 0,
                                          upper = 100,
                                          lambda_w = 1e-10) # minimal smoothing

testthat::expect_equal(
  c(round(fda::eval.fd(evalarg = 60, fdobj = test_2_fd), 3)), # evaluate function at 60
  50 # should return 60
)

# and test that it gives errors in some trivial circumstances

# incorrectly specify domain limits and landmarks then lie outside
testthat::expect_error(create_warping_function_1_lm(target = 50,
                                                    landmark = 60, 
                                                    lower = 0,
                                                    upper = 10, # note
                                                    lambda_w = 1e-10))
# lower > upper domain limit
testthat::expect_error(create_warping_function_1_lm(target = 50,
                                                    landmark = 60, 
                                                    lower = 100, # note
                                                    upper = 0, # note
                                                    lambda_w = 1e-10))

# trying to use negative value of smoothing paramater.
testthat::expect_error(create_warping_function_1_lm(target = 50,
                                                    landmark = 60, 
                                                    lower = 100,
                                                    upper = 0,
                                                    lambda_w = - 1e-10)) # note



# Register a block of curves with the same landmark -----------------------


## Function ---------------------------------------------------------------

landmark_reg_block <- function(fd_obj, lower, upper, n_eval,
                               landmark, target, lambda_y, lambda_w){
  
  # Description:
  # -----------
  # Function to register a group of curves from the same, single landmark
  # to a target landmark
  # It is much faster than the standard 'fda' package function for this special use case.
  # which computes the warping function and registration for each curve separately
  
  # Arguments:
  # ----------
  # fd_obj = fda::fd object containing the group of curves which share a common landmark
  # lower = start point of domain
  # upper = end point of domain
  # n_eval = number of discretisation when re-smoothing the curves for registration
  # landmark = the single, common landmark for all curves, must be  in (lower, upper)
  # target = the target landmark to register to, must be  in (lower, upper)
  # lambda_y = smoothing parameter for registered functions (see explanation below)
  # lambda_w = smoothing parameter for constructing the warping functions.
  
  # Reason for lambda_y parameter is as follows (from fda package):
  # ------------------------------------------------------------
  # "It can happen with high dimensional bases that local wiggles can appear in the registered
  # curves or their derivatives that are not seen in the unregistered versions.
  # In this case, this parameter should be increased to the point where they disappear."
  
  # Some basic checks
  stopifnot(length(landmark) == 1)
  stopifnot(length(target) == 1)
  
  # Create evaluation points for curves
  eval_seq <- seq(from = lower, to = upper, length.out = n_eval)
  
  # Calculate warping function as an fd object, call it h
  h_fd <- create_warping_function_1_lm(target = target,
                                       landmark = landmark,
                                       lower = lower,
                                       upper = upper,
                                       lambda_w = lambda_w)
  
  # Evaluate it on the grid
  h_fun_eval <- c(fda::eval.fd(evalarg = eval_seq, fdobj = h_fd))
  
  # Check its monotonicity
  stopifnot(all(diff(h_fun_eval)  >= 0 ))
  
  # Get inverse warping function by smoothing with the warping function
  # on the abscissa and evaluation points on the ordinal
  h_inv_fd <- fda::smooth.basis(argvals = h_fun_eval,
                           y = eval_seq,
                           fdParobj = h_fd$basis)$fd
  # Evaluate it on the grid
  h_inv_eval <- c(fda::eval.fd(evalarg = eval_seq, fdobj = h_inv_fd))
  
  b <- (upper - lower)/ (h_inv_eval[n_eval] - h_inv_eval[1])
  
  a <- lower - b * h_inv_eval[1]
  
  h_inv_eval <- a + b*h_inv_eval
  
  
  # To make sure the range of the grid points is inside that of the fda basis 
  # (did this because of error)
  h_inv_eval[c(1, n_eval)] <- c(lower, upper)
  
  
  # Re-smooth the data against inverse warping function to register
  # and return the fd object
  
  fda::smooth.basis(argvals = h_inv_eval,
               y = fda::eval.fd(evalarg = eval_seq, fdobj = fd_obj),
               fdParobj = fda::fdPar(fdobj = fd_obj$basis, Lfdobj = 2, lambda = lambda_y))$fd
  
}


## Test -------------------------------------------------------------------


# need to test with a known example.

# lets take the function: sin(x) + cos(x) on [0, 2𝝅]

# this has a known minimum landmark at (5𝝅/4, √2)

# create 50 replicates of this function:
test_1_range <- c(0, 2 * pi)

# create a fourier basis, functions are:
# const, sin, cos (scaled)
test_1_basis <- fda::create.fourier.basis(
  rangeval = test_1_range, nbasis = 3)

test_1_nrep <- 50
# we need to scale the coefficients to make them them
# sin(x) and cos(x) on this interval
test_1_coef <- rep(c(0, 1.77245, 1.77245),
                   times = test_1_nrep)
# put the coefficients in a matrix
test_1_coef <- matrix(data = test_1_coef,
                      nrow = test_1_basis$nbasis,
                      ncol = test_1_nrep, 
                      byrow = FALSE)

# and create an FD object
test_1_fd <- fda::fd(coef = test_1_coef,
                     basisobj = test_1_basis)

# if you want to check the 
# plot(test_1_fd)
# abline(v = (5 * pi)/4)
# abline(h =  - sqrt(2))


# Now, we have to evaluate the function on a grid and re-fit with
# a more flexible bspline basis 

# Reason:
# ------
# The three Fourier basis functions are not flexible to re-fit to the data 
# after registration. BSpline basis functions are not periodic so should 
# be better for this task. Plus, we need to use more of them.

test_1_argvals <- seq(from = test_1_range[1],
                      to = test_1_range[2],
                      length.out = 100)

test_1_bspl <- fda::create.bspline.basis(rangeval = test_1_range,
                                         nbasis = 10,
                                         norder = 4)

test_1_eval <- fda::eval.fd(evalarg = test_1_argvals,
                            fdobj = test_1_fd)

test_1_fd <- fda::smooth.basis(argvals = test_1_argvals,
                               y = test_1_eval,
                               fdParobj = test_1_bspl)$fd

# Again, if you want to inspect visually:
# plot(test_1_fd)
# abline(v = (5 * pi)/4)
# abline(h =  - sqrt(2))

# The test is to warp all thefunctions so that the landmark is at 3.5 insead of (5𝝅)/4)

# Do the warping:
test_1_registered_fd <- landmark_reg_block(fd_obj = test_1_fd,
                                        lower = 0,
                                        upper = 2 * pi,
                                        n_eval = 100,
                                        landmark = (5 * pi)/4,
                                        target = 3.5,
                                        lambda_y = 0,
                                        lambda_w = 10^-8)

# And test:
testthat::expect_equal(object = c(
  fda::eval.fd(evalarg = 3.5,
               fdobj = test_1_registered_fd)),
  expected = rep( - sqrt(2), test_1_nrep),
  tolerance = 0.01)


# remove all objects create during tests from environment:

rm(list = c("test_1_argvals", "test_1_basis",
     "test_1_bspl", "test_1_coef", 
     "test_1_eval", "test_1_fd",
     "test_1_nrep", "test_1_range",
     "test_1_registered_fd", "test_2_fd"))




