# -------------------------------------------------------------------------
# Function to smooth data coming from a data table
# -------------------------------------------------------------------------

# Smooth data from data table ---------------------------------------------

# Wrapper function for fda::smooth.basis

# Should already have sourced: "functions-helper-smoothing.R"

# this function linear time normalises and smooths discretised functional
# data that has been stored in a data table object and returns a list in
# that the coefficients can be easily added to a data table by reference

#
# - takes data.table as input where the rows are curves
#   and columns are time points

# - returns list where each element is a vector of coefficients

# - list length is the number of basis functions

# - length of each coefficient vector in the is number of rows in 
#   data table that is passed.

# smoothing:
# - use smooth.bassis from fda package
# - use bspline basis (either supplied or via specifying parameters,
# i.e. no. of basis functions and order)
# - default is cubic B-Spline basis functions
# - fit via penalised least squares
# - penalty can be specified (default is on second derivative)
# - penalty, lambda, is specified as argument


## Function ---------------------------------------------------------------
smooth_from_dt <- function(raw_dt,
                                   supplied_basis,
                                   norder = 4,
                                   nbasis,
                                   pen_deriv = 2L,
                                   lambda) {
  
  # Step 1 - Some routine checks on the data table
  # others will follow later depending on whether 
  # basis is supplied
  stopifnot(data.table::is.data.table(raw_dt))
  stopifnot(all(sapply(raw_dt, FUN = is.numeric)))
  
  
  
  # Step 2: Determine basis to be used
  # Either use basis that is being supplied or else create a new one
  if(missing(supplied_basis)){
    if(ncol(raw_dt) < nbasis){
      warning("More basis functions than time points being used")
    }
    bspl_basis <- fda::create.bspline.basis(rangeval = c(0, 100),
                                            nbasis = nbasis,
                                            norder = norder)
  } else {
    if(!fda::is.basis(supplied_basis)) {
      stop("supplied_basis must be a basis object")
    }
    bspl_basis <- supplied_basis
  } 
  
  # Step 3: Set up argument values for time normalisation
  argvals <- seq(from = 0, to = 100, length.out = ncol(raw_dt))
  
  # Step 4: Transpose the data because FDA requires the
  # "the rows must correspond to argument values and columns
  # to replications"
  # ^ which is the opposite of how we store our data.
  dt_as_matrix_t <- t(as.matrix(raw_dt))
  
  # Step 5: Smooth the transposed data with the time normalised
  # argument values
  coef_to_return <- fda::smooth.basis(argvals = argvals,
                                 y = dt_as_matrix_t,
                                 fdParobj = fda::fdPar(fdobj = bspl_basis,
                                                  Lfdobj = pen_deriv,
                                                  lambda = lambda))$fd$coef
  # Check before returning
  stopifnot(ncol(coef_to_return) == nrow(raw_dt))
  
  
  get_list_of_rows(coef_to_return)
}



## Test --------------------------------------------------------------------
# Create fake data from basis and coefficients to test:
test_coef_matrix <- matrix(1:16, ncol = 4, nrow = 4, byrow = TRUE)
test_bspl_basis <- fda::create.bspline.basis(rangeval = c(0, 100), nbasis = 4, norder = 4)
test_data_matrix <- fda::eval.fd(evalarg = seq(0, 100, by = 1),
                                 fdobj = fda::fd(coef = test_coef_matrix,
                                                 basisobj = test_bspl_basis))
test_dt_input <- data.table::as.data.table(t(test_data_matrix))
# result (in list):
test_result <- list(c(1, 2, 3, 4),
                    c(5, 6, 7, 8),
                    c(9, 10, 11, 12),
                    c(13, 14, 15, 16))

# test when basis function (used to create data) is supplied:
testthat::expect_equal(object = smooth_from_dt(raw_dt = test_dt_input,
                                                       supplied_basis = test_bspl_basis,
                                                       lambda = 0,
                                                       pen_deriv = 2L),
                      expected = test_result,
                      tolerance = 10^-10)

# test where basis being created inside function:
testthat::expect_equal(object = smooth_from_dt(raw_dt = test_dt_input,
                                                       nbasis = 4, norder = 4,
                                                       lambda = 0,
                                                       pen_deriv = 2L),
                       expected = test_result,
                       tolerance = 10^-10)

# should throw error if columns are not numeric:
test_dt_error_input <- data.table::copy(test_dt_input)[, a := "hello"]

testthat::expect_error(smooth_from_dt(raw_dt = test_dt__error_input,
                                      nbasis = 4, norder = 4,
                                      lambda = 0, pen_deriv = 2L))

# and if it is passed a data frame not a data table
testthat::expect_error(smooth_from_dt(raw_dt = as.data.frame(test_dt_input),
                                      nbasis = 4, norder = 4,
                                      lambda = 0, pen_deriv = 2L))
# and if it is passed a matrix
testthat::expect_error(smooth_from_dt(raw_dt = t(test_data_matrix),
                                      nbasis = 4, norder = 4,
                                      lambda = 0, pen_deriv = 2L))

# remove all objects created for testing:
rm(list = c("test_bspl_basis", "test_coef_matrix",
            "test_data_matrix", "test_dt_input",
            "test_result", "test_dt_error_input"))

# Come back and write a test when lambda > 0.
