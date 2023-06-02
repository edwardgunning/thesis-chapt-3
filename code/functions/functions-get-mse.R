# ------------------------------------------------------------------------#
# Functions for calculating mean squared error in smoothing
# ------------------------------------------------------------------------#

# Get mses from data matrix -----------------------------------------------

## Function ---------------------------------------------------------------

# Function to calculate mean squared error for a data matrix:
# for a varying number of basis functions:
get_smoothing_mses <- function(data_matrix,
                               n_basis_from = 50,
                               n_basis_to = 120,
                               n_basis_by = 5){
  
  # Sequence of basis functions to be used defined by:
  # n_basis_from: start of sequence
  # n_basis_to: end of sequence
  # n_basis_by: step length
  
  argvals <- seq(0, 100, length.out = nrow(data_matrix))
  
  # Use lapply to return a list the length of the sequence of numbers
  # of basis functions
  # Each element of the list is the mse for each curve (each column of data matrix)
  lapply(
    X = seq(from = n_basis_from,
            to = n_basis_to,
            by = n_basis_by),
    FUN = function(y) {
      # perform smoothing:
      smooth <- fda::smooth.basis(
        argvals = argvals,
        y = data_matrix, 
        fdParobj = fda::fdPar(fdobj = fda::create.bspline.basis(
          rangeval = c(0, 100),
          nbasis = y,
          norder = 4),
          Lfdobj = 2, lambda = 0))
      
      # get predicted values:
      hat_vals <- fda::eval.fd(fdobj = smooth$fd,
                               evalarg = argvals)
      # check dimensions are same as observed:
      stopifnot(dim(hat_vals) == dim(data_matrix))
      # residuals = observed - expected:
      error <- data_matrix - hat_vals
      # return mse for each curve using apply:
      apply(error^2, MARGIN = 2, mean)
    })
}



## Test (tbc) -------------------------------------------------------------






# Get mses from data.table (wrapper function) -----------------------------

## Function ---------------------------------------------------------------

# Wrap around the previous function to do smoothing
get_smoothing_mses_from_dt <- function(sub_dt,
                                       n_basis_from = 50,
                                       n_basis_to = 120,
                                       n_basis_by = 5){
  stopifnot(is.data.table(sub_dt))
  stopifnot(all(unlist(lapply(sub_dt, is.numeric))))
  
  # need to transpose data for use with fda package:
  dt_to_data_matrix <- as.matrix(t(sub_dt))
  
  # call mse function:
  get_smoothing_mses(data_matrix = dt_to_data_matrix,
                                    n_basis_from = n_basis_from,
                                    n_basis_to = n_basis_to,
                                    n_basis_by = n_basis_by)
}


## Test (tbc) -------------------------------------------------------------




# Extra - Fourier Functions -----------------------------------------------

get_smoothing_mses_fourier <- function(data_matrix,
                               n_basis_from = 50,
                               n_basis_to = 120,
                               n_basis_by = 5){
  
  # Sequence of basis functions to be used defined by:
  # n_basis_from: start of sequence
  # n_basis_to: end of sequence
  # n_basis_by: step length
  
  argvals <- seq(0, 100, length.out = nrow(data_matrix))
  
  # Use lapply to return a list the length of the sequence of numbers
  # of basis functions
  # Each element of the list is the mse for each curve (each column of data matrix)
  lapply(
    X = seq(from = n_basis_from,
            to = n_basis_to,
            by = n_basis_by),
    FUN = function(y) {
      # perform smoothing:
      smooth <- fda::smooth.basis(
        argvals = argvals, y = data_matrix, 
        fdParobj = fda::create.fourier.basis(
          rangeval = c(0, 100),
          nbasis = y))
      
      # get predicted values:
      hat_vals <- fda::eval.fd(fdobj = smooth$fd,
                               evalarg = argvals)
      # check dimensions are same as observed:
      stopifnot(dim(hat_vals) == dim(data_matrix))
      # residuals = observed - expected:
      error <- data_matrix - hat_vals
      # return mse for each curve using apply:
      apply(error^2, MARGIN = 2, mean)
    })
}


# Wrap around the previous function to do smoothing
get_smoothing_mses_fourier_from_dt <- function(sub_dt,
                                       n_basis_from = 50,
                                       n_basis_to = 120,
                                       n_basis_by = 5){
  stopifnot(is.data.table(sub_dt))
  stopifnot(all(unlist(lapply(sub_dt, is.numeric))))
  
  # need to transpose data for use with fda package:
  dt_to_data_matrix <- as.matrix(t(sub_dt))
  
  # call mse function:
  get_smoothing_mses_fourier(data_matrix = dt_to_data_matrix,
                     n_basis_from = n_basis_from,
                     n_basis_to = n_basis_to,
                     n_basis_by = n_basis_by)
}
