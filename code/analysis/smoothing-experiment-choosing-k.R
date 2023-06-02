# ------------------------------------------------------------------------#
# This script performs the experiment in Chapter 3.3.2 of the Thesis
# to choose the number of cubic B-Spline basis functions to represent
# the data.
# ------------------------------------------------------------------------#


# Packages: ---------------------------------------------------------------
library(data.table) # CRAN v1.14.2
library(ggplot2)    # CRAN v3.4.0 # CRAN v3.4.0



# Source necessary files --------------------------------------------------
source(file = here::here("code",
                         "functions",
                         "functions-helper-smoothing.R"))
source(file = here::here("code",
                         "functions",
                         "functions-get-mse.R"))


# Read in data: -----------------------------------------------------------
risc1_dt <- fread(file = here::here("outputs",
                                    "data",
                                    "risc1_intermediate_copy_01.csv"))
screen_index <- readRDS(file = here::here("outputs",
                                        "data",
                                        "screen_index_intermediate_copy_01.rds"))



# Set random seed ---------------------------------------------------------
set.seed(1996)

# Generate a data set of unique strides to sample from --------------------
sample_dt <- risc1_dt[
  !screen_index, # (make sure these strides are not in screen index by 
  on = .(trial_id = observation)] #, # using an anti-join)
sample_dt <- sample_dt[quantity == "Angles"]

# Smooth this data sample using different number of basis functions -------
# and calculate MSEs 

# first, count number of non-na data values (i.e. curve length)
# (can do this in blocks of strides using by argument
# as all curves from same stride will be same length - speed up!)
sample_dt[, non_na_vals := count_non_na_values(.SD[1, ]),
          .SDcols = data_0:data_197,
          by = .(subject_id,
                 side,
                 stride_num)]

# Use custom functions to calculate mse for different number of basis 
mses_calc_dt <- sample_dt[,
                   paste0("nbasis", seq(from = 50, to = 115, by = 5)) :=
                     get_smoothing_mses_from_dt(
                       sub_dt = .SD[, 1:non_na_vals[1]],
                       n_basis_from = 50,
                       n_basis_to = 115,
                       n_basis_by = 5),
                   .SDcols = data_0:data_197,
                          by = .(subject_id,
                                 side,
                                 stride_num,
                                 non_na_vals)]

# gather data to explore results ------------------------------------------

                    # create vector containing names of columns to to work with 
col_names_select <- c("subject_id",
                      "side",
                      "stride_num",
                      "plane_of_motion",
                      "quantity",
                      "location",
                      "non_na_vals",
                      paste0("nbasis", seq(from = 50, to = 115, by = 5)))

                    # create a long data set for plotting with ggplot:
plot_mses_dt <- melt(data = mses_calc_dt[, ..col_names_select],
                      id.vars = c("subject_id",
                                  "side",
                                  "stride_num",
                                  "plane_of_motion",
                                  "quantity",
                                  "location",
                                  "non_na_vals"), 
                      variable.name = "n_basis",
                      value.name = "mse", 
                      variable.factor = FALSE)[,
                                               n_basis := as.numeric(
                                                 stringr::str_remove(
                                                   n_basis, "nbasis"))
                                               ]

# create rmse variable:
plot_mses_dt[, rmse := sqrt(mse)]

fwrite(x = plot_mses_dt, file = here::here("outputs", "data", "plot_mses_dt_choosing_k.csv"))

# Plots -------------------------------------------------------------------


# Package for plotting ----------------------------------------------------
# + settings
library(ggplot2)    # CRAN v3.4.0 # CRAN v3.4.0
theme_set(new = theme_bw()) # nice theme
options(scipen = 999) # no scientific notation on axes

ggplot(data = plot_mses_dt[quantity == "Angles" & location %in% c("Hip", "Knee", "Ankle") & !(subject_id %in% c("P_4217"))]) +
  geom_vline(xintercept = c(60, 80, 100), col = "grey", lty = "dashed")+
  aes(x = n_basis,
      y = rmse,
      colour = factor(n_basis),
      group = n_basis) +
      geom_boxplot(outlier.size = 0.1) +
  facet_wrap(plane_of_motion ~ location,
             scales = "free_y",
             ncol = 3)

# "P_4213" & side == "right" & stride_num == 83?
plot_mses_dt_trimmed <- plot_mses_dt[!((subject_id == "P_4230" & side == "left" & stride_num == 63) |
                                         (subject_id == "P_4121" & side == "left" & stride_num %in% c(70, 71))|
                                         (subject_id == "P_4121" & side == "right" & stride_num %in% c(70)) |
                                         (subject_id == "P_4122" & side == "left" & stride_num == 67) |
                                         (subject_id == "P_4217" & side == "left" & stride_num %in% c(3:5, 40:41, 60)) |
                                         (subject_id == "P_4217" & side == "right" & stride_num %in% c(3:6, 40:41, 62:63)))] # |
                                         # (subject_id == "P_4217" & side == "right"))]
ggplot(data = plot_mses_dt_trimmed[quantity == "Angles" & location %in% c("Hip", "Knee", "Ankle")]) +
  geom_vline(xintercept = c(60, 80, 100), col = "grey", lty = "dashed")+
  aes(x = n_basis,
      y = rmse,
      colour = factor(n_basis),
      group = n_basis) +
  geom_vline(xintercept = 80, color = "darkgrey") +
  geom_boxplot(outlier.size = 0.1) +
  facet_wrap(plane_of_motion ~ location,
             scales = "free_y",
             ncol = 3)

plot_mses_dt_trimmed[location=="Ankle" & plane_of_motion == "fle" & quantity == "Angles" & n_basis==55][rev(order(mse))][1:10]
# weird_stride <- unlist(risc1_dt[subject_id == "P_4121" & side == "right" & stride_num == 70 & location == "Ankle" & quantity == "Angles" &
#            plane_of_motion == "fle", data_0:data_197])

weird_stride <- unlist(risc1_dt[subject_id == "P_4246" & side == "right" & stride_num == 66 & location == "Ankle" & quantity == "Angles" &
                                  plane_of_motion == "fle", data_0:data_197])

plot(weird_stride, type = "l")
