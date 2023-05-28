# Load ggplot2 package
library(ggplot2)    # CRAN v3.4.0    
library(data.table) # CRAN v1.14.2

# Read in injury data -----------------------------------------------------
# and read in the injury data:
injury_rds_path <- here::here(
  "outputs",
  "data",
  "cleaned-injury-and-demographics-data.RDS"
)

injury_data <- readRDS(injury_rds_path)

risc1_dt <- fread(file = here::here("outputs",
                                    "data",
                                    "risc1_intermediate_copy_01.csv"))
screen_index <- fread(file = here::here("outputs",
                                    "data",
                                    "screen_index_intermediate_copy_01.csv"))

# Data Wrangling ----------------------------------------------------------

                    # create a data set that counts a subject's total strides
risc1_strides_dt <- risc1_dt[,
                             .(total_strides = uniqueN(trial_id)),
                             by = subject_id]

screen_index_strides_dt <- screen_index[
  observation %in% risc1_dt[, unique(trial_id)],
  .(n_remove_strides = uniqueN(observation)),
  by = subject_id]

# Left Join
missing_strides_dt <- merge(
  x = risc1_strides_dt,
  y = screen_index_strides_dt,
  all.x = T)

# All the mssing values from left join are actually 0's
missing_strides_dt[is.na(n_remove_strides),
                   n_remove_strides := 0]


# Join with injury data
injury_and_strides_dt <- missing_strides_dt[
  injury_data,
  on = .(subject_id),
  nomatch = 0]


injury_and_strides_dt[
  , prop_of_strides_removed := n_remove_strides / total_strides]

saveRDS(object = injury_and_strides_dt,
        file = here::here(
          "outputs",
          "full-data",
          "RDS-objects",
          "injury_and_strides_dt.RDS"
        ))

# Plotting ----------------------------------------------------------------
# View(injury_and_strides_dt)

## Age --------------------------------------------------------------------

ggplot(data = injury_and_strides_dt) +
  aes(x = age, y = prop_of_strides_removed) +
  geom_point()

## Sex --------------------------------------------------------------------

ggplot(data = injury_and_strides_dt) +
  aes(x = sex,
      y = prop_of_strides_removed) +
  geom_violin()

ggplot(data = injury_and_strides_dt) +
  aes(x = sex,
      y = prop_of_strides_removed) +
  geom_boxplot()

ggplot(data = injury_and_strides_dt) +
  aes(x = prop_of_strides_removed) +
  geom_histogram() +
  facet_wrap(~ sex)



## Weight -----------------------------------------------------------------

ggplot(data = injury_and_strides_dt) +
  aes(x = weight_kg, y = prop_of_strides_removed) +
  geom_point() +
  geom_smooth(method = "lm")

## Height -----------------------------------------------------------------

ggplot(data = injury_and_strides_dt) +
  aes(x = height_m, y = prop_of_strides_removed) +
  geom_point()



## BMI --------------------------------------------------------------------

ggplot(data = injury_and_strides_dt) +
  aes(x = bmi_kgm, y = prop_of_strides_removed) +
  geom_point()



## Treadmill --------------------------------------------------------------


ggplot(data = injury_and_strides_dt) +
  aes(x = treadmill,
      fill = treadmill,
      y = prop_of_strides_removed) +
  geom_boxplot()

ggplot(data = injury_and_strides_dt) +
  aes(
    x = prop_of_strides_removed,
    group = treadmill,
    fill = treadmill,
    y = stat(density)
    ) +
  geom_histogram(bins = 50) +
  facet_wrap(~ treadmill) +
  theme_bw() +
  scale_y_continuous(expand = c(0, 0.5))

# Because readmill becomes of interest after looking at the plots
# we put it into table:
# (we create a summaary table)
injury_and_strides_dt[
  , proportion_missing_category := cut(prop_of_strides_removed,
                                       seq(0, 1, by = 0.1),
                                       right = FALSE)]

summary_treadmill_dt <- injury_and_strides_dt[, .N,
                                           by = .(proportion_missing_category,
                                                  treadmill)]

summary_treadmill_dt <- dcast(
  data = summary_treadmill_dt,
  value.var = "N", fill = 0,
  formula = proportion_missing_category ~ treadmill)

# and Add a total column
col_totals <- summary_treadmill_dt[, as.list(apply(.SD, MARGIN = 2, sum)),
                                .SDcols = c("old", "new")]
col_totals$proportion_missing_category <- "Total"
rbind(summary_treadmill_dt, col_totals)

## Self selected speed ----------------------------------------------------

ggplot(data = injury_and_strides_dt) +
  aes(x = self_selected_speed_kmph, y = prop_of_strides_removed) +
  geom_point() +
  geom_smooth(method = "lm")

## Injury -----------------------------------------------------------------

ggplot(data = injury_and_strides_dt) +
  aes(x = prospectively_injured_12_mths,
      fill = prospectively_injured_12_mths,
      y = prop_of_strides_removed) +
  geom_violin() +
  theme(legend.position = "none")

ggplot(data = injury_and_strides_dt) +
  aes(x = prospectively_injured_12_mths,
      fill = prospectively_injured_12_mths,
      y = prop_of_strides_removed) +
  geom_boxplot()

ggplot(data = injury_and_strides_dt) +
  aes(
    x = prop_of_strides_removed,
    group = prospectively_injured_12_mths,
    fill = prospectively_injured_12_mths,
    y = stat(density)
  ) +
  geom_histogram(bins = 50) +
  facet_wrap(~ prospectively_injured_12_mths) +
  theme_bw() +
  scale_y_continuous(expand = c(0, 0.5)) +
  theme(legend.position = "none")


# Because injury is of interest we put it into table:
# (we create a summaary table)
injury_and_strides_dt[
  , proportion_missing_category := cut(prop_of_strides_removed,
                                       seq(0, 1, by = 0.1),
                                       right = FALSE)]

summary_injury_dt <- injury_and_strides_dt[
  , .N,
  by = .(proportion_missing_category,
         prospectively_injured_12_mths)]

summary_injury_dt <- dcast(
  data = summary_injury_dt,
  value.var = "N", fill = 0,
  formula = proportion_missing_category ~ prospectively_injured_12_mths)

# and Add a total column
col_totals <- summary_injury_dt[, as.list(apply(.SD, MARGIN = 2, sum)),
                                .SDcols = c("not_injured", "injured")]
col_totals$proportion_missing_category <- "Total"
rbind(summary_injury_dt, col_totals)




# Runner category ---------------------------------------------------------

ggplot(data = injury_and_strides_dt) +
  aes(x = runner_category_2,
      fill = runner_category_2,
      y = prop_of_strides_removed) +
  geom_violin() +
  theme(legend.position = "none")

ggplot(data = injury_and_strides_dt) +
  aes(x = runner_category_2,
      fill = runner_category_2,
      y = prop_of_strides_removed) +
  geom_boxplot()

ggplot(data = injury_and_strides_dt) +
  aes(
    x = prop_of_strides_removed,
    group = runner_category_2,
    fill = runner_category_2,
    y = stat(density)
  ) +
  geom_histogram(bins = 50) +
  facet_wrap(~ runner_category_2) +
  theme_bw() +
  scale_y_continuous(expand = c(0, 0.5)) +
  theme(legend.position = "none")
# note that there is only N = 18 in Novice group!

