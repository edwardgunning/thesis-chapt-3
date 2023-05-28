#-------------------------------------------------------------------------#
# Some discrepancies between stride numbers and frame numbers are also
# present in the screen index data set
# while Shane has said these are OK and can be ignored
# (they were a result of more than one person screening with slightly different
# conventions for numbering framews)
# it is still useful to have a record of them
#-------------------------------------------------------------------------#

# Investigate problem -----------------------------------------------------
# Shorten the observation variable to get rid of frame number
# There should be the same number of unique values of a the original
# and shortened variable

# shorten by removing frame number, creating new variable:
screen_index[, observation_fn_rem := remove_frame_num(obs = observation)]

# check number of unique values:
screen_index[, .(unique_obs = uniqueN(observation),
                 unique_obs_fn_rem = uniqueN(observation_fn_rem))]
#unique_obs unique_obs_fn_rem
#1:       2002              1984

# ... less from shortened variable investigate

# count unique longer variables per shorter variable
screen_index[, `:=`(n_unique_obs = uniqueN(observation)),
             by = observation_fn_rem]

# we are concerned when there is more than 1
screen_index_duplicate_strides <- screen_index[n_unique_obs > 1]

# lets make a table
# each row is a shortened observation name
# one column will contain a list of unique longer observation names that
# shorten to it

# the last column will return its match from the risc1 data set
duplicate_info_dt <- screen_index_duplicate_strides[,
                               .(names = list(unique(observation)),
                                 match_data = risc1_dt[
                                   remove_frame_num(trial_id)
                                   == unique(observation_fn_rem),
                                   unique(trial_id)]),
                               by = .(stride = observation_fn_rem)
                               ]
# check that at the match contained in risc1 data is in screen index
# (i.e. at least one of them is correct!)
stopifnot(duplicate_info_dt[,
  all(match_data %in% unlist(names))
])


# Write this to a csv to share with Shane.
fwrite(x = duplicate_info_dt,
       file = here::here(
         "outputs",
         "tables",
         "screen-index-frame-numbers.csv"))
# do nothing else for now!

# Delete temporary variables from screen index dataset
screen_index[, c("observation_fn_rem", "n_unique_obs") := NULL]

# Remove temporary variables from environment
rm(list = c("screen_index_duplicate_strides",
            "duplicate_info_dt"))


