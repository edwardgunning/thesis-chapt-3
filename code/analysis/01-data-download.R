# ========================================================================#
# Script to download risc1 data .csv file from Google Drive
# to machine. Uses "googledrive" package.
# ========================================================================#

# Authorize Google Drive --------------------------------------------------
googledrive::drive_auth() # opens browser for authorisation

# File information --------------------------------------------------------
risc1_file_info <- googledrive::drive_get(
  id = "1NH1iaeFJ_ptckUShUNg4r50LpD_NPllH")

str(risc1_file_info)
risc1_id <- googledrive::as_id(risc1_file_info$id)

# Download file to computer -----------------------------------------------
# (takes a long time)
# * this part can probably be changed so that it is accessed on
# computer without downloading because I have installed the 
# Google Drive app
googledrive::drive_download(file = risc1_id,
               path = here::here("data", "risc1_full_data.csv"))
