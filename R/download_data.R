# Install/load required packages ---------------------------------------------------------
if(!require(KoboconnectR)) install_github("asitav-sen/KoboconnectR")
if(!require(httr)) install.packages("httr")
if(!require(stringr)) install.packages("stringr")

# Credentials ----------------------------------------------------------------------------
# usethis::edit_r_environ()
pass = Sys.getenv("UNHCR_KOBO_PASS")
usr = Sys.getenv("UNHCR_KOBO_USR")

# Get Data info from Kobo ----------------------------------------------------------------
asset_list <- kobotools_api(url="kobo.humanitarianresponse.info", simplified=T, uname=usr, pwd=pass)
PDM_id <- asset_list$asset[asset_list$name == "Post Distribution Monitoring for GIHA Cash Project"]

# Create Export Files --------------------------------------------------------------------
PDM_data <- kobo_export_create(url="kobo.humanitarianresponse.info", uname= usr, pwd = pass,
                                     assetid=PDM_id, type= "xls", all="false", lang = "_xml",
                                     hierarchy="false", include_grp="false", sleep = 10)

# Download  ------------------------------------------------------------------------------
print("Downloading Dataset:")
# Get Data
PDM_df <- GET(PDM_data, authenticate(user = usr, password = pass), progress())

# Export ---------------------------------------------------------------------------------
# Save Data
writeBin(content(PDM_data, "raw"), "input/raw_data/Post_Distribution_Monitoring_for_GIHA_Cash_Project.xlsx")
writeBin(content(PDM_data, "raw"), paste0("input/raw_data/backup/", str_split(PDM_data, "exports/")[[1]][2])) # Backup


# remove extra objects -------------------------------------------------------------------
rm(usr, pass, asset_list, PDM_id, PDM_data)

