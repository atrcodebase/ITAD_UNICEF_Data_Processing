# Install/load required packages ---------------------------------------------------------
if(!require(stringr)) install.packages("stringr")
if(!require(readxl)) install.packages("readxl")
if(!require(dplyr)) install.packages("dplyr")
if(!require(glue)) install.packages("glue")
if(!require(writexl)) install.packages("writexl")
if(!require(tidyr)) install.packages("tidyr")
source("R/custom_functions.R")
`%notin%` <- Negate(`%in%`)

# Read data ------------------------------------------------------------------------------
convert_to_na <- c("NA", "N/A", "-", " ") # values to convert to NA
# Unlabeled data
# idp <- read_excel("output/unlabeled_data/UNHCR_IDPs_cleaned.xlsx", guess_max = 100000, na = convert_to_na) %>% mutate(KEY=`_uuid`)

# Tool Relevance
pdm_tool_relevancy <- read_excel("input/tool_relevancy_rules/PDM_tool_relevancies.xlsx")

# Check Relevancy Rules ------------------------------------------------------------------
pdm_issues <- check_relevancy_rules(pdm_data, pdm_tool_relevancy)

# Check Select_Multiple Questions --------------------------------------------------------
pdm_SM_issues = check_select_multiple(pdm_data, multi_vars, separator="/", KEY="_id")

# Export ----------------------------------------------------------------------------------
#Relevancy
export_list <- list(
  IDP=idp_issues
  # IDP_SM=idp_SM_issues,
  # Returnee=returnee_issues,
  # # Returnee_SM=ret_SM_issues,
  # Host_Community=host_issues
  # Host_Community_SM=host_SM_issues
  # Perception=perception_issues
)
#Selet Multiple
export_list2 <- list(
  # IDP=idp_SM_issues,
  Returnee=returnee_issues,
  "Host Community"=ret_SM_issues
  # Perception=perception_SM_issues
)

writexl::write_xlsx(export_list, "output/Tool_relevancy_issues.xlsx")
writexl::write_xlsx(export_list2, "output/Tool_select_multiple_issues.xlsx")


#test -----------------
# for(question in multi_vars){
#   print(paste0("Updating: ", question))
#   series_cols <- names(data)[grepl(question, names(data))]
#   rows <- which(!is.na(data[[question]]))
#   
#   for(row_i in rows){
#     data[[question]][row_i]
#     
#     #question value
#     val <- str_split(data[[question]][row_i], " |-")[[1]]
#     #make related series column name
#     series_columns <- paste0(question,separator, val) 
#     other_columns <- series_cols[series_cols %notin% c(series_columns, question) & !grepl("_other", series_cols)]
#     
#     #assign
#     data[[series_columns]][row_i] <- "1"
#     data[row_i, other_columns] <- "0"
#     
#   }}
