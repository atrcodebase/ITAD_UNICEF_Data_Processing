##### Data Processing Script #####
# Install/load required packages -----------------------------------------------------------------
if(!require(devtools)) install.packages("devtools")
if(!require(atRfunctions)) install_github("atrcodebase/atRfunctions")
if(!require(stringr)) install.packages("stringr")
if(!require(readxl)) install.packages("readxl")
if(!require(glue)) install.packages("glue")
if(!require(googlesheets4)) install.packages("googlesheets4")
if(!require(dplyr)) install.packages("dplyr")
if(!require(tidyr)) install.packages("tidyr")
if(!require(lubridate)) install.packages("lubridate")
if(!require(writexl)) install.packages("writexl")
source("R/custom_functions.R")
`%notin%` <- Negate(`%in%`)

# Read data ------------------------------------------------------------------------------
data_path <- "input/raw_data/" # data path
files <- list.files(data_path, pattern = "Stakeholder_Survey_Extenders")
convert_to_na <- c("NA", "N/A", "-", " ") # values to convert to NA
itad_df <- read_excel(paste0(data_path, files), guess_max = 100000, na = convert_to_na)
# Relevancy Rule
itad_tool_relevancy <- read_excel("input/tool_relevancy_rules/ITAD_UNICEF_tool_relevancies.xlsx")

janitor::get_dupes(itad_df, "_id")

# read qa-log, correction log, and translation log -------------------------------------------
url <- "https://docs.google.com/spreadsheets/d/e/2PACX-1vQn-Hwp0ajJvePotGTuXZjbjHpU-ZofGx3HxBZmz1NtV1BduXy6qG8kbr26DfVGlPZICzEhESZ2FK6o/pub?"
qa_log <- readr::read_csv(paste0(url, "gid=2034842709&single=true&output=csv"), col_types = "c")
correction_log <- readr::read_csv(paste0(url, "gid=289655547&single=true&output=csv"), col_types = "c")

correction_log <- correction_log %>% mutate(`New value` = str_squish(`New value`))

# Update Select_multiple series columns ------------------------------------------------------------
multi_vars <- c("Province", "District", "programmer_u_work", "please_specify_the_sector")
itad_df <- update_series_cols(itad_df, multi_vars=multi_vars,"/") 

# Relevancy check ----------------------------------------------------------------------------------
# Check Relevancy Rules
itad_issues <- check_relevancy_rules(itad_df, itad_tool_relevancy)
# Check Select_Multiple Questions
itad_SM_issues = check_select_multiple(itad_df, multi_vars, separator="/", KEY="_id")
#check
itad_issues
itad_SM_issues

# attach value labels ------------------------------------------
tool_path <- "input/tools/ITAD_UNICEF_KOBO_tool.xlsx"
# apply the value labels
itad_df <- labeler(data = itad_df,
                             tool = tool_path,
                             survey_label = "label::English",
                             choice_lable = "label::English",
                             multi_response_sep = ";")

# apply correction log -------------------------------------------
# file.edit("R/apply_cleaning_log.R")
source("R/apply_cleaning_log.R")
if(nrow(correction_log_discrep) !=0){
  print("Correction Logs not applied -------------------")
  correction_log_discrep
}
#Note: the series column for 3 rows of please_specify_the_sector need to be updated

# recode variables -----------------------------------------------------------------------
## use this file in case if we need to modify some variables
# file.edit("R/recode.R")
source("R/recode.R")

# remove extra columns -------------------------------------------------------------------
extra_cols <- c(names(itad_df)[grepl("Province/|District/", names(itad_df))], 
                "deviceid", "phonenumber", "audit", "audit_URL", "survey", "background-audio", 
                "background-audio_URL", "Username", "Password", "Surveyor_Name", "Check_Password", 
                "Valid_Credentials", "Invalid_credentials", "Surveyor_Name_label", "Form_access", 
                "Enumerator_Name", "Resp_ID", "caseid", "call_num", "cur_call_num", "pre_call_num", 
                "last_call_status", "callback_time", "Sampled_Respondent_Name", "Email_Sample", "Len", "n1", "n2", 
                "n3", "n4", "n5", "n7", "closed", "Resp_pn", "Resp_pn0", "Consent", 
                "Reason_Not_reached_out_to_respondent", "Can_you_share_the_number", "phone_number_new_case", 
                "note_extenders", "thank_you_note", "note31", "n58", "n59", "n60", "n61", "_submission_time", 
                "_validation_status", "_notes", "_status", "_submitted_by", "__version__", "_tags",	"_index")

itad_df <- itad_df %>% select(-all_of(extra_cols))

# produce qa-backlog ---------------------------------------------------------------------
qa_log_sub <- qa_log %>% select(qa_status=`QA status`, `_id`=Kobo_id) %>% mutate(`_id`=as.integer(`_id`))
## Filter
QA_backlog_keys <- left_join(
  itad_df %>% filter(choice_ans == "Complete") %>% select(submission_date, `_id`), qa_log_sub, by = "_id") %>% 
  mutate(qa_status = case_when(
    is.na(qa_status) ~ "NA_in_qa_log",
    TRUE ~ qa_status)) %>% filter(qa_status %notin% c("Approved", "Rejected"))
QA_backlog <- QA_backlog_keys %>% 
  group_by(submission_date) %>% 
  count(qa_status, name = "freq") %>% 
  mutate(percent_Host = round((freq/sum(freq) * 100) ,2)) %>%
  ungroup() %>% 
  arrange(submission_date)
# Print
print(knitr::kable(QA_backlog, format = "simple"))

# remove Rejected keys ---------------------------------------------------------
count(qa_log, `QA status`)
itad_df <- itad_df %>% filter(qa_status %notin% "Rejected")


# generate data with missing translations ------------------------------------------------
excluded_cols <- c("Location_Name_Sample", "Post_Dari", "Post_Pashto")
missing_translation_log <- missing_translation(data = itad_df, KEY = "_id", excluded_cols)


# Filter Approved Data only --------------------------------------------------------------
itad_df_filtered <- itad_df %>% 
  filter(qa_status %in% "Approved")

# Renaming Datasets --------------------------------------------------------------------------------
var_map <- read_excel("input/Column Mapping.xlsx")
#Rename
itad_df_renamed <- itad_df_filtered %>% 
  rename_at(vars(var_map$xml), function(x) {x = var_map$english[var_map$xml == x]})

# Export ---------------------------------------------------------------------------------
# QA Backlog
qa_backlog_list <- list(
  unresolved_cases=QA_backlog,
  KEYs=QA_backlog_keys
)

## create the output path
check_path("output/cleaned_data")
## export cleaned datasets
writexl::write_xlsx(list("Stakeholder Survey_ Extenders"=itad_df), paste0("output/cleaned_data/Stakeholder_Survey_Extenders_Cleaned_", today(),".xlsx"), format_headers = F) # Cleaned
writexl::write_xlsx(list("Stakeholder Survey_ Extenders"=itad_df_filtered), paste0("output/cleaned_data/Stakeholder_Survey_Extenders_Cleaned_Approved_", today(),".xlsx"), format_headers = F) # Cleaned & Approved
writexl::write_xlsx(list("Stakeholder Survey_ Extenders"=itad_df_renamed), paste0("output/cleaned_data/Stakeholder_Survey_Extenders_Cleaned_Approved_English_", today(),".xlsx"), format_headers = F) # Cleaned & Approved

## keep a copy of correct & translation log, export log issues, export missing translation, etc.
writexl::write_xlsx(correction_log, "output/correction_log.xlsx", format_headers = F) # correction & translation log
writexl::write_xlsx(correction_log_issues, "output/correction_log_issues.xlsx", format_headers = F) # correction & Translation log issues
writexl::write_xlsx(qa_backlog_list, "output/QA_Backlog_by_Date.xlsx", format_headers = F)
writexl::write_xlsx(correction_log_discrep, "output/correction_log_discrep.xlsx", format_headers = F)
writexl::write_xlsx(missing_translation_log, "output/untranslated_log.xlsx", format_headers = F)

