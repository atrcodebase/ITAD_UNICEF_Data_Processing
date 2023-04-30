### Data Analysis
# Load required packages -----------------------------------------------------------------
if(!require(tidyverse)) install.packages("tidyverse")
if(!require(readxl)) install.packages("readxl")
if(!require(writexl)) install.packages("writexl")
if(!require(glue)) install.packages("glue")
source("R/functions/Analysis_double_disagg.R") # custom function for anlaysis
`%notin%` <- Negate(`%in%`)

# Read data ------------------------------------------------------------------------------
data_path <- "output/cleaned_data/Post_Distribution_Monitoring_for_GIHA_Cash_Project_Cleaned_Approved.xlsx" # data path
convert_to_na <- c("NA", "N/A", "-", " ", 999) # values to convert to NA
pdm_data <- read_excel(data_path, guess_max = 100000, na = convert_to_na)

# Read analysis plans --------------------------------------------------------------------
pdm_ap <- readxl::read_excel("input/analysis_plan/Analysis Plan_PDM - Copy.xlsx")

# Check question names in AP with question/column names in data
## custom function
check_ap_questions_with_data_columns <- function(ap, dt) {
  unmatched_questions <- ap$variable[ap$variable %notin% names(dt)]
  
  if (length(unmatched_questions) == 0) {
    print("All questions in DAP match with questions/columns in data")
  } else {
    print("----Below questions in DAP do not match with questions/column in data")
    print(unmatched_questions)
  }
}

## check
check_ap_questions_with_data_columns(ap = pdm_ap, dt = pdm_data)

# Analysis -------------------------------------------------------------------------------
## PDM
pdm_analysis <- analysis_func(df = pdm_data, ap = pdm_ap, multi_response_sep = ";") %>% 
  arrange(Disaggregation) %>% 
  filter(Denominator != 0)

# export results -------------------------------------------------------------------------
check_path("output/analysis")

writexl::write_xlsx(pdm_analysis, glue::glue("output/analysis/UN_Women_PDM_Analysis.xlsx"), format_headers = F) # PDM
writexl::write_xlsx(pdm_analysis, glue::glue("output/analysis/Time_reached_distribution_area_&_vehicle.xlsx"), format_headers = F) # PDM

