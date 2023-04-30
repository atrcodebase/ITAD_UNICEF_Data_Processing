# clean the cleaning log -----------------------------------------------------------------
options(scipen = 999)
#Filter empty rows
translation_log_filtered <- translation_log %>% 
  filter(!is.na(New_Value))

#identify issues
translation_log_filtered <- translation_log_filtered %>% 
  mutate(issue = case_when(
    Question %notin% names(pdm_data) ~ "question",
    `_id` %notin% pdm_data$`_id` ~ "Id"))

translation_log_filtered$duplicates <- duplicated(translation_log_filtered[, c("_id", "Question")], fromLast = T) | 
  duplicated(translation_log_filtered[, c("_id", "Question")])

translation_log_issues <- translation_log_filtered %>% 
  filter(!is.na(issue) | duplicates == TRUE) %>% 
  arrange(`_id`, Question)

translation_log_filtered <- translation_log_filtered %>% 
  filter(is.na(issue))

# apply the translation-log -------------------------------------------
pdm_data_copy <- pdm_data
pdm_data <- apply_log(data = pdm_data, log = translation_log_filtered, data_KEY = "_id",
                      log_columns = c(question = "Question",
                                      old_value = "Old_Value",
                                      new_value = "New_Value",
                                      KEY = "_id"))

# Verify translation log -------------------------------------------
message("Verifying Translation log, please wait!")
translation_log_discrep <- compare_dt(df1 = pdm_data_copy, df2 = pdm_data, unique_id_df1 = "_id", unique_id_df2 = "_id")

# Removing extra spaces from new_value before joining 
translation_log_discrep <- translation_log_discrep %>%
  mutate(Id = as.character(KEY), KEY = NULL) %>% 
  anti_join(translation_log_filtered %>% 
              mutate(New_Value = str_squish(New_Value)),
            by=c("Id"="_id", "question"="Question", "new_value"="New_Value"))

# remove extra objects -------------------------------------------
rm(pdm_data_copy, translation_log_filtered)
