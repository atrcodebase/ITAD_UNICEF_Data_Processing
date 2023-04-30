# clean the cleaning log -----------------------------------------------------------------
options(scipen = 999)
#Filter empty rows
correction_log_filtered <- correction_log %>% 
  filter(!(is.na(Kobo_id) & is.na(Variable) & is.na(`Old value`)))

#identify issues
correction_log_filtered <- correction_log_filtered %>% 
  mutate(issue = case_when(
    Variable %notin% names(itad_df) ~ "question",
    Kobo_id %notin% itad_df$`_id` ~ "Id"
    ))

correction_log_filtered$duplicates <- duplicated(correction_log_filtered[, c("Kobo_id", "Variable")], fromLast = T) | 
  duplicated(correction_log_filtered[, c("Kobo_id", "Variable")])

correction_log_issues <- correction_log_filtered %>% 
  filter(!is.na(issue) | duplicates == TRUE) %>% 
  arrange(Kobo_id, Variable)

correction_log_filtered <- correction_log_filtered %>% 
  filter(is.na(issue))

# apply the correction-log -------------------------------------------
itad_df_copy <- itad_df
itad_df <- apply_log(data = itad_df, log = correction_log_filtered, data_KEY = "_id",
                                             log_columns = c(question = "Variable",
                                                             old_value = "Old value",
                                                             new_value = "New value",
                                                             KEY = "Kobo_id"))

# Verify correction log -------------------------------------------
message("Verifying Correction log, please wait!")
correction_log_discrep <- compare_dt(df1 = itad_df_copy, df2 = itad_df, unique_id_df1 = "_id", unique_id_df2 = "_id")

# Removing extra spaces from new_value before joining 
correction_log_discrep <- correction_log_discrep %>%
  select(Kobo_id = KEY, Variable=question, "Old value"=old_value, "New value"=new_value) %>% 
  mutate(Kobo_id = as.character(Kobo_id)) %>% 
  anti_join(correction_log_filtered, by=c("Kobo_id", "Variable", "New value")) 
            

# remove extra objects -------------------------------------------
rm(itad_df_copy, correction_log_filtered)

