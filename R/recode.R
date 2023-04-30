# split 'starttime' and 'endtime' columns into separate 'date' and 'time' columns; calculate time difference -------------------------------------------

# Join QA Status -------------------------------------------------------------------------
qa_log_sub <- qa_log %>% 
  select(qa_status=`QA status`, `_id`=Kobo_id) %>% 
  mutate(qa_status = case_when(
    is.na(qa_status) ~ "Pending",
    TRUE ~ qa_status), `_id`=as.integer(`_id`))

itad_df <- itad_df %>% left_join(qa_log_sub, by="_id")

# UN Women PDM Dataset ---------------------------------------------------------
itad_df <- itad_df %>% 
  mutate(id=Enumerator_ID, Enumerator_ID=Enumerator_Gender, Enumerator_Gender=id) %>% # Swapping Id and gender
  mutate(
    startdate = as.Date(start),
    enddate = as.Date(end),
    starttime = paste(hour(start), minute(start), sep = ":"),
    endtime = paste(hour(end), minute(end), sep = ":"),
    submission_date = as.Date(`_submission_time`)) %>%
  select(startdate, enddate, starttime, endtime, submission_date, everything(), -c(start, end, id))


# Remove extra objects -------------------------------------------------------------------
rm(qa_log_sub)
