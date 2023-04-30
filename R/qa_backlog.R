# QA Backlog
qa_log_sub <- qa_log %>% 
  select(qa_status=`QA status`, `_id`)

## Host Community
QA_backlog_keys <- left_join(
  pdm_data %>% filter(choice_ans == "Complete") %>% select(submission_date, `_id`),
  qa_log_sub,
  by = "_id"
) %>% 
  mutate(
    qa_status = case_when(
      is.na(qa_status) ~ "NA_in_qa_log",
      TRUE ~ qa_status
    )
  ) %>% 
  filter(qa_status %notin% QAed_status)

QA_backlog <- QA_backlog_keys %>% 
  group_by(submission_date) %>% 
  count(qa_status, name = "freq") %>% 
  mutate(percent_Host = round((freq/sum(freq) * 100) ,2)) %>%
  ungroup() %>% 
  arrange(submission_date)


cat("Unresovled Cases either Pending or NA in the QA log\n")
cat("Only displays the dates where there is still QA Backlog:")
print(knitr::kable(QA_backlog, format = "simple"))
cat("For details, please check QA_Backlog_by_Date.xlsx in output folder")

# Export
export_list <- list(
  unresolved_cases=QA_backlog,
  KEYs=QA_backlog_keys
)

writexl::write_xlsx(export_list, "output/QA_Backlog_by_Date.xlsx", format_headers = F)

rm(host_com_QA_backlog, idp_QA_backlog, returnee_QA_backlog, perception_df_QA_backlog, 
   host_com_QA_backlog_keys, idp_QA_backlog_keys, returnee_QA_backlog_keys, perception_df_QA_backlog_keys, 
   unresolved_cases_summary, QAed_status, qa_log_sub)
