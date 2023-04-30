# Remove Pilot Data ----------------------------------------------------------------------
#Data collection start date
dt_collection_start <- "2022-09-07"

host_community_df <- host_community_df %>% filter(submission_date >= dt_collection_start)
idp_df <- idp_df %>% filter(submission_date >= dt_collection_start)
perception_df <- perception_df %>% filter(submission_date >= dt_collection_start)
returnee_df <- returnee_df %>% filter(submission_date >= dt_collection_start)


# Filter Rejected Keys -------------------------------------------------------------------
## IDP
idp_df <- idp_df %>% 
  filter(qa_status %notin% removed_qa_status)

## Returnee
returnee_df <- returnee_df %>% 
  filter(qa_status %notin% removed_qa_status)

## Perception Survey
perception_df <- perception_df %>% 
  filter(qa_status %notin% removed_qa_status)

## Host Community
host_community_df <- host_community_df %>% 
  filter(qa_status %notin% removed_qa_status)


rm(removed_qa_status, dt_collection_start)
