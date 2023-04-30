# Relevancy Function V2 ------------------
data = host
tool_relevancy <- read_excel("input/tool_relevancy_rules/Host_tool_relevancies.xlsx")
# #test values
# question="F10"
# data$F7[11467] <- 0
# data$F7[12800] <- 0
# question="B1_other"
# question="F35"
# question="D5_other"
# question="H8_other"
# question="C1"
# #

check_relevancy_rules <- function(data, tool_relevancy){
  # initiate Log
  relevancy_log <- data.frame()
  
  # Loop through relevancy rules
  questions <- unique(tool_relevancy$name)
  for(question in questions){
    
    relevancy_sub <- tool_relevancy[tool_relevancy$name == question,]
    
    # Negate Conditional string
    conditional_string <- relevancy_sub$Rcondition %>% paste0("!(", ., ")") #Negate
    
    #### Dataset Checks
    # Rows where main question is not null
    main_q_logical <- !is.na(data[[question]]) 
    # Rows where relevancy rules does not apply
    relevant_q_logical <- eval(parse(text=conditional_string))
    # Flagged rows
    flagged_rows <- which(main_q_logical & relevant_q_logical)
    
    # Log if rows are flagged
    len_flagged <- length(flagged_rows)
    if(len_flagged > 0){
      # Get the values of relevant questions
      relevant_values <- data[flagged_rows, c("KEY",str_split(relevancy_sub$relevant_question, " - ")[[1]])] %>%
        pivot_longer(-KEY, names_to = "cols", values_to = "value") %>% 
        group_by(KEY) %>% 
        summarize(total = paste0(value, collapse = " - ")) %>% ungroup() %>% pull(total)
      
      log <- data.frame(KEY=data$KEY[flagged_rows],
                        question=rep(question, len_flagged),
                        value=data[[question]][flagged_rows],
                        relevancy_rule=rep(relevancy_sub$relevance_rule[1], len_flagged),
                        relevant_question=rep(paste0(relevancy_sub$relevant_question, collapse = " - "), len_flagged),
                        relev_value=relevant_values)
      relevancy_log <- rbind(relevancy_log, log)
    }
  }
  
  return(relevancy_log)
}



##
writexl::write_xlsx(tool_relevancy, "test.xlsx")

#test
# if(nrow(relevancy_sub) > 1){
#   # Note: update this to work if logical operators are different
#   conditional_string <- paste0(relevancy_sub$Rcondition, collapse = relevancy_sub$logical_opr[1]) %>% 
#     paste0("!(", ., ")") #Negate
# } else {
#   
# }

# Relevancy Function V1 ------------------------------------------------------------------
#NOTE: see if u can use all() any() for logical checks
`%notin%` <- Negate(`%in%`)

multi_val <- function(dt_relev_q_val, relev_var_val, split_opr, relev_var, split_val, split_log_opr){
  split_val <- str_split(relev_var_val, " - ", simplify = T)
  # #test
  # val_i=1
  # val_i=2
  # val_i=3
  # val_i=4
  # val_i=5
  # ###
  
  final_res <- NULL
  prev_res <- NULL
  for(val_i in 1:length(split_val)){
    curr_res <- match.fun(split_opr[val_i])(dt_relev_q_val, split_val[val_i])
    
    if(val_i != 1){
      final_res <- match.fun(split_log_opr[val_i-1])(prev_res, curr_res)
      prev_res <- final_res
    } else {
      prev_res <- curr_res
    }
  }
  
  return(final_res)
}

#test vals
test_questions <- c()

data_rows <- c()
for(q in test_questions){
  data_rows <- c(data_rows, which(!is.na(data[[q]]))[1])
}

#test 1
# dummy data 
# data_backup = data
data = data_backup

#change relevant question value
#test1
#2
data$answered_response[1] <- 2


# loop test vals
tool_sub <- tool_relevancy %>% 
  filter(name %in% test_questions[7])
# data <- data[183,]
i=1
j=1
sheet="main"
#
#Function 2
# start.time <- Sys.time()
tool_sub <- tool_relevancy %>% 
  filter(name %in% names(data))

relevancy_log <- data.frame(KEY=NA,question=NA,value=NA,relevancy=NA,relevant_question=NA,
                            relev_value=NA)
#loop through rows and tool
for(i in 1:nrow(data)){
  
  prev_log_res <- NA
  final_match <- NA
  flagged_val <- c()
  flagged_var <- c()
  for(j in 1:nrow(tool_sub)){
    var_name <- tool_sub$name[j]
    
    data_q_val <- data[[var_name]][i]
    
    if(!is.na(data_q_val)){
      
      split_opr <- str_split(tool_sub$operator[j], " - ", simplify = T)
      # log_opr <- tool_sub$logical_opr[j]
      split_log_opr <- str_split(tool_sub$logical_opr[j], " - ", simplify = T)
      # opr_order <- tool_sub$operator_order[j]
      relev_var <- tool_sub$q[j]
      relev_var_val <- tool_sub$val[j]
      rep <- tool_sub$rep[j]
      last_rep <- tool_sub$last_rep[j]
      #Data relevant question value
      dt_relev_q_val <- data[[relev_var]][i]
      if(is.na(dt_relev_q_val)){
        dt_relev_q_val <- ""
      }
      
      #Using grepl for "Selected()" relevancies 
      if(split_opr[1] == "selected"){
        # In case there are many values
        split_val <- str_replace_all(relev_var_val, " - ", "|")
        curr_log_res <- grepl(split_val, dt_relev_q_val)
        
      } else if(grepl(" - ", relev_var_val)){
        
        curr_log_res <- multi_val(dt_relev_q_val, relev_var_val, split_opr, relev_var, split_val, split_log_opr)
        
      } else if(grepl("\\$\\{", relev_var_val)){
        
        relev_var_val <- str_extract(relev_var_val, "(?<=\\$\\{)(.*?)(?=\\})")
        curr_log_res <- match.fun(split_opr[1])(dt_relev_q_val, data[[relev_var_val]][i])
        
      } else {
        curr_log_res <- match.fun(split_opr[1])(dt_relev_q_val, relev_var_val)
      }
      #For questions that has more than one relevancy
      if(rep > 1){
        # for each repition, keep checking previous and current log result
        final_match <- match.fun(split_log_opr[1])(prev_log_res, curr_log_res)
      } else {
        final_match <- curr_log_res
      }
      #record the current flagged val
      if(!curr_log_res){
        flagged_var <- c(flagged_var, relev_var)
        flagged_val <- c(flagged_val, dt_relev_q_val)
      }
      
      # log if the final check is False
      if(last_rep & !final_match){
        log <- c(data$KEY[i], var_name, data_q_val, tool_sub$relevance[j], 
                 paste0(flagged_var, collapse = " - "), paste0(flagged_val, collapse = " - "))
        relevancy_log <- rbind(relevancy_log, log)
      } 
      #storing Current Final Logical Result for next check
      prev_log_res <- final_match
      #reset flagged values
      if(last_rep){
        flagged_val <- c()
        flagged_var <- c()
      }
    }
  }
}
relevancy_log <- relevancy_log[-1,]

if(nrow(relevancy_log) == 0){
  message("There aren't any relavancy issues!")
} else {
  message("Relevancy issues found in data!")
}
# end.time <- Sys.time()
# time.taken <- end.time - start.time
# print(time.taken)


