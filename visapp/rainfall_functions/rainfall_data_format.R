# Emily Gebhardt
# AVA dataset

# inputdata <- adam_dat[[1]]
# aperiod <- input$rainfall_aperiod_adam_1
# param <- input$rainfall_param_adam_1
# flag_choices <- input$rainfall_flags_adam_1
# visit_num <- input$time_selections
# trt_arm <- input$rainfall_trtarm_adam_1
# dtype <- input$rainfall_dtype_adam_1
# x <- rainfall_data_format(inputdata, aperiod, param, flag_choices, visit_num, trt_arm, dtype)


# This program creates the dataset that is used in the AVA rainfall plots
rainfall_data_format <- function(inputdata, aperiod, param, flag_choices, visit_num, trt_arm, dtype){
  
  library(tidyverse)
  
  req(inputdata)
  req(visit_num)
  req(trt_arm)
  
  # flags <- paste(flag_choices, collapse = ' & ')
  
  if(length(flag_choices) > 1){
    condition <- " == 'Y' & "
    
    flag_list <- paste(flag_choices, collapse = condition)
    
    flag_list_complete <- paste(flag_list, " == 'Y'")
  }
  
  else{
    flag_list_complete <- paste(flag_choices, " == 'Y'")
  }

  subjs <- inputdata %>% filter(PARAMCD == param & eval(parse(text=flag_list_complete)) & !is.na(AVAL) & ABLFL=='Y')
  subjs1 <- unique(subjs$USUBJID)

  data2 <- inputdata %>% filter(PARAMCD == param & eval(parse(text=flag_list_complete)) & 
                             !is.na(AVAL) & (ABLFL=='Y' | APERIOD %in% aperiod) & USUBJID %in% subjs1 &
                             DTYPE %in% c(dtype,NA,""))

  # detach(data)
  # attach(data2)
  
  data2 <- data2 %>% filter(AVISIT %in% visit_num)

  data2$week <- ifelse(data2$ABLFL=='Y', 0, sub("\\(.* ", "",sub(".*\\k ", "", data2$AVISIT))) %>% as.numeric()
  
  data2$sday <- data2$week*7+1

  data2$pimp <- as.numeric(((data2$BASE - data2$AVAL)/data2$BASE)*100)
  
  colnames(data2)[colnames(data2)==trt_arm] <- "treatment"
  
  
  # final <- data2 %>% select(USUBJID, AVISITN, AVAL, BASE, PARAM, PARAMCD, trtp, 
  #                           ITTFL, ABLFL, APERIOD, ANL01FL, AVISIT, week, sday, pimp,
  #                           DTYPE)
  final <- data2 %>% select(USUBJID, AVISITN, AVAL, BASE, PARAM, PARAMCD, treatment, 
                            ITTFL, ABLFL, AVISIT, week, sday, pimp,
                            DTYPE)
  
  final2 <- arrange(final, treatment, USUBJID, AVISITN)
  # final2 <- final[with(final,order(trtp,USUBJID,AVISITN))]
  final3 <- final2 %>% group_by(USUBJID,AVISITN) %>% filter(row_number(AVAL)==1)
  
  
  # detach(data2)
  
  return(final3)

}
