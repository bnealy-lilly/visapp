# Spider Animation Data Function

spider_data_format <- function(data, parameter, flag, col_name, keep){ 

  library(tidyverse)
  library(psych)
  library(reshape)

  req(data)
  req(parameter)
  req(flag)
  req(col_name)
  req(keep)
  
  data$AVISITN[data$AVISITN < 2] <- 2 
  data$AVISIT[data$AVISITN == 2] <- "Baseline" 

  dat <- data[which(data$PARAMCD == parameter &
                      (data[,flag] == 'Y' | data$ABLFL == 'Y') &
                      data$AVISIT %in% keep),]

  dat2 <- describeBy(dat$AVAL,group=list(dat$TRTP,dat$AVISITN), mat=TRUE)
  dat2 <- dat2[,c("group2","group1","n","mean","sd","min","max")]
  row.names(dat2) <- NULL
  
  means <- dat2[,c("group2","group1","mean")]
  colnames(means) <- c("AVISITN", "TRTP", col_name)
  avg <- means
  avg$WEEK <- as.numeric(as.character(avg$AVISITN))
  avg$WEEK_RANK <- dplyr::dense_rank(avg$WEEK)
  avg <- avg[,!(colnames(avg) %in% c("AVISITN","WEEK"))]
  colnames(avg)[colnames(avg)=="WEEK_RANK"] <- "AVISITN"
  
  treatments <- unique(avg$TRTP)
  
  pct <- list()
  
  for(i in 1:length(treatments)){
    pct[[i]] <- avg[avg$TRTP == treatments[i],]
    pct[[i]]$value_1 <- pct[[i]][,col_name][pct[[i]]$AVISITN == 1]
    pct[[i]][,paste0(col_name,"_pct")] <- 1 - ((pct[[i]]$value_1 - pct[[i]][,col_name])/pct[[i]]$value_1)
    pct[[i]][,paste0(col_name,"_pct")][pct[[i]]$AVISITN == 1] <- 1
    pct[[i]] <- pct[[i]][,!(colnames(pct[[i]]) %in% c("value_1"))]
  }
  
  pct_full <- do.call(rbind,pct)
  pct_full <- pct_full[,!(colnames(pct_full) %in% c(col_name))]
  pct_full <- sort_df(pct_full, c("AVISITN","TRTP"))
  
  pct_full$AVISITN <- as.numeric(as.character(pct_full$AVISITN))
  
  return(pct)
}


