## Stephanie Strakbein
## Rainfall Animation Function
# 
# 
# inputdata <- rainfall_adam[[1]]
# studyname <- input$rainfall_studyname
# end.point <- input$rainfall_endpoint
# xlab <- input$rainfall_xlab
# ylab <- input$rainfall_ylab
# outpath <- input$selected_path
# plot_title <- input$plot_title
# bar_type <- input$rainfall_bar_type
# round_override_x <- ""
# round_override_y <- ""
# clock_type <- input$rainfall_clock_type
# zoom_type <- input$rainfall_zoom_type
# trt1 <- input$rainfall_trt1
# trt1_abr <- input$rainfall_trt1_abr
# trt2 <- input$rainfall_trt2
# trt2 <- input$rainfall_trt2_abr
# 
# 
# make_it_rain(inputdata, studyname, end.point, xlab, ylab, outpath, plot_title,
#              bar_type, round_override_x, round_override_y, clock_type, zoom_type,
#              trt1, trt1_abr, trt2, trt2_abr)


make_it_rain <- function(inputdata, studyname, end.point, xlab, ylab, outpath, plot_title,
                         bar_type, round_override_x, round_override_y, clock_type, zoom_type, 
                         trt1, trt1_abr, trt2, trt2_abr){
  
  library(plyr)
  library(gridExtra)
  library(grid)
  
  ## Part 1: inputdata for scatter plot
  names(inputdata) <- tolower(names(inputdata))
  
  inputdata <- rename(inputdata, c(usubjid = "subjid")) %>%
    # subset(select=-c("aperiod", "anl01fl")) %>%
    mutate(trtp = ifelse(treatment == trt1, trt1, ifelse(treatment == trt2, trt2, 'wrong')),
           titrated = end.point,
           base = ifelse((ablfl=='Y' & is.na(base)), aval, base)) %>%
    filter(trtp == trt1 | trtp == trt2)
  
  
  ## Part 2: conduct necessary manipulation to the input inputdata 
  avisitn_base <- min(inputdata$avisitn)
  
  uweek <- unique(inputdata$week) %>% sort(decreasing=F)
  uvisit <- unique(inputdata$avisitn) %>% sort(decreasing=F)
  
  visit_to_week <- tibble(uvisit, uweek) %>%
    mutate(week_lab=paste("Week ", sprintf("%.0f", uweek), sep=''))
  
  mceiling <- function(x,unit){ 
    unit*ceiling(x/unit) 
  }
  
  mfloor <- function(x,unit){ 
    unit*floor(x/unit) 
  }
  
  mround <- function(x,unit){ 
    unit*round(x/unit) 
  }
  
  x_min_obs <- min(inputdata$base)
  y_min_obs <- min(inputdata$aval)
  range <- (max(inputdata$aval) - min(inputdata$aval))
  range_rounded <- mround(range, unit=50)
  
  ifelse(range_rounded < 5, 
         {range_rounded <- mround(range, unit=10)
         minor_unit <- range_rounded/10
         ifelse(range_rounded < 10,
                {range_rounded <- mceiling(range, unit=1)
                minor_unit <- range_rounded/10
                ifelse(range_rounded < 5, 
                       {minor_unit <- range_rounded/10}, 
                       {minor_unit <- range_rounded/5})},
                {range_rounded <- range_rounded
                minor_unit <- range_rounded/10})},
         {range_rounded <- range_rounded
         minor_unit <- range_rounded/10})
  
  x_min <- x_min_obs - (minor_unit/2)
  y_min <- y_min_obs - (minor_unit/2)
  
  percentiles <- c()
  for(i in min(inputdata$avisitn):max(inputdata$avisitn)){
    p <- ceiling(quantile(inputdata$aval[inputdata$avisitn==i], 0.90))
    percentiles <- c(percentiles, p)
  }
  
  if(zoom_type=="zoom"){
    
    if(round_override_x==""){
      round_cutoff_x <- mceiling(ceiling(quantile(inputdata$base, 0.90)), unit=minor_unit)
    } 
    else {
      round_cutoff_x <- round_override_x
    }
    
    if(round_override_y==""){
      round_cutoff_y <- mceiling(max(percentiles), unit=minor_unit)
    } 
    else {
      round_cutoff_y <- round_override_y
    }
    
    x_scale_min <- mfloor(x_min, unit=minor_unit)
    x_base_max <- (round_cutoff_x+minor_unit)
    x_axis_max <- (round_cutoff_x+(minor_unit*1.1))
    y_scale_min <- mfloor(y_min, unit=minor_unit)
    y_aval_max <- (round_cutoff_y+minor_unit)
    y_axis_max <- (round_cutoff_y+(minor_unit*1.1))
    x_scale_vec <- seq(x_scale_min, x_base_max, by=minor_unit)
    y_scale_vec <- seq(y_scale_min, y_aval_max, by=minor_unit)
    x_scale_label <- as.character(x_scale_vec)
    x_scale_label <- x_scale_label[1:(length(x_scale_label)-1)]
    x_scale_label <- c(x_scale_label, paste(">",round_cutoff_x,sep=''))
    y_scale_label <- as.character(y_scale_vec)
    y_scale_label <- y_scale_label[1:(length(y_scale_label)-1)]
    y_scale_label <- c(y_scale_label, paste(">",round_cutoff_y,sep=''))
    
    inputdata$base[inputdata$base > round_cutoff_x] <- x_base_max
    inputdata$aval[inputdata$aval > round_cutoff_y] <- y_aval_max
  } 
  else {
    round_cutoff_x <- mceiling(max(inputdata$base), unit=minor_unit)
    round_cutoff_y <- mceiling(max(inputdata$aval), unit=minor_unit)
    x_scale_min <- mfloor(x_min, unit=minor_unit)
    x_base_max <- max(inputdata$base)
    x_axis_max <- (round_cutoff_x+(minor_unit*1.1))
    y_scale_min <- mfloor(y_min, unit=minor_unit)
    y_aval_max <- max(inputdata$aval)
    y_axis_max <- (round_cutoff_y+(minor_unit*0.6))
    x_scale_vec <- seq(x_scale_min, round_cutoff_x, by=minor_unit)
    y_scale_vec <- seq(y_scale_min, round_cutoff_y, by=minor_unit)
    x_scale_label <- as.character(x_scale_vec)
    x_scale_label <- x_scale_label[1:length(x_scale_label)]
    y_scale_label <- as.character(y_scale_vec)
    y_scale_label <- y_scale_label[1:length(y_scale_label)]
  }
  
  textSizeMajor <- 32
  textSizeMinor <- 27
  
  ### add legends of the single frame;
  Response <- " "
  if(xlab=="") {
    xlab <- paste("Baseline", end.point, "Total Score")
  }
  if(ylab=="") {
    ylab <- paste(end.point, "Total Score")
  }
  if(plot_title == "") {
    plot_title <- paste(end.point, "Response")
  }
  
  ### Eliminate records with "NA" in the input data, usually the number of "NA" is low and won't provide valuable information;
  ### "NA" can come from data creation, for example, if baseline data is missing, then the corresponding % change should be "NA";
  
  row.has.na <- apply(inputdata, 1, function(x){any(is.na(x))})

  inputdata <- inputdata[!row.has.na, ] %>%
    mutate(pimp.cat = case_when(
                        pimp < 0 ~ '<0%', 
                        pimp >= 0 & pimp < 50  ~ '0% - <50%', 
                        pimp >= 50 & pimp < 75  ~ '50% - <75%',  
                        pimp >= 75 & pimp < 90 ~ '75% - <90%', 
                        pimp >= 90 & pimp < 100 ~ '90% - <100%', 
                        pimp == 100 ~ '100%', 
                        TRUE ~ 'WRONG'
                      )
    )

  
  ### factorize the levels of the variable of interest;
  flagorder1 <- c('<0%', '0% - <50%', '50% - <75%', '75% - <90%', '90% - <100%', '100%')
  inputdata$grp <- factor(inputdata$pimp.cat, levels=flagorder1)
  inputdata$grp[inputdata$sday==1] <- "0% - <50%"
  grplist <- levels(inputdata$grp)

  ### add color lists to the levels of variable of interest;
  colst1 <- paste('c("', grplist[1], '"="#FF0000", "', 
                  grplist[2], '"="#FFCE5C", "', 
                  grplist[3], '"="#a7ff42", "', 
                  grplist[4], '"="#68ff42", "', 
                  grplist[5], '"="#2CD723", "', 
                  grplist[6], '"="#006838" 
                  )', sep="") 
  
  group.colors <- eval(parse(text = colst1))
  
  colst2 <- paste('c("',grplist[1],'"="#FF0000","', 
                  grplist[2],'"="#ffb814","', 
                  grplist[3],'"="#6dcc00","',
                  grplist[4],'"="#2ad100","',     
                  grplist[5],'"="#1d8c17","', 
                  grplist[6],'"="#006838" 
                  )',sep="") 
  
  group.colors.dark <- eval(parse(text = colst2))
  
  ### input labels for different levels of variable of interest;
  labst <- paste('c("', grplist[1], '"="<0%", "', 
                 grplist[2], '"="0% - <50%", "', 
                 grplist[3], '"=      "50% - <75%", "', 
                 grplist[4], '"=      "75% - <90%", "', 
                 grplist[5], '"=      "90% - <100%", "', 
                 grplist[6], '"=      "100%"
                 )', sep="")
  
  group.labels <- eval(parse(text = labst))
  
  group.order <- c(grplist[1], grplist[2], grplist[3], grplist[4], grplist[5], grplist[6])
  
  # grpinfo <- data.frame(col = group.colors, 
  #                      darkcol = group.colors.dark, 
  #                      val = c("[-Inf, 0)",  "[0, 50)", "[50, 75)",     "[75, 90)",    "[90, 100)",   "[100, Inf)"), 
  #                      stringsAsFactors = FALSE)
  
  
  ###########################################
  ## Part 3:  create interpolated inputdata set ##
  ###########################################
  
  ### "interpolate.R" is the source function that's used to do the inputdata interpolation;
  respvars <- c("subjid", "avisitn", "aval", "grp")
  
  ## For interpolation rate chosen the interpolated points between the original 2 consecutive time points; 
  ## if rate = M, then there will be (M-1) interpolated time points between 2 consecutive time points; 
  ## we shall use this rate parameter again in part 6;
  
  r <- 28
  
  row.has.na2 <- apply(inputdata, 1, function(x){any(is.na(x))})
  inputdata.int <- inputdata[!row.has.na2, ]
  inputdata.int1 <- ddply(inputdata.int, .(subjid), function(x) interpolate(x, "avisitn", "aval", rate=r))
  inputdata.int2 <- inputdata.int1 %>% filter(!is.na(aval)) 

  ## Coerce that interpolated aval is within plausible range
  inputdata.int2 <- mutate(inputdata.int2, 
                           aval = ifelse(aval < y_min_obs, y_min_obs, aval)) %>% 
                    mutate(aval = ifelse(aval > y_aval_max, y_aval_max, aval))
  
  ## combine interpolated inputdata with observed inputdata to id interpolated obs --------------
  obsd <- inputdata[, respvars]
  obsd$interp <- 0
  alldat <- merge(inputdata.int2, obsd, by=c("subjid", "avisitn", "aval"), 
                  all.x=TRUE, all.y=TRUE)
  alldat[is.na(alldat$interp), "interp"] <- 1
  
  print("three banana")
  
  
  ## Merge subject info back in ----------------------------------------------
  subjinfo <- unique(inputdata[, c("subjid", "trtp", "base")])
  interpolated <- merge(alldat, subjinfo, by="subjid") %>%
    mutate(pimp = ((base - aval)/base)*100) %>% 
    arrange(trtp, subjid, avisitn, interp) %>% 
    group_by(subjid, avisitn) %>% 
    mutate(unq_obs = ifelse(row_number(interp)==1, "unique", "dup")) %>% 
    ungroup() %>% 
    filter(unq_obs=="unique")
  
  ## Remove false red values due to errors around 0 due to interpolation
  worsening <- filter(inputdata, aval > base) 
  worsening <- unique(worsening[, c("subjid", "trtp", "avisitn")])
  worsening <- mutate(worsening, worse = 1, next_visit = avisitn) %>% rename(prev_visit = "avisitn")
  interpolated <- mutate(interpolated, prev_visit = floor(avisitn))
  interpolated <- merge(interpolated, worsening, by = c("subjid", "trtp", "prev_visit"), all.x = T)
  interpolated <- arrange(interpolated, trtp, subjid, avisitn)
  interpolated[is.na(interpolated$worse), "worse"] <- 0
  interpolated <- mutate(interpolated, next_visit = ceiling(avisitn)) %>% rename(prev_worse = worse)
  interpolated <- merge(interpolated, worsening, by = c("subjid", "trtp", "next_visit"), all.x = T)
  interpolated <- arrange(interpolated, trtp, subjid, avisitn)
  interpolated[is.na(interpolated$worse), "worse"] <- 0
  interpolated <- select(interpolated, -prev_visit.y) %>% rename(prev_visit = prev_visit.x, next_worse = worse)
  
  
  print("four banana")
  
  
  interpolated <- mutate(interpolated, pimp = ifelse((next_worse==0 & prev_worse==0 & pimp < 0), 0, pimp)) %>%
                  mutate(grp =  case_when(
                    pimp <    0              ~ '<0%', 
                    pimp >=   0 & pimp < 50  ~ '0% - <50%', 
                    pimp >=  50 & pimp < 75  ~ '50% - <75%', 
                    pimp >=  75 & pimp < 90  ~ '75% - <90%', 
                    pimp >=  90 & pimp < 100 ~ '90% - <100%', 
                    pimp >= 100              ~ '100%', 
                    TRUE ~ 'WRONG'
                  ))  %>%
                  mutate(grp=factor(grp, levels=c('<0%', '0% - <50%', '50% - <75%', '75% - <90%', '90% - <100%', '100%'))) %>%
                  mutate(trtp=factor(trtp, levels=c(trt1, trt2)), 
                         titrated = end.point) 
  
  interpolated$grp[interpolated$avisitn==avisitn_base] <- "0% - <50%"
  
  visind <- interpolated$avisitn
  visind <- unique(visind)
  
  
  print("five banana")
  
  ## prepare scatter plot inputdata
  interpolated <- mutate(interpolated, int_vis = floor(avisitn))
  interpolated <- merge(x=interpolated, y=visit_to_week, by.x = "int_vis", by.y = "uvisit") %>% 
                  mutate(week=uweek) %>%  select(-uweek, -int_vis)
  
  ## prepare bar plot inputdata
  endpoint_freq <- group_by(interpolated, trtp, avisitn, grp) %>%
    dplyr::summarise(freq=n()) %>%
    group_by(trtp, avisitn) %>%
    mutate(pct = freq / sum(freq) * 100) %>%
    ungroup()
  

  ## set up frames 
  vecdiff <- diff(uvisit)
  vecdiff <- r*vecdiff
  tempindex1 <- c(rep(uvisit[1:(length(uvisit)-1)], vecdiff), uvisit[length(uvisit)])
  tempindex_tib <- tibble(tempindex1)
  tempindex_merge <- merge(x=tempindex_tib, y=visit_to_week, by.x = "tempindex1", by.y = "uvisit")
  tempindex_vec <- tempindex_merge$uweek
  tempindex <- factor(tempindex_vec)
  visind2 <- uvisit
  titleindex <- as.numeric(levels(tempindex)[as.integer(tempindex)])
  
  pause.frame.num <- 35
  tot.frame <-length(visind)-length(visind2) + (length(visind2)-1)*pause.frame.num + 2*pause.frame.num
  figind2 <<- rep(0, tot.frame)
  ff <-  Vectorize(function(x) formatC(x, width = 4, format = "d", flag = "0"))
  ind <<- ff(1:tot.frame)
  for (i in 1:tot.frame) {figind2[i] <<- paste("RAINFALL", ind[i], ".JPEG", sep="")}
  frame.counter <- 0
  
  
  ## Part 4: read in annotation inputdata
  sumdata_trt1 <- mutate(visit_to_week, trtp = trt1)
  sumdata_trt2 <- mutate(visit_to_week, trtp = trt2)
  sumdata1 <- rbind(sumdata_trt1, sumdata_trt2)
  sumdata1 <- mutate(sumdata1, week=factor(uweek), 
                     avisitn = uvisit, 
                     base = ifelse(bar_type=="short", (x_axis_max+x_min)/2, (x_axis_max+((x_min+x_scale_min)/2))/2), 
                     aval = round_cutoff_y, 
                     annotext = "")
  
  dummy_vis <- data.frame(avisitn_interp=visind) %>%
    mutate(avisitn=floor(avisitn_interp))
  
  sumdata2 <- left_join(dummy_vis, sumdata1, by=c("avisitn")) # %>%
  # mutate(annotext=ifelse(avisitn_interp==floor(avisitn_interp), annotext, ' '))
  
  ##  Part 5: basic functions to create single frame
  
  ## Base scatter plot function 
  makescatter <- function(framedata, xvn, yvn, grpvn="grp", tvn="week", 
                          shpvc = "titrated", annodata, trtgrp){
    framedata <- filter(framedata, trtp==trtgrp)
    
    if(trtgrp==trt1){
      num <- length(inputdata$subjid[which(inputdata$avisitn==2 & inputdata$trtp==trt1)])
    }
    if(trtgrp==trt2){
      num <- length(inputdata$subjid[which(inputdata$avisitn==2 & inputdata$trtp==trt2)])
    }
    
    annodata <- filter(annodata, trtp==trtgrp)
    
    if(!(exists("group.colors") & exists("group.order") & exists("group.labels"))) {
      stop("At least one group.colors/labels/order does not exist\n")
    }
    if(!exists("xlab")) {xlab = xvn}
    if(!exists("ylab")) {ylab = yvn}
    
    #build initial plot statement from x, y & grouping variable names
    basest <- paste("basechart <- ggplot(framedata, 
                    aes(x=", xvn, ", y=", yvn, ", fill=", grpvn,  ", shape=", shpvc, ")) +
                    coord_cartesian(xlim = c(", x_min, ", ", x_axis_max,"), ylim = c(", y_min, ", ", y_axis_max, "))+ 
                    labs(x='', y='')" , sep="")
    
    basechart <- eval(parse(text=basest))
    
    rm(basest)
    
    # set seed to create the first K duplicate frames
    set.seed(123)
    
    #create plot with inputdata, time var etc...
    
    img <- basechart +
      
      geom_jitter(size=3, alpha=0.6, colour="black")+
      
      ggtitle(paste(trtgrp, ' (N=', num, ')', sep = "")) +
      
      geom_segment(x = 0, y = 0, xend = round_cutoff_x, yend = round_cutoff_x, colour = grpinfo$col[2]) +
      geom_segment(x = 0, y = 0, xend = round_cutoff_x, yend = round_cutoff_x*0.5, colour = grpinfo$col[3]) +
      geom_segment(x = 0, y = 0, xend = round_cutoff_x, yend = round_cutoff_x*0.25, colour = grpinfo$col[4]) +   
      geom_segment(x = 0, y = 0, xend = round_cutoff_x, yend = round_cutoff_x*0.1, colour = grpinfo$col[5]) +   
      
      annotate("text", x=(round_cutoff_x+(minor_unit*0.04)), y=(round_cutoff_x), label=paste(end.point,"0",sep=""), 
               colour = group.colors.dark[2], fontface="bold", size = 5, hjust=0) +
      annotate("text", x=(round_cutoff_x+(minor_unit*0.04)), y=((round_cutoff_x*0.5)), label=paste(end.point,"50",sep=""), 
               colour = group.colors.dark[3], fontface="bold", size = 5, hjust=0) +
      annotate("text", x=(round_cutoff_x+(minor_unit*0.04)), y=((round_cutoff_x*0.25)), label=paste(end.point,"75",sep=""), 
               colour = group.colors.dark[4], fontface="bold", size = 5, hjust=0) +
      annotate("text", x=(round_cutoff_x+(minor_unit*0.04)), y=((round_cutoff_x*0.1)), label=paste(end.point,"90",sep=""), 
               colour = group.colors.dark[5], fontface="bold", size = 5, hjust=0) +
      
      scale_y_continuous(breaks=y_scale_vec, label=y_scale_label) +
      scale_x_continuous(breaks=x_scale_vec, label=x_scale_label) +
      
      scale_fill_manual(Response, values=group.colors, labels=group.labels, limits=names(group.colors)) +
      scale_shape(solid = FALSE) +
      scale_shape_manual(values=c(21, 24), drop=FALSE) +
      
      guides(shape="none", 
             colour=FALSE, 
             fill = guide_legend(override.aes = list(size=5, shape=21)))+
      
      theme(plot.title = element_text(lineheight=.8,  hjust = 0.5, face = "bold", size=textSizeMajor-2, colour="black"), 
            axis.title = element_blank(), 
            axis.text.x = element_text(size=rel(1.5)), 
            axis.text.y = element_text(size=rel(1.5)), 
            panel.grid.minor.x = element_blank(), 
            strip.text=element_text(size=15), 
            strip.background =element_rect(fill="transparent"), 
            legend.text=element_text(size=13), 
            legend.background = element_rect(colour = NULL, fill="transparent"), 
            legend.position="none", 
            legend.key = element_rect(colour = "transparent", fill = "transparent", size=0.5), 
            legend.direction = "horizontal", 
            plot.margin = rep(unit(0, "null"), 4)) +
      
      geom_text(data=annodata, aes(x=base, y=aval, label=annotext), fontface="bold", size=rel(7), 
                color = "black", hjust=0, vjust=0, inherit.aes = FALSE )
    
    if(zoom_type=="zoom"){
      img <- img + 
        geom_rect(xmin = (x_min-(minor_unit*.6)), ymin = (y_aval_max-(minor_unit*0.15)), 
                  xmax = (x_base_max+(minor_unit*0.15)), ymax = (y_aval_max+(minor_unit*0.15)), 
                  fill = "white", colour = "white") +
        geom_rect(ymin = (y_min-(minor_unit*.6)), xmin = (x_base_max-(minor_unit*0.15)), 
                  ymax = (y_aval_max+(minor_unit*0.15)), xmax = (x_base_max+(minor_unit*0.15)), 
                  fill = "white", colour = "white")
    }
    
    return(img)
  }
  
  ## Clock/time-line functions
  
  # Time Index for clock
  temp2 <- unique(inputdata$sday)
  temp2 <- temp2[order(temp2)]
  
  vecdiff2 <- diff(temp2)
  q <- vecdiff2/7
  
  cf <- (r/7)*(temp2-1)
  
  clockindex <- 0
  for (i in 1:(length(cf)-1)) {
    j=i+1
    clockindex.i <- seq(cf[i]+q[i], cf[j], by=q[i])
    clockindex <- c(clockindex, clockindex.i)
  }
  
  # Round Clock
  plot.clock.round <- function(day.current, day.max) {
    require(ggplot2)
    
    dat <- data.frame(d = factor(c(rep(paste(day.current), day.current), 
                                   rep(paste(day.max), day.max - day.current)), 
                                 levels=unique(c(paste(day.current), paste(day.max)))))
    
    if(day.current == 0) {
      cols = c("light grey")
    } 
    else {
      cols = c("dark blue", "light grey")
    }
    
    pie = ggplot(dat, aes(x = factor(1), fill = factor(d))) +
      geom_bar(width = 1) +
      scale_fill_manual(values = cols) +
      coord_polar(theta = "y", direction = -1) +
      geom_text(aes(y="", label="")) +
      xlab("") + ylab("")  +
      theme_bw() +
      theme(legend.position = "none", 
            axis.ticks = element_blank(), 
            axis.title = element_blank(), 
            axis.text  = element_blank(), 
            panel.border = element_blank(), 
            panel.background = element_blank(), 
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(), 
            axis.text.x = element_blank(), 
            axis.text.y = element_blank(), 
            axis.title.x = element_blank(), 
            axis.title.y = element_blank(), 
            axis.ticks.length = unit(0, "null"), 
            plot.margin = unit(c(0, 0, 0, 0), "npc")
      )
  }
  
  # Line Clock / Time-Line
  plot.clock.line <- function(day.current, day.max) {
    require(ggplot2)
    
    dat <- data.frame(d = factor(c(rep(paste(day.current), day.current), 
                                   rep(paste(day.max), day.max - day.current)), 
                                 levels=unique(c(paste(day.current), paste(day.max)))))
    
    if(day.current == 0) {
      cols = c("light grey")
    } 
    else {
      cols = c("dark blue", "light grey")
    }
    
    week_frames <- r*(visit_to_week$uweek)
    week_labs <- visit_to_week$week_lab
    day_size <- rep(15, length(visit_to_week$uweek))
    
    pie = ggplot(dat, aes(x = factor(1), fill = factor(d))) +
      geom_bar(width = 0.7, position = position_stack(reverse = TRUE)) +
      scale_fill_manual(values = cols) + 
      coord_flip() + 
      geom_hline(yintercept = week_frames, size=1) + 
      scale_y_discrete(limits = week_frames, breaks = week_frames, 
                       labels = week_labs) +
      xlab("") + ylab("")  +
      theme_classic() +
      theme(legend.position = "none", 
            plot.margin = unit(c(0, 1.5, 0, 1.5), "cm"), #top, right, bottom, left
            panel.grid = element_blank(), 
            axis.text.y = element_blank(), 
            axis.text.x = element_text(size=day_size, face="bold", hjust=1), 
            axis.ticks = element_blank(), 
            axis.line = element_blank(), 
            plot.title = element_blank()
      )
  }
  
  ## Bar plot functions
  plot.bar.short <- function(inputdata, vp){
    p <- ggplot(data=inputdata, aes(x=1, y=freq, fill=grp)) + geom_bar(stat="identity", colour="black") +
      xlab("") + ylab("") + scale_fill_manual(values=group.colors) + coord_flip()
    
    u <- p + theme( 
      legend.position = "none", 
      panel.background = element_blank(), 
      panel.grid.major = element_blank(), 
      panel.grid.minor = element_blank(), 
      panel.margin = unit(0, "null"), 
      plot.margin = rep(unit(0, "null"), 4), 
      axis.ticks = element_blank(), 
      axis.text.x = element_blank(), 
      axis.text.y = element_blank(), 
      axis.title.x = element_blank(), 
      axis.title.y = element_blank(), 
      axis.ticks.length = unit(0, "null"), 
      axis.ticks.margin = unit(0, "null"))
    
    gt <- ggplot_gtable(ggplot_build(u))
    ge <- subset(gt$layout, name == "panel")
    pushViewport(vp)
    grid.draw(gt[ge$t:ge$b, ge$l:ge$r])
    popViewport()
  }
  
  plot.bar.long <- function(inputdata){
    p <- ggplot(data=inputdata, aes(x=trtp, y=pct, fill=grp)) + 
      geom_bar(stat="identity", colour="transparent", width = 0.7) +
      xlab("") + ylab("% of patients") + scale_fill_manual(values=group.colors)+
      scale_y_continuous(limits = c(0, 100), 
                         breaks=c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100), 
                         minor_breaks = seq(0, 100, 5), 
                         expand=c(0, 1)) +
      scale_x_discrete(position = "top", expand=c(0, 0)) 
    
    u <- p + theme(
      legend.position = "none", 
      panel.background = element_blank(), 
      panel.grid.minor.y = element_line(colour="white"), 
      panel.grid.major.y = element_line(colour="white"), 
      panel.grid.minor.x = element_blank(), 
      panel.grid.major.x = element_blank(), 
      panel.ontop = TRUE, 
      panel.margin = unit(0, "null"), 
      plot.margin = unit(c(0, 0, 0, 0.02), "npc"), 
      axis.text.x = element_text(size=textSizeMinor-7, vjust=0.2), 
      axis.text.y = element_text(size=rel(1.5)), 
      axis.title.x = element_blank(), 
      axis.title.y = element_blank(), 
      axis.ticks.length = unit(1, "null"), 
      axis.ticks.margin = unit(0, "null"))
    
    return(u)
  }
  
  makeblank<-function(){
    img <- ggplot(data=mtcars, aes(wt, mpg)) +
      theme_void()
    return(img)
  }
  
  
  ## Part 6: Intergrated functions to produce final single frame
  interdoplot <- function(framedata, bardata, xvn, yvn, grpvn="grp", tvn="week",
  # interdoplot <- function(framedata, bardata=NULL, xvn, yvn, grpvn="grp", tvn="week", 
                          shpvc = "titrated", sumdata, titleindex, currvis, maxvis, annodata){
    scatter1 <- makescatter(framedata, xvn, yvn, grpvn, tvn, shpvc, annodata, trtgrp=trt1)
    scatter2 <- makescatter(framedata, xvn, yvn, grpvn, tvn, shpvc, annodata, trtgrp=trt2)
    
    clock <- ifelse(clock_type=="round", 
                    plot.clock.round(currvis, maxvis), 
                    plot.clock.line(currvis, maxvis))
    
    # if(bar_type=="long") pctbar <- plot.bar.long(bardata) # Wasn't sure if this was syntatically correct, suggestion:
    if(bar_type=="long"){
      pctbar <- plot.bar.long(bardata)
      }
    
    title <- paste(plot_title, " at Week ", titleindex, " (", studyname, ")", sep="")
    null <- textGrob(" ")
    xaxis.label <- textGrob(xlab, gp=gpar(fontsize=textSizeMinor))
    yaxis.label <- textGrob(ylab, gp=gpar(fontsize=textSizeMinor), rot = 90)

    legndTitle <- textGrob("Percentage Improvement:", gp = gpar(fontsize=textSizeMinor-5))
    
    legndBody <- grobTree(
      gp = gpar(fontsize = textSizeMinor-5, fontface = "bold"), 
      textGrob(label = "<0%", name = "title1", 
               x = unit(14.5, "lines"), y = unit(0.85, "lines"), 
               hjust = 0, vjust = 0, gp = gpar(col = group.colors.dark[1])), 
      textGrob(label = "[0-50%)", name = "title2", 
               x = grobWidth("title1") + unit(15, "lines"), y = unit(0.85, "lines"), 
               hjust = 0, vjust = 0, gp = gpar(col = group.colors.dark[2])), 
      textGrob(label = "[50-75%)", name = "title3", 
               x = grobWidth("title1") + grobWidth("title2") + unit(15.5, "lines"), y = unit(0.85, "lines"), 
               hjust = 0, vjust = 0, gp = gpar(col = group.colors.dark[3])), 
      textGrob(label = "[75-90%)", name = "title4", 
               x = grobWidth("title1") + grobWidth("title2") + grobWidth("title3") + unit(16, "lines"), y = unit(0.85, "lines"), 
               hjust = 0, vjust = 0, gp = gpar(col = group.colors.dark[4])), 
      textGrob(label = "[90-100%)", name = "title5", 
               x = grobWidth("title1") + grobWidth("title2") + grobWidth("title3") + 
                   grobWidth("title4") + unit(16.5, "lines"), y = unit(0.85, "lines"), 
               hjust = 0, vjust = 0, gp = gpar(col = group.colors.dark[5])), 
      textGrob(label = "100%", name = "title6", 
               x = grobWidth("title1") + grobWidth("title2") + grobWidth("title3") + 
                   grobWidth("title4") + grobWidth("title5") + unit(17, "lines"), y = unit(0.85, "lines"), 
               hjust = 0, vjust = 0, gp=gpar(col = group.colors.dark[6]))
    )

    barTitle <- textGrob("% of patients", hjust = 0, vjust = 0, x = unit(0, "lines"), 
                         y = unit(0.4, "lines"),  gp=gpar(fontsize=textSizeMinor))
    
    img <- ifelse(clock_type == "round", 
                  grid.arrange(
                      arrangeGrob(clock, ncol=1), 
                      arrangeGrob(sub2=textGrob(title, gp = gpar(fontsize=textSizeMajor, face="plain"), vjust=0), ncol=1), 
                      arrangeGrob(yaxis.label, scatter1, 
                                  arrangeGrob(barTitle, pctbar, nrow=2, heights=c(0.05, 0.95), top=NULL), 
                                  yaxis.label, scatter2, 
                                  ncol=5, widths=c(0.04, 0.41, 0.1, 0.04, 0.41), nrow=1), 
                      arrangeGrob(makeblank(), xaxis.label, makeblank(), makeblank(), xaxis.label, 
                                  ncol=5, widths=c(0.04, 0.41, 0.1, 0.04, 0.41), nrow=1), 
                      legndBody, 
                      heights=c(0.07, 0.05, 0.76, 0.06, 0.06), 
                      ncol = 1, nrow=5), 
                  grid.arrange(
                      arrangeGrob(sub2=textGrob(title, gp = gpar(fontsize=textSizeMajor, face="plain"), vjust=0.75), ncol=1), 
                      arrangeGrob(clock, ncol=1), 
                      arrangeGrob(makeblank(), ncol=1), 
                      arrangeGrob(yaxis.label, scatter1, makeblank(), 
                                  arrangeGrob(barTitle, pctbar, nrow=2, heights=c(0.07, 0.93), top=NULL), 
                                  makeblank(), yaxis.label, scatter2, makeblank(), 
                                  ncol=8, widths=c(0.045, 0.38, 0.015, 0.11, 0.005, 0.045, 0.38, 0.02), nrow=1), 
                      arrangeGrob(makeblank(), xaxis.label, makeblank(), makeblank(), xaxis.label, ncol=5, 
                                  widths=c(0.04, 0.41, 0.1, 0.04, 0.41), nrow=1), 
                      legndBody, 
                      heights=c(0.113, 0.07, 0.015, 0.682, 0.06, 0.06), 
                      ncol = 1, nrow=6))
    
    return(img)
  }
  
  sinfig1 <- function(inputdata, visit, sumdata, titleindex, currvis, maxvis, annodata){
    testdata <- subset(interpolated, avisitn==visit)
    
    bar_data <- filter(endpoint_freq, avisitn == visit) %>%
                mutate(trtp = ifelse(trtp==trt1, trt1_abr, trt2_abr), 
                       trtp = factor(trtp, levels=c(trt1_abr, trt2_abr)))
    
    testimg <- ifelse(bar_type=="short", 
                      interdoplot(framedata=testdata, bardata=NULL, xvn="base", yvn="aval", grpvn="grp", shpvc = "titrated", 
                                  sumdata=sumdata, titleindex=titleindex, currvis=currvis, maxvis=maxvis, annodata=annodata), 
                      interdoplot(framedata=testdata, bardata=bar_data, xvn="base", yvn="aval", grpvn="grp", shpvc = "titrated", 
                                  sumdata=sumdata, titleindex=titleindex, currvis=currvis, maxvis=maxvis, annodata=annodata))
    
    print(testimg)
  }
  
  vp_l <- viewport(width = 0.17, height = 0.05, x = unit(0.1, "npc"), 
                   y = unit(0.56, "npc"), clip="off", just="center", angle = 90)
  vp_r <- viewport(width = 0.17, height = 0.05, x = unit(0.575, "npc"), 
                   y = unit(0.56, "npc"), clip="off", just="center", angle = 90)
  
  
  
  
  
  ###### ABOVE SHOULD BE "FINISHED" #############
  
  ## Part 7: loops to produce final frames ##

  for (i in 1:length(visind)) {

    if ((visind[i] == floor(visind[i])) & (visind[i] != visind[length(visind)])) {
      for (j in 1:pause.frame.num) {
        frame.counter <- frame.counter+1
        jpeg(filename=file.path(outpath, figind2[frame.counter]), quality=100, width=1280, height=720, type="cairo")            
        sumdata3 <- filter(sumdata2, avisitn_interp==visind[i] & avisitn > avisitn_base)
        a=sinfig1(interpolated, visind[i], summary, titleindex[i], currvis=clockindex[i], maxvis=clockindex[length(clockindex)], annodata=sumdata3);
        print(a)
        
        #### bar plot (left plot)
        bar.data <- filter(endpoint_freq, avisitn==visind[i], trtp==trt1)
        b <- plot.bar.short(bar.data, vp_l)
        print(b)
        
        #### bar plot (right plot)
        bar.data <- filter(endpoint_freq, avisitn==visind[i], trtp==trt2)
        c <- plot.bar.short(bar.data, vp=vp_r)
        print(c) 
        
        dev.off()
      }
    }
    
    else if (visind[i] != visind[length(visind)]) {
      frame.counter <- frame.counter+1
      jpeg(filename=file.path(outpath, figind2[frame.counter]), quality=100, width=1280, height=720, type="cairo")
      sumdata3 <- filter(sumdata2, avisitn_interp==visind[i] & avisitn > avisitn_base)
      a <-sinfig1(interpolated, visind[i], summary, titleindex[i], currvis=clockindex[i], maxvis=clockindex[length(clockindex)], annodata=sumdata3);
      print(a)
      
      #### bar plot (left plot)
      bar.data <- filter(endpoint_freq, avisitn==visind[i], trtp==trt1)
      b=plot.bar.short(bar.data, vp=vp_l)
      print(b)
      
      #### bar plot (right plot)
      bar.data <- filter(endpoint_freq, avisitn==visind[i], trtp==trt2)
      c=plot.bar.short(bar.data, vp=vp_r)
      print(c) 
      
      dev.off()   
    }
    
    else if (visind[i] == visind[length(visind)]) {
      for (j in 1:(2*pause.frame.num)) {
        frame.counter <- frame.counter+1
        jpeg(filename=file.path(outpath, figind2[frame.counter]), quality=100, width=1280, height=720, type="cairo")            
        sumdata3 <- filter(sumdata2, avisitn_interp==visind[i] & avisitn > avisitn_base)
        a=sinfig1(interpolated, visind[i], summary, titleindex[i], currvis=clockindex[i], maxvis=clockindex[length(clockindex)], annodata=sumdata3);
        print(a)
        
        #### bar plot (left plot)
        bar.data <- filter(endpoint_freq, avisitn==visind[i], trtp==trt1)
        b=plot.bar.short(bar.data, vp=vp_l)
        print(b)
        
        #### bar plot (right plot)
        bar.data <- filter(endpoint_freq, avisitn==visind[i], trtp==trt2)
        c=plot.bar.short(bar.data, vp=vp_r)
        print(c) 
        
        dev.off()
      }
    }
  }
  
  
  
  
  
  
  
  
  
  for (i in 1:length(visind)) {
    
    paste("Visind is", visind)
    
    if ((visind[i] == floor(visind[i])) & (visind[i] != visind[length(visind)])) {
      for (j in 1:pause.frame.num) {
           
        sumdata3 <- filter(sumdata2, avisitn_interp==visind[i] & avisitn > avisitn_base)
        a=sinfig1(interpolated, visind[i], summary, titleindex[i], currvis=clockindex[i], maxvis=clockindex[length(clockindex)], annodata=sumdata3);
        print(a)
        
        dev.off()
      }
    }
    
    else if (visind[i] != visind[length(visind)]) {
      frame.counter <- frame.counter+1
      jpeg(filename=file.path(outpath, figind2[frame.counter]), quality=100, width=1280, height=720, type="cairo")            
      #sumdata3 <- filter(sumdata2, avisitn==floor(visind[i]) & avisitn > avisitn_base)
      sumdata3 <- filter(sumdata2, avisitn_interp==visind[i] & avisitn > avisitn_base)
      a=sinfig1(interpolated, visind[i], summary, titleindex[i], currvis=clockindex[i], maxvis=clockindex[length(clockindex)], annodata=sumdata3);
      print(a)
      
      dev.off()   
    }
    
    else if (visind[i] == visind[length(visind)]) {
      for (j in 1:(2*pause.frame.num)) {
        frame.counter <- frame.counter+1
        jpeg(filename=file.path(outpath, figind2[frame.counter]), quality=100, width=1280, height=720, type="cairo")            
        sumdata3 <- filter(sumdata2, avisitn_interp==visind[i] & avisitn > avisitn_base)
        a=sinfig1(interpolated, visind[i], summary, titleindex[i], currvis=clockindex[i], maxvis=clockindex[length(clockindex)], annodata=sumdata3);
        print(a)
        
        dev.off()
      }
    }
  }
}  
  
  
  
  
  
  
  
  



rainfall_animator <- function(outpath, delete_choice, speed){
  Sys.setenv("PATH" = paste(Sys.getenv("PATH"), outpath, sep=":"))
  
  fps <- as.numeric(speed)
  
  cmd <- paste("/lrlhps/apps/ffmpeg/ffmpeg-3.4.2/ffmpeg", " -y", " -framerate ", fps, " -i ", 
               paste(outpath, "RAINFALL%04d.JPEG", sep="/"),  " -qscale:v 1 -vcodec mpeg4 ", 
               paste(outpath, "Rainfall", sep="/"), ".mp4", sep="")
  
  system(cmd)
  
  # Deleting all frames based on user choice
  if(delete_choice == "Yes"){
    path <- paste(outpath, "/", sep="")
    remove.files <- list.files(path, pattern="*.JPEG")
    remove.list <- paste0(path, remove.files)
    invisible(do.call(file.remove, list(remove.list)))
  }
}


