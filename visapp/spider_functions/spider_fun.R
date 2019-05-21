eensy_weensy_spider <- function(radardata, vlabel, outpath, plot_title, footnote1, footnote2,
                                color_choices, opacity, speed, st_end_pause, keep,
                                updateProgress = NULL){
  
  library(plyr)
  library(fmsb)
  library(gridExtra)
  library(grid)
  library(grDevices)
  
  req(radardata)
  req(outpath)
  req(vlabel)
  
  ## reading in the data
  varcols <- colnames(radardata)[!(colnames(radardata) %in% c("TRT","AVISITN"))]
  
  ## need to do week-2 because dataset starts at week 2.
  radardata$week <- radardata$AVISITN - min(radardata$AVISITN)
  
  ## change based on your trts
  # trtorder <- c('Baricitinib 4mg','Baricitinib 2mg', 'Baricitinib 1mg','Placebo')
  
  max <- rep(1,9)
  min <- rep(0,9)
  
  #3 getting final week value
  max.clock <- max(radardata$week,na.rm=TRUE)
  
  ## creating labels, weird spaces added arbitrarily to make sure labels are seen.
  # vlabel <- c("EASI","Skin Pain          ","Itch NRS", "DLQI", "           ADSS")
  
  near <- function(x, y, tol = .Machine$double.eps ^ 0.5) {
    abs(x - y) < tol
  }
  
  
  #####################################
  # function to create clock
  # DON'T NEED TO CHANGE ANYTHING HERE.
  #####################################
  
  makepie <- function(piedat){
    
    pie <-  ggplot(piedat, aes(x = factor(1), fill = factor(d))) +
      geom_bar(width = 1)+ 
      scale_fill_manual(values = c("dark blue", "light grey")) +
      coord_polar(theta = "y",start=0) +
      geom_text(aes(y="",label="")) +
      xlab("") + ylab("") +
      theme_bw() +
      theme(legend.position = "none",
            axis.ticks = element_blank(),
            panel.grid = element_blank(),
            axis.text  = element_blank())
    
    img <- pie
    
    return(img)
  }
  
  
  makepie2 <- function(piedat){
    
    pie <-  ggplot(piedat, aes(x = factor(1), fill = factor(d))) +
      geom_bar(width = 1)+ 
      scale_fill_manual(values = c("light grey","dark blue")) +
      coord_polar(theta = "y",start=0) +
      geom_text(aes(y="",label="")) +
      xlab("") + ylab("") +
      theme_bw() +
      theme(legend.position = "none",
            axis.ticks = element_blank(),
            panel.grid = element_blank(),
            axis.text  = element_blank())
    
    img <- pie
    
    return(img)
  }
  
  ###########################################
  
  # "interpolate.R" is the source function that used to do the data interpolation;
  
  ## list all variables to keep *** THIS CAN BE CHANGED BASED ON WHAT DATA IS REQUESTED.
  
  radarinter <- radardata
  
  r <- 60 #rate in interpolation
  progbar <- "time"
  
  radar_list <- list()
  
  for(i in 1:length(varcols)){
    radar_list[[varcols[i]]] <- ddply(radarinter, .(TRT), function(x) interpolate(x, "week", varcols[i], rate=r),
                                      .progress = progbar)
  }
  
  
  MyMerge <- function(x, y){
    df <- merge(x, y, by= c("week","TRT"), all.x= TRUE, all.y= TRUE)
    return(df)
  }
  
  ## CHANGE LIST HERE IF YOU WANT OTHER VARIABLES
  allint <- Reduce(MyMerge, radar_list)
  
  allint2 <- allint[order(allint$week,allint$TRT),]
  
  ###########################################################################
  
  weekind <- allint2$week
  
  weekind <- unique(weekind)
  
  temp <- unique(radarinter$week)
  vecdiff <- diff(temp)
  vecdiff <- r*vecdiff
  tempindex <- c(rep(temp[1:(length(temp)-1)],vecdiff),temp[length(temp)])
  
  weekind2 <- temp
  titleindex <- tempindex
  
  # number of duplicate copies of 1st and last frame
  
  K <- as.numeric(st_end_pause)*as.numeric(speed)
  
  # number of unique frames excluding the 1st and last frame
  
  L <- length(weekind)-2
  
  ff <- Vectorize(function(x) formatC(x, width = 4, format = "d", flag = "0"))
  
  ind <- ff(1:(L+K*2))
  
  figind <- rep(0,(L+K*2))
  
  # naming the frames
  for (i in 1:(L+K*2)){figind[i] <- paste("COARADAR",ind[i],".JPEG", sep="")}
  
  # color specification (user input)
  colors_border <- NULL
  colors_in <- NULL
  colors_legend <- NULL
  
  for(i in 1:length(color_choices)){
    colors_border[i] <- rgb(col2rgb(color_choices[i])[1]/255,col2rgb(color_choices[i])[2]/255,col2rgb(color_choices[i])[3]/255,.9)
    colors_in[i] <- rgb(col2rgb(color_choices[i])[1]/255,col2rgb(color_choices[i])[2]/255,col2rgb(color_choices[i])[3]/255,opacity)
    colors_legend[i] <- rgb(col2rgb(color_choices[i])[1]/255,col2rgb(color_choices[i])[2]/255,col2rgb(color_choices[i])[3]/255,.9)
  }
  
  # Isolate min and max dates
  datnum <- as.numeric(substring(keep,5))
  datmin <- min(datnum[!is.na(datnum)])
  datmax <- max(datnum[!is.na(datnum)])
  
  # paralellized process to improve efficiency
  cores <- parallel::detectCores()
  cl <- makeSOCKcluster(cores)
  registerDoSNOW(cl)
  
  pb <- txtProgressBar(min=1, max=(L+2*K), style=3)
  progress <- function(n) setTxtProgressBar(pb, n)
  opts <- list(progress=progress)
  
  foreach(i = 1:(L+2*K),.options.snow=opts) %dopar% {

    library(plyr)
    library(fmsb)
    library(ggplot2)
    library(gridExtra)
    library(grid)
    
    source("general_functions/makefootnote.R")
    source("general_functions/interpolate.R")
    source("spider_functions/radar4.R")
    
    # if (is.function(updateProgress)) {
    #   updateProgress()
    # }
    
    
    if(i %in% 1:K){
      
      jpeg(filename=file.path(outpath, figind[i]),pointsize=15, quality=100,width=1280,height=720,type="cairo")
      
      wkdata <- allint2[which(allint2$week==0),]
      data <- rbind(max,min,wkdata)
      
      
      bigradar(data[,-c(1,2)], axistype=1,seg=10,
               #custom polygon
               pcol=colors_border , pfcol=colors_in , plwd=2 , plty=1, 
               #custom the grid
               vlabels=vlabel,
               cglcol="black", cglty=3, axislabcol="black", centerzero=TRUE, 
               caxislabels = c("100%","90%","80%","70%","60%","50%","40%","30%","20%","10%","0"), 
               cglwd=1.5,calcex=1.12, vlcex=1.12,
               title=plot_title)
      
      legend(x=0.9, y=0.92, legend = unique(data$TRT)[!is.na(unique(data$TRT)) & unique(data$TRT) != ""], 
             bty = "n", pch=20 , col=colors_legend , text.col = "black",cex=1.2, pt.cex=2)
      
      makeFootnote(footnote1,height=10)
      makeFootnote(footnote2,height=5)
      
      weeks <- paste("WEEK", 0) 
      
      obs <- max(wkdata$week)
      dispd <- paste(obs)
      dispm <- paste(max.clock)
      pdat <- data.frame(d=factor(c(rep(dispd,obs),rep(dispm,max.clock-obs)),
                                  levels=c(dispd,dispm)))
      clock <- makepie2(pdat)
      
      
      # timevar displayed at top of plot - can change to "months" etc... depending on situation
      
      vp.top <- viewport(height=unit(.25, "npc"), width=unit(0.25, "npc"), 
                         just=c("left","top"), 
                         x = unit(0,"npc"),
                         y= unit(0.7, "npc"))
      
      
      print(clock, vp=vp.top)
      
      makenote("Week", location = 0.11, height=0.45)
      makenote(datmin, location=0.2, height=0.575)
      makenote(datmax, location=0.125, height=0.7)
      dev.off()
    }
    
    if(i %in% (K+1):(K+L)){
      
      jpeg(filename=file.path(outpath, figind[i]), pointsize=15, quality=100,width=1280,height=720,type="cairo")
      
      
      wkdata=allint2[which(near(allint2$week,weekind[(i-K+1)])),]
      data=rbind(max,min,wkdata)
      a=titleindex[(i-K+1)]
      
      
      bigradar(data[,-c(1,2)], axistype=1,seg=10,
               #custom polygon
               pcol=colors_border, pfcol=colors_in , plwd=2 , plty=1, 
               #custom the grid
               vlabels = vlabel,
               cglcol="black", cglty=3, axislabcol="black", centerzero=TRUE, 
               caxislabels = c("100%","90%","80%","70%","60%","50%","40%","30%","20%","10%","0"),
               cglwd=1.5,calcex=1.12, vlcex=1.12,
               title=plot_title
      )
      
      
      legend(x=0.9, y=0.92, legend = unique(data$TRT)[!is.na(unique(data$TRT)) & unique(data$TRT) != ""], 
             bty = "n", pch=20 , col=colors_legend , text.col = "black",cex=1.2, pt.cex=2)
      
      makeFootnote(footnote1,height=10)
      makeFootnote(footnote2,height=5)
      
      #weeks <- paste("WEEK",a) 
      
      obs <- max(wkdata$week) # only use original week numbers
      dispd <- paste(obs)
      dispm <- paste(max.clock)
      pdat <- data.frame(d=factor(c(rep(dispd,obs*r),rep(dispm,max.clock*r-obs*r)),
                                  levels=c(dispm,dispd)))
      clock <- makepie2(pdat)
      
      
      # timevar displayed at top of plot - can change to "months" etc... depending on situation
      
      vp.top <- viewport(height=unit(.25, "npc"), width=unit(0.25, "npc"), 
                         just=c("left","top"), 
                         x = unit(0,"npc"),
                         y= unit(0.7, "npc"))
      
      
      print(clock, vp=vp.top)
      makenote("Week", location = 0.11, height=0.45)
      makenote(datmin, location=0.2, height=0.575)
      makenote(datmax, location=0.125, height=0.7)
      dev.off()
    }
    
    if(i %in% (K+L+1):(2*K+L)){
      
      jpeg(filename=file.path(outpath, figind[i]), pointsize=15, quality=100,width=1280,height=720,type="cairo")
      
      
      wkdata=allint2[which(allint2$week==max(weekind)),]
      data=rbind(max,min,wkdata)
      
      
      bigradar(data[,-c(1,2)], axistype=1,seg=10,
               #custom polygon
               pcol=colors_border, pfcol=colors_in , plwd=2 , plty=1, 
               vlabels = vlabel,
               #custom the grid
               cglcol="black", cglty=3, axislabcol="black", centerzero=TRUE, 
               caxislabels = c("100%","90%","80%","70%","60%","50%","40%","30%","20%","10%","0"),
               cglwd=1.5,calcex=1.12, vlcex=1.12,
               title=plot_title # titleindex need to be defined upfront
      )
      
      
      legend(x=0.9, y=0.92, legend = unique(data$TRT)[!is.na(unique(data$TRT)) & unique(data$TRT) != ""], 
             bty = "n", pch=20 , col=colors_legend , text.col = "black",cex=1.2, pt.cex=2)
      
      makeFootnote(footnote1,height=10)
      makeFootnote(footnote2,height=5)
      
      obs <- max(wkdata$week) # only use original week numbers
      dispd <- paste(obs)
      dispm <- paste(max.clock)
      pdat <- data.frame(d=factor(c(rep(dispd,obs),rep(dispm,max.clock-obs)),
                                  levels=c(dispd)))
      clock <- makepie(pdat)
      
      # timevar displayed at top of plot - can change to "months" etc... depending on situation
      
      vp.top <- viewport(height=unit(.25, "npc"), width=unit(0.25, "npc"), 
                         just=c("left","top"), 
                         x = unit(0,"npc"),
                         y= unit(0.7, "npc"))
      
      print(clock, vp=vp.top)
      makenote("Week", location = 0.11, height=0.45)
      makenote(datmin, location=0.2, height=0.575)
      makenote(datmax, location=0.125, height=0.7)
      dev.off()
    }
  }
  close(pb)
  stopCluster(cl)
}


## Combines frames generated by eensy_weensy_spider function into mp4 formatted video
spider_animator <- function(outpath, delete_choice="No", speed){
  Sys.setenv("PATH" = paste(Sys.getenv("PATH"), outpath, sep=":"))
  
  fps <- as.numeric(speed)
  
  cmd <- paste("/lrlhps/apps/ffmpeg/ffmpeg-3.4.2/ffmpeg"," -y"," -framerate ", fps," -i ",
               paste(outpath,"COARADAR%04d.JPEG",sep="/"),  " -qscale:v 1 -vcodec mpeg4 ",
               paste(outpath,"COAspider",sep="/")   ,".mp4",sep="")
  
  system(cmd)
  
  # Deleting all frames based on user choice
  if(delete_choice == "Yes"){
    path <- paste(outpath, "/", sep="")
    remove.files <- list.files(path, pattern="*.JPEG")
    remove.list <- paste0(path,remove.files)
    invisible(do.call(file.remove,list(remove.list)))
  }
}







