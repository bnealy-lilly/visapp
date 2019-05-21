

make_it_rain <- function(data, labs, outpath, plot_title, footnote1, footnote2,
                         col_format, col_choices, speed, bar_type,
                         clock_type, zoom_type, cat_ranges, trt1, ...){
library(plyr)
library(gridExtra)
library(grid)


### define the path where the data located
# data.loc <- "/lillyce/qa/ly2439821/i1f_mc_rhba/intrm2/data/analysis/shared/custom"
# sum.loc <- "/lillyce/qa/ly2439821/uncover/visual_analytics/data/analysis/custom"
# 
# ### define the path where all single frames were stored
# export.loc <- "/lillyce/qa/ly2439821/uncover/visual_analytics/output/all_zoomed/aad_feb_request/frames"
# 
# ### define the path where the video located
# OUTPUT = "/lillyce/qa/ly2439821/uncover/visual_analytics/output/all_zoomed/aad_feb_request"


##########################
# data for scatter plot
##########################

names(data) <- tolower(names(data))

data <- rename(data, subjid=fsubjid, aval=pasi_total, base = base_pasi) %>%
        mutate(trtp = ifelse(trtp = trt1, 'Taltz 80mg every two weeks', ifelse(trtp=='Placebo', 'Placebo', 'wrong')),
               titrated = 'unc') %>% filter(studyid == "I1F-MC-RHBA", trtp == 'Taltz 80mg every two weeks' | trtp == 'Placebo') %>% 
        select(subjid, avisitn, sday, aval, base, trtp, pimp, pasi50, pasi75, pasi100, pasi90, studyid)

unique_pat <- distinct(data, subjid, trtp, .keep_all=TRUE) %>%
              count(trtp) %>%
              rename(bign = n)
data <- left_join(data, unique_pat, by=c("trtp"))


############################################################
# Part 2: conduct necessary manipulation to the input data #
############################################################

### Eliminate records with "NA" in the input data, usually the number of "NA" is low and won't provide valuable information;
### "NA" can come from data creation, for example, if baseline data is missing, then the corresponding % change should be "NA";

row.has.na <- apply(data, 1, function(x){any(is.na(x))})
naindex <- which(row.has.na == TRUE)

data <- data[!row.has.na,] %>%
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

data$base[data$base > round_cutoff] <- 34.95
data$aval[data$aval > round_cutoff] <- 34.95

### factorize the levels of the variable of interest;

flagorder1 <- c('<0%', '0% - <50%', '50% - <75%', '75% - <90%', '90% - <100%', '100%')

data$grp = factor(data$pimp.cat,levels=flagorder1)

table(data$grp[data$sday==1])

data$grp[data$sday==1] <- "0% - <50%"

grplist <- levels(data$grp)
print(grplist)

### add color lists to the levels of variable of interest;

colst1 <- paste('c("',grplist[1],'"="#FF0000","', 
                grplist[2],'"="#FFCE5C","', 
                grplist[3],'"="#a7ff42","',
                grplist[4],'"="#68ff42","',     
                grplist[5],'"="#2CD723","', 
                grplist[6],'"="#006838" 
)',sep="") 

group.colors = eval(parse(text = colst1))
print(group.colors)

### input labels for different levels of variable of interest;

labst <- paste('c("',grplist[1],'"="<0%","',
               grplist[2],'"="0% - <50%","',
               grplist[3],'"=      "50% - <75%","',
               grplist[4],'"=      "75% - <90%","',
               grplist[5],'"=      "90% - <100%","',
               grplist[6],'"=      "100%"
)',sep="")

group.labels = eval(parse(text = labst))

group.order = c(grplist[1], grplist[2], grplist[3], grplist[4], grplist[5], grplist[6])
print(group.order)

grpinfo = data.frame(col = c("#FF0000", "#FFCE5C", "#a7ff42",  "#68ff42", "#2CD723", "#006838"),
                     lbl = c("<0%", "0% - <50%", "50% - <75%", "75% - <90%", "90% - <100%", "100%"),
                     val = c("[-Inf,0)",  "[0,50)", "[50,75)",     "[75,90)",    "[90,100)",   "[100, Inf)"),
                     stringsAsFactors = FALSE)

textSizeMajor = 32
textSizeMinor = 27

# position bar chart 

rect.x.center <- 12
rect.x.width <- 2.25
rect.y.center <- 26.25
rect.y.height <- 12.7

### add legends of the single frame;

xlab = "Baseline"
ylab = "PASI Total Score"
Response = " "

##########################################
#          Part 3: scatter plot          #
# basic functions to create single frame #
##########################################

makescatter <- function(framedata, xvn, yvn, grpvn="grp", tvn="week", shpvc = "titrated", annodata, trtgrp){
  framedata <- filter(framedata, trtp==trtgrp)
  
  # if (trtgrp=='Taltz 80mg every two weeks'){
  #   num=351  ## From intent to to treat population coded in production summary tables
  # }
  # if (trtgrp=='Placebo'){
  #   num= 168 ## From intent to to treat population coded in production summary tables
  # }
  
  if (trtgrp=='Taltz 80mg every two weeks'){
    num=length(data$studyid[which(data$avisitn==2 & data$trtp=="Taltz 80mg every two weeks" & data$studyid %in% c("I1F-MC-RHBA"))])
  }
  if (trtgrp=='Placebo'){
    num=length(data$studyid[which(data$avisitn==2 & data$trtp=="Placebo" & data$studyid %in% c("I1F-MC-RHBA"))])
  }
  
  annodata <- filter(annodata,trtp==trtgrp)
  if(!(exists("group.colors") & exists("group.order") & exists("group.labels"))) stop(
    "At least one group.colors/labels/order does not exist\n")
  if(!exists("xlab")) xlab = xvn
  if(!exists("ylab")) ylab = yvn
  
  #build initial plot statement from x,y & grouping variable names
  basest <- paste("basechart <- ggplot(framedata, 
                  aes(x=",xvn,",y=",yvn, ",fill=",grpvn,  ",shape=", shpvc,")) +
                  coord_cartesian(xlim = c(9, 35.5),ylim = c(1, 35.5))+ 
                  labs(x='',y='')" ,sep="")
  
  basechart<-eval(parse(text=basest))
  
  rm(basest)
  
  # set seed to create the first K duplicate frames
  set.seed(123)
  
  #create plot with data, time var etc...
  
  img<-basechart + 
    geom_rect(xmin = 6, ymin = 34.2, xmax = 35.7, ymax = 35.7, fill = "white", colour = "white") +
    geom_rect(ymin = -2, xmin = 34.2, ymax = 35.7, xmax = 35.7, fill = "white", colour = "white") +
    
    geom_jitter(size=3, alpha=0.6, colour="black")+
    
    ggtitle(paste(trtgrp, ' (N=', num, ')', sep = "")) +
    geom_segment(x = 0, y = 0, xend = 30, yend = 30, colour = grpinfo$col[2]) +
    # geom_segment(x = 0, y = 0, xend = 30, yend = 30*0.5, colour = grpinfo$col[3]) +
    geom_segment(x = 0, y = 0, xend = 30, yend = 30*0.25, colour = grpinfo$col[4]) +   
    geom_segment(x = 0, y = 0, xend = 30, yend = 30*0.1, colour = grpinfo$col[5]) +   
    annotate("text", x=32.3, y=31, label="PASI 0", colour = "#e09d00", fontface="bold", size = 6) +
    # annotate("text", x=32.1, y=16, label="PASI 50", colour = "#6dcc00" , fontface="bold", size = 6) +
    annotate("text", x=32.1, y=8.5, label="PASI 75", colour = "#2ad100", fontface="bold", size = 6) +
    annotate("text", x=32.1, y=4, label="PASI 90", colour = "#1d8c17", fontface="bold", size = 6) +
    
    scale_y_continuous(breaks=c(0,5,10,15,20,25,30,34.95), label=c("0","5","10","15","20","25","30",">30"))+
    scale_x_continuous(breaks=c(5,10,15,20,25,30,34.95), label=c("5","10","15","20","25","30",">30"))+
    
    scale_fill_manual(Response,values=group.colors,
                      labels=group.labels, limits=names(group.colors))+
    scale_shape(solid = FALSE) +
    scale_shape_manual(values=c(21,24), drop=FALSE) +  #this is to force the shape legend to show even there is only 1 value, the variable must be a factor, cannot be a chacter
    #guides(shape=guide_legend(override.aes = list(size=5, colour="black"), title=NULL),
    guides(shape="none",
           colour=FALSE,
           fill = guide_legend(override.aes = list(size=5, shape=21)))+
    #face="bold",
    theme(plot.title = element_text(lineheight=.8,  hjust = 0.5, face = "bold", size=textSizeMajor-5, colour="black"),
          # theme(plot.title = element_text(lineheight=.8,  hjust = 0.5, size=textSizeMinor, colour=(ifelse(trtgrp=='Taltz 80mg every two weeks',"#49A942","#63478B"))),
          # axis.title = element_text(size=rel(1.65)),
          axis.title = element_blank(),
          axis.text.x = element_text(size=rel(1.5)),
          axis.text.y = element_text(size=rel(1.5)),
          # axis.text.x = element_blank(),
          # axis.text.y = element_blank(),
          panel.grid.minor.x = element_blank(),
          strip.text=element_text(size=15), 
          strip.background =element_rect(fill="transparent"),
          legend.text=element_text(size=13),
          legend.background = element_rect(colour = NULL, fill="transparent"),
          legend.position="none", 
          legend.key = element_rect(colour = "transparent", fill = "transparent", size=0.5),
          legend.direction = "horizontal",
          plot.margin = rep(unit(0,"null"),4)) +
    #plot.margin=unit(c(0,20,0,0), "pt") +
    geom_text(data=annodata, aes(x=base, y=aval+2.5, label=annotext), fontface="bold", size=rel(7), color = "black", hjust=0, inherit.aes = FALSE ) +
    geom_text(data=annodata, aes(x=base, y=aval, label=annotext2), fontface="bold", size=rel(7), color = "black", hjust=0, inherit.aes = FALSE) +
    geom_text(data=annodata, aes(x=base, y=aval-2.5, label=annotext3), fontface="bold", size=rel(7), color = "#12008a", hjust=0, inherit.aes = FALSE) 
  #geom_shadowtext (data=annodata, aes(x=base, y=aval, label=annotext), fontface="bold", size=rel(7), color = "black", bgcolor='black', inherit.aes = FALSE )
  return(img);
}

#### original annoation font sizes: geom_text size=rel(7)

##############
#  pie plot  #
##############

## plot.clock function creates a circle and colors it in based on the percentage
## of time consumed thru day.current out of the total day.max

plot.clock <- function(day.current, day.max) {
  #browser()
  require(ggplot2)
  
  dat <- data.frame(d = factor(c(rep(paste(day.current), day.current), 
                                 rep(paste(day.max), day.max - day.current)),
                               levels=unique(c(paste(day.current),paste(day.max)))))
  
  # if day.current is zero, then all the data in the frame above will be
  # day.max and when we plot it, we will grab the first color which will be
  # dark blue. So, we need a special case to ensure that if day.current is 0,
  # we give it the light blue color.
  
  if(day.current == 0) {
    cols = c("light grey")
  } else {
    cols = c("dark blue", "light grey")
  }
  
  pie = ggplot(dat, aes(x = factor(1), fill = factor(d))) +
    geom_bar(width = 1) +
    scale_fill_manual(values = cols) +
    coord_polar(theta = "y", direction = -1) +
    geom_text(aes(y="",label="")) +
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
          #panel.spacing = unit(0,"null"),
          axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.ticks.length = unit(0,"null"),
          #axis.ticks.margin = unit(0,"null"),
          plot.margin = unit(c(0, 0, 0, 0), "npc")
    )
}

#################################
# bar plot function (long bars) # 
#################################

plot.bar <- function(data){
  #browser()
  p <- ggplot(data=data, aes(x=trtp, y=pct, fill=grp)) + 
    geom_bar(stat="identity",colour="transparent",width = 0.7) +
    xlab("") + ylab("% of patients") + scale_fill_manual(values=group.colors)+
    scale_y_continuous(limits = c(0, 100), 
                       breaks=c(0,10,20,30,40,50,60,70,80,90,100),
                       minor_breaks = seq(0 , 100, 5), 
                       expand=c(0,1)) +
    scale_x_discrete(position = "top", expand=c(0,0)) 
  u <- p + theme(
    legend.position = "none",
    panel.background = element_blank(),
    panel.grid.minor.y = element_line(colour="white"),
    panel.grid.major.y = element_line(colour="white"),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.ontop = TRUE,
    panel.margin = unit(0,"null"),
    plot.margin = unit(c(0, 0, 0, 0.02), "npc"),
    #axis.ticks = element_blank(),
    axis.text.x = element_text(size=textSizeMinor-7, vjust=0.2),
    axis.text.y = element_text(size=rel(1.5)),
    axis.title.x = element_blank(),
    #axis.title.y = element_text(size = rel(1.5), angle = 90),
    axis.title.y = element_blank(),
    axis.ticks.length = unit(1,"null"),
    axis.ticks.margin = unit(0,"null"))
  
  return(u)
}


#########################################
# Part 4:  create interpolated data set #
#########################################

### "interpolate.R" is the source function that's used to do the data interpolation;

PROGRAMS  <- "/lillyce/qa/ly2439821/uncover/visual_analytics/programs/animation_10.0/primary/"
tsep="/"
progbar <- "time"
source(paste(PROGRAMS,"interpolate.R",sep=tsep))

## list all variables to keep

# datavars <- c("subjid", "avisitn", "base", "aval", 
#             "pimp","trtp")
respvars <- c("grp")

## check how many rows in dataset has NA value
## if "NA" value was eliminated in part 2, you should get a null vector from the following command, debug your coding if it returned a non null vector;

row.has.na <- apply(data, 1, function(x){any(is.na(x))})
naindex=which(row.has.na==TRUE)
print(naindex)

## only keep data with non-NA value 
# nadata=data[naindex,]

## For interpolation rate chosen the interpolated points between the original 2 consecutive time points; 
## if rate = M, then there will be (M-1) interpolated time points between 2 consecutive time points; 
## we shall use this rate parameter again in part 6;

r=20

data.int <- data[!row.has.na,]

data.int1 <- ddply(data.int,.(subjid),function(x) interpolate(x,"avisitn","aval",rate=r),
                   .progress = progbar)

data.int2 <- data.int1 %>%
  filter(!is.na(aval)) 
range(data.int2$aval)
data.int2 <- mutate(data.int2, aval = ifelse(aval<0, 0, aval))

## Check that interpolated aval is within plausible range
range(data.int2$aval)
data.int2 <- mutate(data.int2, aval = ifelse(aval<0, 0, aval))
head(data.int2)
#mutate(aval = ifelse(aval>60, 60, aval))

# combine interpolated data with observed data to id interpolated obs --------------
obsd <- data[,c("subjid", "avisitn","aval",respvars)]
obsd$interp <- 0
alldat <- merge(data.int2, obsd, by=c("subjid", "avisitn","aval"),
                all.x=TRUE, all.y=TRUE) %>% 
  mutate(interp = 1)

# Merge subject info back in ----------------------------------------------
subjinfo <- unique(data[,c("subjid", "trtp","base")])
interpolated <- merge(alldat, subjinfo, by="subjid") %>%
  mutate(aval = round(aval,1)) %>%
  #mutate(avisitn = round(avisitn,0)) %>%
  mutate(pimp =-(aval - base)/base*100) %>%
  mutate(pimp2=pimp) %>%
  mutate(grp =  case_when(
    pimp < 0 ~ '<0%',
    pimp>=0 & pimp < 50 ~ '0% - <50%',
    pimp >= 50 & pimp < 75  ~ '50% - <75%',
    pimp >= 75 & pimp < 90 ~ '75% - <90%',
    pimp >= 90 & pimp < 100 ~ '90% - <100%',
    pimp >= 100 ~ '100%', 
    TRUE ~ 'WRONG'
  ))  %>%
  mutate(grp=factor(grp,levels=c('<0%','0% - <50%', '50% - <75%','75% - <90%','90% - <100%', '100%'))) %>%
  mutate(trtp=factor(trtp, levels=c("Placebo", "Taltz 80mg every two weeks")),
         titrated = 'pasi') 

# table(interpolated$grp[interpolated$avisitn==2])

interpolated$grp[interpolated$avisitn==2] <- "0% - <50%"

# table(interpolated$grp[interpolated$avisitn==2])


###############################################################
# Part 5: intergrated functions to produce final single frame #
###############################################################

### 3 intergrated functions: interdoplot, sinfig2;
### intergrated functions are based on basic functions,if you made any modification to the "base" functions, need to re-run intergrated functions;

### function interdoplot to create figure with interpolated data and scatter plot

#interdoplot <- function(framedata, xvn, yvn, grpvn="trtp", tvn="day",sumdata,titleindex){
interdoplot <- function(framedata, bardata, xvn, yvn, grpvn="grp", tvn="week", shpvc = "titrated", sumdata,titleindex,currvis,maxvis,annodata){
  scatter1 <- makescatter(framedata, xvn, yvn, grpvn, tvn, shpvc,annodata,trtgrp="Placebo")
  scatter2 <- makescatter(framedata, xvn, yvn, grpvn, tvn, shpvc,annodata,trtgrp="Taltz 80mg every two weeks")
  clock <- plot.clock(currvis, maxvis)
  pctbar = plot.bar(bardata)
  
  # timevar displayed at top of plot - can change to "months" etc... depending on situation
  title <- paste("PASI Response at Week ",titleindex, " (UNCOVER-2)")
  null <- textGrob(" ")
  xaxis.label <- textGrob("Baseline PASI Total Score", gp=gpar(fontsize=textSizeMinor))
  yaxis.label <- textGrob("PASI Total Score", gp=gpar(fontsize=textSizeMinor),rot = 90)
  # footnote <- textGrob("* Administered every two weeks", gp=gpar(fontsize=textSizeMinor-10), hjust = 0.5)
  
  # Create the legend title grob
  legndTitle = textGrob("Percentage Improvement:", gp = gpar(fontsize=textSizeMinor-5))
  
  ###################### New Legend ########################################################
  legndBody <- grobTree(
    gp = gpar(fontsize = textSizeMinor-5, fontface = "bold"), 
    textGrob(label = "<0%", name = "title1",
             x = unit(14.5, "lines"), y = unit(0.85, "lines"), 
             hjust = 0, vjust = 0, gp = gpar(col = group.colors[1])),
    textGrob(label = "[0-50%)", name = "title2",
             x = grobWidth("title1") + unit(15, "lines"), y = unit(0.85, "lines"),
             hjust = 0, vjust = 0, gp = gpar(col = group.colors[2])),
    textGrob(label = "[50-75%)", name = "title3",
             x = grobWidth("title1") + grobWidth("title2") + unit(15.5, "lines"), y = unit(0.85, "lines"),
             hjust = 0, vjust = 0, gp = gpar(col = group.colors[3])),
    textGrob(label = "[75-90%)", name = "title4",
             x = grobWidth("title1") + grobWidth("title2") + grobWidth("title3") + unit(16, "lines"), y = unit(0.85, "lines"),
             hjust = 0, vjust = 0, gp = gpar(col = group.colors[4])),
    textGrob(label = "[90-100%)", name = "title5",
             x = grobWidth("title1") + grobWidth("title2") + grobWidth("title3") + grobWidth("title4") + unit(16.5, "lines"), y = unit(0.85, "lines"),
             hjust = 0, vjust = 0, gp = gpar(col = group.colors[5])),
    textGrob(label = "100%", name = "title6",
             x = grobWidth("title1") + grobWidth("title2") + grobWidth("title3") + grobWidth("title4") + grobWidth("title5") + unit(17, "lines"), y = unit(0.85, "lines"),
             hjust = 0, vjust = 0, gp=gpar(col = group.colors[6]))
  )
  ################################################################################################
  
  barTitle <- textGrob("% of patients", hjust = 0, vjust = 0, x = unit(0, "lines"), y = unit(0.4, "lines"),  gp=gpar(fontsize=textSizeMinor))
  
  img<-grid.arrange(
    arrangeGrob(clock, ncol=1),
    arrangeGrob(sub2=textGrob(title, gp = gpar(fontsize=textSizeMajor, face="plain"),vjust = 0),ncol=1),
    #arrangeGrob(scatter, ncol=1),
    arrangeGrob(yaxis.label, scatter1,
                arrangeGrob(barTitle,pctbar, nrow=2,heights=c(0.05,0.95), top=NULL),
                yaxis.label, scatter2,
                ncol=5, widths=c(0.04, 0.41, 0.1, 0.04, 0.41), nrow=1),
    arrangeGrob(makeblank(),xaxis.label, makeblank(), makeblank(), xaxis.label,ncol=5, widths=c(0.04, 0.41, 0.1, 0.04, 0.41), nrow=1),
    legndBody,
    # arrangeGrob(footnote, ncol=1), 
    heights=c(0.07, 0.05, 0.76, 0.06, 0.06),
    ncol = 1, nrow=5) #,theme(plot.margin=margin(0,0,1,1))
  
  return(img);
}

makeblank<-function(){
  img<-ggplot(data=mtcars,aes(wt,mpg)) +
    theme_void()
  return(img)
}

sinfig1 = function(data, visit, sumdata, titleindex, currvis, maxvis, annodata){
  
  testdata=subset(interpolated,avisitn==visit)
  bardata=filter(pasi_freq, avisitn == visit) %>%
    mutate(trtp= ifelse(trtp=='Placebo', 'PBO', 'IXE'),
           trtp = factor(trtp, levels=c("PBO", "IXE"))) 
  
  testimg <- interdoplot(framedata=testdata, bardata=bardata, xvn="base", yvn="aval", grpvn="grp", shpvc = "titrated",
                         sumdata=sumdata,titleindex=titleindex, currvis=currvis,maxvis=maxvis,annodata=annodata)
  
  print(testimg)
}

################################################################################

###############################################
# Part 6: loops to produce final single frame #
###############################################

### if rate = M (see part 4 for parameter rate), after interpolation, there will be (M-1) extra points between the 2 orginal consecutive time points;
### for example, between Week 0 and Week 1, now there will be Week 1/M, Week 2/M, ... Week (M-1)/M;
### however, the title of the single frame should remain the same until reach the next consecutive point of the original series;
### for example, for the frame of Week 1/M to Week (M-1)/M, title should remain "Week 0";
### thus, multiple time index and title index should be created to correctly label the frame title while using the correct interpolated weekly data;

visind=interpolated$avisitn
visind=unique(visind)

## visind2 contains the week index of the original data set (before interpolation);
## titleindex contains the week index of the title of the final single frame;
## for example, between Week 0 and Week 1, there are 5 interpolated time points: 0.1666667  0.3333333  0.5000000  0.6666667  0.8333333  
## therefore for the frame of Week 0.16667 to Week 0.833333, the title should remain as ".... at Week 0"
## thus the first 6 elements of vector titleindex should be 0;

temp=unique(data$avisitn)
temp <- sort(temp, decreasing = F)
vecdiff=diff(temp)
vecdiff=r*vecdiff
tempindex1=c(rep(temp[1:(length(temp)-1)],vecdiff),temp[length(temp)])
tempindex <- factor(tempindex1)
tempindex <- forcats::fct_recode(tempindex, "0"="2", "1"="3", "2"="4", "4"="5","8"="6","12"="7", "16"="8", "20"="9", "24"="10", "28"="11", "32"="12", "36"="13", "40"="14", "44"="15", "48"="16" )

visind2 = temp
titleindex = as.numeric(levels(tempindex)[as.integer(tempindex)])

###########################
# read in annotation data #
###########################

sumdata1 <- read.csv(paste(sum.loc, "send_uncover_pasi_animation_MMRMsummarystats_RHBA.csv",sep="/"))
names(sumdata1) <- tolower(names(sumdata1))
sumdata1$trtp <- as.character(sumdata1$trtp)
sumdata1$pimp2 <- as.numeric(sumdata1$pimp2)

meandata <- read.csv(paste(sum.loc, "send_uncover_pasi_animation_means_summarystats_RHBA.csv",sep="/"))
names(meandata) <- tolower(names(meandata))
meandata$trtp <- as.character(meandata$trtp)
meandata$pasi <- as.numeric(meandata$pasi)

sumdata1 <- select(sumdata1, trtp, avisitn, week, pimp, pimp2)
meandata <- select(meandata, trtp, avisitn, pasi)

sumdata1 <- merge(sumdata1, meandata)

sumdata1  <- 
  mutate(sumdata1,
         int_vis = floor(avisitn),
         week = case_when(
           int_vis == 3 ~ 1,           int_vis == 4 ~ 2,            int_vis == 5 ~ 4,
           int_vis == 6 ~ 8,           int_vis == 7 ~ 12,           int_vis == 2 ~ 0
         )) %>% 
  mutate(week=factor(week), trtp=ifelse(trtp=='Ixekizumab 80 mg Q2W', 'Taltz 80mg every two weeks','Placebo'), 
         base = 9, #21.5
         aval = 30.5, #32
         pimp = pimp2,
         annotext=paste("PASI Mean % Improv. (", sprintf("%.0f",pimp2), "%) [MMRM]",sep=''),
         annotext2 = paste("Mean Observed PASI (", sprintf("%.1f", pasi), ")",sep=''),
         # annotext2 = paste("Mean Obs. PASI Score (", sprintf("%.1f", pasi), ")",sep=''),
         ## annotext3 from: \lillyce\prd\ly2439821\i1f_mc_rhba\intrm2\output\shared\nonsdd\efficacy\t_pasi75resp_nri_itt_i.rtf (pg 61)
         annotext3 = case_when(
           # avisitn==7 & trtp =='Etanercept' ~ "Primary: PASI75 (41.6%)",
           avisitn==7 & trtp =='Placebo' ~ "Primary: PASI75 (2.4%)",
           avisitn==7 & trtp == 'Taltz 80mg every two weeks' ~ "Primary: PASI75 (89.7%)",
           TRUE ~ " ")
         # endpoint_header = paste("PASI Endpoint(s) @ Week ", week, ":",sep='')
  ) %>% select(-int_vis)

dummy_vis <- data.frame(avisitn_interp=visind) %>%
  mutate(avisitn=floor(avisitn_interp))

sumdata2 <- left_join(dummy_vis,sumdata1, by=c("avisitn")) # %>%
# mutate(annotext=ifelse(avisitn_interp==floor(avisitn_interp),annotext, ' '))

#############################
# prepare scatter plot data #
#############################

interpolated  <- 
  mutate(interpolated, int_vis = floor(avisitn),
         week = case_when(
           int_vis == 3 ~ 1,           int_vis == 4 ~ 2,            int_vis == 5 ~ 4,
           int_vis == 6 ~ 8,           int_vis == 7 ~ 12,           int_vis == 2 ~ 0
         )) %>%
  select(-int_vis)

#########################
# prepare bar plot data #
#########################

# number of duplicate copies of 1st and last frame
pasi_freq <- group_by(interpolated, trtp,avisitn,grp) %>%
  summarise(freq=n()) %>%
  group_by(trtp,avisitn) %>%
  mutate(pct = freq / sum(freq) * 100) %>%
  ungroup()

##################
# produce frames #
##################

pause.frame.num<-35
tot.frame<-length(visind)-length(visind2) + (length(visind2)-1)*pause.frame.num + 2*pause.frame.num
figind2=rep(0,tot.frame)
ff <-  Vectorize(function(x) formatC(x, width = 4, format = "d", flag = "0"))
ind=ff(1:tot.frame)
for (i in 1:tot.frame){figind2[i] = paste("bapasi",ind[i],".JPEG", sep="")}
frame.counter <- 0
# clockindex <- tempindex1-2
############################# Automation to insert for "clockindex" above #############################
## Added to smooth clock function and make proportionate to actual time elapsed (not visits elepsed) ##
temp2=unique(data$sday)
temp2 <- temp2[order(temp2)]
# temp2
vecdiff2=diff(temp2)
# vecdiff2
q=vecdiff2/7  ### generates q __ weeks for study design
# q
## cf for "clock frame"
cf <- (r/7)*(temp2-1)
# cf

clockindex <- 0
for (i in 1:(length(cf)-1)) {
  j=i+1
  clockindex.i <- seq(cf[i]+q[i], cf[j], by=q[i])
  clockindex <- c(clockindex, clockindex.i)
}
# clockindex
#######################################################################################################
for (i in 1:length(visind)) {
  
  paste("Visind is", visind)
  
  if ((visind[i] == floor(visind[i])) & (visind[i] != visind[length(visind)])) {
    for (j in 1:pause.frame.num) {
      frame.counter <- frame.counter+1
      jpeg(filename=file.path(export.loc, figind2[frame.counter]), quality=100,width=1280,height=720,type="cairo")            
      sumdata3 <- filter(sumdata2, avisitn_interp==visind[i] & avisitn > 2)
      a=sinfig1(interpolated,visind[i],summary,titleindex[i],currvis=clockindex[i], maxvis=clockindex[length(clockindex)], annodata=sumdata3);
      print(a)
      
      dev.off()
    }
  }
  
  else if (visind[i] != visind[length(visind)]) {
    frame.counter <- frame.counter+1
    jpeg(filename=file.path(export.loc, figind2[frame.counter]), quality=100,width=1280,height=720,type="cairo")            
    #sumdata3 <- filter(sumdata2, avisitn==floor(visind[i]) & avisitn > 2)
    sumdata3 <- filter(sumdata2, avisitn_interp==visind[i] & avisitn > 2)
    a=sinfig1(interpolated,visind[i],summary,titleindex[i],currvis=clockindex[i], maxvis=clockindex[length(clockindex)], annodata=sumdata3);
    print(a)
    
    dev.off()   
  }
  
  else if (visind[i] == visind[length(visind)]) {
    for (j in 1:(2*pause.frame.num)) {
      frame.counter <- frame.counter+1
      jpeg(filename=file.path(export.loc, figind2[frame.counter]), quality=100,width=1280,height=720,type="cairo")            
      sumdata3 <- filter(sumdata2, avisitn_interp==visind[i] & avisitn > 2)
      a=sinfig1(interpolated,visind[i],summary,titleindex[i],currvis=clockindex[i], maxvis=clockindex[length(clockindex)], annodata=sumdata3);
      print(a)
      
      dev.off()
    }
  }
}



}



# 


rainfall_animator <- function(outpath, delete_choice="No", speed){
  Sys.setenv("PATH" = paste(Sys.getenv("PATH"), outpath, sep=":"))
  
  fps <- as.numeric(speed)
  
  cmd <- paste("/lrlhps/apps/ffmpeg/ffmpeg-3.4.2/ffmpeg"," -y"," -framerate ", fps," -i ",
               paste(outpath,"COARADAR%04d.JPEG",sep="/"),  " -qscale:v 1 -vcodec mpeg4 ",
               paste(outpath,"Rainfall",sep="/"), ".mp4", sep="")
  
  system(cmd)
  
  # Deleting all frames based on user choice
  if(delete_choice == "Yes"){
    path <- paste(outpath, "/", sep="")
    remove.files <- list.files(path, pattern="*.JPEG")
    remove.list <- paste0(path,remove.files)
    invisible(do.call(file.remove,list(remove.list)))
  }
}

