# Eli Lilly and Company (required)-   GSS, DSS
# CODE NAME (required)              : /lrlhps/data/gssttx/ly2439821/animation/ttx1_animation/prd/programs_nonsdd/author_component_modules/bezier/interpolate.R
# PROJECT NAME (required)           : IXE efficacy animation | NA | ly2439821| 
# DESCRIPTION (required)            : interpolation dataset creation program
# SPECIFICATIONS(required)          : /lrlhps/data/gssttx/ly2439821/animation/ttx1_animation/qa/study_documents/ixe_animation_specificaitons.docx
# VALIDATION TYPE (required)        : Peer Review
# INDEPENDENT REPLICATION (required): N/A, validated using Peer Review type
# ORIGINAL CODE (required)          : N/A, this is the original code
# COMPONENT CODE MODULES            : None
# SOFTWARE/VERSION# (required)      : R version 3.2.0; plyr_1.8.2; bezier_1.1 
# INFRASTRUCTURE                    : Platform: x86_64-unknown-linux-gnu (64-bit)
#                                   : Running under: Red Hat Enterprise Linux Server release 6.6 (Santiago)
# DATA INPUT                        : the data passed to the ‘d’ parameter
# OUTPUT                            : interpolate function return dataframe 
# SPECIAL INSTRUCTIONS              : No special instructions
# -------------------------------------------------------------------------------------------------------------------------------	
#   -------------------------------------------------------------------------------------------------------------------------------
#   DOCUMENTATION AND REVISION HISTORY SECTION (required):
#   
#   Author &
#   Ver# Validator        Code History Description
# ---- ----------------     -----------------------------------------------------------------------------------------------------
#   1.0   Simon Cleall (Author)       Original version of the code
# Zak Skrivanek (Validator)

library(bezier)
library(plyr)


#Interpolate Bezier spliine for a single patient
#takes dataframe, returns dataframe, requires names of time var and var to be 
#interpolated as strings
interpolate<-function(d, timevn, intvn, rate=1){
  n<-nrow(d)
  if(n==1) return (NULL)
  
  #modify input data to copy first and last rows
  d0<-d[1,]
  dn<-d[n,]
  da<-rbind(d0,d,dn)
  
  #make matrix of control points (for entire spline)
  r<-seq(1:(n-1))
  praw<-adply(r, 1,function(x){  
    obs<-as.matrix(da[x:(x+3),c(timevn,intvn)])
    cpts<-setControlPts(obs)
    })
  p<-rbind(praw[1,c("x","y")],praw[duplicated(praw$X1),c("x","y")])
  
  
  #see bezier documentation for t values
  #making this a fine mesh by doing 5x number of points than needed in final output.
  t<-seq(0,(n-1),(n-1)/(5*rate*max(d[,timevn])))
  b<-bezier(t=t, p=p, deg=3)
  
  #timepoints to evaluate at
  timepoints<-seq(0,max(d[,timevn]),1/rate)
  m<-approx(b[,1],b[,2],xout=timepoints)
  ret<-data.frame(t(do.call(rbind, m)))
  colnames(ret)<-c(timevn,intvn)
  return(ret)
}

# Determine location of the 2 control points for a single bezier curve --------
setControlPts<-function(obsdat){
  #function setting the 4 control points for the bezier curve that connects obsdat[2,] 
  #to obsdat[3,]
  
  ctrl<-matrix(ncol=2,nrow=4) 
  colnames(ctrl)<-c("x","y")
  
  #1st & 4th contorl points are just the obs data
  ctrl[1,] <- obsdat[2,]
  ctrl[4,] <- obsdat[3,]
  
  cptdist<-CtrlPtDist(obsdat)
  
  #2nd control point is on a line through obsdat[2,] 
  #running parallel to obsdat[3,]-obsdat[1,]
  #cptdist[2] in length from obsdat[2,] (towards obsdat[3,])
  ctrl[2,] = obsdat[2,]+cptdist[2]*(obsdat[3,]-obsdat[1,])
  
  #3rd control point is on a line through obsdat[3,] 
  #running parallel to obsdat[2,]-obsdat[4,] 
  #cptdist[3] in length from obsdat[2,] (towards obsdat[4,])
  ctrl[3,] = obsdat[3,]+cptdist[3]*(obsdat[2,]-obsdat[4,])
  
  return (ctrl)
}

# subfunction to calculate distances and apply rules regarding distance of ctrl points 
# from observed data
CtrlPtDist<-function(obsdat){
  cd<-vector()
  tol <- 0.0000001
  d<-as.matrix(dist(obsdat))
  #default case
  cd[1]<-NA
  cd[2]<-if(d[1,2] < tol) 1/3*d[3,1] else 1/6*d[3,1]
  cd[3]<-if(d[3,4] < tol) 1/3*d[2,4] else 1/6*d[4,2]
  cd[4]<-NA
  #apply limits
  #case 1 : Both too long
  if(1/6*d[3,1]>0.5*d[3,2] & 1/6*d[2,4]>0.5*d[3,2]){
    cd[2]= 0.5*d[3,2] 
    cd[3]= 0.5*d[3,2] 
  } 
  #case2: cd[2] too long (cd[3] OK)
  else if(1/6*d[3,1]>0.5*d[3,2]){
    cd[2]= 0.5*d[3,2] 
    cd[3]= 0.5*d[3,2] *(d[4,2]/d[3,1])
  }
  #case3: cd[3] too long (cd[2] OK)
  else if(1/6*d[2,4]>0.5*d[3,2]){
    cd[2]= 0.5*d[3,2] *(d[3,1]/d[4,2])
    cd[3]= 0.5*d[3,2] 
  }
  #final scaling
  cd[2]<-cd[2]/d[3,1]
  cd[3]<-cd[3]/d[4,2]
  return(cd)
}
