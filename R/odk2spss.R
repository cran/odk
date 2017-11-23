odk2spss <-
function(xlsx, pc.data=NULL, out.path=NULL){
############################################
## odk.xlsx file to sdf, cdf and ddf creater function xl()
xl<-function(xlsx, pc.data){
  requireNamespace(package="openxlsx")
  requireNamespace(package="gsheet")
  sdf<-openxlsx::read.xlsx(xlsx, sheet = 1, cols = c(1:3), skipEmptyRows = TRUE)
  cdf<-openxlsx::read.xlsx(xlsx, sheet = 2, cols = c(1:3), skipEmptyRows = TRUE)
  url<-openxlsx::read.xlsx(xlsx, sheet = 3, cols = 4, rows = 1:2)

if(is.null(pc.data)){
    url<-as.character(url)
    ddf<-gsheet::gsheet2tbl(url)
  }else{
    ddf<-openxlsx::read.xlsx(pc.data)}
  
  return(list(sdf,cdf,ddf, xlsx))
}
#########################

########################### Start spss data read function #########################################
sps.dat<-function(sdf,cdf,ddf){
  ##############
  ddf_s<-NULL;
################
dx1<-names(ddf)[1]
dx2<-nchar(dx1)-3
dx3<-nchar(dx1)
meta<-substr(dx1,dx2,dx3)

if(meta=="ceID"){ddf_s<-ddf[-1]}else{ddf_s<-ddf[-ncol(ddf)]}
 
##############
  a<-cdf_s(cdf)
  
  m<-cbind(as.character(a$v.var),as.character(a$value),as.character(a$v.lebel))
  ##
  sdf.f<-sdf_s(sdf)
 
  name1.5<-as.character(sdf.f$name.var)
  type_s<-as.character(sdf.f$type_s)
  type_s2<-as.character(sdf.f$type_s2)
  type_s3<-as.character(sdf.f$type_s3)
  lebel1.1<-as.character(sdf.f$lebel.sd)
  m1<-matrix(c(name1.5,type_s,type_s2,lebel1.1,type_s3),ncol=5)
  
  mc<-matrix(c(as.character(a$v.var)),ncol=1)
  mul.count<-NULL;
  for(i in 1:length(m1[,5])){
    if(m1[i,5]=="select_multiple")
      mul.count[i]<-count(mc,m1[i,1])
    else
      mul.count[i]<-1;
  }
  ##
  m2<-cbind(m1[,1],mul.count,m1[,5])
  ###
  
  
  #########################
  ######################################
  

  ####################
  #################### make data ready for print #####
  
  
  ################
  ddf_m<-as.matrix(ddf_s)
  d1<-matrix(1:nrow(ddf_m),ncol=1)
  for(i in 1:length(m2[,1])){
    
    if(m2[i,3]!="select_multiple")
      d1<-cbind(d1,ddf_m[,i])
    else{
############# prob#5=2005#####

m20<-mrm(ddf_m[,i],m2[i,2])
row.names(m20)<-paste(1:nrow(m20))

for(i in 1:nrow(m20)){
for(j in 1:ncol(m20)){

if(is.na(m20[i,j])){ttt=NA}else{
m21<-nchar(m20[i,j])
m21<-as.numeric(m21)
if(m21>=3){m20[i,j]<-substr(m20[i,j],m21-1,m21)}
		}
}}

      d1<-cbind(d1,m20)
#######
	  }
  }
d1<-d1[,-1];
  ############### Print data part
  return(
    ########################################
    
    ########
    for(i in 1:nrow(d1)){
      for(j in 1:ncol(d1)){
        if(j==1 && i==1)cat("\n\nBEGIN DATA\n")
        if(j==ncol(d1)){cat(d1[i,j],"\n")}else
          cat(d1[i,j],";")
      }
      if(i==nrow(d1))cat("END DATA\n")
    }
    ################
  )
}
#### End  sps.dat
###############################################################
###################################################
################### print Nominal/Ordinal/Scale function sps.nos()
sps.nos<-function(sdf,cdf){
  ##
  sdf.f<-sdf_s(sdf)
  
  name1.4<-as.character(sdf.f$name.var)
  type_s<-as.character(sdf.f$type_s)
  type_s2<-as.character(sdf.f$type_s2)
  type_s3<-as.character(sdf.f$type_s3)
  lebel1.2<-as.character(sdf.f$lebel.sd)
  m<-matrix(c(name1.4,type_s,type_s2,lebel1.2,type_s3),ncol=5)
  ##############
  a<-cdf_s(cdf)
  mc<-matrix(as.character(a$v.var),ncol=1)
  ##
  mul.count<-NULL;
  for(i in 1:length(m[,5])){
    if(m[i,5]=="select_multiple")
      mul.count[i]<-count(mc,m[i,1])
    else
      mul.count[i]<-1;
  }
  m<-matrix(c(name1.4,type_s,type_s2,lebel1.2,type_s3,mul.count),ncol=6)
  
  ###############

  ##################################
  mm<-NULL;
  nn<-length(m[,6])
  for(i in 1:nn){
    
    if(m[i,5]!="select_multiple"){
      sig<-cbind(m[i,1],m[i,2],m[i,3],m[i,4])
      mm<-rbind(mm,sig)}
    else
    {
      n<-as.numeric(m[i,6])
      mul1<-paste(m[i,1],1:n,sep=".")
      mul2<-rep(m[i,2],n)
      mul3<-rep(m[i,3],n)
      mul4<-rep(m[i,4],n)
      mul<-cbind(mul1,mul2,mul3,mul4)
      mm<-rbind(mm,mul)
    }
  }
  m<-mm;
  ##################################
  return(
    
    ####### VARIABLE LEVEL
    for(i in 1:length(m[,1])){
      if(i==1)
        cat("VARIABLE LEVEL",sep="")
      
      if(i!=length(m[,1]))
        cat("\n/",m[i,1]," ",m[i,3],sep="")
      else
        cat("\n/",m[i,1]," ",m[i,3],".\n \n",sep="")
    } #end_for
    
  )#end_return
  ##
}#end sps.nos
###################################################
################ Print sdf eliments by  sps.ntw() ###################
sps.ntw<-function(sdf,cdf){
  ##
  sdf.f<-sdf_s(sdf)
  
  name1.3<-as.character(sdf.f$name.var)
  type_s<-as.character(sdf.f$type_s)
  type_s2<-as.character(sdf.f$type_s2)
  type_s3<-as.character(sdf.f$type_s3)
  lebel1.3<-as.character(sdf.f$lebel.sd)
  m<-matrix(c(name1.3,type_s,type_s2,lebel1.3,type_s3),ncol=5)
  ##############
  a<-cdf_s(cdf)
  
  mc<-matrix(as.character(a$v.var),ncol=1)
  
  ##
  mul.count<-NULL;
  for(i in 1:length(m[,5])){
    if(m[i,5]=="select_multiple")
      mul.count[i]<-count(mc,m[i,1])
    else
      mul.count[i]<-1;
  }
  m<-matrix(c(name1.3,type_s,type_s2,lebel1.3,type_s3,mul.count),ncol=6)
  
  ###############
  
  #
  ##################################
  mm<-NULL;
  nn<-length(m[,6])
  for(i in 1:nn){
    
    if(m[i,5]!="select_multiple"){
      sig<-cbind(m[i,1],m[i,2],m[i,3],m[i,4])
      mm<-rbind(mm,sig)}
    else
    {
      n<-as.numeric(m[i,6])
      mul1<-paste(m[i,1],1:n,sep=".")
      mul2<-rep(m[i,2],n)
      mul3<-rep(m[i,3],n)
      mul4<-rep(m[i,4],n)
      mul<-cbind(mul1,mul2,mul3,mul4)
      mm<-rbind(mm,mul)
    }
  }
  m<-mm;
  ##################################
  
  return(
    ########## data list
    for(i in 1:length(m[,1])){
      if(i==1)
        cat("data list list (\";\")\n/",sep="")
      
      if(i!=length(m[,1]))
      {
        cat(m[i,1]," ",m[i,2]," ",sep="")
        if((i%%5)==0)cat("\n")
      }
      else
        cat(m[i,1]," ",m[i,2],".\n\n",sep="")
    } #end_for
    
  )#end_return
  ##
}#end sps.ntw


###################################
################## print VARIABLE LABELS function sps.var.l()
sps.var.l<-function(sdf,cdf){
  ##
  sdf.f<-sdf_s(sdf)
  
  name1.2<-as.character(sdf.f$name.var)
  type_s<-as.character(sdf.f$type_s)
  type_s2<-as.character(sdf.f$type_s2)
  type_s3<-as.character(sdf.f$type_s3)
  lebel1.4<-as.character(sdf.f$lebel.sd)
  m<-matrix(c(name1.2,type_s,type_s2,lebel1.4,type_s3),ncol=5)
  ##############
  a<-cdf_s(cdf)
 
  mc<-matrix(as.character(a$v.var),ncol=1)

  ##
  mul.count<-NULL;
  for(i in 1:length(m[,5])){
    if(m[i,5]=="select_multiple")
      mul.count[i]<-count(mc,m[i,1])
    else
      mul.count[i]<-1;
  }
  m<-matrix(c(name1.2,type_s,type_s2,lebel1.4,type_s3,mul.count),ncol=6)
  
  ###############
  
  #
  ##################################
  mm<-NULL;
  nn<-length(m[,6])
  for(i in 1:nn){
    
    if(m[i,5]!="select_multiple"){
      sig<-cbind(m[i,1],m[i,2],m[i,3],m[i,4])
      mm<-rbind(mm,sig)}
    else
    {
      n<-as.numeric(m[i,6])
      mul1<-paste(m[i,1],1:n,sep=".")
      mul2<-rep(m[i,2],n)
      mul3<-rep(m[i,3],n)
      mul4<-rep(m[i,4],n)
      mul<-cbind(mul1,mul2,mul3,mul4)
      mm<-rbind(mm,mul)
    }
  }
  m<-mm;
  ##################################
  return(
    
    ####### VARIABLE LEVELS
    for(i in 1:length(m[,1])){
      if(i==1)
        cat("\nVARIABLE LABELS",sep="")
      
      if(i!=length(m[,1]))
        cat("\n",m[i,1]," \"",m[i,4],"\"",sep="")
      else
        cat("\n",m[i,1]," \"",m[i,4],"\".\n\n",sep="")
    } #end_for
    
  )#end_return
  ##
}#end sps.var.l
##################################################
################## Start sps.vl ############################
sps.vl<-function(sdf,cdf){
  a<-cdf_s(cdf)
  m<-matrix(c(as.character(a$v.var),as.character(a$value),as.character(a$v.lebel)),ncol=3)
  ##
  sdf.f<-sdf_s(sdf)
 
  name1.1<-as.character(sdf.f$name.var)
  type_s<-as.character(sdf.f$type_s)
  type_s2<-as.character(sdf.f$type_s2)
  type_s3<-as.character(sdf.f$type_s3)
  lebel1.5<-as.character(sdf.f$lebel.sd)
  m1<-matrix(c(name1.1,type_s,type_s2,lebel1.5,type_s3),ncol=5)
  mc<-matrix(as.character(a$v.var),ncol=1)
  mul.count<-NULL;
  for(i in 1:length(m1[,5])){
    if(m1[i,5]=="select_multiple")
      mul.count[i]<-count(mc,m1[i,1])
    else
      mul.count[i]<-1;
  }
  ##
  m2<-cbind(m1[,1],mul.count,m1[,5])
  ###
  #########################
  mm<-NULL;
  n1<-length(m[,1])
  i<-1;
  repeat
  {
    if(i>=n1) break;
    n2<-slf(m[i,1],m2[,1])
    
    if(m2[n2,3]!="select_multiple")
    {
      sig<-cbind(m[i,1],m[i,2],m[i,3])
      mm<-rbind(mm,sig);i<-i+1
    }else
    {
      n3<-as.numeric(m2[n2,2])
      mul1<-rep(paste(m[i,1],1:n3,sep="."),each=n3)
      mul2<-rep(m[i:(i+n3-1),2],n3)
      mul3<-rep(m[i:(i+n3-1),3],n3)
      mul<-cbind(mul1,mul2,mul3)
      mm<-rbind(mm,mul);i<-(i+n3)
    }
  }
  
  m<-mm;
  #########################
  return(
    ######### print part###############
    for(i in 1:(length(m[,1])-1))
    {
      if(i==1)
        cat("VALUE LABELS\n",m[1,1],"\n",m[1,2]," '",m[1,3],"'", sep="")
      
      if(m[i,1]==m[i+1,1])
        cat("\n",m[i+1,2]," '",m[i+1,3],"'", sep="")
      else
        cat("\n/",m[i+1,1],"\n",m[i+1,2]," '",m[i+1,3],"'", sep="")
      
      if(i==(length(m[,1])-1))
        cat(".\n\n")
    }
    #########################
  )
}

###### end sps.vl #####################
######################Sub function start##########################
################## sub function 1 ##############################################################
cdf_s<-function(cdf)
{
  names(cdf)[2:3]<-c("value","v.lebel")
  v.var<-c1(cdf$list.name)
  cdf_s<-cbind(cdf,v.var);  
  return(cdf_s[1:4])
}
##################################################
#################### sub function 2 ################################
##### var name separetor for cdf value lebels
c1<-function(x){
  ## x is list.name ##
  x<-as.character(x);
  xx<-unlist(strsplit(x,"_"))
  
  seq<-seq(1,length(xx),2)
  r<-xx[-seq(1,length(xx),2)]
  
  return(r)
}
##################################################
######## sub function 4 ##########################################
########### mul.choose number count function
count<-function(where,what){
  y=0;
  for(i in 1:length(where)){
    if(what==where[i])
      y<-y+1
  }
  return(y)
}
####################################################
####################################################
########## sdf_s() use to build a type column for spss frame

sdf_s<-function(sdf)
{
  names(sdf)[3]<-"lebel.sd"
  names(sdf)[2]<-"name.var"
  type_s<-AF(sdf$type);
  type_s2<-AF2(sdf$type);
  type_s3<-s2(sdf$type);
  sdf_s<-cbind(sdf,type_s,type_s2,type_s3);
  return(sdf_s)
}
##################################################
##################################################
####################### var (ORDINAL)(NOMINAL)(SCALE) selection function

AF2<-function(x)
{
  x<-s2(x);
  for(i in 1:length(x))
  {
    if(any(x[i]==c("text","range","select_one","select_multiple","date","time","geopoint")))
      x[i]<-"(NOMINAL)"
    else if(any(x[i]==c("integer","decimal")))
      x[i]<-"(SCALE)"
    else
      x[i]<-"(ORDINAL)"
  }
  return(x)
}
##################################################
##################################################
################ odk var type to spss var type_s Function

AF<-function(x)
{
  x<-s2(x);
  for(i in 1:length(x))
  {
    if(any(x[i]==c("integer","decimal","range","select_one","select_multiple")))
      x[i]<-"(f8.2)"
    else if(any(x[i]==c("date","time","geopoint")))
      x[i]<-"(A50)"
    else
      x[i]<-"(A200)"
  }
  return(x)
}
###################################################
############ sub function 6&7 #######################################
#####som = select one or multiple identifier function

som<-function(x){
  xx<-unlist(strsplit(x," "))
  return(xx[1])
}
##### function "s2()" take type vector and return spss var. type
s2<-function(x)
{
  x<-as.character(x)
  for(i in 1:length(x))
  {
    if(any(x[i]!=c("integer","range","text","date","time","geopoint")))
      x[i]<-som(x[i])
  }
  return(x)
}
###################################################
###################################################
####################### serial number finder  #####

slf<-function(what,where){
  for(i in 1:length(where)){
    if(what==where[i])
      break;
  }
  return(i)
}
#################################################

  ############### mul.res matrix 
  mrm<-function(v,ncol){
    v<-as.character(v)
    ncol<-as.numeric(ncol)
    d1<-matrix(1:ncol,nrow=1)
    for(i in 1:length(v)){
      
      vv<-as.vector(unlist(strsplit(v[i]," ")))
      vv<-c(vv,rep(NA,(ncol-length(vv))))
      
      d1<-rbind(d1,vv)
    }
    return(d1[-1,])
  }
#####################################################
###################Sub function end##############################

odk<-xl(xlsx, pc.data)
sdf<-odk[[1]]
cdf<-odk[[2]]
ddf<-odk[[3]]

path1<-odk[[4]]
path11<-out.path;

if(is.null(path11)){
name11<-"SPSS_dataframe.sps"
path11 <- file.path(tempdir(), name11)
}else
{
if((dirname(path11))=="."){
name11<-path11;
path11 <- file.path(tempdir(), name11)
}
}
wd1.1<-dirname(path11);

requireNamespace(package="utils")

out1<-utils::capture.output(sps.ntw(sdf,cdf))
out2<-utils::capture.output(sps.nos(sdf,cdf))
out3<-utils::capture.output(sps.var.l(sdf,cdf))
out4<-utils::capture.output(sps.vl(sdf,cdf))
out5<-utils::capture.output(sps.dat(sdf,cdf,ddf))
cat(out1,out2,out3,out4,out5, file = path11, sep="\n")
cat("\n\nSPSS syntax file for SPSS data frame created\nin directory--",wd1.1,
    "\n")
}
