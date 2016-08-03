

#parsing data
#The purpose of this scripts is to parse both the chryonhego and opta data, and merge them. The main output will be an R-format file, with all events by frame
# 
# rm(list=ls())
# library(XML)
# 
# data_dir <-"/home/ubuntu/data/"
# data_dir <-"C:/Users/FOLK/Documents/Work/Analysis/2016-07-26 manchester/"
# 
# 
# type<-"Chryonhego"
# 
# game<-"02 - City vs Chelsea (3-0)"
# file<-"803174_Man City-Chelsea.dat"
# 
# # game<-"01 - West Brom vs City (0-3)"
# # file<-"803165_West Brom-Man City.dat"
# 
# 
# head(data)
# 
# data<-read.table(paste0(data_dir,type,"/",game,"/",file),stringsAsFactors = F,sep=":",row.names=1)
# 
# 
# 
# data<-read.table(paste0(data_dir,type,"/",game,"/",file),stringsAsFactors = F,sep=":",row.names=1)
# 
# 
# data[,3]<-NULL
# colnames(data)<-c("players","ball")
# by_player<-t(apply(data[,"players",drop=F],1,function(x){strsplit(x,";")[[1]]}))
# 
# for(j in 1:ncol(by_player)){
#   p<-as.data.frame(t(apply(by_player[,j,drop=F],1,function(x){strsplit(x,",")[[1]]})),stringsAsFactors = F)
#   colnames(p)<-c("team","sys_id","jersey","x","y","speed")
#   
#   #replacing -1 with NA
#   for(i in 1:ncol(p)){
#     p[p[,i]=="-1",i] <- NA
#   }
#   
#   #converting to numeric
#   for(numeric in c("x","y","speed")){
#     p[,numeric] <- as.numeric(p[,numeric])  
#   }
#   
#   #converting to factor
#   for(factor in c("team","sys_id","jersey")){
#     p[,factor] <- as.factor(p[,factor])  
#   }
#   
#   #checking sys_id ok and then renaming cols, then merging in
#   if( length(levels(p[,"sys_id"]))!=1)stop("")
#   print(paste("Merging in player",levels(p[,"sys_id"])))
#   colnames(p)<-paste0(levels(p[,"sys_id"]),"_",colnames(p))
#   data<-cbind(data,p)
# }
# #removing source
# data[,"players"]<-NULL
# 
# 
# 
# #ball handling - first add an NA value when dev info is missing
# b1<-apply(data[,"ball",drop=F],1,function(x){strsplit(x,",")[[1]]})
# ball_data_length<-apply(data[,"ball",drop=F],1,function(x){length(strsplit(x,",")[[1]])})
# data[ball_data_length%in%6,"ball"]<-sub(";",",NA;",data[ball_data_length%in%6,"ball"])
# 
# #then convert to data frame and name columns
# ball<-as.data.frame(t(apply(data[,"ball",drop=F],1,function(x){strsplit(x,",")[[1]]})),stringsAsFactors = F)
# colnames(ball)<-c("ball_x","ball_y","ball_z","ball_speed","ball_team","ball_status","ball_contact_device")
# 
# #remove trailing ; and correct NA's
# ball[,"ball_contact_device"]<-sub(";$","",ball[,"ball_contact_device"])
# ball[ball[,"ball_contact_device"]%in%"NA","ball_contact_device"]<-NA
# 
# #converting to numeric
# for(numeric in c("ball_x","ball_y","ball_z","ball_speed")){
#   ball[,numeric] <- as.numeric(ball[,numeric])  
# }
# 
# #converting to factor
# for(factor in c("ball_team","ball_status")){
#   ball[,factor] <- as.factor(ball[,factor])  
# }
# 
# #merging in and removing source
# data<-cbind(data,ball)
# data[,"ball"] <- NULL
# 
# 
# 
# #add meta data
# xml_path<-paste0(data_dir,type,"/",game,"/", sub("\\.dat$","_metadata.xml",file))
# meta_data<-xmlToList(xml_path)
# 
# 
# #add timestamp data
# timestamp_path<-paste0(data_dir,type,"/",game,"/", sub("_.+$","",file),"-timestamp.txt")
# timestamps<-read.table(timestamp_path,header=F,stringsAsFactors = F)
# timestamps[,"time"]<-sub(":[0-9]+$","",timestamps[,1])
# timestamps[,"frame_id"]<-sub("^.+\\:","",timestamps[,1])
# rownames(timestamps) <- timestamps[,"frame_id"]
# 
# if(nrow(timestamps) != nrow(data))stop("Must be equal content")
# if(!all(rownames(timestamps) == rownames(data)))stop("Must be equal content")
# #note that this fails sometimes if time-stamp has headers or not (must handle manually)
# 
# data[,"frame_id"] <- as.numeric(timestamps[,"frame_id"])
# data[,"time"] <-timestamps[,"time"]
# 
# 
# save(data,meta_data,file=paste0("2016-07-28_",sub("_.+$","",file),"_chryonhego.rdata"))
# 
# 
# 
# 
# #2016-07-28 parsing opta data
# rm(list=ls())
# library(XML)
# 
# data_dir <-"/home/ubuntu/data/"
# data_dir <-"C:/Users/FOLK/Documents/Work/Analysis/2016-07-26 manchester/"
# 
# #iterate over these two
# match<-"803174"
# match<-"803165"
# load(paste0(data_dir,"2016-07-28_",match,"_chryonhego.rdata"))
# 
# 
# 
# xml_path<-paste0(data_dir,"opta/f24-8-2015-",match,"-eventdetails.xml")
# xml_file<-xmlToList(xml_path)
# # attributes<-xml_file[["Game"]][[".attrs"]]
# 
# #we want to be accurate to one tenth of a second
# data[,"time_one_decimal"]<-substr(data[,"time"],1,10)
# data[,"event2_id"]<-data[,"event1_id"]<-NA
# 
# 
# sk_ev<-0
# for(i in 1:sum(names(xml_file[["Game"]])%in%"Event")){
#   
#   
#   #diving the events with or without qualifiers
#   if(!".attrs"%in%names(xml_file[["Game"]][[i]])){
#     qualifiers<-""
#     event<-xml_file[["Game"]][[i]]
#   }else{
#     qualifiers<-paste(sapply(xml_file[["Game"]][[i]][names(xml_file[["Game"]][[i]])%in%"Q"],function(x){paste(x[c("qualifier_id","value")],collapse=":")}),collapse=" // ")
#     event<-xml_file[["Game"]][[i]][[".attrs"]]
#   }
#   
#   #getting opta time from timestamp  
#   game_sec<-event[["sec"]]
#   game_min<-event[["min"]]
#   
#   
#   timestamp<-event[["timestamp"]]
#   seconds<-sub("^.+T[0-9]+:[0-9]+:","",timestamp)
#   minutes<-sub(":.+","",sub("^.+T[0-9]+:","",timestamp))
#   hour<-sub(":.+","",sub("^.+T","",timestamp))
#   #not same time-zone it seems
#   hour<-as.character(as.numeric(hour)+1)
#   seconds_one_decimal <- substr(seconds,1,4)
#   opta_time<-paste(hour,minutes,seconds_one_decimal,sep=":")
#   ch_time<-which(data[,"time_one_decimal"]%in%opta_time)
#   if(length(ch_time)==0){
#     #if this is true, then the opta event is outside of the ch scope and therefore will be ignored
#     print(paste("Skipping opta event at",opta_time,"game time:",game_min,game_sec,"- no corresponding ch time point"))
#     next
#   }else if(length(ch_time)>1){ #if more than one match, just pick earliest (still accurate to 1 decimal of a second)
#     ch_time <- ch_time[1]
#   }
#   
#   
#   #ok, so insert the event
#   if(is.na(data[ch_time,"event1_id"])){
#     data[ch_time,"event1_id"]<-event[["event_id"]]
#     data[ch_time,"event1_type_id"]<-event[["type_id"]]
#     data[ch_time,"event1_outcome"]<-event[["outcome"]]
#     data[ch_time,"event1_team_id"]<-event[["team_id"]]
#     data[ch_time,"event1_qualifiers"]<-qualifiers
#     
#   }else{
#     if(is.na(data[ch_time,"event2_id"])){
#       data[ch_time,"event2_id"]<-event[["event_id"]]
#       data[ch_time,"event2_type_id"]<-event[["type_id"]]
#       data[ch_time,"event2_outcome"]<-event[["outcome"]]
#       data[ch_time,"event2_team_id"]<-event[["team_id"]]
#       data[ch_time,"event2_qualifiers"]<-qualifiers
#     }else{
#       sk_ev<-sk_ev+1
#     }
#   }
# }
# 
# 
# print(paste("Skipped",sk_ev,"events completely because there was more than 2 at the same time point. If this number is larger than a handful, it should be investigated"))
# 
# 
# #Adding pass-info
# passes<-c(which(data[,"event1_type_id"]%in%"1"),which(data[,"event2_type_id"]%in%"1"))
# for(pass in passes){
#   #is it event 1 or 2 at this time point
#   if(data[pass,"event1_type_id"]%in%"1"){
#     e<-"1"
#   }else{
#     e<-"2"
#   }
#   #check qualifiers and 
#   q1<-data[pass,paste0("event",e,"_qualifiers")]
#   q2<-strsplit(q1," // ")[[1]]
#   q_id<-sub(":.+","",q2)
#   q_val<-sub("^.+:","",q2)
#   if(any(q_id %in% c("2", "5", "6", "107",  "123","124"))){
#     #in these cases it is not actually a pass
#     print(paste("Skipping pass",pass,"because it had qualifier",q1))
#     next
#   }
#   data[pass,"pass_team"]<-data[pass,paste0("event",e,"_team_id")]
#   if(data[pass,paste0("event",e,"_outcome")] == "0"){
#     data[pass,"pass_outcome"]<- "Fail" 
#   }else{
#     data[pass,"pass_outcome"]<- "Succes"  
#   }
# }
# 
# 
# 
# 
# 
# 
# 
# 
# # #re-calculating speed with splines
# # x<-data[,"ball_x"]
# # y<-data[,"ball_y"]
# # calculated_ball_speed<-((x[2:(nrow(data))] - x[1:(nrow(data)-1)])^2 + (y[2:(nrow(data))] - y[1:(nrow(data)-1)])^2)^0.5
# # data[2:nrow(data),"calculated_ball_speed"] <- calculated_ball_speed
# # 
# # library(KernSmooth)
# # 
# # bkde(data[,"calculated_ball_speed"], kernel = "normal", canonical = FALSE, bandwidth=6)
# # 
# # 
# # hist(data[,"calculated_ball_speed"])
# # hist(y)
# # ksmooth()
# # ??KernSmooth
# save(data,meta_data,file=paste0("2016-07-29_",match,"_chryonhego_opta.rdata"))
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# #output as txt
# rm(list=ls())
# source("functions.R")
# data_dir <-"/home/ubuntu/data/2016-07-26_parsed_data/"
# data_dir <-"C:/Users/FOLK/Documents/Work/Analysis/2016-07-26 manchester/"
# 
# for(match in c("803174","803165")){
#   
#   
#   load(paste0(data_dir,"2016-07-29_",match,"_chryonhego_opta.rdata"))
#   
#   filename<-paste0(data_dir,"2016-07-29_",match,"_chryonhego_opta.txt")
#   # gz1 <- gzfile(filename,"w" )
#   write.table(data,file=filename,col.names=NA,sep="\t")
#   # close(gz1)
# }
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 




















































#2016-07-30 parsing data - this is the script used for getting in all 10 matches
rm(list=ls())
library(XML)

data_dir <-"/home/ubuntu/data/"
# data_dir <-"C:/Users/FOLK/Documents/Work/Analysis/2016-07-26 manchester/"
data_dir<-"C:/Users/FOLK/Documents/Work/Analysis/2016-07-26 manchester/import_data/"

type<-"Chryonhego"

games<-sub("_tracabRaw","",list.files(paste0(data_dir,type)))

for(game in games){
  print(game)
  
  file<-grep("\\.dat$",list.files(paste0(data_dir,type,"/",game,"_tracabRaw/")),value=T)
  
  
  data<-read.table(paste0(data_dir,type,"/",game,"_tracabRaw/",file),stringsAsFactors = F,sep=":",row.names=1)
  
  
  
  # data<-read.table(paste0(data_dir,type,"/",game,"/",file),stringsAsFactors = F,sep=":",row.names=1)
  
  
  data[,3]<-NULL
  colnames(data)<-c("players","ball")
  by_player<-t(apply(data[,"players",drop=F],1,function(x){strsplit(x,";")[[1]]}))
  
  for(j in 1:ncol(by_player)){
    p<-as.data.frame(t(apply(by_player[,j,drop=F],1,function(x){strsplit(x,",")[[1]]})),stringsAsFactors = F)
    colnames(p)<-c("team","sys_id","jersey","x","y","speed")
    
    #replacing -1 with NA
    for(i in 1:ncol(p)){
      p[p[,i]=="-1",i] <- NA
    }
    
    #converting to numeric
    for(numeric in c("x","y","speed")){
      p[,numeric] <- as.numeric(p[,numeric])  
    }
    
    #converting to factor
    for(factor in c("team","sys_id","jersey")){
      p[,factor] <- as.factor(p[,factor])  
    }
    
    #checking sys_id ok and then renaming cols, then merging in
    if( length(levels(p[,"sys_id"]))!=1)stop("")
    print(paste("Merging in player",levels(p[,"sys_id"])))
    colnames(p)<-paste0(levels(p[,"sys_id"]),"_",colnames(p))
    data<-cbind(data,p)
  }
  #removing source
  data[,"players"]<-NULL
  
  
  
  #ball handling - first add an NA value when dev info is missing
  b1<-apply(data[,"ball",drop=F],1,function(x){strsplit(x,",")[[1]]})
  ball_data_length<-apply(data[,"ball",drop=F],1,function(x){length(strsplit(x,",")[[1]])})
  data[ball_data_length%in%6,"ball"]<-sub(";",",NA;",data[ball_data_length%in%6,"ball"])
  
  #then convert to data frame and name columns
  ball<-as.data.frame(t(apply(data[,"ball",drop=F],1,function(x){strsplit(x,",")[[1]]})),stringsAsFactors = F)
  colnames(ball)<-c("ball_x","ball_y","ball_z","ball_speed","ball_team","ball_status","ball_contact_device")
  
  #remove trailing ; and correct NA's
  ball[,"ball_contact_device"]<-sub(";$","",ball[,"ball_contact_device"])
  ball[ball[,"ball_contact_device"]%in%"NA","ball_contact_device"]<-NA
  
  #converting to numeric
  for(numeric in c("ball_x","ball_y","ball_z","ball_speed")){
    ball[,numeric] <- as.numeric(ball[,numeric])  
  }
  
  #converting to factor
  for(factor in c("ball_team","ball_status")){
    ball[,factor] <- as.factor(ball[,factor])  
  }
  
  #merging in and removing source
  data<-cbind(data,ball)
  data[,"ball"] <- NULL
  
  
  
  #add meta data
  
  xml_path<-grep("\\.xml$",list.files(paste0(data_dir,type,"/",game,"_tracabRaw/")),value=T)
  xml_path2<-paste0(data_dir,type,"/",game,"_tracabRaw/", xml_path)
  meta_data<-xmlToList(xml_path2)
  
  
  #add timestamp data
  # timestamp_path<-paste0(data_dir,type,"/",game,"/", sub("_.+$","",file),"-timestamp.txt")
  # timestamps<-read.table(timestamp_path,header=F,stringsAsFactors = F)
  # timestamps[,"time"]<-sub(":[0-9]+$","",timestamps[,1])
  # timestamps[,"frame_id"]<-sub("^.+\\:","",timestamps[,1])
  # rownames(timestamps) <- timestamps[,"frame_id"]
  
  
  date<-meta_data[["match"]][[".attrs"]][["dtDate"]]
  startFrame<-as.numeric(rownames(data)[1])
  data[,"frame_number"]<-as.numeric(rownames(data))-startFrame
  date2<-strptime(date,format="%Y-%m-%d %H:%M:%S")
  
  data[,"time2"]<-date2+data[,"frame_number"]/25
  data[,"tenth_of_a_second"]<-data[,"frame_number"]/25-floor(data[,"frame_number"]/25)
  data[,"time"]<-paste0(format(data[,"time2"],"%H:%M:%S"),substr(as.character(data[,"tenth_of_a_second"]),2,3))
  
  
  
  
  
  
  opta_files<-grep(game,list.files(paste0(data_dir,"Opta")),value=T)
  xml_path3<-paste0(data_dir,"Opta/",grep("eventdetails",opta_files,value=T))
  xml_file<-xmlToList(xml_path3)
  
  head(data[,"time"])
  # substr(data[,"time"],1,10)
  #we want to be accurate to one tenth of a second
  data[,"time_one_decimal"]<-substr(data[,"time"],1,10)
  data[,"event2_id"]<-data[,"event1_id"]<-NA
  
  
  sk_ev<-0
  for(i in 1:sum(names(xml_file[["Game"]])%in%"Event")){
    
    
    #diving the events with or without qualifiers
    if(!".attrs"%in%names(xml_file[["Game"]][[i]])){
      qualifiers<-""
      event<-xml_file[["Game"]][[i]]
    }else{
      qualifiers<-paste(sapply(xml_file[["Game"]][[i]][names(xml_file[["Game"]][[i]])%in%"Q"],function(x){paste(x[c("qualifier_id","value")],collapse=":")}),collapse=" // ")
      event<-xml_file[["Game"]][[i]][[".attrs"]]
    }
    
    #getting opta time from timestamp  
    game_sec<-event[["sec"]]
    game_min<-event[["min"]]
    
    
    timestamp<-event[["timestamp"]]
    seconds<-sub("^.+T[0-9]+:[0-9]+:","",timestamp)
    minutes<-sub(":.+","",sub("^.+T[0-9]+:","",timestamp))
    hour<-sub(":.+","",sub("^.+T","",timestamp))
    #not same time-zone it seems
    hour<-as.character(as.numeric(hour)+1)
    seconds_one_decimal <- substr(seconds,1,4)
    opta_time<-paste(hour,minutes,seconds_one_decimal,sep=":")
    ch_time<-which(data[,"time_one_decimal"]%in%opta_time)
    if(length(ch_time)==0){
      #if this is true, then the opta event is outside of the ch scope and therefore will be ignored
      print(paste("Skipping opta event at",opta_time,"game time:",game_min,game_sec,"- no corresponding ch time point"))
      next
    }else if(length(ch_time)>1){ #if more than one match, just pick earliest (still accurate to 1 decimal of a second)
      ch_time <- ch_time[1]
    }
    
    
    #ok, so insert the event
    if(is.na(data[ch_time,"event1_id"])){
      data[ch_time,"event1_id"]<-event[["event_id"]]
      data[ch_time,"event1_type_id"]<-event[["type_id"]]
      data[ch_time,"event1_outcome"]<-event[["outcome"]]
      data[ch_time,"event1_team_id"]<-event[["team_id"]]
      data[ch_time,"event1_qualifiers"]<-qualifiers
      
    }else{
      if(is.na(data[ch_time,"event2_id"])){
        data[ch_time,"event2_id"]<-event[["event_id"]]
        data[ch_time,"event2_type_id"]<-event[["type_id"]]
        data[ch_time,"event2_outcome"]<-event[["outcome"]]
        data[ch_time,"event2_team_id"]<-event[["team_id"]]
        data[ch_time,"event2_qualifiers"]<-qualifiers
      }else{
        sk_ev<-sk_ev+1
      }
    }
  }
  
  
  print(paste("Skipped",sk_ev,"events completely because there was more than 2 at the same time point. If this number is larger than a handful, it should be investigated"))
  
  
  #Adding pass-info
  passes<-c(which(data[,"event1_type_id"]%in%"1"),which(data[,"event2_type_id"]%in%"1"))
  for(pass in passes){
    #is it event 1 or 2 at this time point
    if(data[pass,"event1_type_id"]%in%"1"){
      e<-"1"
    }else{
      e<-"2"
    }
    #check qualifiers and 
    q1<-data[pass,paste0("event",e,"_qualifiers")]
    q2<-strsplit(q1," // ")[[1]]
    q_id<-sub(":.+","",q2)
    q_val<-sub("^.+:","",q2)
    if(any(q_id %in% c("2", "5", "6", "107",  "123","124"))){
      #in these cases it is not actually a pass
      print(paste("Skipping pass",pass,"because it had qualifier",q1))
      next
    }
    data[pass,"pass_team"]<-data[pass,paste0("event",e,"_team_id")]
    if(data[pass,paste0("event",e,"_outcome")] == "0"){
      data[pass,"pass_outcome"]<- "Fail" 
    }else{
      data[pass,"pass_outcome"]<- "Succes"  
    }
  }
  
  
  
  
  
  save(data,meta_data,file=paste0("2016-07-30_",game,"_chryonhego_opta.rdata"))
  
}












#output as txt
rm(list=ls())
source("functions.R")
data_dir <-"/home/ubuntu/data/2016-07-26_parsed_data/"
data_dir <-"C:/Users/FOLK/Documents/Work/Analysis/2016-07-26 manchester/"

for(match in c("803174","803165")){
  
  
  load(paste0(data_dir,"2016-07-29_",match,"_chryonhego_opta.rdata"))
  
  filename<-paste0(data_dir,"2016-07-29_",match,"_chryonhego_opta.txt")
  # gz1 <- gzfile(filename,"w" )
  write.table(data,file=filename,col.names=NA,sep="\t")
  # close(gz1)
}
