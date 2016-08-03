
#This is pass-analysis modules as used in the presented data, i.e. everything that concerns passing analysis. It ends with export of stats-ready file, for processing with logistic regression.


#2016-07-28 trying with new opta data
rm(list=ls())
library(randomForest)

source("functions.R")
data_dir <-"/home/ubuntu/data/2016-07-26_parsed_data/"
data_dir <-"C:/Users/FOLK/Documents/Work/Analysis/2016-07-26 manchester/"

match<-"803174"
match<-"803165"
load(paste0(data_dir,"2016-07-29_",match,"_chryonhego_opta.rdata"))


time<-c(15,90)
data[,"frame"]<-as.numeric(rownames(data))
match_start<-min(data[,"frame"])
show_start<-time[1]*60*25+match_start
show_end<-time[2]*60*25+match_start
d<-data[data[,"frame"]>show_start & data[,"frame"]<show_end,]
# d<-d[seq(from=1,nrow(d),by=25),]


#just grab the pass and +/- x frames
x<-60
look_at_these<-which(!is.na(d[,"pass_team"]))
look_at_these<-which(d[,"pass_outcome"]%in%"Fail")

colnames(d)
look_at_these_expanded<-vector()
for(look_at_this in look_at_these){
  look_at_these_expanded<-c(look_at_these_expanded,(look_at_this-x):(look_at_this+x))
}
look_at_these_expanded<-unique(look_at_these_expanded)

player_x_col<-grep("[0-9]{1,2}_x",colnames(d),value=T)
player_y_col<-grep("[0-9]{1,2}_y",colnames(d),value=T)
player_team_col<-grep("[0-9]{1,2}_team",colnames(d),value=T)

colours<-c("blue","red","grey50","grey50")
names(colours)<-c("0","1","2","3")

col_ball<-c(rgb(238,0,0,128,maxColorValue = 256),rgb(0,0,238,128,maxColorValue = 256),"grey50")
names(col_ball)<-c("H","A","NA")




# for(i in 1:nrow(d)){
for(i in look_at_these_expanded){
  
  print(i)
  x<-t(d[i,player_x_col])[,1]/100
  y<-t(d[i,player_y_col])[,1]/100
  team<-t(d[i,player_team_col])[,1]
  colours[team]
  
  plotField(xmin= -55,xmax=55,ymin= -68/2,ymax=68/2)  
  
  #plot players
  points(x,y,pch=19,col=colours[team])
  
  if(!is.na(d[i,"pass_outcome"])){
    print(paste("pass at line",i,"with ",d[i,"pass_outcome"]))
    points(d[i,"ball_x"]/100,d[i,"ball_y"]/100,pch=21,cex=2, fg="black",bg="black")
    
    Sys.sleep(1)
  }else{
    points(d[i,"ball_x"]/100,d[i,"ball_y"]/100,pch=21,cex=2, fg="black",bg=col_ball[ as.character(d[i,"ball_team"])])
    
    
    
  }
  
  #plot ball
  
  
  Sys.sleep(0.2)
}














#2016-07-30 12:00 - this is the key component of the per-pass algorithmic processing
rm(list=ls())
# library(randomForest) #run with logistic regression instead

source("functions.R")
data_dir <-"/home/ubuntu/data/2016-07-26_parsed_data/"
data_dir <-"C:/Users/FOLK/Documents/Work/Analysis/2016-07-26 manchester/"
data_dir <-"C:/Users/FOLK/Documents/Work/Analysis/2016-07-26 manchester/"
data_dir <- "//HLU.CORP.LUNDBECK.COM/Users$/HLU/FOLK/Win7-SetupData/Desktop/toTransfer/Miscellanous/2016-07-31_hackmcfc_rawdata/imported/"


digested_files<-grep("^2016-07-30",grep("rdata$",list.files(data_dir),value=T),value=T)

for(digested_file in digested_files){
  match<-sub("^.+_","",sub("_chryon.+$","",digested_file))
  load(paste0(data_dir,digested_file))
  d<-data
  print(match)
  
  
  #setting constants
  frames_before_after<-30 #the frames before and after to grab first
  d_theta <- pi/4 #the max change in ball-theta allowed (probably not used)
  
  
  #creating output frame
  passes<-data.frame(pass_row=which(!is.na(d[,"pass_team"])))
  
  #defining colours
  colours<-c("blue","red","grey50","grey50")
  names(colours)<-c("0","1","2","3")
  col_ball<-c(rgb(238,0,0,128,maxColorValue = 256),rgb(0,238,0,128,maxColorValue = 256),"grey50")
  names(col_ball)<-c("Fail","Succes","NA")
  col_ball_outline<-c("red","green","grey50")
  names(col_ball_outline)<-c("Fail","Succes","NA")
  
  
  # dev.off()
  
  pdf(paste0("2016-07-30_passes_",match,".pdf"))
  for(i in 1:nrow(passes)){
  # for(i in sample(nrow(passes),100)){
      
    #getting data  
    pass_row<-passes[i,"pass_row"] 
    dataHere<-data[(pass_row-frames_before_after):(pass_row+frames_before_after),]
    frame_0<-rownames(dataHere[frames_before_after+1,])
    
    
    #getting outcome
    passes[i,"outcome"]<-dataHere[frame_0,"pass_outcome"]
    
    #getting qualifier data
    if(dataHere[frame_0,"event1_type_id"]%in%"1"){
      e<-"1"
    }else{
      e<-"2"
    }
    q1<-dataHere[frame_0,paste0("event",e,"_qualifiers")]
    q2<-strsplit(q1," // ")[[1]]
    q_id<-sub(":.+","",q2)
    q_val<-sub("^.+:","",q2)
    
    #loop over all qualifiers  
    for(m in 1:length(q_id)){
      q_name<-paste0("qualifier_",q_id[m])
      passes[i,q_name] <- q_val[m]
    }
    
    
    #getting angle and length from opta
    opta_angle<-as.numeric(q_val[q_id%in%"213"])
    opta_length<-as.numeric(q_val[q_id%in%"212"])
    passes[i,"abs_opta_angle"] <- abs(opta_angle)
    
    
    
    #getting ball speed for X frames after pass
    ball_x_before<-dataHere[(frames_before_after-25):(frames_before_after+1),"ball_x"]
    ball_y_before<-dataHere[(frames_before_after-25):(frames_before_after+1),"ball_y"]
    dist_before<-sqrt((ball_x_before[1:24]-ball_x_before[2:25])^2 + (ball_y_before[1:24]-ball_y_before[2:25])^2)
    
    ball_x_after<-dataHere[(frames_before_after+1):(frames_before_after+25),"ball_x"]
    ball_y_after<-dataHere[(frames_before_after+1):(frames_before_after+25),"ball_y"]
    dist_after<-sqrt((ball_x_after[1:24]-ball_x_after[2:25])^2 + (ball_y_after[1:24]-ball_y_after[2:25])^2)

    passes[i,"ball_speed_before_pass"]<-sum(dist_before)/100
    passes[i,"ball_speed_after_pass"]<-sum(dist_after)/100
    
    
        
    # #calculating direction from ch
    for(j in 1:(nrow(dataHere)-1)){
      x_vec<-dataHere[j,"ball_x"]-dataHere[j+1,"ball_x"]
      y_vec<-dataHere[j,"ball_y"]-dataHere[j+1,"ball_y"]
      dataHere[j,"ball_theta"]<-atan2(y_vec,x_vec)
    }

    
    #first try was using the opta_angle to re-align data, but it was to removed from the ch-data
    # dir_0<- opta_angle
    #Instead we calculate it as the mean direction of the first 5 frames of ch data
    dir_0<-mean(dataHere[(frames_before_after+1):(frames_before_after+4),"ball_theta"],na.rm=T)+pi
    
    
    
    #transform to polar coordinates so we can realign all the fields according to pass directions
    pos_cols<-sub("_x$","",grep("x$",colnames(dataHere),value=T))
    for(pos_col in pos_cols){
      x<-dataHere[,paste0(pos_col,"_x")]
      y<-dataHere[,paste0(pos_col,"_y")]
      x<- x-dataHere[frame_0,paste0("ball_x")]
      y<- y-dataHere[frame_0,paste0("ball_y")]
      r <- sqrt((x^2 + y^2))
      t <- atan2(y,x)
      t_new <-t-dir_0+pi/2
      dataHere[,paste0(pos_col,"_theta_new")] <- t_new
      dataHere[,paste0(pos_col,"_r_new")] <- r
      x_new <- r*cos(t_new)
      y_new <- r*sin(t_new)
      dataHere[,paste0(pos_col,"_x_new")] <- x_new
      dataHere[,paste0(pos_col,"_y_new")] <- y_new
      
    }
    
    
    
    #quantify opportunity
    opportunity<-opportunity_quantifier(dataHere, frame_0,dist_cutoff=2000, opposing_player_angle_cutoff=2*pi/36)
    passes[i,"opportunities"]<-opportunity[["opportunities"]]
    
    
    
    
    #initialize plotting
    # png(paste0("pngs/pass_",i,".png"),width=480,height=480*3/4)
    
    dist<-1000
    xlim<-c(dataHere[frame_0,"ball_x_new"]-dist,dataHere[frame_0,"ball_x_new"]+dist)
    ylim<-c(dataHere[frame_0,"ball_y_new"]-dist,dataHere[frame_0,"ball_y_new"]+dist)
    plot(NULL,xlim=xlim,ylim=ylim,xaxt="n",yaxt="n",xlab="",ylab="",frame=T)
    
    #quantify forward player presence
    #drawing forward triangles
    t1<-triangulator(dataHere,frame_0,theta=0.1,r=dist,col=rgb(0.1,0.1,0.1,0.1))
    t2<-triangulator(dataHere,frame_0,theta=0.15,r=dist,col=rgb(0.1,0.1,0.1,0.15))
    
    passes[i,"friendly_players_narrowly_in_front"] <- t1[["friendly"]]
    passes[i,"opposing_players_narrowly_in_front"] <- t1[["opposing"]]
    passes[i,"friendly_players_widely_in_front"] <- t2[["friendly"]]
    passes[i,"opposing_players_widely_in_front"] <- t2[["opposing"]]
    
    front_count<-paste0("Narrow: ",t1[["friendly"]],"/",t1[["opposing"]],", Wide: ",t2[["friendly"]],"/",t2[["opposing"]])
    
    #plotting last 25 frames of ball as solid line
    lines(
      x=dataHere[(frames_before_after-25):(frames_before_after+1),"ball_x_new"],
      y=dataHere[(frames_before_after-25):(frames_before_after+1),"ball_y_new"])
    
    #drawing future 25 frames of ball as dotted line
    lines(
      x=dataHere[(frames_before_after+1):(frames_before_after+25),"ball_x_new"],
      y=dataHere[(frames_before_after+1):(frames_before_after+25),"ball_y_new"],lty=3,lwd=2)
    
    
    points(dataHere[frame_0,"ball_x_new"],dataHere[frame_0,"ball_y_new"],pch=21,cex=1.2, fg=col_ball_outline[dataHere[frame_0,"pass_outcome"]],bg=col_ball[dataHere[frame_0,"pass_outcome"]])  
    
    
    #drawing players
    for(pos_col in pos_cols){
      if(pos_col=="ball")next
      x<-dataHere[frame_0,paste0(pos_col,"_x_new")]
      y<-dataHere[frame_0,paste0(pos_col,"_y_new")]
      team<-as.character(dataHere[frame_0,paste0(pos_col,"_team")])
      points(x,y,col=colours[team],pch=19)
      # text(x,y,label=pos_col,cex=2)
      
      #drawing last 25 frames of players
      x_path<-dataHere[(frames_before_after-25):(frames_before_after+1),paste0(pos_col,"_x_new")]
      y_path<-dataHere[(frames_before_after-25):(frames_before_after+1),paste0(pos_col,"_y_new")]
      points(x_path,y_path,col=colours[team])
    }
    
    
    #drawing opportunities
    for(op in names(opportunity[["opportunity_players"]])){
      x3<-opportunity[["opportunity_players"]][[op]][["x"]]
      y3<-opportunity[["opportunity_players"]][[op]][["y"]]
      lines(c(0,x3),c(0,y3),col="grey50",lty=2)
    }
    
    #adding text info
    
    mtext(paste("Pass outcome:",dataHere[frame_0,"pass_outcome"]),adj=0,cex=0.6,padj=-5)
    mtext(paste("Frame:",i,frame_0, "ball x:" , dataHere[frame_0,"ball_x"],"y:",dataHere[frame_0,"ball_x"]),adj=0,cex=0.6,padj=-4)
    mtext(paste("Front players:",front_count),adj=0,cex=0.6,padj=-3)
    
    mtext(paste("Opta pass length:",opta_length,"yards"),adj=0,cex=0.6,padj=-2)
    mtext(paste("Pitch zone:",passes[i,"qualifier_56"]),adj=0,cex=0.6,padj=-1)
    mtext(paste("Opportunity:",opportunity[["opportunities"]]),adj=0,cex=0.6,padj=0)
    
    
    
  }
  dev.off()
  
  
  write.table(passes,file=paste0("2016-07-30_1834_for_",match,"_stats.txt"),col.names=NA,sep="\t")
  
}



try(rm("out"))
for(digested_file in digested_files){
  match<-sub("^.+_","",sub("_chryon.+$","",digested_file))
  d<-read.table(paste0(data_dir,"/2016-07-30_1834_for_",match,"_stats.txt"),sep="\t",header=T)
  d[,"match"]<-match
  
  if(exists("out")){
    d[,colnames(out)[!colnames(out)%in%colnames(d)]]<-NA
    
    out<-rbind(out,d[,colnames(out)])
  }else{
    out<-d
  }
  
}

write.table(out,file=paste0("2016-07-30_213_for_merged_stats.txt"),col.names=NA,sep="\t")













