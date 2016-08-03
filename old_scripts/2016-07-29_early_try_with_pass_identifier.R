

rm(list=ls())
# library(XML)
# readKeyValueDB(doc, ...)

data_dir <-"/home/ubuntu/data/2016-07-26_parsed_data/"
data_dir <-"C:/Users/FOLK/Documents/Work/Analysis/2016-07-26 manchester/"

match<-"803174"

match<-"803165"
load(paste0(data_dir,"2016-07-26_",match,"chryonhego.rdata"))


#identifying passes
player_speed_col<-grep("[0-9]{1,2}_speed",colnames(data),value=T)
hist(
  stack(data[,player_speed_col])[,1],
  breaks=100,main="Histogram of player speeds",ylim=c(0,500000),xlab="speed (m/s)")

min_run_speed<-1
max_run_speed<-15
abline(v=min_run_speed,lwd=2,col="blue")
abline(v=max_run_speed,lwd=2,col="red")


hist(data[,"ball_speed"],breaks=100)
#ok clearly some impossible ball speeds going on here

#try to insert a calculated ball-speed
x<-data[,"ball_x"]
y<-data[,"ball_y"]
calculated_ball_speed<-((x[2:(nrow(data))] - x[1:(nrow(data)-1)])^2 + (y[2:(nrow(data))] - y[1:(nrow(data)-1)])^2)^0.5

data[2:nrow(data),"calculated_ball_speed"] <- calculated_ball_speed

#playing around with smoothing (later abandoned)
# plot(data[,"ball_speed"],data[,"calculated_ball_speed"])
# #smoothing values
# plot(ksmooth(data[,"ball_speed"],data[,"calculated_ball_speed"],bandwidth = 40),xlab="ball speed",ylab="calculated ball speed",xlim=c(0,500),ylim=c(5,800/25))


#making assumptions on ball speeds
max_possible_ball_speed <- 300
max_possible_ball_speed <- 100
data[!is.na(data[,"ball_speed"]) & data[,"ball_speed"] > max_possible_ball_speed,"ball_speed"]<-NA

hist(data[,"ball_speed"],breaks=100,main="ball speeds")

typical_max_pass_speed<-32
abline(v=max_run_speed,lwd=2,col="red")
abline(v=typical_max_pass_speed,lwd=2,col="blue")


data[,"ball_state"]<-"other state"
p1<-which(!is.na(data[,"ball_speed"]) & data["ball_speed"] > max_run_speed )
data[p1,"ball_state"] <- "being passed"


p2<-which(!is.na(data[,"ball_speed"]) & data["ball_speed"] > min_run_speed & data[,"ball_speed"] <= max_run_speed)
data[p2,"ball_state"] <- "being dribbled"


p3<-which(!is.na(data[,"ball_speed"]) & data["ball_speed"] <= min_run_speed )
data[p3,"ball_state"] <- "stopped"

# p4<-which(!is.na(data[,"ball_speed"]) & data["ball_speed"] > typical_max_pass_speed )
# data[p4,"ball_state"] <- "being passed"



# data[data[,"ball_state"]%in%"other state",][10000:10020,]

table(data[,"ball_state"])


#visualizing
# time<-sample(27:40,1)
time<-c(27,28)
data[,"frame"]<-as.numeric(rownames(data))
match_start<-min(data[,"frame"])
show_start<-time[1]*60*25+match_start
show_end<-time[2]*60*25+match_start
d<-data[data[,"frame"]>show_start & data[,"frame"]<show_end,]
# d<-d[seq(from=1,nrow(d),by=25),]

player_x_col<-grep("[0-9]{1,2}_x",colnames(d),value=T)
player_y_col<-grep("[0-9]{1,2}_y",colnames(d),value=T)
player_team_col<-grep("[0-9]{1,2}_team",colnames(d),value=T)

colours<-c("blue","red","grey50","grey50")
names(colours)<-c("0","1","2","3")

col_ball<-c("orange","darkgreen","orange","grey50")
names(col_ball)<-c("being dribbled","being passed","stopped","other state")


for(i in 1:nrow(d)){
  x<-t(d[i,player_x_col])[,1]/100
  y<-t(d[i,player_y_col])[,1]/100
  team<-t(d[i,player_team_col])[,1]
  colours[team]
  
  plotField(xmin= -55,xmax=55,ymin= -68/2,ymax=68/2)  
  
  #plot players
  points(x,y,pch=19,col=colours[team])
  
  #plot ball
  if(d[i,"ball_status"]%in%"Alive"){
    points(d[i,"ball_x"]/100,d[i,"ball_y"]/100,pch=21,cex=2, fg="black",bg=col_ball[d[i,"ball_state"]])
  }else{
    points(d[i,"ball_x"]/100,d[i,"ball_y"]/100,cex=1)
    
  }
  
  Sys.sleep(0.1)
}




# 
# 
# 
# 
# #read xml
# xml_path<-paste0(data_dir,"opta/f24-8-2015-",match,"-eventdetails.xml")
# xml_file<-xmlToList(xml_path)
# # names(xml_file[["Game"]])
# attributes<-xml_file[["Game"]][[".attrs"]]
# xml_file[["Game"]][[".attrs"]]<-NULL
# events<-xml_file[["Game"]]
# for(i in 1:length(events)){
#   if(events[[i]][[".attrs"]][["type_id"]]=="1"){
#     a<-events[[i]][[".attrs"]]
#     events[[i]][[".attrs"]]<-NULL
#     qualifiers<-sapply(events[[i]],function(x){x[["qualifier_id"]]})
#     "2", "5", "6", "107",  "123","124"
#   }
#   
# }
# 









