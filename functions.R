

plotField<-function(xmin=-50,xmax=50,ymin=-50,ymax=50){
  
  plot(NULL,xlim=c(xmin,xmax),ylim=c(ymin,ymax),xaxt="n",yaxt="n",xlab="",ylab="",frame=F)
  rect(xleft=xmin,xright=xmax,ybottom=ymin,ytop=ymax)
  rect(xleft=xmin,xright=xmin+(xmax-xmin)*0.5,ybottom=ymin,ytop=ymax)
  rect(xleft=xmin,xright=xmin+(xmax-xmin)*0.17,ybottom=ymin+(ymax-ymin)*0.21,ytop=ymin+(ymax-ymin)*0.79)
  rect(xleft=xmin+(xmax-xmin)*0.83,xright=xmax,ybottom=ymin+(ymax-ymin)*0.21,ytop=ymin+(ymax-ymin)*0.79)
  rect(xleft=xmin,xright=xmin+(xmax-xmin)*0.06,ybottom=ymin+(ymax-ymin)*0.368,ytop=ymin+(ymax-ymin)*0.632)
  rect(xleft=xmin+(xmax-xmin)*0.94,xright=xmax,ybottom=ymin+(ymax-ymin)*0.368,ytop=ymin+(ymax-ymin)*0.632)
}


triangulator<-function(dataHere,frame_0,theta=0.25,r=10,col="black",plot=T){
  counts<-list("friendly"=0,"opposing"=0)
  left_triangle_x<-r*cos(pi*0.5+theta)
  left_triangle_y<-r*sin(pi*0.5+theta)
  right_triangle_x<-r*cos(pi*0.5-theta)
  right_triangle_y<-r*sin(pi*0.5-theta)
  if(plot){
    polygon(
      x=c(0,left_triangle_x,right_triangle_x),
      y=c(0,left_triangle_y,right_triangle_y),
      col=col,border = NA
    )
  }
  team_names<-c("H","A")
  names(team_names)<-c("1","0")

  ball_possesion<-as.character(dataHere[frame_0,"ball_team"])
  for(p in 1:27){
    player<-paste0(p,"_team")
    team<-team_names[as.character(dataHere[frame_0,player])]
    if(is.na(team))next
    theta2<-atan2(dataHere[frame_0,paste0(p,"_y_new")],dataHere[frame_0,paste0(p,"_x_new")])-pi/2
    r2<-sqrt(dataHere[frame_0,paste0(p,"_y_new")]^2+dataHere[frame_0,paste0(p,"_x_new")]^2)
    
    
    # _theta_new
    # theta2<-dataHere[frame_0,paste0(p,"_theta_new")] - pi/2
    # r2<-dataHere[frame_0,paste0(p,"_r_new")]
    # 
    
    if(is.na(r2))next
    if(is.na(theta2))next
    if(theta2< -pi)theta2<- theta2+2*pi
    
    
    
    if(r2 > r | abs(theta2)>theta)next #too far or off
    
    if(team==ball_possesion){#team mate
      counts[["friendly"]] <- counts[["friendly"]]+1
    }else{
      counts[["opposing"]] <- counts[["opposing"]]+1
    }
  }
  return(counts)
}





opportunity_quantifier<-function(dataHere, frame_0,dist_cutoff=500, opposing_player_angle_cutoff=2*pi/36){
  opportunities<-0
  opportunity_players <- list()
  team_names<-c("H","A")
  names(team_names)<-c("1","0")
  ball_possesion<-as.character(dataHere[frame_0,"ball_team"])
  for(p in 1:27){
    player<-paste0(p,"_team")
    team<-team_names[as.character(dataHere[frame_0,player])]
    if(is.na(team))next
    
    if(team==ball_possesion){#team mate
      theta<-atan2(dataHere[frame_0,paste0(p,"_y_new")],dataHere[frame_0,paste0(p,"_x_new")])
      r<-sqrt(dataHere[frame_0,paste0(p,"_y_new")]^2+dataHere[frame_0,paste0(p,"_x_new")]^2)
      if(is.na(r))next
      
      if(r<dist_cutoff){ #close enough, then check for oponents
        have_threat<-FALSE
        for(p2 in 1:27){
          player2<-paste0(p2,"_team")
          team2<-team_names[as.character(dataHere[frame_0,player2])]
          if(is.na(team2))next #not oponennt
          if(team2==ball_possesion)next #not oponennt
          theta2<-atan2(dataHere[frame_0,paste0(p2,"_y_new")],dataHere[frame_0,paste0(p2,"_x_new")])
          r2<-sqrt(dataHere[frame_0,paste0(p2,"_y_new")]^2+dataHere[frame_0,paste0(p2,"_x_new")]^2)
          if(is.na(r2))next #not interesting
          if(r2 > r)next #too far
          if(abs(theta2-theta)>opposing_player_angle_cutoff)next
          have_threat<-TRUE
        }
        if(!have_threat){
          opportunities<-opportunities+1
          if(as.character(p)%in%names(opportunity_players))stop("already")
          opportunity_players[[as.character(p)]] <-list(
            x=dataHere[frame_0,paste0(p,"_x_new")],
            y=dataHere[frame_0,paste0(p,"_y_new")]
          )
          
          
        }
      }
    }
  }
  if(opportunities == 0){
    return(list(opportunities=NA,opportunity_players=opportunity_players))
  }else{
    
    return(list(opportunities=opportunities-1,opportunity_players=opportunity_players))
  }
}

















