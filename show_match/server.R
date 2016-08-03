# The purpose of this script is basically just to show the match, as is, from a normal spectator point of view.

library("shiny")


data_dir <-"/home/ubuntu/data/2016-07-26_parsed_data/"
data_dir <-"C:/Users/FOLK/Documents/Work/Analysis/2016-07-26 manchester/"



# Define server logic for random distribution application
shinyServer(function(input, output) {
  
  output$plot1 <- renderPlot({ 
    # Take a dependency on input$goButton
    if(input$goButton == 0){
      return(NULL)
    }else if(input$goButton > 0) {
      print(paste("Ok",input$goButton))
    }
    
    time <- isolate(input$time)
    match <- isolate(input$match)
    
    #for testing
    match<-"803174"
    time<-c(24,40.5)
    time<-c(40,90.5)
    
    
    load(paste0(data_dir,"2016-07-26_",match,"chryonhego.rdata"))

    
    
        
    data[,"frame"]<-as.numeric(rownames(data))
    match_start<-min(data[,"frame"])
    show_start<-time[1]*60*25+match_start
    show_end<-time[2]*60*25+match_start
    d<-data[data[,"frame"]>show_start & data[,"frame"]<show_end,]
    d<-d[seq(from=1,nrow(d),by=25),]
    
    
    
    
    player_x_col<-grep("[0-9]{1,2}_x",colnames(d),value=T)
    player_y_col<-grep("[0-9]{1,2}_y",colnames(d),value=T)
    player_team_col<-grep("[0-9]{1,2}_team",colnames(d),value=T)
    
    colours<-c("blue","red","grey50","grey50")
    names(colours)<-c("0","1","2","3")
    
    for(i in 1:nrow(d)){
      x<-t(d[i,player_x_col])[,1]/100
      y<-t(d[i,player_y_col])[,1]/100
      team<-t(d[i,player_team_col])[,1]
      colours[team]
      
      plotField(xmin= -55,xmax=55,ymin= -68/2,ymax=68/2)  
      
      #plot players
      points(x,y,pch=19,col=colours[team])
      
      #plot ball
      points(d[i,"ball_x"]/100,d[i,"ball_y"]/100)
      
      Sys.sleep(0.2)
    }
    
  })
  
})



