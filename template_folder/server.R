library("shiny")

basedir <-"/home/ubuntu/srv/hackmcfc"




# Define server logic for random distribution application
shinyServer(function(input, output) {
	
	output$plot1 <- renderPlot({ 
		# Take a dependency on input$goButton
		
		if(input$goButton == 0){
			return(NULL)
		}else if(input$goButton > 0) {
			print(paste("Ok",input$goButton))
		}
		
		plot(1:10,1:10)
	  
	})
	
})


