# shiny functionality
# retrieve input when changed
# add test dependent input (between, within, numbers, averages)
# show specification
# provide output using functions of susanne
source("susanne.r")

## Test ----

# means <- array(c(1:12,(1:12)*2), dim =c(2,3,4))
# nrOfFactors <- 3
# cellSD <- 6

# results <- effectsMultiway(nrOfFactors, means, cellSD)

# means2 <- matrix(1:6, nrow = 2, ncol = 3)
# cellSD <- 2
# correlation <- 0.3

# results2 <- effectsRepeatedMeasures(means2, cellSD, correlation)

library(reshape2)
library(ggplot2)
server <- function(input, output) {

	getInput <- reactive({
		out <- list()
		# validate
		validate(
			need(input$between, 'specify between group number factor levels'),
			need(input$within, 'specify within group number factor levels')
		)
		# return
		if(input$test=="two way anova"){
			out <- list(test=input$test,wthn=as.numeric(input$within),btwn=as.numeric(input$between))
		}
		if(input$test=="repeated measures anova"){
			# for(it in )
			out <- list(test=input$test,wthn=as.numeric(input$within),btwn=as.numeric(input$between))
		}
		out
	})
	getMeans <- reactive({
		validate(
			need(input$stddev, 'specify pooled standard deviation'),
			need(input$corrrm, 'specify intra-unit correlation')
		)
		.means <- matrix(NA,ncol=input$within,nrow=input$between)
		for(j in seq(input$within)){
		for(i in seq(input$between)){
			.means[i,j] <- as.numeric(eval(parse(text=paste0("input$range",i,"_",j))))
		}}
		if(input$test=="two way anova"){
			out <- list(means=.means,sd=as.numeric(input$stddev),xab=input$xabmw)
		}
		if(input$test=="repeated measures anova"){
			out <- list(means=.means,sd=as.numeric(input$stddev),cor=as.numeric(input$corrrm),xab=input$xabrm)
		}
		out
	})
	# getXab <- reactive({
		# list(xab=input$xab)
	# })
	output$results <- renderTable({
		# set <- getInput()
		set <- getInput()
		set2 <- getMeans()
		# set3 <- getXab()
		if(set$test=="repeated measures anova"){
			.tmp <- effectsRepeatedMeasures(set2$means, set2$sd, set2$cor, set2$xab)
		}
		if(set$test=="two way anova"){
			.ttmp <- effectsMultiway(2,set2$means, set2$sd, set2$xab)
			if(set2$xab=="yes"){
				.ttmp <- rbind(.ttmp$mainEffects,c(.ttmp$interactionEffects,.ttmp$interactionDf))
				.ttmp <- data.frame(.ttmp)
				row.names(.ttmp) <- c("A","B","AxB")
			}
			if(set2$xab=="no"){
				.ttmp <- data.frame(.ttmp$mainEffects)
				row.names(.ttmp) <- c("A","B")
			}
			names(.ttmp) <- c("effect","df")
			.tmp <- .ttmp
		}
		data.frame(nms=dimnames(.tmp)[[1]],.tmp)
	})
	output$betweenrm <- renderUI({
		if(input$test=='repeated measures anova'){
			numericInput("between", "between: # levels", value = 2, min=1)
		}
	})
	output$betweenmw <- renderUI({
		if(input$test=='two way anova'){
			numericInput("between", "A: # levels", value = 2, min=1)
		}
	})
	output$withinrm <- renderUI({
		if(input$test=='repeated measures anova'){
			numericInput("within", "within: # levels", value = 3, min=1)
		}
	})
	output$withinmw <- renderUI({
		if(input$test=='two way anova'){
			numericInput("within", "B: # levels", value = 3, min=1)
		}
	})
	output$xrm <- renderUI({
		if(input$test=='repeated measures anova'){
			selectInput("xabrm", "x", choices = c("yes","no"))
		}
	})
	output$xmw <- renderUI({
		if(input$test=='two way anova'){
			selectInput("xabmw", "x", choices = c("yes","no"))
		}
	})
	output$comments <- renderPrint({
		getMeans()$xab
	})
	# output$numbers <- renderUI({
		# if(input$test=='two way anova'){
			# textInput('numbers','factor specific # factor levels (+=main, *=interaction), eg., 3;2;2','2;2')
		# }
	# })
	output$stddevrm <- renderUI({
		if(input$test=='repeated measures anova'){
			textInput('stddev','sd','1')	
		}
	})
	output$corrrm <- renderUI({
		if(input$test=='repeated measures anova'){
			textInput('corrrm','cor','0')	
		}
	})
	output$stddevmw <- renderUI({
		if(input$test=='two way anova'){
			textInput('stddev','sd','1')		
		}
	})
	output$plot <- renderPlot({
		set2 <- getMeans()
		plot(1:10)
		.tmp <- data.frame(btw=1:2,set2$means)
		names(.tmp)[-1] <- paste('t',1:ncol(.tmp))
		.tmp <- melt(.tmp,id='btw')
		ggplot(.tmp,aes(y=value,x=variable,group=btw)) + geom_line()
	})

	output$averages <- renderUI({
		set <- getInput()
		lapply(seq(set$wthn), function(j){
			column(width=2,
				wellPanel(			
					lapply(seq(set$btwn),function(i){
						textInput(inputId = paste0("range",i,"_",j),label=paste0("",i,"_",j),value=0) 
					})
				)				
			)
		})
	})

}
