#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(randomForest)
library(shinythemes)
library(data.table) #load function transpose
#READ DATA
weather <- read.csv("https://raw.githubusercontent.com/dataprofessor/data/master/weather-weka.csv")

#BUILD MODELS
model <- randomForest(as.factor(play) ~ ., data=weather ,ntree=500, mtry=4,importance=TRUE)
# Define UI for application that draws a histogram
ui <- fluidPage(theme=shinytheme("readable"),

    # Application title
    headerPanel("Play Golf?"),
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(h4("Input Parameters"),
          selectInput("outlook",
                      "Outlook:",
                      choices = c(unique(weather$outlook)),
                      selected = "rainy"
                      ),
          
           sliderInput("temperature",
                       "Temperature:",
                       min=64,
                       max=86,
                       value = 70
                       ),
           sliderInput("humidity",
                       "Humidity:",
                       min=65,
                       max=96,
                       value = 90
           ),
          selectInput("windy",
                      "Windy",
                      choices = c(unique(weather$windy)),
                      selected = TRUE
                      ),
          actionButton("submitbutton",
                       "Submit",
                       class="btn btn-primary"
                       )
        ),

        # Show a plot of the generated distribution
        mainPanel(
          h3("Status/Output"), #status/Output text box
           verbatimTextOutput("contents"),
          tableOutput("tabledata") #prediction results table
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output,session) {
  #input
  datasetInput <- reactive({
    #outlook,temperature,humidity,windy,play
    df <- data.frame(
      Name= c("outlook",
              "temperature",
              "humidity",
              "windy"),
      value= as.character(c(input$outlook,
                            input$temperature,
                            input$humidity,
                            input$windy)),
      stringsAsFactors = FALSE
    )
    play <- "play"
    df <- rbind(df,play)
    input <-transpose(df)
   
    #print(input)
    
    write.table(input,"input.csv",sep=",", quote = FALSE, row.names = FALSE, col.names = FALSE)
    test <- read.csv(paste("input",".csv",sep=""),header=TRUE)
    output <- data.frame(prediction=predict(model,test),round(predict(model,test,typ="prob"),3))
    print(output)
  })
  
  
  output$contents <- renderPrint({
     if(input$submitbutton >0){
       isolate("Calculation Complete")
     }
    else{
      return("server is ready for calculation")
    }
  })
  
  #prediction results table
  output$tabledata <- renderTable({
    if (input$submitbutton>0){
      isolate(datasetInput())
    }
  })
 
}

# Run the application 
shinyApp(ui = ui, server = server)
