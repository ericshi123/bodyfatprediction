library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("BodyFat Calculator"),

    # Sidebar with a slider input for number of bins 
    column(10,wellPanel(
        
            helpText("Please input your measurement below"),
            textInput("weight",
                        "Weight(kg)",
                        value = 80),
            textInput("height",
                      "Height(cm)",
                      value = 180),
            textInput("age",
                      "Age",
                      value = 30),
            textInput("chest",
                      "Chest Circumference(cm)",
                      value = 80),
            submitButton("Calculate",icon("refresh"))
    )
        ),

        # Show a plot of the generated distribution
        column(10,
               verbatimTextOutput("predicted")
        ),
    column(10,
           helpText("Please contact Shuren He, Xintong Shi or Xiaoyu Liu if you have any question.")
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    y<-reactive({
      height=(as.numeric(input$height))/100
      weight=(as.numeric(input$weight))
      bmi=weight/(height*height)
      res=-33.652+0.112*as.numeric(input$age)+0.940*bmi+0.232*as.numeric(input$chest)
      res=as.character(round(res,2))
      if (is.na(res) | input$age<0 | input$height<0 | input$chest<0 | input$weight<0){
        return("Please check your input, all the fields should be filled and within reasonable range.")
      }
      else{
        return(paste0("Your bodyfat percentage is ",res))
      }
      
    })
    output$predicted <- renderText({y()})
}

# Run the application 
shinyApp(ui = ui, server = server)
