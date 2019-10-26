#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# Deploy
# rsconnect::setAccountInfo(name='tlngu39', 
#                           token='308014F98D2B41AF0FD97B32C5065443', 
#                           secret='3YfC3W96ZfqWxIJNNHY9E37S+YYyUnR7L18tVRkU')

# Load libraries
#library(rsconnect)
#rsconnect::deployApp(
library(shiny)
source("helpers.R")
source("dataHelper.R")

path = file.path("data", "portfolios_formed_on_me.csv")
ff_data <- load(path, frequency = "monthly", model = "5FF")

# Construct excess returns
ff_data <- ff_data %>% mutate(Lo30Ex = `Lo 30` - RF, Med40Ex = `Med 40` - RF, Hi30Ex = `Hi 30` - RF)


# This UI utilises tabset layout 
ui <- fluidPage(
  tags$head(tags$style(HTML("hr {border-top: 1px solid #000000;}"))),
  titlePanel("Visualising Case Influence"),
  
  sidebarLayout(
    sidebarPanel(
      checkboxGroupInput("factors",
                         strong("Choose which factors to display"),
                         choices = list("Market",
                                        "Size",
                                        "Book-to-Equity Ratio",
                                        "Profitability",
                                        "Investment"),
                         selected = c("Market", "Size", "Book-to-Equity Ratio")),
      
      br(),
      
      sliderInput("angleSlider", label = strong("Choose angle of plot"), min = 0, max = 360, value = 0),

      div(style = "display: inline-block; vertical-align:top; width: 150px", actionButton("rotateAnimation", "Rotate")),
      div(style = "display: inline-block; vertical-align:top; width: 25px", HTML("<br>")), 
      div(style = "display: inline-block; vertical-align:top; width: 150px", actionButton("stopAnimation", "Stop")),
      
      hr(),
      
      # Slider range for dates is probably better
      sliderInput("startDate",
                  label = strong("Choose starting date"),
                  min = as.Date(paste(ff_data$Time[1],"01", sep = ""), "%Y%m%d"), 
                  max = as.Date(paste(ff_data$Time[length(ff_data$Time)],"01", sep = ""), "%Y%m%d"), 
                  value = as.Date("2000-01-01"), 
                  timeFormat = "%b %Y"),
      
      sliderInput("endDate",
                  label = strong("Choose ending date"),
                  min = as.Date(paste(ff_data$Time[1],"01", sep = ""), "%Y%m%d"), 
                  max = as.Date(paste(ff_data$Time[length(ff_data$Time)],"01", sep = ""), "%Y%m%d"), 
                  value = as.Date("2005-01-01"), 
                  timeFormat = "%b %Y"),
      hr(),

      #actionButton("debugButton", "Debug")
      sliderInput("sliderThreshold",
                  label = strong("Choose threshold"),
                  min = 0,
                  max = 1,
                  value = 0.22,
                  step = 0.001)
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel(
          "Flag by date",
          h2("PCA Display"),
          plotOutput("animationOutput", height = "550px"),
          plotOutput("twodPlot", height = "400px")
        ),
        
        tabPanel(
          "Flag by threshold",
          h2("PCA Display"),
          plotOutput("thresholdOutput", height = "550px")
        )
      )
    )
  )
)

# Define server logic 
server <- function(input, output, session) {
  #########################
  ###### 3D Plot Tab ###### 
  #########################
  width_ = 550
  height_ = 550
  
  # Reactive variables
  variables = reactiveValues(model_state = "3FF")
  modelState = reactive({
    getModelState(input$factors)
  })
  
  # Reactive function which checks if factors checkboxes has changed and updates data + model_state
  modelData = reactive({
    df = getModel(modelState())
    getModelColour(df, ff_data$Time, sDate = input$startDate, eDate = input$endDate, sep = "-")
  })
  
  # Button testing
  observeEvent(input$debugButton, {
    print(modelState())
  })
  
  # Displays plot based on angle selected
  output$animationOutput = renderImage({
    outfile = tempfile(fileext = ".png")
    png(outfile, width = width_, height = height_)
    plotScatter3D(modelData(), input$angleSlider)
    dev.off()
    
    list(src = outfile)
  }, deleteFile = TRUE)
  
  # Outputs animation upon button press
  observeEvent(input$rotateAnimation, {
    output$animationOutput = renderImage({
      gif_path = paste(modelState(), ".gif", sep = "")
      fileName = normalizePath(file.path("./www", gif_path))
      list(src = fileName)
    }, deleteFile = FALSE)
  })
  
  # Stops animation by displaying plot
  observeEvent(input$stopAnimation, {
    output$animationOutput = renderImage({
      outfile = tempfile(fileext = ".png")
      png(outfile, width = width_, height = height_)
      plotScatter3D(modelData(), input$angleSlider)
      dev.off()
      
      list(src = outfile)
    }, deleteFile = TRUE)
  })
  
  # Displays excess returns over time
  # Reactive dataframe of excess returns over time
  ff_reactive = reactive({
    time = processDate(ff_data$Time, sep = "")
    return(data.frame(Time = time, Excess = ff_data$Lo30Ex))
  })
  
  # Reactive values for vertical lines
  startVLine = reactive({
    return(processDate(input$startDate, sep = "-"))
  })
  endVLine = reactive({
    return(processDate(input$endDate, sep = "-"))
  })
  
  output$twodPlot = renderPlot({
    ggplot(ff_reactive(), aes(x = Time, y = Excess)) + geom_line() +
      geom_rect(aes(xmin = startVLine(), xmax = endVLine(), ymin = -Inf, ymax = Inf), alpha = 0.015, fill = "pink") +
      geom_vline(xintercept = startVLine(), linetype = "dotted") +
      geom_vline(xintercept = endVLine(), linetype = "dotted")
  })
  
  # With start and end vertical lines defined, assign 'highlight' state to those observations
  
  #########################
  ###### PCA2 Tab #######
  #########################
  modelData_t = reactive({
    df = getModel(modelState())
    getModelColour(df, ff_data$Time, sDate = input$startDate, eDate = input$endDate, sep = "-", threshold = input$sliderThreshold)
  })
  
  # Displays plot based on angle selected
  output$thresholdOutput = renderImage({
    outfile = tempfile(fileext = ".png")
    png(outfile, width = width_, height = height_)
    plotScatter3D(modelData_t(), input$angleSlider)
    dev.off()
    
    list(src = outfile)
  }, deleteFile = TRUE)
  
  # Outputs animation upon button press
  observeEvent(input$rotateAnimation, {
    output$thresholdOutput = renderImage({
      gif_path = paste(modelState(), ".gif", sep = "")
      fileName = normalizePath(file.path("./www", gif_path))
      list(src = fileName)
    }, deleteFile = FALSE)
  })
  
  # Stops animation by displaying plot
  observeEvent(input$stopAnimation, {
    output$thresholdOutput = renderImage({
      outfile = tempfile(fileext = ".png")
      png(outfile, width = width_, height = height_)
      plotScatter3D(modelData_t(), input$angleSlider)
      dev.off()
      
      list(src = outfile)
    }, deleteFile = TRUE)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
