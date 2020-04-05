library(shiny)
library(ggplot2)

#######################################################################################################################


# Reading in the data file
data <- read.csv('assignment-02-data-formated.csv')

# Converting from a 'factor' to a 'numeric' datatype
data$value <- gsub("%", "", data$value)
data$value <- as.numeric(data$value)

# Ordering the location by latitude
dataset.ordered <-
  data[order(data$latitude, decreasing = TRUE),]
order <- unique(dataset.ordered$location)
data$location = factor(data$location, levels = c(as.character(order)))

#######################################################################################################################


# Defining UI
ui <- fluidPage(
  # Adding sidebar layout
  sidebarLayout(
    # Defining inputs
    sidebarPanel(
      # Select variable for coral type
      selectInput(inputId = "coral", 
                  label = "Select Coral Type:",
                  choices = c(  "Blue Corals" = "blue corals",
                                "Hard Corals" = "hard corals",
                                "Sea Pens" = "sea pens",
                                "Soft Corals" = "soft corals",
                                "Sea Fans" = "sea fans"), 
                  selected = "blue corals"),
      
      # Select variable for smoother type
      selectInput(inputId = "smoother", 
                  label = "Select smoother Type:", 
                  c("Linear Smooth" = "lm", 
                    "Generalised Linear Model" = "glm",
                    "Loess Smooth" = "loess",
                    "Generalized Additive Model" = "gam"),
                  selected = "lm")
    ),
    
    # Defining outputs
    mainPanel(
      plotOutput(outputId = "cbplot")
    )
  )
)

#######################################################################################################################


# Define server function required to create the plot
server <- function(input, output) {
  
  # Create the plot object the plotOutput function is expecting
  output$cbplot <- renderPlot({
    # Finding the input variables for coral type
    if (input$coral == "blue corals") {
      newdata <- data[ which(data$coralType == input$coral), ]
    }
    else if (input$coral == "hard corals") {
      newdata <- data[ which(data$coralType == input$coral), ]
    }
    else if (input$coral == "sea fans") {
      newdata <- data[ which(data$coralType == input$coral), ]
    }
    else if (input$coral == "sea pens") {
      newdata <- data[ which(data$coralType == input$coral), ]
    }
    else if (input$coral == "soft corals") {
      newdata <- data[ which(data$coralType == input$coral), ]
    }
    # Finding the input variables for smoother
    if (input$smoother == "lm") {
      plot <- ggplot(newdata, aes(year, value)) + geom_point(aes(color = location)) + facet_wrap(c(location~coralType), labeller = "label_both") + stat_smooth(method = "lm", color="grey")
      print(plot)
    }
    else if (input$smoother == "glm") {
      plot <- ggplot(newdata, aes(year, value)) + geom_point(aes(color = location)) + facet_wrap(c(location~coralType), labeller = "label_both") + stat_smooth(method = "loess", color="grey")
      print(plot)
    }
    else if (input$smoother == "loess") {
      plot <- ggplot(newdata, aes(year, value)) + geom_point(aes(color = location)) + facet_wrap(c(location~coralType), labeller = "label_both") + stat_smooth(method = "glm", color="grey")
      print(plot)
    }
    else if (input$smoother == "gam") {
      plot <- ggplot(newdata, aes(year, value)) + geom_point(aes(color = location)) + facet_wrap(c(location~coralType), labeller = "label_both") + stat_smooth(method = "gam", color="grey")
      print(plot)
    }
  })
}

#######################################################################################################################


# Create a Shiny app object
shinyApp(ui = ui, server = server)

