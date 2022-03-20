library(tidyverse)
library(gsheet)
library(magrittr)
library(lubridate)
library(shiny)

## data creation

url <- c("https://docs.google.com/spreadsheets/d/1fiRUwYv8FtysQQg7Z25Py-FW6KJeuMeoFCXuPmf89EE/edit?usp=sharing")
dat <- gsheet::gsheet2tbl(url)

dat$Date %<>% as.Date(format("%d/%m/%Y"))
dat$Athlete %<>% as.factor()
dat$Type %<>% as.factor()
dat$Season %<>% as.factor()

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("West London Track & Field Athlete Monitering"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectInput("athlete",
                        "Select Athlete(s):",
                        choices = unique(dat$Athlete)),
            dateRangeInput("date.range",
                           "Select the dates of interest:"),
            checkboxGroupInput("tests",
                               "Select the tests of interest:",
                               choices = unique(dat$Type)),
            checkboxGroupInput("season",
                               "Select the seasons of interest:",
                               choices = unique(dat$Season))
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("jump.graph")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    dat1 <- reactive({
        d <- dat %>%
            filter(Athlete %in% input$athlete &
                   Type %in% input$tests &
                   Season %in% input$season)
        
        d
    })

    
    output$jump.graph <- renderPlot({
        
        d <- dat1()
        
        jump.graph <- ggplot(data = d, mapping = aes(x = Date, y = Score)) +
            geom_point(mapping = aes(colour = Type, shape = Athlete)) +
            geom_line(mapping = aes(colour = Type, group = interaction(Athlete, Type))) +
            scale_x_date(date_breaks = "1 month",
                         date_labels = "%B %Y") +
            labs(y = "Height (mm)",
                 x = "Date") +
            guides(x = guide_axis(angle = 90)) +
            theme_classic()
        
        print(jump.graph)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
