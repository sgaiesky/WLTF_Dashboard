library(tidyverse)
library(gsheet)
library(magrittr)
library(lubridate)
library(shiny)
library(shinythemes)

## data creation

url <- c("https://docs.google.com/spreadsheets/d/1fiRUwYv8FtysQQg7Z25Py-FW6KJeuMeoFCXuPmf89EE/edit?usp=sharing")
dat <- gsheet::gsheet2tbl(url)

dat$Date %<>% as.Date(format("%d/%m/%Y"))
dat$Athlete %<>% as.factor()
dat$Type %<>% as.factor()
dat$Season %<>% as.factor()

# Define UI for application that draws a histogram
ui <- fluidPage(
    shinythemes::themeSelector(),

    # Application title
    titlePanel("West London Track & Field Athlete Monitoring"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectizeInput("athlete",
                           "Select Athlete(s):",
                           choices = unique(dat$Athlete),
                           multiple = TRUE,
                           options = list(maxItems = 3)),
            dateRangeInput("date.range",
                           "Select the dates of interest:",
                           start = "2021-09-01",
                           end = NULL),
            checkboxGroupInput("tests",
                               "Select the tests of interest:",
                               choices = c("CMJ (mm)", "SJ (mm)",
                                           "RCMJ (mm)", "LCMJ (mm)")),
            #checkboxGroupInput("season",
                               #"Select the seasons of interest:",
                               #choices = unique(dat$Season))
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("jump.graph"),
           tableOutput("pb.tbl"),
           tableOutput("avg.tbl")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    dat1 <- reactive({
        d <- dat %>%
            filter(Athlete %in% input$athlete &
                   Type %in% input$tests & 
                   #Season %in% input$season &
                       Date >= input$date.range[1] &
                       Date <= input$date.range[2])
        
        d
    })

    output$jump.graph <- renderPlot({
        
        d <- dat1()
        
        jump.graph <- ggplot(data = d, mapping = aes(x = Date, y = Score)) +
            geom_point(mapping = aes(colour = Type, shape = Athlete),
                       size = 3) +
            geom_line(mapping = aes(colour = Type, group = interaction(Athlete, Type),
                                    linetype = Athlete),
                      size = 1.2) +
            scale_x_date(date_breaks = "1 month",
                         date_labels = "%B %Y") +
            labs(y = "Height (mm)",
                 x = "Date") +
            guides(x = guide_axis(angle = 90)) +
            theme_classic()
        
        print(jump.graph)
    })
    
    output$pb.tbl <- renderTable({
        d <- dat1()
        
        pb.tbl <- d %>%
            group_by(Athlete, Type, Season) %>%
            summarise(PB = max(Score),
                      Average = mean(Score)) %>%
            mutate_if(is.numeric, round, 0) %>%
            ungroup() %>%
            select(!Average) %>%
            pivot_wider(names_from = Type, values_from = PB)
            
    })
    
    output$avg.tbl <- renderTable({
        d <- dat1()
        
        avg.tbl <- d %>%
            group_by(Athlete, Type, Season) %>%
            summarise(PB = max(Score),
                      Average = mean(Score)) %>%
            mutate_if(is.numeric, round, 0) %>%
            ungroup() %>%
            select(!PB) %>%
            pivot_wider(names_from = Type, values_from = Average)
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
