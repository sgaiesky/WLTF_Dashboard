library(tidyverse)
library(gsheet)
library(magrittr)
library(lubridate)
library(shiny)
library(shinythemes)
library(plotly)

## data creation

url <- c("https://docs.google.com/spreadsheets/d/1fiRUwYv8FtysQQg7Z25Py-FW6KJeuMeoFCXuPmf89EE/edit?usp=sharing")
dat <- gsheet::gsheet2tbl(url)

dat$Date %<>% as.Date(format("%d/%m/%Y"))
dat$Athlete %<>% as.factor()
dat$Type %<>% as.factor()
dat$Season %<>% as.factor()

# Define UI for application that draws a histogram
ui <- navbarPage("West London Track & Field Athlete Monitoring",
                 theme = shinytheme("flatly"),

    tabPanel("Jump Testing",
    

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
            downloadButton("jump.report", "Generate Jump Report")
        ),

        # Show a plot of the generated distribution
        mainPanel(
            plotlyOutput("plotly"),
           #plotOutput("jump.graph"),
           #width = 8
        )
    ),
    fluidRow(column(offset = 4, width = 8, h4("Past 31 Days"))),
    
    fluidRow(column(offset = 4, width = 8, h5("Personal Best"))),
    
    fluidRow(column(offset = 4, width = 8, tableOutput("mth.pb.tbl"))),
    
    fluidRow(column(offset = 4, width = 8, h5("Average"))),
    
    fluidRow(column(offset = 4, width = 8, tableOutput("mth.avg.tbl"))),
    
    fluidRow(column(offset = 4, width = 8, h4("Season Personal Best Jumps"))),
    
    fluidRow(column(offset = 4, width = 8, tableOutput("pb.tbl"))),
    
    fluidRow(column(offset = 4, width = 8, h4("Season Average Jumps"))),
    
    fluidRow(column(offset = 4, width = 8, tableOutput("avg.tbl")))

    ),
    tabPanel("Performances",
             fluidRow(column(width = 12, h1("COMING SOON!")
                             )
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
        d$Athlete <- droplevels(d$Athlete)
        d$Type <- droplevels(d$Type)
        
        d
    })

    output$plotly <- renderPlotly({
        d <- dat1()
        
        pal <- c("red", "blue", "orange")
        pal <- setNames(pal, unique(d$Athlete))
        sym <- c("circle", "o", "x", "x-open")
        sym <- setNames(sym, c("CMJ (mm)", "SJ (mm)", "RCMJ (mm)", "LCMJ (mm)"))
        
        fig <- plot_ly(
            data = d,
            x = ~Date,
            y = ~Score,
            color = ~Athlete,
            linetype = ~Athlete,
            text = ~paste("", Athlete,
                          "<br>", Date,
                          "<br>", Type,
                          "<br>", Score),
            hoverinfo = "text",
            type = "scatter",
            mode = "markers+lines",
            line = list(width = 0.5, shape = "spline"),
            symbol = ~Type,
            symbols = sym,
            colors = pal
        )
        
        print(fig)
    })
    
    output$jump.graph <- renderPlot({
        
        d <- dat1()
        
        jump.graph <- ggplot(data = d, mapping = aes(x = Date, y = Score)) +
            geom_point(mapping = aes(colour = Athlete, shape = Type),
                       size = 4) +
            geom_line(mapping = aes(colour = Athlete, group = interaction(Athlete, Type),
                                    linetype = Type),
                      size = 1.0) +
            scale_x_date(date_breaks = "1 month",
                         date_labels = "%B %Y") +
            labs(y = "Height (mm)",
                 x = "") +
            guides(x = guide_axis(angle = 45), linetype = FALSE) +
            theme_classic() +
            theme(axis.text.x = element_text(face = "bold", size = 12),
                  axis.title.y = element_text(face = "bold", size = 12),
                  axis.text.y = element_text(face = "bold", size = 8),
                  legend.title = element_text(face = "bold", size = 12),
                  legend.text = element_text(size = 10),
                  panel.grid.major.y = element_line(linetype = "solid", colour = "grey", size = 0.5))
        
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
            
    }, striped = TRUE, bordered = TRUE, width = "85%", align = "c")
    
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
    }, striped = TRUE, bordered = TRUE, width = "85%", align = "c")
    
    output$mth.pb.tbl <- renderTable({
        d <- dat1()
        
        mth.pb.tbl <- d %>%
            filter(Date >= input$date.range[2]-31) %>%
            group_by(Athlete, Type, Season) %>%
            summarise(PB = max(Score)) %>%
            mutate_if(is.numeric, round, 0) %>%
            ungroup() %>%
            pivot_wider(names_from = Type, values_from = PB)
    }, striped = TRUE, bordered = TRUE, width = "85%", align = "c")
    
    output$mth.avg.tbl <- renderTable({
        d <- dat1()
        
        mth.avg.tbl <- d %>%
            filter(Date >= input$date.range[2]-31) %>%
            group_by(Athlete, Type, Season) %>%
            summarise(Average = mean(Score)) %>%
            mutate_if(is.numeric, round, 0) %>%
            ungroup() %>%
            pivot_wider(names_from = Type, values_from = Average)
    }, striped = TRUE, bordered = TRUE, width = "85%", align = "c")
    
    output$jump.report <- downloadHandler(filename = paste0("report.html"),
                                          content = function(file) {
                                              tempReport <- file.path(tempdir(), "report_dash.Rmd")
                                              file.copy("report_dash.Rmd", tempReport, overwrite = TRUE)
                                              
                                              params <- list(athlete = input$athlete,
                                                             tests = input$tests,
                                                             date.range = input$date.range)
                                              
                                              rmarkdown::render(tempReport, output_file = file,
                                                                params = params,
                                                                envir = new.env(parent = globalenv())
                                                                )
                                          })
}

# Run the application 
shinyApp(ui = ui, server = server)
