library(tidyverse)
library(gsheet)
library(magrittr)
library(lubridate)
library(shiny)
library(shinythemes)
library(plotly)

####functions####
tbl.format <- function(x) {
  x$Date %<>% as.Date(format("%d %B %Y"))
  x$Athlete %<>% as.factor()
  x$Type %<>% as.factor()
  x$Season %<>% as.factor()
  
  return(x)
}

####data retrieval####
url <- c("https://docs.google.com/spreadsheets/d/1IYawLj4DywKXo8HfxALWXuv3sWAb1GaUS_kNSqOcfEg/edit#gid=253542207")
url2 <- c("https://docs.google.com/spreadsheets/d/1IYawLj4DywKXo8HfxALWXuv3sWAb1GaUS_kNSqOcfEg/edit#gid=511309038")

dat <- gsheet::gsheet2tbl(url)
dat2 <- gsheet::gsheet2tbl(url2)

jump.dat <- tbl.format(dat)
split.dat <- tbl.format(dat2)

####ui####
ui <- navbarPage("West London Track & Field Athlete Monitoring",
                 theme = shinytheme("flatly"),

    tabPanel("Jump Testing",
    

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectizeInput("athlete",
                           "Select Athlete(s):",
                           choices = unique(jump.dat$Athlete),
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
            plotlyOutput("jump.plot"),
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
    tabPanel("Splits",
             sidebarLayout(
               sidebarPanel(
                 selectizeInput("athlete.split",
                                "Select Athlete(s):",
                                choices = unique(split.dat$Athlete),
                                multiple = TRUE,
                                options = list(maxItems = 1)),
                 dateRangeInput("split.date.range",
                                "Select the dates of interest:",
                                start = "2021-09-01",
                                end = NULL),
                 checkboxGroupInput("split.tests",
                                    "Select the tests of interest:",
                                    choices = unique(split.dat$Type)),
                 downloadButton("split.report", "Generate Split Report")
               ),
               mainPanel(
                 fluidRow(
                   plotlyOutput("split.plot")
                   ),
               )
             )
    )
)


####server####

server <- function(input, output) {
  
####data####  
  #jump data creation - filtered
    jump.dat1 <- reactive({
        d <- jump.dat %>%
            filter(Athlete %in% input$athlete &
                   Type %in% input$tests & 
                   #Season %in% input$season &
                       Date >= input$date.range[1] &
                       Date <= input$date.range[2]) %>%
          arrange(Date)
        
        d$Athlete <- droplevels(d$Athlete)
        d$Type <- droplevels(d$Type)
        
        return(d)
    })
    
  #split data creation - filtered
    split.dat1 <- reactive({
      d <- split.dat %>%
        filter(Athlete %in% input$athlete.split &
                 Date >= input$split.date.range[1] &
                 Date <= input$split.date.range[2] &
                 Type %in% input$split.tests) %>%
        arrange(Date)
      
      d$Athlete <- droplevels(d$Athlete)
      d$Type <- droplevels(d$Type)
      
      return(d)
    })
    
####plots####
  #jump data plot
    output$jump.plot <- renderPlotly({
        d <- jump.dat1()
        
        pal <- c("red", "blue", "orange")
        pal <- setNames(pal, input$athlete)
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
        ) %>%
          layout(
            xaxis = list(fixedrange = TRUE),
            yaxis = list(fixedrange = TRUE)
          )
        
        print(fig)
    })
    
  #split data plot
    output$split.plot <- renderPlotly({
      d <- split.dat1()
      
      pal <- c("red", "blue", "orange")
      pal <- setNames(pal, input$split.tests)
      
      fig <- plot_ly(
        data = d,
        x = ~Date,
        y = ~Time,
        color = ~Type,
        text = ~paste("", Type,
                      "<br>", Time),
        hoverinfo = "text",
        type = "scatter",
        mode = "markers+lines",
        line = list(width = 0.5),
        colors = pal
      ) %>%
        layout(
          xaxis = list(fixedrange = TRUE),
          yaxis = list(fixedrange = TRUE)
        )
      
      print(fig)
    })
    
    
####tbls####   
  #jump pb table
    output$pb.tbl <- renderTable({
        d <- jump.dat1()
        
        pb.tbl <- d %>%
            group_by(Athlete, Type, Season) %>%
            summarise(PB = max(Score, na.rm = TRUE),
                      Average = mean(Score, na.rm = TRUE)) %>%
            mutate_if(is.numeric, round, 0) %>%
            ungroup() %>%
            select(!Average) %>%
            pivot_wider(names_from = Type, values_from = PB)
            
    }, striped = TRUE, bordered = TRUE, width = "85%", align = "c")
  
  #jump avg table
    output$avg.tbl <- renderTable({
        d <- jump.dat1()
        
        avg.tbl <- d %>%
            group_by(Athlete, Type, Season) %>%
            summarise(PB = max(Score, na.rm = TRUE),
                      Average = mean(Score, na.rm = TRUE)) %>%
            mutate_if(is.numeric, round, 0) %>%
            ungroup() %>%
            select(!PB) %>%
            pivot_wider(names_from = Type, values_from = Average)
    }, striped = TRUE, bordered = TRUE, width = "85%", align = "c")
    
    output$mth.pb.tbl <- renderTable({
        d <- jump.dat1()
        
        mth.pb.tbl <- d %>%
            filter(Date >= input$date.range[2]-31) %>%
            group_by(Athlete, Type, Season) %>%
            summarise(PB = max(Score, na.rm = TRUE)) %>%
            mutate_if(is.numeric, round, 0) %>%
            ungroup() %>%
            pivot_wider(names_from = Type, values_from = PB)
    }, striped = TRUE, bordered = TRUE, width = "85%", align = "c")
    
  #jump month avg table
    output$mth.avg.tbl <- renderTable({
        d <- jump.dat1()
        
        mth.avg.tbl <- d %>%
            filter(Date >= input$date.range[2]-31) %>%
            group_by(Athlete, Type, Season) %>%
            summarise(Average = mean(Score, na.rm = TRUE)) %>%
            mutate_if(is.numeric, round, 0) %>%
            ungroup() %>%
            pivot_wider(names_from = Type, values_from = Average)
    }, striped = TRUE, bordered = TRUE, width = "85%", align = "c")
    
  # #split table
  #   output$split.tbl <- renderTable({
  #     d <- 
  #   })
  
####reports####  
  #jump report
    output$jump.report <- downloadHandler(filename = paste0("report.html"),
                                          content = function(file) {
                                              tempReport <- file.path(tempdir(), "jump_report.Rmd")
                                              file.copy("jump_report.Rmd", tempReport, overwrite = TRUE)
                                              
                                              params <- list(athlete = input$athlete,
                                                             tests = input$tests,
                                                             date.range = input$date.range)
                                              
                                              rmarkdown::render(tempReport, output_file = file,
                                                                params = params,
                                                                envir = new.env(parent = globalenv())
                                                                )
                                          })
    
  #split report
    output$split.report <- downloadHandler(filename = paste0("report.html"),
                                          content = function(file) {
                                            tempReport <- file.path(tempdir(), "split_report.Rmd")
                                            file.copy("split_report.Rmd", tempReport, overwrite = TRUE)
                                            
                                            params <- list(athlete = input$athlete.split,
                                                           tests = input$split.tests,
                                                           date.range = input$split.date.range)
                                            
                                            rmarkdown::render(tempReport, output_file = file,
                                                              params = params,
                                                              envir = new.env(parent = globalenv())
                                            )
                                          })
}

####run app####
shinyApp(ui = ui, server = server)
