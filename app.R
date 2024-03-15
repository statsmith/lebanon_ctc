

# Libraries ----

library(shiny)
library(shinyjs)
library(shinyWidgets)
library(plotly)
library(DT)
library(tidyverse)
library(lubridate)
library(RColorBrewer)

# Constants ----


print(getwd())

path <- NA
source(file.path("info.R"))

# Data ----

# df <- read_csv(file.path("df.csv"))
df <- readRDS(file.path("data.rds"))


l_df1 <- df %>% split(~.$q)
l_names1 <- names(l_df1)


l_df2 <-
    
    df %>%
    filter(gender == "All")  %>% 
    filter(grade != "All") %>% 
    
    mutate(year = factor(year)) %>% 
    mutate(grade = as.numeric(as.character(grade))) %>% 
    split(.$source)



# Testing ----
testing <- !TRUE


ui <- function(req) {
    
    fluidPage(
        
        title = "App Name",
        useShinyjs(),
        
        includeCSS(path = "www/app.css"),
        
        ## Title ----
        div(
            class = "title",
            "Lebanon PAYS Explorer"
        ),
        
        # actionLink(inputId = "help", label = "", icon("info-circle"), style = "color: white"),
        
        # Test ----
        verbatimTextOutput(outputId = "test"),  
        
        div( # for margins on phone
            
            # Outputs ----
            
            div(
                
                tabsetPanel(
                    
                    ### plots ----
                    tabPanel(
                        
                        title = "Plots",
                        
                        div(
                            
                            class = "content-box mb20",
                            
                            div(
                                class = "center-flex medblue em1-5",
                                "By Grade"
                            ),
                            
                            div(
                              
                                class = "margin30",
                                
                                "
                                Select the data source form the drop-down menu.  
                                The data for all students (all genders) is plotted.
                                The top row of plots is for Lebanon County.
                                The bottom row of plots is for the state of PA.
                                Each column corresponds to a question in the selected data source.
                                The x-axis is grade, making it easy to see how students progress through middle school and high school.
                                The y-axis is the percent of students.
                                "
                                  
                            ),
                            
                            div(
                                
                                class = "center-flex",
                                
                                pickerInput(
                                    inputId = "source", 
                                    label = "", 
                                    choices = unique(df$source) %>% sort(), 
                                    multiple = FALSE
                                )
                            ),
                            
                            div(plotOutput("p_by_grade", height = "600px")), 
                            
                        ),
                        
                        div(
                            
                            class = "content-box mb20",
                            
                            div(
                                class = "center-flex medblue em1-5",
                                "By Year"
                            ),
                            
                            div(
                                
                                class = "margin30",
                                
                                "
                                This plot allows the user to drill down into specific demographics for selected questions.
                                Select the question from the drop down menu.  
                                Only questions in the data source (selected above) will be shown.
                                Rows of plots correspond to grade.
                                Columns of plots correspond to gender.
                                The x-axis is year (of the survey).
                                The y-axis is the percent of students.
                                "
                                
                            ),
                            
                            div(
                                
                                class = "center-flex",
                                
                                pickerInput(
                                    inputId = "q", 
                                    label = "", 
                                    choices = unique(df$q) %>% sort(), 
                                    multiple = FALSE
                                )
                            ),
                            
                            div(plotOutput("p_by_yr", height = "600px"))
                        )
                    ),
                    
                    tabPanel(
                        
                        ### tables ----
                        title = "Tables",
                        
                        div(
                            
                            class = "content-box mb20",
                            
                            div(
                                class = "center-flex medblue em1-5",
                                "Differences: By Locale"
                            ),
                            
                            div(
                                
                                class = "margin30",
                                
                                "
                                This table compares county level data to state level data.
                                Click the grey triangles next to column names to sort. 
                                This makes it easy to find the biggest differences.
                                Filter the table using the boxes below each column name.
                                Click the Excel button to download the data.
                                "
                                
                            ),
                            
                            div(DT::dataTableOutput("dt_locale"))
                        ),
                        
                        
                        div(
                            
                            class = "content-box mb20",
                            
                            div(
                                class = "center-flex medblue em1-5",
                                "Differences: By Gender"
                            ),
                            
                            div(
                                
                                class = "margin30",
                                
                                "
                                This table compares data by gender.
                                Click the grey triangles next to column names to sort. 
                                This makes it easy to find the biggest differences.
                                Filter the table using the boxes below each column name.
                                Click the Excel button to download the data.
                                "
                                
                            ),
                            
                            div(DT::dataTableOutput("dt_gender"))
                        ),
                        
                        div(
                            
                            class = "content-box mb20",
                            
                            div(
                                class = "center-flex medblue em1-5",
                                "Differences: By Grade"
                            ),
                            
                            div(
                                
                                class = "margin30",
                                
                                "
                                This table compares data by grade.
                                Click the grey triangles next to column names to sort. 
                                This makes it easy to find the biggest differences. 
                                Specifically, what are the biggest challenges as kids go from 6th grade to 8th grade, 8th grade to 10th grade, and 10th grade to 12th grade.
                                Filter the table using the boxes below each column name.
                                Click the Excel button to download the data.
                                "
                                
                            ),
                            div(DT::dataTableOutput("dt_grade"))
                        )
                        
                        
                    )
                )
                
            ), # End tabSetPanel
            
            
            # Citation ----
            
            div(
                class = "center-flex margin30",
                div(
                    
                    tags$a(
                        href = "mailto:dstatsmith@gmail.com?subject=Have A Nice Day&body=Help!", 
                        icon("envelope")
                    ),
                    tags$a(href="https://github.com/statsmith/lebanon_ctc", icon("github"), target = "_blank"),
                    tags$a(href="https://www.pccd.pa.gov/Juvenile-Justice/pages/pennsylvania-youth-survey-(pays).aspx", img(src='pccd_icon.png', height = "20%", width = "20%"), target = "_blank")
                    
                )
            )
            
        )
        
    )
    
} # end UI



# SERVER --------------------------------------------------------------------------------------------------------------

server <- function(input, output, session) {
    
    
    # TEST ----
    output$test <- renderPrint({
        
        if(testing){
            
            print("testing")
            
        }
    })
    
    # Help ----
    
    observeEvent(input$help,{
        
        showModal(modalDialog(
            
            title = "Help",
            
            info_helpUI("i"),
            info_helpServer("i"),
            
            size = "l",
            easyClose = TRUE,
            fade = TRUE
        ))
        
    })
    
    
    # Reactive Values ----
    rv <- reactiveValues(go = 1)
    
    click("go")
    
    # Update Inputs ----
    
    observe({
        
        questions <- df %>% filter(source == input$source) %>% distinct(q) %>% pull(q) %>% sort()
        updatePickerInput(session, inputId = "q", label = "", choices = questions)
        
    }) %>% 
        
        bindEvent(input$source)
    
    # plots ----
    
    ## plot by yr ----
    
    output$p_by_yr <-
        
        renderPlot({
            
            p_by_yr(.df = l_df1[[input$q]], .var = input$q)
            
        })
    
    ## plot by grade ----
    
    output$p_by_grade <- 
        
        renderPlot({
            
            p_by_grade(.df = l_df2[[input$source]], .var = input$source)
            
        })
    
    # tables ----
    
    ## diff by locale ----
    
    output$dt_locale <- 
        
        DT::renderDataTable({
            
            .df <- 
                
                df %>% 
                
                pivot_wider(names_from = locale, values_from = percent, values_fn = mean) %>% 
                mutate(leb_state = `Lebanon County` - State) %>% 
                arrange(-year, -abs(leb_state)) %>% 
                
                select(
                    Source = source,
                    Question = q,
                    Year = year,
                    Grade = grade,
                    Gender = gender,
                    `Lebanon County`,
                    State,
                    `Diff: Lebanon - State` = leb_state
                    
                ) %>%
                
                mutate(across(c(Source:Gender), ~factor(.)))
            
            
            make_dt(.df) %>% 
                
                formatPercentage(
                    columns = 6:8,
                    digits = 1
                )
            
            
        })
    
    ## diff by gender ----
    
    output$dt_gender <- 
        
        DT::renderDataTable({
            
            .df <- 
                
                df %>%
                
                pivot_wider(names_from = gender, values_from = percent, values_fn = mean) %>% 
                
                mutate(fm_diff = Female - Male) %>% 
                mutate(om_diff = Other - Male) %>% 
                mutate(of_diff = Other - Female) %>% 
                
                arrange(-year, -abs(fm_diff)) %>% 
                
                select(
                    Source = source,
                    Question = q,
                    Year = year,
                    Grade = grade,
                    Locale = locale,
                    All,
                    Female,
                    Male, 
                    Other,
                    `Diff: Female - Male` = fm_diff,
                    `Diff: Other - Male` = om_diff,
                    `Diff: Other - Female` = of_diff

                ) %>%

                mutate(across(c(Source:Locale), ~factor(.)))
            
            make_dt(.df) %>% 
                
                formatPercentage(
                    columns = 6:12,
                    digits = 1
                )
            
        })
    
    ## diff by grade ----
    
    output$dt_grade <-
        
        DT::renderDataTable({
            
            .df <- 
                
                df %>%
                
                filter(grade != "All") %>% 
                pivot_wider(names_from = grade, values_from = percent, values_fn = mean) %>% 
                
                mutate(g12g10 = `12` - `10`) %>% 
                mutate(g10g8 = `10` - `8`) %>% 
                mutate(g8g6 = `8` - `6`) %>% 
                
                arrange(-year, -abs(g8g6)) %>% 
                
                select(
                    Source = source,
                    Question = q,
                    Year = year,
                    Gender = gender,
                    Locale = locale,
                    `12`, `10`, `8`, `6`,
                    
                    `Diff: 12-10` = g12g10,
                    `Diff: 10-8` = g10g8,
                    `Diff: 8-6` = g8g6,
                    
                ) %>%
                
                mutate(across(c(Source:Locale), ~factor(.))) %>% 
                mutate(across(c(`Diff: 12-10`, `Diff: 10-8`, `Diff: 8-6`), ~as.numeric(.)))
            
            
            make_dt(.df)  %>% 
                
                formatPercentage(
                    columns = 6:12,
                    digits = 1
                )
        })
    
    
    
    
} # End server

shinyApp(ui = ui, server = server)