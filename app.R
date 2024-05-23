

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

path <- NA
source(file.path("info.R"))

# Data ----

# df <- read_csv(file.path("df.csv"))
d <- readRDS(file.path("data.rds"))

df <- d$df
df_corr <- d$df_corr

l_df1 <- df %>% split(~.$q)
l_names1 <- names(l_df1)

l_df2 <-
    
    df %>%
    filter(gender == "All")  %>% 
    filter(grade != "All") %>% 
    
    mutate(year = factor(year)) %>% 
    mutate(grade = as.numeric(as.character(grade))) %>% 
    split(.$source)

# year as numeric for plots, factor for tables
df <- df %>% mutate(year = factor(year, levels = rev(unique(.$year))))


# Testing ----
testing <- !TRUE


ui <- function(req) {
    
    fluidPage(
        
        title = "Lebanon PAYS Explorer",
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
                        
                        # div(
                        
                        # class = "center-flex",
                        
                        # div(
                        #     
                        #     class = "center-wrap margin30",
                        #     
                        #         "Select data source form the drop-down menu", tags$br(),
                        #         "Data for all students (all genders) is shown in the next two plots (By Grade, By Cohort)", tags$br(),
                        #         "Top Row of Plots: Lebanon county;  Bottom Row of Plots: state of PA", tags$br(),
                        #         "Columns of Plots: questions in the selected data source", tags$br(),
                        #         "x-axis: grade or cohort (making it easy to see how students progress through school)", tags$br(),
                        #         "y-axis: percent (%) of students"
                        # ),
                        
                        div(
                            
                            class = "center-flex",
                            
                            div(
                                class = "margin10",
                                
                                pickerInput(
                                    inputId = "source", 
                                    label = "", 
                                    choices = unique(df$source) %>% sort(), 
                                    multiple = FALSE
                                )
                            ),
                            
                            div(
                                class = "margin10",
                                
                                pickerInput(
                                    inputId = "pal", 
                                    label = "", 
                                    choices = c("Blues", "Set1"), 
                                    multiple = FALSE,
                                    width = "150px"
                                )
                            )
                            
                        ),
                        
                        
                        div(
                            
                            class = "content-box mb20",
                            
                            div(class = "center-flex medblue em1-5", "Compare Grades"),
                            div(class = "mb20", plotOutput("p_by_grade", height = "500px"))
                            
                        ),
                        
                        div(
                            
                            class = "content-box mb20",
                            
                            div(class = "center-flex medblue em1-5", "Compare Cohorts"),
                            div(class = "mb20", plotOutput("p_by_cohort", height = "500px"))
                            
                        ),
                        
                        
                        div(
                            
                            class = "content-box mb20",
                            
                            div(class = "center-flex medblue em1-5", "Longitudinal Drill Down"),
                            
                            # div(
                            #     
                            #     class = "center-wrap margin30",
                            #     
                            #     "This plot allows the user to drill down into specific demographics for the specific question", tags$br(),
                            #     "The data source is selected at the top of this page", tags$br(),
                            #     "Select the question (within the data source) from the drop down menu", tags$br(),
                            #     "Rows of plots correspond to grade", tags$br(),
                            #     "Columns of plots correspond to gender", tags$br(),
                            #     "The x-axis is year (of the survey)", tags$br(), 
                            #     "The y-axis is the percent of students"
                            #     
                            # ),
                            
                            div(
                                
                                class = "center-wrap",
                                
                                div(
                                    class = "margin10",
                                    
                                    pickerInput(
                                        inputId = "q", 
                                        label = "Question", 
                                        choices = unique(df$q) %>% sort(), 
                                        multiple = FALSE
                                    )
                                ),
                                
                                div(
                                    class = "margin10",
                                    
                                    pickerInput(
                                        inputId = "gender", 
                                        label = "Gender", 
                                        choices = unique(levels(df$gender)), 
                                        selected = c("Female", "Male", "Other"),
                                        multiple = TRUE,
                                        width = "150px"
                                    )
                                ),
                                
                                div(
                                    class = "margin10",
                                    
                                    pickerInput(
                                        inputId = "grade", 
                                        label = "Grade", 
                                        choices = unique(levels(df$grade)), 
                                        selected = c("12", "10", "8", "6"),
                                        multiple = TRUE,
                                        width = "150px"
                                    )
                                ),
                                
                                div(
                                    class = "margin10",
                                    actionLink(inputId = "go", label = "Go", icon = icon("paper-plane"))
                                )
                            ),
                            
                            div(plotOutput("p_by_yr", height = "500px"))
                            
                        )
                    ),  # End Tab Panel Plots
                    
                    tabPanel(
                        
                        ### tables ----
                        title = "Tables",
                        
                        tabsetPanel(
                            
                            tabPanel(
                                
                                title = "By Locale",
                                
                                div(
                                    
                                    class = "content-box mb20",
                                    
                                    # div(
                                    #     class = "center-flex medblue em1-5",
                                    #     "Differences: By Locale"
                                    # ),
                                    
                                    #     div(
                                    #         
                                    #         class = "margin30",
                                    #         
                                    #         "
                                    # This table compares county level data to state level data.
                                    # Click the grey triangles next to column names to sort. 
                                    # This makes it easy to find the biggest differences.
                                    # Filter the table using the boxes below each column name.
                                    # Click the Excel button to download the data.
                                    # "
                                    # 
                                    #     ),
                                    
                                    DT::dataTableOutput("dt_locale")
                                )
                                # 
                                # div(
                                #     class = "content-box mb20",
                                #     plotOutput("p_df_locale")
                                # )
                            ),
                            
                            tabPanel(
                                
                                title = "By Gender",
                                
                                div(
                                    
                                    class = "content-box mb20",
                                    
                                    # div(
                                    #     class = "center-flex medblue em1-5",
                                    #     "Differences: By Gender"
                                    # ),
                                    
                                    #     div(
                                    #         
                                    #         class = "margin30",
                                    #         
                                    #         "
                                    # This table compares data by gender.
                                    # Click the grey triangles next to column names to sort. 
                                    # This makes it easy to find the biggest differences.
                                    # Filter the table using the boxes below each column name.
                                    # Click the Excel button to download the data.
                                    # "
                                    # 
                                    #     ),
                                    
                                    div(DT::dataTableOutput("dt_gender"))
                                )
                            ),
                            
                            tabPanel(
                                
                                title = "By Grade",
                                
                                div(
                                    
                                    class = "content-box mb20",
                                    
                                    # div(
                                    #     class = "center-flex medblue em1-5",
                                    #     "Differences: By Grade"
                                    # ),
                                    
                                    # div(
                                    #     
                                    #     class = "margin30",
                                    #     
                                    #     "
                                    #     This table compares data by grade.
                                    #     Click the grey triangles next to column names to sort. 
                                    #     This makes it easy to find the biggest differences. 
                                    #     Specifically, what are the biggest challenges as kids go from 6th grade to 8th grade, 8th grade to 10th grade, and 10th grade to 12th grade.
                                    #     Filter the table using the boxes below each column name.
                                    #     Click the Excel button to download the data.
                                    #     "
                                    #     
                                    # ),
                                    div(DT::dataTableOutput("dt_grade"))
                                )
                            ),
                            
                            tabPanel(
                                
                                title = "By Cohort",
                                
                                div(
                                    class = "content-box mb20",
                                    div(DT::dataTableOutput("dt_cohort"))
                                )
                            )
                        )
                    ),
                    
                    ### correlations ----
                    tabPanel(
                        
                        title = "Correlations",
                        div(
                            class = "content-box mb20",
                            "Note: some goofy counter-intuitive correlations - TODO check code, dig into survey domain",
                            div(DT::dataTableOutput("dt_corr"))
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
                    tags$a(href="https://www.pccd.pa.gov/Juvenile-Justice/pages/pennsylvania-youth-survey-(pays).aspx", img(src='pccd_icon2.png', height = "20%", width = "20%"), target = "_blank")
                    
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
            
            print("locale")
            print(input$dt_locale_rows_selected)
            
            print(df_p_cohort())
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
        updatePickerInput(session, inputId = "q", label = "Question", choices = questions)
        
    }) %>% 
        
        bindEvent(input$source)
    
    # force drill down plot color update
    observe({
        
        click("go")
        
    }) %>% 
        
        bindEvent(input$pal)
    
    # plots ----
    
    
    ## plot by grade ----
    
    output$p_by_grade <- 
        
        renderPlot({
            
            p_by_grade(.df = l_df2[[input$source]], .var = input$source, .pal = input$pal)
            
        })
    
    ## plot by cohort ----
    
    output$p_by_cohort <- 
        
        renderPlot({
            
            p_by_cohort(.df = l_df2[[input$source]], .var = input$source, .pal = input$pal)
            
        })
    
    ## plot by yr ----
    
    
    click("go")
    
    output$p_by_yr <-
        
        renderPlot({
            
            df_in <- l_df1[[input$q]]
            
            if(!is.null(input$gender)) {df_in <- df_in %>% filter(gender %in% input$gender)}
            if(!is.null(input$grade)) {df_in <- df_in %>% filter(grade %in% input$grade)}
            
            p_by_yr(.df = df_in, .var = input$q, .pal = input$pal) 
            
        }) %>% 
        
        bindEvent(input$go)
    
    # tables ----
    
    
    
    ## diff by locale ----
    
    dt_locale <- 
        
        reactive({
            
            .df <- 
                
                df %>% 
                select(-cohort) %>% 
                pivot_wider(names_from = locale, values_from = percent, values_fn = mean) %>% 
                mutate(leb_state = `Lebanon County` - State) %>% 
                arrange(-year, -abs(leb_state)) %>% 
                desired_outcome() %>% 
                
                select(
                    `Desired Outcome` = desired_outcome,
                    Source = source,
                    Question = q,
                    Year = year,
                    Grade = grade,
                    # Cohort = cohort,
                    Gender = gender,
                    `Lebanon County`,
                    State,
                    `Diff: Lebanon - State` = leb_state
                    
                ) %>%
                
                mutate(across(c(Source:Gender), ~factor(.))) 
            
            
        })
    
    output$dt_locale <- 
        
        DT::renderDataTable({
            
            make_dt(dt_locale()) %>% 
                
                formatPercentage(
                    columns = 7:9,
                    digits = 1
                ) %>% 
                
                formatStyle(
                    columns = 9,
                    color = styleInterval(c(-0.05, 0.05), c("blue", "black", "blue"))
                ) 
        })
    
    ## diff by gender ----
    
    dt_gender <- 
        
        reactive({
            
            .df <- 
                
                df %>%
                select(-cohort) %>%
                pivot_wider(names_from = gender, values_from = percent, values_fn = mean) %>% 
                
                mutate(fm_diff = Female - Male) %>% 
                mutate(om_diff = Other - Male) %>% 
                mutate(of_diff = Other - Female) %>% 
                arrange(-year, -abs(fm_diff)) %>% 
                desired_outcome() %>% 
                
                select(
                    `Desired Outcome` = desired_outcome,
                    Source = source,
                    Question = q,
                    Year = year,
                    Grade = grade,
                    # Cohort = cohort,
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
        })
    
    
    output$dt_gender <- 
        
        DT::renderDataTable({
            
            make_dt(dt_gender()) %>% 
                
                formatPercentage(
                    columns = 7:13,
                    digits = 1
                ) %>% 
                
                formatStyle(
                    columns = 11:13,
                    color = styleInterval(c(-0.05, 0.05), c("blue", "black", "blue"))
                ) 
            
        })
    
    ## diff by grade ----
    
    dt_grade <- 
        
        reactive({
           
            .df <- 
                
                df %>%
                
                filter(grade != "All") %>% 
                select(-cohort) %>% 
                pivot_wider(names_from = grade, values_from = percent, values_fn = mean) %>% 
                
                mutate(g12g10 = `12` - `10`) %>% 
                mutate(g10g8 = `10` - `8`) %>% 
                mutate(g8g6 = `8` - `6`) %>% 
                arrange(-year, -abs(g8g6)) %>% 
                desired_outcome() %>% 
                
                select(
                    `Desired Outcome` = desired_outcome,
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
            
        })
    
    output$dt_grade <-
        
        DT::renderDataTable({
            
            make_dt(dt_grade())  %>% 
                
                formatPercentage(
                    columns = 7:13,
                    digits = 1
                ) %>% 
                
                formatStyle(
                    columns = 11:13,
                    color = styleInterval(c(-0.05, 0.05), c("blue", "black", "blue"))
                ) 
        })
    
    ## diff by cohort ----
    
    dt_cohort <- 
        
        reactive({
            
            .df <-
                
                df %>% 
                filter(!is.na(cohort)) %>% 
                # count(cohort) %>% 
                select(-year) %>%
                pivot_wider(names_from = cohort, values_from = percent, values_fn = mean) %>% 
                desired_outcome() %>% 
                
                select(
                    `Desired Outcome` = desired_outcome,
                    Source = source,
                    Question = q,
                    # Year = year,
                    Grade = grade,
                    Gender = gender,
                    Locale = locale,
                    contains("20")
                    
                ) %>%
                
                mutate(across(c(Source:Locale), ~factor(.))) 
            
        })
    
    output$dt_cohort <-
        
        DT::renderDataTable({
            
            make_dt(dt_cohort())  %>% 
                
                formatPercentage(
                    columns = 7:ncol(dt_cohort()),
                    digits = 1
                ) 
            
        })
    
    
    # modal plots ----
    
    ## locale ----
    
    df_p_locale <- 
        
        reactive({
            
            df_mod_locale <- 
                
                dt_locale() %>% 
                slice(input$dt_locale_rows_selected) %>% 
                select(Source, Question) %>% 
                inner_join(dt_locale()) %>% 
                select(-contains("iff"), -contains("come")) %>% 
                pivot_longer(cols = c("Lebanon County", "State"), names_to = "locale", values_to = "percent") %>% 
                rename(q = Question)
            
            names(df_mod_locale) <- str_to_lower(names(df_mod_locale))
            
            df_mod_locale <- 
                df_mod_locale %>% 
                mutate(year = as.numeric(as.character(year))) %>% 
                filter(gender != "All") %>% 
                filter(grade != "All")
            
            df_mod_locale
        })
    
    output$p_df_locale <- 
        
        renderPlot({
            
            req(!is.null(input$dt_locale_rows_selected))
            p_by_yr(.df = df_p_locale(), .var = unique(df_p_locale()$q), .pal = input$pal)
            
        })
    
    
    observe({
        
        showModal(
            
            modalDialog(
                
                div(
                    class = "content-box mb20",
                    plotOutput("p_df_locale")
                ),
                
                size = "l", easyClose = TRUE, fade = TRUE
            )
        )
        
    }) %>% 
        
        bindEvent(input$dt_locale_rows_selected)
    
    
    ## gender ----
    
    df_p_gender <- 
        
        reactive({
            
            df_mod_gender <- 
                
                dt_gender() %>% 
                slice(input$dt_gender_rows_selected) %>% 
                select(Source, Question) %>%
                inner_join(dt_gender()) %>% 
                select(-contains("iff"), -contains("come")) %>%
                pivot_longer(cols = c("Female", "Male", "Other", "All"), names_to = "gender", values_to = "percent") %>%
                rename(q = Question)

            names(df_mod_gender) <- str_to_lower(names(df_mod_gender))

            df_mod_gender <-
                df_mod_gender %>%
                mutate(year = as.numeric(as.character(year))) %>%
                filter(gender != "All") %>%
                filter(grade != "All")
            
            df_mod_gender
        })
    
    output$p_df_gender <-

        renderPlot({

            req(!is.null(input$dt_gender_rows_selected))
            p_by_yr(.df = df_p_gender(), .var = unique(df_p_gender()$q), .pal = input$pal)

        })
    
    observe({

        showModal(

            modalDialog(

                div(
                    class = "content-box mb20",
                    plotOutput("p_df_gender")
                ),

                size = "l", easyClose = TRUE, fade = TRUE
            )
        )

    }) %>%

        bindEvent(input$dt_gender_rows_selected)
    
    
    ## grade ----
    
    df_p_grade <- 
        
        reactive({
            
            df_mod_grade <- 
                
                dt_grade() %>% 
                slice(input$dt_grade_rows_selected) %>% 
                select(Source, Question) %>%
                inner_join(dt_grade()) %>% 
                select(-contains("iff"), -contains("come")) %>%
                pivot_longer(cols = c("6", "8", "10", "12"), names_to = "grade", values_to = "percent") %>%
                rename(q = Question) %>% 
                mutate(grade = factor(grade, levels = rev(c("6", "8", "10", "12"))))

            names(df_mod_grade) <- str_to_lower(names(df_mod_grade))

            df_mod_grade <-
                df_mod_grade %>%
                mutate(year = as.numeric(as.character(year))) %>%
                filter(grade != "All") %>%
                filter(grade != "All")
            
            df_mod_grade
        })
    
    output$p_df_grade <-
        
        renderPlot({
            
            req(!is.null(input$dt_grade_rows_selected))
            p_by_yr(.df = df_p_grade(), .var = unique(df_p_grade()$q), .pal = input$pal)
            
        })
    
    observe({

        showModal(

            modalDialog(

                div(
                    class = "content-box mb20",
                    plotOutput("p_df_grade")
                ),

                size = "l", easyClose = TRUE, fade = TRUE
            )
        )

    }) %>%

        bindEvent(input$dt_grade_rows_selected)
    
    
    
    ## cohort ----
    
    df_p_cohort <- 
        
        reactive({
            
            df_mod_cohort <- 
                
                dt_cohort() %>% 
                slice(input$dt_cohort_rows_selected) %>% 
                select(Source, Question, Locale, Gender) %>%
                inner_join(dt_cohort()) %>%
                
                select(-contains("iff"), -contains("come")) %>%
                pivot_longer(cols = contains("20"), names_to = "cohort", values_to = "percent") %>%
                rename(q = Question) %>%
                mutate(cohort = factor(cohort, levels = as.character(seq(2018,2050,2)))) 

            names(df_mod_cohort) <- str_to_lower(names(df_mod_cohort))

            df_mod_cohort <-
                df_mod_cohort %>%
                mutate(grade = as.numeric(as.character(grade)))
            
            df_mod_cohort
        })
    
    
    output$p_df_cohort <-

        renderPlot({

            req(!is.null(input$dt_cohort_rows_selected))
            p_by_cohort(.df = df_p_cohort(), .var = unique(df_p_cohort()$q), .pal = input$pal)

        })
    
    observe({

        showModal(

            modalDialog(

                div(
                    class = "content-box mb20",
                    plotOutput("p_df_cohort")
                ),

                size = "l", easyClose = TRUE, fade = TRUE
            )
        )

    }) %>%

        bindEvent(input$dt_cohort_rows_selected)
    
    
    # correlations ----
    
    output$dt_corr <- DT::renderDataTable({
        
        df_corr %>% 
            
            make_dt() %>% 
            
            formatStyle(
                
                columns = "Correlation", 
                target = "cell",
                backgroundColor = styleInterval(c(seq(-1, 1, 0.01)), colorRampPalette(c("#b3cde3", "#FFFFFF", "#fbb4ae"))(202))
            )
        
    })
    
    
    
} # End server

shinyApp(ui = ui, server = server)