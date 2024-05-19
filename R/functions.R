

p_by_yr <- function(.df, .var, .pal = "Blues"){
    
    p <- 
        
    .df %>% 
        
        ggplot() +
        geom_point(aes(x = year, y = percent, col = locale)) +
        geom_line(aes(x = year, y = percent, col = locale)) +
        
        # State Confidence Intervals
        geom_segment(
            data = .df %>% filter(locale == "State"),
            # NOTE took confidence intervals from PAYS website; these are approximate
            aes(x = year, xend = year, y = percent - 0.015, yend = percent + 0.015, col = locale)
        ) +
        
        # Reference Line
        # FIXME turned off when filters added (in case All not selected)
        # geom_hline(
        #     aes(
        #         yintercept = .df %>% 
        #             filter(locale == "State") %>% 
        #             filter(grade == "All") %>% 
        #             filter(gender == "All") %>% 
        #             filter(year == max(year)) %>% 
        #             pull(percent)
        #     ),
        #     col = brewer.pal(brewer.pal.info["Dark2", "maxcolors"], "Dark2")[3],
        #     lty = 2,
        #     alpha = 0.9
        # ) +
        
        facet_grid(grade ~ gender) +
        
        ggtitle(unique(.df$source), subtitle = .var) +
        xlab("Year") +
        ylab("") +
        
        scale_y_continuous(labels = scales::percent_format()) +
        scale_x_continuous(
            limits = c(min(.df$year) - 0.37, max(.df$year) + 0.37),
            breaks = .df %>% distinct(year) %>% pull(year)
        ) +
        expand_limits(y = 0) +
        
        # scale_color_brewer(palette = .pal) +
        
        theme_bw(base_size = 15) +
        
        theme(
            legend.title = element_blank(),
            plot.title = element_text(hjust = 0.5),
            plot.subtitle = element_text(hjust = 0.5),
            panel.grid.major.x = element_blank(),
            panel.grid.minor.x = element_blank(),
            panel.grid.minor.y = element_blank(),
            axis.ticks.y = element_blank(),
            
            panel.border = element_rect(colour = "grey95"),
            # panel.border = element_blank(),
            strip.background = element_rect(fill = "grey95", colour = "grey90")
        )
    
    if(.pal == "Blues") p <- p + scale_color_manual(values = brewer.pal(7, .pal)[c(7,4)])
    if(.pal != "Blues") p <- p + scale_color_brewer(palette = .pal)
    
    p
    
    
}



p_by_grade <- function(.df, .var, .pal = "Blues"){
    
    # n <- .df %>% distinct(year) %>% nrow()
    # mypalette <- brewer.pal(n+3, .pal)[3:(n+3)]
    
    .df %>% 
        ggplot() +
        geom_point(aes(x = grade, y = percent, col = year)) +
        geom_line(aes(x = grade, y = percent, col = year)) +
        
        # # State Confidence Intervals
        # geom_segment(
        #     data = df_plot2 %>% filter(locale == "state"),
        #     aes(x = grade, xend = year, y = percent - 0.015, yend = percent + 0.015, col = grade)
        # ) +
        
        facet_grid(locale ~ q, labeller = label_wrap_gen(width = 20, multi_line = TRUE)) +
        
        
        xlab("Grade") +
        ylab("") +
        ggtitle(.var) +
        
        scale_y_continuous(labels = scales::percent_format()) +
        expand_limits(y = 0) +
        
        scale_color_brewer(palette = .pal) +
        # scale_color_manual(values = mypalette) +
        
        theme_bw(base_size = 15) +
        
        theme(
            # legend.title = element_blank(),
            plot.title = element_text(hjust = 0.5),
            panel.grid.major.x = element_blank(),
            panel.grid.minor.x = element_blank(),
            panel.grid.minor.y = element_blank(),
            axis.ticks.y = element_blank(),
            
            panel.border = element_rect(colour = "grey95"),
            strip.background = element_rect(fill = "grey95",color = "grey90"),
            strip.text = element_text(size = 10)
        )
}


# .var = "Access and Willingness to Use"
# .df = l_df2[[.var]]

p_by_cohort <- function(.df, .var, .pal = "Blues"){
    
    .df <- .df %>% mutate(year = as.numeric(as.character(year)))
    
    
    .df %>% 
        
        ggplot() +
        geom_point(aes(x = grade, y = percent, col = cohort)) +
        geom_line(aes(x = grade, y = percent, col = cohort)) +
        
        # State Confidence Intervals
        geom_segment(
            data = .df %>% filter(locale == "State"),
            # NOTE took confidence intervals from PAYS website; these are approximate
            aes(x = grade, xend = grade, y = percent - 0.015, yend = percent + 0.015, col = cohort)
        ) +
        
        facet_grid(locale ~ q, labeller = label_wrap_gen(width = 20, multi_line = TRUE)) +
        # facet_grid(locale ~ q) +
        
        ggtitle(.var) +
        xlab("Grade") +
        ylab("") +
        
        scale_y_continuous(labels = scales::percent_format()) +
        # scale_x_continuous(
            # limits = c(min(.df$year) - 0.37, max(.df$year) + 0.37),
            # breaks = .df %>% distinct(year) %>% pull(year)
        # ) +
        expand_limits(y = 0) +
        
        scale_color_brewer(palette = .pal) +
        
        theme_bw(base_size = 15) +
        
        theme(
            # legend.title = element_blank(),
            plot.title = element_text(hjust = 0.5),
            plot.subtitle = element_text(hjust = 0.5),
            panel.grid.major.x = element_blank(),
            panel.grid.minor.x = element_blank(),
            panel.grid.minor.y = element_blank(),
            axis.ticks.y = element_blank(),
            
            panel.border = element_rect(colour = "grey95"),
            # panel.border = element_blank(),
            strip.background = element_rect(fill = "grey95", colour = "grey90")
        )
}


desired_outcome <- function(.df, .source = "source", .q = "q"){
    
    .df %>% 
        
        # distinct(source, q) %>% 
        
        mutate(
            
            desired_outcome = 
                
                case_when(
                    grepl(pattern = "^attitude|involve|^positive|^percept|^protective", !!sym(.source), ignore.case = TRUE) ~ "High",
                    grepl(pattern = "adults would think it was wrong|would be caught by the police|knows where i am|clear family rules|rules in family", !!sym(.q), ignore.case = TRUE) ~ "High",
                    grepl(pattern = "I was not bullied", !!sym(.q), ignore.case = TRUE) ~ "High",
                    grepl("^No", !!sym(.q), ignore.case = TRUE) & grepl("Bullying Freq", !!sym(.source), ignore.case = TRUE) ~ "High",
                    TRUE ~ "Low"
                )
        ) %>% 
        
        mutate(desired_outcome = factor(desired_outcome))
}

make_dt <- function(.df){
    
    .df %>% 
    
    datatable(
        options = list(
            dom = "Bftip",
            buttons = c('excel'),
            pageLength = 100, 
            scrollY = 400, 
            scrollX = TRUE,
            # fixedColumns = list(leftColumns = 2),
            columnDefs = list(
                list(className = 'dt-center', targets = 0:(ncol(.)-1))
            )
        ),
        rownames = FALSE,
        filter = "top",
        style = "bootstrap",
        selection = "single",
        extensions = c("Buttons")
        # escape = FALSE   
    )
    
    
}

