

p_by_yr <- function(.df, .var){
    
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
        xlab("") +
        ylab("") +
        
        scale_y_continuous(labels = scales::percent_format()) +
        scale_x_continuous(
            limits = c(min(.df$year) - 0.37, max(.df$year) + 0.37),
            breaks = .df %>% distinct(year) %>% pull(year)
        ) +
        expand_limits(y = 0) +
        
        scale_color_manual(values = c("DarkBlue", "CornflowerBlue")) +
        
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
}



p_by_grade <- function(.df, .var){
    
    n <- .df %>% distinct(year) %>% nrow()
    mypalette <- brewer.pal(n+3,"Blues")[3:(n+3)]
    # mypalette <- brewer.pal(6,"Blues")[3:6]    
    
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
        
        
        xlab("") +
        ylab("") +
        ggtitle(.var) +
        
        scale_y_continuous(labels = scales::percent_format()) +
        expand_limits(y = 0) +
        
        scale_color_manual(values = mypalette) +
        
        theme_bw(base_size = 15) +
        
        theme(
            legend.title = element_blank(),
            plot.title = element_text(hjust = 0.5),
            panel.grid.major.x = element_blank(),
            panel.grid.minor.x = element_blank(),
            panel.grid.minor.y = element_blank(),
            axis.ticks.y = element_blank(),
            
            panel.border = element_rect(colour = "grey85"),
            strip.background = element_rect(fill = "grey90",color = "grey90"),
            strip.text = element_text(size = 10)
        )
}


make_dt <- function(.df){
    
    .df %>% 
    
    datatable(
        options = list(
            dom = "Bt",
            buttons = c('excel'),
            pageLength = nrow(.), 
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

