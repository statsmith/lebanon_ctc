
# Help Info
# Module to reduce app space (line of code)

info_helpUI <- function(id){
      
      tagList(
            
            div(
                  
                  div(
                        tabsetPanel(
                              
                              tabPanel(title = "Info 1",
                                       "Hello"
                              ),
                              
                              tabPanel(title = "Info 2",
                                       "World"
                              )
                              
                              
                        )
                        
                  )
            )
      )
      
}

info_helpServer <- function(id){}
