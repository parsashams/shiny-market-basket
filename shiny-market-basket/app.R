#   shiny market basket - quick and dirty tool for market basket analysis
# using Apriori rule search. 

library(shinydashboard)
library(dplyr)
library(magrittr)
library(arules)

data("Groceries")

ui <- dashboardPage(skin="yellow",
  dashboardHeader(title = "Market Basket"),
  dashboardSidebar(
    fileInput("file1","Choose CSV Data",accept=c(".csv","text/csv","text/plain")),
    sidebarMenu(
      menuItem("Overview", tabName = "overview", icon = icon("dashboard"), selected = T),
      menuItem("Explore", tabName = "explore", icon = icon("binoculars"), selected = F)
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "overview",
        fluidPage(
          
        )
      ),
      tabItem(tabName = "explore",
        fluidPage(
        fluidRow(box(selectInput("item","Item",choices = c(), selected = "")
                     , width = 12)),
        fluidRow(box(plotOutput("plt_item_freq"))),
        fluidRow(box(verbatimTextOutput("txt_overview"),
                     title = "Summary", width=12))
        )
      )
    )
  )
)


server <- function(input, output, session) {
  ret_data_file <- reactive({
    message("loading ", input$file1$datapath)
    # data_file <<- tryCatch(read.transactions(input$file1$datapath, sep = ","),
    #                        error=function(e) e)
    # data_file
    Groceries
  })
  
  df_items <- reactive({
    data_file <- ret_data_file()
    
    ret <- as.data.frame(itemFrequency(data_file))
    ret$item=rownames(ret)
    row.names(ret) <- NULL
    names(ret) <- c("freq","item")
    ret %<>% arrange(desc(freq))
    updateSelectInput(session,"item",choices = ret$item, selected = ret$item[1])
    ret
  })
  
  output$txt_overview <- renderPrint({
    validate(need(input$file1$datapath, "no data yet..."))
    summary(data_file)
  })
  
  output$plt_item_freq <- renderPlot({
    a <- df_items()
    b <- input$item
    ggplot(a %>% filter(item==input$item), aes(x=1,y=freq)) +
      geom_bar(stat="identity", fill="blue3") + ylim(0,max(a$freq)) + 
      theme_minimal() +
      labs(x=input$item,y="Frequency") +
      scale_x_continuous(breaks=NULL)
  })
  
}

shinyApp(ui, server)