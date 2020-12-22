### Book Reading Dashboard ###
# Clear Environment#
rm(list = ls(all.names = TRUE))

#Load Packages#
library(readxl)
library(dplyr)
library(plotly)
library(ggplot2)
library(shinythemes)
library(shinyWidgets)
library(shiny)


## Load Data ##


pdata <- read_excel("Book_Collection.xlsx", sheet = 1)



### UI View ###

ui <- fluidPage(setBackgroundColor("#F4ECF7"),
                titlePanel("Reading Dashboard"),
                sidebarLayout(sidebarPanel(radioButtons("yearfilter",
                                                        "Filter by Year Read",
                                                        choices = c("All Years", unique(pdata$Year_Read)))),
                              mainPanel(fluidRow(column(6,h3(htmlOutput("bcount", align = "center"))),
                                                 column(6,h3(htmlOutput("ucount",align = "center")))),
                                        h3("Book Graphs", align = "center"),
                                        fluidRow(column(6,plotlyOutput("histplot", height = "300px", width = "100%")),
                                                 column(6,plotlyOutput("barplot", height = "300px", width = "100%"))),
                                        h3("Table", align = "center"),
                                        DT::dataTableOutput("dtable"))
                ))#mainpanel end







### Server View ###

server <- function(input,output) {
  ### Filter for graphs ###
  
  ## data for plots filtered##
  p1data <- reactive({
    if(input$yearfilter == "All Years") {
      pdata
    } else {pdata %>%
        filter(Year_Read == input$yearfilter)}
  })
  
  ## book stats ##
  sdata <- reactive({
    if(input$yearfilter == "All Years"){
      pdata} else{pdata %>%
          filter(Year_Read == input$yearfilter)}
    
  })
  
  bookcount <- reactive({nrow(sdata()[,"Book"])})
  
  uniquecount <- reactive({n_distinct(sdata()[,"Book"])})
  
  ## book stats output ##
  output$bcount <- reactive({paste("Total Books Read:", bookcount())})
  
  output$ucount <- reactive({paste("Unique Books Read:", uniquecount())})
  
  
  
  ##output for histogram##
  output$histplot <- renderPlotly({
    plt1 <- ggplot(data = p1data(), mapping = aes(x = Pages)) + geom_density(alpha = .2, fill = "#FF6666") +
      geom_vline(aes(xintercept = mean(Pages, na.rm = T)), color = 'black', size = .5) +
      ggtitle("Distribution of Page Count") + xlab("")
    plt1 <- ggplotly(plt1)
    plt1 <- plt1 %>% layout(showlegend = FALSE, plot_bgcolor =  "rgba(0, 0, 0, 0)",
                            paper_bgcolor = "rgba(0, 0, 0, 0))")
  })
  
  
  ## data for barplot ##
  
  p2data <- reactive({ if(input$yearfilter == "All Years"){
    pdata %>%
      group_by(F_NF)%>%
      tally()%>%
      mutate(Count = n)%>%
      select(-c(n))%>%
      data.frame()} else {
        pdata %>% 
          filter(Year_Read == input$yearfilter)%>%
          group_by(F_NF)%>%
          tally()%>%
          mutate(Count = n)%>%
          select(-c(n))%>%
          data.frame()}
  })
  ## Output for barplot ##
  output$barplot <- renderPlotly({
    plt2 <- ggplot(data = p2data(), mapping = aes(x = F_NF, y = Count, fill = F_NF)) +
      geom_bar(stat = 'identity', show.legend = FALSE) + ggtitle("Category of Books") +xlab("")
    
    plt2 <- ggplotly(plt2)
    plt2 <- plt2 %>% layout(showlegend = FALSE, plot_bgcolor =  "rgba(0, 0, 0, 0)",
                            paper_bgcolor = "rgba(0, 0, 0, 0))")
  })
  
  
  
  
  
  
  ### Filter for Table ###
  
  output$dtable <- DT::renderDataTable({
    if(input$yearfilter == "All Years") { 
      pdata } 
    else {
      pdata %>% 
        filter(Year_Read == input$yearfilter)}
  })
  
}


shinyApp(ui,server)
