library(shiny)
library(ggplot2)
library(API)

if(!("API" %in% installed.packages()[,"Package"])){
  
  devtools::install_github("Siddhesh19991/Lab_5")
  
  library("API")
  
}


#Packages used: "readxl", "shiny", "ggplot2" , "dplyr" 

Sweden_election<-election()

Sweden_election$clean()

ui <- shiny::fluidPage(
  
  # App title ----
  shiny::titlePanel(
    h1("Sweden Election Data ",align = "center")
  ),
  
  shiny::sidebarLayout(
    shiny::sidebarPanel(
      shiny::selectInput("refer","Choose:",choices = c("2022 election party vote counts","Share of total votes","2022 vs 2018 vote difference"))
    ),
    
    shiny::mainPanel(
      shiny::plotOutput("myplot",height = 800)
    )
  )
)

server <- function(input, output) {
  
  output$myplot <- shiny::renderPlot({
    answer <- input$refer
    
    if (answer == "2022 election party vote counts"){
      ggplot2::ggplot(Sweden_election$election_data,aes(Parti,sort(Roster_2022,decreasing = TRUE)))+geom_col(fill = "forestgreen")+
        labs(x="Party names", y ="Total Number of Votes", title = "2022 election party vote counts")+ 
        theme(text = element_text(size=20),axis.text.x = element_text(angle=90, hjust=1), plot.title = element_text(hjust = 0.5)) + 
        scale_y_continuous(labels = scales::comma_format())
    }else if (answer== "2022 vs 2018 vote difference"){
      ggplot2::ggplot(Sweden_election$election_data,aes(Parti,sort(Diff_4, decreasing = TRUE)))+geom_col(fill = "maroon")+
        labs(x="Party names", y ="Difference", title = "2022 vs 2018 vote difference")+ 
        theme(text = element_text(size=20),axis.text.x = element_text(angle=90, hjust=1), plot.title = element_text(hjust = 0.5)) + 
        scale_y_continuous(labels = scales::comma_format())
    } else if (answer == "Share of total votes"){
      total<-sum(Sweden_election$election_data$Roster_2022)
      Sweden_election$election_data$Party<-Sweden_election$election_data$Parti
      Sweden_election$election_data$share<-(Sweden_election$election_data$Roster_2022/total)*100
      ggplot2::ggplot(Sweden_election$election_data,aes(x="",y=share,fill = Party))+ geom_bar(stat="identity")+ labs(x = "", y="", title = "Share of total votes")+
        coord_polar("y", start=0)+ theme(text = element_text(size=20), plot.title = element_text(hjust = 0.5))
    }
  })
}

shiny::shinyApp(ui = ui, server = server)