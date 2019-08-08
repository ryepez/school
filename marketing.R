## app.R ##
library(shinydashboard)
library(ggplot2)
library(shiny)

#change the file path for the data 
MF <- read.csv("~/Desktop/Spring2018/Marketing_Campaign_Effectiveness.csv")
MF$MarketID <- as.factor(MF$MarketID)
MF$LocationID <- as.factor(MF$LocationID)
MF$Promotion <- as.factor(MF$Promotion)
MF$week <- as.factor(MF$week)
MFC <- subset(MF, select = c("AgeOfStore", "SalesInThousands"))
MFD <- subset(MF, select=c("MarketID", "MarketSize", "LocationID", "Promotion", "week"))
MFN <- subset(MF, select=c("MarketID", "SalesInThousands"))
MFN$twoMarkets <- ifelse(MFN$MarketID!=3, 1, 2)
MFnames <- c("Linear Regression with all Markets", "Linear Regression without market 3", "Linear Regression with only market 3")
s <- subset(MF, MF$MarketID!=3)
s1 <- subset(MF, MF$MarketID==3)

ui <- dashboardPage(
  dashboardHeader(title = "Marketing Metrics"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Background Information", tabName = "widgetsB", icon = icon("info")),
      menuItem("Business Problem", tabName = "widgets", icon = icon("briefcase")),
      menuItem("Data Exploration", tabName = "dashboard1", icon = icon("flask")),
      menuItem("Model", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Recomendation", tabName = "widgets1", icon = icon("book"))
    )
  ),
  
  
  dashboardBody(
    tabItems(
      # 3 tab content
      tabItem(tabName = "dashboard",
              fluidRow(
                         box(width = 10, 
                             selectInput("xvariableh","Model:", choices = MFnames),
                             verbatimTextOutput("model")
                           
                           )
                
              )
              
              
              
      ),
      
      
      # First tab content
      tabItem(tabName = "dashboard1",
              fluidRow(
                box(width = 8,
                    fluidPage(
                      sliderInput("xvariable", "Select Graphs", 1, 3, 1),
                      #selectInput("xvariable","Variable:", choices = names(MFN)),
                      plotOutput("plot1")
                    )), 
                box(width = 4,
                    fluidPage(
                      h1("Observations"),
                      h3(htmlOutput("caption"))
                      )
                    )
                
              ),
              
              fluidRow(
                box(width = 8, fluidPage(
                  
                  # Output: Header + summary of distribution ----
                  h4("Summary"),
                  verbatimTextOutput("summary"),
                  # Output: Header + table of distribution ----
                  tableOutput("view")
                )
                    )
                
              )
              
              
              
      ),
      
      
      
      # Second tab content
      tabItem(tabName = "widgets",
              h1("What marketing campaign is the most effective?"),
              br(" "),
              h2(tags$ul(
                tags$li("Three possible marketing campaigns for promoting the new product"), 
                tags$li("The item was introduced randomly at selected markets with a different promotion each week"), 
                tags$li("The goal is to determine which promotion had the greatest effect on sales")
              )
              )), 
      
      # 3 tab content
      tabItem(tabName = "widgets1",
              h2("Promotion one is the most effective"),
              h2("Market three (independent of everything) is the top selling market"),
              h2("We should look at market three to see why it is selling so much")
              ),
     
       # background
      tabItem(tabName = "widgetsB",
              h1("Fast food chain plans to add a new item to its menu"),
              br(" "),
              h2(tags$ul(
                tags$li("They are still undecided between three possible marketing campaigns for promoting the new product"), 
                tags$li("They ran a study on their 548 stores"), 
                tags$li("The dataset created from the study has the following columns: MarketID, MarketSize, LocationID, AgeOFstore,Promotion, Week, SalesInThousands")
              )
              ))
    
      
      
      
      
        
    )
  )
)
server <- function(input, output) {
 
  output$plot1 <- renderPlot({ if (input$xvariable == "1") {
    output$caption <- renderText({
      str1 <- "- Ten unique markets"
      str3 <- "  "
      str2 <- "- They are not evenly distributed"
      str4 <- "- Market two is the smallest and market three is the biggest"
      HTML(paste(str1, str3, str2, str3, str4, sep = '<br/>'))
    
    }) 
    
    output$summary <- renderPrint({
      summary(MF$MarketID)
    })
    
    ggplot(MF, aes(x=MF[,input$xvariable])) +
      geom_bar(fill="#3b5998")+ ggtitle("Distribution of stores by market ID")+theme(plot.title = element_text(hjust = 0.5))+xlab("Market ID")
   
    
    } else if (input$xvariable == "2") {
      output$caption <- renderText({
        str1 <- "- We don't see a normal distribution for sales"
        str3 <- "  "
        str2 <- "- It looks like we have two distinct markets"
        str4 <- "- The mean is $53,470 in sales per week"
        HTML(paste(str1, str3, str2, str3, str4, sep = '<br/>'))
      }) 
      
      output$summary <- renderPrint({
        summary(MF$SalesInThousands)
      })
      ggplot(MF, aes(x=SalesInThousands)) + 
        geom_histogram(aes(y=..density..),    
                       binwidth=0.5,
                       fill="#192C47") + geom_density(alpha=.6, fill="#75AADB")+ggtitle("Distribution of Sales")+theme(plot.title = element_text(hjust = 0.5))
      #ggplot(MF, aes(MF[,input$xvariable]))+ geom_histogram(aes(y=..density..), binwidth=0.5,
       #                                                     fill="#192C47")+geom_density(alpha=.6, fill="#75AADB")+ ggtitle("Distribution of Sales")+theme(plot.title = element_text(hjust = 0.5))+xlab("Sales Amount (in thousands dollars)")
    }
    else if (input$xvariable == "3") { 
      output$summary <- renderPrint({
        summary(MF$SalesInThousands)
      })
      
      output$caption <- renderText({
        str1 <- "- We see that market three has a different distribution than the other markets"
        str3 <- "  "
        str2 <- "- The mean sales for market three is $84,970 while it is $47,440 for the rest of the markets"
        str4 <- "- We should take into consideration those facts when we create our model to measure the effectiveness of the promotions"
        HTML(paste(str1, str3, str2, str3, str4, sep = '<br/>'))
      })
    ggplot(MF, aes(x=SalesInThousands, fill=MarketID)) +
      geom_histogram(binwidth=.4, alpha=.5, position="identity")+ggtitle("Distribution of Sales")+theme(plot.title = element_text(hjust = 0.5))
    }
    })
  output$model <- renderPrint({
  if (input$xvariableh == "Linear Regression with all Markets") {
    reg1 <-  lm(MF$SalesInThousands~MF$Promotion+MF$MarketID, data=MF) 
    summary(reg1)
    
  } else if (input$xvariableh == "Linear Regression without market 3") {
    
    regNot3 <-  lm(s$SalesInThousands~s$Promotion, data=s) 
    summary(regNot3)
  }
    
    else if (input$xvariableh == "Linear Regression with only market 3") {
      
      reg3 <-  lm(s1$SalesInThousands~s1$Promotion, data=s1) 
      summary(reg3)
    } 
    
    
  })
  
}

shinyApp(ui, server)