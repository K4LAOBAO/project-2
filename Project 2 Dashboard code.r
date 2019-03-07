library(ggplot2)
library(ggthemes)
library(dplyr)
library(stringr)
library(shiny)
library(shinydashboard)


data = read.csv('WA_Fn-UseC_-Telco-Customer-Churn.csv')

ui = dashboardPage(
  skin = 'red',
  
  dashboardHeader(title = 'Telco Customer Churn'),
  
  dashboardSidebar(disable = TRUE),
  
  dashboardBody(
              
              fluidRow(
                infoBox("No. of customers","7043" ,icon = icon("users"), color = 'light-blue'),
                infoBox('Churn Rate', "26.5%", icon = icon("thumbs-up", lib = "glyphicon"),color = 'light-blue'),
                infoBox("Different variables", "21", icon = icon("list"), color = 'light-blue')
                
              ),
              fluidRow(
                box(
                  title = "Distribution of variables",width = 6,status = "primary", height = 620,
                  selectInput(
                    inputId = "var1", 
                    label = HTML("Below are distributions of different variables, 
                                 you can choose the variable you want to see. "), 
                    choices = c("Customer Churn"="Churn",
                                "Gender"="gender",
                                "Senior Citizen"="SeniorCitizen",
                                "Number of months spent with us"="tenure",
                                "Phone Service"="PhoneService",
                                "Multiple Lines"="MultipleLines",
                                "Internet Service"="InternetService",
                                "Contract Type"="Contract",
                                "Paperless Billing"="PaperlessBilling",
                                'Payment Method'='PaymentMethod',
                                'Monthly Charges'= 'MonthlyCharges',
                                'Total Charges' = 'TotalCharges'
                                )),
                  
                  plotOutput(outputId = "dist")),
                
                box(
                  title = "Correlation between varibales and churn rate",width = 6,status = "primary",height = 620,
                  selectInput(
                    inputId = "var2", 
                    label = HTML("Below charts show the correlation between variables and churn rate/amount."),
                    choices = c(
                      "Gender"="gender",
                      "Senior Citizen"="SeniorCitizen",
                      "Number of months spent with us"="tenure",
                      "Phone Service"="PhoneService",
                      "Multiple Lines"="MultipleLines",
                      "Internet Service"="InternetService",
                      "Contract Type"="Contract",
                      "Paperless Billing"="PaperlessBilling",
                      'Payment Method'='PaymentMethod',
                      'Monthly Charges'= 'MonthlyCharges',
                      'Total Charges' = 'TotalCharges')),
                  selectInput(
                    inputId = "y_var",label = 'Please choose churn indicator.',
                    choices = c('Churn amount'='yes',
                                'Churn rate' ='ratio')),
                  
                  plotOutput(outputId = "correl"))
                
                )
  )
)


server = function(input,output){
  
  output$dist = renderPlot({
    if (input$var1 %in% c("tenure","MonthlyCharges","TotalCharges")) {
      ggplot(data,aes_string(x= input$var1)) +
        geom_histogram(fill = 'White',color = 'black')+
        ylab('number of people')+
        theme(panel.background = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(), 
              panel.border = element_blank(),
              axis.title = element_blank(), 
              axis.ticks = element_blank()) 
      
    } else if (input$var1 == "PaymentMethod"){
      ggplot(data,aes_string(x= input$var1)) +
        geom_bar(fill = 'White',color = 'black',width =0.4 )+
        coord_flip()+
        ylab('number of people')+
        theme(panel.background = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(), 
              panel.border = element_blank(),
              axis.title = element_blank(), 
              axis.ticks = element_blank())
    } else {
      ggplot(data,aes_string(x= input$var1)) +
        geom_bar(fill = 'White',color = 'black',width = 0.4)+
        ylab('number of people')+
        theme(panel.background = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(), 
              panel.border = element_blank(),
              axis.title = element_blank(), 
              axis.ticks = element_blank())
    }
    
  })
  

  output$correl <- renderPlot({
    data2 = data %>%
      group_by_(input$var2) %>%
      summarise(count = n(), yes = sum(Churn=="Yes"), no = sum(Churn=="No"))
    
    data2$ratio = data2$yes / data2$count
    
    ggplot(data2, aes_string(x=input$var2, y=input$y_var)) +
      geom_col(fill = 'white',color = 'black',width = 0.4)+
      theme(panel.background = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(), 
            panel.border = element_blank(),
            axis.title = element_blank(), 
            axis.ticks = element_blank())
    
  })  
  
}

shinyApp(ui,server)