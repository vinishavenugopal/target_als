#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

#####Acronym
#NOS-number of subjects
#####

library(dplyr)
library(tidyverse)
library(readxl)
library(tibble)
library(ggplot2)
library(DT)
library(shiny)
library(shinydashboard)

subjects_enrolled<-c(2,2,1,1,2,2,1,2,2,2,3,
                     2,0,0,0,0,0,0,1,3,7,2)

data<-as.data.frame(matrix(subjects_enrolled,11,2,byrow=FALSE))
colnames(data)<-c("ALS","Control")
data$Dates = seq(as.Date("07-01-2021",  format = "%m-%d-%Y"), length.out = 11, by = "month")

data_T = as.data.frame(t(data))
colnames(data_T)<-seq(1,11,by=1)
pred_data<- data[,1:2]

nsubjects_ALS<- function(x) print(sum(data[c(1:x),1],na.rm=T))
nsubjects_control<- function(x) print(sum(data[c(1:x),2],na.rm=T))


total_visit_ALS<-function(x)
{
  if(x>length(data_T)) set<-append(as.numeric(data_T[1,1:length(data_T)]),replicate(x-length(data_T),0)) else set<-as.numeric(data_T[1,1:x])
  c<-seq(x-1,0,by=-1)
  d<-ifelse(c>=4|c==0,as.integer(c/4)+1,1)
  #e_is_to_ensure_the_number_of_visits_doesn't_cross_5
  e<-ifelse(d>=5,5,d)
  return(sum(set*e))
}

total_visit_control<-function(x)
{
  if(x>length(data_T)) set<-append(as.numeric(data_T[2,1:length(data_T)]),replicate(x-length(data_T),0)) else set<-as.numeric(data_T[2,1:x])
  c<-seq(x-1,0,by=-1)
  d<-ifelse(c>=6|c==0,as.integer(c/6)+1,1)
  #e_is_to_ensure_the_number_of_visits_doesn't_cross_5
  e<-ifelse(d>=2,2,d)
  return(sum(set*e))
}


#####################Prediction_model
pred_data<- data[,1:2]

for (i in 1:11) pred_data$sum_ALS[i]<-nsubjects_ALS(i)
for (i in 1:11) pred_data$sum_control[i]<-nsubjects_control(i)

pred_data[12,]<-c(0,0,50,20)
pred_data[24,]<-c(0,0,110,45)
pred_data[36,]<-c(0,0,180,75)

#d$time<-seq(1,36,1)
pred_data$Dates = seq(as.Date("01-07-2021",  format = "%d-%m-%Y"), length.out = 36, by = "month")

#model<- glm(sum_ALS~time,data=d)
model_ALS<- glm(sum_ALS~Dates,data=pred_data)
model_control<- glm(sum_control~Dates,data=pred_data)
#####################


# Define UI for application that draws a histogram
ui <- fluidPage(
  
  titlePanel("TARGET ALS - Subject Enrollment"),
  
  sidebarLayout(
    sidebarPanel(
      varSelectInput("group", "Choose a group:", data[1:2]),
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Enrollment",
                 plotOutput("NOS_barplot"),
                 div(DT::dataTableOutput('table'),style="font-size: 75%; width: 50%")),
        tabPanel("Total Visits at Month:Year",
                 #varSelectInput("group", "Choose a group:", data[1:2]),
                 selectInput("month","Total no.of subjects at [YYYY-MM-DD] ",pred_data$Dates),
                 verbatimTextOutput("subjects"),
                 htmlOutput("print_visit"),
                 verbatimTextOutput("samples")),
        tabPanel("Prediction",
                 #varSelectInput("group", "Choose a group:", data[1:2]),
                 selectInput("month_pred","Predicted number of subjects at [YYYY-MM-DD]",pred_data$Dates,multiple = TRUE),
                 uiOutput("prediction"),
                 dataTableOutput("model"))
      )
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$table<- DT::renderDataTable(
    datatable(data,options=list(searching=TRUE))
  )
  
  output$NOS_barplot <- renderPlot({
    data %>%
      ggplot(aes(x=Dates,y=!!input$group,color=!!input$group,fill=!!input$group))+
      geom_bar(stat="identity")+
      scale_x_date(date_labels = "%b-%Y", date_breaks = "1 month")+
      theme(axis.text.x = element_text(angle=90,vjust=0.5),
            axis.title.y = element_text(vjust = 1,size=10),
            plot.title = element_text(hjust=0.5,color="darkred",size = 10,face = "bold"),
            legend.title = element_text(size=8),
            legend.text = element_text(size=8))+
      labs(title=paste("No. of subject enrolled for the first year"), x="",y="No.of subjects(n)")
  })
  
  output$subjects <- renderPrint({
    if(!!input$group == "ALS"){
      print(paste("The total number of ALS subjects at",input$month, "is:"))
      nsubjects_ALS(which(pred_data$Dates==input$month))
      print(paste("The total number of visits by ALS at", input$month, "is:"))
      print(total_visit_ALS(which(pred_data$Dates==input$month)))
    }else 
    {
      print(paste("The total number of Controls at",input$month, "is:"))
      nsubjects_control(which(pred_data$Dates==input$month))
      print(paste("The total number of visits by controls at", input$month, "is:"))
      total_visit_control(which(pred_data$Dates==input$month))
    }
  })
  
  output$print_visit <- renderText({
    paste('<B>Total Samples: </B> <br>')
  }
  )
  
  output$samples <- renderPrint({
    if(!!input$group == "ALS")
    {
      x=total_visit_ALS(which(pred_data$Dates==input$month))
      csf=32*x; serum= 16*x; plasma=22*x; urine=11*x; total_sample=csf+serum+plasma+urine;
      print("The ideal total sample is:")
      print(total_sample)
      print("Let's say only 90% of the visits collect CSF:")
      csf=32*floor(0.9*x);
      total_sample=csf+serum+plasma+urine;
      print(total_sample)
    }else
    {
      x= total_visit_control(which(pred_data$Dates==input$month))
      csf=32*x; serum= 16*x; plasma=22*x; urine=11*x; total_sample=csf+serum+plasma+urine;
      print("The ideal total sample is:")
      print(total_sample)
      print("Let's say only 90% of the visits collect CSF:")
      csf=32*floor(0.9*x);
      total_sample=csf+serum+plasma+urine;
      print(total_sample)
    }
  }
  )
  
  
  output$prediction <- renderUI({
    if(!!input$group == "ALS"){
      gg<- pred_data %>%
        ggplot(aes(x=Dates,y=sum_ALS))+
        geom_point(aes(x=Dates,y=sum_ALS))+
        stat_smooth(method="gam", color="darkred")+
        scale_x_date(date_labels = "%b-%Y", date_breaks = "2 month")+
        theme(axis.text.x = element_text(angle=90,vjust=0.5),
              axis.title.y = element_text(vjust = 1,size=10),
              plot.title = element_text(hjust=0.5,color="darkred",size = 10,face = "bold"),
              legend.title = element_text(size=8),
              legend.text = element_text(size=8))+
        labs(title="Predicted subject enrollment from 2022-2023", x="",y="No. of subjects(n)")
      renderPlot(gg)
    }else
    {
      gg<-pred_data %>%
        ggplot(aes(x=Dates,y=sum_control))+
        geom_point(aes(x=Dates,y=sum_control))+
        stat_smooth(method="gam", color="darkred")+
        scale_x_date(date_labels = "%b-%Y", date_breaks="2 month")+
        theme(axis.text.x = element_text(angle=90,vjust=0.5),
              axis.title.y = element_text(vjust = 1,size=10),
              plot.title = element_text(hjust=0.5,color="darkred",size = 10,face = "bold"),
              legend.title = element_text(size=8),
              legend.text = element_text(size=8))+
        labs(title="Predicted subject enrollment from 2022-2023", x="",y="No. of subjects(n)")
      renderPlot(gg)
    }
  })
  
  output$model <- renderDataTable({
    newdata<- data.frame(Dates=as.Date(input$month_pred))
    
    ans<-data.frame(predicted_ALS=predict(model_ALS,newdata))
    
    c_ans<-cbind(newdata,ans)
    
    ans_c<-data.frame(predicted_control=predict(model_control,newdata))
    d_ans<-cbind(c_ans,ans_c)
    d_ans$predicted_ALS<-as.integer(d_ans$predicted_ALS)
    d_ans$predicted_control<-as.integer(d_ans$predicted_control)
    print(d_ans)
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
