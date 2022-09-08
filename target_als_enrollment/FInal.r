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
library(ggiraph)

real_data_both <-c(2,2,1,1,2,2,1,2,2,2,3,
                     2,0,0,0,0,0,0,1,3,7,2)

real_data<-as.data.frame(matrix(real_data_both,11,2,byrow=FALSE))
colnames(real_data)<-c("ALS","Control")
real_data$Dates = seq(as.Date("07-01-2021",  format = "%m-%d-%Y"), length.out = 11, by = "month")

real_data_T = as.data.frame(t(real_data))
colnames(real_data_T)<-seq(1,11,by=1)




ui <- fluidPage(
  
  titlePanel("TARGET ALS - Subject Enrollment"),
  
  sidebarLayout(
    sidebarPanel(
      varSelectInput("group", "Choose a group:", real_data[1:2]),
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Real data - Enrolled subjects",
                uiOutput("NOS_barplot"),
                div(DT::dataTableOutput('table'),style="font-size: 75%; width: 50%")
        ),
        tabPanel("Predicted data",
                DT::dataTableOutput("print"))
      )
    )
  )
)

server <- function(input,output){

    data <- reactive({
        real_data %>% dplyr::select(type=!!input$group,Dates)
    })
    
    data_nos <- reactive({
        real_data %>% dplyr::select(type=!!input$group)
    })

    n_subjects <- function(x) print(sum(data()[c(1:x),1],na.rm = T))

    output$print<- DT::renderDataTable({
        pred_data <- data_nos()
        for(i in 1:11) pred_data$sum[i]<-n_subjects(i)
        if(!!input$group=="ALS"){
            pred_data[12,]<-c(0,50)
            pred_data[24,]<-c(0,110)
            pred_data[36,]<-c(0,180)
        }else {
            pred_data[12,]<-c(0,20)
            pred_data[24,]<-c(0,45)
            pred_data[36,]<-c(0,75)
        }
        pred_data$Dates = seq(as.Date("01-07-2021",  format = "%d-%m-%Y"), length.out = 36, by = "month")
        model <- glm(sum~Dates,data=pred_data)
        newdata<- data.frame(Dates=seq(as.Date("01-07-2021",  format = "%d-%m-%Y"), length.out = 36, by = "month"))
        ans<-data.frame(predicted=predict(model,newdata))
        c_ans<-cbind(newdata,ans)
        #datatable(ans)
        stat_table<- c_ans %>%
            mutate(nos_month = floor(predicted - lag(predicted, default=first(predicted))))
        data_T<-stat_table %>%
            dplyr::select(type=nos_month) %>%
            rownames_to_column() %>%
            pivot_longer(, cols = -rowname) %>%
            pivot_wider(, names_from = rowname) %>%
            as.data.frame()

        rownames(data_T)<-data_T$name
        data_T<-data_T[,-1]

        final<-merge(stat_table,pred_data,by="Dates")
        datatable(final)
    })

    output$table<- DT::renderDataTable(
        datatable(data(),options=list(searching=TRUE))
  )

    output$NOS_barplot <- renderUI({
    gg<- data() %>%
        ggplot(aes(x=Dates,y=type,color=type,fill=type))+
        geom_bar(stat="identity")+
        scale_x_date(date_labels = "%b-%Y", date_breaks = "1 month")+
        theme(axis.text.x = element_text(angle=90,vjust=0.5),
                axis.title.y = element_text(vjust = 1,size=10),
                plot.title = element_text(hjust=0.5,color="darkred",size = 10,face = "bold"),
                legend.title = element_text(size=8),
                legend.text = element_text(size=8))+
        labs(title=paste("No. of subject enrolled for the first year"), x="",y="No.of subjects(n)")

    renderPlot(gg)
  })

}

shinyApp(ui=ui, server = server)



