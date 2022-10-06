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
real_data$total<-real_data$ALS+real_data$Control
rd<-pivot_longer(real_data,cols=c(1,2,4),names_to = "Group",values_to = "nos")

real_data_T = as.data.frame(t(real_data))
colnames(real_data_T)<-seq(1,11,by=1)

Dates=seq(as.Date("01-07-2021",  format = "%d-%m-%Y"), length.out = 36, by = "month")

ui <- fluidPage(
  
  titlePanel("TARGET ALS - Subject Enrollment"),
  
  sidebarLayout(
    sidebarPanel(
      varSelectInput("group", "Choose a group:", real_data[1:2]),
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Real data - Enrolled subjects",
                br(),
                strong("TARGET ALS- Subject Enrollment"),
                h6("The plot shows the distribution of the current subject enrollment"),
                em("Please change Group on the left tab to see distribution for Controls"),
                uiOutput("NOS_barplot"),
                #div(DT::dataTableOutput('table'),style="font-size: 75%; width: 50%"),
                strong("Distribution across each group - ALS, Control, Total"),
                uiOutput("NOS_barplot2")
        ),
        tabPanel("Predicted graph - Visits",
                h6("The prediction curve is based on the current data and minimum annual enrollment"),
                strong("Assumptions for the prediction:"),
                h6("1.Minimum annual enrollment for consecutive years to be 50,110,180 for ALS"),
                h6("2.Minimum annual enrollment for consecutive years to be 20,45,75 for Controls"),
                strong("Subjects enrolled and total number of visits"),
                ggiraphOutput("Predicted_samples3"),
                strong("Visit detials:"),
                h6("1. ALS subjects visits every 4 months scheduled for 5 visits"),
                h6("2. Controls visits every 6 months scheduled for 2 visits"),
                br(),
                em("Hover around the green dots in the plot to find the number of subjects that needs to be enrolled for every visit at specific month/year you select"),
                ggiraphOutput("Predicted_samples1"),
                em("For a table display of visits, please use"), strong("Predicted data"), em("tab")),
        tabPanel("Predicted graph - Samples",
                strong("Sample information:"),
                h6("For every visit, subjects provide different samples- urine, serum, plasma and CSF"),
                h6("Assuming we collect CSF in ALS atleast for 3 visits and other samples collected at every visit,we have csf_min and csf_max that gives us a range of expected sample"),
                br(),
                em("Hover around the green dots in the plot to find the predicted sample amount to be collected for every visit at specific month/year you select"),
                ggiraphOutput("Predicted_samples2"),
                em("For a table display of samples, please use"), strong("Predicted data"), em("tab")),
        tabPanel("Predicted data",
                 selectInput("month","Predicted number of subjects at [YYYY-MM-DD]",Dates,selected = "2021-08-01"),
                 strong("Visits:"),
                 br(),
                 div(DT::dataTableOutput('print1'),style="font-size: 75%; width: 50%"),
                 strong("Samples:"),
                 br(),
                 div(DT::dataTableOutput('print2'),style="font-size: 75%; width: 50%"),
                 strong("DNA and RNA samples:"),
                 br(),
                 div(DT::dataTableOutput('print3'),style="font-size: 75%; width: 50%"))
      )
    )
  )
)

server <- function(input,output){

    visits_sample = reactiveVal()

    data <- reactive({
        real_data %>% dplyr::select(type=!!input$group,Dates)
    })
    
    #remove the dates, you can add them later on after inserting the minimum annual enrollment
    data_nos <- reactive({
        real_data %>% dplyr::select(type=!!input$group)
    })

    n_subjects <- function(x) print(sum(data()[c(1:x),1],na.rm = T))


    final_data = reactive({
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
        
        total_visit_ALS<-function(x)
        {
        if(x>length(data_T)) set<-append(as.numeric(data_T[1,1:length(data_T)]),replicate(x-length(data_T),0)) else set<-as.numeric(data_T[1,1:x])
        c<-seq(x-1,0,by=-1)
        if(!!input$group=="ALS"){
            d<-ifelse(c>=4|c==0,as.integer(c/4)+1,1)
            e<-ifelse(d>=5,5,d)
        }else{
            d<-ifelse(c>=6|c==0,as.integer(c/6)+1,1)
            e<-ifelse(d>=2,2,d)
        }
        total_vists=sum(set*e)
        
        df=data.frame(visits=e,nos=set)
        
        f<- df%>%
            group_by(visits)%>%
            summarise(totalnos=sum(nos))%>%
            mutate(plasma=22*visits*totalnos,serum=16*visits*totalnos,urine=11*visits*totalnos,
                csf_min=ifelse(visits<=2,0,visits-2), csf_max=ifelse(visits<=3,visits,3),
                csf_min_sample=32*totalnos*csf_min, csf_max_sample=32*totalnos*csf_max,
                rna=2*totalnos, dna= totalnos, total_dna_rna=3*totalnos)
        
        result<- data.frame(Plasma=sum(f$plasma),Serum=sum(f$serum),Urine=sum(f$urine),
                            CSF_min=sum(f$csf_min_sample),CSF_max=sum(f$csf_max_sample),
                            RNA=sum(f$rna),DNA=sum(f$dna),DNA_RNA=sum(f$total_dna_rna))
        
        result_sub <- data.frame(total_subjects=sum(final[c(1:x),3],na.rm = T), total_visits=total_vists)
        
        return(list(f %>% select(visits,subjects=totalnos), result, result_sub))
        }

        visits_sample = total_visit_ALS(which(final$Dates==input$month))[[1]]

        for (i in 1:36) final$output1[i]<-total_visit_ALS(i)[1]
        for (i in 1:36) final$output2[i]<- total_visit_ALS(i)[2]
        for (i in 1:36) final$output3[i]<- total_visit_ALS(i)[3]
        
        for (i in 1:36) final$tooltip1[i] <- knitr::kable(final$output1[i], format = "html")
        for (i in 1:36) final$tooltip2[i] <- knitr::kable(final$output2[i], format = "html")
        for (i in 1:36) final$tooltip3[i]<- knitr::kable(final$output3[i], format = "html")
        
        final
    })

    
    output$print1<- renderDataTable({
        t1<-final_data() %>% 
            filter(Dates==input$month) %>%
            dplyr::select(output1) %>%
            as.data.frame()
        datatable(t1[[1]][[1]],
                  options = list(searching = FALSE, pageLength = 15, lengthChange = FALSE),
                  rownames= FALSE)
    })
    
    output$print2<- renderDataTable({
        t2<-final_data() %>% 
            filter(Dates==input$month) %>%
            dplyr::select(output2) %>%
            as.data.frame()
            
        
        t2<- t2[[1]][[1]] %>%
          mutate(total_samples=paste(Plasma+Serum+Urine+CSF_min,"-",Plasma+Serum+Urine+CSF_max))
        datatable(t2[c(1:5,9)],
                  options = list(searching = FALSE, pageLength = 15, lengthChange = FALSE),
                  rownames= FALSE)
    })

    output$print3<- renderDataTable({
      t3<-final_data() %>%
        filter(Dates==input$month) %>%
        dplyr::select(output2) %>%
        as.data.frame()
      datatable(t3[[1]][[1]][6:8],
                options = list(searching = FALSE, pageLength = 15, lengthChange = FALSE),
                rownames= FALSE)
    })
    
    output$Predicted_samples1 <- renderggiraph({
    
      gg<-final_data() %>% 
        ggplot(aes(x=Dates,y=predicted,tooltip=tooltip1))+
        geom_point_interactive(aes(color="Predicted"))+
        geom_smooth_interactive(method="gam",color="#69b3a2",tooltip="")+
        scale_x_date(date_labels = "%b-%Y", date_breaks = "2 month")+
        geom_point(aes(x=Dates,y=sum,color="Actual"))+
        geom_smooth(aes(x=Dates,y=sum),method="gam")+
        theme(axis.text.x = element_text(angle=90,vjust=0.5),
              axis.title.y = element_text(vjust = 1,size=10),
              plot.title = element_text(hjust=0.5,color="darkred",size = 10,face = "bold"),
              legend.title = element_text(size=0),
              legend.text = element_text(size=6))+
       labs(title="Predicted subject enrollment(visit details) from June 2021- June 2024", x="",y="No. of subjects(n)")
        
        girafe(ggobj = gg,
              options= list(
                opts_tooltip(
                  opacity = 0.8, use_fill = TRUE,
                  use_stroke = FALSE, 
                  css = "padding:5pt;font-family: Open Sans;color:white"),
                opts_hover_inv(css = "opacity:0.5"), 
                opts_hover(css = "fill:#4c6061;")
              ))

    })

    output$Predicted_samples2 <- renderggiraph({
    
      gg<-final_data() %>% 
        ggplot(aes(x=Dates,y=predicted,tooltip=tooltip2))+
        geom_point_interactive(aes(color="Predicted"))+
        geom_smooth_interactive(method="gam",color="#69b3a2",tooltip="")+
        scale_x_date(date_labels = "%b-%Y", date_breaks = "2 month")+
        geom_point(aes(x=Dates,y=sum,color="Actual"))+
        geom_smooth(aes(x=Dates,y=sum),method="gam")+
        theme(axis.text.x = element_text(angle=90,vjust=0.5),
              axis.title.y = element_text(vjust = 1,size=10),
              plot.title = element_text(hjust=0.5,color="darkred",size = 10,face = "bold"),
              legend.title = element_text(size=0),
              legend.text = element_text(size=6))+
        labs(title="Predicted subject enrollment(sample details) from June 2021- June 2024", x="",y="No. of subjects(n)")
    
      girafe(ggobj = gg,
            options= list(
              opts_tooltip(
                opacity = 0.8, use_fill = TRUE,
                use_stroke = FALSE, 
                css = "padding:5pt;font-family: Open Sans;color:white"),
              opts_hover_inv(css = "opacity:0.5"), 
              opts_hover(css = "fill:#4c6061;")
            ))

    })

    output$Predicted_samples3 <- renderggiraph({
      
      gg<-final_data() %>% 
        ggplot(aes(x=Dates,y=predicted,tooltip=tooltip3))+
        geom_point_interactive(aes(color="Predicted"))+
        geom_smooth_interactive(method="gam",color="#69b3a2",tooltip="")+
        scale_x_date(date_labels = "%b-%Y", date_breaks = "2 month")+
        geom_point(aes(x=Dates,y=sum,color="Actual"))+
        geom_smooth(aes(x=Dates,y=sum),method="gam")+
        theme(axis.text.x = element_text(angle=90,vjust=0.5),
              axis.title.y = element_text(vjust = 1,size=10),
              plot.title = element_text(hjust=0.5,color="darkred",size = 10,face = "bold"),
              legend.title = element_text(size=0),
              legend.text = element_text(size=6))+
        labs(title="Predicted subject enrollment from June 2021- June 2024", x="",y="No. of subjects(n)")
      
      girafe(ggobj = gg,
             options= list(
               opts_tooltip(
                 opacity = 0.8, use_fill = TRUE,
                 use_stroke = FALSE, 
                 css = "padding:5pt;font-family: Open Sans;color:white"),
               opts_hover_inv(css = "opacity:0.5"), 
               opts_hover(css = "fill:#4c6061;")
             ))
      
    })
    
    
    output$table<- DT::renderDataTable(
        datatable(data(),colnames=c("No. of Subjects","Date"),
                  options = list(searching = FALSE, pageLength = 15, lengthChange = FALSE),
                  rownames= FALSE)
    )

    output$NOS_barplot <- renderUI({
    gg<- data() %>%
        ggplot(aes(x=Dates,y=type,color=type,fill=type))+ 
        geom_bar(stat="identity")+
        scale_x_date(date_labels = "%b-%Y", date_breaks = "1 month")+
        theme(axis.text.x = element_text(angle=90,vjust=0.5),
                axis.title.y = element_text(vjust = 1,size=10),
                plot.title = element_text(hjust=0.5,color="darkred",size = 12,face = "bold"),
                legend.title = element_text(size=10),
                legend.text = element_text(size=10))+
        labs(title=paste("No. of subject enrolled for the first year"), x="",y="No.of subjects(n)")

    renderPlot(gg)
  })

    output$NOS_barplot2 <- renderUI({
      gg<- rd %>% ggplot(aes(x=Dates, y=nos, color=Group, fill=Group))+
        geom_bar(stat="identity", position="dodge")+
        scale_x_date(date_labels = "%b-%Y", date_breaks = "1 month")+
        scale_y_continuous(breaks=seq(0,10,by=1))+
        theme(axis.text.x = element_text(angle=90,vjust=0.5),
              axis.title.y = element_text(vjust = 1,size=12),
              plot.title = element_text(hjust=0.5,color="darkred",size = 12,face = "bold"),
              legend.title = element_text(size=10),
              legend.text = element_text(size=10))+
        labs(title=paste("No. of subject enrolled for the first year"), x="",y="No.of subjects(n)")
      
      renderPlot(gg)
    })
    
}

shinyApp(ui=ui, server = server)



