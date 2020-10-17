### Data Cleaning and Modelling for predictions ####

library(readxl)

#library(h2o)
library(shinydashboard)
library(caret)
library(dplyr)

#setwd("/home/ren200")



data <- readxl::read_excel("Program-Data-10_06_2020_1.xlsx", col_names = T)

#h2o.init(nthreads=1, max_mem_size="4g")



#data<-read_excel("/home/cao373/Final Project/Program-Data-10_06_2020_1.xlsx")



nzv <- nearZeroVar(data, saveMetrics = TRUE)

data <- data[, c(TRUE,!nzv$zeroVar[2:ncol(data)])]



data1<-data%>%
    
    filter(!is.na(AvgStartingSalary))%>%
    
    select(c(R,Python,SAS,DatabaseSQL,DataMiningDescriptiveAnalytics,PredictiveAnalytics,
             
             MachineLearning,DeepLearning,PrescriptiveAnalyticsOptimization,Simulation,
             
             DataVisualizationTableau,BigDataHadoopSparkHive,CommunicationPublicspeaking,
             
             TextAnalyticsNLP,WebminingWebCrawlingDataScraping,CloudAWSGCP,LinuxUbuntu,
             
             DataEthics,AvgStartingSalary))

AvgStartingSalary<-select(data1,AvgStartingSalary)

data1<-data1%>%
    
    select(-AvgStartingSalary)%>%
    
    mutate_all(~replace(., is.na(.), 0))%>%
    
    mutate_all(as.factor)%>%
    
    mutate_all(~recode(.,"0" = "N","1"="Y"))

#summary(data2)

data1<-cbind(data1,AvgStartingSalary)





#Model for AVG Salary

#y <- "AvgStartingSalary"                                # target variable to learn

#x <- setdiff(names(data1), y)                # feature variables are all other columns

#data_avg<-as.h2o(data1)

#aml_avg <- h2o.automl(x, y, data_avg
#                      
#                      , max_runtime_secs = 180     # max time to run in seconds
#                      
#                      , max_models = 10            # max num of models
#                      
#                      , seed = 123                # for reproducibility.
#                      
#)

#pred <- h2o.predict(aml_avg@leader, test)


library(caret)
model<-glm(AvgStartingSalary~R+Python+SAS+DatabaseSQL+DataMiningDescriptiveAnalytics+PredictiveAnalytics+
           
           MachineLearning+DeepLearning+
           
           DataVisualizationTableau+CommunicationPublicspeaking+
           
           TextAnalyticsNLP+WebminingWebCrawlingDataScraping+CloudAWSGCP+
           
           DataEthics,data = data1)







### Data cleaning for maps####

library(shiny)



library(dplyr)





library(choroplethrMaps)



library(ggplot2)



library(shinythemes)





library(readxl)

#install.packages("caret")

library(caret)



#install.packages('data.table')

library(data.table)



library(sqldf)



library(h2o)



library(e1071)



library(markdown)



library(ggthemes)



library(ggmap)



library(maps)

#install.packages('mapdata')

library(mapdata)



library(wesanderson)



library(grid)

library(RColorBrewer)





program_data <- readxl::read_excel("Program-Data-10_06_2020_1.xlsx", col_names = T)



data1<-program_data%>%
    
    group_by(State)%>%
    
    summarise(avg_salary=mean(AvgStartingSalary,na.rm = TRUE))%>%
    
    select(State,avg_salary)%>%
    
    distinct()%>%
    
    ungroup()%>%
    
    mutate(State=tolower(State))



data2<-program_data%>%
    
    group_by(State)%>%
    
    summarise(num_state=n())%>%
    
    select(State,num_state)%>%
    
    distinct()%>%
    
    ungroup()%>%
    
    mutate(State=tolower(State))



data3<-program_data%>%
    
    group_by(State)%>%
    
    summarise(num_palcement=sum(PlacementRolesListed))%>%
    
    select(State,num_palcement)%>%
    
    distinct()%>%
    
    ungroup()%>%
    
    mutate(State=tolower(State))





us_map <- map_data("state")

colnames(us_map)[5] <- "State"



data3<-full_join(data3,us_map,by="State")





data2<-full_join(data2,us_map,by="State")



data1<-full_join(data1,us_map,by="State")









###Data Cleaning for graphs####

data_graph<-read_xlsx("Program-Data-10_06_2020_1.xlsx")



data_graph1<-data_graph%>%
    
    select(AvgDuration,CreditHrs,AvgCost,NumRecLetters,StudentAge)%>%
    
    mutate_all(as.numeric)

data_graph2<-data_graph%>%
    
    select(State,STEMCertified,Interview,Online,Hybrid)%>%
    
    mutate_all(as.factor)%>%
    
    mutate_all(~recode(.,"0"="No","1"="Yes"))



d<-cbind(data_graph1,data_graph2)



####Start of Your Shiny

header <- dashboardHeader(title = "Value for Analytics Program")



#Sidebar content of the dashboard

sidebar <- dashboardSidebar(sidebarMenu(
    
    menuItem("Geographical Distribution", tabName = "maps", icon = icon("calendar")),
    menuItem(
        
        "Data Analysis",    tabName = "EDA",    icon = icon("calendar")
        
    ),
    menuItem("Predict Base on Curriculum", tabName = "predict", icon = icon("book"))
    
))



tbs <- tabItems(
    
    ######################################################
    
    #### All Graphs #####
    
    tabItem(
        
        tabName = "EDA",
        
        fluidRow(
            
            box(
                
                title = "Average Duration"        ,
                
                status = "primary"        ,
                
                solidHeader = TRUE        ,
                
                collapsible = TRUE        ,
                
                plotOutput("AverageDuration", height = "300px")
                
            )
            
            
            
            ,
            
            box(
                
                title = "Credit Hours"        ,
                
                status = "primary"        ,
                
                solidHeader = TRUE        ,
                
                collapsible = TRUE        ,
                
                plotOutput("CreditHours", height = "300px")
                
            )
            
        ) #End of Fluid Row 1
        
        ,
        
        fluidRow(
            
            box(
                
                title = "Average Cost"        ,
                
                status = "primary"        ,
                
                solidHeader = TRUE        ,
                
                collapsible = TRUE        ,
                
                plotOutput("AverageCost", height = "300px")
                
            )
            
            ,
            
            box(
                
                title = "Stem Certified"        ,
                
                status = "primary"        ,
                
                solidHeader = TRUE        ,
                
                collapsible = TRUE        ,
                
                plotOutput("StemCertified", height = "300px")
                
            )
            
            
            
        ) # End of fluid row 2
        
        ,
        
        fluidRow(
            
            box(
                
                title = "Interview"        ,
                
                status = "primary"        ,
                
                solidHeader = TRUE        ,
                
                collapsible = TRUE        ,
                
                plotOutput("Interview", height = "300px")
                
            ),
            
            box(
                
                title = "Num of Letters"        ,
                
                status = "primary"        ,
                
                solidHeader = TRUE        ,
                
                collapsible = TRUE        ,
                
                plotOutput("NumRecLetters", height = "300px")
                
            )
            
        ),
        
        # End of fluid Row 3
        
        
        
        fluidRow(
            
            box(
                
                title = "Student Age Distribution"        ,
                
                status = "primary"        ,
                
                solidHeader = TRUE        ,
                
                collapsible = TRUE        ,
                
                plotOutput("StudentAge", height = "300px")
                
            )
            
            
            
            ,
            
            box(
                
                title = "Hybrid and Online Class"        ,
                
                status = "primary"        ,
                
                solidHeader = TRUE        ,
                
                collapsible = TRUE        ,
                
                plotOutput("hybridoronline", height = "300px")
                
            ))),
    
    # End of fluid Row 4
    
    
    
    #######################################################
    
    ####Predict####
    
    tabItem(tabName = "predict",
            
            fluidRow(
                
                
                
                column(3,
                       
                       
                       
                       h4("Predicted Average Salary After Graduate from Graduate School"),
                       
                       
                       
                       radioButtons(inputId='R', label='R', c('N','Y'), selected = NULL, inline = FALSE,width = NULL),
                       
                       
                       
                       br(),
                       
                       
                       
                       radioButtons(inputId='Python', label='Python', c('N','Y'), selected = NULL, inline = FALSE,width = NULL),
                       
                       
                       
                       br(),
                       
                       
                       
                       radioButtons(inputId='SAS', label='SAS', c('N','Y'), selected = NULL, inline = FALSE,width = NULL),
                       
                       
                       
                       br(),
                       
                       
                       
                       radioButtons(inputId='DatabaseSQL', label='Database(eg.SQL)', c('N','Y'), selected = NULL, inline = FALSE,width = NULL),
                       
                       
                       
                       br(),
                       
                       
                       
                       radioButtons(inputId='DataMiningDescriptiveAnalytics', label='Data Minning and Descriptive Analytics', c('N','Y'), selected = NULL, inline = FALSE,width = NULL),
                       
                       br(),
                       
                       
                       
                       radioButtons(inputId='PredictiveAnalytics', label='Predictive Analytics', c('N','Y'), selected = NULL, inline = FALSE,width = NULL)
                       
                       
                       
                       
                       
                       
                       
                ),
                
                
                
                column(4, offset = 1,
                       
                       
                       
                       radioButtons(inputId='MachineLearning', label='Machine Learning', c('N','Y'), selected = NULL, inline = FALSE,width = NULL),
                       
                       
                       
                       br(),
                       
                       
                       
                       radioButtons(inputId='DeepLearning', label='Deep Learning', c('N','Y'), selected = NULL, inline = FALSE,width = NULL),
                       
                       
                       
                       br(),
                       
                       
                       
                       radioButtons(inputId='PrescriptiveAnalyticsOptimization', label='Prescriptive Analytics Optimization', c('N','Y'), selected = NULL, inline = FALSE,width = NULL),
                       
                       
                       
                       br(),
                       
                       
                       
                       radioButtons(inputId='Simulation', label='Simulation', c('N','Y'), selected = NULL, inline = FALSE,width = NULL),
                       
                       
                       
                       br(),
                       
                       
                       
                       radioButtons(inputId='DataVisualizationTableau', label='Data Visualization', c('N','Y'), selected = NULL, inline = FALSE,width = NULL),
                       
                       br(),
                       
                       
                       
                       radioButtons(inputId='BigDataHadoopSparkHive', label='Bigdat Hadoop Spark Hive', c('N','Y'), selected = NULL, inline = FALSE,width = NULL)
                       
                       
                       
                ),
                
                
                
                column(4,
                       
                       
                       
                       radioButtons(inputId='CommunicationPublicspeaking', label='Communication(eg. Public Speaking)', c('N','Y'), selected = NULL, inline = FALSE,width = NULL),
                       
                       
                       
                       br(),
                       
                       
                       
                       radioButtons(inputId='TextAnalyticsNLP', label='Text Analytics NLP', c('N','Y'), selected = NULL, inline = FALSE,width = NULL),
                       
                       
                       
                       br(),
                       
                       
                       
                       radioButtons(inputId='WebminingWebCrawlingDataScraping', label='Webmining Web Crawling Data Scraping', c('N','Y'), selected = NULL, inline = FALSE,width = NULL),
                       
                       
                       
                       br(),
                       
                       
                       
                       radioButtons(inputId='CloudAWSGCP', label='Cloud AWS GCP', c('N','Y'), selected = NULL, inline = FALSE,width = NULL),
                       
                       
                       
                       br(),
                       
                       
                       
                       radioButtons(inputId='LinuxUbuntu', label='Linux Ubuntu', c('N','Y'), selected = NULL, inline = FALSE,width = NULL),
                       
                       br(),
                       
                       
                       
                       radioButtons(inputId='DataEthics', label='Data Ethics', c('N','Y'), selected = NULL, inline = FALSE,width = NULL)
                       
                       
                       
                       
                       
                ),
                
                
                
                actionButton("plot", "Forcast!"),
                
                dataTableOutput("Pred")
                
            )),
    
    #### Maps####
    
    tabItem(tabName = "maps",
            
            fluidRow(                           
                
                box(
                    
                    title = "Salary Distribution"        ,
                    
                    status = "primary"        ,
                    
                    solidHeader = TRUE        ,
                    
                    collapsible = TRUE        ,
                    
                    plotOutput("salarymap")
                    
                ),
                
                box(
                    
                    title = "Number of Program Distribution"        ,
                    
                    status = "primary"        ,
                    
                    solidHeader = TRUE        ,
                    
                    collapsible = TRUE        ,
                    
                    plotOutput("state_num")
                    
                ),
                
                box(
                    
                    title = "Number of Placement Distribution"        ,
                    
                    status = "primary"        ,
                    
                    solidHeader = TRUE        ,
                    
                    collapsible = TRUE        ,
                    
                    plotOutput("placement")
                    
                )
                
                
                
                #END OF FLUID ROW 1
                
            )))



body <- dashboardBody(tbs,
                      
                      tags$head(
                          
                          tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
                          
                      ))



#completing the ui part with dashboardPage

ui <-
    
    dashboardPage(title = 'This is my Page title', header, sidebar, body, skin =
                      
                      'green')



####Start the Server



####Ui####

body <- dashboardBody(tbs,
                      
                      tags$head(
                          
                          tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
                          
                      ))



#completing the ui part with dashboardPage

ui <-
    
    dashboardPage(title = 'This is my Page title', header, sidebar, body, skin =
                      
                      'blue')



####Server####

server <- function(input, output) {
    
    
    
    data <- eventReactive(input$plot,{

        data233<-data.frame(R=input$R,
                                   
                                   
                                   
                                   Python=input$Python,
                                   
                                   
                                   
                                   SAS=input$SAS,
                                   
                                   
                                   
                                   DatabaseSQL=input$DatabaseSQL,
                                   
                                   
                                   
                                   DataMiningDescriptiveAnalytics=input$DataMiningDescriptiveAnalytics,
                                   
                                   
                                   
                                   PredictiveAnalytics=input$PredictiveAnalytics,
                                   
                                   
                                   
                                   MachineLearning=input$MachineLearning,
                                   
                                   
                                   
                                   DeepLearning=input$DeepLearning,
                                   
                                   
                                   
                                   PrescriptiveAnalyticsOptimization=input$PrescriptiveAnalyticsOptimization,
                                   
                                   
                                   
                                   Simulation=input$Simulation,
                                   
                                   
                                   
                                   DataVisualizationTableau=input$DataVisualizationTableau,
                                   
                                   
                                   
                                   BigDataHadoopSparkHive=input$BigDataHadoopSparkHive,
                                   
                                   
                                   
                                   CommunicationPublicspeaking=input$CommunicationPublicspeaking,
                                   
                                   
                                   
                                   TextAnalyticsNLP=input$TextAnalyticsNLP,
                                   
                                   
                                   
                                   WebminingWebCrawlingDataScraping=input$WebminingWebCrawlingDataScraping,
                                   
                                   
                                   
                                   CloudAWSGCP=input$CloudAWSGCP,

                                   DataEthics=input$DataEthics)
        
        
        
        # weight=input$weight,
        
        
        
        # cholesterol=input$cholesterol)
        
        
        
    })
    
    pred <- eventReactive(input$plot,{
        
        
        
        as.data.table(c("Predicted Average Salary",predict(model, data())))
       
        #as.data.table(c("Predicted Average Salary", predict(model,data1[1,])))
        
        #h2o.shutdown()
        
        
        
    })
    
    output$Pred <- renderDataTable(pred())
    
    output$salarymap<-renderPlot({
        
        p1<-ggplot(data1, aes(x = long, y = lat, group = group,fill=avg_salary)) +
            
            geom_polygon( colour = "white")+
            
            labs(title="Average Salary for graduates of Analytics Program",
                 
                 fill="Average Salary",x="",y="")+
            
            theme_void()
        
        hm1.palette <- colorRampPalette(rev(brewer.pal(9, 'GnBu')), space='Lab')
        
        p1 + scale_fill_gradientn(colours = hm1.palette(100))
        
    })
    
    output$state_num<-renderPlot({
        
        p2<-ggplot(data2, aes(x = long, y = lat, group = group,fill=num_state)) +
            
            geom_polygon( colour = "white")+
            
            labs(title="Number of Programs By State",
                 
                 fill="Number of Programs",x="",y="")+
            
            theme_void()
        
        hm2.palette <- colorRampPalette(rev(brewer.pal(9, 'GnBu')), space='Lab')
        
        p2 + scale_fill_gradientn(colours = hm2.palette(100))
        
    })
    
    output$placement<-renderPlot({
        
        p3<-ggplot(data3, aes(x = long, y = lat, group = group,fill=num_palcement)) +
            
            geom_polygon( colour = "white")+
            
            labs(title="Number of People Who Find a Placement Role By State",
                 
                 fill="Number of people \nwho find a Placement Role",x="",y="")+
            
            theme_void()
        
        hm3.palette <- colorRampPalette(rev(brewer.pal(9, 'GnBu')), space='Lab')
        
        p3 + scale_fill_gradientn(colours = hm3.palette(100))
        
    })
    
    output$AverageDuration <- renderPlot({
        
        ggplot(d, aes(x=AvgDuration)) +
            
            geom_bar(fill="#9ecae1") + coord_flip()  +
            
            scale_fill_manual(values = c("#00bcd4", "#8bc34a"))+
            
            labs(title = "Distribution of Average Duration of the Program")+
            
            theme_bw()
        
    })
    
    
    
    output$CreditHours <- renderPlot({
        
        hist(d$CreditHrs,xlab="Credit Hours",main="Distribution of Credit Hours",ylab="",col="#9ecae1")
        
    })
    
    
    
    output$AverageCost <- renderPlot({
        
        hist(d$AvgCost,xlab="Average Cost",main="Distribution of Average Cost",ylab="",col="#9ecae1")
        
        
        
    })
    
    
    
    output$StemCertified <- renderPlot({
        
        plot(d$STEMCertified,xlab="Average Cost",main="Distribution of STEM or NON-STEM Program",
             
             ylab="",col="#9ecae1")
        
    })
    
    
    
    output$Interview <- renderPlot({
        
        plot(d$Interview,xlab="Interview Required Or Not",
             
             main="Distribution of Interview Required Program \nor No Interview Required",
             
             ylab="",col="#9ecae1")
        
    })
    
    
    
    
    
    output$NumRecLetters <- renderPlot({
        
        hist(d$NumRecLetters,xlab="Number of Recommendation Letters",main="Distribution of Number of

         Recommendation Letter submitted",ylab="",col="#9ecae1")
        
    })
    
    
    
    output$StudentAge<-renderPlot({
        
        hist(d$StudentAge,xlab="Average Student Age",main="Distribution of Student Age",ylab="",
             
             col="#9ecae1")
        
    })
    
    
    
    output$hybridoronline<-renderPlot({
        
        par(mfrow=c(1,2))
        
        plot(d$Hybrid,xlab="Hybrid Class or Not",main="Number of Hybrid Programs",ylab="",col="#9ecae1")
        
        plot(d$Online,xlab="Online Class or Not",main="Number of Online Programs",ylab="",col="#9ecae1")
        
        par(mfrow=c(1,1))
        
    })
    
}

shinyApp(ui, server)
