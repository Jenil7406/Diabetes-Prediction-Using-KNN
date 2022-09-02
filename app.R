## Only run examples in interactive R sessions

# KNN Classifier
library(shiny)
library(class) 
library(shinythemes)


    
    ui <- fluidPage(titlePanel("KNN Classifier",),
        sidebarLayout(
            sidebarPanel(
                fileInput("file1", "Choose CSV File",
                          accept = c(
                              "text/csv",
                              "text/comma-separated-values,text/plain",
                              ".csv")
                ),
                tags$hr(),
                checkboxInput("header", "Header", TRUE),
                 
                           numericInput("k", 
                                        h4("Enter K (Nearest Neighbors)", align = "center"), 
                                        value = 1)   
                
                
            ),
            mainPanel(
                
                h1("K-Nearest Neighbors", align = "center"),
                tabsetPanel(type="tabs",
                tabPanel("Data", tableOutput("table")),
                tabPanel("Summary", verbatimTextOutput("sum")),
                tabPanel("Prediction", tableOutput("knn")),
    
                tabPanel("Confusion Matrix",
                         sidebarLayout(sidebarPanel(
                             uiOutput("var1_select_1"),
                             uiOutput("rest_var_select_1")),
                             mainPanel(
                                 
                                 h3("Confusion Matrix",align="center"),
                                 verbatimTextOutput("con_matrix")))),
                tabPanel("Contact", 
                         h2("Project Members"),br(),
                         h4("19162121022   -  Jenil Patel  :- jenilpatel19@gnu.ac.in "),br(),
                         h4("19162121020   -  Fenil Patel  :- fenilapatel19@gnu.ac.in"),br(),
                         h4("19162121017   -  Ankit Parmar :- ankitparmar19@gnu.ac.in"),br(),
                         h4("19162121001   -  Soham Bhavsar:- sohambhavsar19@gnu.ac.in"))
                ))
        )
    )

    
    server <- function(input, output) {
        
        data<-reactive({
            inFile <- input$file1
            if (is.null(inFile))
                return(NULL)
            read.csv(inFile$datapath, header = input$header)
        })
    
        diabetes.subset <- reactive(data()[c('Outcome','Pregnancies','Glucose','BloodPressure','SkinThickness','Insulin','BMI','DiabetesPedigreeFunction','Age')])
        data_norm <- reactive(function(x){((x - min(x))/(max(x) - min(x)))})
        
        diabetes.subset.n <- reactive(as.data.frame(lapply(diabetes.subset()[1:8], data_norm())))
        
        dat.d <- reactive(sample(1:nrow(diabetes.subset.n()),size = nrow(diabetes.subset.n())*0.8,replace = FALSE))
        train.diabetes <- reactive(diabetes.subset()[dat.d(),]) # 80% training data / 614 rows
        
        test.diabetes <- reactive(diabetes.subset()[-dat.d(),]) # remaining 20% test data / 154 rows
        
        # Now creating seperate dataframe for 'Outcome' feature which is our target.
        train.diabetes_lables <- reactive(diabetes.subset()[dat.d(),1])
        test.diabetes_lables <- reactive(diabetes.subset()[-dat.d(),1])
        
        
        knn.27 <- reactive(knn(train = train.diabetes(), test = test.diabetes(), cl=train.diabetes_lables(), k=as.numeric(input$k)))
       
        ACC.27 <- reactive(100 * sum(test.diabetes_lables() == knn.27())/NROW(test.diabetes_lables())) # for knn = 27
       
        
        output$knn <- renderTable({
            table(knn.27() ,test.diabetes_lables()) # To check prediction against actual value in tabular form
        })
        
        output$table<- renderTable({
            data()
        })
        
        output$sum <-renderPrint({
            if(is.null(data())){return ()}
            summary(data())
        })
        
        output$var1_select_1<-renderUI({
            selectInput("ind_var_select_1","Select Independent Var", choices =as.list(names(data())),multiple = FALSE)
        })
        output$rest_var_select_1<-renderUI({
            checkboxGroupInput("other_var_select_1","Select other Var",choices =as.list(names(data())))
        })
        
        output$con_matrix<-renderPrint({
            td <- reactive(read.csv(input$file1$datapath))
            td2 <- reactive(na.omit(td()))
            input$other_var_select_1
            input$ind_var_select_1
            f<-data()
            
            library(caret)
            library(e1071)
            form <- sprintf("%s~%s",input$ind_var_select_1,paste0(input$other_var_select_1,collapse="+"))
            print(form)
            
            logreg <-glm(as.formula(form),family=binomial(),data=f)
            predicted_result <- predict(logreg,newdata=td2(),type='response')
            predicted_result2 <- ifelse(predicted_result > 0.5,1,0)
            confusionMatrix(data=as.factor(predicted_result2), reference=as.factor(td2()[,input$ind_var_select_1]))
            
            
            
        })
        
    }
    
    shinyApp(ui, server)

#https://jenilpatel740.shinyapps.io/KNN_Algorithm/?_ga=2.172935523.1362578590.1635081437-572493641.1624293732