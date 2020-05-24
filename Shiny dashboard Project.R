library(shiny)
library(shinydashboard)
library(gbm)
library(caret)
library(caretEnsemble)


ui <- fluidPage(
    dashboardPage(
    dashboardHeader(title="Basic Dashboard",
                    dropdownMenu(type = "messages",
                                 messageItem(
                                     from = "Personal Doctor",
                                     message = "Please schedule an Appointment if there is any issue"
                                 ),
                                 messageItem(
                                     from = "New User",
                                     message = "How do I use the Dashboard?",
                                     icon = icon("question"),
                                     time = "13:45"
                                 ),
                                 messageItem(
                                     from = "Support",
                                     message = "The new server is ready.",
                                     icon = icon("life-ring"),
                                     time = "2020-05-13"
                                 )
                    ),
                    dropdownMenu(type = "notifications",
                                 notificationItem(
                                     text = "7 new users today",
                                     icon("users")
                                 ),
                                 notificationItem(
                                     text = "5 Doctors are available ",
                                     icon("hospital"),
                                     status = "success"
                                 ),
                                 notificationItem(
                                     text = "Server load at 97%",
                                     icon = icon("exclamation-triangle"),
                                     status = "warning"
                                 )
                    ),
                    dropdownMenu(type = "tasks", badgeStatus = "success",
                                 taskItem(value = 90, color = "green",
                                          "Heart Disease"
                                 )
                    )),
    
    #Sidebar Content
    dashboardSidebar(
        sidebarMenu(
            menuItem("Heart Disease",tabName = "Heart Disease",icon=icon("dashboard"))
        )
    ),
    dashboardBody(
        fluidRow(
            valueBoxOutput("age"),
            valueBoxOutput("gender")
            
            
        ),
        
        box(
            title="Your Probability of Having Heart Disease is:",
            textOutput("Pred")
        ),

            box(
                title = "Please Fill in the Following to Get the Likelihood of Having Heart Disease",status="danger",background="navy", solidHeader = TRUE,
                numericInput(inputId='age', 
                             label='1.Please Enter your Age:', 
                             value=NA,min = NA, 
                             max = NA, step = NA,
                             width = NULL),
                checkboxGroupInput(inputId='sex', 
                                   label='Please check your gender:', 
                                   c('female','male'), 
                                   selected = NULL, 
                                   inline = FALSE,width = NULL),
                checkboxGroupInput(inputId = 'cp',label='Please describe the type of chest pain you have experienced:',
                                   c('normal pains','extreme pain','no pains','asymptomatic pain'),
                                   selected=NULL,
                                   inline = FALSE,width=NULL),
                numericInput('trestbps',label='Please Enter your resting blood pressure in mm/Hg:',
                             min=50,
                             max=200,
                             value = 120),
                numericInput(inputId = 'chol',label='Please enter your cholestrol levels:',
                             value=203,
                             min=80,
                             max=NA,
                             step=NA,
                             width=NULL),
            ),
            box(
                title = "Please Fill in the Following to Get the Likelihood of Having Heart Disease",status="danger",background="navy", solidHeader = TRUE,
                checkboxGroupInput(inputId = 'fbs',label='How can you describe your blood sugar levels after fasting:',
                                   c('normal','abnormal'),selected=NULL,
                                   inline=FALSE,width=NULL),
                
                checkboxGroupInput(inputId = 'restecg',label='How is your breathing when you are resting?',
                                   c('normal','probable difficulties','difficult'),
                                   selected=NULL,
                                   inline=FALSE,width=NULL),
                numericInput(inputId = 'thalach',label='what is the maximum heart rate you can achieve?',
                             value=100,
                             min=71,
                             max=NA,
                             step=NA,
                             width=NULL),
                checkboxGroupInput(inputId = 'exang',label='Have you experienced any chest pains after exercising?',
                                   c('no','yes'),selected=NULL,
                                   inline = FALSE,
                                   width = NULL),
                checkboxGroupInput(inputId = 'ca',label='Have you had any blood circulation problems?',
                                   c('all the time','a few times','quite a few times','almost never','never'),selected=NULL,
                                   inline=FALSE,
                                   width=NULL),
                checkboxGroupInput(inputId = 'thal',label = 'How can you describe the nature of your blood?',
                                   c('has unknown defect','is normal','has fixed defects','has reversible defects'),
                                   selected=NULL,
                                   inline = FALSE,
                                   width=NULL)
            )
        )
    )
)


server <- function(input, output,session) { 
    model=load("F:/Project Machine learning/Saved Models/model_gbm.RDS")
    
    output$age <- renderValueBox({
        valueBox(
            paste0(input$age), "Age", icon = icon("thumbs-up"),
            color = "light-blue"
        )
    })
    
    output$gender <- renderValueBox({
        valueBox(
            paste0(input$sex),"Gender",icon = icon("list"),
            color = "aqua"
        )
    })
    
   
    data<-eventReactive({
        
        req(input$sex)
        req(input$cp)
        req(input$fbs)
        req(input$restecg)
        req(input$exang)
        req(input$ca)
        req(input$thal)
        req(input$age)
        req(input$trestbps)
        req(input$chol)
        req(input$thalach)
       df_final= data.frame(
                            age=input$age,
                            sex=input$sex,
                            cp=input$cp,
                            trestbps=input$trestbps,
                            chol=input$chol,
                            fbs=input$fbs,
                            restecg=input$restecg,
                            thalach=input$thalach,
                            exang=input$exang,
                            ca=input$ca,
                            thal=input$thal
                            )
        })
    
   
    
    output$Pred <- renderText({
        
        predict(model,data(),type="response")
        
       })



    }

shinyApp(ui, server)