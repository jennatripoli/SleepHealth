library(shiny)
library(pROC)
library(nnet)
library(splitstackshape)

setup <- function() {
  sleepData <<- read.csv("sleepData.csv")
  sleepData$BMI.Category[sleepData$BMI.Category == "Normal Weight"] <- "Normal"
  
  suppressWarnings({sleepData <- concat.split(sleepData, "Blood.Pressure", sep="/")})
  colnames(sleepData)[colnames(sleepData) == "Blood.Pressure_1"] <- "BP.Systolic"
  colnames(sleepData)[colnames(sleepData) == "Blood.Pressure_2"] <- "BP.Diastolic"
  sleepData <<- subset(sleepData, select=-c(Blood.Pressure))
}

convert_numerical <- function() {
  # create data set with numerical values only
  sleepDataNum <<- sleepData
  
  # convert gender to numeric values
  sleepDataNum$Gender[sleepDataNum$Gender == "Male"] <<- 0
  sleepDataNum$Gender[sleepDataNum$Gender == "Female"] <<- 1
  sleepDataNum$Gender <<- as.numeric(sleepDataNum$Gender)
  
  #convert occupation to numeric values
  sleepDataNum$Occupation[sleepDataNum$Occupation == "Accountant"] <<- 0
  sleepDataNum$Occupation[sleepDataNum$Occupation == "Doctor"] <<- 1
  sleepDataNum$Occupation[sleepDataNum$Occupation == "Engineer"] <<- 2
  sleepDataNum$Occupation[sleepDataNum$Occupation == "Lawyer"] <<- 3
  sleepDataNum$Occupation[sleepDataNum$Occupation == "Manager"] <<- 4
  sleepDataNum$Occupation[sleepDataNum$Occupation == "Nurse"] <<- 5
  sleepDataNum$Occupation[sleepDataNum$Occupation == "Sales Representative"] <<- 6
  sleepDataNum$Occupation[sleepDataNum$Occupation == "Salesperson"] <<- 7
  sleepDataNum$Occupation[sleepDataNum$Occupation == "Scientist"] <<- 8
  sleepDataNum$Occupation[sleepDataNum$Occupation == "Software Engineer"] <<- 9
  sleepDataNum$Occupation[sleepDataNum$Occupation == "Teacher"] <<- 10
  sleepDataNum$Occupation <<- as.numeric(sleepDataNum$Occupation)
  
  # convert bmi category to numeric values
  sleepDataNum$BMI.Category[sleepDataNum$BMI.Category == "Normal"] <<- 0
  sleepDataNum$BMI.Category[sleepDataNum$BMI.Category == "Overweight"] <<- 1
  sleepDataNum$BMI.Category[sleepDataNum$BMI.Category == "Obese"] <<- 2
  sleepDataNum$BMI.Category <<- as.numeric(sleepDataNum$BMI.Category)
  
  # convert sleep disorder to numeric values
  sleepDataNum$Sleep.Disorder[sleepDataNum$Sleep.Disorder == "None"] <<- 0
  sleepDataNum$Sleep.Disorder[sleepDataNum$Sleep.Disorder == "Insomnia"] <<- 1
  sleepDataNum$Sleep.Disorder[sleepDataNum$Sleep.Disorder == "Sleep Apnea"] <<- 2
  sleepDataNum$Sleep.Disorder <<- as.numeric(sleepDataNum$Sleep.Disorder)
}

# split the sleepData and sleepDataNum data frames in half randomly
split_data <- function() {
  split <- sample(c(TRUE, FALSE), 374, replace=TRUE, prob=c(0.5, 0.5))
  trainData <<- sleepData[split,]
  testData <<- sleepData[!split,]
  trainDataNum <<- sleepDataNum[split,]
  testDataNum <<- sleepDataNum[!split,]
  # reassign row numbers to remove missing rows
  row.names(trainData) <- NULL
  row.names(testData) <- NULL
  row.names(trainDataNum) <- NULL
  row.names(testDataNum) <- NULL
}

model_predict <- function(model, data, column) {
  # use model to predict values in data set
  data[[column]] <- as.numeric(predict(model, newdata=data))
  return (data)
}

make_model <- function(features) {
  req(length(features) > 0)
  modelFormula <<- "Sleep.Disorder ~"
  
  # create formula with selected features
  for (i in 1:length(features)) {
    modelFormula <<- paste(modelFormula, features[i])
    if (i != length(features)) modelFormula <<- paste(modelFormula, "+")
  }
  
  # make model with selected features and training data
  model <- multinom(eval(parse(text=modelFormula)), data=trainDataNum, trace=FALSE)
  # use model to predict values for training and testing data
  trainDataNum <<- model_predict(model, trainDataNum, "Predicted")
  testDataNum <<- model_predict(model, testDataNum, "Predicted")
}

model_accuracy <- function(features, data, column, type) {
  req(length(features) > 0)
  # create confusion matrix
  table <- table(data$Sleep.Disorder, data[[column]])
  # calculate accuracy of the model
  return (toString(paste(type, "Accuracy:", round((sum(diag(table))/sum(table))*100, 2))))
}

plot_roc <- function(features) {
  req(length(features) > 0)
  # create roc curve for current model
  return (roc(testDataNum$Sleep.Disorder, testDataNum$Predicted))
}

setup()
convert_numerical()
split_data()

ui <- fluidPage(
  titlePanel("Predicting Sleeping Disorders"),
  
  sidebarLayout(
    sidebarPanel(
      checkboxGroupInput(inputId = "features", label = "Features to Use in Model:",
                         c("Gender", "Age", "Occupation", "Sleep.Duration",
                           "Quality.of.Sleep", "Physical.Activity.Level",
                           "Stress.Level", "BMI.Category", "Heart.Rate",
                           "Daily.Steps", "BP.Systolic", "BP.Diastolic")),
      actionButton("splitData", "Re-Split Data Set"),
      uiOutput("lineBreak"),
      textOutput("trainingAccuracy"),
      textOutput("testingAccuracy")
    ),
  
    mainPanel(
      uiOutput("test"),
      plotOutput("rocPlot")
    )
  )
)

server <- function(input, output, session) {
  # randomize data split
  observeEvent(input$splitData, {
    split_data()
    make_model(input$features)
    output$rocPlot <- renderPlot ({ plot(plot_roc(input$features), main="ROC Curve for Model") })
    output$trainingAccuracy <- renderText({ model_accuracy(input$features, trainDataNum, "Predicted", "Training") })
    output$testingAccuracy <- renderText({ model_accuracy(input$features, testDataNum, "Predicted", "Testing ") })
  })
  
  # create plot with selected features
  observeEvent(input$features, {
    make_model(input$features)
    output$rocPlot <- renderPlot ({ plot(plot_roc(input$features), main="ROC Curve for Model") })
    output$trainingAccuracy <- renderText({ model_accuracy(input$features, trainDataNum, "Predicted", "Training") })
    output$testingAccuracy <- renderText({ model_accuracy(input$features, testDataNum, "Predicted", "Testing ") })
  })
  
  output$lineBreak <- renderUI({ HTML("<br>") })
  
  output$test <- renderUI({
    HTML("<br> In this case study, we will look at a data set about sleep health and lifestyle for 374 participants.
    The data set includes features such as gender, age, occupation, sleep duration, quality of sleep, physical activity level,
    stress level, BMI category, blood pressure, heart rate, daily steps, and what sleep disorder they have (if applicable).
    The purpose of this case study is to find what features are useful in predicting if a patient has a sleeping disorder and
    creating a classification algorithm to accomplish this with a testing data set. <br><br> In this application, a multinomial 
    classification model was developed to predict whether a participant has a sleeping disorder. The plot below is the ROC curve, 
    which shows the overall performance of the classification model. The check boxes in the side panel allows for selecting desired 
    features to use for the model. To develop a classification model, the data set must be split into a training set and testing 
    set. The button in the side panel will randomly split the original data set in half to create these two data sets.<br><br>")
  })
}

shinyApp(ui=ui, server=server)
