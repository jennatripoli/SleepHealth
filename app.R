require(shiny)
library(splitstackshape)
library(pROC)
library(nnet)
library(ggplot2)
library(ggeffects)
library(corrplot)

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

  # convert sleep disorder to numeric values
  trainDataNum2 <<- trainDataNum
  trainDataNum2$Sleep.Disorder[trainDataNum2$Sleep.Disorder == "None"] <<- 0
  trainDataNum2$Sleep.Disorder[trainDataNum2$Sleep.Disorder == "Insomnia"] <<- 1
  trainDataNum2$Sleep.Disorder[trainDataNum2$Sleep.Disorder == "Sleep Apnea"] <<- 2
  trainDataNum2$Sleep.Disorder <<- as.numeric(trainDataNum2$Sleep.Disorder)
}

model_predict <- function(model, data, column) {
  # use model to predict values in data set
  data[[column]] <- as.numeric(predict(model, newdata=data))
  return (data)
}

make_model <- function(features) {
  # require that at least 1 feature is selected
  req(length(features) > 0)

  # create formula with selected features
  modelFormula <<- "Sleep.Disorder ~"
  for (i in 1:length(features)) {
    modelFormula <<- paste(modelFormula, features[i])
    if (i != length(features)) modelFormula <<- paste(modelFormula, "+")
  }
  
  # make model with selected features and training data
  model <- multinom(eval(parse(text=modelFormula)), data=trainDataNum, trace=FALSE)
  # use model to predict values for training and testing data
  trainDataNum <<- model_predict(model, trainDataNum, "Predicted")
  testDataNum <<- model_predict(model, testDataNum, "Predicted")
  return (model)
}

model_accuracy <- function(features, data, column, type) {
  # require that at least 1 feature is selected
  req(length(features) > 0)
  
  # calculate accuracy of the model
  table <- table(data$Sleep.Disorder, data[[column]])
  return (toString(paste(type, "Accuracy:", round((sum(diag(table))/sum(table))*100, 2))))
}

make_roc <- function(features) {
  # require that at least 1 feature is selected
  req(length(features) > 0)
  
  # calculate roc curve for current model
  return (suppressMessages(multiclass.roc(testDataNum$Sleep.Disorder, testDataNum$Predicted, AUC=TRUE)))
}

plot_roc <- function(features, roc) {
  # require that at least 1 feature is selected
  req(length(features) > 0)
  
  # create plot for roc curve
  plot <- plot.roc(roc[['rocs']][[1]], main="ROC Curve for Current Model")
  sapply(2:length(roc[['rocs']]), function(i) { lines.roc(roc[['rocs']][[i]], col=i) })
  legend("bottomright", c("None", "Insomnia", "Sleep Apnea"), fill=c("black", "red", "green"))
  return (plot)
}

plot_regression <- function(features, model) {
  # require that at least 1 feature is selected
  req(length(features) > 0)
  
  # create plot for logistic regression model
  return (plot(ggpredict(model, se=TRUE, interactive=TRUE)))
}

setup()
convert_numerical()
split_data()

ui <- fluidPage(
  titlePanel("Predicting Sleeping Disorders"),
  
  sidebarLayout(
    sidebarPanel(
      checkboxGroupInput(inputId = "features", label = "Features to Use in Model:",
                         c("Gender", "Age", "Occupation", "Sleep Duration" = "Sleep.Duration", "Sleep Quality" = "Quality.of.Sleep", 
                           "Physical Activity Level" = "Physical.Activity.Level", "Stress Level" = "Stress.Level", 
                           "BMI Category" = "BMI.Category", "Heart Rate" = "Heart.Rate", "Daily Steps" = "Daily.Steps", 
                           "Systolic Blood Pressure" = "BP.Systolic", "Diastolic Blood Pressure" = "BP.Diastolic")),
      actionButton("splitData", "Re-Split Data Set"), uiOutput("lineBreak1"), 
      textOutput("auc0"), textOutput("auc1"), textOutput("auc2"), htmlOutput("auc3")
    ),
  
    mainPanel(
      tabsetPanel(type = "tabs",
                  tabPanel("Description", uiOutput("description")),
                  tabPanel("Regression Plot", uiOutput("lineBreak2"), plotOutput("regressionPlot")),
                  tabPanel("ROC Plot", uiOutput("lineBreak3"), plotOutput("rocPlot")),
                  tabPanel("Mathematics", uiOutput("mathematics1"), uiOutput("formula1"), uiOutput("mathematics2"), uiOutput("formula2"), uiOutput("mathematics3")),
                  tabPanel("Analysis", uiOutput("analysis1"), plotOutput("corrPlot"), uiOutput("analysis2"))
      )
    )
  )
)

server <- function(input, output, session) {
  display_data <- function() {
    if (length(input$features) > 0) {
      output$auc0 <- renderText({ toString(paste("AUC for None:", round(auc(roc[['rocs']][[1]]), 2))) })
      output$auc1 <- renderText({ toString(paste("AUC for Insomnia:", round(auc(roc[['rocs']][[2]]), 2))) })
      output$auc2 <- renderText({ toString(paste("AUC for Sleep Apnea:", round(auc(roc[['rocs']][[3]]), 2))) })
      output$auc3 <- renderText({ toString(paste("<b>Overall AUC:", round(auc(roc), 2), "</b>")) })
    } else {
      output$auc0 <- renderText({ toString(paste("")) })
      output$auc1 <- renderText({ toString(paste("")) })
      output$auc2 <- renderText({ toString(paste("")) })
      output$auc3 <- renderText({ toString(paste("<b>Please select at least 1 feature.</b>")) })
    }
    
    model <- make_model(input$features)
    roc <- make_roc(input$features)
    output$regressionPlot <- renderPlot ({ plot_regression(input$features, model) })
    output$rocPlot <- renderPlot ({ plot_roc(input$features, roc) })
  }
  
  # create plot with selected features
  observeEvent(input$features, { display_data() }, ignoreNULL=FALSE)
  
  # randomize data split and create plot
  observeEvent(input$splitData, {
    split_data()
    display_data()
    output$corrPlot <- renderPlot({ corrplot(cor(trainDataNum2[, -1]), tl.col="black", tl.cex=1) })
  })
  
  output$lineBreak1 <- renderUI({ HTML("<br>") })
  output$lineBreak2 <- renderUI({ HTML("<br>") })
  output$lineBreak3 <- renderUI({ HTML("<br>") })

  output$description <- renderUI({
    HTML("<br> In this case study, we will look at a data set about sleep health and lifestyle for 374 participants. 
    The data set includes features such as gender, age, occupation, sleep duration, quality of sleep, physical activity level, 
    stress level, BMI category, blood pressure, heart rate, daily steps, and what sleep disorder they have (if applicable). 
    The original data set can be found on Kaggle <a href='https://www.kaggle.com/datasets/uom190346a/sleep-health-and-lifestyle-dataset'>here</a>. 
    The purpose of this case study is to find what features are useful in predicting if a patient has a sleeping disorder and create a 
    classification algorithm to accomplish this with a testing data set. The motivation for researching this topic is that it is a very 
    relevant subject since many people suffer from sleep issues. When looking for a data set, it was important to me to find one that was about 
    a topic that I find personal interest in and had a variety of different features to look at. <br><br>
    
    In this application, a multinomial classification model was developed to predict whether a participant has a sleeping disorder. The regression 
    plot displays the distribution of (p) and (1-p) for each class (none, insomnia, and sleep apnea). The ROC plot shows the overall performance of 
    the classification model for each class. The checkboxes in the side panel allow for selecting desired features to use for the model. To develop 
    a classification model, the data set must be split into a training set and a testing set. The button in the side panel will randomly split the 
    original data set in half to create these two data sets. <br><br>
    
    Sleep disorder is a categorical variable since there are three discrete values it can have -- 'None', 'Insomnia', and 'Sleep Apnea'. The algorithm 
    we want to use should be for supervised learning because we are making a predictive model. Knowing that we are predicting a categorical variable 
    with supervised learning, there are a variety of options for algorithms to use. I selected logistic regression because it represents the relationship 
    between a single dependent discrete variable (response) and one or more independent variables (predictors). Additionally, logistic regression is 
    especially useful in this case because there are multiple classes to predict. The features being input were both numerical and categorical, and 
    logistic regression can handle this well with classification. <br><br>") })
  
  output$mathematics1 <- renderUI({ 
    HTML("<br>Visualizing a logistic regression model involves plotting the overall fraction of the response variable, with probability on the y-axis. 
    The model returns a score that estimates the probability for each of the three classes of sleep disorder. There is one plot for each discrete option 
    for the value of sleep disorder since this is a multinomial model. Each graph compares the distribution of participants with that specific value for 
    sleep disorder and those who do not as a function of the model's predicted probability. We can use a ROC curve as a visual diagnostic tool for the 
    logistic regression model. The area under the curve represents how well the model can predict, with the ideal value being AUC = 1. On an ROC curve, 
    the x-axis is the False Positive Rate and the y-axis is the True Positive Rate. When AUC = 1, the curve of the graph is to the upper left corner. 
    This means that the closer the ROC curve is to the upper left corner, the higher the accuracy of the model is. Below is the formula for logistic regression:") })
  
  output$formula1 <- renderUI({ withMathJax(helpText("$$ln(\\frac{p}{1-p})=b_{0}+b_{1}x$$")) })
  output$mathematics2 <- renderUI({ HTML("For modeling purposes, this formula is rearranged to become the following formula:") })
  output$formula2 <- renderUI({ withMathJax(helpText("$$p=\\frac{1}{1+e^{-(b_{0}+b_{1}x)}}$$")) })
  
  output$mathematics3 <- renderUI({ 
    HTML("Plotting logistic regression has an S-curve, with the predicted y-values lying within the range of 0 to 1. The case of interest is y = 1 because 
         this indicates 'true'. Logit(P(y = 1)) is inverted by the sigmoid function to compute label probabilities. Categorical variables are expanded with 
         one-hot encoding. Logistic regression is an iterative solution since the least squares are re-weighted over and over until the optimized model is found.") })

  output$analysis1 <- renderUI({
    HTML("<br> Through testing out different combinations of features, we can find a good group of predictors to use for the model. To have some guidance on what 
         features to look at, I created a correlation matrix for all available features in the training data. <br><br>") })
  
  output$corrPlot <- renderPlot({ corrplot(cor(trainDataNum2[, -1]), tl.col="black", tl.cex=1) })
  
  output$analysis2 <- renderUI({
    HTML("<br> From the correlation matrix, we can see that BMI category, systolic blood pressure, and diastolic blood pressure all have strong 
         positive correlations with sleep disorder. Age and occupation also have moderate positive correlations with sleep disorder. 
         Therefore, these are the variables that we will test out first with our model. The variables sleep duration, quality of sleep, 
         stress level, and daily steps all have very weak correlations with sleep disorder, so they will likely not be considered for 
         our optimal model and are noisy data. <br><br>
         
         When looking at the ROC curve for a model with predictors of the three features with strong positive correlations, we can observe 
         that the overall AUC is usually above 0.8, which is a good sign. However, we can see some decent variability in the overall AUC 
         when re-splitting the data to change the training set. This is an indication to add additional features to the model, so I decided 
         to include age and occupation as well. This reduced the variability in the overall AUC and made it consistently above 0.82, making 
         this model the optimal choice. To prove the point about the other available features being noisy data, we can also a model with all 
         12 features selected as predictors. In doing this, we can see that the overall AUC has very similar behavior to the previous model 
         with 5 features, showing that the other 7 variables are not necessary when predicting sleep disorder. <br><br>") })
}

shinyApp(ui=ui, server=server)