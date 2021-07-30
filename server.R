# The code creates tables in the global scope showing results of 
# three binary classification methods
# It then pass those tables to shinyServer() function to view on the web
#
# The code is splitted into three parts
# Part 1: get the data and clean the data
# Part 2: create confusion tables from various methods  
# Part 3: use shiny to post results on the web
#
#
library(caret);
library(party);
library(randomForest);
library(shiny);
library(class);
library(datasets);
#setwd("~/Users/nurur/ugw/dnsc05_computation/shinyApp")



## PART 1
##########################################################################
# GET THE TITANIC DATA
# ATTACH DATA FRAME TO MAKE OBJECTS WITHIN IT ACCESSIBLE IN R
##########################################################################
# mydata = read.csv("http://biostat.mc.vanderbilt.edu/wiki/pub/Main/DataSets/titanic3.csv")
mydata = read.csv("https://hbiostat.org/data/repo/titanic3.csv")


# CREATE A SUBET OF DATA WITH NO MISSING VALUES
mydata.subset <- subset(mydata, select = c(survived, pclass, sex, age, sibsp))
# Convert character type to int type
mydata.subset$sex = ifelse(mydata.subset$sex=='female',0,1)
mydata.nomiss <- na.omit(mydata.subset)
attach(mydata.nomiss)




## PART 2
##########################################################################
## GLM LOGISTIC
## CREATE A TABLE TO PRESENT MODEL PREDICTION VS ACTUAL DATA
##########################################################################
# BUILD A CONDITIONAL TREE MODEL WITH CLEAN DATA
t.lr = glm(survived ~ pclass + sex + pclass:sex + age + sibsp, 
           data = mydata.nomiss, family = binomial(logit))

# ESTIMATE THE PROBABILITIES OF SURVIVAL BASED ON LR MODEL
pred.survived.lr <- predict(t.lr, type="response")

# PREPARE TO ASSIGN VALUES 0 OR 1 BASED ON PROBABILITIES
a <- confusionMatrix( as.factor(round(pred.survived.lr)), 
                      as.factor(mydata.nomiss$survived), 
                      positive='1' )
# RETURN THE TABLE
tableA <- a$table


##########################################################################
## CONDITIONAL TREE
## CREATE A TABLE TO PRESENT MODEL PREDICTION VS ACTUAL DATA
##########################################################################
# BUILD A CONDITIONAL TREE MODEL WITH TITANIC DATA
t.ct <- ctree(as.factor(survived) ~ pclass + sex + age + sibsp, 
             data=mydata.nomiss, 
             controls = ctree_control(mincriterion = 0.95))

# PREDICT SURVIVAL USING TREE MODEL
pred.survived.ct <- predict(t.ct, type="response")

#attach(mydata);
tableB <- table(survived, pred.survived.ct, deparse.level = 2);


##########################################################################
## RANDOM FORESTS 
## CREATE A TABLE TO PRESENT MODEL PREDICTION VS ACTUAL DATA
##########################################################################
# BUILD A RANDOM FOREST MODEL WITH TITANIC DATA
t.rf = randomForest(as.factor(survived) ~ pclass + sex + age + sibsp, 
                    data=mydata.nomiss, ntree=5000)

# PREDICT SURVIVAL USING FOREST MODEL
pred.survived.rf <- predict(t.rf, type="response")

# PREPARE TO ASSIGN VALUES 0 OR 1 BASED ON PROBABILITIES
tc <- t.rf$confusion
tableC <- as.table( ( tc[1:2,1:2]) )




## PART 3
##########################################################################
# shinyServer() Function  
##########################################################################
# Define server logic required to generate and plot a random distribution
#
shinyServer(function(input, output) 
{
 
   LogisticRegression = tableA
   ClassificationTree = tableB
   RandomForest       = tableC


   datasetInput <- reactive({
   switch(input$variable,
           "LogisticRegression" = LogisticRegression,
           "ClassificationTree" = ClassificationTree,
           "RandomForest" = RandomForest)
   })


   output$viewTable <- renderTable({
     datasetInput();
   })

   # Generate a summary of the dataset
  output$summary <- renderPrint({
    #summary( datasetInput() )
    print('Reference : Column-wise', quote=FALSE)
    print('Prediction: Row-wise', quote=FALSE)
  })


})
##########################################################################

# Detach the dataset to clean up
detach(mydata.nomiss)

