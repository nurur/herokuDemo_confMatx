library(shiny)


# Define UI for slider demo application
shinyUI(pageWithSidebar(

  ##########################################################
  # Input Part of the code
  # Application title
  headerPanel("Binary Classification Methods"),

  # Sidebar with sliders that demonstrate various available options
  sidebarPanel(
   selectInput("variable", "Choose a Method:",
                choices = c("LogisticRegression", 
                            "ClassificationTree", 
                            "RandomForest")),

    br(),
    br(),                         #Create space between the dialog box 
                                  #and the submit button
		 
    submitButton("Update View")   #Create the submit button
    ),


  #########################################################
  # Output Part of the code
  # Show a table summarizing the values entered
  mainPanel(
    verbatimTextOutput("summary"),
    tableOutput("viewTable")
  )
  #########################################################
))


