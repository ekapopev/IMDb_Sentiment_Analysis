# Functions for creating fluid page layouts in shiny Application.
# A fluid page layout consists of rows which in turn include columns
################################################################################
ui = fluidPage(
  
  # Setting a theme for the Shiny app. The theme that is used here is called 'darkly'
  # package used here is shinytheme
  theme = shinytheme("darkly"),
  # Defining the header Panel on the shiny application 
  # h3- argument is used to obtain a specific size for the header/ title.
  # windowTitle - The title that should be displayed by the browser window. 
  headerPanel(h3("IMDb review Sentiment Analysis"), windowTitle = "Group 3"),
  # Sidebar Layout - used to create a layout with a sidebar and main area in the Shiny Aplication.
  sidebarLayout(
    # Create a sidebar panel containing input controls that can in turn be passed to sidebarLayout.
    # img argument is used to load an image into the sodeba panel of the shiny Application
    sidebarPanel(
                 # radioButtons -Create a set of radio buttons used to select an item from a list.          
                  radioButtons("typeInput", "Sentiment Review by: ",
                               list("Upload a file" = "fileUpload", "Type a review"= "typeReview")),
                 # Creates a panel that is visible or not, depending on the value of the input.
                 # Condition 1 - Only show this panel if input type is "Upload a file"    
                 conditionalPanel(
                   condition = "input.typeInput == 'fileUpload'",
                   #Create an input control for entry of unstructured text values
                   fileInput('file1', 'Load Data File',
                             accept = c('text/csv','text/comma-separated-values',
                                        '.csv'))
                 ),
                 
                 # Condition 2 - Only show this panel if Input type is "Type a review"
                 conditionalPanel(
                   condition = "input.typeInput == 'typeReview'",
                   textAreaInput("reviewInput", "Review", placeholder = "Type review here")),
                 
                 
                 # actionButton - Used to create a go button, that allows the shiny Application the execute the input
                   actionButton("goButton", "Search", icon("upload"),
                                style="color: #fff; background-color: #337ab7") ,width = 3),
    
    # Panel to display output
    # mainPanel - Create a main panel containing output elements that can in turn be passed to sidebarLayout.
    mainPanel(
      # Tabsets - used for dividing output into multiple independently viewable sections.
      # Dividing the main panel into multiple tabs
      tabsetPanel(
        # tabPanel - Create a tab panel that can be included within a tabsetPanel.
        # Argument plotOutput - used to create a plot as an output element based on the inputid that is passed to it
        tabPanel("Sentiment Class", withSpinner(dataTableOutput("imdbTable"))),
        navbarMenu("Frequency Plot",
                   tabPanel("TM", withSpinner(plotOutput("plot1"))),
                   tabPanel("TFIDF", withSpinner(plotOutput("plot2")))),
        navbarMenu("Frequency Plot TM",
                   tabPanel("Positive", withSpinner(plotOutput("plot3"))),
                   tabPanel("Negative", withSpinner(plotOutput("plot4")))),
        navbarMenu("Frequency Plot TFIDF",
                   tabPanel("Positive", withSpinner(plotOutput("plot5"))),
                   tabPanel("Negative", withSpinner(plotOutput("plot6")))),
        navbarMenu("Word Clouds TM",
                   tabPanel("Positive", withSpinner(wordcloud2Output("wordCloud1", width = "100%", height = "400px"))),
                   tabPanel("Negative", withSpinner(wordcloud2Output("wordCloud2", width = "100%", height = "400px")))),
        navbarMenu("Word Clouds TFIDF",
                   tabPanel("Positive", withSpinner(wordcloud2Output("wordCloud3", width = "100%", height = "400px"))),
                   tabPanel("Negative", withSpinner(wordcloud2Output("wordCloud4", width = "100%", height = "400px")))),
        
        tabPanel("Words Cooccurences Network", withSpinner(plotOutput("plot7"))),
        tabPanel("Overall Sentiment Emotions", withSpinner(plotOutput("plot8"))),
        tabPanel("Overall Sentiment Result", withSpinner(plotOutput("plot9")))
        ,type = "pills"), width = 9)
  )
)

