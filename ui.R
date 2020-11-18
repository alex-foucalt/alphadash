#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(

    # Application title
    titlePanel("Alpha dashboard"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            fileInput("data", "Choose file", accept = "xslx"),
            br(),
            actionButton("daystat", "Count stats")
        ),

        # Show a plot of the generated distribution
        mainPanel(
            tabsetPanel(type = "tabs",
                        tabPanel("Summary",
                                 value = "summary",
                                 fluidRow(
                                     column(width = 12,
                                            h2("File summary"),
                                            uiOutput("summary")
                                     )),
                                 fluidRow(
                                     column(width = 12,
                                            uiOutput("days")
                                     ))
                                 ),
                        tabPanel("Themes",
                                 value = "themes",
                                 uiOutput("themes")),
                        tabPanel("Sources",
                                 value = "sources",
                                 uiOutput("sources"))
        )

        ) # mainpanel end
    )
))





