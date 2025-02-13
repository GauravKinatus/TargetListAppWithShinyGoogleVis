suppressPackageStartupMessages(library(shiny))
suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(markdown))
suppressPackageStartupMessages(library(ISLR))


# Lets build the UI with a two columns

# side bar will take all the inputs from the User
shinyUI(
        pageWithSidebar(
                #Title of the app
                headerPanel(textOutput('targetTech')),
        
                sidebarPanel(
#                 sliderInput("Number of Beds", "Occuppied Beds", 
#                             min=10, max=250, value=150,  step=10,
#                             format="###0",animate=TRUE),
                        h3(textOutput('TAM')),
                        p("Total Available Market - All Hospitals and Practices in the US"),
                        h3(textOutput('SAM')),      
                        p("Serviceable Available Market - Technology Not Installed + Active Buyers"),
                        h3(textOutput('SOM')),
                        p("Serviceable Obtainable Market"),
                        radioButtons('varGroupBy', 
                             label = h4("Subset SOM by?"),
                             choices = list("Not Installed" = 1,
                                       "Active Buyers (New)" = 2,
                                       "Active Buyers (Replacement)" = 3,
                                       "All Prospects" = 4),
                                   selected = 4
                                   ) #radioButtons
                ),#/sidebarPane

                # Main panel will show the output of the plot and the documentation Tab        
                mainPanel(
                        p('This application is an intellectual excercise, a proof-of-concept.'), 
                        p('The intensity map is visualized using GoogleVis APIs + ShinyApps.io, powered by RStudio.
                           Data is generated by the intersecting GPO member list and HIMSS Analytics. 
                           Some filters may be applied for internal use. All data is property of its respective owner(s).' ),
                        htmlOutput("gvis"),
                        p('Feedback/comments: please send a note to gaurav underscore garg @ yahoo dot com')
            )#end of mainPanel

        )#pageWithSidebar
)#end of ShinyUI