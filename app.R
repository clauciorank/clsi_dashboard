library(shiny)
library(rhandsontable)
library(shinydashboard)
library(dplyr)
library(mcr)
library(CLSIEP15)
library(shinyhelper)

# Define UI for application that draws a histogram
ui <- dashboardPage(
  dashboardHeader(title = "CLSI Metrics"),
  dashboardSidebar(
    sidebarMenu(
      menuItem('Info', tabName = 'info',
               icon = icon('info', 'fa-lg')),
      menuItem("Method Comparison", tabName = "plots",
               icon = icon("line-chart", "fa-lg"),
               comparison_sidebar('1')
      ),
      menuItem("Bias and Precision", tabName = "bias",
               icon = icon("bullseye", "fa-lg"),
               bias_sidebar('2')
    ),
    HTML(paste0(
      "<table style='margin-left:auto; margin-right:auto;'>",
      "<tr>",
      "<td style='padding: 5px;'><a href='https://www.facebook.com/nationalparkservice' target='_blank'><i class='fab fa-github fa-lg'></i></a></td>",
      "</tr>",
      "</table>",
      "<br>"))
  )
  ),
  dashboardBody(
    includeCSS("www/style.css"),
    tabItems(
      # Info tab
      tabItem('info',
              h1('CLSI Metrics Dashboard'),
              h3('This comprehensive dashboard is dedicated to calculating CLSI metrics for Method Comparison, Bias, and Precision, offering precise insights into laboratory performance.'),
              h5('All calculations are derived from the esteemed CLSI Manuals, ensuring accuracy and reliability.'),
              h5('For Bias and Precision analysis, references are made to EP15-A3 User Verification of Precision and Estimation of Bias; Approved Guideline—Third Edition.'),
              h5('For Method Comparison evaluations, EP09-A3 Measurement Procedure Comparison and Bias Estimation Using Patient Samples; Approved Guideline—Third Edition serves as the foundation.'),
              br(),
              h5('Developed and mantained by Claucio Antonio Rank Filho¹ with funding from Hilab'),
              h5('¹ Data Science - HILAB - clauciorank@gmail.com')
              ),
      # Method Comparison tabs UI
      comparison_data_box('1'),
      comparison_scatter_box('1'),
      comparison_coefficient_box('1'),
      comparison_difference_box('1'),
      comparison_bias_box('1'),
      t_test_box('1'),
      comparison_download_box('1'),
      # Bias and Precision tabs
      bias_data_box('2'),
      bias_precision_box('2'),
      bias_bias_box('2'),
      download_box('2')
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  observe_helpers()
  callModule(comparison_server, id = '1')
  callModule(bias_server, id = '2')
}

# Run the application 
shinyApp(ui = ui, server = server)
