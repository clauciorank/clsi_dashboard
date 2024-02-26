# Sidebar of method comparison
comparison_sidebar <- function(id){
  ns <- NS(id)
  
  tagList(
    menuSubItem("Data", tabName = ns("data")),
    menuSubItem("Scatter Plot", tabName = ns("scatter")),
    menuSubItem("Coefficient Plot", tabName = ns("coef")),
    menuSubItem("Difference Plot", tabName = ns("bland")),
    menuSubItem("Bias Plot", tabName = ns("bias")),
    menuSubItem("T test", tabName = ns("t_test")),
    menuSubItem("Download", tabName = ns("download_comparison"))  
  )
}

comparison_download_box <- function(id){
  ns <- NS(id)
  
  tabItem(
    tabName = ns('download_comparison'),
    title = 'Generate Report',
    actionButton(ns('generate_pdf'), 'Generate'),
    uiOutput(ns('pdf_viewer'))
  )
}

# UI for inserting data and regression analysis parameters
comparison_data_box <- function(id){
  ns <- NS(id)
  
  tabItem(
    tabName = ns('data'),  
    box(
      title = 'Enter Data',
      status = 'info',
      rHandsontableOutput(ns("hot"))
    ),
    box(
      title =  helper('Regression Analysis Parameters', type = 'markdown', content = 'regression', style = "padding-left: 20px;"),
      status = 'info',
      fixedRow(
        column(6,
               selectInput(ns('regmodel'), h5('Regression Model'), 
                           choices=list('Ordinary Least Square' = 'LinReg',
                                        'Deming' = 'Deming',
                                        'Passing-Bablok' = 'PaBa'
                                        )
               )
               ),
        column(6,
               selectInput(ns('cormet'), h5('Correlation Method'),
                           choices = list('Pearson' = 'pearson',
                                          'Spearman' = 'spearman',
                                          'Kendall' = 'kendall'
                           )
               )
               )
      ),
      fixedRow(
        column(6,
               selectInput(ns('alpha'), h5('Significance Level (α)'),
                           choices = c(.1, .05, .01),
                           selected = .05)
               ),
        column(6,
               selectInput(ns('cimethod'), h5('CI Method'), 
                           choices=list('Analytical' = 'analytical',
                                        'Jackknife' = 'jackknife',
                                        'Bootstrap' = 'bootstrap'
                           )
               )
               )
      ),
      fixedRow(
        actionButton(ns('calculate'), label = 'Generate calculations')
      ),
      fixedRow(
          verbatimTextOutput(ns('reg_parameters'))
      ),
    )
  )
}

# UI for scatter plot showing
comparison_scatter_box <- function(id){
  ns <- NS(id)
  
  tabItem(tabName = ns('scatter'),
            title = 'Scatter Plot',
            fixedRow(
              column(4, numericInput(ns('clinical_decision1'), 'Clinical Decision Boundaries', value = NA)),
              column(4, numericInput(ns('clinical_decision2'), 'Clinical Decision Boundaries', value = NA)),
              column(4, numericInput(ns('clinical_decision3'), 'Clinical Decision Boundaries', value = NA))
            ),
            plotOutput(ns('scatter_plot'), height = 820),
            plotOutput(ns('ci_plot'), height = 820)
          )
}

# UI for coefficient plot
comparison_coefficient_box <- function(id){
  ns <- NS(id)
  
  tabItem(tabName = ns('coef'),
          title = 'Coefficient Plot',
          plotOutput(ns('coefficient_plot'), height = 820)
  )
}

# UI for difference plot
comparison_difference_box <- function(id){
  ns <- NS(id)
  
  tabItem(tabName = ns('bland'),
          title = 'Difference Plot',
          numericInput(ns('maximum_error'), label = 'Maximum allowed error (%)', value = 2),
          tableOutput(ns('tae')),
          plotOutput(ns('difference_plot_perc'), height = 400),
          plotOutput(ns('difference_plot_abs'), height = 400)
  )
}

# UI for bias plot
comparison_bias_box <- function(id){
  ns <- NS(id)
  
  tabItem(tabName = ns('bias'),
          title = 'Bias Plot',
          fixedRow(
            column(4,
                   numericInput(ns('bias_abs_allowed'), label = 'Absolute Bias Allowed', value = NA)
                   ),
            column(4,
                   numericInput(ns('bias_perc_allowed'), label = 'Percentage Bias Allowed', value = NA)
            ),
            column(4,
                   selectInput(ns('ci_bias'), label = 'Confidence Interval', choices = c(90,95,99), selected = 95)
            )
          ),
          plotOutput(ns('bias_plot'), height = 800)
  )
}

# T test comparison
t_test_box <- function(id){
  ns <- NS(id)
  
  tabItem(tabName = ns('t_test'),
          title = 'T test',
          fixedRow(
            column(width = 12, selectInput(ns('t_test_param'), 't test type', choices = c('Parametric', 'Non-Parametric')))
          ),
          verbatimTextOutput(ns('t_test_output')),
          plotOutput(ns('boxplot_t_test'), height = 600)
          )
}


# Shiny server for module comparison
comparison_server <- function(input, output, session){
  ns <- session$ns
  
  # Data input 
  output$hot <- renderRHandsontable({
    a <- datasetInput()
    rhandsontable(a, height = 482) %>%
      hot_col(col = 'Method1', format = '0.00', type = 'numeric') %>%
      hot_col(col = 'Method2', format = '0.00', type = 'numeric')
    
  })
  
  # Reactive data for calculations
  datasetInput <- reactive({
    if (is.null(input$hot)) {
      mat <- data.frame('Method1'= round(c(rep(NA, 10)), digits = 2),
                        'Method2'= round(c(rep(NA, 10)), digits = 2))
    } else {
      mat <- hot_to_r(input$hot)
    }
  })
  
  
  # Regression
  regression_info <- eventReactive(input$calculate, {
    dat <- 
      hot_to_r(input$hot) |> 
      rename(X = Method1, Y = Method2) |> 
      filter(!is.na(X) & !is.na(Y))
    
    mcreg(dat$X, dat$Y, method.ci = input$cimethod, 
          method.reg = input$regmodel, alpha = as.numeric(input$alpha))
  })
  
  # Output regression parameters
  output$reg_parameters <- renderPrint({
    req(regression_info())
    
    printSummary(regression_info())
  })
  
  # Output scatter
  output$scatter_plot <- renderPlot({
    req(regression_info())
    
    plot_regression(regression_info(), input$cormet, input$clinical_decision1, 
                    input$clinical_decision2, input$clinical_decision3)
  })
  
  # Output CI for clinical decision
  output$ci_plot <- renderPlot({
    req(regression_info())
    req(!is.na(input$clinical_decision1) | 
          !is.na(input$clinical_decision2) |
          !is.na(input$clinical_decision3)
    )
    
    plot_ci(regression_info(),input$clinical_decision1, 
            input$clinical_decision2, input$clinical_decision3)
  })
  
  # Output coefficient plot
  output$coefficient_plot <- renderPlot({
    req(regression_info())
    
    compareFit(regression_info())
  })
  
  # Output TAE
  output$tae <- renderTable({
    req(regression_info())
    
    get_tae(hot_to_r(input$hot))
  }, align = 'c', width = '100%', bordered = TRUE)
  
  # Output difference plot percentage
  output$difference_plot_perc <- renderPlot({
    req(regression_info())
    req(input$maximum_error)
    
    difference_plots(regression_info(), input$maximum_error/100, 'percentage')
  })
  
  # Output difference plot absolute
  output$difference_plot_abs <- renderPlot({
    req(regression_info())
    req(input$maximum_error)
    
    difference_plots(regression_info(), input$maximum_error/100, 'absolute')
  })
  
  # Output bias
  output$bias_plot <- renderPlot({
    req(regression_info())
    req(input$ci_bias)

    limits <- calculate_total_bias(regression_info()@data$x, regression_info()@data$y, alpha = 1-as.numeric(input$ci_bias)/100)
    generate_bias_plot(limits, as.numeric(input$bias_abs_allowed), as.numeric(input$bias_perc_allowed)/100)
  })
  
  # Output t-test
  output$t_test_output <- renderPrint({
    req(regression_info())
    req(input$t_test_param)
    
    dat <- 
      hot_to_r(input$hot)
    
    if(input$t_test_param == 'Parametric'){
      t.test(dat$Method1, dat$Method2)
    }else{
      wilcox.test(dat$Method1, dat$Method2)
    }
  })
  
  # Boxplot t test
  output$boxplot_t_test <- renderPlot({
    req(regression_info())

    dat <- 
      hot_to_r(input$hot) |> 
      tidyr::pivot_longer(1:2)
    
    dat |> 
      ggplot(aes(name, value))+
      geom_boxplot()+
      theme_bw()+
      labs(y = 'Result')+
      theme(text = element_text(size = 22),
            axis.title.x = element_blank())
    
    
  })
  
  ## Generate pdf
  observeEvent(input$generate_pdf, {

    # create temp file
    tmp <- tempfile(fileext = '.pdf', tmpdir = 'tmp')
    # render markdown
    rmarkdown::render('markdown/method_comparison.Rmd', output_format = 'pdf_document', output_file = tmp, output_dir = 'tmp')
    # add path and alias to tmp directory
    addResourcePath('tmp', 'tmp')

    # Render PDF in the screen
    output$pdf_viewer <- renderUI({
      req(length(list.files('tmp/')) > 0)
      tags$iframe(
        src = tmp,
        width = "100%",
        height = "850px"
      )
    })

  })
}