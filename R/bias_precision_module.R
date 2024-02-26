bias_sidebar <- function(id){
  ns <- NS(id)
  
  tagList(
    menuSubItem("Data", tabName = ns("data")),
    menuSubItem("Precision", tabName = ns("precision")),
    menuSubItem("Bias", tabName = ns("bias_bias")),
    menuSubItem("Download", tabName = ns("download"))
  )
}

download_box <- function(id){
  ns <- NS(id)
  
  tabItem(
    tabName = ns('download'),
    title = 'Generate Report',
    actionButton(ns('generate_pdf'), 'Generate'),
    uiOutput(ns('pdf_viewer'))
  )
}

bias_data_box <- function(id){
  ns <- NS(id)
  
  tabItem(
    tabName = ns('data'),  
      title = 'Enter Data',
      status = 'info',
      box(
        fixedRow(
          column(6, selectInput(ns('ncols'), 'Nº Runs', choices = 5:7)),
          column(6, selectInput(ns('nrows'), 'Nº Repetitions', choices = 5:7))
        ),
        actionButton(ns('resize'), 'Resize Table')
      ),
      box(
        title = 'Insert Data',
        rHandsontableOutput(ns("hot"))
      )
    )
}

bias_precision_box <- function(id){
  ns <- NS(id)
  
  tabItem(
    tabName = ns('precision'),
    title = 'Precision',
    status = 'info',
    box(width = 10,
        fixedRow(
            column(2, numericInput(ns('s_r'), 'Allowed error repetability (SR)', value = NA)),
            column(2, numericInput(ns('s_wl'), 'Allowed error within-lab (SWL)', value = NA)),
            column(2, numericInput(ns('cv_r'), 'CV repetability(%) (CVR)', value = NA)),
            column(2, numericInput(ns('cv_wl'), 'CV within-lab(%) (CVWL)', value = NA))
        )
    ),
    fixedRow(
      box(
         h3('ANOVA Parameters'),
         tableOutput(ns('anova_table')),
         h3('Upper Value Limits'),
         tableOutput(ns('uvl_table'))
      ),
      box(
        plotOutput(ns('plot_precision'), height = 675)
      )
    )
  )
}

bias_bias_box <- function(id){
  ns <- NS(id)
  
  tabItem(
    tabName = ns('bias_bias'),
    title = 'Bias',
    status = 'info',
    box(
      title = helper('Scenario', type = 'markdown', content = 'scenarios'),
      width = 12,
      selectInput(ns('scenario'), 'Select Scenario', choices = c('A', 'B or C', 'D or E')),
      uiOutput(ns('subscenario')),
      uiOutput(ns('A_Uk')),
      uiOutput(ns('A_u')),
      uiOutput(ns('A_Ucoverage')),
      uiOutput(ns('A_lowerupper')),
      uiOutput(ns('BC')),
      uiOutput(ns('DE'))
    ),
    box(
      width = 12,
      tableOutput(ns('bias_df')),
      plotOutput(ns('plot_bias'), height = 500)
    )
  )
  
}



bias_server <- function(input, output, session){
  ns <- session$ns
  
  # Data
  
  observeEvent(input$resize, {
    cols <- paste0('Run', 1:as.numeric(input$ncols))
    
    df <- data.frame(matrix(NA, ncol = length(cols), nrow = as.numeric(input$nrows)))
    colnames(df) <- cols
    
    output$hot <- renderRHandsontable({
      rhandsontable(df) |> 
        hot_cols(format = '0.00', type = 'numeric')
    })
  })
  
  data_input <- reactive({
    req(input$hot)
    
    df <- hot_to_r(input$hot) |> 
      mutate(rep = 1:n()) |> 
      relocate(rep, .before = everything())
    
    create_table_ep_15(df, data_type = 'wider')
    
  })
  
  # Precision
  precision_results <- reactive({
    req(data_input())
    req(nrow(data_input()) > 10)
    
    
    calculate_precision_outputs(data_input(), s_allowed_r = input$s_r, 
                                input$s_wl, input$cv_r, input$cv_wl
                                )
    
  })
  
  
  output$anova_table <- renderTable({
    req(precision_results())
    
    precision_results()$df
  },striped = TRUE, bordered = TRUE,  spacing = 'm',  
  width = '100%', colnames = FALSE, align = 'c')
  
  output$uvl_table <- renderTable({
    req(precision_results())
    
    precision_results()$df_uvl
  },striped = TRUE, bordered = TRUE,  spacing = 'm',  
  width = '100%', align = 'c')
  
  output$plot_precision <- renderPlot({
    req(precision_results())
    
    precision_results()$plot +
      theme(text = element_text(size = 20), axis.title.y = element_blank())
  })
  
  
  # BIAS
  output$subscenario <- renderUI({
    req(input$scenario == 'A')
    
    selectInput(ns('subscenario'), 'Select Subscenario', choices = c('u', 'Uk', 'Ucoverage', 'lowerupper'))
  })
  
  # Dynamic UI
  output$A_u <- renderUI({
    req(input$scenario == 'A')
    req(input$subscenario == 'u')
    
    fixedRow(
          column(4, numericInput(ns(paste0('expected_mean', input$scenario, input$subscenario)), 'True Value', value = NA)),
          column(4, numericInput(ns(paste0('u', input$scenario, input$subscenario)), 'u', value = NA)),
          column(4, actionButton(ns(paste0('generate_bias', input$scenario, input$subscenario)), 'Generate'))
        )
  })
  
  output$A_Uk <- renderUI({
    req(input$scenario == 'A')
    req(input$subscenario == 'Uk')
    
      fixedRow(
          column(3, numericInput(ns(paste0('expected_mean', input$scenario, input$subscenario)), 'True Value', value = NA)),
          column(3, numericInput(ns(paste0('U', input$scenario, input$subscenario)), 'U', value = NA)),
          column(3, numericInput(ns(paste0('k', input$scenario, input$subscenario)), 'k', value = NA)),
          column(3, actionButton(ns(paste0('generate_bias', input$scenario, input$subscenario)), 'Generate'))
        )
  })
  
  output$A_Ucoverage <- renderUI({
    req(input$scenario == 'A')
    req(input$subscenario == 'Ucoverage')
    
    fixedRow(
      column(3, numericInput(ns(paste0('expected_mean', input$scenario, input$subscenario)), 'True Value', value = NA)),
      column(3, numericInput(ns(paste0('U', input$scenario, input$subscenario)), 'U', value = NA)),
      column(3, numericInput(ns(paste0('coverage', input$scenario, input$subscenario)), 'Coverage', value = NA)),
      column(3, actionButton(ns(paste0('generate_bias', input$scenario, input$subscenario)), 'Generate'))
    )
  })
  
  output$A_lowerupper <- renderUI({
    req(input$scenario == 'A')
    req(input$subscenario == 'lowerupper')
    
    fixedRow(
      column(3, numericInput(ns(paste0('expected_mean', input$scenario, input$subscenario)), 'True Value', value = NA)),
      column(2, numericInput(ns(paste0('lower', input$scenario, input$subscenario)), 'Lower value', value = NA)),
      column(2, numericInput(ns(paste0('upper', input$scenario, input$subscenario)), 'Upper value', value = NA)),
      column(2, numericInput(ns(paste0('coverage', input$scenario, input$subscenario)), 'Coverage percentage', value = NA)),
      column(3, actionButton(ns(paste0('generate_bias', input$scenario, input$subscenario)), 'Generate'))
    )
  })
  
  output$BC <- renderUI({
    req(input$scenario == 'B or C')
    
    fixedRow(
      column(3, numericInput(ns(paste0('expected_mean', input$scenario, input$subscenario)), 'True Value', value = NA)),
      column(3, numericInput(ns(paste0('sd_rm', input$scenario, input$subscenario)), 'SD RM', value = NA)),
      column(3, numericInput(ns(paste0('nlab', input$scenario, input$subscenario)), 'Nº Labs', value = NA)),
      column(3, actionButton(ns(paste0('generate_bias', input$scenario, input$subscenario)), 'Generate'))
    )
  })
  
  output$DE <- renderUI({
    req(input$scenario == 'D or E')
    
    fixedRow(
      column(3, numericInput(ns(paste0('expected_mean', input$scenario, input$subscenario)), 'True Value', value = NA)),
      column(3, actionButton(ns(paste0('generate_bias', input$scenario, input$subscenario)), 'Generate'))
    )
  })
  
  # Calculate bias intervals
  
  bias_interval_calc <- eventReactive(input[[paste0('generate_bias', input$scenario, input$subscenario)]], {

    req(precision_results())
    
    scenario_dict <- list(
      'A' = 'A',
      'B or C' = 'B',
      'D or E' = 'D'
    )

    s <- calculate_bias_interval(scenario = scenario_dict[[input$scenario]],
                                 nrun = precision_results()$aov_info$k,
                                 nrep = precision_results()$aov_info$n0,
                                 SWL = precision_results()$aov_info$SWL,
                                 SR = precision_results()$aov_info$SR,
                                 nsamples = 1,
                                 expected_mean = input[[paste0('expected_mean', input$scenario, input$subscenario)]],
                                 user_mean = precision_results()$aov_info$mean,
                                 subscenario = input$subscenario,
                                 u = input[[paste0('u', input$scenario, input$subscenario)]],
                                 U = input[[paste0('U', input$scenario, input$subscenario)]],
                                 coverage = input[[paste0('coverage', input$scenario, input$subscenario)]],
                                 k = input[[paste0('k', input$scenario, input$subscenario)]],
                                 lower = input[[paste0('lower', input$scenario, input$subscenario)]],
                                 upper = input[[paste0('upper', input$scenario, input$subscenario)]],
                                 sd_rm = input[[paste0('sd_rm', input$scenario, input$subscenario)]],
                                 nlab = input[[paste0('nlab', input$scenario, input$subscenario)]]
    )

    return(s)
  })
  
  # Make bias outputs and save into object
  
  bias_outputs_results <- eventReactive(bias_interval_calc(), {
    req(bias_interval_calc())
    
    bias_outputs(bias_interval_calc())
  })
  
  # Outputs Bias
  
  output$bias_df <- renderTable({
    req(bias_outputs_results())
    
    bias_outputs_results()$df
  }, striped = TRUE, bordered = TRUE, spacing = 'm', width = '100%', align = 'c')
  
  output$plot_bias <- renderPlot({
    req(bias_outputs_results())
    
    bias_outputs_results()$plot+
      theme(text = element_text(size = 22))
  })
  
  observeEvent(input$generate_pdf, {
    
    # create temp file
    tmp <- tempfile(fileext = '.pdf', tmpdir = 'tmp')
    # render markdown
    rmarkdown::render('markdown/bias_and_precision.Rmd', output_format = 'pdf_document', output_file = tmp, output_dir = 'tmp')
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

