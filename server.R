require(shiny)
require(DT)
require(shinyWidgets)
require(GAD)
require(shinydashboard)
require(multcompView)
require(lsmeans)
require(highcharter)
require(htmltools)
source("get_hist_data.R")


brks <- c(0, 0.001, 0.01, 0.05)

clrs <- c("white", "#cc000060", "#ffc3a060", "#ffc3a060", "white")

tempDir <- tempdir()

shinyServer(function(input, output, session) {
  ### FIRST TAB
  
  
  # Load data
  
  inData <- reactive({
    req(input$file1)
    
    inFile <- input$file1
    d <-
      read.csv(
        inFile$datapath,
        header = input$header,
        sep = input$sep,
        quote = input$quote
      )
    d
    
  })
  
  
  # Define selectors for predictors and response
  
  output$response_selector <- renderUI({
    req(inData())
    
    if (is.null(inData())) {
      return(NULL)
    } else{
      pickerInput(
        inputId = "selected_response",
        "Choose the response variable",
        choices = colnames(inData()),
        selected = NULL,
        multiple = F,
        options = list(
          'actions-box' = TRUE,
          title = "Click here",
          'deselect-all-text' = "None",
          'select-all-text' = "All"
        ),
        width = '80%'
      )
      
    }
    
    
    
  })
  
  output$predictors_selector <- renderUI({
    req(input$selected_response)
    
    if (input$selected_response == '') {
      return(NULL)
    } else{
      pickerInput(
        inputId = "selected_predictors",
        "Choose the categorical predictors",
        choices = colnames(inData())[!colnames(inData()) %in% input$selected_response],
        selected = NULL,
        multiple = T,
        options = list(
          'actions-box' = TRUE,
          title = "Click here",
          'deselect-all-text' = "None",
          'select-all-text' = "All"
        ),
        width = '80%'
      )
    }
    
    
    
  })
  
  
  # Define predictors
  
  predictors <- reactive({
    # req(input$selected_predictors)
    if (is.null(input$selected_predictors))
      NULL
    
    inPred <- input$selected_predictors
    
    inPred
    
  })
  
  # Define response
  
  response <- reactive({
    # req(input$selected_response)
    
    if (is.null(input$selected_response))
      NULL
    
    inResp <- input$selected_response
    
    inResp
    
  })
  
  
  # Draw data table
  
  output$uploaded_data <- DT::renderDataTable({
    if (!is.null(inData())) {
      data_table <- DT::datatable(inData())
      
      if (is.null(predictors()) & is.null(response())) {
        data_table
      } else{
        # data_table %>% formatStyle(columns = which(colnames(inData()) %in% c(predictors(), response())), backgroundColor = "#00808030")
        data_table %>%
          formatStyle(columns = which(colnames(inData()) %in% c(predictors())),
                      backgroundColor = "#00808030") %>%
          formatStyle(columns = which(colnames(inData()) %in% c(response())),
                      backgroundColor = "#fa807230")
      }
    } else{
      NULL
    }
    
  })
  
  
  
  ### SECOND TAB
  
  # QQ plot
  
  
  output$QQ_plot <- renderHighchart({
    req(inData(), response())
    
    
    resp_col <- which(colnames(inData()) == response())
    
    if (input$selected_transformation == 'none') {
      dataPlot <- inData()[, resp_col]
    } else{
      dataPlot <- transformedData()
    }
    
    if (is.null(dataPlot))
      return(NULL)
    
    if (sum(is.na(dataPlot)) > 1)
      return(NULL)
    
    qqPlot <- qqnorm(dataPlot, plot = F)
    qqPlot <- as.data.frame(qqPlot)
    qqPlot <- qqPlot[order(qqPlot$x), ]
    mod <- lm(y ~ x, qqPlot)
    qqPlot$line <-
      predict(mod, newdata = data.frame(x = sort(qqPlot$x)))
    
    highchart() %>%
      hc_add_series(
        data = qqPlot,
        type = "scatter",
        hcaes(x = x, y = y),
        color = "#6fcb9f",
        name = "qqplot",
        zIndex = 1
      ) %>%
      hc_tooltip(
        formatter = JS(
          "function(){
                     return ('x: '+ Highcharts.numberFormat(this.x, 2) +', '+'y: '+ Highcharts.numberFormat(this.y, 2))
                  }"
        )
      ) %>%
      hc_add_series(
        data = qqPlot,
        type = "spline",
        hcaes(x = x,
              y = line),
        color = "#ff7f50",
        dashStyle = "ShortDot",
        marker = list(enabled = F)
      ) %>%
      hc_xAxis(title = list(text = "Theoretical quantiles")) %>%
      hc_yAxis(title = list(text = "Sample quantiles")) %>%
      hc_title(text = "QQ plot") %>%
      hc_legend(enabled = FALSE)
    
    
  })
  
  # Histogram
  
  
  output$hist <- renderHighchart({
    resp_col <- which(colnames(inData()) == response())
    
    if (input$selected_transformation == 'none') {
      dataPlot <- inData()[, resp_col]
    } else{
      dataPlot <- transformedData()
    }
    
    if (is.null(dataPlot))
      return(NULL)
    
    if (sum(is.na(dataPlot)) > 1)
      return(NULL)
    
    h <- hist(dataPlot, plot = F)
    hx <- get_hist_data(h)
    hx$color <- "#6fcb9f80"
    
    highchart() %>%
      hc_add_series(
        data = list_parse(hx),
        type = "column"
        ,
        groupPadding = 0
        ,
        pointPadding =  0
      ) %>%
      hc_tooltip(formatter =
                   JS("function() { return  this.point.name + '<br/>' + this.y; }")) %>%
      hc_xAxis(title = list(text = colnames(inData())[resp_col])) %>%
      hc_yAxis(title = list(text = "Frequency")) %>%
      hc_title(text = paste0("Histogram of ", colnames(inData())[resp_col])) %>%
      hc_legend(enabled = FALSE)
    
  })
  
  
  # Normality test
  
  
  test_results <- reactive({
    req(inData(), response())
    
    if (is.null(inData()))
      return(NULL)
    
    resp_col <- which(colnames(inData()) == response())
    
    ks.test(inData()[, resp_col],
            pnorm,
            mean(inData()[, resp_col], na.rm = T),
            sd(inData()[, resp_col], na.rm = T))
    
    
  })
  
  
  
  output$test_results_table <- renderUI({
    if (is.null(inData()))
      return(HTML("<p style=' text-align: center;'>Feed me data :Q</p>"))
    
    
    HTML(
      paste0(
        "<p  style='font-size:150%;text-align:center;'><b>",
        response(),
        "<br>",
        test_results()$method,
        '</b></p><br>',
        "<p  style='font-size:100%;text-align:center;'>",
        names(test_results()$statistic),
        " statistic: ",
        formatC(test_results()$statistic),
        '<br> ',
        "p-value: ",
        formatC(test_results()$p.value),
        "</p>"
      )
    )
    
  })
  
  
  output$normality_icon <- renderUI({
    icon(ifelse(
      test_results()$p.value > 0.05,
      "thumbs-o-up",
      "thumbs-o-down"
    ),
    "fa-3x")
    
  })
  
  
  # Transformations
  
  # Choose the transformation
  
  output$transformation_selector <- renderUI({
    if (is.null(inData()))
      return(NULL)
    
    pickerInput(
      inputId = "selected_transformation",
      "Choose a transformation",
      choices = list(
        None = 'none',
        'Square root' = 'sqrt',
        'Natural log + 1' = 'log',
        "Arcsine of square root" = 'asinroot'
      ),
      selected = 'none',
      multiple = F,
      options = list('actions-box' = TRUE, title = "Click here"),
      width = '80%'
    )
    
    
  })
  
  # Choose the denominator for the asine-square root transformation
  
  output$denominator_selector <- renderUI({
    if (input$selected_transformation == '') {
      return(NULL)
    } else if (input$selected_transformation != 'asinroot') {
      return(NULL)
    } else{
      numericInput('den',
                   'Denominator',
                   value = 100,
                   width = '50%')
    }
    
  })
  
  # Calculate the transformed data
  
  transformedData <- reactive({
    resp_col <- which(colnames(inData()) == response())
    if (input$selected_transformation == 'none') {
      as.numeric(inData()[, resp_col])
    } else if (input$selected_transformation == 'sqrt') {
      sqrt(as.numeric(inData()[, resp_col]))
    } else if (input$selected_transformation == 'log') {
      log(as.numeric(inData()[, resp_col] + 1))
    } else if (input$selected_transformation == 'asinroot') {
      asin(sqrt(as.numeric(inData()[, resp_col]) / input$den))
    }
    
    
  })
  
  
  # Calculate the transformed data normality test
  
  
  trans_test_results <- reactive({
    if (input$selected_transformation == 'none') {
      return(NULL)
    }
    
    if (is.na(transformedData()[1]))
      return(NULL)
    
    ks.test(
      transformedData(),
      pnorm,
      mean(transformedData(), na.rm = T),
      sd(transformedData(), na.rm = T)
    )
    
  })
  
  
  output$trans_test_results_table <- renderUI({
    if (input$selected_transformation == 'none') {
      return(NULL)
    } else{
      HTML(
        paste0(
          "<p  style='font-size:150%;text-align:center;'><b> Transformed ",
          response(),
          "<br>",
          trans_test_results()$method,
          '</b></p><br>',
          "<p  style='font-size:100%;text-align:center;'>",
          names(trans_test_results()$statistic),
          " statistic: ",
          formatC(trans_test_results()$statistic),
          '<br> ',
          "p-value: ",
          formatC(trans_test_results()$p.value),
          "</p>"
        )
      )
    }
    
  })
  
  
  output$trans_normality_icon <- renderUI({
    if (input$selected_transformation == '') {
      return(NULL)
    } else{
      icon(
        ifelse(
          trans_test_results()$p.value > 0.05,
          "thumbs-o-up",
          "thumbs-o-down"
        ),
        "fa-3x"
      )
    }
  })
  
  output$trans_warning <- renderUI({
    if (sum(is.na(transformedData())) > 1) {
      showModal(modalDialog(
        title = "Warning!",
        HTML(
          "This is probably not an appropriate transformation for your response variable.<br>
           This transformation is recommended only when data are ratios or percentages, and only after selecting an appropriate denominator (e.g. 100 in case of a percentage)"
        ),
        easyClose = TRUE
      ))
      
    } else{
      NULL
    }
    
    
  })
  
  
  ### THIRD TAB
  
  
  
  factors_df <- reactive({
    req(inData(), input$selected_predictors)
    
    if (is.null(input$selected_predictors)) {
      return(NULL)
    } else{
      df <-
        data.frame(inData()[, colnames(inData()) %in% input$selected_predictors])
      colnames(df) <- input$selected_predictors
      df <- lapply(df, FUN = as.factor)
      df <- data.frame(df)
      df
      
    }
    
  })
  
  
  
  homo_test_results <- reactive({
    req(transformedData(), factors_df())
    
    
    if (is.null(transformedData()))
      return(NULL)
    
    if (is.null(factors_df()))
      return(NULL)
    
    factors_formula <- vector(mode = "character")
    for (i in 1:nrow(factors_df())) {
      factors_formula[i] <- paste0(factors_df()[i, ], collapse = "")
    }
    factors_formula <- as.factor(factors_formula)
    
    
    tryCatch({
      bartlett.test(transformedData() ~ factors_formula)
    },
    error = function(cond) {
      message("Bla bla bla")
      return({
        NULL
      })
    })
    
  })
  
  
  output$homo_test_results_table <- renderUI({
    req(inData())
    
    if (is.null(inData()))
      return(HTML("<p style=' text-align: center;'>Feed me data :Q</p>"))
    
    if (is.null(homo_test_results())) {
      return(NULL)
    } else{
      HTML(
        paste0(
          "<p  style='font-size:150%;text-align:center;'><b>",
          ifelse(
            input$selected_transformation == 'none',
            '',
            'Transformed '
          ),
          response(),
          " ~ ",
          paste0(predictors(), collapse = ":"),
          "<br>",
          homo_test_results()$method,
          '</b></p><br>',
          "<p  style='font-size:100%;text-align:center;'>",
          names(homo_test_results()$statistic),
          " statistic: ",
          formatC(homo_test_results()$statistic),
          '<br> ',
          "p-value: ",
          formatC(homo_test_results()$p.value),
          "</p>"
        )
      )
    }
    
  })
  
  output$homo_icon <- renderUI({
    icon(
      ifelse(
        homo_test_results()$p.value > 0.05,
        "thumbs-o-up",
        "thumbs-o-down"
      ),
      "fa-3x"
    )
    
  })
  
  
  
  
  # # Define boxplot
  
  output$contents <- renderHighchart({
    req(inData(), predictors())
    
    resp_col <- which(colnames(inData()) == response())
    
    Data <- inData()[, predictors()]
    Data <- data.frame(Data)
    for (i in 1:ncol(Data)) {
      Data[, i] <- as.character(Data[, i])
    }
    Data$var <- NA
    for (i in 1:nrow(Data)) {
      Data[i, ]$var <- paste0(Data[i, 1:(ncol(Data) - 1)], collapse = "_")
    }
    x <- cbind(transformedData(), Data)
    x$color <- colorize(x$var)
    height <- 200 * sqrt(length(unique(x$var)))
    
    highchart(height = height) %>%
      hc_chart(inverted = T) %>%
      hc_legend(enabled = F) %>%
      hc_add_series_boxplot(
        x = x[, 1],
        by = x$var,
        colorByPoint = FALSE,
        fillColor = "#c0d6e4",
        outliers = F
      ) %>%
      hc_xAxis(categories = unique(x$var)) %>%
      hc_yAxis(title = list(text = colnames(inData())[resp_col])) %>%
      hc_title(text = paste0(
        "Boxplot of ",
        colnames(inData())[resp_col],
        " by ",
        paste(predictors(), collapse = ", ")
      )) %>%
      hc_tooltip(
        pointFormat = "Maximum: {point.high}<br/>
                 Upper quartile: {point.q3}<br/>
                 Median: {point.median}<br/>
                 Lower quartile: {point.q1}<br/>
                 Minimum: {point.low}<br/>"
      )
    
    
    
  })
  
  # Define height
  
  plot_height <- reactive({
    req(inData(), predictors())
    Data <- inData()[, predictors()]
    Data <- data.frame(Data)
    Data$var <- NA
    for (i in 1:nrow(Data)) {
      Data[i, ]$var <- paste0(Data[i, 1:(ncol(Data) - 1)], collapse = "_")
    }
    x <- cbind(transformedData(), Data)
    x$color <- colorize(x$var)
    200 * sqrt(length(unique(x$var)))
    
    
  })
  
  # wrap highchartOutput in renderUI
  output$boxplot <- renderUI({
    req(plot_height())
    highchartOutput("contents", height = plot_height(), width = "100%")
  })
  
  
  
  output$pseudoreplication <- renderUI({
    if (is.null(homo_test_results())) {
      showModal(modalDialog(
        title = "Warning!",
        HTML(
          "You are probably reading this message because your model suffers from pseudoreplication (i.e. too few data points per combination of factors). <br>
           Either reduce the complexity of the model (e.g. by removing one factor), or consider switching to a different kind of test, such as mixed modelling."
        ),
        easyClose = TRUE
      ))
      
      # HTML("<font color = 'red'>Warning! You are probably reading this message because your model suffers from pseudoreplication (i.e. too few data points per combination of factors). <br>
      #      Either reduce the complexity of the model (e.g. by removing one factor), or consider switching to a different kind of test, such as mixed modelling.</font>")
    } else{
      NULL
    }
    
  })
  
  ### FOURTH TAB
  
  # Define factors as fixed or random
  
  output$factors_title <- renderUI({
    if (is.null(inData()))
      return(NULL)
    
    HTML(
      "<p  style='font-size:150%;text-align:center;'> Define the type of each factor</p>
         <p style='text-align:center;'> Check <a target='_blank' href = 'https://www.ma.utexas.edu/users/mks/statmistakes/fixedvsrandom.html'>here</a> for a little help.</p>"
    )
    
  })
  
  output$factors_definition <- renderUI({
    if (is.null(inData()))
      return(NULL)
    
    number_of_factors <-
      as.integer(length(input$selected_predictors))
    lapply(1:number_of_factors, function(i) {
      radioButtons(
        inputId = paste0(input$selected_predictors[i], '_def'),
        label = input$selected_predictors[i],
        c(Fixed = 'fixed',
          Random = 'random')
      )
    })
    
  })
  
  
  
  
  # Define model design
  
  output$factors_relationship <- renderUI({
    if (length(input$selected_predictors) <= 1) {
      return(NULL)
    } else{
      HTML(
        "<p  style='font-size:150%;text-align:center;'> For each random factor, choose the nesting factor. Otherwise just leave 'None'</p>
         <p style='text-align:center;'> Check <a target='_blank' href = 'http://www.theanalysisfactor.com/the-difference-between-crossed-and-nested-factors/'>here</a> for a little help.</p>"
      )
    }
    
  })
  
  
  output$factors_nestedness <- renderUI({
    if (length(input$selected_predictors) <= 1)
      return(NULL)
    
    number_of_factors <-
      as.integer(length(input$selected_predictors))
    lapply(1:number_of_factors, function(i) {
      pickerInput(
        inputId = paste0(input$selected_predictors[i], '_nest'),
        paste0(input$selected_predictors[i], " nested in"),
        choices = c('None', predictors()[predictors() != predictors()[i]]),
        selected = 'None',
        multiple = F,
        options = list(
          'actions-box' = TRUE,
          title = "Click here",
          'deselect-all-text' = "None",
          'select-all-text' = "All"
        ),
        width = '80%'
      )
    })
    
  })
  
  
  output$anova_button <- renderUI({
    if (is.null(inData()))
      return(NULL)
    
    actionButton('vai', "Run ANOVA!")
    
  })
  
  
  
  
  # Perform ANOVA and produce results table
  
  linear_model <- eventReactive(input$vai, {
    req(factors_df())
    
    if (is.null(factors_df()))
      return(NULL)
    
    if (is.null(input$selected_predictors))
      return(NULL)
    
    
    types <- vector(mode = "list", length = ncol(factors_df()))
    names(types) <- colnames(factors_df())
    
    factors_definitions <-
      vector(mode = "character", length = ncol(factors_df()))
    
    for (i in 1:ncol(factors_df())) {
      if (ncol(factors_df()) == 1) {
        factors_definitions[i] <-
          input[[paste0(input$selected_predictors[i], '_def')]]
        types[[i]] <- as.fixed(factors_df()[, i])
      } else{
        factors_definitions[i] <-
          input[[paste0(input$selected_predictors[i], '_nest')]]
        
        if (input[[paste0(input$selected_predictors[i], '_def')]] == 'fixed') {
          types[[i]] <- as.fixed(factors_df()[, i])
        } else if (input[[paste0(input$selected_predictors[i], '_def')]] == 'random') {
          types[[i]] <- as.random(factors_df()[, i])
        }
        
      }
      
    }
    
    
    
    #  Define scenarios
    
    
    if (length(factors_definitions) == 1) {
      #  Just one factor
      
      formula_lh <- paste0("transformedData() ~ ")
      
      formula_lh <-
        paste0(formula_lh, paste0("types$", names(types)[1]))
      
    } else{
      #  More than one factor
      
      if (sum(factors_definitions == 'None') == length(factors_definitions)) {
        #  All crossed factors
        
        formula_lh <- paste0("transformedData() ~ ")
        
        rh <-
          paste0(paste0("types$", names(types)), collapse = " * ")
        formula_lh <- paste0(formula_lh, rh)
        
      } else{
        # Case when nested factors are present
        
        formula_lh <- paste0("transformedData() ~ ")
        rh_crossed <- vector(mode = "character")
        rh_nested <- vector(mode = "character")
        
        for (m in 1:length(factors_definitions)) {
          if (factors_definitions[m] == 'None') {
            rh_crossed <- c(rh_crossed, paste0("types$", names(types)[m]))
            
          } else{
            rh_nested <-
              c(
                rh_nested,
                paste0(
                  "types$",
                  names(types)[m],
                  "%in%types$",
                  factors_definitions[m]
                )
              )
            
            if (factors_definitions[which(names(types) == factors_definitions[m])] == 'None') {
              rh_nested
            } else{
              rh_nested[length(rh_nested)] <-
                paste0(rh_nested[length(rh_nested)], "%in%types$", factors_definitions[which(names(types) ==
                                                                                               factors_definitions[m])])
              
            }
            
          }
          
        }
        
        
        interactions <- paste0(rh_crossed, collapse = " * ")
        
        
        for (n in 1:length(rh_crossed)) {
          elements_to_remove <-
            grep(strsplit(rh_crossed[n], "\\$")[[1]][2], rh_nested)
          
          to_interact <-
            ifelse(length(elements_to_remove) == 0,
                   rh_nested,
                   rh_nested[-elements_to_remove])
          
          if (!is.na(to_interact[1])) {
            interactions <-
              c(interactions, paste0(paste0(
                rh_crossed[n], " * ", to_interact
              ), collapse = " + "))
            
          }
          
        }
        
        
        rh_crossed <- paste0(rh_crossed, collapse = " + ")
        rh_nested <- paste0(rh_nested, collapse = " + ")
        interactions <- paste0(interactions, collapse = " + ")
        
        
        
        rh <-
          paste0(rh_crossed, " + ", rh_nested, " + ", interactions)
        formula_lh <- paste0(formula_lh, rh)
        
        
      }
      
    }
    
    
    
    
    formula_anova <- as.formula(formula_lh)
    
    saveRDS(factors_definitions,
            paste0(tempDir, "/factors_definitions.rds"))
    
    
    lm(formula_anova)
    
    
    
    
  })
  
  
  
  anova_results <- reactive({
    req(linear_model())
    
    # if(is.null(linear_model()))
    #   return(NULL)
    
    res <- tryCatch(
      gad(linear_model()),
      error = c
    )
    
    
    if (!is.null(res)) {
      attributes(res)$row.names[1:(length(attributes(res)$row.names) - 1)] <-
        gsub("types\\$", "", attributes(res)$row.names[1:(length(attributes(res)$row.names) -
                                                            1)])
      attributes(res)$heading[2] <-
        gsub("transformedData\\(\\)",
             as.character(response()),
             attributes(res)$heading[2])
      
    }
    res
  })
  
  
  output$anova_res <- DT::renderDataTable({
    req(anova_results())
    
    res_form <- anova_results()
    
    for (i in 1:ncol(anova_results())) {
      res_form[, i] <- formatC(anova_results()[, i], 3)
      
      res_form[is.na(anova_results()[, i]), i] <- ""
      
    }
    
    
    datatable(res_form, options = list(dom = 't', pageLength = 50)) %>%
      formatStyle('Pr(>F)',
                  target = 'row',
                  backgroundColor = styleInterval(brks, clrs))
    
    
  })
  
  
  
  
  output$popup_no_fixed <- renderUI({
    if (is.null(anova_results()$message))
      return(NULL)
    
    showModal(modalDialog(title = "Warning!",
                          HTML(
                            paste0("<p text-aling=center;>", anova_results()$message, "</p>")
                          ),
                          easyClose = TRUE))
  })
  
  
  
  # Produce chart
  
  output$alpha_selector <- renderUI({
    req(anova_results())
    
    # if(is.null(anova_results())){
    #   return(NULL)
    # }else{
    
    radioButtons(
      'selected_alpha',
      'Choose alpha',
      c(
        '0.05' = .95,
        '0.01' = .99,
        '0.001' = .999
      ),
      selected = .95
    )
    
    # }
    
  })
  
  alpha <- reactive({
    req(input$selected_alpha)
    
    # if(is.null(input$selected_alpha)){
    #   0.05
    # }else{
    as.numeric(input$selected_alpha)
    # }
    
  })
  
  
  
  
  F_den <- reactive({
    req(anova_results(), linear_model(), alpha())
    
    den <-
      vector(mode = "numeric", length = (nrow(anova_results()) - 1))
    
    
    factors_denominators <- estimates(linear_model())$f.versus
    row.names(factors_denominators) <-
      gsub("types\\$", "", row.names(factors_denominators))
    factors_denominators[, 1] <-
      gsub("types\\$", "", factors_denominators[, 1])
    factors_denominators[factors_denominators == "No test", 1] <-
      NA
    
    
    for (i in 1:NROW(factors_denominators)) {
      rows <- row.names(anova_results()) == factors_denominators[i]
      den[i] <- anova_results()[rows, ]$Df
      
    }
    
    den
    
  })
  
  
  
  
  
  F_source <- reactive({
    req(anova_results(), F_den())
    
    
    #  Define empty list to store x and F(x) values
    
    fs <- vector(mode = "list", length = (NROW(anova_results()) - 1))
    
    #  For each source of variability, calculate x min and x max and respective F values, and store them in the
    #  appropriate element of the list
    
    for (f in 1:length(fs)) {
      if (is.na(F_den()[f])) {
        fs[[f]] <- next
      } else{
        fs[[f]]$hq <- qf(alpha(), anova_results()[f, ]$Df, F_den()[f])
        fs[[f]]$hf <-
          df(fs[[f]]$hq, anova_results()[f, ]$Df, F_den()[f])
        
        maximum <-
          ifelse(
            anova_results()[f, ]$"F value" > fs[[f]]$hq,
            1.10 * anova_results()[f, ]$"F value" ,
            1.10 * fs[[f]]$hq
          )
        maximum <- 1.20 * maximum
        
        names(fs)[f] <- attributes(anova_results())$row.names[f]
        fs[[f]]$x <- seq(0, 1.50 * maximum, length.out = 500)
        
        
        for (i in 1:length(fs[[f]]$x)) {
          fs[[f]]$F[i] <- df(fs[[f]]$x[i], anova_results()[f, ]$Df, F_den()[f])
        }
      }
    }
    fs
  })
  
  
  
  output$F_chart <- renderUI({
    req(F_source(), F_den(), anova_results())
    
    # saveRDS(F_source(), "report/F_source.rds")
    # saveRDS(F_den(), "report/F_den.rds")
    # saveRDS(alpha(), "report/alpha.rds")
    
    charts <- vector(mode = "list")
    for (i in 1:length(F_source())) {
      if (is.na(F_den()[i])) {
        next
      } else{
        Fs <- data.frame(F_source()[[i]])
        Fs$group <- as.character(Fs$x < Fs$hq)
        
        charts[[length(charts) + 1]] <-
          Fs %>%
          hchart("areaspline",
                 hcaes(
                   x = x,
                   y = F,
                   group = group
                 ),
                 color = c("#ff7f50", "#999999"))  %>%
          hc_xAxis(
            plotLines = list(
              list(
                label = list(
                  text = paste0("F test = ", format(
                    anova_results()[i, ]$"F value", digits = 3
                  )),
                  rotation = 0,
                  textAlign = "left",
                  y = 15
                ),
                color = "#6dc066",
                dashStyle = "ShortDot",
                width = 2,
                value = anova_results()[i, ]$"F value",
                zIndex = 10
              ),
              list(
                label = list(
                  text = paste0(
                    as.character(alpha()),
                    "th quantile = ",
                    format(F_source()[[i]]$hq, digits = 3)
                  ),
                  rotation = 0,
                  textAlign = "left",
                  y = 30
                ),
                color = "red",
                dashStyle = "ShortDot",
                width = 2,
                value = F_source()[[i]]$hq,
                zIndex = 10
              )
            ),
            title = list(text = "F ratio"),
            max = max(Fs$x)
          ) %>%
          hc_yAxis(title = list(text = "Probability")) %>%
          hc_title(text = names(F_source())[i]) %>%
          hc_legend(FALSE) %>%
          hc_tooltip(
            formatter = JS(
              "function(){
                          return ('F ratio: '+ Highcharts.numberFormat(this.x, 3) +', '+'Probability: '+ Highcharts.numberFormat(this.y, 3))
       }"
            )
          )
      }
      
    }
    
    hw_grid(charts, rowheight = 400, ncol = 1)
    
  })
  
  
  
  
  ### FIFTH TAB
  
  padjust_method <- reactive({
    input$adjustment
    
  })
  
  ph_results <- reactive({
    if (is.null(linear_model()))
      return(NULL)
    
    df_ph <- cbind(transformedData(), factors_df())
    colnames(df_ph)[1] <- "resp_ph"
    
    model <- lm(resp_ph ~ ., data = df_ph)
    
    
    ph_f <- colnames(factors_df())
    
    leastsquare <-
      lsmeans(model, specs = ph_f, adjust = padjust_method())
    
    ph_r <- cld(
      leastsquare,
      alpha = 0.05,
      Letters = letters,
      adjust = padjust_method()
    )
    
    ph_r
    
  })
  
  
  output$posthoc_res_table <- DT::renderDataTable({
    if (is.null(ph_results()))
      return(NULL)
    
    ph_r_form <- ph_results()
    
    for (i in 1:ncol(ph_results())) {
      ph_r_form[, i] <- formatC(ph_results()[, i], 4)
      
      ph_r_form[is.na(ph_results()[, i]), i] <- ""
      
    }
    
    colnames(ph_r_form)[(length(predictors()) + 1):ncol(ph_r_form)] <-
      c("Least squares mean",
        "SE",
        "Df",
        "Lower CL",
        "Upper CL",
        "Group")
    
    
    datatable(ph_r_form,
              options = list(pageLength = 100),
              rownames = F) %>% formatStyle(
                "Least squares mean",
                background = styleColorBar(range(
                  as.numeric(ph_r_form$"Least squares mean")
                ), 'lightblue'),
                backgroundSize = '98% 88%',
                backgroundRepeat = 'no-repeat',
                backgroundPosition = 'center'
              )
    
    
  })
  
  output$choose_method_title <- renderUI({
    if (is.null(linear_model()))
      return(NULL)
    
    HTML(
      "<p  style='font-size:150%;text-align:center;'> Choose the correction method for pairwise comparisons</p>
          <p style='text-align:center;'> Check <a target='_blank' href = 'https://en.wikipedia.org/wiki/Multiple_comparisons_problem'>here</a> for a little help.</p>"
    )
    
  })
  
  output$adjustment_selector <- renderUI({
    if (is.null(linear_model()))
      return(NULL)
    
    radioButtons(
      'adjustment',
      '',
      c(
        None = 'none',
        Holm = 'holm',
        Hochberg = 'hochberg',
        Hommel = 'hommel',
        Tukey = 'tukey',
        Bonferroni = 'bonferroni',
        'False discovery rate' = 'fdr'
      ),
      selected = 'none'
    )
    
  })
  
  
  ### SIXTH TAB
  
  output$downloadSampleDatasets <- downloadHandler(
    filename = function() "sample_datasets.zip",
    content = function(file) {
      zip(
        file, files = dir("sample_datasets", full.names = TRUE)
      )
    },
    contentType = "application/zip"
  )
  
  
  
  observeEvent(input$how_to, {
    showModal(modalDialog(title = "How to",
                          HTML(
                            paste0(
                              "Load a csv dataset by clicking the 'Browse...' button (in case you don't have any just yet, feel free to download and use the sample datasets by clicking the 'Download sample datasets button'.<br>
                   Choose response variable and categorical predictors.<br><br>
                   In the 'Check normality' tab you will see the results of a Kolmogorov-Smirnov normality test. In case of failure, you have the option to trasform your data by choosing an available transformation from the menù.<br><br>
                   In the 'Check homoscedasticity' tab the results of a Bartlett test from homogeneity of variances are shown. In case data do not pass this test, you have the option to click back to the 'Normality test' tab and select a different transformation.<br><br>
                   In the 'Test hypotheses' tab you will run the actual ANOVA. For each factor, define whether it is fixed or random, and then selected (if present) nesting factors. When done click on the 'Run ANOVA!' button. Yay!<br><br>
                   The 'Post hoc tests' tab will give the possibility to visualize the results of pairwise post hoc tests on your data, and to select the adjustment method for the p values.<br><br>
                   A complete report of your analysis can be downloaded from the 'Download results' tab, once you have completed all the steps.<br><br>
                   This app was developed by <b>Danilo Pecorino</b> (danilo.pecorino@gmail.com), who loves to hear from people who like stats :)
                   "
                            )
                          ),
                          easyClose = TRUE))
  })
  
  
  
  output$report <- downloadHandler(
    # For PDF output, change this to "report.pdf"
    filename = "report.html",
    content = function(file) {
      saveRDS(inData(), paste0(tempDir, "/inData.rds"))
      saveRDS(response(), paste0(tempDir, "/response.rds"))
      saveRDS(predictors(), paste0(tempDir, "/predictors.rds"))
      saveRDS(transformedData(),
              paste0(tempDir, "/transformedData.rds"))
      saveRDS(test_results(), paste0(tempDir, "/test_results.rds"))
      saveRDS(trans_test_results(),
              paste0(tempDir, "/trans_test_results.rds"))
      saveRDS(homo_test_results(),
              paste0(tempDir, "/homo_test_results.rds"))
      saveRDS(anova_results(), paste0(tempDir, "/anova_results.rds"))
      saveRDS(linear_model(), paste0(tempDir, "/linear_model.rds"))
      saveRDS(ph_results(), paste0(tempDir, "/ph_results.rds"))
      saveRDS(F_source(), paste0(tempDir, "/F_source.rds"))
      saveRDS(F_den(), paste0(tempDir, "/F_den.rds"))
      saveRDS(alpha(), paste0(tempDir, "/alpha.rds"))
      
      
      # Copy the report file to a temporary directory before processing it, in
      # case we don't have write permissions to the current working dir (which
      # can happen when deployed).
      tempReport <- file.path(tempDir, "anova_report.rmd")
      tempSource <- file.path(tempDir, "get_hist_data.R")
      file.copy("report/anova_report.rmd", tempReport, overwrite = TRUE)
      file.copy("get_hist_data.R", tempSource, overwrite = TRUE)
      # Set up parameters to pass to Rmd document
      params <-
        list(
          transformation = input$selected_transformation,
          posthoc = input$adjustment
        )
      
      # Knit the document, passing in the `params` list, and eval it in a
      # child of the global environment (this isolates the code in the document
      # from the code in this app).
      rmarkdown::render(
        tempReport,
        output_file = file,
        params = params,
        envir = new.env(parent = globalenv())
      )
    }
  )
  
  output$downloadReport <- renderUI({
    if (!is.null(ph_results())) {
      downloadButton("report", "Generate report")
    }
  })
  
})
