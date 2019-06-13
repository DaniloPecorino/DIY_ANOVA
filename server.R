library(shiny)
library(shinymeta)
library(shinydashboard)
library(shinyWidgets)
library(htmltools)
library(dplyr)
library(tidyr)
library(rlang)
library(DT)
library(GAD)
library(multcomp)
library(multcompView)
library(lsmeans)
library(highcharter)

format_c <- function(x, digits = 3) {
  y <- formatC(x, digits)
  ifelse(is.na(y), "", y)
}

shinyServer(function(input, output, session) {
  
  inData <- reactive({
    req(input$file1)
    
    read.csv(
      input$file1$datapath,
      header = input$header,
      sep = input$sep,
      quote = input$quote
    )
  })
  
  # Define selectors for predictors and response
  output$response_selector <- renderUI({
    req(inData())
    
    pickerInput(
      inputId = "selected_response",
      "Choose the response variable",
      choices = colnames(inData()),
      selected = NULL,
      multiple = FALSE,
      options = list(
        'actions-box' = TRUE,
        title = "Click here",
        'deselect-all-text' = "None",
        'select-all-text' = "All"
      ),
      width = '80%'
    )
    
  })
  
  output$predictors_selector <- renderUI({
    req(input$selected_response)
    
    pickerInput(
      inputId = "selected_predictors",
      "Choose the categorical predictors",
      choices = setdiff(names(inData()), input$selected_response),
      selected = NULL,
      multiple = TRUE,
      options = list(
        'actions-box' = TRUE,
        title = "Click here",
        'deselect-all-text' = "None",
        'select-all-text' = "All"
      ),
      width = '80%'
    )
    
  })
  
  # Define predictors
  predictors <- reactive(input$selected_predictors)
  
  # Define response
  response <- reactive(input$selected_response)
  
  # Draw data table
  output$uploaded_data <- DT::renderDataTable({
    req(inData())
    
    data_table <- DT::datatable(inData())
    
    if (length(predictors())) {
      data_table <- formatStyle(
        data_table, 
        columns = predictors(),
        backgroundColor = "#00808030"
      )
    }
    
    if (length(response())) {
      data_table <- formatStyle(
        data_table, 
        columns = response(),
        backgroundColor = "#fa807230"
      )
    }
    
    data_table
  })
  
  
  ### SECOND TAB
  
  # QQ plot
  output$QQ_plot <- metaRender2(renderHighchart, {
    req(inData(), response())
    
    metaExpr({
      qqPlot <- qqnorm(!!response_transf(), plot = F)
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
    
  })
  
  # Histogram
  output$hist <- metaRender2(renderHighchart, {
    req(response_transf())
    req(sum(is.na(response_transf())) <= 1)
    
    metaExpr({
      y <- !!response_transf()
      h <- hist(y, plot = F)
      d <- diff(h$breaks)[1]
      
      hx <- data.frame(
        x = h$mids,
        y = h$counts, 
        name = sprintf(
          "(%s, %s]",
          h$mids - d / 2,
          h$mids + d / 2
        ),
        color = "#6fcb9f80"
      )
      
      highchart() %>%
        hc_add_series(
          data = list_parse(hx),
          type = "column",
          groupPadding = 0,
          pointPadding =  0
        ) %>%
        hc_tooltip(formatter =
                     JS("function() { return  this.point.name + '<br/>' + this.y; }")) %>%
        hc_xAxis(title = list(text = response())) %>%
        hc_yAxis(title = list(text = "Frequency")) %>%
        hc_title(text = paste0("Histogram of ", response())) %>%
        hc_legend(enabled = FALSE)
    })
  })
  
  
  # Normality test
  # Note: normal_test_transf has test on the transformed data
  normal_test <- metaReactive2({
    req(inData(), response())
    
    metaExpr({
      ks.test(
        !!response_vector(), 
        pnorm, 
        mean(!!response_vector(), na.rm = T), 
        sd(!!response_vector(), na.rm = T)
      )
    })
  })
  
  output$normal_test_table <- renderUI({
    if (is.null(inData()))
      return(HTML("<p style=' text-align: center;'>Feed me data :Q</p>"))
    
    
    HTML(
      paste0(
        "<p  style='font-size:150%;text-align:center;'><b>",
        response(),
        "<br>",
        normal_test()$method,
        '</b></p><br>',
        "<p  style='font-size:100%;text-align:center;'>",
        names(normal_test()$statistic),
        " statistic: ",
        formatC(normal_test()$statistic),
        '<br> ',
        "p-value: ",
        formatC(normal_test()$p.value),
        "</p>"
      )
    )
    
  })
  
  output$normality_icon <- renderUI({
    icon(
      ifelse(
        normal_test()$p.value > 0.05,
        "thumbs-o-up",
        "thumbs-o-down"
      ),
      "fa-3x"
    )
  })
  
  # Choose the transformation
  # TODO: a box-cox transformation would be nice here...
  output$transformation_selector <- renderUI({
    req(inData())
    
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
    req(response_transf())
    
    if (identical(input$selected_transformation, 'asinroot')) {
      numericInput(
        'den',
        'Denominator',
        value = 100,
        width = '50%'
      )
    }
  })
  
  response_vector <- metaReactive({
    !!inData() %>%
      pull(!!response()) %>%
      as.numeric()
  })
  
  # Calculate the transformed data
  response_transf <- metaReactive2({
    transf <- input$selected_transformation %||% ""
    
    transf_fun <- switch(
      transf,
      sqrt = structure(sqrt, caption = "transformed (square root)"),
      log = structure(log, caption = "transformed (natural logarithm of x + 1)"),
      asinroot = structure(function(x) asin(sqrt(x / input$den)), caption = "transformed (arcsine of square root)"),
      identity
    )
    
    metaExpr({
      !!response_vector() %>%
        (!!transf_fun)()
    })
  })
  
  # Calculate the transformed data normality test
  normal_test_transf <- metaReactive2({
    req(response_transf())
    
    metaExpr({
      ks.test(
        !!response_transf(),
        pnorm,
        mean(!!response_transf(), na.rm = T),
        sd(!!response_transf(), na.rm = T)
      )
    })
  })
  
  
  output$normal_test_transf_table <- renderUI({
    req(normal_test_transf())
    
    HTML(
      paste0(
        "<p  style='font-size:150%;text-align:center;'><b> Transformed ",
        response(),
        "<br>",
        normal_test_transf()$method,
        '</b></p><br>',
        "<p  style='font-size:100%;text-align:center;'>",
        names(normal_test_transf()$statistic),
        " statistic: ",
        formatC(normal_test_transf()$statistic),
        '<br> ',
        "p-value: ",
        formatC(normal_test_transf()$p.value),
        "</p>"
      )
    )
  })
  
  output$trans_normality_icon <- renderUI({
    req(normal_test_transf())
    
    icon(
      ifelse(
        normal_test_transf()$p.value > 0.05,
        "thumbs-o-up",
        "thumbs-o-down"
      ),
      "fa-3x"
    )
  })
  
  output$trans_warning <- renderUI({
    # TODO: this should prob be testing if _new_ NAs were introduced
    if (anyNA(response_transf())) {
      showModal(modalDialog(
        title = "Warning!",
        HTML(
          "This is probably not an appropriate transformation for your response variable.<br>
           This transformation is recommended only when data are ratios or percentages, and only after selecting an appropriate denominator (e.g. 100 in case of a percentage)"
        ),
        easyClose = TRUE
      ))
    } 
  })
  
  
  ### THIRD TAB
  factors_df <- metaReactive2({
    req(inData(), predictors())
    
    metaExpr({
      !!inData() %>%
        select_(.dots = !!predictors()) %>%
        mutate_all(as.factor)
    })
  })
  
  factors_vector <- metaReactive2({
    req(factors_df())
    
    metaExpr({
      !!factors_df() %>%
        tidyr::unite(var) %>%
        pull(var) %>%
        as.factor()
    })
  })
  
  variance_test <- metaReactive2({
    req(response_transf(), factors_df())
    
    
    tryCatch({
      metaExpr({
        bartlett.test(!!response_transf(), !!factors_vector())
      })
    },
    error = function(cond) {
      warning(cond$message)
      NULL
    })
  })
  
  output$variance_test_table <- renderUI({
    if (is.null(inData()))
      return(HTML("<p style=' text-align: center;'>Feed me data :Q</p>"))
    
    req(variance_test())
    
    HTML(
      paste0(
        "<p  style='font-size:150%;text-align:center;'><b>",
        ifelse(
          !identical(response_transf(), identity),
          '',
          'Transformed '
        ),
        response(),
        " ~ ",
        paste0(predictors(), collapse = ":"),
        "<br>",
        variance_test()$method,
        '</b></p><br>',
        "<p  style='font-size:100%;text-align:center;'>",
        names(variance_test()$statistic),
        " statistic: ",
        formatC(variance_test()$statistic),
        '<br> ',
        "p-value: ",
        formatC(variance_test()$p.value),
        "</p>"
      )
    )
  })
  
  output$variance_icon <- renderUI({
    icon(
      ifelse(
        variance_test()$p.value > 0.05,
        "thumbs-o-up",
        "thumbs-o-down"
      ),
      "fa-3x"
    )
  })
  
  # # Define boxplot
  output$contents <- metaRender2(renderHighchart, {
    req(factors_df())
    
    metaExpr({
      by <- !!factors_vector()
      by_categories <- unique(by)
      
      highchart(height = 200 * sqrt(length(by_categories))) %>%
        hc_chart(inverted = T) %>%
        hc_legend(enabled = F) %>%
        hc_add_series_boxplot(
          x = !!response_transf(),
          by = by,
          colorByPoint = FALSE,
          fillColor = "#c0d6e4",
          outliers = FALSE
        ) %>%
        hc_xAxis(categories = by_categories) %>%
        hc_yAxis(title = list(text = !!response())) %>%
        hc_title(text = !!paste0(
          "Boxplot of ", response(),
          " by ", paste(predictors(), collapse = ", ")
        )) %>%
        hc_tooltip(
          pointFormat = "Maximum: {point.high}<br/>
                 Upper quartile: {point.q3}<br/>
                 Median: {point.median}<br/>
                 Lower quartile: {point.q1}<br/>
                 Minimum: {point.low}<br/>"
        )
    })
  })
  
  # Define height
  
  plot_height <- reactive({
    req(factors_df())
    
    by <- factors_df() %>%
      tidyr::unite(var) %>%
      pull(var)
    
    200 * sqrt(length(unique(by)))
  })
  
  # wrap highchartOutput in renderUI
  output$boxplot <- renderUI({
    req(plot_height())
    highchartOutput("contents", height = plot_height(), width = "100%")
  })
  
  output$pseudoreplication <- renderUI({
    if (is.null(variance_test())) {
      showModal(modalDialog(
        title = "Warning!",
        HTML(
          "You are probably reading this message because your model suffers from pseudoreplication (i.e. too few data points per combination of factors). <br>
           Either reduce the complexity of the model (e.g. by removing one factor), or consider switching to a different kind of test, such as mixed modelling."
        ),
        easyClose = TRUE
      ))
    }
  })
  
  ### FOURTH TAB
  
  # Define factors as fixed or random
  
  output$factors_title <- renderUI({
    req(inData())
    
    HTML(
      "<p  style='font-size:150%;text-align:center;'> Define the type of each factor</p>
         <p style='text-align:center;'> Check <a target='_blank' href = 'https://www.ma.utexas.edu/users/mks/statmistakes/fixedvsrandom.html'>here</a> for a little help.</p>"
    )
  })
  
  output$factors_definition <- renderUI({
    req(inData())
    
    lapply(predictors(), function(x) {
      radioButtons(
        inputId = paste0(x, '_def'),
        label = x,
        c(Fixed = 'fixed', Random = 'random')
      )
    })
    
  })
  
  # Define model design
  
  output$factors_relationship <- renderUI({
    req(length(input$selected_predictors) > 1)
    
    HTML(
      "<p  style='font-size:150%;text-align:center;'> For each random factor, choose the nesting factor. Otherwise just leave 'None'</p>
         <p style='text-align:center;'> Check <a target='_blank' href = 'http://www.theanalysisfactor.com/the-difference-between-crossed-and-nested-factors/'>here</a> for a little help.</p>"
    )
    
  })
  
  output$factors_nestedness <- renderUI({
    req(length(predictors()) > 1)
    
    lapply(predictors(), function(x) {
      pickerInput(
        inputId = paste0(x, '_nest'),
        paste0(x, " nested in"),
        choices = c('None', setdiff(predictors(), x)),
        selected = 'None',
        multiple = FALSE,
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
    req(inData())
    actionButton('vai', "Run ANOVA!")
  })
  
  
  model_dat <- metaReactive2({
    req(input$vai, factors_df())
    
    random <- unlist(lapply(predictors(), function(x) {
      if ("random" %in% tolower(input[[paste0(x, '_def')]])) x
    }))
    
    metaExpr({
      !!factors_df() %>%
        mutate_all(as.fixed) %>%
        mutate_at(!!random, as.random) %>%
        bind_cols(
          setNames(tibble(!!response_transf()), !!response())
        )
    })
  })
  
  
  # Perform ANOVA and produce results table
  linear_model <- metaReactive2({
    req(model_dat())
    
    predictor_defs <- lapply(predictors(), function(x) {
      # keep track of what predictor is nested inside this one (if any)
      nested <- input[[paste0(x, '_nest')]]
      nested <- if (identical(nested, "None")) NULL else nested
      list(
        name = x,
        nested = nested,
        # the formula term
        term = if (length(nested)) call("%in%", sym(x), sym(nested)) else sym(x)
      )
    })
    
    is_nested <- vapply(predictor_defs, function(def) length(def$nested) > 0, logical(1))
    terms <- sapply(predictor_defs, "[[", "term")
    
    # construct the right-hand side of the lm formula
    # TODO: run this across Danilo and see if the huerestics here 
    # are at least somewhat sensible (especially for nested factors)
    rhs <- if (length(predictors()) == 1) {
      
      sym(predictors())
      
    } else if (sum(is_nested) == 0) {
      
      #  All crossed factors
      Reduce(function(x, y) call("*", x, y), syms(predictors()))
      
    } else {
      # Case when nested factors are present
      
      nested <- Reduce(function(x, y) call("+", x, y), terms[is_nested])
      crossed <- Reduce(function(x, y) call("*", x, y), terms[!is_nested])
      call("+", crossed, nested)
    }
    
    form <- new_formula(sym(response()), rhs)
    
    metaExpr({
      lm(!!form, data = !!model_dat())
    })
  })
  
  anova_results <- metaReactive2({
    req(linear_model())
    
    tryCatch(
      metaExpr({
        gad(!!linear_model())
      }),
      error = c
    )
  })
  
  
  output$anova_res <- metaRender2(renderDataTable, {
    req(anova_results())
    
    metaExpr({
      !!anova_results() %>%
        mutate_all(format_c, 3) %>%
        datatable(
          rownames = row.names(!!anova_results()),
          options = list(dom = 't', pageLength = 50)
        ) %>%
        formatStyle(
          'Pr(>F)',
          target = 'row',
          backgroundColor = styleInterval(
            c(0, 0.001, 0.01, 0.05),
            c("white", "#cc000060", "#ffc3a060", "#ffc3a060", "white")
          )
        )
    })
  })
  
  output$popup_no_fixed <- renderUI({
    req(anova_results()$message)
    
    showModal(
      modalDialog(
        title = "Warning!",
        tags$p(`text-align` = "center", anova_results()$message),
        easyClose = TRUE
      )
    )
  })
  
  
  # Produce chart
  output$alpha_selector <- renderUI({
    req(anova_results())
    
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
  })
  
  alpha <- reactive({
    req(input$selected_alpha)
    as.numeric(input$selected_alpha)
  })
  
  F_den <- metaReactive2({
    req(anova_results(), linear_model(), alpha())
    
    metaExpr({
      den <-
        vector(mode = "numeric", length = (nrow(!!anova_results()) - 1))
      
      factors_denominators <- estimates(!!linear_model())$f.versus
      row.names(factors_denominators) <-
        gsub("types\\$", "", row.names(factors_denominators))
      factors_denominators[, 1] <-
        gsub("types\\$", "", factors_denominators[, 1])
      factors_denominators[factors_denominators == "No test", 1] <-
        NA
      
      for (i in 1:NROW(factors_denominators)) {
        res <- !!anova_results()
        rows <- row.names(res) == factors_denominators[i]
        den[i] <- res[rows, ]$Df
      }
      
      den
    })
    
  })
  
  F_source <- metaReactive2({
    req(anova_results(), F_den())
    
    metaExpr({
      n <- length(!!F_den())
      fs <- purrr::compact(lapply(seq_len(n), function(i) {
        f <- (!!F_den())[i]
        if (is.na(f)) return(NULL)
        anova <- (!!anova_results())[i, ]
        hq <- qf(!!alpha(), anova$Df, f)
        maximum <- 1.32 * 
          ifelse(anova$"F value" > hq, anova$"F value", hq)
        x <- seq(0, 1.50 * maximum, length.out = 500)
        list(
          hq = hq,
          hf = df(hq, anova$Df, f),
          maximum = maximum,
          x = x,
          F_theory = sapply(x, df, anova$Df, f),
          F_obs = anova$"F value"
        )
      }))
      setNames(fs, row.names(!!anova_results())[-1])
    })
  })
  
  
  
  output$F_chart <- metaRender2(renderUI, {
    req(F_source(), F_den(), anova_results())
    
    metaExpr({
      charts <- lapply(!!F_source(), function(x) {
        Fs <- data.frame(x)
        Fs$group <- as.character(Fs$x < Fs$hq)
        
        hchart(
          Fs, "areaspline",
          hcaes(x = x, y = F_theory, group = group),
          color = c("#ff7f50", "#999999")
        ) %>%
          hc_xAxis(
            plotLines = list(
              list(
                label = list(
                  text = paste0("F test = ", format(
                    x$F_obs, digits = 3
                  )),
                  rotation = 0,
                  textAlign = "left",
                  y = 15
                ),
                color = "#6dc066",
                dashStyle = "ShortDot",
                width = 2,
                value = x$F_obs,
                zIndex = 10
              ),
              list(
                label = list(
                  text = paste0(
                    as.character(!!alpha()),
                    "th quantile = ",
                    format(x$hq, digits = 3)
                  ),
                  rotation = 0,
                  textAlign = "left",
                  y = 30
                ),
                color = "red",
                dashStyle = "ShortDot",
                width = 2,
                value = x$hq,
                zIndex = 10
              )
            ),
            title = list(text = "F ratio"),
            max = max(x$x)
          ) %>%
          hc_yAxis(title = list(text = "Probability")) %>%
          #hc_title(text = names(F_source())[i]) %>%
          hc_legend(FALSE) %>%
          hc_tooltip(
            formatter = JS(
              "function() {
                 return ('F ratio: '+ Highcharts.numberFormat(this.x, 3) +', '+'Probability: '+ Highcharts.numberFormat(this.y, 3))
              }"
            )
          )
      })
      
      hw_grid(charts, rowheight = 400, ncol = 1)
    })
  })
  
  ### FIFTH TAB
  ph_results <- metaReactive2({
    req(linear_model())
    
    metaExpr({
      df_ph <- cbind(!!response_transf(), !!factors_df())
      colnames(df_ph)[1] <- "resp_ph"
      
      model <- lm(resp_ph ~ ., data = df_ph)
      
      ph_f <- colnames(!!factors_df())
      
      leastsquare <-
        lsmeans(model, specs = ph_f, adjust = !!input$adjustment)
      
      cld(
        leastsquare,
        alpha = 0.05,
        Letters = letters,
        adjust = !!input$adjustment
      )
    })
  })
  
  
  output$posthoc_res_table <- metaRender2(renderDataTable, {
    req(ph_results())
    
    metaExpr({
      ph_r_form <- mutate_all(!!ph_results(), format_c, 4)
      
      colnames(ph_r_form)[(length(!!predictors()) + 1):ncol(ph_r_form)] <-
        c("Least squares mean",
          "SE",
          "Df",
          "Lower CL",
          "Upper CL",
          "Group")
      
      
      datatable(
        ph_r_form,
        options = list(pageLength = 100),
        rownames = F
      ) %>% 
        formatStyle(
          "Least squares mean",
          background = styleColorBar(range(
            as.numeric(ph_r_form$"Least squares mean")
          ), 'lightblue'),
          backgroundSize = '98% 88%',
          backgroundRepeat = 'no-repeat',
          backgroundPosition = 'center'
        )
      
    })
  })
  
  
  output$choose_method_title <- renderUI({
    req(linear_model())
    
    HTML(
      "<p  style='font-size:150%;text-align:center;'> Choose the correction method for pairwise comparisons</p>
          <p style='text-align:center;'> Check <a target='_blank' href = 'https://en.wikipedia.org/wiki/Multiple_comparisons_problem'>here</a> for a little help.</p>"
    )
  })
  
  output$adjustment_selector <- renderUI({
    req(linear_model())
    
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
    showModal(
      modalDialog(
        title = "How to",
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
        easyClose = TRUE
      )
    )
  })
  
  
  
  output$report <- downloadHandler(
    "anova_report.zip",
    content = function(out) {
      
      saveRDS(inData(), "inData.rds")
      
      # patch all the reactives!
      patch_calls <- list(
        inData = quote(inData),
        response = quote(response),
        predictors = quote(predictors),
        response_vector = quote(response_vector),
        response_transf = quote(response_transf),
        normal_test_transf = quote(normal_test_transf),
        factors_df = quote(factors_df),
        factors_vector = quote(factors_vector),
        model_dat = quote(model_dat),
        linear_model = quote(linear_model),
        anova_results = quote(anova_results),
        variance_test = quote(variance_test),
        F_den = quote(F_den),
        F_source = quote(F_source),
        ph_results = quote(ph_results)
      )
      
      init <- expandCode(
        {
          library(dplyr)
          library(tidyr)
          library(rlang)
          library(DT)
          library(GAD)
          library(multcomp)
          library(multcompView)
          library(lsmeans)
          library(highcharter)
          inData <- readRDS("inData.rds")
          response <- !!response()
          predictors <- !!predictors()
        },
        patchCalls = patch_calls
      )
      
      normal_test_transf <- expandCode(
        {
          response_vector <- !!response_vector()
          response_transf <- !!response_transf()  
          normal_test_transf <- !!normal_test_transf()
        }, 
        patchCalls = patch_calls
      )
      
      qq_plot <- expandCode(
        !!output$QQ_plot(),
        patchCalls = patch_calls
      )
      
      variance_test <- expandCode(
        {
          factors_df <- !!factors_df()
          factors_vector <- !!factors_vector()
          '# Enter `summary(variance_test)`` in your R console for more details'
          variance_test <- !!variance_test()
        },
        patchCalls = patch_calls
      )
      
      boxplot <- expandCode(
        {
          !!output$contents()
        },
        patchCalls = patch_calls
      )
      
      linear_model <- expandCode(
        {
          model_dat <- !!model_dat()
          linear_model <- !!linear_model()
        },
        patchCalls = patch_calls
      )
      
      anova_results <- expandCode(
        {
          anova_results <- !!anova_results()
        },
        patchCalls = patch_calls
      )
      
      F_chart <- expandCode(
        {
          F_den <- !!F_den()
          F_source <- !!F_source()
          !!output$F_chart()
        },
        patchCalls = patch_calls
      )
      
      posthoc_results <- expandCode(
        {
          format_c <- function(x, digits = 3) {
            y <- formatC(x, digits)
            ifelse(is.na(y), "", y)
          }
          ph_results <- !!ph_results()
          !!output$posthoc_res_table()
        },
        patchCalls = patch_calls
      )
      
      
      buildRmdBundle(
        "anova_report.rmd",
        out,
        vars = list(
          init = init,
          normal_test_transf = normal_test_transf,
          qq_plot = qq_plot,
          variance_test = variance_test,
          boxplot = boxplot,
          linear_model = linear_model,
          anova_results = anova_results,
          F_chart = F_chart,
          posthoc_results = posthoc_results
        ),
        include_files = "inData.rds",
        render_args = list(
          params = list(posthoc = input$adjustment)
        )
      )
      
    }
  )
  
  output$downloadReport <- renderUI({
    if (!is.null(ph_results())) {
      downloadButton("report", "Generate report")
    }
  })
  
})
