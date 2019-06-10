require(shiny)
require(shinyWidgets)
require(shinydashboard)
require(highcharter)
require(dplyr)
require(shinycssloaders)
source("navbarPageWithText.R")

shinyUI(

  navbarPageWithText(#theme = "customStyle.css",
    img(src='bg.jpeg', align = "left"),


             #  Data upload panel
             
             tabPanel("Upload data",
                      tags$style(type = "text/css", ".navbar-brand {padding-top: 0px!important; padding-left: 0px!important;}"),
                      tags$style(type = "text/css", "#normality_icon {text-align: center !important;}"),
                      tags$style(type = "text/css", "#trans_normality_icon {text-align: center !important;}"),
                      tags$style(type = "text/css", "#homo_icon {text-align: center !important;}"),
                      tags$style(type="text/css",
                                 ".shiny-output-error { visibility: hidden; }",
                                 ".shiny-output-error:before { visibility: hidden; }"
                      ),
                      
                      sidebarLayout(
                        sidebarPanel(
                          fileInput('file1', 'Choose CSV File',
                                    accept=c('text/csv', 
                                             'text/comma-separated-values,text/plain', 
                                             '.csv')),
                          tags$hr(),
                          checkboxInput('header', 'Header', TRUE),
                          radioButtons('sep', 'Separator',
                                       c(Comma=',',
                                         Semicolon=';',
                                         Tab='\t'),
                                       ','),
                          radioButtons('quote', 'Quote',
                                       c(None='',
                                         'Double Quote'='"',
                                         'Single Quote'="'"),
                                       '"'),
                          hr(),
                          uiOutput("response_selector"),
                          uiOutput("predictors_selector"),
                          hr(),
                          hr(),
                          fluidRow(downloadButton('downloadSampleDatasets', "Download sample datasets"), actionButton('how_to', "How to & credits"))
                        ),
                        mainPanel(
                        
                          DT::dataTableOutput('uploaded_data')
                        
                        )
                      )
                      
                      ),
             
             #  Check assumptions
                          
             tabPanel("Check normality",
                      sidebarLayout(
                        sidebarPanel(
                         htmlOutput('test_results_table'),
                         htmlOutput('normality_icon'),
                         hr(),
                         uiOutput('transformation_selector'),
                         uiOutput('denominator_selector'),
                         uiOutput('trans_test_results_table'),
                         htmlOutput('trans_normality_icon'),
                         uiOutput('trans_warning')
                        ),
                        mainPanel(
                          highchartOutput("QQ_plot")  %>% withSpinner(color="#6fcb9f80"),
                          highchartOutput("hist")  %>% withSpinner(color="#6fcb9f80")
                        )
                      )
                     ),
             
             tabPanel("Check homoscedasticity",
                      sidebarLayout(
                        sidebarPanel(
                          uiOutput('homo_test_results_table'),
                          htmlOutput('homo_icon'),
                          uiOutput('pseudoreplication')
                        ),
                        mainPanel(
                           uiOutput('boxplot')  %>% withSpinner(color="#6fcb9f80")
                        )
                      )

                      ),
             
             
             
             tabPanel("Test hypotheses",
                      tags$style(type = "text/css", "#vai {width: 80%;margin-left: 10%;margin-right: 10px;  font-weight: bold;}"),
                      tags$style(type = "text/css", "#vai_nest {width: 80%;margin-left: 10%;margin-right: 10px; !important;}"),
                      tags$style(type = "text/css", ".modal-content {text-align: center;}"),

                      sidebarLayout(
                        sidebarPanel(
                          htmlOutput('factors_title'),
                          uiOutput('factors_definition'),

                          htmlOutput('factors_relationship'),
                          uiOutput('factors_nestedness'),

                          hr(),
                          hr(),
                         #actionButton('vai', "Run ANOVA!"),
                          uiOutput('anova_button'),
                          hr(),
                          uiOutput('alpha_selector')

                        ),
                        mainPanel(

                          DT::dataTableOutput('anova_res'),
                          htmlOutput("F_chart")  %>% withSpinner(color="#6fcb9f80"),
                          uiOutput('popup_no_fixed')
                        )
                      )
                      ),
    
    
             tabPanel("Post hoc tests",
                      
                      sidebarLayout(
                        sidebarPanel(
                          uiOutput('choose_method_title'),
                          uiOutput('adjustment_selector')

                        ),
                        mainPanel(
                          DT::dataTableOutput('posthoc_res_table') %>% withSpinner(color="#6fcb9f80")
                        )

                      )
                      ),
    
    
             tabPanel("Download results",
                      tags$div(class="header", checked=NA,
                               tags$p("An action button to download your report will appear here once you have finished your analysis.")
                      ),
                      uiOutput("downloadReport")
                      # fluidRow(downloadButton("report", "Generate report")),
                      # hr(),
                      # fluidRow(downloadButton('downloadResults', "Download ANOVA table")),
                      # hr(),
                      # fluidRow(downloadButton('downloadPostHoc', "Download post hoc table")),
                      # hr(),
                      # fluidRow(downloadButton('downloadDIY_ANOVA', "Download this app"))
                      ),
    text = HTML("<font size='1'><i>by Danilo Pecorino</i></font>")

    
    
  )
)