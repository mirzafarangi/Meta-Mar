library("metafor")
library("meta")
library(esc)
library("ps")
library("readxl")
library(shiny)
library(shinythemes)
library('shinyWidgets')
library(shinyjs)




# Define UI
ui <- fluidPage(theme = shinytheme("flatly"),
                tags$style(HTML("@import url('https://fonts.googleapis.com/css2?family=Roboto:wght@300&display=swap'); * {font-family: 'Roboto', sans-serif; font-weight: bold;};")),
                tags$head(
                  tags$script(HTML('<script src="https://cmp.osano.com/6oaTlT8UZXXm8UD/97ac88a5-f3b1-471c-b90c-96cdca6cedaf/osano.js"></script>')),
                  tags$script(HTML("(function(w,d,s,l,i){w[l]=w[l]||[];w[l].push({'gtm.start':new Date().getTime(),event:'gtm.js'});var f=d.getElementsByTagName(s)[0],j=d.createElement(s),dl=l!='dataLayer'?'&l='+l:'';j.async=true;j.src='https://www.googletagmanager.com/gtm.js?id='+i+dl;f.parentNode.insertBefore(j,f);})(window,document,'script','dataLayer','GTM-TQD5XK8');")),
                  HTML("<!-- Global site tag (gtag.js) - Google Analytics --><script async src='https://www.googletagmanager.com/gtag/js?id=G-9BDG6Z7VQZ'></script>
                                  <script>window.dataLayer = window.dataLayer || [];function gtag(){dataLayer.push(arguments);}gtag('js', new Date());gtag('config', 'G-9BDG6Z7VQZ');</script>"),
                  
                  tags$meta(name='description',content="is a online free meta analysis calculator / effect size calculator developed based on R & Shiny! Standardized Mean Difference (SMD), mean difference, correlations and Dichotomous Models (relative risk, ratios, risk difference) can be set as models of meta-analysis. Heterogeneity calculations, forest plot, funnel plot, drapery plot, trim and fill, Bias of publication,  Fail-N Safe based on Rosenthal, Orwin, and Rosenberg, and Subgroup (moderator) analysis, meta regression are also included."),
                  tags$meta(name="keywords", content="meta-mar, meta analysis calculator, meta-analysis calculator, effect size calculator, free online meta-analysis, meta-regression, subgroup analysis, heterogeneity calculator, forest plot, drapery, funnel, Rosenthal Orwin Rosenberg Fail-N Safe , ashkan beheshti, اشکان بهشتی, cohen d, hedges g, q Cochrane, meta analysis software,free  meta analysi,s free software"),
                  tags$meta(name="msvalidate.01", content="F5D2786950F28A18F301F925652D2412"),
                  
                  tags$meta(name="google-site-verification", content="r0pB8XGLRIUjRXKoxQcWPOMQ3-JF9MNSNFEPxa5UCtg"),
                  tags$title("Meta-Mar free online meta-analysis calculator"),
                  tags$link(
                    rel="stylesheet", href="https://cdn.jsdelivr.net/npm/bootstrap-icons@1.8.2/font/bootstrap-icons.css"),
                  tags$style(HTML("
      .shiny-output-error-validation {
        color: green;
      } 
    "))
                ),
                navbarPage(id="calc",
                           
                           tags$a(href="https://www.meta-mar.com",style="text-decoration:none;",list(icon("robot"),HTML("Meta-Mar"))), 
                           tabPanel("welcome",mainPanel(
                             HTML("<noscript><iframe src='https://www.googletagmanager.com/ns.html?id=GTM-TQD5XK8' height='0' width='0' style='display:none;visibility:hidden'></iframe></noscript>"),
                             
                             tags$p(),
                             tags$p(),
                             tags$style(HTML(".tit {
  background-color: #F2F4F4;
  
  border: 1.5px solid #D3D3D3;
  border-radius: 5px;
  padding: 20px;
  margin: 20px;
} .code {color: #16A085 ; text-decoration:none;} #code {background-color: #F2F4F4; color: #5a5a5a ; text-decoration:none;} h1 {font-size: 20px;} .lettersp {letter-spacing: 3px;background-color: white; color: #5a5a5a ; text-decoration:none;}")),
                             tags$code(class="lettersp","start(Meta-Mar)"),
                             tags$div(class="tit",tags$p(tags$h1("Meta-Mar",  "[", actionLink(class="code", "metacalc", "meta-analysis calculator"),",", actionLink(class="code", "effcalc","effect size calculator"),"]")), tags$p(" -- v3.5.1 © Copyright 2018")),
                             tags$br(),
                             
                             tags$code(class="lettersp","help(Meta-Mar)"),
                             
                             tags$div(class="tit", tags$p(tags$code(id="code",icon("robot"),"-- To run your meta-analysis, navigate to [meta-analysis calculator] and:")),
                                      tags$p(tags$code(id="code","# Step 1. Identify which effect size or outcome measure should be calculated.")),
                                      tags$p(tags$code(id="code", "# Step 2. Download the excel file and set your data (without changing the column labels).")),
                                      tags$p(tags$code(id="code","# Step 3. Enter your data (copy-paste from excel file or upload the .xlsx/.csv file). "))),
                             p(),
                             p(),img(src="models.png", width="250", height="250"),img(src="data.png", width="250", height="250"),img(src="result.png", width="250", height="250"),
                             tags$br(),
                             tags$br(),tags$br(),
                             
                             tags$img(src="freud_4.jpg", align="right"))),
                           tabPanel("meta analysis calculator",
                                    HTML("<noscript><iframe src='https://www.googletagmanager.com/ns.html?id=GTM-TQD5XK8' height='0' width='0' style='display:none;visibility:hidden'></iframe></noscript>"),
                                    
                                    sidebarPanel(
                                      
                                      tags$body(icon("robot"), strong("Step 1. model setting")),
                                      
                                      tags$p(),
                                      tags$p(),
                                      radioButtons("model", "* Model of analysis:",
                                                   choiceNames = list(HTML("<b>Standardized Mean Difference</b><h6>(SMD: Mean<sub>1,2</sub>, SD<sub>1,2</sub>, n<sub>1,2</sub>)</h6>"),
                                                                      HTML("<b>Correlations </b><h6>(COR: n, r)</h6>"),
                                                                      HTML("<b>Dichotomous Models</b> <h6>(DM: risks & ratios)</h6>")),
                                                   choiceValues = list(
                                                     "smd", "cor","dm"
                                                   ),
                                                   selected = "smd"
                                      ),
                                      tags$p(),
                                      uiOutput("smd_method"),
                                      uiOutput("cor_method"),
                                      uiOutput("dm_method_output"),
                                      
                                      HTML("<b>* Does your data include subgroup values?</b>"),
                                      checkboxInput("subgroup_check", "Yes! -> [ meta regression , subgroup analysis ]", value = TRUE),
                                      tags$p(),
                                      uiOutput("fail_method"),
                                      tags$p(),
                                      tags$body(icon("robot"), strong("Step 2. data setting")),
                                      tags$br(),
                                      tags$p(),
                                      tags$p(HTML('Download:')), uiOutput("dl"),
                                      tags$p(),
                                      tags$br(),
                                      
                                      tags$body(icon("robot"), strong("Step 3. data entry")),
                                      uiOutput("data_method"),
                                      tags$p(),
                                      uiOutput("up"),
                                      tags$p(),
                                      
                                      
                                      
                                      
                                    ), # sidebarPanel
                                    
                                    mainPanel(uiOutput("area"),
                                              tags$br(),
                                              uiOutput("error_check"),
                                              uiOutput("example"),
                                              tags$br(),
                                              tags$blockquote(icon("robot"),HTML("<b>Data Summary</b>")),
                                              dataTableOutput('dataTable'),
                                              tags$br(),tags$br(),
                                              tags$br(),
                                              tags$blockquote(icon("robot"),HTML("<b>Forest plot of fixed model</b>")),
                                              downloadButton("forest_FE_dl"),
                                              selectInput("dl_methode_FE", "",choices=c("PDF"="pdf", "PNG"="png"), selected = "pdf", width="90px"),
                                              tags$br(),
                                              plotOutput("forest_FE"),
                                              tags$br(),
                                              tags$blockquote(icon("robot"),HTML("<b>Forest plot of random model</b>")),
                                              downloadButton("forest_RE_dl"),
                                              selectInput("dl_methode_RE", "",choices=c("PDF"="pdf", "PNG"="png"), selected = "pdf", width="90px"),
                                              plotOutput("forest_RE"),
                                              tags$br(),
                                              tags$blockquote(icon("robot"),HTML("<b>Drapery plot</b>")),
                                              downloadButton("drapery_dl"),
                                              selectInput("dl_methode_dr", "",choices=c("PDF"="pdf", "PNG"="png"), selected = "pdf", width="90px"),
                                              plotOutput("drapery_pl"),
                                              tags$br(),
                                              tags$blockquote(icon("robot"),HTML("<b>Summary of meta-analysis</b>")),
                                              verbatimTextOutput('summary'),
                                              tags$br(),
                                              tags$blockquote(icon("robot"),HTML("<b>Publication bias (fail-safe N) </b>")),
                                              verbatimTextOutput('fsn'),
                                              tags$br(),
                                              tags$blockquote(icon("robot"),HTML("<b>Funnel plot (Trim and Fill)</b>")),
                                              downloadButton("funnel_dl"),
                                              selectInput("dl_methode_funnel", "",choices=c("PDF"="pdf", "PNG"="png"), selected = "pdf", width="90px"),
                                              plotOutput("funnel_"),
                                              tags$br(),
                                              tags$blockquote(icon("robot"),HTML("<b>Publication bias: Egger’s Regression Test </b>")),
                                              tags$br(),
                                              tags$p(HTML("To get more valid results: number of studies  > 9.")),
                                              verbatimTextOutput("egger"),
                                              tags$br(),
                                              tags$blockquote(icon("robot"),HTML("<b>Subgroup: Boxplot</b>")),
                                              downloadButton("boxplot_dl"),
                                              selectInput("dl_methode_box", "",choices=c("PDF"="pdf", "PNG"="png"), selected = "pdf", width="90px"),
                                              plotOutput("subg"),
                                              tags$br(),
                                              tags$blockquote(icon("robot"),HTML("<b>Subgroup: Meta regression </b>")),
                                              verbatimTextOutput('meta_regression'),
                                              tags$br()
                                    ) # mainPanel
                                    
                           ), # Navbar 1, tabPanel
                           tabPanel("effect size calculator",
                                    HTML("<noscript><iframe src='https://www.googletagmanager.com/ns.html?id=GTM-TQD5XK8' height='0' width='0' style='display:none;visibility:hidden'></iframe></noscript>"),
                                    
                                    mainPanel(
                                      tabsetPanel(
                                        
                                        tabPanel("Correlation coefficient", 
                                                 tags$br(),
                                                 sidebarPanel("", width = 6,
                                                              selectInput("es_type_r", label = "Effect size convertor", choices = c("Cohen's d"="d","Hedges’ g"="g","correlation"="r", "odds ratio"="or" ,"log odds"="logit"), selected = "g"),
                                                              tags$p("Enter values for r, n1, n2"),
                                                              #r div
                                                              
                                                              numericInput("rpb",label='r', value=0.35),
                                                              numericInput("rpb1n",label='n1', value=44),
                                                              numericInput("rpb2n",label='n2', value=54),
                                                              
                                                              
                                                              verbatimTextOutput("esc_r")
                                                              
                                                 ),
                                                 sidebarPanel("", width = 6,
                                                              selectInput("es_type_pr", label = "Effect size convertor", choices = c("Cohen's d"="d","Hedges’ g"="g","correlation"="r", "odds ratio"="or" ,"log odds"="logit"), selected = "g"),
                                                              tags$p("Enter values for p value, n1, n2"),
                                                              #r div
                                                              
                                                              numericInput("pr",label='p value', value=0.045),
                                                              numericInput("pr1n",label='n1', value=120),
                                                              numericInput("pr2n",label='n2', value=210),
                                                              
                                                              
                                                              verbatimTextOutput("esc_pr")
                                                              
                                                 )
                                                 
                                                 
                                        ),
                                        tabPanel("Regression coefficient",
                                                 tags$br(),
                                                 sidebarPanel("", width = 6,
                                                              tags$br(),
                                                              selectInput("es_type_ub", label = "Effect size convertor", choices = c("Cohen's d"="d","Hedges’ g"="g","correlation"="r", "odds ratio"="or" ,"log odds"="logit"), selected = "g"),
                                                              tags$p("Enter values for unstandardized B, sd y, n1, n2"),
                                                              tags$br(),
                                                              #ub div
                                                              tags$div(id="ub_model",
                                                                       numericInput("bb1",label='unstandardized coefficient B', value=7.4),
                                                                       numericInput("bb2",label='standard deviation of the dependent variable', value=7),
                                                                       numericInput("bb3",label='treatment group sample size', value=110),
                                                                       numericInput("bb4",label='control group sample size', value=250),
                                                                       tags$br(),
                                                                       verbatimTextOutput("esc_B")),
                                                 ),
                                                 sidebarPanel("", width = 6,
                                                              tags$br(),
                                                              selectInput("es_type_sb", label = "Effect size convertor", choices = c("Cohen's d"="d","Hedges’ g"="g","correlation"="r", "odds ratio"="or" ,"log odds"="logit"), selected = "g"),
                                                              tags$p("Enter values for standardized B, sd y, n1, n2"),
                                                              tags$br(),
                                                              
                                                              #sb div
                                                              
                                                              numericInput("b1",label='standardized coefficient B', value=3.3),
                                                              numericInput("b2",label='standard deviation of the dependent variable', value=5),
                                                              numericInput("b3",label='treatment group sample size', value=100),
                                                              numericInput("b4",label='control group sample size', value=150),
                                                              tags$br(),
                                                              verbatimTextOutput("esc_beta"),
                                                 )
                                                 
                                        ),
                                        tabPanel("Mean & Standard Error", 
                                                 tags$br(),
                                                 sidebarPanel("", width = 6,
                                                              selectInput("es_type_smd", label = "Effect size convertor", choices = c("Cohen's d"="d","Hedges’ g"="g","correlation"="r", "odds ratio"="or" ,"log odds"="logit"), selected = "g"),
                                                              tags$p("Enter values for n, mean, sd"),
                                                              #smd div
                                                              HTML("Group I"),
                                                              numericInput("grp1n",label='sample size', value=350),
                                                              numericInput("grp1m",label='mean', value=44.37),
                                                              numericInput("grp1sd",label='standard deviation', value=2.8),
                                                              
                                                              HTML("Group II"),
                                                              numericInput("grp2n",label='sample size', value=390),
                                                              numericInput("grp2m",label='mean', value=27.89),
                                                              numericInput("grp2sd",label='standard deviation', value=4.6),
                                                              
                                                              verbatimTextOutput("esc_mean_sd")
                                                              
                                                 ),
                                                 sidebarPanel("", width = 6,
                                                              selectInput("es_type_sme", label = "Effect size convertor", choices = c("Cohen's d"="d","Hedges’ g"="g","correlation"="r", "odds ratio"="or" ,"log odds"="logit"), selected = "g"),
                                                              tags$p("Enter values for n, mean, se"),
                                                              #sme div
                                                              HTML("Group I"),
                                                              numericInput("grp1ne",label='sample size', value=350),
                                                              numericInput("grp1me",label='mean', value=44.37),
                                                              numericInput("grp1se",label='standard error', value=2.8),
                                                              
                                                              HTML("Group II"),
                                                              numericInput("grp2ne",label='sample size', value=390),
                                                              numericInput("grp2me",label='mean', value=27.89),
                                                              numericInput("grp2se",label='standard error', value=4.6),
                                                              
                                                              verbatimTextOutput("esc_mean_se")
                                                              
                                                 )),
                                        
                                        tabPanel("2 by 2 Contingency table", 
                                                 tags$br(),
                                                 sidebarPanel("",
                                                              selectInput("es_type_dm", label = "Effect size convertor", choices = c("Cohen's d"="d","Hedges’ g"="g","correlation"="r", "odds ratio"="or" ,"log odds"="logit" , "eta square"="eta"), selected = "g"),
                                                              tags$p("Enter values for a, b, c, d"),
                                                              #dm div
                                                              HTML("Group I"),
                                                              numericInput("grp1yes",label='true positive', value=350),
                                                              numericInput("grp1no",label='false negative', value=69),
                                                              HTML("Group II"),
                                                              numericInput("grp2yes",label='false positive', value=89),
                                                              numericInput("grp2no",label='true negative', value=400)
                                                 ),
                                                 
                                                 
                                                 mainPanel(verbatimTextOutput("esc_2x2"))),
                                        
                                        tabPanel("Chi-Square coefficient",
                                                 tags$br(),
                                                 sidebarPanel("", width = 6,
                                                              tags$br(),
                                                              selectInput("es_type_ch", label = "Effect size convertor", choices = c("Cohen's d"="d","Hedges’ g"="g","correlation"="r", "odds ratio"="or" ,"log odds"="logit"), selected = "g"),
                                                              tags$p("Enter values for Chi-Square, n"),
                                                              tags$br(),
                                                              #ch div
                                                              tags$div(id="ch_model",
                                                                       numericInput("chisq",label='Chi-Square', value=9.9),
                                                                       numericInput("totaln_ch",label='total sample size', value=150),
                                                                       tags$br()),
                                                              verbatimTextOutput("esc_chisq")),
                                                 
                                                 sidebarPanel("", width = 6,
                                                              tags$br(),
                                                              selectInput("es_type_pch", label = "Effect size convertor", choices = c("Cohen's d"="d","Hedges’ g"="g","correlation"="r", "odds ratio"="or" ,"log odds"="logit"), selected = "g"),
                                                              tags$p("Enter values for p value, n"),
                                                              tags$br(),
                                                              #ch div
                                                              tags$div(id="pch_model",
                                                                       numericInput("pch",label='p value', value=0.024),
                                                                       numericInput("totaln_pch",label='total sample size', value=150),
                                                                       tags$br()),
                                                              verbatimTextOutput("esc_pchisq"))),
                                        tabPanel("Student’s t-test",
                                                 tags$br(),
                                                 sidebarPanel("",width = 6,
                                                              tags$br(),
                                                              selectInput("es_type_t", label = "Effect size convertor", choices = c("Cohen's d"="d","Hedges’ g"="g","correlation"="r", "odds ratio"="or" ,"log odds"="logit"), selected = "g"),
                                                              tags$p("Enter values for t, total n=n1+n2"),
                                                              tags$br(),
                                                              #t div
                                                              tags$div(id="t_model",
                                                                       numericInput("t",label='t', value=3.3),
                                                                       numericInput("totaln_t",label='total sample size', value=100),
                                                                       tags$br()),
                                                              verbatimTextOutput("esc_t")),
                                                 
                                                 sidebarPanel("",width = 6,
                                                              tags$br(),
                                                              selectInput("es_type_pt", label = "Effect size convertor", choices = c("Cohen's d"="d","Hedges’ g"="g","correlation"="r", "odds ratio"="or" ,"log odds"="logit"), selected = "g"),
                                                              tags$p("Enter values for p value, total n=n1+n2"),
                                                              tags$br(),
                                                              #t div
                                                              tags$div(id="pt_model",
                                                                       numericInput("pt",label='p value', value=3.3),
                                                                       numericInput("totaln_pt",label='total sample size', value=100),
                                                                       tags$br()),
                                                              verbatimTextOutput("esc_pt")),
                                        ),
                                        tabPanel("One-Way ANOVAs", 
                                                 tags$br(),
                                                 sidebarPanel("",
                                                              selectInput("es_type_f", label = "Effect size convertor", choices = c("Cohen's d"="d","Hedges’ g"="g","correlation"="r", "odds ratio"="or" ,"log odds"="logit"), selected = "g"),
                                                              tags$p("Enter values for F value, n1, n2"),
                                                              #r div
                                                              tags$div(id="f_model",
                                                                       numericInput("f",label='F value', value=16.46),
                                                                       numericInput("f1n",label='n1', value=110),
                                                                       numericInput("f2n",label='n2', value=100),
                                                                       
                                                                       
                                                              )
                                                              
                                                 ), mainPanel(verbatimTextOutput("esc_f"))),
                                      ) #tabsetPanel
                                      
                                    ) # mainPanel
                                    
                           ),
                           tabPanel("data frame examples",
                                    HTML("<noscript><iframe src='https://www.googletagmanager.com/ns.html?id=GTM-TQD5XK8' height='0' width='0' style='display:none;visibility:hidden'></iframe></noscript>"),
                                    
                                    sidebarPanel(
                                      radioButtons("model_", "Model of analysis:",
                                                   choiceNames = list(HTML("<b>Standardized Mean Difference</b><h6>(SMD: Mean<sub>1,2</sub>, SD<sub>1,2</sub>, n<sub>1,2</sub>)</h6>"),
                                                                      HTML("<b>Correlations </b><h6>(COR: n, r)</h6>"),
                                                                      HTML("<b>Dichotomous Models</b> <h6>(DM: risks & ratios)</h6>")),
                                                   choiceValues = list(
                                                     "smd", "cor", "dm"
                                                   ),
                                                   selected = "smd"
                                      ),
                                      
                                      
                                    ),# sidebarPanel
                                    mainPanel(width = 12,
                                              dataTableOutput('first_'),
                                              
                                              
                                    ) # mainPanel
                                    
                           ),
                           tabPanel(HTML("citation & references"), 
                                    HTML("<noscript><iframe src='https://www.googletagmanager.com/ns.html?id=GTM-TQD5XK8' height='0' width='0' style='display:none;visibility:hidden'></iframe></noscript>"),
                                    
                                    mainPanel(
                                      useShinyjs(),
                                      tags$p(),
                                      tags$p(),
                                      tags$br(),
                                      actionLink("cite", "Cite Meta-Mar"),
                                      tags$div(id="div_cite", class="tit","Beheshti, A., Chavanon, M. L., & Christiansen, H. (2020). Emotion dysregulation in adults with attention deficit hyperactivity disorder: a meta-analysis. BMC psychiatry, 20(1), 1-11.",a(href="https://doi.org/10.1186/s12888-020-2442-7","DOI:https://doi.org/10.1186/s12888-020-2442-7")),
                                      tags$p(),
                                      tags$br(),
                                      actionLink("selfref", "References"),
                                      tags$div(id="div_selfref", class="tit", p("Borenstein, M., Hedges, L. V., Higgins, J. P., & Rothstein, H. R. (2021). Introduction to meta-analysis. John Wiley & Sons.") , tags$br(),
                                               p("Harrer, M., Cuijpers, P., Furukawa, T.A., & Ebert, D.D. (2021). Doing Meta-Analysis with R: A Hands-On Guide. Boca Raton, FL and London: Chapman & Hall/CRC Press. ISBN 978-0-367-61007-4."), tags$br(), 
                                               p("Balduzzi S, Rücker G, Schwarzer G (2019). “How to perform a meta-analysis with R: a practical tutorial.” Evidence-Based Mental Health, 153–160."), tags$br(),
                                               p("Ellis, P. D. (2010). The essential guide to effect sizes: Statistical power, meta-analysis, and the interpretation of research results. Cambridge university press."), tags$br(),
                                               
                                      ),
                                      tags$p(),
                                      tags$br(),
                                      actionLink("ref", "Meta-Mar in research area"),
                                      tags$div(id="div_ref", class="tit","Awori, M., Mehta, N., Kebba, N., & Makori, E. (2020). Adding Blood to St Thomas Solution Does Not Improve Mortality in Pediatric Cardiac Surgery: A Meta-analysis of a Homogenous Population. Annals of African Surgery, 17(2), 60-64." , br(),p(), 
                                               "Demirci, M. (2021). Dialister in Microbiome of Cancer Patients: A Systematic Review and Meta-Analysis. Eurasian Journal of Medicine and Oncology.", br(),p(),
                                               "Urnia, E. E., Noor, M. S., Hartoyo, E., Suhartono, E., & Budinurdjaja, P. (2020). Meta Analysis: the Influence of Health Education about Early Marriage to Knowledge on Teenagers (Review of Caution Using Motion Picture, Booklet and Leaflet). Available at SSRN 3918814." , br(),p(), 
                                               "Akbar, J. R., & Yudiarso, A. (2020). Kecerdasan emosional kurang efektif terhadap kinerja karyawan: Studi meta analisis. Jurnal Diversita, 6(2), 260-269.", br(),p(),
                                               "Köcher, L. M., Schneider, K., & Christiansen, H. (2021). Thinking about worry: A systematic review and meta-analysis on the assessment of metacognitions in children and adolescents. World Journal of Psychiatry, 11(9), 635." , br(),p(), 
                                               "Rohmatulloh, R., Syamsuri, S., Nindiasari, H., & Fatah, A. (2022). Analisis Meta: Pengaruh Model Pembelajaran Problem Based Learning (PBL) Terhadap Kemampuan Penalaran Matematis Siswa. Jurnal Cendekia: Jurnal Pendidikan Matematika, 6(2), 1558-1567.", br(),p(),
                                               "Tanoko, S. M. (2021). BENARKAH ADA HUBUNGAN ANTARA SELF-ESTEEM DENGAN DEPRESI? SEBUAH STUDI META ANALISIS. Insight: Jurnal Ilmiah Psikologi, 23(1), 35-45." , br(),p(), 
                                               "Prameswari, A., & Yudiarso, A. (2021). Efektivitas Mindfulness-Based Cognitive Therapy untuk Menurunkan Depresi: Meta-Analisis. Psycho Idea, 19(2), 151-160.", br(),p(),
                                               "Kowsar, R., Rahimi, A. M., Sroka, M., Mansouri, A., Sadeghi, K., Bonakdar, E., ... & Mahdavi, A. H. (2022). Risk of mortality in COVID-19 patients: a meta-and network analysis." , br(),p(), 
                                               "Fadillah, N., & Yudiarso, A. (2021). Studi meta analisis: Efektivitas progressive muscle relaxation untuk menurunkan kecemasan orang dengan penderita penyakit kronis. Jurnal Ilmiah Psikologi Terapan, 9(1), 52-63.", br(),p(),
                                               "Lubis, I. H. Studi Meta-Analisis: Hubungan Antara Beban Kerja dengan Stres Kerja Meta-Analysis Study: A Correlation Between workload and work stress." , br(),p(), 
                                               "Nadzif, M. L., & Yudiarso, A. (2021). Studi Meta-Analisis: Hubungan antara Stres Kerja dan Kinerja Karyawan. Jurnal Diversita, 7(1), 43-52.", br(),p(),
                                               "Issalillah, F. (2022). PENGARUH STRES KERJA DENGAN KINERJA PERAWAT DI RUMAH SAKIT. Jurnal Ilmiah Satyagraha, 5(1), 48-56.", br(),p(),
                                               "Sa’adah, N. S. S., Alwandri, H., Nugroho, L. H., Sukirno, S., & Nuringtyas, T. R. (2022, May). A Meta-Analysis Study on Spodoptera exigua and Spodoptera litura Control: Biopesticides vs. Synthetic Pesticides. In 7th International Conference on Biological Science (ICBS 2021) (pp. 519-527). Atlantis Press.", br(),p(),
                                               "Tavares, C. B., Alves-Ribeiro, F. A., Junior, E. D. J. N., de Vasconcelos-Valença, R. J., Campos-Verdes, L. C., Gomes, F. D. C. S. A., ... & da Silva, B. B. (2020). Association of XRCC1 rs1799782 and ERCC2 rs13181 Polymorphisms with Glioma Risk: A Systematic Review and Meta-Analysis.", br(),p()
                                      ),
                                      tags$p(),
                                      tags$br(),
                                      actionLink("mail", "Contact"),
                                      tags$div(id="div_mail", class="tit", tags$text(tags$a(href='mailto:s.ashkan.beheshti@gmail.com',"s.ashkan.beheshti@gmail.com"))),p(),p(),br(),
                                      actionLink("lic", "License"),
                                      tags$div(id="div_lic", class="tit", uiOutput("lic")),p(),p(),br(),
                                      tags$img(src="meta00.jpg", align="right", width="340px", height="240px")
                                    )
                           )
                           
                ) # navbarPage
) # fluidPage


# Define server function  
server <- function(input, output,session) {
  
  #which calc
  observeEvent(input$metacalc, {
    
    updateNavbarPage(session, "calc", "meta analysis calculator")
  })
  observeEvent(input$effcalc, {
    
    updateNavbarPage(session, "calc","effect size calculator")
  })
  
  
  #smd method
  output$smd_method<-renderUI({
    if(input$model=="smd"){
      
      radioButtons("smd_method", "* Estimate of the avarage effect size:",
                   choiceNames = list("Hedges’ g", "Cohen’s d", "Glass’ delta"),
                   
                   
                   choiceValues = list(
                     "g", "d", "delta"
                   ),
                   selected = "g"
      )
    }
  })
  
  smd_method_func<-reactive({
    if (input$smd_method=="g"){return("Hedges")}
    else if (input$smd_method=="d"){return("Cohen")}
    else if (input$smd_method=="delta"){return("Glass")}
  })
  
  
  #cor method
  output$cor_method<-renderUI({
    if(input$model=="cor"){
      
      radioButtons("cor_method", "* Estimate of the avarage effect size:",
                   choiceNames = list("Correlation coefficient", "Fisher Z-Transformation"),
                   choiceValues = list(
                     "c", "z"
                   ),
                   selected = "c"
      )
    }
  })
  
  cor_method_func<-reactive({
    if (input$cor_method=="c") {return(TRUE)}
    else if (input$cor_method=="z") {return(FALSE)}
  })
  
  #dm method
  output$dm_method_output<-renderUI({
    if(input$model=="dm"){
      radioButtons("dm_method_input", "* Estimate of the avarage effect size:",
                   choiceNames = list("log risk ratio", "log odds ratio", "risk difference"),
                   
                   
                   choiceValues = list(
                     "rr", "or", "rd"
                   ),
                   selected = "rr"
      )}
  })
  dm_method_func<-reactive({
    if (input$dm_method_input=="rr") {return("RR")}
    else if (input$dm_method_input=="or") {return("OR")}
    else if (input$dm_method_input=="rd") {return("RD")}
  })
  
  #publication bias
  output$fail_method<-renderUI({
    radioButtons("fail_", HTML("* Publication Bias: <h6>File Drawer Analysis (fail-safe N)</h6>"),
                 choiceNames = list("Rosenthal", "Orwin", "Rosenberg"),
                 
                 
                 choiceValues = list(
                   "rosenthal", "orwin", "rosenberg"
                 ),
                 selected = "rosenthal"
    )
  })
  
  #download  
  output$dl<-renderUI({
    if (input$subgroup_check==TRUE) {
      if(input$model=="smd"){
        list(downloadLink("smd_sub_dl","SMD.csv"),HTML("  or  "),HTML("<a href='https://docs.google.com/spreadsheets/d/1sdtblzEZWuwmIUzNaMYUEzpAAtx5Szhu/edit?usp=sharing&ouid=102191620218365792171&rtpof=true&sd=true' target='_blank'>SMD.xlsx </a>")
        )
      }
      else if(input$model=="cor"){
        list(downloadLink("cor_sub_dl","COR.csv"),HTML("  or  "),
             HTML("<a href='https://docs.google.com/spreadsheets/d/1KDxkNL8903UgqnVOWXpTJjTZKWbgDXG-/edit?usp=sharing&ouid=102191620218365792171&rtpof=true&sd=true' target='_blank'>COR.xlsx </a>"))}
      
      else if(input$model=="dm"){
        list(downloadLink("dm_sub_dl","DM.csv"),HTML("  or  "),
             HTML("<a href='https://docs.google.com/spreadsheets/d/1kMBzsXa0Sh9QN6XD0fFbQg7QIjYqdRD-/edit?usp=sharing&ouid=102191620218365792171&rtpof=true&sd=true' target='_blank'>DM.xlsx </a>"))}}
    
    else if (input$subgroup_check==FALSE) {
      if(input$model=="smd"){
        list(downloadLink("smd_nosub_dl","SMD.csv"),HTML("  or  "),
             HTML("<a href='https://docs.google.com/spreadsheets/d/11U2WpJj4gZPzsb8jpseLd6wQWH51PyPn/edit?usp=sharing&ouid=102191620218365792171&rtpof=true&sd=true' target='_blank'>SMD.xlsx </a>"))}
      
      else if(input$model=="cor"){
        list(downloadLink("cor_nosub_dl","COR.csv"),HTML("  or  "),
             HTML("<a href='https://docs.google.com/spreadsheets/d/1BQ1ED11C3hdqvLSsZWn_TIHQWqs_ol0E/edit?usp=sharing&ouid=102191620218365792171&rtpof=true&sd=true' target='_blank'>COR.xlsx </a>"))}
      else if(input$model=="dm"){
        list(downloadLink("dm_nosub_dl","DM.csv"),HTML("  or  "),
             HTML("<a href='https://docs.google.com/spreadsheets/d/1V2ypQg1ty5Mu151_xm8hUNy6lgp3Jaxd/edit?usp=sharing&ouid=102191620218365792171&rtpof=true&sd=true' target='_blank'>DM.xlsx </a>"))}
    } 
  })
  output$smd_sub_dl <- downloadHandler(
    filename = function() {
      paste0("SMD", ".csv")
    },
    content = function(file) {
      write.csv(data(), file)
    }
  )
  
  output$cor_sub_dl <- downloadHandler(
    filename = function() {
      paste0("COR", ".csv")
    },
    content = function(file) {
      write.csv(data(), file)
    }
  )
  output$dm_sub_dl <- downloadHandler(
    filename = function() {
      paste0("DM", ".csv")
    },
    content = function(file) {
      write.csv(data(), file)
    }
  )
  output$smd_nosub_dl <- downloadHandler(
    filename = function() {
      paste0("SMD", ".csv")
    },
    content = function(file) {
      write.csv(data(), file)
    }
  )
  output$cor_nosub_dl <- downloadHandler(
    filename = function() {
      paste0("COR", ".csv")
    },
    content = function(file) {
      write.csv(data(), file)
    }
  )
  output$dm_nosub_dl <- downloadHandler(
    filename = function() {
      paste0("DM", ".csv")
    },
    content = function(file) {
      write.csv(data(), file)
    }
  )
  
  #data method
  output$data_method<-renderUI({
    radioButtons("data_method", "",
                 choiceNames = list("copy-paste data from the Excel file", "or upload the .xlsx/.csv file"),
                 choiceValues = list(
                   "cpm", "upm"
                 ),
                 selected = "cpm"
    )
  })
  #upload method
  output$up<-renderUI({
    if (input$data_method=="upm") {
      if(input$model=="smd"){
        fileInput("file_mean",  NULL,placeholder = "*** file name: SMD.xlsx or SMD.csv", multiple = FALSE, accept = c(".xlsx", ".xls", ".csv"))
      }
      
      else if(input$model=="cor"){
        fileInput("file_mean", NULL,placeholder = "*** file name: COR.xlsx or COR.csv",  multiple = FALSE, accept = c(".xlsx", ".xls", ".csv"))
      }
      else if(input$model=="dm"){
        fileInput("file_mean", NULL, placeholder = "*** file name: DM.xlsx or DM.csv", multiple = FALSE, accept = c(".xlsx", ".xls", ".csv"))
      }}
  })
  
  
  #textarea method
  output$area<-renderUI({
    if (input$data_method=="cpm"){
      if (input$subgroup_check==TRUE) {
        if(input$model=="smd"){
          textAreaInput("drop", "To start, remove the SMD model example below and copy-paste your data directly from Excel here \n (Data includes subgroups):", "Study	N1	Mean1	Sd1	N2	Mean2	Sd2	subgroup
A	155	55	47	156	75	64	subgroup1
B	31	27	7	32	29	4	subgroup1
C	75	64	17	71	119	29	subgroup1
D	18	66	20	18	137	48	subgroup2
E	8	14	8	13	18	11	subgroup2
F	57	19	7	52	18	4	subgroup2
G	34	52	45	33	41	34	subgroup3
H	110	21	16	183	31	27	subgroup3
I	60	30	27	52	23	20	subgroup3", rows=15, cols=30, width = "100%")
        }
        else if(input$model=="cor"){
          textAreaInput("drop", "To start, remove the COR model example below and copy-paste your data directly from Excel here \n (Data includes subgroups):", "Study	n	r	subgroup
A	23	0.33	subgroup1
B	47	0.44	subgroup1
C	44	0.55	subgroup1
D	78	0.22	subgroup2
E	311	0.11	subgroup2
F	144	0.23	subgroup2
G	79	0.88	subgroup3
H	59	0.66	subgroup3
I	214	0.55	subgroup3
J	110	0.73	subgroup3", rows=15, cols=30, width = "100%")
        }
        else if(input$model=="dm"){
          textAreaInput("drop", "To start, remove the DM model example below and copy-paste your data directly from Excel here \n (Data includes subgroups):", "Study	True_Positive	False_Negative	False_Positive	True_Negative	subgroup
A	4	119	11	128	subgroup1
B	6	300	29	274	subgroup1
C	3	228	11	209	subgroup1
D	62	13536	248	12619	subgroup2
E	33	5036	47	5761	subgroup2
F	180	1361	372	1079	subgroup2
G	8	2537	10	619	subgroup3
H	505	87886	499	87892	subgroup3
I	29	7470	45	7232	subgroup4
J	17	1699	65	1600	subgroup4
K	186	50448	141	27197	subgroup4
L	5	2493	3	2338	subgroup4
M	27	16886	29	17825	subgroup4", rows=15, cols=30, width = "100%")
        }
      }
      else if (input$subgroup_check==FALSE) {
        if(input$model=="smd"){
          textAreaInput("drop", "To start, remove the SMD model example below and copy-paste your data directly from Excel here \n (Data includes no subgroup):", "Study	N1	Mean1	Sd1	N2	Mean2	Sd2
A	155	55	47	156	75	64
B	31	27	7	32	29	4
C	75	64	17	71	119	29
D	18	66	20	18	137	48
E	8	14	8	13	18	11
F	57	19	7	52	18	4
G	34	52	45	33	41	34
H	110	21	16	183	31	27
I	60	30	27	52	23	20", rows=15, cols=30, width = "100%")
        }
        else if(input$model=="cor"){
          textAreaInput("drop", "To start, remove the COR model example below and copy-paste your data directly from Excel here \n (Data includes no subgroup):", "Study	n	r
A	23	0.33
B	47	0.44
C	44	0.55
D	78	0.22
E	311	0.11
F	144	0.23
G	79	0.88
H	59	0.66
I	214	0.55
J	110	0.73", rows=15, cols=30, width = "100%")
        }
        else if(input$model=="dm"){
          textAreaInput("drop", "To start, remove the DM model example below and copy-paste your data directly from Excel here \n (Data includes no subgroup):", "Study	True_Positive	False_Negative	False_Positive	True_Negative
A	4	119	11	128
B	6	300	29	274
C	3	228	11	209
D	62	13536	248	12619
E	33	5036	47	5761
F	180	1361	372	1079
G	8	2537	10	619
H	505	87886	499	87892
I	29	7470	45	7232
J	17	1699	65	1600
K	186	50448	141	27197
L	5	2493	3	2338
M	27	16886	29	17825", rows=15, cols=30, width = "100%")
        }
      }
      
    }
  })
  
  
  #results vs example
  output$example<-renderUI(if (input$data_method=="upm"){if (is.null(input$file_mean)) return(if (input$model=="smd"){HTML("<b>This is an example of SMD model, to get your results please upload your data file:</b>")}
                                                                                              else if (input$model=="cor"){HTML("<b>This is an example of COR model, to get your results please upload your data file:</b>")}
                                                                                              else if (input$model=="dm"){HTML("<b>This is an example of DM model, to get your results please upload your data file:</b>")}) 
    else return({HTML("<b>Results of your meta-analysis:</b>")})}
    else if (input$data_method=="cpm") {
      validate(
        need(input$drop!="", "Error: Please check if the data is correctly entered! \n - Specify whether your data includes subgroup values (see Step.1 in the sidebar). \n - Column labels should be matched with the selected model, i.e. SMD, COR, or DM (see 'data frame examples' tab).  \n - It is recommended to use the .xlsx template from 'Step.2' in the sidebar.")
      )
      HTML("<b>Results of your meta-analysis:</b>")
    }
  )
  
  
  #from file vs from server
  #comma to dot  
  cpm_rev<-reactive({
    gsub(",","\\.", input$drop)
  })
  
  data<-reactive(if (input$data_method=="upm") { if (input$subgroup_check==TRUE) {if (is.null(input$file_mean)) return(if (input$model=="smd"){read_excel("models/sub/SMD.xlsx")}
                                                                                                                       else if (input$model=="cor"){read_excel("models/sub/COR.xlsx")}
                                                                                                                       else if (input$model=="dm"){read_excel("models/sub/DM.xlsx")})
    
    else return({read_excel(input$file_mean$datapath)})} 
    else if (input$subgroup_check==FALSE) {if (is.null(input$file_mean)) return(if (input$model=="smd"){read_excel("models/nosub/SMD.xlsx")}
                                                                                else if (input$model=="cor"){read_excel("models/nosub/COR.xlsx")}
                                                                                else if (input$model=="dm"){read_excel("models/nosub/DM.xlsx")})
      
      else return({read_excel(input$file_mean$datapath)})}
  } else if (input$data_method=="cpm") {read.table(file = textConnection(cpm_rev()), header = TRUE, sep = "\t")}) 
  
  my_data <- reactive({
    if(input$model=="smd"){
      metacont(
        mean.e = Mean1, sd.e = Sd1, n.e = N1,
        mean.c = Mean2, sd.c = Sd2, n.c = N2,
        data=data(), studlab = Study, sm = "SMD",
        method.smd = smd_method_func(),
        method.tau = "REML",
        hakn = TRUE,
        title = "Model: SMD - Fixed & Random Effect",
        subgroup=if (input$subgroup_check==TRUE) {subgroup} 
      )}
    
    else if(input$model=="cor"){
      metacor(r, n, data=data(), studlab = Study, 
              method.tau = "REML",  backtransf = cor_method_func(),
              hakn = TRUE,
              title = "Model: COR - Fixed & Random Effect",
              subgroup=if (input$subgroup_check==TRUE) {subgroup})}
    
    else if(input$model=="dm"){
      metabin(event.e=True_Positive, n.e=False_Negative+True_Positive, event.c=False_Positive, n.c=True_Negative+False_Positive,
              data = data(), studlab = Study, sm = dm_method_func(), method = "Inverse", 
              method.tau = "REML",
              hakn = TRUE,
              title = "Model: DM - Fixed & Random Effect",
              subgroup=if (input$subgroup_check==TRUE) {subgroup})}
  })
  
  #publication bias: File Drawer Analysis (fail safe n)
  fsn_<-reactive({
    if(input$fail_=="rosenthal"){
      fsn(my_data()$TE, sei=my_data()$seTE, type="Rosenthal",data=my_data())}
    else if(input$fail_=="orwin"){
      fsn(my_data()$TE, sei=my_data()$seTE, type="Orwin",data=my_data())}
    else if(input$fail_=="rosenberg"){
      fsn(yi=my_data()$TE, sei=my_data()$seTE, type="Rosenberg",data=my_data())}
  })
  
  
  #summary 
  smd_sum<-reactive({if (input$model=="smd") {
    return(summary(my_data()))
  }
  })
  
  cor_sum<-reactive({if (input$model=="cor") {
    return(summary(my_data()))
  }
  })
  
  
  dm_sum<-reactive({if (input$model=="dm") {
    return(summary(my_data()))
  }
  })
  
  #forest
  forest_FE<-reactive({
    forest.meta(my_data(), random=FALSE, layout = "RevMan5")
  })
  #download forest_FE
  
  output$forest_FE_dl<-downloadHandler(
    filename = function() {
      if (input$dl_methode_FE=="png") paste("forest_FE", ".png", sep="")
      else paste("forest_FE", ".pdf", sep="")
    },
    content = function(file) {
      if (input$dl_methode_FE=="png") png(file, res = 300, width = 4000, height = 5000)
      else pdf(file, width = 15, height = 20)
      forest.meta(my_data(), random=FALSE, layout = "RevMan5")
      dev.off()
    }
  )
  forest_RE<-reactive({
    forest.meta(my_data(), fixed=FALSE, layout = "RevMan5")
  })
  
  #download forest_RE
  
  output$forest_RE_dl<-downloadHandler(
    filename = function() {
      if (input$dl_methode_RE=="png") paste("forest_RE", ".png", sep="")
      else paste("forest_RE", ".pdf", sep="")
    },
    content = function(file) {
      if (input$dl_methode_RE=="png") png(file, res = 300, width = 4000, height = 5000)
      else pdf(file, width = 15, height = 20)
      forest.meta(my_data(), random=FALSE, layout = "RevMan5")
      dev.off()
    }
  )
  
  #drapery plot
  drapery_pl<-reactive({
    drapery(my_data(), 
            labels = "studlab",
            type = "pval", 
            legend = TRUE)
  })
  
  #drapery download 
  output$drapery_dl<-downloadHandler(
    filename = function() {
      if (input$dl_methode_dr=="png") paste("drapery", ".png", sep="")
      else paste("drapery", ".pdf", sep="")
    },
    content = function(file) {
      if (input$dl_methode_dr=="png") png(file)
      else pdf(file)
      drapery(my_data(), 
              labels = "studlab",
              type = "pval", 
              legend = TRUE)
      dev.off()
    }
  )
  
  #funnel
  
  funnel_<-reactive({
    funnel_p<-funnel.meta(my_data(),studlab = TRUE, col="green",level = 0.95, contour = c(0.9, 0.95, 0.99))$col.contour
    legend(0.05, 0.05,
           c("0.1 > p > 0.05", "0.05 > p > 0.01", "< 0.01"), fill = funnel_p)
  })
  
  #egger
  egger_data <- reactive({
    if(input$model=="smd"){
      metacont(
        mean.e = Mean1, sd.e = Sd1, n.e = N1,
        mean.c = Mean2, sd.c = Sd2, n.c = N2,
        data=data(), studlab = Study, sm = "SMD",
        method.smd = smd_method_func(),
        method.tau = "REML",
        hakn = TRUE,
        title = "Model: SMD - Fixed & Random Effect" 
      )}
    
    else if(input$model=="cor"){
      metacor(r, n, data=data(), studlab = Study, 
              method.tau = "REML",  backtransf = cor_method_func(),
              hakn = TRUE,
              title = "Model: COR - Fixed & Random Effect")}
    
    else if(input$model=="dm"){
      metabin(event.e=True_Positive, n.e=False_Negative+True_Positive, event.c=False_Positive, n.c=True_Negative+False_Positive,
              data = data(), studlab = Study, sm = dm_method_func(), method = "Inverse", 
              method.tau = "REML",
              hakn = TRUE,
              title = "Model: DM - Fixed & Random Effect")}
  })
  
  
  egger <- reactive({
    metabias(egger_data(), method.bias = "linreg", k.min=5)
  })
  
  #download funnel
  
  output$funnel_dl<-downloadHandler(
    filename = function() {
      if (input$dl_methode_funnel=="png") paste("funnel", ".png", sep="")
      else paste("funnel", ".pdf", sep="")
    },
    content = function(file) {
      if (input$dl_methode_funnel=="png") png(file)
      else pdf(file)
      funnel_p<-funnel.meta(my_data(),studlab = TRUE, col="green",level = 0.95, contour = c(0.9, 0.95, 0.99))$col.contour
      legend(0.05, 0.05,
             c("0.1 > p > 0.05", "0.05 > p > 0.01", "< 0.01"), fill = funnel_p)
      dev.off()
    }
  )
  
  ###outputs
  
  #data table
  output$dataTable <- renderDataTable({
    data()
  })
  
  #summary 
  output$summary <- renderPrint({if (input$model=="smd") {smd_sum()}
    else if (input$model=="cor") {cor_sum()}
    else if (input$model=="dm")  {dm_sum()}
  })
  
  #publication bias
  output$fsn <- renderPrint({
    fsn_()
  })
  
  #forest
  output$forest_FE<-renderPlot({
    forest_FE()
  }, alt = "forest plot fixed model")
  
  
  
  output$forest_RE<-renderPlot({
    forest_RE()
  } , alt = "forest plot random model")
  
  #drapery
  output$drapery_pl<-renderPlot({
    drapery_pl()
  },alt = "drapery plot")
  
  #funnel
  output$funnel_<-renderPlot({
    funnel_()
  },alt = "funnel plot")
  
  #egger  
  
  output$egger <- renderPrint({
    egger()
  })
  
  #subgroup
  
  output$subg<-renderPlot({
    if (input$subgroup_check==TRUE) boxplot(TE ~ subgroup, main = "Subgroup BoxPlot", ylab ="Effect Size", xlab ="Subgroups", data = my_data())
  })
  
  #download box
  
  output$boxplot_dl<-downloadHandler(
    filename = function() {
      if (input$dl_methode_box=="png") paste("boxplot", ".png", sep="")
      else paste("boxplot", ".pdf", sep="")
    },
    content = function(file) {
      if (input$dl_methode_box=="png") png(file)
      else pdf(file)
      boxplot(TE ~ subgroup, main = "Subgroup BoxPlot", ylab ="Effect Size", xlab ="Subgroups", data = my_data())
      dev.off()
    }
  )
  output$meta_regression<-renderPrint({
    if (input$subgroup_check==TRUE) metareg(my_data(),subgroup)
  })
  
  
  
  
  ####examples
  data_ <- reactive({
    if(input$model_=="smd"){
      read_excel('models/sub/SMD.xlsx')}
    else if(input$model_=="cor"){
      read_excel('models/sub/COR.xlsx')}
    else if(input$model_=="dm"){
      read_excel('models/sub/DM.xlsx')}
  })
  
  
  output$first_ <- renderDataTable({
    data_()
  })
  
  ###cite metamar
  
  observeEvent(!input$cite, {
    
    toggle(id="div_cite", anim = TRUE)
    
  })
  observeEvent(!input$ref, {
    
    toggle(id="div_ref", anim = TRUE)
    
  })
  observeEvent(!input$selfref, {
    
    toggle(id="div_selfref", anim = TRUE)
    
  })
  observeEvent(!input$mail, {
    
    toggle(id="div_mail", anim = TRUE)
    
  })
  observeEvent(!input$lic, {
    
    toggle(id="div_lic", anim = TRUE)
    
  })
  
  output$lic<-renderUI({
    if (input$lic){
      HTML("<style> div {text-align: left;} </style><div>Meta-Mar - © Copyright 2018 - Ashkan Beheshti</div>")
    }})
  
  
  #effect size calculator
  #smd
  es_type_smd<-reactive({
    if (input$es_type_smd=="d") return("d")
    else if (input$es_type_smd=="g") return("g")
    else if (input$es_type_smd=="r") return("r")
    else if (input$es_type_smd=="or") return("or")
    else if (input$es_type_smd=="logit") return("logit")
    
  })
  output$esc_mean_sd<-renderPrint({
    
    esc_mean_sd(grp1m = input$grp1m, grp2m = input$grp2m, 
                grp1sd = input$grp1sd, grp2sd = input$grp2sd, 
                grp1n = input$grp1n, grp2n = input$grp2n, es.type = es_type_smd())
  })
  #sme
  es_type_sme<-reactive({
    if (input$es_type_sme=="d") return("d")
    else if (input$es_type_sme=="g") return("g")
    else if (input$es_type_sme=="r") return("r")
    else if (input$es_type_sme=="or") return("or")
    else if (input$es_type_sme=="logit") return("logit")
    
  })
  output$esc_mean_se<-renderPrint({
    
    esc_mean_se(grp1m = input$grp1me, grp2m = input$grp2me, 
                grp1se = input$grp1se, grp2se = input$grp2se, 
                grp1n = input$grp1ne, grp2n = input$grp2ne, es.type = es_type_sme())
  })
  
  #r
  es_type_r<-reactive({
    if (input$es_type_r=="d") return("d")
    else if (input$es_type_r=="g") return("g")
    else if (input$es_type_r=="r") return("r")
    else if (input$es_type_r=="or") return("or")
    else if (input$es_type_r=="logit") return("logit")
    
  })
  output$esc_r<-renderPrint({
    
    esc_rpb(r = input$rpb, grp1n = input$rpb1n, grp2n = input$rpb2n, es.type = es_type_r())
  })
  
  #pr
  es_type_pr<-reactive({
    if (input$es_type_pr=="d") return("d")
    else if (input$es_type_pr=="g") return("g")
    else if (input$es_type_pr=="r") return("r")
    else if (input$es_type_pr=="or") return("or")
    else if (input$es_type_pr=="logit") return("logit")
    
  })
  output$esc_pr<-renderPrint({
    
    esc_rpb(p = input$pr, grp1n = input$pr1n, grp2n = input$pr2n, es.type = es_type_pr())
  })
  
  #chsq
  es_type_ch<-reactive({
    if (input$es_type_ch=="d") return("d")
    else if (input$es_type_ch=="g") return("g")
    else if (input$es_type_ch=="r") return("r")
    else if (input$es_type_ch=="or") return("or")
    else if (input$es_type_ch=="logit") return("logit")
    
  })
  output$esc_chisq<-renderPrint({
    
    esc_chisq(chisq = input$chisq, totaln = input$totaln_ch, 
              es.type = es_type_ch())
  })
  
  #pchsq
  es_type_pch<-reactive({
    if (input$es_type_pch=="d") return("d")
    else if (input$es_type_pch=="g") return("g")
    else if (input$es_type_pch=="r") return("r")
    else if (input$es_type_pch=="or") return("or")
    else if (input$es_type_pch=="logit") return("logit")
    
  })
  output$esc_pchisq<-renderPrint({
    
    esc_chisq(p = input$pch, totaln = input$totaln_pch, 
              es.type = es_type_pch())
  })
  
  #t
  es_type_t<-reactive({
    if (input$es_type_t=="d") return("d")
    else if (input$es_type_t=="g") return("g")
    else if (input$es_type_t=="r") return("r")
    else if (input$es_type_t=="or") return("or")
    else if (input$es_type_t=="logit") return("logit")
    
  })
  output$esc_t<-renderPrint({
    
    esc_t(t = input$t, totaln = input$totaln_t, 
          es.type = es_type_t())
  })
  
  #pt
  es_type_pt<-reactive({
    if (input$es_type_pt=="d") return("d")
    else if (input$es_type_pt=="g") return("g")
    else if (input$es_type_pt=="r") return("r")
    else if (input$es_type_pt=="or") return("or")
    else if (input$es_type_pt=="logit") return("logit")
    
  })
  output$esc_pt<-renderPrint({
    
    esc_t(p = input$pt, totaln = input$totaln_pt, 
          es.type = es_type_pt())
  })
  
  #anova
  es_type_f<-reactive({
    if (input$es_type_f=="d") return("d")
    else if (input$es_type_f=="g") return("g")
    else if (input$es_type_f=="r") return("r")
    else if (input$es_type_f=="or") return("or")
    else if (input$es_type_f=="logit") return("logit")
    
  })
  output$esc_f<-renderPrint({
    
    esc_f(f = input$f, grp1n=input$f1n, grp2n = input$f2n, 
          es.type = es_type_f())
  })
  
  #2x2
  es_type_dm<-reactive({
    if (input$es_type_dm=="d") return("d")
    else if (input$es_type_dm=="g") return("g")
    else if (input$es_type_dm=="r") return("r")
    else if (input$es_type_dm=="or") return("or")
    else if (input$es_type_dm=="logit") return("logit")
    
  })
  output$esc_2x2<-renderPrint({
    
    esc_2x2(grp1yes = input$grp1yes, grp1no = input$grp1no, 
            grp2yes = input$grp1yes, grp2no = input$grp2no, 
            es.type = es_type_dm())
  })
  
  
  #regr ub
  es_type_ub<-reactive({
    if (input$es_type_ub=="d") return("d")
    else if (input$es_type_ub=="g") return("g")
    else if (input$es_type_ub=="r") return("r")
    else if (input$es_type_ub=="or") return("or")
    else if (input$es_type_ub=="logit") return("logit")
    
  })
  output$esc_B<-renderPrint({
    
    esc_B(input$bb1, input$bb2, input$bb3, input$bb4,
          es.type = es_type_ub())
  })
  
  #regr sb
  es_type_sb<-reactive({
    if (input$es_type_sb=="d") return("d")
    else if (input$es_type_sb=="g") return("g")
    else if (input$es_type_sb=="r") return("r")
    else if (input$es_type_sb=="or") return("or")
    else if (input$es_type_sb=="logit") return("logit")
    
  })
  output$esc_beta<-renderPrint({
    
    esc_beta(input$b1, input$b2, input$b3, input$b4,
             es.type = es_type_sb())
  })
  
  
  options(shiny.sanitize.errors = TRUE)
  
} # server


# Create Shiny object
shinyApp(ui = ui, server = server)