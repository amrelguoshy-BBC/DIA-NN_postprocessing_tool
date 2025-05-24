ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      /* Main background with scientific theme */
      body {
        background: linear-gradient(rgba(255,255,255,0.9), rgba(255,255,255,0.9)), 
                    url('https://example.com/proteomics-bg.jpg');
        background-size: cover;
        background-attachment: fixed;
        font-family: 'Roboto', sans-serif;
      }
      
      /* Title styling */
      .title-container {
        background-color: rgba(255,255,255,0.95);
        padding: 20px;
        border-radius: 10px;
        box-shadow: 0 4px 12px rgba(0,0,0,0.1);
        margin-bottom: 25px;
        border-left: 5px solid #3498db;
      }
      .title {
        text-align: center;
        color: #2c3e50;
        font-weight: 700;
        font-size: 28px;
        margin-bottom: 8px;
      }
      .subtitle {
        text-align: center;
        color: #2c3e50;
        font-size: 16px;
        font-weight: 400;
      }
      
      /* Panel styling */
      .well {
        background-color: rgba(255,255,255,0.97);
        border-radius: 8px;
        box-shadow: 0 4px 12px rgba(0,0,0,0.08);
        margin-bottom: 20px;
        border-top: 3px solid #3498db;
        padding: 20px;
      }
      
      /* Table containment */
      .dataTables_wrapper {
        width: 100% !important;
        margin: 0 auto;
      }
      .dataTables_scroll {
        overflow-x: auto !important;
        width: 100% !important;
      }
      #pep_info1 {
        width: 100% !important;
        margin: 0;
      }
      .shiny-table-output {
        max-width: 100%;
      }
      
      /* Tab styling */
      .nav-tabs {
        border-bottom: 2px solid #e0e0e0;
        margin-bottom: 0;
      }
      .nav-tabs > li > a {
        color: #2c3e50;
        font-weight: 500;
        background-color: rgba(240,248,255,0.8);
        margin-right: 5px;
        border-radius: 4px 4px 0 0 !important;
      }
      .nav-tabs > li.active > a {
        color: #2c3e50;
        background-color: rgba(255,255,255,0.97);
        border-bottom: 3px solid #3498db;
      }
      
      /* Button styling */
      .btn {
        border-radius: 4px;
        font-weight: 500;
        transition: all 0.3s ease;
        padding: 8px 16px;
      }
      .btn-primary {
        background-color: #3498db;
        border-color: #2980b9;
      }
      .btn-primary:hover {
        background-color: #2980b9;
        transform: translateY(-2px);
        box-shadow: 0 4px 8px rgba(0,0,0,0.15);
      }
      
      /* Header styling */
      h3, h4, h5 {
        color: #2c3e50;
        border-bottom: 2px solid #e0e0e0;
        padding-bottom: 10px;
        margin-top: 0;
        font-weight: 600;
      }
      
      /* Log panel styling */
      .log-panel {
        max-height: 200px;
        overflow-y: auto;
        background-color: #f8f9fa;
        border-radius: 4px;
        padding: 12px;
        font-family: 'Courier New', monospace;
        font-size: 13px;
        border: 1px solid #e0e0e0;
      }
      
      /* Custom scrollbar */
      ::-webkit-scrollbar {
        width: 8px;
        height: 8px;
      }
      ::-webkit-scrollbar-thumb {
        background: #3498db;
        border-radius: 4px;
      }
    ")),
    # Add Roboto font from Google Fonts
    tags$link(href = "https://fonts.googleapis.com/css2?family=Roboto:wght@400;500;700&display=swap", rel = "stylesheet")
  ),
  
  div(class = "title-container",
      div(class = "title", "Shiny Application for Enhanced Post-Processing Analysis and Visualization of Quantitative Proteomics and Peptidomics by MS and DIA-NN analysis"),
      #div(class = "subtitle", "Mass Spectrometry Data Processing and Visualization")
  ),
  
  useShinyjs(),
  tabsetPanel(
    id = "tabs",
    type = "pills",
    
    # Tab 1: Spectrum Similarity
    tabPanel("Spectrum Similarity", icon = icon("chart-line"),
      fluidRow(
        column(width = 5,
          wellPanel(
            h4(icon("flask"), " Input Parameters"),
            radioButtons("lib_data_source", "Choose Library data source:",
             choices = c("Sample Data" = "sample",
                         "Upload from Computer" = "upload",
                         "Download from Google Drive" = "gdrive"),
             selected = "upload"),
            selectInput("lib_type", "Library Type", 
                      choices = c("DIA", "DDA" ),
                      width = "100%"),
            
            fluidRow(
              column(7, 
                actionButton("use_sample_lib", "Use Library Sample", 
                           class = "btn btn-primary btn-block",
                           icon = icon("database"))),
              column(5,
                actionButton("upload_own_lib", "Upload Library", 
                           class = "btn btn-primary btn-block",
                           icon = icon("upload")))
            ),
            uiOutput("file_ui_lib"),
            
            #hr(),
            
            fluidRow(
              column(7,
                radioButtons("DIAnn_report_data_source", "Choose DIA-NN report data source:",
              choices = c("Sample Data" = "sample",
                         "Upload from Computer" = "upload",
                         "Download from Google Drive" = "gdrive"),
             selected = "upload"),
                actionButton("use_sample_diann_report", "use DIA-NN Report Sample", 
                           class = "btn btn-primary btn-block",
                           icon = icon("file-import"))),
              column(5,
                actionButton("upload_own_diann_report", "Upload Report", 
                           class = "btn btn-primary btn-block",
                           icon = icon("upload")))
            ),
            uiOutput("file_ui_diann_report")
          )
        ),
        
        column(width = 3,
          wellPanel(
            h4(icon("sliders-h"), " Analysis Settings"),
            numericInput("tol", "Mass Tolerance (Da)", 
                       value = 0.05, min = 0, max = 0.5, step = 0.01,
                       width = "100%"),
            numericInput("base", "Intensity Threshold (%)", 
                       value = 0, min = 0, max = 100, step = 5,
                       width = "100%"),
            actionButton("spec_sim", "Calculate Similarity", 
                        class = "btn btn-primary btn-block",
                        icon = icon("calculator"))
          )
        ),
        
        column(width = 4,
          wellPanel(
            h4(icon("terminal"), " Process Log"),
            div(class = "log-panel",
                verbatimTextOutput("msg1"),
                verbatimTextOutput("msg2")
                            )
          ),
          
          #wellPanel(
              

            #h4(icon("chart-bar"), " Summary Statistics"),
            #plotOutput("summary_plot", height = "250px")
          #)
        )
      ),
      
      fluidRow(
        column(width = 12,
          wellPanel(
            style = "padding: 15px;",
            div(style = "overflow-x: auto;",
                shinycssloaders::withSpinner(
                  DTOutput('pep_info1'),
                  type = 6, 
                  color = "#3498db"
                )
            ),
          
          wellPanel(
            h4(icon("filter"), " Filter Results"),
            sliderInput("cos", "Cosine Similarity Range", 
                       0, 1, c(0,1), width = "100%"),
            downloadButton("download", "Export Results", 
                         class = "btn btn-success btn-block",
                         icon = icon("file-export"))
          )
          )
        )
      )
    ),

    # Tab 2: Spectrum Visualization
    tabPanel("Spectrum Visualization", icon = icon("image"), value = "tab2",
      fluidRow(
        column(4,
          wellPanel(
            selectInput("SM", "Select Sample", choices = NULL, selected = ""),
            div(class = "log-panel", verbatimTextOutput("msg4"))
          )
        ),
        column(4,
          wellPanel(
            selectInput("prt_acc", "Select Protein", choices = NULL, selected = ""),
            div(class = "log-panel", verbatimTextOutput("msg5"))
          )
        ),
        column(4,
          wellPanel(
            selectizeInput("pep_seq", "Select Peptide", choices = NULL, selected = ""),
            div(class = "log-panel", verbatimTextOutput("msg6"))
          )
        )
      ),
      fluidRow(
        column(width = 3, wellPanel(div(style = "overflow-x: auto;",
                shinycssloaders::withSpinner(plotOutput("fig1", height = "400px"))))),
        column(width = 3, wellPanel(div(style = "overflow-x: auto;",
                shinycssloaders::withSpinner(plotOutput("fig2", height = "400px"))))),
        column(width = 3, wellPanel(div(style = "overflow-x: auto;",
                shinycssloaders::withSpinner(plotOutput("fig3", height = "400px"))))),
        column(width = 3, wellPanel(div(style = "overflow-x: auto;",
                shinycssloaders::withSpinner(plotOutput("fig4", height = "400px")))))
      ),
      fluidRow(
        column(width = 8, offset = 2,
          wellPanel(
            plotOutput("fig5", height = "400px")
          )
        )
      )
    ),
    
    # Tab 3: Library Enhancement
    tabPanel("Library Enhancement", icon = icon("cogs"), value = "tab3",
      fluidRow(
        column(width = 4, 
          wellPanel(
            h4("Library 1 Configuration"),
            fluidRow(
              column(6, actionButton("use_sample_cos_sim_data_lib1", "Sample cos_sim1", class = "btn btn-primary btn-block")),
              column(6, actionButton("upload_own_cos_sim_data_lib1", "Upload cos_sim1", class = "btn btn-primary btn-block"))
            ),
            uiOutput("file_ui_cos_sim_data_lib1"),
            selectInput("lib1_type", "Library Type", choices = c("DDA", "DIA")),
            fluidRow(
              column(6, actionButton("use_sample_lib1", "Sample Library1", class = "btn btn-primary btn-block")),
              column(6, actionButton("upload_own_lib1", "Upload Library1", class = "btn btn-primary btn-block"))
            ),
            uiOutput("file_ui_lib1"),
            actionButton("compare", "Compare Libraries", 
                        class = "btn btn-primary btn-block",
                        style = "margin-top: 15px;")
          )
        ),
        column(width = 4,
          wellPanel(
            h4("Library 2 Configuration"),
            fluidRow(
              column(6, actionButton("use_sample_cos_sim_data_lib2", "Sample cos_sim2", class = "btn btn-primary btn-block")),
              column(6, actionButton("upload_own_cos_sim_data_lib2", "Upload cos_sim2", class = "btn btn-primary btn-block"))
            ),
            uiOutput("file_ui_cos_sim_data_lib2"),
            selectInput("lib2_type", "Library Type", choices = c("DIA", "DDA")),
            fluidRow(
              column(6, actionButton("use_sample_lib2", "Sample Library2", class = "btn btn-primary btn-block")),
              column(6, actionButton("upload_own_lib2", "Upload Library2", class = "btn btn-primary btn-block"))
            ),
            uiOutput("file_ui_lib2"),
            actionButton("retrieve", "Retrieve Spectra", 
                       class = "btn btn-primary btn-block",
                       style = "margin-top: 15px;")
          )
        ),
        column(width = 4,
          wellPanel(
            h4("Process Logs"),
            div(class = "log-panel",
                verbatimTextOutput("msg_cos1"),
                verbatimTextOutput("msg_lib1"),  
                verbatimTextOutput("msg_cos2"),
                verbatimTextOutput("msg_lib2")
            )
          )
        )
      ),
      fluidRow(
        column(width = 3, wellPanel(plotOutput("fig6", height = "350px"))),
        column(width = 6, wellPanel(plotOutput("fig7", height = "350px"))),
        column(width = 3, wellPanel(plotOutput("fig8", height = "350px")))
      ),
      fluidRow(
        column(width = 4, offset = 4,
          wellPanel(
            downloadButton("download2", "Download Enhanced Library", 
                         class = "btn btn-primary btn-block")
          )
        )
      )
    ),
    
    # Tab 4: Documentation
    tabPanel("Documentation",  icon = icon("book"), value = "tab4",
      fluidRow(
        column(width = 8, offset = 2,
          wellPanel(
            h3("Application Documentation"),
            selectInput("file_type", "Select File Type", 
                      choices = c("DDA Library", "DIA Library", "DIAnn Report", "cosine similarity from library1", "cosine similarity from library2")),
            div(class = "log-panel", verbatimTextOutput("msg7")),
            HTML("<div style='margin-top: 20px;'>
                 <h4>Application Guide</h4>
                 <p>This application provides advanced tools for quantitative urine proteomics analysis using mass spectrometry data:</p>
                 <ol>
                   <li><strong>Spectrum Similarity:</strong> Calculate spectral similarity scores between experimental and library spectra</li>
                   <li><strong>Spectrum Visualization:</strong> Compare and validate spectral matches visually</li>
                   <li><strong>Library Enhancement:</strong> Combine and optimize spectral libraries for improved analysis</li>
                 </ol>
                 <p style='margin-top: 20px;'><strong>Note:</strong> For optimal results, use high-quality DDA libraries and recent DIA-NN report files.</p>
                 </div>")
          )
        )
      )
    )
  )
)