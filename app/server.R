server <- function(input, output, session){
     options(shiny.maxRequestSize = 300 * 1024^2)

    global <- reactiveValues(diann_report = NULL, cosine_values_flt_no_rdn = NULL, diann_report_sample = NULL, diann_report_prt = NULL, diann_report_pep = NULL, proc_lib_notify = NULL, proc_diann_report_notify = NULL, reset_notify = NULL, lib_sample_dda_notify = NULL, lib_sample_dia_notify = NULL, lib_upload_dda_notify = NULL, lib_upload_dia_notify = NULL, lib_gdrive_dda_notify = NULL, lib_gdrive_dia_notify = NULL, diann_sample_dda_notify = NULL, diann_sample_dda_data = NULL, diann_sample_dia_notify = NULL, diann_sample_dia_data = NULL, diann_upload_notify = NULL, diann_upload_data = NULL, diann_gdrive_notify = NULL, diann_gdrive_data = NULL, start_proc = NULL, lib1_sample_dda_notify = NULL, lib1_sample_dia_notify = NULL, lib2_sample_dda_notify = NULL, lib2_sample_dia_notify = NULL, lib1_upload_dda_notify = NULL, lib1_upload_dia_notify = NULL, lib2_upload_dda_notify = NULL, lib2_upload_dia_notify = NULL, lib1_gdrive_dda_notify = NULL, lib1_gdrive_dia_notify = NULL, lib2_gdrive_dda_notify = NULL, lib2_gdrive_dia_notify = NULL, cos_sim_lib1_sample_notify= NULL, cos_sim_lib1_upload_notify = NULL, cos_sim_lib1_gdrive_notify = NULL, cos_sim_lib2_sample_notify = NULL, cos_sim_lib2_upload_notify = NULL, cos_sim_lib2_gdrive_notify = NULL)
  
  # Paths to the sample data RDS files
  sample_data_dda_path <- "data/sample_data_dda_lib.rds"
  sample_data_dia_path <- "data/sample_data_dia_lib.rds"
  sample_data_diann_report_dia_path <- "data/sample_data_diann_report_dia.rds"
  sample_data_diann_report_dda_path <- "data/sample_data_diann_report_dda.rds"
  sample_data_cos_sim_dda_path <- "data/sample_data_cosine_similarity_scores_dda.rds"
  sample_data_cos_sim_dia_path <- "data/sample_data_cosine_similarity_scores_dia.rds"
  


  # Function to reset Spectrum Similarity tab outputs
  reset_spectrum_similarity_outputs <- function() {
    # Clear the pep_info table
    output$pep_info1 <- DT::renderDataTable({NULL})
    # Reset select inputs
    updateSelectizeInput(session, "SM", choices = character(0), selected = "")
    updateSelectizeInput(session, "prt_acc", choices = character(0), selected = "")
    updateSelectizeInput(session, "pep_seq", choices = character(0), selected = "")
    output$msg4 <- renderText({NULL})
    output$msg5 <- renderText({NULL})
    output$msg6 <- renderText({NULL})
    
    # Clear plots in Spectrum Visualization tab
    output$fig1 <- renderPlot({NULL})
    output$fig2 <- renderPlot({NULL})
    output$fig3 <- renderPlot({NULL})
    output$fig4 <- renderPlot({NULL})
    output$fig5 <- renderPlot({NULL})
     output$msg1 <- renderText(NULL)
     output$msg2 <- renderText(NULL)
    # Reset cosine similarity filter slider
    updateSliderInput(session, "cos", value = c(0, 1))
  }
 ###
  observeEvent(input$lib_type, {
     reset_spectrum_similarity_outputs()
  })
  
  # Observe all data loading buttons and reset outputs
  observeEvent(input$lib_data_source, {
    reset_spectrum_similarity_outputs()
    # Rest of your existing use_sample_lib code...
  })

  observeEvent(input$DIAnn_report_data_source, {
    reset_spectrum_similarity_outputs()
    # Rest of your existing upload_own_diann_report code...
  })
  observeEvent(input$spec_sim, {
     reset_spectrum_similarity_outputs()
    
  })
  
  # Allow user to switch back to uploading their own data
  #observeEvent(input$lib_data_source == "lib_upload", {
    #use_sample_lib(FALSE)
    #output$msg1 <- renderText(NULL)
  #})
  #observeEvent(input$lib_data_source == "lib_gdrive", {
    #use_sample_lib(FALSE)
    #output$msg1 <- renderText(NULL)
  #})
  
 
  
# Ensure `add_log_message()` is defined (or replace it with a simple logging function)
add_log_message <- function(msg) {
    cat(msg, "\n")  # Log to console (or replace with actual logging method)
}
##################################
output$file_ui_lib <- renderUI({
    if (input$lib_data_source == "lib_sample") {
      tags$div(class = "alert alert-info", "Library Sample Data Loaded. Upload complete.")
    } else if(input$lib_data_source == "lib_upload") {
      fileInput("lib", "Upload your library file from computer", accept = c(".csv", ".tsv"))
    } else if(input$lib_data_source == "lib_gdrive") {
      textInput("fileID_lib", "Enter Google Drive File ID:")
    } 
  })
  

#######################################################################
#######################################################################
#######################################################################
# Reactive value to process and store library data (Optimized version)
lib_proc <- eventReactive(input$spec_sim,{
    #output$msg1 <- renderText(NULL)
    #output$msg2 <- renderText(NULL)
    req(input$lib_data_source)
    req(input$lib_type)
    # 2. Handle Sample Source
    if (input$lib_data_source == "lib_sample") {
        if (input$lib_type == "DDA") {
            global$lib_sample_dda_notify <- showNotification(
                  HTML("<div style='font-size:16px;'>
                  <i class='fas fa-cogs'></i> loading and processing sample DDA library data...
                  </div>"),
                  duration = NULL
                  ,
                  type = "message"
             )
             lib_sample_dda_data <- readRDS(sample_data_dda_path)
             lib_processed_data <- process_lib(lib_sample_dda_data, "DDA", input$base)
             on.exit(removeNotification(global$lib_sample_dda_notify))
        } else if (input$lib_type == "DIA"){
             global$lib_sample_dia_notify <- showNotification(
                  HTML("<div style='font-size:16px;'>
                  <i class='fas fa-cogs'></i> loading and processing sample DIA library data...
                  </div>"),
                  duration = NULL
                  ,
                  type = "message"
             )
             lib_sample_dia_data <- readRDS(sample_data_dia_path)
             lib_processed_data <- process_lib(lib_sample_dia_data, "DIA", input$base)
             on.exit(removeNotification(global$lib_sample_dia_notify))
       }
    }else if (input$lib_data_source == "lib_upload") {
          req(input$lib)
          #output$msg1 <- renderText(NULL)
          # Validate library file based on type
          lib_path <- input$lib$datapath
          if (input$lib_type == "DDA") {
             global$lib_upload_dda_notify <- showNotification(
                  HTML("<div style='font-size:16px;'>
                  <i class='fas fa-cogs'></i> loading and processing DDA data from user uploaded file...
                  </div>"),
                  duration = NULL
                  ,
                  type = "message"
             )
            is_DDA_lib_valid <- validate_DDA_lib(lib_path)
            if (is.character(is_DDA_lib_valid)) {
                add_log_message(is_DDA_lib_valid)
                output$msg1 <- renderText(is_DDA_lib_valid)  # Show error message in UI
                on.exit(removeNotification(global$lib_upload_dda_notify))
                return(NULL)  # Stop further processing
            }
            lib_upload_dda_data <- read_csv(lib_path)
            lib_processed_data <- process_lib(lib_upload_dda_data, input$lib_type, input$base)
            on.exit(removeNotification(global$lib_upload_dda_notify), add = TRUE)
        } else if (input$lib_type == "DIA"){
            global$lib_upload_dia_notify <- showNotification(
            HTML("<div style='font-size:16px;'>
            <i class='fas fa-cogs'></i> loading and processing DIA data from user uploaded file...
            </div>"),
            duration = NULL
            ,
            type = "message"
             )
            is_DIA_lib_valid <- validate_DIA_lib(lib_path)
            if (is.character(is_DIA_lib_valid)) {
                add_log_message(is_DIA_lib_valid)
                output$msg1 <- renderText(is_DIA_lib_valid)  # Show error message in UI
                on.exit(removeNotification(global$lib_upload_dia_notify))
                return(NULL)
            }
            lib_upload_dia_data <- read_tsv(lib_path)
            lib_processed_data <- process_lib(lib_upload_dia_data, input$lib_type, input$base)
           on.exit(removeNotification(global$lib_upload_dia_notify), add = TRUE)
       }
    }
    # 4. Handle Google Drive Source
     else if (input$lib_data_source == "lib_gdrive") {
        req(input$fileID_lib)
        if (input$lib_type == "DDA") {
            global$lib_gdrive_dda_notify <- showNotification(
            HTML("<div style='font-size:16px;'><i class='fas fa-cogs'></i> Loading and processing DDA library from Google Drive...</div>"),
              duration = NULL, type = "message"
            )
            drive_deauth()  # No login needed
            drive_download(as_id(input$fileID_lib), path = "user_uploaded.csv", overwrite = TRUE)
            is_DDA_lib_valid <- validate_DDA_lib("user_uploaded.csv")
            if (is.character(is_DDA_lib_valid)) {
                add_log_message(is_DDA_lib_valid)
                output$msg1 <- renderText(is_DDA_lib_valid)
                removeNotification(global$lib_gdrive_dda_notify)
                return(NULL)
            }
            lib_gdrive_dda_data <- read_csv("user_uploaded.csv")
            lib_processed_data <- process_lib(lib_gdrive_dda_data, "DDA", input$base)
            removeNotification(global$lib_gdrive_dda_notify)
        } else if (input$lib_type == "DIA") {
            global$lib_gdrive_dia_notify <- showNotification(
              HTML("<div style='font-size:16px;'><i class='fas fa-cogs'></i> Loading and processing DIA library from Google Drive...</div>"),
              duration = NULL, type = "message"
            )
            drive_deauth()  # No login needed
            drive_download(as_id(input$fileID_lib), path = "user_uploaded.tsv", overwrite = TRUE)
            is_DIA_lib_valid <- validate_DIA_lib("user_uploaded.tsv")
            if (is.character(is_DIA_lib_valid)) {
                add_log_message(is_DIA_lib_valid)
                output$msg1 <- renderText(is_DIA_lib_valid)
                removeNotification(global$lib_gdrive_dia_notify)
                return(NULL)
            }
            lib_gdrive_dia_data <- read_delim("user_uploaded.tsv")
            lib_processed_data <- process_lib(lib_gdrive_dia_data, "DIA", input$base)
            removeNotification(global$lib_gdrive_dia_notify)
        }
    }
    return(lib_processed_data)
})
##########################################################################
##########################################################################
##########################################################################

 ########################### 
  # Display message based on processed library data
  observeEvent(lib_proc(), {
    #req(input$spec_sim)
    lib_unique_pep <- length(unique(lib_proc()$ModifiedPeptide))
    lib_unique_pep_chr <- length(unique(paste(lib_proc()$ModifiedPeptide, lib_proc()$lib_PrecursorCharge)))
    lib_unique_prot <- length(unique(lib_proc()$UniprotID))
    lib_frg_ions <- length(unique(paste(lib_proc()$lib_FragmentMz, lib_proc()$lib_FragmentCharge)))
    output$msg1 <- renderText({
      paste("Your", input$lib_type, "spectral library has", lib_unique_pep, "distinct peptides in\n", lib_unique_pep_chr, "peptidoforms,", lib_unique_prot, "distinct precursor proteins and\n", lib_frg_ions, "distinct fragment ions")
    })
  })
  


################################################################################
################################################################################################################################################################
# Dynamically render the file input or a message based on the sample data usage for library
    output$file_ui_diann_report <- renderUI({
    if (input$DIAnn_report_data_source == "diann_sample") {
      tags$div(class = "alert alert-info", "DIAnn_report Sample Data will be used as DIAnn_report file example.")
    } else if(input$DIAnn_report_data_source == "diann_upload") {
      fileInput("diann_report", "Upload your DIAnn_report file from computer", accept = c(".csv", ".tsv"))
    } else if(input$DIAnn_report_data_source == "diann_gdrive") {
      textInput("fileID_diann", "Enter Google Drive File ID:")
    } 
  }) 
#####################################
# Reactive value to process and store library data (Optimized version)
diann_report_proc <- eventReactive(input$spec_sim,{
    #output$msg1 <- renderText(NULL)
    #output$msg2 <- renderText(NULL)
    req(input$DIAnn_report_data_source)
    req(input$lib_type)
    #req(lib_proc())
    # 2. Handle Sample Source
    if (input$DIAnn_report_data_source == "diann_sample") {
        if (input$lib_type == "DDA") {
            global$diann_sample_dda_notify <- showNotification(
                  HTML("<div style='font-size:16px;'>
                  <i class='fas fa-cogs'></i> loading and processing sample DIA-nn report data...
                  </div>"),
                  duration = NULL
                  ,
                  type = "message"
             )
             global$diann_sample_dda_data <- readRDS(sample_data_diann_report_dda_path)
             diann_processed_data <- process_diann_report(global$diann_sample_dda_data, input$base)
             on.exit(removeNotification(global$diann_sample_dda_notify))
        } else if (input$lib_type == "DIA"){
             global$diann_sample_dia_notify <- showNotification(
                  HTML("<div style='font-size:16px;'>
                  <i class='fas fa-cogs'></i> loading and processing sample DIA-nn report data...
                  </div>"),
                  duration = NULL
                  ,
                  type = "message"
             )
             global$diann_sample_dia_data <- readRDS(sample_data_diann_report_dia_path)
             diann_processed_data <- process_diann_report(global$diann_sample_dia_data, input$base)
             on.exit(removeNotification(global$diann_sample_dia_notify))
       }
    }else if (input$DIAnn_report_data_source == "diann_upload") {
          req(input$diann_report)
          #output$msg1 <- renderText(NULL)
          # Validate library file based on type
          diann_upload_path <- input$diann_report$datapath
             global$diann_upload_notify <- showNotification(
                  HTML("<div style='font-size:16px;'>
                  <i class='fas fa-cogs'></i> loading and processing DIA-nn report data from user uploaded file...
                  </div>"),
                  duration = NULL
                  ,
                  type = "message"
             )
            is_diann_report_valid <- validate_DIAnn_report(diann_upload_path)
            if (is.character(is_diann_report_valid)) {
                add_log_message(is_diann_report_valid)
                output$msg2 <- renderText(is_diann_report_valid)  # Show error message in UI
                on.exit(removeNotification(global$diann_upload_notify))
                return(NULL)  # Stop further processing
            }
            global$diann_upload_data <- read_delim(diann_upload_path)
            diann_processed_data <- process_diann_report(global$diann_upload_data,  input$base)
            on.exit(removeNotification(global$diann_upload_notify), add = TRUE)
        
    }
    # 4. Handle Google Drive Source
     else if (input$DIAnn_report_data_source == "diann_gdrive") {
        req(input$fileID_diann)
        
        global$diann_gdrive_notify <- showNotification(
              HTML("<div style='font-size:16px;'><i class='fas fa-cogs'></i> Loading and processing DIA-nn report from Google Drive...</div>"),
              duration = NULL, type = "message"
            )
            drive_deauth()  # No login needed
            drive_download(as_id(input$fileID_diann), path = "user_uploaded.tsv", overwrite = TRUE)

            is_diann_report_valid <- validate_DIAnn_report("user_uploaded.tsv")
            if (is.character(is_diann_report_valid)) {
                add_log_message(is_diann_report_valid)
                output$msg2 <- renderText(is_diann_report_valid)
                removeNotification(global$diann_gdrive_notify)
                return(NULL)
            }
            
            global$diann_gdrive_data <- read_delim("user_uploaded.tsv")
            diann_processed_data <- process_diann_report(global$diann_gdrive_data, input$base)
            removeNotification(global$diann_gdrive_notify)
    }
    return(diann_processed_data)
})
#######
# Display message based on processed DIA-NN report data
  observeEvent(diann_report_proc(), {
    diann_report_unique_pep <- length(unique(diann_report_proc()$ModifiedPeptide))
    diann_report_unique_pep_chr <- length(unique(paste(diann_report_proc()$ModifiedPeptide, diann_report_proc()$diann_report_PrecursorCharge)))
    diann_report_unique_prot <- length(unique(diann_report_proc()$UniprotID))
    diann_report_frg_ions <- length(unique(paste(diann_report_proc()$diann_report_FragmentMz, diann_report_proc()$diann_report_FragmentCharge)))
    output$msg2 <- renderText({
      paste("'report.tsv' file created by DIA-NN has", diann_report_unique_pep, "distinct peptides in\n", diann_report_unique_pep_chr, "peptidoforms,", diann_report_unique_prot, "distinct precursor proteins and\n", diann_report_frg_ions, "distinct fragment ions")
    })
  })
##############################################################
###########fun3,4
        calculated_cos_sim <- eventReactive(input$spec_sim,{
             req(diann_report_proc())
             req(lib_proc())
             global$start_proc <- showNotification(
             HTML("<div style='font-size:16px;'>
           <i class='fas fa-cogs'></i> start aligning the library and diann report data. then, calculating the consine similarity scores...
           </div>"),
           duration = NULL
           ,
           type = "message"
           )
             aligned_diann_report_lib_frgs <- align_diann_report_lib_frgs(diann_report_proc(), lib_proc(), input$tol)
             calculated_cos_sim <- calculate_cos_sim(aligned_diann_report_lib_frgs)
            return(calculated_cos_sim)
    })
        #####
        observeEvent(calculated_cos_sim(), {
        global$calculated_cos_sim_no_rdn <- calculated_cos_sim()%>%
        filter(!is.na(lib_PrecursorCharge))%>%
        select(File.Name, UniprotID, ModifiedPeptide, PrecursorCharge = lib_PrecursorCharge, cosine_similarity)%>%
        distinct(.)
        output$pep_info1 <- DT::renderDataTable({
        global$calculated_cos_sim_no_rdn
        }, server = FALSE)
      on.exit(removeNotification(global$start_proc))
      })
      #########
       ######flt
       cosine_values_flt <- reactive({
             cosine_values_flt <- calculated_cos_sim()%>%
             filter(between(cosine_similarity,input$cos[1], input$cos[2]))
             return(cosine_values_flt)
       })
       ######
      observeEvent(cosine_values_flt(), {
       global$cosine_values_flt_no_rdn <- cosine_values_flt()%>%
             filter(!is.na(lib_PrecursorCharge))%>%
             transmute(File.Name, UniprotID, ModifiedPeptide, PrecursorCharge = lib_PrecursorCharge, 
cosine_similarity)%>%
             distinct(.)
       output$pep_info1 <- DT::renderDataTable({
            global$cosine_values_flt_no_rdn
           }, server = TRUE)
      })
     ##cosine_values_flt_no_rdn need to be global reactive variable to be called from the following block
       output$download <- downloadHandler(
          filename = function() {
              paste0("cosine_similarity_scores", ".tsv")
           },
          content = function(file) {
          vroom::vroom_write(global$cosine_values_flt_no_rdn, file)
         }
  )
 
#############
 observeEvent(cosine_values_flt(),{
               choices1 <- unique(cosine_values_flt()$File.Name)
               updateSelectizeInput(inputId = "SM", choices = choices1,  selected = choices1[1])
              output$msg4 <- renderText({
                 paste0("samples was updated; please select one to compare between DDA and DIA data")
              })
              choices2 <- c("----------", unique(cosine_values_flt()[cosine_values_flt()$File.Name == input$SM,]$UniprotID))
               updateSelectizeInput(inputId = "prt_acc", choices = choices2,  selected = "----------")
              output$msg5 <- renderText({
                 paste0("protein acccesion list was updated; please select one protein to filter")
              })
              choices3 <- unique(cosine_values_flt()[cosine_values_flt()$File.Name == input$SM,]$ModifiedPeptide)
               updateSelectizeInput(inputId = "pep_seq", choices = choices3, selected = "")
              output$msg6 <- renderText({
                 paste0("peptide sequence list was updated; please select one peptide to visualize")
              })
        })
##################
         #####
        cosine_values_SM_flt <- reactive({
               req(input$SM)
               filter(cosine_values_flt(), File.Name == input$SM)
          })
        #####
        cosine_values_prt_flt <- reactive({
               req(input$prt_acc)
               filter(cosine_values_SM_flt(), UniprotID == input$prt_acc)
          })
       #####
       cosine_values_pep_flt <- reactive({
               req(input$ModifiedPeptide)
               filter(cosine_values_prt_flt(), ModifiedPeptide == input$pep_seq)
          })
       ####
####################################################################################################################################################################
#-------------------------------
# 1. Plotting helper function
#-------------------------------
render_all_figures <- function(data_all, data_protein = NULL, data_peptide = NULL, key_expr) {
  
  # Reusable plotting function for a single figure
  plot_base <- function(x, y, xlab, ylab) {
    ggplot(data_all, aes_string(x, y)) +
      geom_point(color = "grey", shape = 21, alpha = 0.7) +
      {if (!is.null(data_protein)) geom_point(data = data_protein, aes_string(x, y), color = "red", size = 2)} +
      {if (!is.null(data_peptide)) geom_point(data = data_peptide, aes_string(x, y), color = "blue", size = 3)} +
      labs(x = xlab, y = ylab) +
      theme_bw() +
      theme(text = element_text(size = 16, face = "bold")) +
      theme(axis.text.x = element_text(angle = 90))
  }
  
  # All 4 plots
  output$fig1 <- renderCachedPlot({
    plot_base("RT", "iRT", "RT", "iRT")
  }, cacheKeyExpr = { key_expr })
  
  output$fig2 <- renderCachedPlot({
    plot_base("IM", "iIM", "IM", "iIM")
  }, cacheKeyExpr = { key_expr })
  
  output$fig3 <- renderCachedPlot({
    plot_base("Q.Value", "PEP", "Q.Value", "PEP")
  }, cacheKeyExpr = { key_expr })
  
  output$fig4 <- renderCachedPlot({
    plot_base("Ms1.Area", "Precursor.Quantity", "Ms1.Area", "Precursor.Quantity")
  }, cacheKeyExpr = { key_expr })
}
####################################################################################################################################################################
       observe({
           req(input$SM)
           global$diann_report_SM <- NULL
           global$diann_report_prt <- NULL
           global$diann_report_pep <- NULL 
           updateSelectizeInput(inputId = "pep_seq", choices = unique(cosine_values_SM_flt()$ModifiedPeptide), selected = "")
           updateSelectizeInput(inputId = "prt_acc", choices = c("----------", unique(cosine_values_SM_flt()$UniprotID)), selected = "----------")
           ########
#####################################################################################################################################################
           isolate({
               if (is.null(global$diann_report_SM)) {
                   global$diann_report_SM <- if(input$DIAnn_report_data_source == "diann_sample") {
                   if (input$lib_type == "DDA") {
                       req(global$diann_sample_dda_data)
global$diann_sample_dda_data[global$diann_sample_dda_data$File.Name == input$SM, ]
                    } else if(input$lib_type == "DIA"){
                      req(global$diann_sample_dia_data)
             global$diann_sample_dia_data[global$diann_sample_dia_data$File.Name == input$SM, ]
                    }
                } else if(input$DIAnn_report_data_source == "diann_upload"){
                    req(global$diann_upload_data)
                    global$diann_upload_data[global$diann_upload_data$File.Name == input$SM, ]
                } else if(input$DIAnn_report_data_source == "diann_gdrive"){
                    req(global$diann_gdrive_data)
                    global$diann_gdrive_data[global$diann_gdrive_data$File.Name == input$SM, ]
               }
           render_all_figures(
                data_all = global$diann_report_SM,
                data_protein = NULL,
                data_peptide = NULL,
                key_expr = paste(input$SM, Sys.time())
           )
      }
  })
})
       ##
       observe({
  req(input$prt_acc)

  global$diann_report_prt <- NULL  # Always reset
  global$diann_report_pep <- NULL  # Reset peptide info as well

  if (input$prt_acc == "----------") {
    updateSelectizeInput(inputId = "pep_seq", choices = unique(cosine_values_SM_flt()$ModifiedPeptide), selected = "")
    render_all_figures(
      data_all = global$diann_report_SM,
      data_protein = NULL,
      data_peptide = NULL,
      key_expr = paste(input$SM, Sys.time())
    )
  } else {
    updateSelectizeInput(inputId = "pep_seq", choices = unique(cosine_values_prt_flt()$ModifiedPeptide), selected = "")
    
    global$diann_report_prt <- global$diann_report_SM[global$diann_report_SM$Protein.Ids == input$prt_acc, ]
    
    render_all_figures(
      data_all = global$diann_report_SM,
      data_protein = global$diann_report_prt,
      data_peptide = NULL,
      key_expr = paste(input$SM, input$prt_acc, Sys.time())
    )
  }
})

##############
cosine_values_flt_l <- reactive({
        selected_lib_data <- cosine_values_flt() %>%
                  transmute(File.Name, ModifiedPeptide, PrecursorCharge = lib_PrecursorCharge, UniprotID, FragmentMz = lib_FragmentMz,  RelativeIntensity = - lib_RelativeIntensity,  FragmentType = lib_FragmentType, FragmentSeriesNumber = lib_FragmentSeriesNumber, cosine_similarity)
               selected_diann_report_data <- cosine_values_flt() %>% 
                  transmute(File.Name, ModifiedPeptide, PrecursorCharge = diann_report_PrecursorCharge, UniprotID, FragmentMz = diann_report_FragmentMz, RelativeIntensity = diann_report_RelativeIntensity, FragmentType = diann_report_FragmentType, FragmentSeriesNumber = diann_report_FragmentSeriesNumber, cosine_similarity)
               cosine_values_flt_l <- bind_rows("Library" = selected_lib_data, "DIAnn_report" = selected_diann_report_data, .id = "cat")
           })

selected_pep_data <- reactive({
               req(input$pep_seq)
               selected_pep_data <- cosine_values_flt_l() %>%
               filter(ModifiedPeptide == input$pep_seq,  File.Name == input$SM)%>%
                mutate(File.Name = str_wrap(File.Name, width = 10))
             
          })

observeEvent(selected_pep_data(), {
  req(input$pep_seq)

  output$fig5 <- renderCachedPlot({
    ggplot(selected_pep_data()[!is.na(selected_pep_data()$PrecursorCharge),],
           aes(FragmentMz, RelativeIntensity, col = as.factor(cat))) +
      geom_col(width = 1.5, position = "dodge") +
      geom_hline(yintercept = 0) +
      scale_color_manual(values = c("red", "darkblue")) +
      theme_bw() +
      theme(text = element_text(size = 18, face = "bold")) +
      ggtitle(paste(
        selected_pep_data()[!is.na(selected_pep_data()$PrecursorCharge), ]$UniprotID[1],
        "\n",
        selected_pep_data()[!is.na(selected_pep_data()$PrecursorCharge), ]$ModifiedPeptide[1]
      )) +
      theme(plot.title = element_text(size = 15, hjust = 0.5, face = "bold")) +
      theme(legend.title = element_blank()) +
      geom_label_repel(aes(label = paste(FragmentType, FragmentSeriesNumber, sep = "")), size = 4) +
      facet_wrap(
        paste("Cosine similarity score = ", cosine_similarity) ~
        paste("Peptide Charge = ", PrecursorCharge),
        labeller = label_wrap_gen(width = 35)
      )
  }, cacheKeyExpr = {
    paste(input$SM, input$pep_seq, Sys.time())
  })

  global$diann_report_pep <- global$diann_report_SM[global$diann_report_SM$Stripped.Sequence == input$pep_seq, ]

  render_all_figures(
    data_all = global$diann_report_SM,
    data_protein = global$diann_report_prt,
    data_peptide = global$diann_report_pep,
    key_expr = paste(input$SM, input$pep_seq, Sys.time())
  )
})
################################################################
################################################################
################################################################
################################################################
################################################################
################################################################
# Dynamically render the file input or a message based on the sample data usage for library
output$file_ui_lib1 <- renderUI({
    if (input$lib1_data_source == "lib1_sample") {
      tags$div(class = "alert alert-info", "1st Sample library Loaded. Upload complete.")
    } else if(input$lib1_data_source == "lib1_upload") {
      fileInput("lib1", "Upload your 1st library file from computer", accept = c(".csv", ".tsv"))
    } else if(input$lib1_data_source == "lib1_gdrive") {
      textInput("fileID_lib1", "Enter Google Drive File ID for the 1st library:")
    } 
  })
################################################################################
output$file_ui_lib2 <- renderUI({
    if (input$lib2_data_source == "lib2_sample") {
      tags$div(class = "alert alert-info", "2nd Sample library Loaded. Upload complete.")
    } else if(input$lib2_data_source == "lib2_upload") {
      fileInput("lib2", "Upload your 2nd library file from computer", accept = c(".csv", ".tsv"))
    } else if(input$lib2_data_source == "lib2_gdrive") {
      textInput("fileID_lib2", "Enter Google Drive File ID for the 2nd library:")
    } 
  })
################################################################################

# Load sample data when the action button is clicked
# Reactive value to process and store library data (Optimized version)
lib1_proc <- eventReactive(input$retrieve,{
    #output$msg1 <- renderText(NULL)
    #output$msg2 <- renderText(NULL)
    req(input$lib1_data_source)
    req(input$lib1_type)
    # 2. Handle Sample Source
    if (input$lib1_data_source == "lib1_sample") {
        if (input$lib1_type == "DDA") {
            global$lib1_sample_dda_notify <- showNotification(
                  HTML("<div style='font-size:16px;'>
                  <i class='fas fa-cogs'></i> loading and processing sample DDA library data...
                  </div>"),
                  duration = NULL
                  ,
                  type = "message"
             )
             lib1_sample_dda_data <- readRDS(sample_data_dda_path)
             lib1_processed_data <- process_lib(lib1_sample_dda_data, "DDA", input$base)
             on.exit(removeNotification(global$lib1_sample_dda_notify))
        } else if (input$lib1_type == "DIA"){
             global$lib1_sample_dia_notify <- showNotification(
                  HTML("<div style='font-size:16px;'>
                  <i class='fas fa-cogs'></i> loading and processing sample DIA library data...
                  </div>"),
                  duration = NULL
                  ,
                  type = "message"
             )
             lib1_sample_dia_data <- readRDS(sample_data_dia_path)
             lib1_processed_data <- process_lib(lib1_sample_dia_data, "DIA", input$base)
             on.exit(removeNotification(global$lib1_sample_dia_notify))
       }
    }else if (input$lib1_data_source == "lib1_upload") {
          req(input$lib1)
          #output$msg1 <- renderText(NULL)
          # Validate library file based on type
          lib1_path <- input$lib1$datapath
          if (input$lib1_type == "DDA") {
             global$lib1_upload_dda_notify <- showNotification(
                  HTML("<div style='font-size:16px;'>
                  <i class='fas fa-cogs'></i> loading and processing DDA data from user uploaded file...
                  </div>"),
                  duration = NULL
                  ,
                  type = "message"
             )
            is_DDA_lib1_valid <- validate_DDA_lib(lib1_path)
            if (is.character(is_DDA_lib1_valid)) {
                add_log_message(is_DDA_lib1_valid)
                output$msg1 <- renderText(is_DDA_lib1_valid)  # Show error message in UI
                on.exit(removeNotification(global$lib1_upload_dda_notify))
                return(NULL)  # Stop further processing
            }
            lib1_upload_dda_data <- read_csv(lib1_path)
            lib1_processed_data <- process_lib(lib1_upload_dda_data, "DDA", input$base)
            on.exit(removeNotification(global$lib1_upload_dda_notify), add = TRUE)
        } else if (input$lib1_type == "DIA"){
            global$lib1_upload_dia_notify <- showNotification(
            HTML("<div style='font-size:16px;'>
            <i class='fas fa-cogs'></i> loading and processing DIA data from user uploaded file...
            </div>"),
            duration = NULL
            ,
            type = "message"
             )
            is_DIA_lib1_valid <- validate_DIA_lib(lib1_path)
            if (is.character(is_DIA_lib1_valid)) {
                add_log_message(is_DIA_lib1_valid)
                output$msg1 <- renderText(is_DIA_lib1_valid)  # Show error message in UI
                on.exit(removeNotification(global$lib1_upload_dia_notify))
                return(NULL)
            }
            lib1_upload_dia_data <- read_tsv(lib1_path)
            lib1_processed_data <- process_lib(lib1_upload_dia_data, "DIA", input$base)
           on.exit(removeNotification(global$lib1_upload_dia_notify), add = TRUE)
       }
    }
    # 4. Handle Google Drive Source
     else if (input$lib1_data_source == "lib1_gdrive") {
        req(input$fileID_lib1)
        if (input$lib1_type == "DDA") {
            global$lib1_gdrive_dda_notify <- showNotification(
            HTML("<div style='font-size:16px;'><i class='fas fa-cogs'></i> Loading and processing DDA library from Google Drive...</div>"),
              duration = NULL, type = "message"
            )
            drive_deauth()  # No login needed
            drive_download(as_id(input$fileID_lib1), path = "user_uploaded.csv", overwrite = TRUE)
            is_DDA_lib1_valid <- validate_DDA_lib("user_uploaded.csv")
            if (is.character(is_DDA_lib1_valid)) {
                add_log_message(is_DDA_lib1_valid)
                output$msg1 <- renderText(is_DDA_lib1_valid)
                removeNotification(global$lib1_gdrive_dda_notify)
                return(NULL)
            }
            lib1_gdrive_dda_data <- read_csv("user_uploaded.csv")
            lib1_processed_data <- process_lib(lib1_gdrive_dda_data, "DDA", input$base)
            removeNotification(global$lib1_gdrive_dda_notify)
        } else if (input$lib1_type == "DIA") {
            global$lib1_gdrive_dia_notify <- showNotification(
              HTML("<div style='font-size:16px;'><i class='fas fa-cogs'></i> Loading and processing DIA library from Google Drive...</div>"),
              duration = NULL, type = "message"
            )
            drive_deauth()  # No login needed
            drive_download(as_id(input$fileID_lib1), path = "user_uploaded.tsv", overwrite = TRUE)
            is_DIA_lib1_valid <- validate_DIA_lib("user_uploaded.tsv")
            if (is.character(is_DIA_lib1_valid)) {
                add_log_message(is_DIA_lib1_valid)
                output$msg1 <- renderText(is_DIA_lib1_valid)
                removeNotification(global$lib1_gdrive_dia_notify)
                return(NULL)
            }
            lib1_gdrive_dia_data <- read_delim("user_uploaded.tsv")
            lib1_processed_data <- process_lib(lib1_gdrive_dia_data, "DIA", input$base)
            removeNotification(global$lib1_gdrive_dia_notify)
        }
    }
    return(lib1_processed_data)
})
#############################################################################
# Load sample data when the action button is clicked
# Reactive value to process and store library data (Optimized version)
lib2_proc <- eventReactive(input$retrieve,{
    #output$msg1 <- renderText(NULL)
    #output$msg2 <- renderText(NULL)
    req(input$lib2_data_source)
    req(input$lib2_type)
    # 2. Handle Sample Source
    if (input$lib2_data_source == "lib2_sample") {
        if (input$lib2_type == "DDA") {
            global$lib2_sample_dda_notify <- showNotification(
                  HTML("<div style='font-size:16px;'>
                  <i class='fas fa-cogs'></i> loading and processing sample DDA library data...
                  </div>"),
                  duration = NULL
                  ,
                  type = "message"
             )
             lib2_sample_dda_data <- readRDS(sample_data_dda_path)
             lib2_processed_data <- process_lib(lib2_sample_dda_data, "DDA", input$base)
             on.exit(removeNotification(global$lib2_sample_dda_notify))
        } else if (input$lib2_type == "DIA"){
             global$lib2_sample_dia_notify <- showNotification(
                  HTML("<div style='font-size:16px;'>
                  <i class='fas fa-cogs'></i> loading and processing sample DIA library data...
                  </div>"),
                  duration = NULL
                  ,
                  type = "message"
             )
             lib2_sample_dia_data <- readRDS(sample_data_dia_path)
             lib2_processed_data <- process_lib(lib2_sample_dia_data, "DIA", input$base)
             on.exit(removeNotification(global$lib2_sample_dia_notify))
       }
    }else if (input$lib2_data_source == "lib2_upload") {
          req(input$lib2)
          #output$msg1 <- renderText(NULL)
          # Validate library file based on type
          lib2_path <- input$lib2$datapath
          if (input$lib2_type == "DDA") {
             global$lib2_upload_dda_notify <- showNotification(
                  HTML("<div style='font-size:16px;'>
                  <i class='fas fa-cogs'></i> loading and processing DDA data from user uploaded file...
                  </div>"),
                  duration = NULL
                  ,
                  type = "message"
             )
            is_DDA_lib2_valid <- validate_DDA_lib(lib2_path)
            if (is.character(is_DDA_lib2_valid)) {
                add_log_message(is_DDA_lib2_valid)
                output$msg1 <- renderText(is_DDA_lib2_valid)  # Show error message in UI
                on.exit(removeNotification(global$lib2_upload_dda_notify))
                return(NULL)  # Stop further processing
            }
            lib2_upload_dda_data <- read_csv(lib2_path)
            lib2_processed_data <- process_lib(lib2_upload_dda_data, input$lib2_type, input$base)
            on.exit(removeNotification(global$lib2_upload_dda_notify), add = TRUE)
        } else if (input$lib2_type == "DIA"){
            global$lib2_upload_dia_notify <- showNotification(
            HTML("<div style='font-size:16px;'>
            <i class='fas fa-cogs'></i> loading and processing DIA data from user uploaded file...
            </div>"),
            duration = NULL
            ,
            type = "message"
             )
            is_DIA_lib2_valid <- validate_DIA_lib(lib2_path)
            if (is.character(is_DIA_lib2_valid)) {
                add_log_message(is_DIA_lib2_valid)
                output$msg1 <- renderText(is_DIA_lib2_valid)  # Show error message in UI
                on.exit(removeNotification(global$lib2_upload_dia_notify))
                return(NULL)
            }
            lib2_upload_dia_data <- read_tsv(lib2_path)
            lib2_processed_data <- process_lib(lib2_upload_dia_data, input$lib2_type, input$base)
           on.exit(removeNotification(global$lib2_upload_dia_notify), add = TRUE)
       }
    }
    # 4. Handle Google Drive Source
     else if (input$lib2_data_source == "lib2_gdrive") {
        req(input$fileID_lib2)
        if (input$lib2_type == "DDA") {
            global$lib2_gdrive_dda_notify <- showNotification(
            HTML("<div style='font-size:16px;'><i class='fas fa-cogs'></i> Loading and processing DDA library from Google Drive...</div>"),
              duration = NULL, type = "message"
            )
            drive_deauth()  # No login needed
            drive_download(as_id(input$fileID_lib2), path = "user_uploaded.csv", overwrite = TRUE)
            is_DDA_lib2_valid <- validate_DDA_lib("user_uploaded.csv")
            if (is.character(is_DDA_lib2_valid)) {
                add_log_message(is_DDA_lib2_valid)
                output$msg1 <- renderText(is_DDA_lib2_valid)
                removeNotification(global$lib2_gdrive_dda_notify)
                return(NULL)
            }
            lib2_gdrive_dda_data <- read_csv("user_uploaded.csv")
            lib2_processed_data <- process_lib(lib2_gdrive_dda_data, "DDA", input$base)
            removeNotification(global$lib2_gdrive_dda_notify)
        } else if (input$lib2_type == "DIA") {
            global$lib2_gdrive_dia_notify <- showNotification(
              HTML("<div style='font-size:16px;'><i class='fas fa-cogs'></i> Loading and processing DIA library from Google Drive...</div>"),
              duration = NULL, type = "message"
            )
            drive_deauth()  # No login needed
            drive_download(as_id(input$fileID_lib2), path = "user_uploaded.tsv", overwrite = TRUE)
            is_DIA_lib2_valid <- validate_DIA_lib("user_uploaded.tsv")
            if (is.character(is_DIA_lib2_valid)) {
                add_log_message(is_DIA_lib2_valid)
                output$msg1 <- renderText(is_DIA_lib2_valid)
                removeNotification(global$lib2_gdrive_dia_notify)
                return(NULL)
            }
            lib2_gdrive_dia_data <- read_delim("user_uploaded.tsv")
            lib2_processed_data <- process_lib(lib2_gdrive_dia_data, "DIA", input$base)
            removeNotification(global$lib2_gdrive_dia_notify)
        }
    }
    return(lib2_processed_data)
})
###############################################################################
###############################################################################
################################################################################
output$file_ui_cos_sim_lib1 <- renderUI({
    if (input$cos_sim_lib1_data_source == "cos_sim_lib1_sample") {
      tags$div(class = "alert alert-info", "1st Sample cosine similarity file Loaded. Upload complete.")
    } else if(input$cos_sim_lib1_data_source == "cos_sim_lib1_upload") {
      fileInput("cos_sim_lib1", "Upload your 1st cosine similarity file from computer", accept = c(".csv", ".tsv"))
    } else if(input$cos_sim_lib1_data_source == "cos_sim_lib1_gdrive") {
      textInput("fileID_cos_sim_lib1", "Enter Google Drive File ID for the 1st cosine similarity file:")
    } 
  })
################################################################################
output$file_ui_cos_sim_lib2 <- renderUI({
    if (input$cos_sim_lib2_data_source == "cos_sim_lib2_sample") {
      tags$div(class = "alert alert-info", "2nd Sample cosine similarity file Loaded. Upload complete.")
    } else if(input$cos_sim_lib2_data_source == "cos_sim_lib2_upload") {
      fileInput("cos_sim_lib2", "Upload your 2nd cosine similarity file from computer", accept = c(".csv", ".tsv"))
    } else if(input$cos_sim_lib2_data_source == "cos_sim_lib2_gdrive") {
      textInput("fileID_cos_sim_lib2", "Enter Google Drive File ID for the 2nd cosine similarity file:")
    } 
  })
################################################################################
  # Load sample cosine similarity1 for library1 when the action button is clicked
cos_sim_lib1_proc <- eventReactive(input$compare,{
  if(input$cos_sim_lib1_data_source == "cos_sim_lib1_sample") {
    if(input$lib1_type == "DDA"){
       global$cos_sim_lib1_sample_notify <- showNotification(
           HTML("<div style='font-size:16px;'>
           <i class='fas fa-cogs'></i> Loading and processing 1st sample cosine similarity score file...
           </div>"),
            duration = NULL
            ,
            type = "message"
        )
        cos_sim_lib1_data <- readRDS(sample_data_cos_sim_dda_path)
   }
   else if(input$lib1_type == "DIA"){
       global$cos_sim_lib1_sample_notify <- showNotification(
           HTML("<div style='font-size:16px;'>
           <i class='fas fa-cogs'></i> Loading and processing 1st sample cosine similarity score file...
           </div>"),
            duration = NULL
        ,
        type = "message"
        )
      cos_sim_lib1_data <- readRDS(sample_data_cos_sim_dia_path)
    }
  }
  else if(input$cos_sim_lib1_data_source == "cos_sim_lib1_upload") {
         req(input$cos_sim_lib1)
         global$cos_sim_lib1_upload_notify <- showNotification(
           HTML("<div style='font-size:16px;'>
           <i class='fas fa-cogs'></i> Loading and processing 1st uploaded cosine similarity score file...
           </div>"),
            duration = NULL
            ,
            type = "message"
         )
         cos_sim_lib1_path <- input$cos_sim_lib1$datapath
         is_cos_sim_lib1_valid <- validate_cos_sim_data_lib(cos_sim_lib1_path)
            if (is.character(is_cos_sim_lib1_valid)) {
                add_log_message(is_cos_sim_lib1_valid)
                output$msg_cos1 <- renderText(is_cos_sim_lib1_valid)  # Show error message in UI
                on.exit(removeNotification(global$cos_sim_lib1_upload_notify))
                return(NULL)  # Stop further processing
            }
         cos_sim_lib1_data <- read_file_based_on_extension(cos_sim_lib1_path)
         on.exit(removeNotification(global$cos_sim_lib1_upload_notify))
     }
     else if (input$cos_sim_lib1_data_source == "cos_sim_lib1_gdrive") {
        req(input$fileID_cos_sim_lib1)
        global$cos_sim_lib1_gdrive_notify <- showNotification(
        HTML("<div style='font-size:16px;'><i class='fas fa-cogs'></i> Loading and processing 1st cosine similarity score file from Google Drive...</div>"),
              duration = NULL, type = "message"
            )
            drive_deauth()  # No login needed
            drive_download(as_id(input$fileID_cos_sim_lib1), path = "user_uploaded.tsv", overwrite = TRUE)
            is_cos_sim_lib1_valid <- validate_cos_sim_data_lib("user_uploaded.tsv")
            if (is.character(is_cos_sim_lib1_valid)) {
                add_log_message(is_cos_sim_lib1_valid)
                output$msg_cos1 <- renderText(is_cos_sim_lib1_valid)  # Show error message in UI
                on.exit(removeNotification(global$cos_sim_lib1_gdrive_notify))
                return(NULL)  # Stop further processing
            }
            cos_sim_lib1_data <- read_file_based_on_extension("user_uploaded.tsv")
            on.exit(removeNotification(global$cos_sim_lib1_gdrive_notify))
     }
     return(cos_sim_lib1_data)
  })
#########################################################################
  # Load sample cosine similarity1 for library1 when the action button is clicked
cos_sim_lib2_proc <- eventReactive(input$compare,{
  if(input$cos_sim_lib2_data_source == "cos_sim_lib2_sample") {
    if(input$lib2_type == "DDA"){
       global$cos_sim_lib2_sample_notify <- showNotification(
           HTML("<div style='font-size:16px;'>
           <i class='fas fa-cogs'></i> Loading and processing 2nd sample cosine similarity score file...
           </div>"),
            duration = NULL
            ,
            type = "message"
        )
        cos_sim_lib2_data <- readRDS(sample_data_cos_sim_dda_path)
   }
   else if(input$lib2_type == "DIA"){
       global$cos_sim_lib2_sample_notify <- showNotification(
           HTML("<div style='font-size:16px;'>
           <i class='fas fa-cogs'></i> Loading and processing 2nd sample cosine similarity score file...
           </div>"),
            duration = NULL
        ,
        type = "message"
        )
      cos_sim_lib2_data <- readRDS(sample_data_cos_sim_dia_path)
   }
 }
  else if(input$cos_sim_lib2_data_source == "cos_sim_lib2_upload") {
         req(input$cos_sim_lib2)
         global$cos_sim_lib2_upload_notify <- showNotification(
           HTML("<div style='font-size:16px;'>
           <i class='fas fa-cogs'></i> Loading and processing 2nd uploaded cosine similarity score file...
           </div>"),
            duration = NULL
            ,
            type = "message"
         )
         cos_sim_lib2_path <- input$cos_sim_lib2$datapath
         is_cos_sim_lib2_valid <- validate_cos_sim_data_lib(cos_sim_lib2_path)
            if (is.character(is_cos_sim_lib2_valid)) {
                add_log_message(is_cos_sim_lib2_valid)
                output$msg_cos2 <- renderText(is_cos_sim_lib2_valid)  # Show error message in UI
                on.exit(removeNotification(global$cos_sim_lib2_upload_notify))
                return(NULL)  # Stop further processing
            }
         cos_sim_lib2_data <- read_file_based_on_extension(cos_sim_lib2_path)
         on.exit(removeNotification(global$cos_sim_lib2_upload_notify))
     }
     else if (input$cos_sim_lib2_data_source == "cos_sim_lib2_gdrive") {
        req(input$fileID_cos_sim_lib2)
        global$cos_sim_lib2_gdrive_notify <- showNotification(
        HTML("<div style='font-size:16px;'><i class='fas fa-cogs'></i> Loading and processing 2nd cosine similarity score file from Google Drive...</div>"),
              duration = NULL, type = "message"
            )
            drive_deauth()  # No login needed
            drive_download(as_id(input$fileID_cos_sim_lib2), path = "user_uploaded.tsv", overwrite = TRUE)
            is_cos_sim_lib2_valid <- validate_cos_sim_data_lib("user_uploaded.tsv")
            if (is.character(is_cos_sim_lib2_valid)) {
                add_log_message(is_cos_sim_lib2_valid)
                output$msg_cos2 <- renderText(is_cos_sim_lib2_valid)  # Show error message in UI
                on.exit(removeNotification(global$cos_sim_lib2_gdrive_notify))
                return(NULL)  # Stop further processing
            }
            cos_sim_lib2_data <- read_file_based_on_extension("user_uploaded.tsv")
            on.exit(removeNotification(global$cos_sim_lib2_gdrive_notify))
     }
     return(cos_sim_lib2_data)
  })


 ########################### 
  # Display message based on processed library1 data
  observeEvent(lib1_proc(), {
    lib1_unique_pep <- length(unique(lib1_proc()$ModifiedPeptide))
    lib1_unique_pep_chr <- length(unique(paste(lib1_proc()$ModifiedPeptide, lib1_proc()$lib_PrecursorCharge)))
    lib1_unique_prot <- length(unique(lib1_proc()$UniprotID))
    lib1_frg_ions <- length(unique(paste(lib1_proc()$lib_FragmentMz, lib1_proc()$lib_FragmentCharge)))
    output$msg_lib1 <- renderText({
      paste("Your", input$lib1_type, "spectral library has", lib1_unique_pep, "distinct peptides in\n", lib1_unique_pep_chr, "peptidoforms,", lib1_unique_prot, "distinct precursor proteins and\n", lib1_frg_ions, "distinct fragment ions")
    })
  })
  
# Display message based on cosine similarity data1
  observeEvent(cos_sim_lib1_proc(), {
    cos_sim_lib1_unique_pep <- length(unique(cos_sim_lib1_proc()$ModifiedPeptide))
    cos_sim_lib1_unique_pep_chr <- length(unique(paste(cos_sim_lib1_proc()$ModifiedPeptide, cos_sim_lib1_proc()$PrecursorCharge)))
    cos_sim_lib1_unique_prot <- length(unique(cos_sim_lib1_proc()$UniprotID))
    output$msg_cos1 <- renderText({
      paste("'cos_sim_lib1.tsv' has", cos_sim_lib1_unique_pep, "distinct peptides in\n", cos_sim_lib1_unique_pep_chr, "peptidoforms,", cos_sim_lib1_unique_prot, "distinct precursor proteins and\n")
    })
  })
####################################################################################################################################################################################
  # Display message based on processed library2 data
  observeEvent(lib2_proc(), {
    lib2_unique_pep <- length(unique(lib2_proc()$ModifiedPeptide))
    lib2_unique_pep_chr <- length(unique(paste(lib2_proc()$ModifiedPeptide, lib2_proc()$lib_PrecursorCharge)))
    lib2_unique_prot <- length(unique(lib2_proc()$UniprotID))
    lib2_frg_ions <- length(unique(paste(lib2_proc()$lib_FragmentMz, lib2_proc()$lib_FragmentCharge)))
    output$msg_lib2 <- renderText({
      paste("Your", input$lib2_type, "spectral library has", lib2_unique_pep, "distinct peptides in\n", lib2_unique_pep_chr, "peptidoforms,", lib2_unique_prot, "distinct precursor proteins and\n", lib2_frg_ions, "distinct fragment ions")
    })
  })
###########################################################
  
  # Display message based on cosine similarity data1
  observeEvent(cos_sim_lib2_proc(), {
    cos_sim_lib2_unique_pep <- length(unique(cos_sim_lib2_proc()$ModifiedPeptide))
    cos_sim_lib2_unique_pep_chr <- length(unique(paste(cos_sim_lib2_proc()$ModifiedPeptide, cos_sim_lib2_proc()$PrecursorCharge)))
    cos_sim_lib2_unique_prot <- length(unique(cos_sim_lib2_proc()$UniprotID))
    output$msg_cos2 <- renderText({
      paste("'cos_sim_lib2.tsv' has", cos_sim_lib2_unique_pep, "distinct peptides in\n", 
            cos_sim_lib2_unique_pep_chr, "peptidoforms,", cos_sim_lib2_unique_prot, "distinct precursor proteins and\n")
    })
  })
#####################################################################
cos_sim_compare <- eventReactive(input$compare,{
             req(cos_sim_lib1_proc())
             req(cos_sim_lib2_proc())
             global$cos_sim_compare_notify <- showNotification(
                 HTML("<div style='font-size:16px;'>
                 <i class='fas fa-cogs'></i> Starting the comparision of the uploaded 2 cosine similarity files generated from 2 differnt libraries...
                 </div>"),
                 duration = NULL
                 ,
                 type = "message"
            )
             cos_sim_compare <- summarize_cos_sim(cos_sim_lib1_proc(), cos_sim_lib2_proc())
            return(cos_sim_compare)
    })

observeEvent(cos_sim_compare(),{
               output$fig6 <- renderCachedPlot({
                      ggplot(cos_sim_compare(), aes(as.factor(pep_cou), fill = cat)) + geom_bar(size = 1, position = "dodge") + theme_bw() + theme(text = element_text(size = 16)) + labs() + labs(x = "Number of Samples", y = "Peptide count")
               },cacheKeyExpr = { cos_sim_compare() })  
               output$fig7 <- renderCachedPlot({
                      ggplot(cos_sim_compare(), aes(cosine_similarity_mad, fill = cat)) + geom_histogram(alpha = 0.5, position = 'identity') + theme_bw() + theme(text = element_text(size = 16)) + facet_wrap(pep_cou ~ ., scales = "free")

               },cacheKeyExpr = { cos_sim_compare() })
              on.exit(removeNotification(global$notify_cos_sim_compare))
      })
#####################################################################
best_in_lib1_lib2 <- eventReactive(input$compare,{
             req(cos_sim_lib1_proc())
             req(cos_sim_lib2_proc())
             best_in_lib1_lib2 <- select_the_best_in_lib1_lib2(cos_sim_lib1_proc(), cos_sim_lib2_proc())
            return(best_in_lib1_lib2)
    })
  
observeEvent(best_in_lib1_lib2(),{
           output$fig8 <- renderCachedPlot({
               if("better_in_lib1_by" %in% names(best_in_lib1_lib2())){
                     ggplot(best_in_lib1_lib2(), aes(better_in_lib1_by)) + geom_histogram() + theme_bw() + theme(text = element_text(size = 16))
                }
                 },cacheKeyExpr = {best_in_lib1_lib2() })  
                     
         on.exit(removeNotification(global$notify_cos_sim_compare))          
  })
#######################
retrieved_from_lib <- eventReactive(input$retrieve,{
             req(best_in_lib1_lib2())
             req(lib1_proc())
             req(lib2_proc())
             retrieved_from_lib <- retrieve_from_lib(cos_sim_lib1_proc(), cos_sim_lib2_proc(), best_in_lib1_lib2(), lib1_proc(), lib2_proc())
            return(retrieved_from_lib)
    })
#################
observeEvent(retrieved_from_lib(),{
          showNotification(HTML("################################<br>The library version 2 was created, please download using download button<br>################################"), duration = 5, type = "message")
          #output$retrieve_data <- DT::renderDataTable({
          #retrieved_from_lib()
        #}, server = FALSE)               
  })
###########
output$download2 <- downloadHandler(
          filename = function() {
              paste0("lib_v2", ".tsv")
           },
          content = function(file) {
          vroom::vroom_write(retrieved_from_lib(), file)
         }
  )
#####################################################################
observeEvent(input$file_type,{ 
           req(input$file_type)  
            
              if(input$file_type == "DDA Library"){
                    output$msg7 <- renderText({
                      paste0("for proper data processing;\nDDA library file should contain the following columns;\nModifiedPeptide\nRetentionTime\nUniprotID\nPrecursorMz\nPrecursorCharge\nFragmentMz\nFragmentCharge\nRelativeIntensity\nFragmentType\nFragmentSeriesNumber\nFragmentLossType")
                   })
               }
              else if(input$file_type == "DIA Library"){
                   output$msg7 <- renderText({
                      paste0("for proper data processing;\nDIA library file should contain the following.please note the DIA library created by DIAnn contain all of the follwoing columns by default. columns;\nPeptideSequence\nTr_recalibrated\nUniprotID\nPrecursorMz\nPrecursorCharge\nProductMz\nFragmentCharge\nLibraryIntensity\nFragmentType\nFragmentSeriesNumber\nFragmentLossType")
                  })
               }
              else if(input$file_type == "DIAnn Report"){
                  output$msg7 <- renderText({
                     paste0("for proper data processing;\nDIAnn report file should contain the following columns;\nFile.Name\nStripped.Sequence\nProtein.Ids\nPrecursor.Mz\nPrecursor.Charge\nRT\niRT\nIM\niIM\nQ.Value\nPEP\nMs1.Area\nPrecursor.Quantity\nFragment.Info\nFragment.Quant.Raw\nFragment.Quant.Corrected")
                 })
              }
              else if(input$file_type == "cosine similarity from library1"){
                  output$msg7 <- renderText({
                     paste0("for proper data processing;\ncosine similarity from library1 file should contain the following columns;\nFile.Name\nUniprotID\nModifiedPeptide\nPrecursorCharge\ncosine_similarity")
                 })
              }
              else if(input$file_type == "cosine similarity from library2"){
                  output$msg7 <- renderText({
                     paste0("for proper data processing;\ncosine similarity from library2 file should contain the following columns;\nFile.Name\nUniprotID\nModifiedPeptide\nPrecursorCharge\ncosine_similarity")
                 })
              }

   })
}