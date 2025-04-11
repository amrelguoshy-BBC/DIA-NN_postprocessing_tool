server <- function(input, output, session){
     options(shiny.maxRequestSize = 300 * 1024^2)
  
  # Reactive values to store the data
  data_dia <- reactiveVal()
  data_dda <- reactiveVal()
  data_cos_sim_lib1 <- reactiveVal()
  data_cos_sim_lib2 <- reactiveVal()
  data_diann_report <- reactiveVal()
  global <- reactiveValues(diann_report = NULL, cosine_values_flt_no_rdn = NULL, diann_report_SM = NULL, diann_report_prt = NULL, diann_report_pep = NULL, proc_lib_notify = NULL, proc_diann_report_notify = NULL)
  
  # Paths to the sample data RDS files
  sample_data_dda_path <- "C:\\Users\\User\\Desktop\\diann_postprocess\\sample_data_dda_lib.rds"
  sample_data_dia_path <- "C:\\Users\\User\\Desktop\\diann_postprocess\\sample_data_dia_lib.rds"
  sample_data_diann_report_path <- "C:\\Users\\User\\Desktop\\diann_postprocess\\sample_data_diann_report.rds"
  sample_cos_sim_data_lib1_path <- "C:\\Users\\User\\Desktop\\diann_postprocess\\cos_sim_data_lib1.RDS"
  sample_cos_sim_data_lib2_path <- "C:\\Users\\User\\Desktop\\diann_postprocess\\cos_sim_data_lib2.rds"
  
  
  # Reactive values to track whether sample data is used
  use_sample_dda <- reactiveVal(FALSE)
  use_sample_lib <- reactiveVal(FALSE)
  use_sample_diann_report <- reactiveVal(FALSE)
  use_sample_cos_sim_data_lib1 <- reactiveVal(FALSE)
  use_sample_cos_sim_data_lib2 <- reactiveVal(FALSE)

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
    
    # Reset cosine similarity filter slider
    updateSliderInput(session, "cos", value = c(0, 1))
  }
  
  # Observe all data loading buttons and reset outputs
  observeEvent(input$use_sample_lib, {
    reset_spectrum_similarity_outputs()
    # Rest of your existing use_sample_lib code...
  })
  
  observeEvent(input$use_sample_diann_report, {
    reset_spectrum_similarity_outputs()
    # Rest of your existing use_sample_diann_report code...
  })
  
  observeEvent(input$upload_own_lib, {
    reset_spectrum_similarity_outputs()
    # Rest of your existing upload_own_lib code...
  })
  
  observeEvent(input$upload_own_diann_report, {
    reset_spectrum_similarity_outputs()
    # Rest of your existing upload_own_diann_report code...
  })

  # Load sample data when the action button is clicked
  observeEvent(input$use_sample_lib, {
    if (input$lib_type == "DDA") {
        sample_data_dda_lib <- readRDS(sample_data_dda_path)
        data_dda(sample_data_dda_lib)
        use_sample_lib(TRUE)
     }
    else{
        sample_data_dia_lib <- readRDS(sample_data_dia_path)
        data_dia(sample_data_dia_lib)
        use_sample_lib(TRUE)
       }
  })
  
  # Load sample DIA-NN report when the action button is clicked
  observeEvent(input$use_sample_diann_report, {
    sample_data_diann_report <- readRDS(sample_data_diann_report_path)
    data_diann_report(sample_data_diann_report)
    use_sample_diann_report(TRUE)
  })
  
  # Allow user to switch back to uploading their own data
  observeEvent(input$upload_own_lib, {
    use_sample_lib(FALSE)
    output$msg1 <- renderText(NULL)
  })
  
  observeEvent(input$upload_own_diann_report, {
    use_sample_diann_report(FALSE)
    output$msg2 <- renderText(NULL)
  })
  
  # Dynamically render the file input or a message based on the sample data usage for library
  output$file_ui_lib <- renderUI({
    if (use_sample_lib()) {
      tags$div(class = "alert alert-info", "Sample Data Loaded. Upload complete.")
    } else {
      fileInput("lib", "Upload your library file", accept = c(".csv", ".tsv"))
    }
  })
  
  # Dynamically render the file input or a message based on the sample data usage for DIA-NN report
  output$file_ui_diann_report <- renderUI({
    if (use_sample_diann_report()) {
      tags$div(class = "alert alert-info", "Sample Data Loaded. Upload complete.")
    } else {
      fileInput("diann_report", "Upload your DIA-NN report file")
    }
  })
  
  
# Ensure `add_log_message()` is defined (or replace it with a simple logging function)
add_log_message <- function(msg) {
    cat(msg, "\n")  # Log to console (or replace with actual logging method)
}

# Reactive value to store processed library data
lib_proc <- reactive({
    if (use_sample_lib()) {
        
        if (input$lib_type == "DDA") {
            #output$pep_info1 <- DT::renderDataTable({NULL})
            req(data_dda())
            processed_lib <- process_lib(data_dda(), "DDA", input$base)
        } else {
            #output$pep_info1 <- DT::renderDataTable({NULL})
            req(data_dia())
            processed_lib <- process_lib(data_dia(), "DIA", input$base)
        }
    } else {
        #output$pep_info1 <- DT::renderDataTable({NULL})
        req(input$lib)
        output$msg1 <- renderText(NULL)
        # Validate library file based on type
        lib_path <- input$lib$datapath
        if (input$lib_type == "DDA") {
            is_DDA_lib_valid <- validate_DDA_lib(lib_path)
            if (is.character(is_DDA_lib_valid)) {
                add_log_message(is_DDA_lib_valid)
                output$msg1 <- renderText(is_DDA_lib_valid)  # Show error message in UI
                return(NULL)  # Stop further processing
            }
            lib <- read_csv(lib_path)
        } else {
            is_DIA_lib_valid <- validate_DIA_lib(lib_path)
            if (is.character(is_DIA_lib_valid)) {
                add_log_message(is_DIA_lib_valid)
                output$msg1 <- renderText(is_DIA_lib_valid)  # Show error message in UI
                return(NULL)
            }
            lib <- read_delim(lib_path)
        }
        global$proc_lib_notify <- showNotification(HTML("################################<br>The processing of library file is IN PROCESS<br>################################"), duration = NULL, type = "message")
        processed_lib <- process_lib(lib, input$lib_type, input$base)
    }
    
    return(processed_lib)
})

# Observe validation messages separately to prevent errors inside reactive expressions
observeEvent(lib_proc(), {
    if (is.null(lib_proc())) {
        output$msg1 <- renderText("Library processing failed due to validation errors.")
    } else {
        output$msg1 <- renderText("Library successfully processed.")
    }
})

 ########################### 
  # Display message based on processed library data
  observeEvent(lib_proc(), {
    removeNotification(global$proc_lib_notify
)
    lib_unique_pep <- length(unique(lib_proc()$ModifiedPeptide))
    lib_unique_pep_chr <- length(unique(paste(lib_proc()$ModifiedPeptide, lib_proc()$lib_PrecursorCharge)))
    lib_unique_prot <- length(unique(lib_proc()$UniprotID))
    lib_frg_ions <- length(unique(paste(lib_proc()$lib_FragmentMz, lib_proc()$lib_FragmentCharge)))
    output$msg1 <- renderText({
      paste("Your", input$lib_type, "spectral library has", lib_unique_pep, "distinct peptides in\n", lib_unique_pep_chr, "peptidoforms,", lib_unique_prot, "distinct precursor proteins and\n", lib_frg_ions, "distinct fragment ions")
    })
  })
  
  # Reactive value to store processed DIA-NN report data
  diann_report_proc <- reactive({
    if (use_sample_diann_report()) { 
      #output$pep_info1 <- DT::renderDataTable({NULL})
      req(data_diann_report())
      diann_report_proc <- process_diann_report(data_diann_report(), input$base)
    } else {

      #output$pep_info1 <- DT::renderDataTable({NULL})
      req(input$diann_report)
      output$msg2 <- renderText(NULL)
      report_path <- input$diann_report$datapath
      is_DIAnn_report_valid <- validate_DIAnn_report(report_path)
            if (is.character(is_DIAnn_report_valid)) {
                add_log_message(is_DIAnn_report_valid)
                output$msg2 <- renderText(is_DIAnn_report_valid)  # Show error message in UI
                return(NULL)  # Stop further processing
            }
      global$proc_diann_report_notify <- showNotification(HTML("################################<br>The processing of DIA-nn report file is IN PROCESS<br>################################"), duration = NULL, type = "message")
      global$diann_report <- read_file_based_on_extension(input$diann_report$datapath)
      diann_report_proc <- process_diann_report(global$diann_report, input$base)
    }
    return(diann_report_proc)
  })
  
  # Display message based on processed DIA-NN report data
  observeEvent(diann_report_proc(), {
    removeNotification(global$proc_diann_report_notify)
    diann_report_unique_pep <- length(unique(diann_report_proc()$ModifiedPeptide))
    diann_report_unique_pep_chr <- length(unique(paste(diann_report_proc()$ModifiedPeptide, diann_report_proc()$diann_report_PrecursorCharge)))
    diann_report_unique_prot <- length(unique(diann_report_proc()$UniprotID))
    diann_report_frg_ions <- length(unique(paste(diann_report_proc()$diann_report_FragmentMz, diann_report_proc()$diann_report_FragmentCharge)))
    output$msg2 <- renderText({
      paste("'report.tsv' file created by DIA-NN has", diann_report_unique_pep, "distinct peptides in\n", diann_report_unique_pep_chr, "peptidoforms,", diann_report_unique_prot, "distinct precursor proteins and\n", diann_report_frg_ions, "distinct fragment ions")
    })
  })
################################################################################################################################################################################################################################################
      ###########fun3,4
        calculated_cos_sim <- eventReactive(input$spec_sim,{
            #req(input$tol)
            #req(input$base)
             req(diann_report_proc())
             lib_proc()
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
               #updateTabsetPanel(session, "tabs", selected = "tab2")
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
       observeEvent(input$SM, {
           updateSelectizeInput(inputId = "pep_seq", choices = unique(cosine_values_SM_flt()$ModifiedPeptide), selected = "")
           updateSelectizeInput(inputId = "prt_acc", choices = c("----------", unique(cosine_values_SM_flt()$UniprotID)), selected = "----------")
           ########
          #####################################################################################################################################################
           if (use_sample_diann_report()) {
                req(data_diann_report())
                global$diann_report_SM <- data_diann_report()[data_diann_report()$File.Name == input$SM, ]
                output$fig1 <- renderCachedPlot({
                      ggplot(global$diann_report_SM, aes(RT, iRT)) + geom_point(color = "grey", shape = 21) + theme_bw() + theme(text = element_text(size = 18, face = "bold"))
               },cacheKeyExpr = { input$SM })
               output$fig2 <- renderCachedPlot({
                     ggplot(global$diann_report_SM, aes(IM, iIM)) + geom_point(color = "grey", shape = 21) + theme_bw() + theme(text = element_text(size = 18, face = "bold"))
      },cacheKeyExpr = { input$SM })
               output$fig3 <- renderCachedPlot({
                     ggplot(global$diann_report_SM, aes(Q.Value, PEP)) + geom_point(color = "grey", shape = 21) + theme_bw() + theme(text = element_text(size = 18, face = "bold"))
               },cacheKeyExpr = { input$SM })
               output$fig4 <- renderCachedPlot({
                    ggplot(global$diann_report_SM, aes(Ms1.Area, Precursor.Quantity)) + geom_point(color = "grey", shape = 21) + theme_bw() + theme(text = element_text(size = 18, face = "bold"))
               },cacheKeyExpr = { input$SM })
     }
     else{
           global$diann_report_SM <- global$diann_report[global$diann_report$File.Name == input$SM, ]
           output$fig1 <- renderCachedPlot({
                ggplot(global$diann_report_SM, aes(RT, iRT)) + geom_point(color = "grey", shape = 21) + theme_bw() + theme(text = element_text(size = 18, face = "bold"))
           },cacheKeyExpr = { input$SM })
           output$fig2 <- renderCachedPlot({
                ggplot(global$diann_report_SM, aes(IM, iIM)) + geom_point(color = "grey", shape = 21) + theme_bw() + theme(text = element_text(size = 18, face = "bold"))
           },cacheKeyExpr = { input$SM })
           output$fig3 <- renderCachedPlot({
                ggplot(global$diann_report_SM, aes(Q.Value, PEP)) + geom_point(color = "grey", shape = 21) + theme_bw() + theme(text = element_text(size = 18, face = "bold"))
           },cacheKeyExpr = { input$SM })
           output$fig4 <- renderCachedPlot({
               ggplot(global$diann_report_SM, aes(Ms1.Area, Precursor.Quantity)) + geom_point(color = "grey", shape = 21) + theme_bw() + theme(text = element_text(size = 18, face = "bold"))
           },cacheKeyExpr = { input$SM })
   }
 })
       ##
       observeEvent(input$prt_acc, {
       #####################################################################################################################################################
          if(input$prt_acc == "----------"){
               updateSelectizeInput(inputId = "pep_seq", choices = unique(cosine_values_SM_flt()$ModifiedPeptide), selected = "")
          }else if(input$prt_acc != "----------"){
               updateSelectizeInput(inputId = "pep_seq", choices = unique(cosine_values_prt_flt()$ModifiedPeptide), selected = "")
              #output$msg6 <- renderText({
                #  paste0("peptide sequence list was updated for selected protein; please select one peptide to visualize")
               ######
            if (use_sample_diann_report()) {
               req(data_diann_report())
               global$diann_report_SM <- data_diann_report()[data_diann_report()$File.Name == input$SM, ]
               global$diann_report_prt <- global$diann_report_SM[global$diann_report_SM$Protein.Ids == input$prt_acc, ]
               output$fig1 <- renderCachedPlot({
                     ggplot(global$diann_report_SM, aes(RT, iRT)) + geom_point(color = "grey", shape = 21)+ geom_point(data = global$diann_report_prt, aes(RT, iRT), color = "red", size = 1.5) + theme_bw() + theme(text = element_text(size = 18, face = "bold"))
               },cacheKeyExpr = { paste(input$SM, input$prt_acc, Sys.time()) })
               output$fig2 <- renderCachedPlot({
                     ggplot(global$diann_report_SM, aes(IM, iIM)) + geom_point(color = "grey", shape = 21) + geom_point(data = global$diann_report_prt, aes(IM, iIM), color = "red", size = 1.5)+ theme_bw() + theme(text = element_text(size = 18, face = "bold"))
               },cacheKeyExpr = { paste(input$SM, input$prt_acc, Sys.time())  })
               output$fig3 <- renderCachedPlot({
                     ggplot(global$diann_report_SM, aes(Q.Value, PEP)) + geom_point(color = "grey", shape = 21) + geom_point(data = global$diann_report_prt, aes(Q.Value, PEP), color = "red", size = 1.5)+ theme_bw() + theme(text = element_text(size = 18, face = "bold"))
               },cacheKeyExpr = { paste(input$SM, input$prt_acc, Sys.time())  })
               output$fig4 <- renderCachedPlot({
                     ggplot(global$diann_report_SM, aes(Ms1.Area, Precursor.Quantity)) + geom_point(color = "grey", shape = 21) + geom_point(data = global$diann_report_prt, aes(Ms1.Area, Precursor.Quantity), color = "red", size = 1.5)+ theme_bw() + theme(text = element_text(size = 18, face = "bold"))
               },cacheKeyExpr = { paste(input$SM, input$prt_acc, Sys.time())  })
           }
           else{
               global$diann_report_SM <- global$diann_report[global$diann_report$File.Name == input$SM, ]
               global$diann_report_prt <- global$diann_report_SM[global$diann_report_SM$Protein.Ids == input$prt_acc, ]
               output$fig1 <- renderCachedPlot({
                     ggplot(global$diann_report_SM, aes(RT, iRT)) + geom_point(color = "grey", shape = 21)+ geom_point(data = global$diann_report_prt, aes(RT, iRT), color = "red", size = 1.5) + theme_bw() + theme(text = element_text(size = 18, face = "bold"))
               },cacheKeyExpr = { paste(input$SM, input$prt_acc, Sys.time())  })
               output$fig2 <- renderCachedPlot({
                     ggplot(global$diann_report_SM, aes(IM, iIM)) + geom_point(color = "grey", shape = 21) + geom_point(data = global$diann_report_prt, aes(IM, iIM), color = "red", size = 1.5)+ theme_bw() + theme(text = element_text(size = 18, face = "bold"))
               },cacheKeyExpr = { paste(input$SM, input$prt_acc, Sys.time())  })
               output$fig3 <- renderCachedPlot({
                     ggplot(global$diann_report_SM, aes(Q.Value, PEP)) + geom_point(color = "grey", shape = 21) + geom_point(data = global$diann_report_prt, aes(Q.Value, PEP), color = "red", size = 1.5)+ theme_bw() + theme(text = element_text(size = 18, face = "bold"))
               },cacheKeyExpr = { paste(input$SM, input$prt_acc, Sys.time())  })
               output$fig4 <- renderCachedPlot({
                     ggplot(global$diann_report_SM, aes(Ms1.Area, Precursor.Quantity)) + geom_point(color = "grey", shape = 21) + geom_point(data = global$diann_report_prt, aes(Ms1.Area, Precursor.Quantity), color = "red", size = 1.5)+ theme_bw() + theme(text = element_text(size = 18, face = "bold"))
              },cacheKeyExpr = {paste(input$SM, input$prt_acc, Sys.time()) })
         }
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
      ########
      
      #####################################################################################################################################################
      output$fig5 <- renderCachedPlot({ 
               ggplot(selected_pep_data()[!is.na(selected_pep_data()$PrecursorCharge),], aes(FragmentMz, RelativeIntensity, col = as.factor(cat))) + geom_col(width = 1.5, position = "dodge") + geom_hline(yintercept = 0) + scale_color_manual(values = c("red", "darkblue")) + theme_bw() + theme(text = element_text(size = 18, face = "bold"))  + ggtitle(paste(selected_pep_data()[!is.na(selected_pep_data()$PrecursorCharge),]$UniprotID[1], "\n", selected_pep_data()[!is.na(selected_pep_data()$PrecursorCharge),]$ModifiedPeptide[1])) + theme(plot.title = element_text(size = 15, hjust = 0.5, face = "bold")) + theme(legend.title = element_blank()) + geom_label_repel(aes(label = paste(FragmentType, FragmentSeriesNumber, sep = "")), size = 4) + facet_wrap(paste("Cosine similarity score = ",cosine_similarity) ~ paste("Peptide Charge = ", PrecursorCharge), labeller = label_wrap_gen(width = 35))
      },cacheKeyExpr = { paste(input$SM, input$pep_seq, Sys.time()) })
     if (use_sample_diann_report()) {
             req(data_diann_report())
             global$diann_report_SM <- data_diann_report()[data_diann_report()$File.Name == input$SM, ]
             global$diann_report_pep <- global$diann_report_SM[global$diann_report_SM$Stripped.Sequence == input$pep_seq, ]
             output$fig1 <- renderCachedPlot({
                ggplot(global$diann_report_SM, aes(RT, iRT)) + geom_point(color = "grey", shape = 21)+ geom_point(data = global$diann_report_pep, aes(RT, iRT), color = "red", size = 1.5) + theme_bw() + theme(text = element_text(size = 18, face = "bold"))
             },cacheKeyExpr = { paste(input$SM, input$pep_seq, Sys.time())  })
             output$fig2 <- renderCachedPlot({
                ggplot(global$diann_report_SM, aes(IM, iIM)) + geom_point(color = "grey", shape = 21) + geom_point(data = global$diann_report_pep, aes(IM, iIM), color = "red", size = 1.5)+ theme_bw() + theme(text = element_text(size = 18, face = "bold"))
             },cacheKeyExpr = {paste(input$SM, input$pep_seq, Sys.time())  })
             output$fig3 <- renderCachedPlot({
                ggplot(global$diann_report_SM, aes(Q.Value, PEP)) + geom_point(color = "grey", shape = 21) + geom_point(data = global$diann_report_pep, aes(Q.Value, PEP), color = "red", size = 1.5)+ theme_bw() + theme(text = element_text(size = 18, face = "bold"))
             },cacheKeyExpr = { paste(input$SM, input$pep_seq, Sys.time())  })
             output$fig4 <- renderCachedPlot({
               ggplot(global$diann_report_SM, aes(Ms1.Area, Precursor.Quantity)) + geom_point(color = "grey", shape = 21) + geom_point(data = global$diann_report_pep, aes(Ms1.Area, Precursor.Quantity), color = "red", size = 1.5)+ theme_bw() + theme(text = element_text(size = 18, face = "bold"))
            },cacheKeyExpr = { paste(input$SM, input$pep_seq, Sys.time()) })
    }
    else{
        global$diann_report_SM <- global$diann_report[global$diann_report$File.Name == input$SM, ]
        global$diann_report_pep <-global$diann_report_SM[global$diann_report_SM$Stripped.Sequence == input$pep_seq, ]
        
        output$fig1 <- renderCachedPlot({
                ggplot(global$diann_report_SM, aes(RT, iRT)) + geom_point(color = "grey", shape = 21)+ geom_point(data = global$diann_report_pep, aes(RT, iRT), color = "red", size = 1.5) + theme_bw() + theme(text = element_text(size = 18, face = "bold"))
       },cacheKeyExpr = { paste(input$SM, input$pep_seq, Sys.time())  })
       output$fig2 <- renderCachedPlot({
                ggplot(global$diann_report_SM, aes(IM, iIM)) + geom_point(color = "grey", shape = 21) + geom_point(data = global$diann_report_pep, aes(IM, iIM), color = "red", size = 1.5)+ theme_bw() + theme(text = element_text(size = 18, face = "bold"))
      },cacheKeyExpr = { paste(input$SM, input$pep_seq, Sys.time())  })
       output$fig3 <- renderCachedPlot({
                ggplot(global$diann_report_SM, aes(Q.Value, PEP)) + geom_point(color = "grey", shape = 21) + geom_point(data = global$diann_report_pep, aes(Q.Value, PEP), color = "red", size = 1.5)+ theme_bw() + theme(text = element_text(size = 18, face = "bold"))
      },cacheKeyExpr = { paste(input$SM, input$pep_seq, Sys.time()) })
      output$fig4 <- renderCachedPlot({
               ggplot(global$diann_report_SM, aes(Ms1.Area, Precursor.Quantity)) + geom_point(color = "grey", shape = 21) + geom_point(data = global$diann_report_pep, aes(Ms1.Area, Precursor.Quantity), color = "red", size = 1.5)+ theme_bw() + theme(text = element_text(size = 18, face = "bold"))
       },cacheKeyExpr = { paste(input$SM, input$pep_seq, Sys.time())  })
    }
############################
###########################
#########################
}) 
############################################################################################################################
####################################################################################################################################################################################
####################################################################################################################################################################################

#############  
  # Reactive values to track whether sample data is used
  use_sample_dda <- reactiveVal(FALSE)
  use_sample_lib1 <- reactiveVal(FALSE)
  use_sample_cos_sim_data_lib1 <- reactiveVal(FALSE)
  

  # Load sample data when the action button is clicked
  observeEvent(input$use_sample_lib1, {
    if (input$lib1_type == "DDA") {
        sample_data_dda_lib1 <- readRDS(sample_data_dda_path)
        data_dda(sample_data_dda_lib1)
        use_sample_lib1(TRUE)
     }
    else{
        sample_data_dia_lib1 <- readRDS(sample_data_dia_path)
        data_dia(sample_data_dia_lib1)
        use_sample_lib1(TRUE)
       }
  })
  
  # Load sample cosine similarity for library1 when the action button is clicked
  observeEvent(input$use_sample_cos_sim_data_lib1, {
    sample_cos_sim_data_lib1 <- readRDS(sample_cos_sim_data_lib1_path)
    data_cos_sim_lib1(sample_cos_sim_data_lib1)
    use_sample_cos_sim_data_lib1(TRUE)
  })
  
  # Allow user to switch back to uploading their own data
  observeEvent(input$upload_own_lib1, {
    use_sample_lib1(FALSE)
    output$msg_lib1 <- renderText(NULL)
  })
  
  observeEvent(input$upload_own_cos_sim_data_lib1, {
    use_sample_cos_sim_data_lib1(FALSE)
    output$msg_cos1 <- renderText(NULL)
  })
  
  # Dynamically render the file input or a message based on the sample data usage for library
  output$file_ui_lib1 <- renderUI({
    if (use_sample_lib1()) {
      tags$div(class = "alert alert-info", "Sample Data Loaded. Upload complete.")
    } else {
      fileInput("lib1", "Upload your library file", accept = c(".csv", ".tsv"))
    }
  })
  
  # Dynamically render the file input or a message based on the sample data usage for DIA-NN report
  output$file_ui_cos_sim_data_lib1 <- renderUI({
    if (use_sample_cos_sim_data_lib1()) {
      tags$div(class = "alert alert-info", "Sample Data Loaded. Upload complete.")
    } else {
      fileInput("cos_sim_data_lib1", "Upload your cosine similarity from library1")
    }
  })
  
  
# Ensure `add_log_message()` is defined (or replace it with a simple logging function)
add_log_message <- function(msg) {
    cat(msg, "\n")  # Log to console (or replace with actual logging method)
}

# Reactive value to store processed library data
lib1_proc <- reactive({
    if (use_sample_lib1()) {
        if (input$lib1_type == "DDA") {
            req(data_dda())
            output$msg_lib1 <- renderText(NULL)
            processed_lib1 <- process_lib(data_dda(), "DDA", input$base)
        } else {
            req(data_dia())
            output$msg_lib1 <- renderText(NULL)
            processed_lib1 <- process_lib(data_dia(), "DIA", input$base)
        }
    } else {
        req(input$lib1)
        output$msg_lib1 <- renderText(NULL)
        # Validate library file based on type
        lib1_path <- input$lib1$datapath
        if (input$lib1_type == "DDA") {
            is_DDA_lib1_valid <- validate_DDA_lib(lib1_path)
            if (is.character(is_DDA_lib1_valid)) {
                add_log_message(is_DDA_lib1_valid)
                output$msg_lib1 <- renderText(is_DDA_lib1_valid)  # Show error message in UI
                return(NULL)  # Stop further processing
            }
            lib1 <- read_file_based_on_extension(lib1_path)
        } else {
            is_DIA_lib1_valid <- validate_DIA_lib(lib1_path)
            if (is.character(is_DIA_lib1_valid)) {
                add_log_message(is_DIA_lib1_valid)
                output$msg_lib1 <- renderText(is_DIA_lib1_valid)  # Show error message in UI
                return(NULL)
            }
            lib1 <- read_file_based_on_extension(lib1_path)
        }

        processed_lib1 <- process_lib(lib1, input$lib1_type, input$base)
    }
    
    return(processed_lib1)
})

# Observe validation messages separately to prevent errors inside reactive expressions
observeEvent(lib1_proc(), {
    if (is.null(lib1_proc())) {
        output$msg_lib1 <- renderText("Library processing failed due to validation errors.")
    } else {
        output$msg_lib1 <- renderText("Library successfully processed.")
    }
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
###########################################################
 # Reactive value to store DIA-NN report data without processing
  cos_sim_data_lib1_proc <- reactive({
    if (use_sample_cos_sim_data_lib1()) {
      req(data_cos_sim_lib1())
      cos_sim_data_lib1_proc <- data_cos_sim_lib1()  # Directly use raw report data
    } else {
      req(input$cos_sim_data_lib1)
      output$msg_cos1 <- renderText(NULL)
      cos_sim_data_lib1_path <- input$cos_sim_data_lib1$datapath
      is_cos_sim_data_lib1_valid <- validate_cos_sim_data_lib(cos_sim_data_lib1_path)
            if (is.character(is_cos_sim_data_lib1_valid)) {
                add_log_message(is_cos_sim_data_lib1_valid)
                output$msg_cos1 <- renderText(is_cos_sim_data_lib1_valid)  # Show error message in UI
                return(NULL)  # Stop further processing
            }
      cos_sim_data_lib1 <- read_file_based_on_extension(cos_sim_data_lib1_path)
      cos_sim_data_lib1_proc <- cos_sim_data_lib1  # Directly use raw report data
    }
    return(cos_sim_data_lib1_proc)
  })
  
  # Display message based on DIA-NN report data
  observeEvent(cos_sim_data_lib1_proc(), {
    cos_sim_data_lib1_unique_pep <- length(unique(cos_sim_data_lib1_proc()$ModifiedPeptide))
    cos_sim_data_lib1_unique_pep_chr <- length(unique(paste(cos_sim_data_lib1_proc()$ModifiedPeptide, cos_sim_data_lib1_proc()$PrecursorCharge)))
    cos_sim_data_lib1_unique_prot <- length(unique(cos_sim_data_lib1_proc()$UniprotID))
   # diann_report_frg_ions <- length(unique(paste(diann_report_proc()$diann_report_FragmentMz, diann_report_proc()$diann_report_FragmentCharge)))
    output$msg_cos1 <- renderText({
      paste("'cos_sim_data_lib1.tsv' has", cos_sim_data_lib1_unique_pep, "distinct peptides in\n", 
            cos_sim_data_lib1_unique_pep_chr, "peptidoforms,", cos_sim_data_lib1_unique_prot, "distinct precursor proteins and\n")
    })
  })
####################################################################################################################################################################################

#############  
  # Reactive values to track whether sample data is used
  use_sample_lib2 <- reactiveVal(FALSE)
  use_sample_cos_sim_data_lib2 <- reactiveVal(FALSE)
  

  # Load sample data when the action button is clicked
  observeEvent(input$use_sample_lib2, {
    if (input$lib2_type == "DDA") {
        sample_data_dda_lib2 <- readRDS(sample_data_dda_path)
        data_dda(sample_data_dda_lib2)
        use_sample_lib2(TRUE)
     }
    else{
        sample_data_dia_lib2 <- readRDS(sample_data_dia_path)
        data_dia(sample_data_dia_lib2)
        use_sample_lib2(TRUE)
       }
  })
  
  # Load sample cosine similarity for library2 when the action button is clicked
  observeEvent(input$use_sample_cos_sim_data_lib2, {
    sample_cos_sim_data_lib2 <- readRDS(sample_cos_sim_data_lib2_path)
    data_cos_sim_lib2(sample_cos_sim_data_lib2)
    use_sample_cos_sim_data_lib2(TRUE)
  })
  
  # Allow user to switch back to uploading their own data
  observeEvent(input$upload_own_lib2, {
    use_sample_lib2(FALSE)
    output$msg_lib2 <- renderText(NULL)
  })
  
  observeEvent(input$upload_own_cos_sim_data_lib2, {
    use_sample_cos_sim_data_lib2(FALSE)
    output$msg_cos2 <- renderText(NULL)
  })
  
  # Dynamically render the file input or a message based on the sample data usage for library
  output$file_ui_lib2 <- renderUI({
    if (use_sample_lib2()) {
      tags$div(class = "alert alert-info", "Sample Data Loaded. Upload complete.")
    } else {
      fileInput("lib2", "Upload your library file", accept = c(".csv", ".tsv"))
    }
  })
  
  # Dynamically render the file input or a message based on the sample data usage for DIA-NN report
  output$file_ui_cos_sim_data_lib2 <- renderUI({
    if (use_sample_cos_sim_data_lib2()) {
      tags$div(class = "alert alert-info", "Sample Data Loaded. Upload complete.")
    } else {
      fileInput("cos_sim_data_lib2", "Upload your cosine similarity from library2")
    }
  })
  
  
# Ensure `add_log_message()` is defined (or replace it with a simple logging function)
add_log_message <- function(msg) {
    cat(msg, "\n")  # Log to console (or replace with actual logging method)
}

# Reactive value to store processed library data
lib2_proc <- reactive({
    if (use_sample_lib2()) {
        if (input$lib2_type == "DDA") {
            req(data_dda())
            output$msg_lib2 <- renderText(NULL)
            processed_lib2 <- process_lib(data_dda(), "DDA", input$base)
        } else {
            req(data_dia())
            output$msg_lib2 <- renderText(NULL)
            processed_lib2 <- process_lib(data_dia(), "DIA", input$base)
        }
    } else {
        req(input$lib2)
        output$msg_lib2 <- renderText(NULL)
        # Validate library file based on type
        lib2_path <- input$lib2$datapath
        if (input$lib2_type == "DDA") {
            is_DDA_lib2_valid <- validate_DDA_lib(lib2_path)
            if (is.character(is_DDA_lib2_valid)) {
                add_log_message(is_DDA_lib2_valid)
                output$msg_lib2 <- renderText(is_DDA_lib2_valid)  # Show error message in UI
                return(NULL)  # Stop further processing
            }
            lib2 <- read_file_based_on_extension(lib2_path)
        } else {
            is_DIA_lib2_valid <- validate_DIA_lib(lib2_path)
            if (is.character(is_DIA_lib2_valid)) {
                add_log_message(is_DIA_lib2_valid)
                output$msg_lib2 <- renderText(is_DIA_lib2_valid)  # Show error message in UI
                return(NULL)
            }
            lib2 <- read_file_based_on_extension(lib2_path)
        }

        processed_lib2 <- process_lib(lib2, input$lib2_type, input$base)
    }
    
    return(processed_lib2)
})

# Observe validation messages separately to prevent errors inside reactive expressions
observeEvent(lib2_proc(), {
    if (is.null(lib2_proc())) {
        output$msg_lib2 <- renderText("Library processing failed due to validation errors.")
    } else {
        output$msg_lib2 <- renderText("Library successfully processed.")
    }
})

 ########################### 
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
 # Reactive value to store DIA-NN report data without processing
  cos_sim_data_lib2_proc <- reactive({
    if (use_sample_cos_sim_data_lib2()) {
      req(data_cos_sim_lib2())
      cos_sim_data_lib2_proc <- data_cos_sim_lib2()  # Directly use raw report data
    } else {
      req(input$cos_sim_data_lib2)
      output$msg_cos2 <- renderText(NULL)
      cos_sim_data_lib2_path <- input$cos_sim_data_lib2$datapath
      is_cos_sim_data_lib2_valid <- validate_cos_sim_data_lib(cos_sim_data_lib2_path)
            if (is.character(is_cos_sim_data_lib2_valid)) {
                add_log_message(is_cos_sim_data_lib2_valid)
                output$msg_cos2 <- renderText(is_cos_sim_data_lib2_valid)  # Show error message in UI
                return(NULL)  # Stop further processing
            }
      cos_sim_data_lib2 <- read_file_based_on_extension(cos_sim_data_lib2_path)
      cos_sim_data_lib2_proc <- cos_sim_data_lib2  # Directly use raw report data
    }
    return(cos_sim_data_lib2_proc)
  })
  
  # Display message based on DIA-NN report data
  observeEvent(cos_sim_data_lib2_proc(), {
    cos_sim_data_lib2_unique_pep <- length(unique(cos_sim_data_lib2_proc()$ModifiedPeptide))
    cos_sim_data_lib2_unique_pep_chr <- length(unique(paste(cos_sim_data_lib2_proc()$ModifiedPeptide, cos_sim_data_lib2_proc()$PrecursorCharge)))
    cos_sim_data_lib2_unique_prot <- length(unique(cos_sim_data_lib2_proc()$UniprotID))
   # diann_report_frg_ions <- length(unique(paste(diann_report_proc()$diann_report_FragmentMz, diann_report_proc()$diann_report_FragmentCharge)))
    output$msg_cos2 <- renderText({
      paste("'cos_sim_data_lib2.tsv' has", cos_sim_data_lib2_unique_pep, "distinct peptides in\n", 
            cos_sim_data_lib2_unique_pep_chr, "peptidoforms,", cos_sim_data_lib2_unique_prot, "distinct precursor proteins and\n")
    })
  })
#####################################################################
cos_sim_compare <- eventReactive(input$compare,{
             req(cos_sim_data_lib1_proc())
             req(cos_sim_data_lib2_proc())
             cos_sim_compare <- summarize_cos_sim(cos_sim_data_lib1_proc(), cos_sim_data_lib2_proc())
            return(cos_sim_compare)
    })

observeEvent(cos_sim_compare(),{
               output$fig6 <- renderCachedPlot({
                      ggplot(cos_sim_compare(), aes(as.factor(pep_cou), fill = cat)) + geom_bar(size = 1, position = "dodge") + theme_bw() + theme(text = element_text(size = 16)) + labs() + labs(x = "Number of Samples", y = "Peptide count")
               },cacheKeyExpr = { cos_sim_compare() })  
               output$fig7 <- renderCachedPlot({
                      ggplot(cos_sim_compare(), aes(cosine_similarity_mad, fill = cat)) + geom_histogram(alpha = 0.5, position = 'identity') + theme_bw() + theme(text = element_text(size = 16)) + facet_wrap(pep_cou ~ ., scales = "free")

               },cacheKeyExpr = { cos_sim_compare() })
      })
#####################################################################
best_in_lib1_lib2 <- eventReactive(input$compare,{
             req(cos_sim_data_lib1_proc())
             req(cos_sim_data_lib2_proc())
             best_in_lib1_lib2 <- select_the_best_in_lib1_lib2(cos_sim_data_lib1_proc(), cos_sim_data_lib2_proc())
            return(best_in_lib1_lib2)
    })
  
observeEvent(best_in_lib1_lib2(),{
           output$fig8 <- renderCachedPlot({
               if("better_in_lib1_by" %in% names(best_in_lib1_lib2())){
                     ggplot(best_in_lib1_lib2(), aes(better_in_lib1_by)) + geom_histogram() + theme_bw() + theme(text = element_text(size = 16))
                }
                 },cacheKeyExpr = {best_in_lib1_lib2() })  
                     
                   
  })
#######################
retrieved_from_lib <- eventReactive(input$retrieve,{
             req(best_in_lib1_lib2())
             req(lib1_proc())
             req(lib2_proc())
             retrieved_from_lib <- retrieve_from_lib(cos_sim_data_lib1_proc(), cos_sim_data_lib2_proc(), best_in_lib1_lib2(), lib1_proc(), lib2_proc())
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
                      paste0("for proper data processing;\nDDA library file should contain the following columns;\nModifiedPeptide\nUniprotID\nPrecursorMz\nPrecursorCharge\nFragmentMz\nFragmentCharge\nRelativeIntensity\nFragmentType\nFragmentSeriesNumber\nFragmentLossType")
                   })
               }
              else if(input$file_type == "DIA Library"){
                   output$msg7 <- renderText({
                      paste0("for proper data processing;\nDIA library file should contain the following columns;\nPeptideSequence\nUniprotID\nPrecursorMz\nPrecursorCharge\nProductMz\nFragmentCharge\nLibraryIntensity\nFragmentType\nFragmentSeriesNumber\nFragmentLossType")
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
