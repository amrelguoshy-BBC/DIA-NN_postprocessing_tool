### Load required libraries
library(dplyr)
library(tidyr)
library(readr)
library(stringr)
library(stringi)
library(seqinr)
library(shiny)
library(DT)
library(Rcpp)
library(rlang)
library(scales)
library(withr)
library(ggplot2)
library(ggrepel)
library(shinyBS)
library(shinyjs)
library(shinyWidgets)
library(data.table)
##############################################################
########### to get the cosine similarity between DIA-NN data and the created library.
calculate_cosine_sim_4_DIAnn <- function(diann_report, lib, lib_type = "DDA", tol = 0.05, base = 0){
    diann_report_proc <- process_diann_report(diann_report, base)
    lib_proc <- process_lib(lib, lib_type, base)
    aligned_diann_report_lib_frgs <- align_diann_report_lib_frgs(diann_report_proc, lib_proc, tol)
    calculated_cos_sim <- calculate_cos_sim(aligned_diann_report_lib_frgs)
    return(calculated_cos_sim)
}
#############
##########
plot_certain_peptide <- function(calculated_cos_sim, selected_peptide = ""){
     tst_dda <- calculated_cos_sim[, c(1,2,9:16)]
     tst_dia <- calculated_cos_sim[, c(1:8,15:16)]
     names(tst_dda) <- c("File.Name", "ModifiedPeptide", "FragmentMz", "FragmentCharge", "RelativeIntensity", "FragmentType", "FragmentSeriesNumber", "FragmentLossType", "mz_dif", "cos_sim")
     tst_dda <- tst_dda %>%
         mutate(RelativeIntensity = - RelativeIntensity)
     names(tst_dia) <- c("File.Name", "ModifiedPeptide", "FragmentMz", "FragmentCharge", "RelativeIntensity", "FragmentType", "FragmentSeriesNumber", "FragmentLossType", "mz_dif", "cos_sim")
     tst_dia$FragmentMz <- as.numeric(tst_dia$FragmentMz)
     tst_dia$FragmentCharge <- as.numeric(tst_dia$FragmentCharge)

     merged <- bind_rows("dia" = tst_dia, "dda" = tst_dda, .id = "cat")
     merged_sel <- merged[merged$ModifiedPeptide == selected_peptide, ]
     merged_sel$File.Name <- factor(merged_sel$File.Name)

plot <- ggplot(merged_sel, aes(FragmentMz, RelativeIntensity, col = as.factor(cat))) +
    geom_col(width = 1, position = "dodge") +
    facet_wrap(~File.Name, scales = "free_y") +
    theme_bw() +
    theme(text = element_text(size = 18)) +
    geom_text_repel(aes(label = paste(FragmentType, FragmentSeriesNumber, sep = "\n")), size = 4, nudge_x = -15)
     return(plot)
}

##############
###########
validate_DDA_lib <- function(lib_path) {
  required_cols <- c("ModifiedPeptide", "UniprotID", "RetentionTime", "PrecursorMz", "PrecursorCharge", "FragmentMz", "FragmentCharge", "RelativeIntensity", "FragmentType", "FragmentSeriesNumber",  "FragmentLossType")

  # Try reading the file safely
  DDA_lib <- tryCatch(read_delim(lib_path), error = function(e) return(paste("Error reading file:", e$message)))

  # If read_delim failed, return the error message
  if (is.character(DDA_lib)) return(DDA_lib)

 

  # Check for missing columns
  missing_cols <- setdiff(required_cols, colnames(DDA_lib))
  if (length(missing_cols) > 0) {
    return(paste("Error: Missing columns in DDA library:", paste(missing_cols, collapse = ", ")))
  }

  return(TRUE)  # Validation successful
}

############
validate_DIA_lib <- function(lib_path) {
    required_cols <- c("PeptideSequence", "UniprotID", "Tr_recalibrated", "PrecursorMz", "PrecursorCharge", 
                       "ProductMz", "FragmentCharge", "LibraryIntensity", "FragmentType", 
                       "FragmentSeriesNumber", "FragmentLossType")
    
    # Try reading the file safely
    DIA_lib <- tryCatch(read_delim(lib_path), error = function(e) return(paste("Error reading file:", e$message)))
    
    # If read_delim failed, return the error message
    if (is.character(DIA_lib)) return(DIA_lib)

    # Check for missing columns
    missing_cols <- setdiff(required_cols, colnames(DIA_lib))
    if (length(missing_cols) > 0) {
        return(paste("Error: Missing columns in DIA library:", paste(missing_cols, collapse = ", ")))
    }
    
    return(TRUE)  # Validation successful
}

#############
validate_DIAnn_report <- function(report_path) {
    required_cols <- c("File.Name",  "Stripped.Sequence","Protein.Ids", "Precursor.Mz", "Precursor.Charge", "Fragment.Quant.Raw", "Fragment.Quant.Corrected", "Fragment.Info")
    # Try reading the file safely
    DIAnn_report <- tryCatch(read_delim(report_path), error = function(e) return(paste("Error reading file:", e$message)))
    # If read_delim failed, return the error message
    if (is.character(DIAnn_report)) return(DIAnn_report)

    # Check for missing columns
    missing_cols <- setdiff(required_cols, colnames(DIAnn_report))
    if (length(missing_cols) > 0) {
        return(paste("Error: Missing columns in DIAnn report:", paste(missing_cols, collapse = ", ")))
    }
    
    return(TRUE)  # Validation successful
}
##############
validate_cos_sim_data_lib <- function(cos_sim_data_lib_path) {
    required_cols <- c("File.Name",  "UniprotID","ModifiedPeptide", "PrecursorCharge", "cosine_similarity")
    # Try reading the file safely
    cos_sim_data_lib <- tryCatch(read_delim(cos_sim_data_lib_path), error = function(e) return(paste("Error reading file:", e$message)))
    # If read_delim failed, return the error message
    if (is.character(cos_sim_data_lib)) return(cos_sim_data_lib)

    # Check for missing columns
    missing_cols <- setdiff(required_cols, colnames(cos_sim_data_lib))
    if (length(missing_cols) > 0) {
        return(paste("Error: Missing columns in cos_sim_data_lib:", paste(missing_cols, collapse = ", ")))
    }
    
    return(TRUE)  # Validation successful
}
#########
read_file_based_on_extension <- function(file_path) {
    ext <- tools::file_ext(file_path)  # Get file extension
    
    if (ext == "csv") {
        data <- read_csv(file_path)
    } else if (ext == "tsv") {
        data <- read_delim(file_path, delim = "\t")
    } else {
        stop("Unsupported file format. Please provide a CSV or TSV file.")
    }
    
    return(data)
}
##########
process_lib <- function(lib, lib_type, base) {
    dt <- as.data.table(lib)
     # Check if IonMobility column exists, and add it if missing after RetentionTime
     if (!"IonMobility" %in% colnames(dt)) {
         retention_time_index <- which(colnames(dt) == "RetentionTime")
         dt <- dt %>% mutate(IonMobility = NA) %>% relocate(IonMobility, .after = retention_time_index)
     }
     if (lib_type == "DDA") {
         
        dt <- dt[, .(ModifiedPeptide, UniprotID,  
                     RetentionTime, IonMobility, 
                     lib_PrecursorMz = PrecursorMz,
                     lib_PrecursorCharge = PrecursorCharge,
                     lib_FragmentMz = FragmentMz, 
                     lib_FragmentCharge = FragmentCharge, 
                     lib_RelativeIntensity = RelativeIntensity, 
                     lib_FragmentType = FragmentType, 
                     lib_FragmentSeriesNumber = FragmentSeriesNumber, 
                     lib_FragmentLossType = FragmentLossType)]
        
        dt[, lib_FragmentMass := (lib_FragmentMz * lib_FragmentCharge) - (1.00728 * lib_FragmentCharge)]
        
        dt <- dt[, .(
            lib_FragmentType = paste(lib_FragmentType, collapse = ";"),
            lib_FragmentSeriesNumber = paste(lib_FragmentSeriesNumber, collapse = ";"),
            lib_FragmentLossType = paste(lib_FragmentLossType, collapse = ";")
        ), by = .(ModifiedPeptide, UniprotID, RetentionTime, IonMobility, lib_PrecursorMz, lib_PrecursorCharge, lib_FragmentMz, lib_FragmentCharge, lib_FragmentMass, lib_RelativeIntensity)]
        
        dt <- dt[lib_RelativeIntensity > base]
        
    } else if (lib_type == "DIA") {
        dt <- dt[, .( ModifiedPeptide = PeptideSequence, 
                     UniprotID, 
                     RetentionTime = Tr_recalibrated,
                     IonMobility,
                     lib_PrecursorMz = PrecursorMz, 
                     lib_PrecursorCharge = PrecursorCharge, 
                     lib_FragmentMz = ProductMz, 
                     lib_FragmentCharge = FragmentCharge, 
                     lib_RelativeIntensity = LibraryIntensity, 
                     lib_FragmentType = FragmentType, 
                     lib_FragmentSeriesNumber = FragmentSeriesNumber, 
                     lib_FragmentLossType = FragmentLossType)]
        
        dt[, lib_FragmentMass := (lib_FragmentMz * lib_FragmentCharge) - (1.00728 * lib_FragmentCharge)]
        
        dt <- dt[, .(
            lib_FragmentType = paste(lib_FragmentType, collapse = ";"),
            lib_FragmentSeriesNumber = paste(lib_FragmentSeriesNumber, collapse = ";"),
            lib_FragmentLossType = paste(lib_FragmentLossType, collapse = ";")
        ), by = .(ModifiedPeptide, UniprotID, RetentionTime, IonMobility, lib_PrecursorMz, lib_PrecursorCharge, lib_FragmentMz, lib_FragmentCharge, lib_FragmentMass, lib_RelativeIntensity)]
        
        dt <- dt[lib_RelativeIntensity > base]
    }
 #}

    
    return(dt)
}

#############
library(data.table)
library(stringr)

process_diann_report <- function(diann_report, base){
    # Convert to data.table
    dt <- as.data.table(diann_report)
    # Remove trailing semicolons
    cols_to_clean <- c("Fragment.Quant.Raw", "Fragment.Quant.Corrected", "Fragment.Info")
    dt[, (cols_to_clean) := lapply(.SD, function(x) str_remove(x, ";$")), .SDcols = cols_to_clean]
    
    # Separate rows by semicolon
    dt <- dt[, lapply(.SD, function(x) unlist(strsplit(x, ";"))), by = .(File.Name, Stripped.Sequence, Protein.Ids, Precursor.Mz, Precursor.Charge), .SDcols = cols_to_clean]
    
    dt[, c("FragmentType", "FragmentMz") := tstrsplit(Fragment.Info, "/", fixed = TRUE)]
    # Split Fragment.Info into multiple columns
    dt[, c("FragmentType", "FragmentCharge") := tstrsplit(FragmentType, "^", fixed = TRUE)]
    dt[, c("FragmentType", "FragmentLossType") := tstrsplit(FragmentType, "-", fixed = TRUE)]
    dt[, c("FragmentType", "FragmentSeriesNumber") := tstrsplit(FragmentType, "(?<=[A-Za-z])(?=[0-9])", perl = TRUE)]
    
    # Convert columns to numeric
    dt[, Fragment.Quant.Raw := as.numeric(Fragment.Quant.Raw)]
    dt[, Fragment.Quant.Corrected := as.numeric(Fragment.Quant.Corrected)]
    dt[, FragmentMz := as.numeric(FragmentMz)]
    dt[, FragmentCharge := as.numeric(FragmentCharge)]
    dt[, FragmentSeriesNumber := as.numeric(FragmentSeriesNumber)]
    
    # Calculate FragmentMass
    dt[, FragmentMass := (FragmentMz * FragmentCharge) - (1.00728 * FragmentCharge)]
    
    # Calculate RelativeIntensity
    dt[, RelativeIntensity := Fragment.Quant.Raw / max(Fragment.Quant.Raw), by = .(File.Name, Stripped.Sequence, Precursor.Mz, Precursor.Charge)]
    
    # Select and rename columns
    dt <- dt[, .(File.Name, ModifiedPeptide = Stripped.Sequence, UniprotID = Protein.Ids, 
                 diann_report_PrecursorMz = Precursor.Mz, diann_report_PrecursorCharge = Precursor.Charge,
                 diann_report_FragmentMz = FragmentMz, diann_report_FragmentCharge = FragmentCharge, 
                 diann_report_FragmentMass = FragmentMass, diann_report_RelativeIntensity = RelativeIntensity, 
                 diann_report_FragmentType = FragmentType, diann_report_FragmentSeriesNumber = FragmentSeriesNumber, 
                 diann_report_FragmentLossType = FragmentLossType)]
    
    # Group and summarize
    dt <- dt[, .(diann_report_FragmentType = paste(diann_report_FragmentType, collapse = ";"), 
                 diann_report_FragmentSeriesNumber = paste(diann_report_FragmentSeriesNumber, collapse = ";"), 
                 diann_report_FragmentLossType = paste(diann_report_FragmentLossType, collapse = ";")),
             by = .(File.Name, ModifiedPeptide, UniprotID, diann_report_PrecursorMz, diann_report_PrecursorCharge, 
                    diann_report_FragmentMz, diann_report_FragmentCharge, diann_report_FragmentMass, 
                    diann_report_RelativeIntensity)]
    
    # Filter by RelativeIntensity
    dt <- dt[diann_report_RelativeIntensity > base]
   
    return(dt)
}
########
align_diann_report_lib_frgs <- function(diann_report_proc, lib_proc, tol) {
    # Extract distinct peptides and file names from diann_report_proc
    diann_report_peps_SM_pair <- diann_report_proc %>%
        select(File.Name, ModifiedPeptide) %>%
        distinct()
    # Join with lib_proc to get all possible pairs
    lib_proc <- diann_report_peps_SM_pair %>%
        inner_join(lib_proc, by = "ModifiedPeptide", relationship = "many-to-many")
    
    # Find matching fragments within the tolerance
    matched_frgs_within_tol <- diann_report_proc %>%
        inner_join(lib_proc, by = c("ModifiedPeptide", "UniprotID", "File.Name"), relationship = "many-to-many") %>%
        mutate(mz_dif = abs(lib_FragmentMz - diann_report_FragmentMz)) %>%
        filter(mz_dif <= tol,
               lib_PrecursorCharge == diann_report_PrecursorCharge,
               lib_FragmentCharge == diann_report_FragmentCharge)
    # Identify unmatched fragments in lib_proc
    unmatched_frgs_lib <- lib_proc %>%
        anti_join(matched_frgs_within_tol, by = c("File.Name", "ModifiedPeptide", "UniprotID", "lib_FragmentMz"))
    
    # Identify unmatched fragments in diann_report_proc
    unmatched_frgs_diann_report <- diann_report_proc %>%
        anti_join(matched_frgs_within_tol, by = c("File.Name", "ModifiedPeptide", "UniprotID", "diann_report_FragmentMz"))
    
    # Combine matched and unmatched fragments
    aligned_diann_report_lib_frgs <- bind_rows(matched_frgs_within_tol, unmatched_frgs_lib, unmatched_frgs_diann_report)
    
    # Arrange and clean up the combined data
    aligned_diann_report_lib_frgs <- aligned_diann_report_lib_frgs %>%
        arrange(File.Name, ModifiedPeptide) %>%
        distinct() %>%
        mutate(
            lib_RelativeIntensity = ifelse(is.na(lib_RelativeIntensity), 0, lib_RelativeIntensity),
            diann_report_RelativeIntensity = ifelse(is.na(diann_report_RelativeIntensity), 0, diann_report_RelativeIntensity),
            lib_PrecursorCharge2 = ifelse(is.na(lib_PrecursorCharge), diann_report_PrecursorCharge, lib_PrecursorCharge),
            diann_report_PrecursorCharge2 = ifelse(is.na(diann_report_PrecursorCharge), lib_PrecursorCharge,  diann_report_PrecursorCharge)
        )
    
    return(aligned_diann_report_lib_frgs)
}

#############
calculate_cos_sim <- function(aligned_diann_report_lib_frgs){
     cosine_similarity <- function(x, y) sum(x * y)/(sqrt(sum(x^2))* sqrt(sum(y^2)))
     calculated_cos_sim <- aligned_diann_report_lib_frgs %>%
            group_by(File.Name, ModifiedPeptide, diann_report_PrecursorCharge2, lib_PrecursorCharge2)%>%
            mutate(cosine_similarity = round(cosine_similarity(diann_report_RelativeIntensity, lib_RelativeIntensity), 2))%>%
            ungroup()
     calculated_cos_sim <- calculated_cos_sim[, c(1,2,3,4,15,5,16,6,17,7,18,8,19,9,20, 10,21,11,22, 12,23,27)]
     return(calculated_cos_sim)
}
#######
###
summarize_cos_sim <- function(cos_sim_lib1, cos_sim_lib2){
        cos_sim_lib1_smry <- cos_sim_lib1 %>%
             group_by(UniprotID, ModifiedPeptide, PrecursorCharge)%>%
             summarize(pep_cou = n(), cosine_similarity_mad = mad(cosine_similarity))%>%
             ungroup()
        cos_sim_lib2_smry <- cos_sim_lib2 %>%
             group_by(UniprotID, ModifiedPeptide, PrecursorCharge)%>%
             summarize(pep_cou = n(), cosine_similarity_mad = mad(cosine_similarity), cosine_similarity_mad = cosine_similarity_mad)%>%
             ungroup()
        cos_sim_lib_smry <- bind_rows("lib1" = cos_sim_lib1_smry, "lib2" = cos_sim_lib2_smry, .id = "cat")
        return(cos_sim_lib_smry)
}
##########
select_the_best_in_lib1_lib2 <- function(cos_sim_lib1, cos_sim_lib2){
  shared_lib1_lib2_by_pep <- cos_sim_lib1 %>% 
    inner_join(cos_sim_lib2, 
               by = c("File.Name", "UniprotID", "ModifiedPeptide", "PrecursorCharge"),
               suffix = c("_lib1", "_lib2")) %>% 
    mutate(cos_sim_dif = cosine_similarity_lib1 - cosine_similarity_lib2)
  
  if (all(shared_lib1_lib2_by_pep$cos_sim_dif > 0)) {
    shared_lib1_lib2_by_pep <- shared_lib1_lib2_by_pep %>% 
      mutate(match = "better_match_in_lib1")
  } else if (all(shared_lib1_lib2_by_pep$cos_sim_dif < 0)) {
    shared_lib1_lib2_by_pep <- shared_lib1_lib2_by_pep %>% 
      mutate(match = "better_match_in_lib2")
  } else if (all(shared_lib1_lib2_by_pep$cos_sim_dif == 0)) {
    shared_lib1_lib2_by_pep <- shared_lib1_lib2_by_pep %>% 
      mutate(match = "the_same_match")
  } else {
    shared_lib1_lib2_by_pep <- shared_lib1_lib2_by_pep %>% 
      mutate(match = case_when(
        cos_sim_dif > 0 ~ "better_match_in_lib1",
        cos_sim_dif == 0 ~ "the_same_match",
        cos_sim_dif < 0 ~ "better_match_in_lib2"
      ))
  }
  
  shared_lib1_lib2_by_pep <- shared_lib1_lib2_by_pep %>%
    group_by(UniprotID, ModifiedPeptide, PrecursorCharge) %>%
    mutate(sample_cou = n()) %>%
    group_by(UniprotID, ModifiedPeptide, PrecursorCharge, match, sample_cou) %>%
    summarize(match_cou = n(), .groups = "drop")

  shared_lib1_lib2_by_pep_l <- pivot_wider(shared_lib1_lib2_by_pep, names_from = match, 
                                           values_from = match_cou, values_fill = 0)

  col_names <- names(shared_lib1_lib2_by_pep_l)

  if ("better_match_in_lib1" %in% col_names & "better_match_in_lib2" %in% col_names) {
    best_in_lib1_lib2 <- shared_lib1_lib2_by_pep_l %>%
      mutate(better_match_in_lib1_percent = round(better_match_in_lib1 / sample_cou * 100, 2),
             better_match_in_lib2_percent = round(better_match_in_lib2 / sample_cou * 100, 2),
             better_in_lib1_by = better_match_in_lib1_percent - better_match_in_lib2_percent,
             retrieve_from = ifelse(better_in_lib1_by >= 0 , "lib1", "lib2"))
  } else if ("better_match_in_lib1" %in% col_names) {
    best_in_lib1_lib2 <- shared_lib1_lib2_by_pep_l %>%
      mutate(retrieve_from = "lib1")
  } else if ("better_match_in_lib2" %in% col_names) {
    best_in_lib1_lib2 <- shared_lib1_lib2_by_pep_l %>%
      mutate(retrieve_from = "lib2")
  } else {
    best_in_lib1_lib2 <- shared_lib1_lib2_by_pep_l %>%
      mutate(retrieve_from = "lib1")
  }

  return(best_in_lib1_lib2)
}
##########################
retrieve_from_lib <- function(cos_sim_lib1 , cos_sim_lib2, best_in_lib1_lib2, lib1, lib2){
   shared_lib1_best <- best_in_lib1_lib2 %>%
          filter(retrieve_from == "lib1")%>%
          select(ModifiedPeptide, PrecursorCharge)
#############
   shared_lib2_best <- best_in_lib1_lib2 %>%
          filter(retrieve_from == "lib2")%>%
          select(ModifiedPeptide, PrecursorCharge)
#############
   unique_lib1 <- cos_sim_lib1 %>% 
          anti_join(cos_sim_lib2, by  = c("File.Name", "UniprotID", "ModifiedPeptide", "PrecursorCharge"))%>%
          select(ModifiedPeptide, PrecursorCharge)
#######
   unique_lib2 <- cos_sim_lib2 %>% 
          anti_join(cos_sim_lib1, by  = c("File.Name", "UniprotID", "ModifiedPeptide", "PrecursorCharge"))%>%
          select(ModifiedPeptide, PrecursorCharge)
#######
   unique_retrieve_from_lib1 <- lib1 %>%
          inner_join(unique_lib1, by = c("ModifiedPeptide", c("lib_PrecursorCharge" = "PrecursorCharge")))   
######
   
   unique_retrieve_from_lib2 <- lib2 %>%
          inner_join(unique_lib2, by = c("ModifiedPeptide", c("lib_PrecursorCharge" = "PrecursorCharge"))) 
######
   shared_retrieve_from_lib1 <- lib1 %>%
          inner_join(shared_lib1_best, by = c("ModifiedPeptide", c("lib_PrecursorCharge" = "PrecursorCharge")))
######
   shared_retrieve_from_lib2 <- lib2 %>%
          inner_join(shared_lib2_best, by = c("ModifiedPeptide", c("lib_PrecursorCharge" = "PrecursorCharge")))
######
   lib_v2 <- bind_rows("shared_lib1" = shared_retrieve_from_lib1,
                         "unique_lib1" = unique_retrieve_from_lib1,
                         "shared_lib2" = shared_retrieve_from_lib2,
                         "unique_lib2" = unique_retrieve_from_lib2, .id ="cat")
   lib_v2 <- lib_v2 %>%
      rename_with(~ gsub("^lib_", "", .), starts_with("lib_"))
     return(lib_v2)
}

#####################
##############ui
library(shiny)
library(shinyjs)
library(shinyBS)
library(shinycssloaders)

ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      body {
        background-color: #ADD8E6; /* lightblue */
      }
      .title {
        text-align: center; /* Center the title */
        color: #000000; /* Title color */
        font-weight: bold; /* Make the title bold */
        font-size: 24px; /* Increase font size for better visibility */
      }
    "))
  ),
  
  # Application title
  titlePanel(div(class = "title", "Shiny Application for Enhanced Post-Processing Analysis and Visualization of Quantitative Urine Proteomics and Peptidomics by MS and DIA-NN analysis")),
  
  useShinyjs(),  # Initialize shinyjs
  tabsetPanel(
    id = "tabs",
    tabPanel("calculate the cosine similarity of the library and DIAnn report spectra", value = "tab1",
             fluidRow(
               column(width = 4, 
                      h3("Input"),
                      selectInput("lib_type", "Library Type", choices = c("DIA", "DDA")),
                      #bsTooltip(id = "lib_type", title = "Select your library type (DIA or DDA library)"),
                      actionButton("use_sample_lib", "Use Sample Data for Library"),
                      actionButton("upload_own_lib", "Upload Own Library File"),
                      uiOutput("file_ui_lib"),  # Dynamically rendered UI for library file input
                      
                      actionButton("use_sample_diann_report", "Use Sample Data for DIA-NN Report"),
                      actionButton("upload_own_diann_report", "Upload Own DIA-NN Report File"),
                      uiOutput("file_ui_diann_report")  # Dynamically rendered UI for DIA-NN report file input
               ),
               column(width = 3,
                      h3("Setting"), 
                      numericInput("tol", "fragment ion mass tolerance", value = 0.05, min = 0, max = 0.5, step = 0.01),
                      bsTooltip(id = "tol", title = "Set fragment ion mass tolerance used to match DDA and DIA libraries"),
                      numericInput("base", "fragment ion intensity threshold", value = 0, min = 0, max = 1, step = 0.1),
                      bsTooltip(id = "base", title = "Set the fragment ion intensity threshold to filter only those above the selected threshold")
               ),
               column(width = 5,
                      h3("Logs"),
                      verbatimTextOutput("msg1"),
                      verbatimTextOutput("msg2")
               ),
               column(width = 6,
                      actionButton("spec_sim", "Calculate Spectrum Similarity"), 
                      shinycssloaders::withSpinner(DTOutput('pep_info1')),
                      sliderInput("cos", "Filter", 0, 1, c(0,1))
               ),
               column(width = 6,
                      h3("Output2"),
                      #actionButton("plot", "Plot the library versus DIAnn report spectra for selected peptides"),
                      downloadButton("download", "Download")
               )
             )
    ),
    tabPanel("plot the library and DIAnn report data", value = "tab2",
             fluidRow(
               column(4,
                      selectInput("SM", "Select SM", choices = NULL, selected = ""),
                      verbatimTextOutput("msg4")
               ),
               column(4,
                      selectInput("prt_acc", "Select Protein", choices = NULL, selected = ""),
                      verbatimTextOutput("msg5")
               ),
               column(4,
                      selectizeInput("pep_seq", "Select Peptide", choices = NULL, selected = ""),
                      verbatimTextOutput("msg6")
               )
             ),
             fluidRow(
               column(width = 3,
                      plotOutput("fig1", height = 400)
               ),
               column(3,
                      plotOutput("fig2", height = 400)
               ),
               column(3,
                      plotOutput("fig3", height = 400)
               ),
               column(3,
                      plotOutput("fig4", height = 400)
               )
             ),
             fluidRow(
               column(6,
                      plotOutput("fig5", height = 400)
               )
             )
    ),
     tabPanel("Creating better lib", value = "tab3",
             fluidRow(
               column(width = 4, 
                      h5("Input1"),
                      actionButton("use_sample_cos_sim_data_lib1", "Use Sample similarity Data1"),
                      bsTooltip(id = "use_sample_cos_sim_data_lib1", title = "Use Sample Data for cosine similarity of library1 against DIA data"),
                      actionButton("upload_own_cos_sim_data_lib1", "Upload Own similarity Data1"),
                      bsTooltip(id = "upload_own_cos_sim_data_lib1", title = "Upload your Own Data for cosine similarity of library1 against DIA data"),
                      uiOutput("file_ui_cos_sim_data_lib1"),  # Dynamically rendered UI for DIA-NN report file input
                      selectInput("lib1_type", "Library1 Type", choices = c("DDA", "DIA")),
                      #bsTooltip(id = "lib_type", title = "Select your library type (DIA or DDA library)"),
                      actionButton("use_sample_lib1", "Use Sample Data for Library"),
                      actionButton("upload_own_lib1", "Upload Own Library File"),
                      uiOutput("file_ui_lib1"),  # Dynamically rendered UI for library file input
                      actionButton("compare", "compare the cosine similarities of 2 datasets")
                     
                      
               ),
               column(width = 4,
                      h5("Input2"),
                      actionButton("use_sample_cos_sim_data_lib2", "Use Sample similarity Data1"),
                      bsTooltip(id = "use_sample_cos_sim_data_lib2", title = "Use Sample Data for cosine similarity of library1 against DIA data"),
                      
                      actionButton("upload_own_cos_sim_data_lib2", "Upload Own similarity Data1"),
                      bsTooltip(id = "upload_own_cos_sim_data_lib2", title = "Upload your Own Data for cosine similarity of library1 against DIA data"),
                      uiOutput("file_ui_cos_sim_data_lib2"),  # Dynamically rendered UI for DIA-NN report file input
                      selectInput("lib2_type", "Library2 Type", choices = c("DIA", "DDA")),
                      #bsTooltip(id = "lib_type", title = "Select your library type (DIA or DDA library)"),
                      actionButton("use_sample_lib2", "Use Sample Data for Library"),
                      actionButton("upload_own_lib2", "Upload Own Library File"),
                      uiOutput("file_ui_lib2"),  # Dynamically rendered UI for library file input
                      actionButton("retrieve", "retrieve from either library1 or library2"),
    #selectInput("instrument", "select an instrument", choices = c("Bruker", "Absciex", "Thermo")),
                      
               ),
              column(width = 4, 
                     h5("logs"),
                     verbatimTextOutput("msg_cos1"),
                     verbatimTextOutput("msg_lib1"),  
                     h5("logs"),
                      verbatimTextOutput("msg_cos2"),
                      verbatimTextOutput("msg_lib2")          
             )
         ),
        fluidRow(
           
           column(4,
                 plotOutput("fig6", height = 400)
              ),
           column(4,
                 plotOutput("fig7", height = 400)
              ),
           column(4,
                 plotOutput("fig8", height = 400)
              )
          ),
         fluidRow(
         
          column(4,
              downloadButton("download2", "Download")
              #shinycssloaders::withSpinner(DTOutput('retrieve_data'))  
            )
         )       
    ),
    tabPanel("Help", value = "tab4",
             fluidRow(
               column(4,
                      selectInput("file_type", "Input files", choices = c("DDA", "DIA", "DIAnn report file"))
               ),
               column(6,
                      verbatimTextOutput("msg7")
               )
             )
    )
  )
)
          
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
            req(data_dda())
            processed_lib <- process_lib(data_dda(), "DDA", input$base)
        } else {
            req(data_dia())
            processed_lib <- process_lib(data_dia(), "DIA", input$base)
        }
    } else {
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
            lib <- read_file_based_on_extension(lib_path)
        } else {
            is_DIA_lib_valid <- validate_DIA_lib(lib_path)
            if (is.character(is_DIA_lib_valid)) {
                add_log_message(is_DIA_lib_valid)
                output$msg1 <- renderText(is_DIA_lib_valid)  # Show error message in UI
                return(NULL)
            }
            lib <- read_file_based_on_extension(lib_path)
        }
        global$proc_lib_notify <- showNotification(HTML("################################<br>The processing of DIA-nn report file is IN PROCESS<br>################################"), duration = NULL, type = "message")
        processed_lib <- process_lib(lib, input$lib_type, input$base)
    }
    
    return(processed_lib)
})

# Observe validation messages separately to prevent errors inside reactive expressions
observeEvent(lib_proc(), {
    removeNotification(global$proc_lib_notify)
    if (is.null(lib_proc())) {
        output$msg1 <- renderText("Library processing failed due to validation errors.")
    } else {
        output$msg1 <- renderText("Library successfully processed.")
    }
})

 ########################### 
  # Display message based on processed library data
  observeEvent(lib_proc(), {
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
      req(data_diann_report())
      diann_report_proc <- process_diann_report(data_diann_report(), input$base)
    } else {
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
            
                   if(input$file_type == "DDA"){
                      output$msg7 <- renderText({
                         paste0("for proper data processing;\nDDA library file should contain the following columns;\nModifiedPeptide\nUniprotID\nPrecursorMz\nPrecursorCharge\nFragmentMz\nFragmentCharge\nRelativeIntensity\nFragmentType\nFragmentSeriesNumber\nFragmentLossType")
                       })
                   }
              else if(input$file_type == "DIA"){
                   output$msg7 <- renderText({
                      paste0("for proper data processing;\nDIA library file should contain the following columns;\nPeptideSequence\nUniprotID\nPrecursorMz\nPrecursorCharge\nProductMz\nFragmentCharge\nLibraryIntensity\nFragmentType\nFragmentSeriesNumber\nFragmentLossType")
                  })
               }
             else{
                  output$msg7 <- renderText({
                     paste0("for proper data processing;\nDIAnn report file should contain the following columns;\nFile.Name\nStripped.Sequence\nProtein.Ids\nPrecursor.Mz\nPrecursor.Charge\nRT\niRT\nIM\niIM\nQ.Value\nPEP\nMs1.Area\nPrecursor.Quantity\nFragment.Info\nFragment.Quant.Raw\nFragment.Quant.Corrected\nFragment.Correlations")
                 })
              }

   })
} 
shinyApp(ui, server)