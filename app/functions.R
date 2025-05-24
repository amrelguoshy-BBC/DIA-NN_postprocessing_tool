##############################################################functions used in tab1 in shiny application
########################################################
### Core Function for Cosine Similarity Calculation ###
########################################################

### Calculate cosine similarity between DIA-NN experimental data and spectral library
# Processes input data, aligns fragment ions, and computes spectral similarity
# 
# Parameters:
#   diann_report: DIA-NN output report data
#   lib: Spectral library data (DDA or DIA format)
#   lib_type: Library type - "DDA" or "DIA" (default: "DDA")
#   tol: Mass tolerance for fragment matching (default: 0.05 Da)
#   base: Intensity threshold for fragment inclusion (default: 0)
#
# Returns:
#   Dataframe with cosine similarity scores for each peptide precursor

calculate_cosine_sim_4_DIAnn <- function(diann_report, lib, lib_type = "DDA", tol = 0.05, base = 0){
    diann_report_proc <- process_diann_report(diann_report, base)
    lib_proc <- process_lib(lib, lib_type, base)
    aligned_diann_report_lib_frgs <- align_diann_report_lib_frgs(diann_report_proc, lib_proc, tol)
    calculated_cos_sim <- calculate_cos_sim(aligned_diann_report_lib_frgs)
    return(calculated_cos_sim)
}
##############################################
### Library Validation Functions ###
##############################################

### Validate DDA Library Format
# Checks required columns and file readability for DDA libraries
#
# Parameters:
#   lib_path: Path to DDA library file
#
# Returns:
#   TRUE if valid, error message with missing columns or read error if invalid

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

### Validate DIA Library Format
# Checks required columns and file readability for DIA libraries
#
# Parameters:
#   lib_path: Path to DIA library file
#
# Returns:
#   TRUE if valid, error message with missing columns or read error if invalid
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

###validate_DIAnn_report file
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
###validate_cosine similarity data for corresponding library
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

##############################################
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

###############################################
### Data Processing Functions ###
###############################################

### Process Spectral Library Data
# Standardizes library format and calculates fragment masses
#
# Parameters:
#   lib: Library dataframe
#   lib_type: "DDA" or "DIA"
#   base: Intensity threshold for fragment inclusion
#
# Returns:
#   Processed library data.table with standardized columns

process_lib <- function(lib, lib_type, base) {
    dt <- as.data.table(lib)
     # Check if column exists, and add it if missing after RetentionTime
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

### Process DIA-NN Report Data
# Parses and standardizes DIA-NN output for downstream analysis
#
# Parameters:
#   diann_report: DIA-NN report dataframe
#   base: Intensity threshold for fragment inclusion
#
# Returns:
#   Processed DIA-NN data.table with fragment annotations

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
###############################################
### Alignment and Similarity Calculation ###
###############################################

### Align Experimental and Library Fragments
# Matches fragments between experimental data and library within mass tolerance
#
# Parameters:
#   diann_report_proc: Processed DIA-NN data
#   lib_proc: Processed library data
#   tol: Mass tolerance in Da
#
# Returns:
#   Aligned dataframe with matched/unmatched fragments
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

### Calculate Cosine Similarity
# Computes spectral similarity between aligned experimental and library fragments
#
# Parameters:
#   aligned_diann_report_lib_frgs: Aligned fragments dataframe
#
# Returns:
#   Dataframe with cosine similarity scores and metadata
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
########################################################
### Chimeric Library Creation Functions (Tab 3) ###
########################################################

### Compare Library Performances
# Aggregates cosine similarity results from two libraries
#
# Parameters:
#   cos_sim_lib1: Results from first library
#   cos_sim_lib2: Results from second library
#
# Returns:
#   Summary dataframe with MAD of cosine similarities

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
### Select Optimal Library Matches
# Identifies which library provides better matches for shared peptides
#
# Parameters:
#   cos_sim_lib1: Results from first library
#   cos_sim_lib2: Results from second library
#
# Returns:
#   Decision dataframe indicating optimal library for each peptide
 
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
### Construct Chimeric Library
# Combines optimal matches from both libraries into a new hybrid library
#
# Parameters:
#   cos_sim_lib1: Results from first library
#   cos_sim_lib2: Results from second library
#   best_in_lib1_lib2: Selection results from select_the_best_in_lib1_lib2
#   lib1: Original first library
#   lib2: Original second library
#
# Returns:
#   Combined library with optimal fragments from both input libraries
retrieve_from_lib <- function(cos_sim_lib1 , cos_sim_lib2, best_in_lib1_lib2, lib1, lib2){
   shared_lib1_best <- best_in_lib1_lib2 %>%
          filter(retrieve_from == "lib1")%>%
          select(ModifiedPeptide, PrecursorCharge)

   shared_lib2_best <- best_in_lib1_lib2 %>%
          filter(retrieve_from == "lib2")%>%
          select(ModifiedPeptide, PrecursorCharge)

   unique_lib1 <- cos_sim_lib1 %>% 
          anti_join(cos_sim_lib2, by  = c("File.Name", "UniprotID", "ModifiedPeptide", "PrecursorCharge"))%>%
          select(ModifiedPeptide, PrecursorCharge)

   unique_lib2 <- cos_sim_lib2 %>% 
          anti_join(cos_sim_lib1, by  = c("File.Name", "UniprotID", "ModifiedPeptide", "PrecursorCharge"))%>%
          select(ModifiedPeptide, PrecursorCharge)

   unique_retrieve_from_lib1 <- lib1 %>%
          inner_join(unique_lib1, by = c("ModifiedPeptide", c("lib_PrecursorCharge" = "PrecursorCharge")))   

   
   unique_retrieve_from_lib2 <- lib2 %>%
          inner_join(unique_lib2, by = c("ModifiedPeptide", c("lib_PrecursorCharge" = "PrecursorCharge"))) 

   shared_retrieve_from_lib1 <- lib1 %>%
          inner_join(shared_lib1_best, by = c("ModifiedPeptide", c("lib_PrecursorCharge" = "PrecursorCharge")))

   shared_retrieve_from_lib2 <- lib2 %>%
          inner_join(shared_lib2_best, by = c("ModifiedPeptide", c("lib_PrecursorCharge" = "PrecursorCharge")))

   lib_v2 <- bind_rows("shared_lib1" = shared_retrieve_from_lib1,
                         "unique_lib1" = unique_retrieve_from_lib1,
                         "shared_lib2" = shared_retrieve_from_lib2,
                         "unique_lib2" = unique_retrieve_from_lib2, .id ="cat")
   lib_v2 <- lib_v2 %>%
      rename_with(~ gsub("^lib_", "", .), starts_with("lib_"))
     return(lib_v2)
}
