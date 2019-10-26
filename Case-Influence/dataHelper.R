# This file defines all the functions used in the analysis of case influence

# Loads the .csv files obtained from Kenneth French's website into appropriate data frames

# Loads the data without initial text thats in the .csv files - PRIVATE FUNCTION
read_data <- function(path) {
  skip_num <- 0
  rows <- 1
  df <- read_csv(path, col_names = FALSE, col_types = cols(col_double()))
  while(is.na(df$X1[rows])) {
    skip_num <- skip_num + 1
    df <- read_csv(path, col_names = FALSE, col_types = cols(col_double()), skip = skip_num)
  }
  final_df <- read_csv(path, skip = skip_num-1)
  #print(skip_num)
  return(final_df)
}

load <- function(path, frequency, weight = "value", model = "5FF") {
  # Load data in appropriate formats
  src_df <- read_data(path)
  num_df <- apply(src_df, 2, as.numeric) %>% as.tibble
  
  if (model == "3FF") {
    path = file.path("data", "f-f_research_data_factors.csv")
    factors_df <- read_data(path)
    
    # Define function which selects and mutates appropriate columns depending on model choice
    FF_mutate <- function(df, factors_df) {
      temp_df <- df %>% mutate(`MKT-RF` = factors_df$`Mkt-RF`, SMB = factors_df$SMB, HML = factors_df$HML, RF = factors_df$RF) %>%
        dplyr::select(Time, `MKT-RF`, SMB, HML, RF, everything())
      temp_df <- apply(temp_df, 2, as.numeric) %>% as.tibble
      return(temp_df)
    }
  }
  
  else {
    path = file.path("data", "f-f_research_data_5_factors_2x3.csv")
    factors_df <- read_data(path)
    
    FF_mutate <- function(df, factors_df) {
      temp_df <- df %>% mutate(`MKT-RF` = factors_df$`Mkt-RF`, SMB = factors_df$SMB, HML = factors_df$HML, RF = factors_df$RF, RMW = factors_df$RMW, 
                               CMA = factors_df$CMA) %>%
        dplyr::select(Time, `MKT-RF`, SMB, HML, RMW, CMA, RF, everything())
      temp_df <- apply(temp_df, 2, as.numeric) %>% as.tibble
      return(temp_df)
    }
  }
  factors_df <- apply(factors_df, 2, as.numeric) %>% as.tibble
  
  # Get start/end dates
  START_m <- num_df %>% filter(str_length(X1) == str_length(201901)) %>% dplyr::select(X1) %>% min(na.rm = TRUE)
  # Need to get highest date from factors_df and num_df to have consistency
  START_m <- max(as.numeric(factors_df %>% filter(str_length(X1) == str_length(201901)) %>% dplyr::select(X1) %>% min(na.rm = TRUE)), START_m)
  START_y <- as.numeric(substr(START_m, 1, 4))
  # Rounds appropriately
  if (as.numeric(substr(START_m, 5, 6)) >= 6) {
    START_y <- START_y + 1
  }
  END_m <- num_df %>% filter(str_length(X1) == str_length(201901)) %>% dplyr::select(X1) %>% max(na.rm = TRUE)
  END_m <- max(as.numeric(factors_df %>% filter(str_length(X1) == str_length(201901)) %>% dplyr::select(X1) %>% min(na.rm = TRUE)), END_m)
  END_y <- as.numeric(substr(END_m, 1, 4))
  if (as.numeric(substr(END_m, 5, 6)) < 6) {
    END_y <- END_y - 1
  }
  
  # Slice vector based on dates and portfolio type
  flags_start_m <- which(src_df == START_m)
  flags_end_m <- which(src_df == END_m)
  flags_start_y <- which(src_df == START_y)
  flags_end_y <- which(src_df == END_y)
  
  # Slice dataframe of factors -> First check which
  factors_start_m <- which(factors_df == START_m)
  factors_end_m <- which(factors_df == END_m)
  factors_start_y <- which(factors_df == START_y)
  factors_end_y <- which(factors_df == END_y)
  
  # .CSV file is organised as Value-Monthly, Equal-Monthly, Value-Annual, Value-Annual
  if (frequency == "monthly") {
    if (weight == "equal"){
      factors_df <- factors_df[factors_start_m[1]:factors_end_m[1],]
      final_df <- num_df[flags_start_m[2]:flags_end_m[2],] %>% rename(Time = X1) 
      final_df <- FF_mutate(final_df, factors_df)
      return(final_df)
    }
    else if (weight == "value"){
      factors_df <- factors_df[factors_start_m[1]:factors_end_m[1],]
      final_df <- num_df[flags_start_m[1]:flags_end_m[1],] %>% rename(Time = X1)
      final_df <- FF_mutate(final_df, factors_df)
      return(final_df)
    }
    else
      return(NULL)
  }
  else if(frequency == "annual") {
    if (weight == "equal") {
      factors_df <- factors_df[factors_start_m[2]:factors_end_m[2],]
      final_df <- num_df[flags_start_m[2]:flags_end_m[2],] %>% rename(Time = X1)
      final_df <- FF_mutate(final_df, factors_df)
      return(final_df)
    }
    else if (weight == "value") {
      factors_df <- factors_df[factors_start_m[2]:factors_end_m[2],]
      final_df <- num_df[flags_start_m[1]:flags_end_m[1],] %>% rename(Time = X1)
      final_df <- FF_mutate(final_df, factors_df)
      return(final_df)
    }
    else
      return(NULL)
  }
  else
    return(NULL)
}
