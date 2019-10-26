# Load all libraries
library(tidyverse)
library(dplyr)
library(reshape2)
library(gridExtra)
library(MCMCpack)
library(gganimate)
library(ggplot2)
library(plotly)
library(grid)
library(stargazer)
library(scatterplot3d)
library(plot3D)
library(animation)
library(gganimate)
library(MASS)
library(coda)

# This function is for processing data obtained from https://mba.tuck.dartmouth.edu/pages/faculty/ken.french/data_library.html
# Returns a cleaned data.frame

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
    factors_df <- read_data("data/F-F_Research_Data_Factors.csv")
    
    # Define function which selects and mutates appropriate columns depending on model choice
    FF_mutate <- function(df, factors_df) {
      temp_df <- df %>% mutate(MKTRF = factors_df$`Mkt-RF`, SMB = factors_df$SMB, HML = factors_df$HML, RF = factors_df$RF) %>%
        dplyr::select(Time, MKTRF, SMB, HML, RF, everything())
      temp_df <- apply(temp_df, 2, as.numeric) %>% as.tibble
      return(temp_df)
    }
  }
  
  else {
    factors_df <- read_data("data/F-F_Research_Data_5_Factors_2x3.csv")
    
    FF_mutate <- function(df, factors_df) {
      temp_df <- df %>% mutate(MKTRF = factors_df$`Mkt-RF`, SMB = factors_df$SMB, HML = factors_df$HML, RF = factors_df$RF, RMW = factors_df$RMW, 
                               CMA = factors_df$CMA) %>%
        dplyr::select(Time, MKTRF, SMB, HML, RMW, CMA, RF, everything())
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
#############################

# This function fits the Bayesian model
# Function takes data and vector of factors as argument
# The W matrix is stored (or loaded depending on 'write' status)
# Returns the eigen-decomposition of varcov(W), MCMC draws, and the W matrix itself
fitBayesianCAPM = function(model_data, factors, fileName, write) {
  # Define burn-in and samples
  B = 2000
  G = 10000
  M = G
  
  factors_ = factors
  # Manipulating regression formula
  formula = paste(factors[1], " ~ ", factors[2])
  if(length(factors) > 2){
    for (i in 3:length(factors)){
      formula = paste(formula, " + ", factors[i])
    }
  }
  #print(formula)
  
  # Fitting Bayesian model
  model_mcmc = MCMCregress(as.formula(formula), data = model_data, burnin = B, mcmc = M, seed = 27806189, b0 = 0, B0 = 0, c0 = 0.001, d0 = 0.001)
  
  # Converting elements in MCMC to numeric
  model_mcmc.data = apply(model_mcmc, 2, as.numeric) %>% as.matrix()
  colnames(model_mcmc)[1] = "Intercept"
  
  # Apply Thomas et al Method
  theta = model_mcmc.data[, 1:(ncol(model_mcmc.data) - 1)]
  sigma2 = model_mcmc.data[, ncol(model_mcmc.data)]
  n = length(model_data$Time)
  W = matrix(0, M, n)
  
  # Setting up y vector and X matrix
  y = as.matrix(model_data[factors[1]]) # y is excess returns
  #factors_[factors == "\`MKT-RF\`"] = "MKT-RF"
  factors_ = factors_[-1]
  X = model_data[factors_]
  X$intercept = 1 
  X = X %>% dplyr::select(intercept, everything()) # X is actual factors including intercept
  
  # Need to have everything as.matrix for matrix arithmetic to work
  
  if (write){
    # Evaluating log case importance sampling deletion weights
    for (i in 1:n){
      for (j in 1:M){
        W[j,i] = 0.5*log(2*pi*sigma2[j]) + 0.5*(1/sigma2[j])*(y[i] - as.matrix(X[i,]) %*% theta[j,] )^2
        #print(c(i, j))
      }
      #print(i)
    } 
    saveRDS(W, file = fileName)
  }
  else if (!write)
  {
    print(paste("Reading from ", fileName, sep = ""))
    W = readRDS(file = fileName)
  }
  else
    return("Error in specifying read or write of RDS file")
  
  # Storing covariance matrix of weights
  w_matrix = W
  Cov = cov(W)
  
  # Get eigen-decomposition
  Loadings_ = round(eigen(Cov)$vectors, 4)
  prop_ = round(eigen(Cov)$values / sum(eigen(Cov)$values), 4)
  
  # Returns list of: (1) dataframe containing loadings and proportions, (2) MCMC draws
  # (1) is the result of performing an eigenvector decomp on cov(W)
  return(list(data.frame(Loadings = Loadings_, prop = prop_), model_mcmc, w_matrix))
} 

# Plot PCA as plot_ly
plotPCA = function(data_df, model_df, title_) {
  # model_df should be the loadings + prop object created by fitBayesianCAPM
  # PC1 = z-axis, PC2 = x-axis, PC3 = y-axis
  zlab = paste("PC1 (", round(100 * model_df$prop[1], 0), "%)", sep = "")
  xlab = paste("PC2 (", round(100 * model_df$prop[2], 0), "%)", sep = "")
  ylab = paste("PC3 (", round(100 * model_df$prop[3], 0), "%)", sep = "")
  
  time = data_df$Time
  Loadings.df = data.frame(Time = time, PC1 = model_df[, 1], PC2 = model_df[, 2], PC3 = model_df[, 3])
  plot_ly(Loadings.df, z = ~PC1, x = ~PC2, y = ~PC3, text = ~paste("Time: ", Time, "<br>PC1: ", PC1, "<br>PC2: ", PC2, "<br>PC3: ", PC3),
          hoverinfo = "text", marker = list(size = 4)) %>%
    layout(title = title_, scene = list(xaxis = list(title = xlab), yaxis = list(title = ylab), zaxis = list(title = zlab)))
}

# Independent importance reasmpling function
resampleDraws = function(W, model_draws, case_delete){
  model_draws.df = as.data.frame(model_draws)
  # Normalise weights (must also transform since Thomas et al. computes the LOG weights)
  w = exp(W[, case_delete])
  L = nrow(W)
  w_star = w/sum(w)
  beta_resample = sample_n(model_draws.df, L, weight = w, replace = TRUE)
  return(beta_resample)
}

plotScatter3D = function(model_df, theta, threshold, xadj = -0.01, yadj = -0.01, zadj = -0.03, hide = NULL){
  pca.df = data.frame(PC1 = model_df[, 1], PC2 = model_df[, 2], PC3 = model_df[, 3],
                      prop = model_df[, ncol(model_df)], t = seq(1, ncol(model_df) - 1))
  pca.df$t = ""
  eu_dist = rep(0, nrow(pca.df))
  for(i in 1:nrow(pca.df)){
    eu_dist[i] = dist(rbind(pca.df[i, 1:3], c(0, 0, 0)))
    if(eu_dist[i] > threshold)
      pca.df$t[i] = i
  }
  
  # Allow overiding labels of certain obs
  pca.df$t[hide] = ""
  scatter3D(x = pca.df$PC2,
            y = pca.df$PC3,
            z = pca.df$PC1,
            xlab = paste("PC2 (", round(100 * pca.df$prop[2], 0), "%)", sep = ""),
            ylab = paste("PC3 (", round(100 * pca.df$prop[3], 0), "%)", sep = ""),
            zlab = paste("PC1 (", round(100 * pca.df$prop[1], 0), "%)", sep = ""),
            col = "black", 
            #colvar = pca.df$t,
            colkey = FALSE,
            surface = FALSE, 
            theta = theta, 
            phi = 20, 
            pch = 20, 
            cex = 1,
            bty = "b2",
            ticktype = "simple")
  text3D(x = pca.df$PC2 + xadj, y = pca.df$PC3 + yadj, z = pca.df$PC1 + zadj,  labels = pca.df$t,
         add = TRUE, colkey = FALSE, cex = 1)
}