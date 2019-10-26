# Defines all functions used in Shiny App

# Load relevant libraries
library(plot3D)
library(ggplot2)
library(tidyverse)

# Load model object from RDS
# The object is a list which contains:
# (1) Dataframe of Loadings + Proportions resulting from Thomas Method
loadModel = function(nameRDS){
  temp.df = readRDS(nameRDS)
  final.df = data.frame(PC1 = temp.df$Loadings.1,
                        PC2 = temp.df$Loadings.2,
                        PC3 = temp.df$Loadings.3,
                        prop = temp.df$prop)
  return(final.df)
}

# Plots scatter3D based on model object
# NOTE: The "model" object must be defined in a certain way
plotScatter3D = function(model_df, theta){
  # Colvar doesn't work with entire vector being same value
  tempCol = model_df$highlight
  if (sum(tempCol) == length(model_df$highlight) || sum(tempCol) == 0){
    scatter3D(x = model_df[, 2],
              y = model_df[, 3],
              z = model_df[, 1],
              xlab = paste("PC2 (", round(100 * model_df$prop[2], 0), "%)", sep = ""),
              ylab = paste("PC3 (", round(100 * model_df$prop[3], 0), "%)", sep = ""),
              zlab = paste("PC1 (", round(100 * model_df$prop[1], 0), "%)", sep = ""),
              col = "black", 
              #colvar = tempCol,
              colkey = FALSE,
              surface = FALSE, 
              theta = theta, 
              phi = 20, 
              pch = 20, 
              cex = 1,
              bty = "b2",
              ticktype = "detailed")
  }
  
  else{
    scatter3D(x = model_df[, 2],
              y = model_df[, 3],
              z = model_df[, 1],
              xlab = paste("PC2 (", round(100 * model_df$prop[2], 0), "%)", sep = ""),
              ylab = paste("PC3 (", round(100 * model_df$prop[3], 0), "%)", sep = ""),
              zlab = paste("PC1 (", round(100 * model_df$prop[1], 0), "%)", sep = ""),
              col = c("#000000FF", "#F8766D"), 
              colvar = tempCol,
              colkey = FALSE,
              surface = FALSE, 
              theta = theta, 
              phi = 20, 
              pch = 20, 
              cex = 1,
              bty = "b2",
              ticktype = "detailed")
  }
}

# Plots plotly with user-specified title
plot_ly_helper = function(model_df, data_df, title_){
  # PC variables make the functions easier to understand
  PC1 = 1
  PC2 = 2
  PC3 = 3
  
  zlab = getLabel(model_df, PC1)
  xlab = getLabel(model_df, PC2)
  ylab = getLabel(model_df, PC3)
  
  time = data_df$Time
  Loadings.df = data.frame(Time = time, PC1 = model_df[, 1], PC2 = model_df[, 2], PC3 = model_df[, 3])
  plot_ly(Loadings.df, z = ~PC1, x = ~PC2, y = ~PC3, text = ~paste("Time: ", Time, "<br>PC1: ", PC1, "<br>PC2: ", PC2, "<br>PC3: ", PC3),
          hoverinfo = "text", marker = list(size = 4)) %>%
    layout(title = title_, scene = list(xaxis = list(title = xlab), yaxis = list(title = ylab), zaxis = list(title = zlab)))
}

# Computes xyz labels from df containing loadings + prop
getLabel = function(model_df, PC){
  lab = paste("PC", PC, "(", round(100 * model_df$prop[PC], 0), "%)", sep = "")
  return(lab)
}

# Animate plot
animatePlot = function(model_df, startAngle,endFrame, width, height, speed, fileName) {
  nextAngle = startAngle
  saveGIF({
    for (i in 1:endFrame){
      scatter3D(x = model_df[,2], 
                y = model_df[,3], 
                z = model_df[,1],
                xlab = paste("PC2 (", round(100 * model_df$prop[2], 0), "%)", sep = ""),
                ylab = paste("PC3 (", round(100 * model_df$prop[3], 0), "%)", sep = ""),
                zlab = paste("PC1 (", round(100 * model_df$prop[1], 0), "%)", sep = ""),
                col = "black", 
                surface = FALSE, 
                theta = nextAngle, 
                phi = 20, 
                pch = 20, 
                cex = 1,
                bty = "b2",
                ticktype = "detailed")
      nextAngle = nextAngle + 1
    }
  }, interval = speed, movie.name = fileName, ani.width = width, ani.height = height) 
}

# Returns appropriate fileName given model state
getModelPCA = function(model_state) {
  state = switch(model_state, "3FF" = "3FF_PCA.gif", "3FF_cma" = "3FF_CMA_PCA.gif", "3FF_rmw" = "3FF_RMW_PCA.gif", "5FF" = "5FF_PCA.gif")
  return(state)
}

# Shiny specific: Custom reactive function for model selection via check boxes
getModelState = function(checkbox){
  # Define all possible Factor Combos and return appropriate name
  factors = sort(c("Market", "Size", "Book-to-Equity Ratio", "Profitability", "Investment"))
  factors_short = sort(c("MKTRF", "SMB", "HML", "RMW", "CMA"))

  # All Factor combinations
  factors_comb = data.frame(V1 = rep("", 5))
  for(i in 1:5){
    t = combn(factors, i)
    for(j in 1:ncol(t)){
      l = t[, j]
      # Fill column with zeroes if not of length 5
      if(length(l) != 5){
        l = c(l, rep("", 5 - length(l)))
      }
      factors_comb[, ncol(factors_comb) + 1] = sort(l, decreasing = FALSE)
    }
    # Get rid of first row on final loop which is just 0's
    # Sort is to get alphabetical naming convention
    if(i == 5)
      factors_comb = factors_comb[,-1]
  }
  
  # Check which column (portfolio) user-selection corresponds with
  col = 0
  for(i in 1:ncol(factors_comb)){
    temp = sort(unique(factors_comb[, i]))
    if(temp[1] == "")
      temp = temp[-1]
    if(all(sort(checkbox) == temp))
      col = i
  }
  portfolio = factors_comb[, col]
  
  # Turn vector of factors into fileName
  name = ""
  for(i in portfolio){
    if(i != ""){
      name = paste(name, factors_short[which(i == factors)], sep = "_")
    }
  }
  fileName = paste("shiny", name, sep = "")
  return(fileName)
}

# The above gets the states and the following returns data frame
getModel = function(state){
  fileName = paste("objects/", state, ".rds", sep = "")
  return(readRDS(fileName))
}

# Assigns 'highlight' states based on date range
# The args sDate, eDate should be strings of dates
getModelColour = function(df, time, sDate = 200000, eDate = 200001, sep_ = "", threshold = -99){
  startDate = processDate(sDate, sep = sep_)
  endDate = processDate(eDate, sep = sep_)
  
  # Process Time vector into decimals
  tempTime = processDate(time)
  
  # Observations between dates
  repl = (endDate - startDate) * 12
  
  # need to create vector with noCOlor * distance between start and date
  # color * repl + endDate + end 
  #highlightVector = c(rep(FALSE, (startDate - tempTime[1]) * 12), rep(TRUE, (endDate - startDate) * 12 + 1), rep(FALSE, (tempTime[length(tempTime)] - endDate) * 12 + 1))
  highlightVector = c()
  if (threshold == -99){
    highlightVector = c(rep(FALSE, (which(startDate == tempTime) - 1)), 
                        rep(TRUE, which(endDate == tempTime) - which(startDate == tempTime) + 1), 
                        rep(FALSE, length(tempTime) - which(endDate == tempTime)))
  }
  else if(threshold >= 0){
    # Compare euclidean distance
    
    for(i in 1:nrow(df)){
      distance = dist(rbind(df[i, 1:3], c(0, 0, 0)))
      if(distance > threshold)
        highlightVector[i] = TRUE
      else
        highlightVector[i] = FALSE
    }
  }
  
  df = df %>% mutate(highlight = highlightVector)
  return(df)
}

# Function to process dates into decimal form
processDate = function(date, sep = ""){
  # Date 2001-12 should have sep = "-"
  # Date 200112 should have sep = ""
  temp.date = substr(date, 0, 7)
  final.date = as.numeric(substr(temp.date, 0, 4)) + as.numeric(substr(temp.date, 5 + nchar(sep), 5 + nchar(sep) + 1))/12
  return(final.date)
}