# Visualising Bayesian Case Influence
Repository for code used in the 2019 EBS thesis 'Visualising Bayesian Case Influence'. 

Also includes code for the 'Shiny App'.

The data folder contains some data from https://mba.tuck.dartmouth.edu/pages/faculty/ken.french/data_library.html but more importantly, the .rds objects generated from the code. Some files were too large (~40mb) and couldn't be uploaded. These objects will need to be generated again by setting 'write=TRUE' in the 'fitBayesianCAPM' function. However, many of these objects were used in generating .gif files for the Shiny App so its not absolutely necessary to have them.

The file function_definitions.R contain all the functions used in the main file.

The file Main.rmd contains all the relevant code to produce the diagnostics in the paper. Many of the chunks are self-contained and can be executed without any prerequisite code. The exception is that the .rds objects for the specified model doesn't exist. In that case, they must be generated again by setting 'write=TRUE' in the 'fitBayesianCAPM' function.

Some chunks produce outputs specifically for the Shiny App, such as the .rds objects and .gif files. 
