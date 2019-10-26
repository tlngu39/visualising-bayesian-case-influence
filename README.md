# Visualising Bayesian Case Influence
Repository for code used in the 2019 EBS thesis 'Visualising Bayesian Case Influence'. 

Also includes code for the 'Shiny App'. The app is hosted at https://tlngu39.shinyapps.io/case-influence/. The shinyapps.io hosting service only allows 25 active hours of use, and so, the link may not work depending on when the user is accessing it. It is likely that the only use this app will see is when I use it to show off to my friends and family. That is unlikely to take up all 25 hours.

The data folder contains some data from https://mba.tuck.dartmouth.edu/pages/faculty/ken.french/data_library.html but more importantly, the .rds objects generated from the code. Some files were too large (~40mb) and couldn't be uploaded. These objects will need to be generated again by setting 'write=TRUE' in the 'fitBayesianCAPM' function. However, many of these objects were used in generating .gif files for the Shiny App so its not absolutely necessary to have them.

The file function_definitions.R contain all the functions used in the main file.

The file Main.rmd contains all the relevant code to produce the diagnostics in the paper. Many of the chunks are self-contained and can be executed without any prerequisite code. The exception is when the .rds objects for the specified model doesn't exist. In that case, they must be generated again by setting 'write=TRUE' in the 'fitBayesianCAPM' function. Although written as an .rmd file, the code isn't really designed to be knitted.

Some chunks produce outputs specifically for the Shiny App, such as the .rds objects and .gif files. 
