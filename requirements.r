pacotes <- c("CLSIEP15",
             "dplyr",
             "ggExtra",
             "ggplot2",
             "mcr",
             "rhandsontable",
             "shiny",
             "shinydashboard",
             "tidyr",
             "shinyhelper")

if(sum(as.numeric(!pacotes %in% installed.packages())) > 0){
  instalador <- pacotes[!pacotes %in% installed.packages()]
  for(i in 1:length(instalador)) {
    install.packages(instalador, dependencies = T)
    break()}
}
