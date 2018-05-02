library(dplyr)
library(tidyr)
library(zoo)

loadBilancio <- function(){
  setwd("C:/MyDesktop/Personale/Bilancio/")
  INPUT_PATH <- "./"
  
  files <- dir(INPUT_PATH)
  files <- files[grepl(pattern = "Monefy.Data.", files)]
  files <- files[grepl(pattern = ".csv", files)]
  filesdates <- as.Date(substr(files, 13, 20), "%d-%m-%y")
  dtf_monthsDates <- data.frame("MESE" = as.yearmon(filesdates),
                            "DATA" = filesdates)
  
  dtf_datesToLoad <- group_by(dtf_monthsDates, MESE) %>% 
    dplyr::summarise(date = max(DATA)) %>% ungroup()
  
  filesToLoad <- files[grepl(pattern = paste0(format(dtf_datesToLoad$date, "%d-%m-%y"), 
                                              collapse = "|"),
                             x = files)]
  
  bilancio <- data.frame()
  
  for (file in filesToLoad){
    
    dtf_thisMonthbil <- read.csv2(file)
    bilancio <- rbind(bilancio, dtf_thisMonthbil)
    
  }
  
  bilancio$amount <- as.numeric(gsub(pattern = ",", 
                                     replacement = ".", 
                                     gsub(pattern = "\\.", 
                                          replacement = "",
                                          as.character(bilancio$amount))))
  
  bilancio$date <- as.Date(as.character(bilancio$date), "%d/%m/%Y")
  
  return(bilancio)
}
