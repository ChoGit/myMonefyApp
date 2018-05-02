library(dplyr)
library(tidyr)
library(zoo)

filterBilancio <- function(bilancio, field, GrEqLe = "=", value){
  
  str_toEval <- "bilancio %>% dplyr::filter("
  
  if(is.character(value)){
    value = paste0("'", value, "'")
  }
  
  if(value == "'ALL'"){
    return(bilancio)
  }else if(GrEqLe == "="){
    str_toEval <- paste0(str_toEval, field, " == ", value, ")")
  }else if(GrEqLe == ">"){
    str_toEval <- paste0(str_toEval, field, " > ", value, ")")
  }else if(GrEqLe == ">="){
    str_toEval <- paste0(str_toEval, field, " >= ", value, ")")
  }else if(GrEqLe == "<"){
    str_toEval <- paste0(str_toEval, field, " < ", value, ")")
  }else if(GrEqLe == ">="){
    str_toEval <- paste0(str_toEval, field, " <= ", value, ")")
  }else if(GrEqLe == "contains"){
    str_toEval <- paste0(str_toEval, "grepl(", value, ", ", field, ", ","ignore.case = TRUE)", ")")
  }else{
    stop("Errore: simbolo di relazione non riconosciuto (scegliere tra: =, >, >=, <, <=, contains)")
  }
  
  FilteredBil <- eval(parse(text = str_toEval))
  
  return(FilteredBil)
}

#filterBilancio(bilancio = b, field = "description", GrEqLe = "contains", value = "mont")
