plotBilancio <-  function(data, cumulative = TRUE, threshold = 1500, datesRange){
  
  
  minDate <- datesRange[1]
  maxDate <- datesRange[2]
  
  dtf_days <- data.frame("date" = seq(from = minDate, to = maxDate, by = 1))
  
  data <- dplyr::left_join(dtf_days, data, by = c("date"))
  data$amount[is.na(data$amount)] <- 0

  data <- group_by(data, date) %>% dplyr::summarise(amount = sum(amount), 
                                                    descrizione = paste0(description, collapse = "\n"))
  
  
  data$descrizione[data$descrizione == "NA"] <- ""
  
  
  if(cumulative){
    data$amount <- cumsum(data$amount)
  }
  
  miny <- min(0, threshold, min(data$amount))
  maxy <- max(threshold, max(data$amount))*1.2
  
  plot <- plotly::plot_ly(data, x = ~date, y = ~amount, 
                          type = 'scatter', 
                          mode = 'lines', 
                          line = list(color = 'green', width = 4),
                          name = "Balance",
                          text = ~descrizione) %>%
    add_segments(x = minDate, xend = maxDate, y = threshold, yend = threshold,
                 line = list(color = 'orange', width = 4), name = "") %>%
    layout(xaxis = list(range = c(minDate, maxDate)), 
           yaxis = list(range = c(miny, maxy)),
           showlegend = FALSE,
           hovermode = 'compare')
  
  return(plot)
}

plotUscite <- function(data, cumulative = TRUE, threshold = 1500, datesRange){
  
  
  minDate <- datesRange[1]
  maxDate <- datesRange[2]
  
  dtf_days <- data.frame("date" = seq(from = minDate, to = maxDate, by = 1))
  
  data <- dplyr::left_join(dtf_days, data, by = c("date"))
  data$amount[is.na(data$amount)] <- 0
  data$amount <- -data$amount
  
  data <- group_by(data, date) %>% dplyr::summarise(amount = sum(amount), 
                                                    descrizione = paste0(description, collapse = "\n"))
  
  
  data$descrizione[data$descrizione == "NA"] <- ""
  
  
  if(cumulative){
    data$amount <- cumsum(data$amount)
  }
  
  miny <- min(0, threshold, min(data$amount))
  maxy <- max(threshold, max(data$amount))*1.2
  
  plot <- plotly::plot_ly(data, x = ~date, y = ~amount, 
                          type = 'scatter', 
                          mode = 'lines', 
                          line = list(color = 'red', width = 4),
                          name = "Outcomes",
                          text = ~descrizione) %>%
    add_segments(x = minDate, xend = maxDate, y = threshold, yend = threshold,
                 line = list(color = 'orange', width = 4), name = "") %>%
    layout(xaxis = list(range = c(minDate, maxDate)), 
           yaxis = list(range = c(miny, maxy)),
           showlegend = FALSE,
           hovermode = 'compare')
  
  return(plot)
}
