library(dplyr)
library(tidyr)
library(zoo)

setwd("C:/MyDesktop/Personale/Bilancio/")
INPUT_PATH <- "./"

files <- dir(INPUT_PATH)
files <- files[grepl(pattern = "Monefy.Data.", files)]
files <- files[grepl(pattern = ".csv", files)]
lastFile <- max(files)

thisMonth <- paste0("20", substr(lastFile, 19, 20), "-", substr(lastFile, 16, 17), "-01")
thisMonth <- as.yearmon(thisMonth)
nextMonth <- thisMonth + 1/12

dtf_days <- data.frame("DATA" = seq(from = as.Date(thisMonth), to = as.Date(nextMonth)-1, by = 1))

bilancio <- read.csv2(file = lastFile)

usefulCols <- c("date", "category", "amount", "description")
bilancio <- bilancio[, usefulCols] 
bilancio$amount <- as.numeric(gsub(pattern = ",", 
                                   replacement = ".", 
                                   gsub(pattern = "\\.", 
                                        replacement = "",
                                        as.character(bilancio$amount))))

bilancio$date <- as.Date(as.character(bilancio$date), "%d/%m/%Y")

bilancio$category <- toupper(as.character(bilancio$category))
bilancio$category[bilancio$category == "DEPOSITI"] <- "RIMBORSI"

myCategories <- c("CIBO",	           "CASA",	                       "COMUNICAZIONI",	
                  "ABBIGLIAMENTO",	 "SPORT",	                       "TRASPORTI",	"SVAGO", 
                  "MANGIARE FUORI",  "SALUTE E ASSICURAZIONI VITA",	 "VACANZE",	  "VARIE E IMPREVISTI",
                  "REGALI")

dtf_dummy <- data.frame("date" = "01/04/2018", 
                        "category" = myCategories, 
                        "amount" = 0, 
                        "description" = "dummyCost")

myRevenues <- c("STIPENDI", "BUONI PASTO", "RIMBORSI")

isReve <- bilancio$category %in% myRevenues

revenues <- bilancio[isReve,]
costs <- bilancio[!isReve,]

costs <- rbind(costs, dtf_dummy)

costs <- dplyr::group_by(costs, date, category) %>%
  dplyr::summarise(amount = sum(amount))

revenues$amount <- abs(revenues$amount)
costs$amount <- abs(costs$amount)

costs$category <- as.factor(costs$category)

costs <- spread(costs, category, amount, fill = 0, drop = TRUE)

colnames(costs)[colnames(costs) == "date"] = "DATA"

costs <- dplyr::left_join(dtf_days, costs, by = c("DATA"))
costs[is.na(costs)] <- 0

costs <- costs[,c("DATA", myCategories)]

write.csv2(x = costs, file = "spreadCosts.csv", row.names = FALSE)

