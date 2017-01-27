states <- state.abb
states[length(states)+1] <- c("DC")

library(RCurl) 
library(XML)
library(scrapeR)
# url <- "http://townhall.com/election/2016/president/wa/county"
# tables <- readHTMLTable(url)
# 
# wa.state <- as.data.frame(tables[[1]])
# wa.county <- as.data.frame(tables[[2]])

county.list <- list()
state.list <- list()

for (i in 1:51) {
  url <- paste("http://townhall.com/election/2016/president/", states[i], 
               "/county", sep="")
  tables <- readHTMLTable(url)
  county.list[[states[i]]] <- as.data.frame(tables[[2]])
  state.list[[states[i]]] <- as.data.frame(tables[[1]])
}
