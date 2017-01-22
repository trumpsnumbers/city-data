# Function and example code for loading finance data from Yahoo
# without the need of any additional package.
#
# Written by Fotis Papailias & Dimitrios Thomakos on Dec. 31, 2011
# Contact Details: papailias@quantf.com
#                  dimitrios.thomakos@gmail.com, thomakos@quantf.com
#
# All material is provided for use as is, with no guarrantees, either expressed or implied.
# Copyright (C) under the authors' names Papailias, Fotis and Thomakos, Dimitrios for both
#
#-------------------------------------------------------------------------#
#             Quantitative Finance & Technical Trading                    #
#                     http://www.quantf.com                               #
#-------------------------------------------------------------------------#
#
# PLEASE MAINTAIN THIS HEADER IN ALL COPIES OF THIS FILE THAT YOU USE

###############################################################################################
# Main Function
#
# Input
# -----
#   tickers (text strings)
#   start.date (dates)
#   end.date (dates)
#
# Output
# -------
# 6 Double Matrices: Open, High, Low, Close, Volume, Adj. Close
###############################################################################################

data.loading <- function(tickers, start.date, end.date)
{
  # Change the locale
  #sl <- Sys.setlocale(locale="US")
  
  # Create the universe of dates
  all.dates <- seq(as.Date(start.date), as.Date(end.date), by="day")
  all.dates <- subset(all.dates,weekdays(all.dates) != "Sunday" & weekdays(all.dates) != "Saturday")
  all.dates.char <- as.matrix(as.character(all.dates))
  
  # Create sparse matrices
  open <- matrix(NA, NROW(all.dates.char), length(tickers))
  hi <- open
  low <- open
  close <- open
  volume <- open
  adj.close <- open
  
  # Name the rows correctly
  rownames(open) <- all.dates.char
  rownames(hi) <- all.dates.char
  rownames(low) <- all.dates.char
  rownames(close) <- all.dates.char
  rownames(volume) <- all.dates.char
  rownames(adj.close) <- all.dates.char
  
  # Split the start and end dates to be used in the ULR later on
  splt <- unlist(strsplit(start.date, "-"))
  a <- as.character(as.numeric(splt[2])-1)
  b <- splt[3]
  c <- splt[1]
  
  splt <- unlist(strsplit(end.date, "-"))
  d <- as.character(as.numeric(splt[2])-1)
  e <- splt[3]
  f <- splt[1]
  
  # Create the two out of the three basic components for the URL loading
  str1 <- "http://ichart.finance.yahoo.com/table.csv?s="
  str3 <- paste("&a=", a, "&b=", b, "&c=", c, "&d=", d, "&e=", e, "&f=", f, "&g=d&ignore=.csv", sep="")
  
  # Main loop for all assets
  for (i in seq(1,length(tickers),1))
  {
    str2 <- tickers[i]
    strx <- paste(str1,str2,str3,sep="")
    x <- read.csv(strx)
    
    datess <- as.matrix(x[1])
    
    replacing <- match(datess, all.dates.char)
    open[replacing,i] <- as.matrix(x[2])
    hi[replacing,i] <- as.matrix(x[3])
    low[replacing,i] <- as.matrix(x[4])
    close[replacing,i] <- as.matrix(x[5])
    volume[replacing,i] <- as.matrix(x[6])
    adj.close[replacing,i] <- as.matrix(x[7])
  }
  
  # Name the cols correctly
  colnames(open) <- tickers
  colnames(hi) <- tickers
  colnames(low) <- tickers
  colnames(close) <- tickers
  colnames(volume) <- tickers
  colnames(adj.close) <- tickers
  
  # Return the ouput
  return(list(open=open, high=hi, low=low, close=close, volume=volume, adj.close=adj.close))
}

data.loading("MSFT", Sys.Date(), Sys.Date())
