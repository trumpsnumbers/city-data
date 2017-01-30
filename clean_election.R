clean.county.data <- function (state) {
    column <- state$County
    indices <- grep("\n", column)
    lengths <- c(indices[-1], length(column)+1) - indices
    county.names <- sub("\n.*", "", column[indices])
    index.booleans <- rep(FALSE, length(column))
    index.booleans[indices] <- TRUE
    county <- rep(county.names, lengths)
    candidate <- ifelse(index.booleans, as.character(state$Candidate), as.character(state$County))
    votes <- ifelse(index.booleans, as.character(state$Votes), as.character(state$Candidate))
    won <- ifelse(index.booleans, as.character(state$`% Won`), as.character(state$Votes))
    data.frame(County=county, Candidate=candidate, Votes=votes, Won=won)
}

county.list2 <- lapply(county.list, clean.county.data)
