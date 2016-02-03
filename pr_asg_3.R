best <- function(state, outcome)
{
	d <- read.csv("outcome-of-care-measures.csv")
	
	if (outcome == "heart attack") outcomen = 11
	else if (outcome == "heart failure") outcomen = 17
	else if (outcome == "pneumonia") outcomen = 23
	else {
		message ("Error in best(\"",state,"\",\"",outcome,"\"): invalid outcome")
		return(NULL)
	}
	
      suppressMessages(best)	
	d <- data.frame(d[,c(2,7)], as.numeric(as.vector(d[,outcomen])))
      ok <- complete.cases(d)
	d <- d[ok,]
	d <- d[d[[2]] == state,]

	if (nrow(d) == 0) {
		message ("Error in best(\"",state,"\",\"",outcome,"\"): invalid state")
		return(NULL)
	}

	m <- min(d[[3]],na.rm = TRUE)
	name <- as.character(d[d[[3]] == m,][1,1])
	name
}