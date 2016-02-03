best <- function(state, outcome)
{
	d <- read.csv("outcome-of-care-measures.csv")
	
	if (outcome == "heart attack") outcomen = 11
	else if (outcome == "heart failure") outcomen = 17
	else if (outcome == "pneumonia") outcomen = 23
	else {
		message("Error in best(\"",state,"\",\"",outcome,"\"): invalid outcome")
		return(NA)
	}
	
      d <- data.frame(d[,c(2,7)], as.numeric(as.vector(d[,outcomen])))
      ok <- complete.cases(d)
	d <- d[ok,]
	d <- d[d[[2]] == state,]

	if (nrow(d) == 0) {
		message("Error in best(\"",state,"\",\"",outcome,"\"): invalid state")
		return(NA)
	}

      names(d) <- c("Hospital.Name","State","Rate")
	d <- d[ order(d[,3], d[,1]), ] 

	name <- as.character(d[1,"Hospital.Name"])
	name
}