rankhospital <- function(state, outcome, num = "best") {
	## Read outcome data
	d <- read.csv("outcome-of-care-measures.csv")

	## Check that state and outcome are valid
	if (outcome == "heart attack") outcomen = 11
	else if (outcome == "heart failure") outcomen = 17
	else if (outcome == "pneumonia") outcomen = 23
	else {
		message("Error in rankhospital (\"",state,"\",\"",outcome,"\",\"",num,"\"): invalid outcome")
		return(NA)
	}

	d <- data.frame(d[,c(2,7)], as.numeric(as.vector(d[,outcomen])))
      ok <- complete.cases(d)
	d <- d[ok,]
	d <- d[d[[2]] == state,]

	if (nrow(d) == 0) {
		message("Error in rankhospital (\"",state,"\",\"",outcome,"\",\"",num,"\"): invalid state")
		return(NA)
	}


	## Return hospital name in that state with the given rank
	## 30-day death rate
	
	if (is.character(num)){
		if (num == "best") {       
			n <- 1
		}
	      else if (num == "worst") {
			n <- nrow(d)
		}		
		else {
			message("Error in rankhospital (\"",state,"\",\"",outcome,"\",\"",num,"\"): invalid num")
			return(NA)
		}	
	} else {
		if (!is.numeric(num)) {
			message("Error 1 in rankhospital (\"",state,"\",\"",outcome,"\",\"",num,"\"): invalid num")
			return(NA)
		} else if (num > nrow(d)) {
			return(NA)
		} else {
			n <- num
		}

	}
	
      names(d) <- c("Hospital.Name","State","Rate")
	d <- d[ order(d[,3], d[,1]), ] 

	as.character(d[n, "Hospital.Name"])
}
