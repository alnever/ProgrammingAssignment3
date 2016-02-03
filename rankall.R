rankall <- function(outcome, num = "best") {
	## Read outcome data

      d <- read.csv("outcome-of-care-measures.csv", colClasses = "character")

	## Check that state and outcome are valid
	# For each state, find the hospital of the given rank
	# Return a data frame with the hospital names
	# (abbreviated) state name

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

	d <- d[ order(d[,2], d[,3], d[,1]), ] 
      names(d) <- c("Hospital.Name","State","Rate")
	
	if (is.character(num)){
		if (num == "best") {       
 			n <- 1
			hosp <- data.frame(tapply(d[[1]], d$State, head, n = 1))
                  state <- data.frame(tapply(d[[2]], d$State, head, n = 1))
			r <- cbind(hosp, state)
			
		}
	      else if (num == "worst") {
			n <- nrow(d)
			hosp <- data.frame(tapply(d[[1]], d$State, tail, n = 1))
                  state <- data.frame(tapply(d[[2]], d$State, tail, n = 1))
			r <- cbind(hosp, state)
		}		
		else {
			message("Error in rankhospital (\"",state,"\",\"",outcome,"\",\"",num,"\"): invalid num")
			return(NA)
		}	
	} else {
		if (!is.numeric(num)) {
			message("Error in rankhospital (\"",state,"\",\"",outcome,"\",\"",num,"\"): invalid num")
			return(NA)
		} else if (num > nrow(d)) {
			return(NA)
		} else {
			n <- num
                  l <- split(d, d[2])
			r <- data.frame(row.names =  c("Hospital.Name","State"))
			for (i in 1:length(l)) {
				p <- l[[i]][n,c(1,2)]
				names(p) <- c("Hospital.Name","State")
				p$State <- l[[i]][1,"State"]
				r <- rbind(r,p)
			}
		}

	}
      names(r) <- c("hospital","state")
	r
}