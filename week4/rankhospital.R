rankhospital <- function(state, outcome, num = "best") {
	## Read outcome data
	## Check that state and outcome are valid
	## Return hospital name in that state with the given rank
	## 30-day death rate

	data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")

	if (! is.element(state, data[,"State"])) {
		stop ("invalid state")
	}

	data <- subset(data, State==state)

	## building column name
	s <- strsplit(outcome, " ")[[1]]
	outcome_name <- paste(toupper(substring(s, 1,1)), substring(s, 2),
		sep="", collapse=".")

	outcome_name = paste("Hospital.30.Day.Death..Mortality..Rates.from.", outcome_name, sep="")

	if (! is.element(outcome_name, names(data))) {
		stop ("invalid outcome")
	}
	
	## transfaring values to numeric values
	data[,outcome_name] <- suppressWarnings(as.numeric(data[,outcome_name]))

	data <- data[c("Hospital.Name", outcome_name)]
	data <- data[complete.cases(data),]

	# sort results
	data <- data[order(data[outcome_name], data["Hospital.Name"]),]

	# return relevant result
	if (num == "best")
	{
		data[1,"Hospital.Name"]		
	}
	else if (num =="worst")
	{
		data[nrow(data),"Hospital.Name"]
	}
	else
	{
		data[num, "Hospital.Name"]
	}
}
