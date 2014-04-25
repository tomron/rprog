best <- function(state, outcome) {
	## Read outcome data
	## Check that state and outcome are valid
	## Return hospital name in that state with lowest 30-day death
	## rate

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


	## min val
	min_val = min(data[,outcome_name], na.rm=TRUE)
	hospital_names = data[data[[outcome_name]] == min_val,]["Hospital.Name"]
	hospital_names <- hospital_names[!is.na(hospital_names)]
	# sort and return the first hospital name
	sort(hospital_names)[[1]]
}

