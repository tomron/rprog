rankall <- function(outcome, num = "best") {
	## Read outcome data
	## Check that state and outcome are valid
	## For each state, find the hospital of the given rank
	## Return a data frame with the hospital names and the
	## (abbreviated) state name


	data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")

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

	data <- data[c("State", "Hospital.Name", outcome_name)]
	data <- data[complete.cases(data),]

	# sort results
	data <- data[order(data[outcome_name], data["Hospital.Name"]),]


	split_data <- split(data, data["State"])
	counter = 0
	results = data.frame(Hospital.Name= NA, State= NA)[numeric(0),]
	for (s in split_data)
	{
		state <- s[[1,"State"]]
		if (num == "best") {hospital_name <- s[1,c("Hospital.Name", "State")]}
		else if (num == "worst") { hospital_name <- s[nrow(s),c("Hospital.Name", "State")]}
		else {hospital_name <- s[num,c("Hospital.Name","State")]}

		hospital_name["State"][is.na(hospital_name["State"])] <- state
		results <- rbind(results, hospital_name)
	}
	names(results) <- c("hospital", "state")
	results
}
