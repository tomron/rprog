complete <- function(directory, id = 1:332) {
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files

        ## 'id' is an integer vector indicating the monitor ID numbers
        ## to be used
        
        ## Return a data frame of the form:
        ## id nobs
        ## 1  117
        ## 2  1041
        ## ...
        ## where 'id' is the monitor ID number and 'nobs' is the
        ## number of complete cases

	complete_cases <- rep(0, length(id))
	counter <- 1
	for (i in id)
	{
		# read data from csv
		data = read.csv(paste(directory,"/", sprintf("%03d",i), ".csv", sep=""))
		
		# get number of complete cases and add it to list
		complete_cases[counter] <- sum(complete.cases(data))
		
		# increase counter
		counter <- counter +1
	}
	df <-data.frame(id,complete_cases)
	names(df) <- c("id", "nobs")
	return (df)
}
