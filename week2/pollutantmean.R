pollutantmean <- function(directory, pollutant, id = 1:332) {
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files

        ## 'pollutant' is a character vector of length 1 indicating
        ## the name of the pollutant for which we will calculate the
        ## mean; either "sulfate" or "nitrate".

        ## 'id' is an integer vector indicating the monitor ID numbers
        ## to be used

        ## Return the mean of the pollutant across all monitors list
        ## in the 'id' vector (ignoring NA values)
	
	pollutant_sum = 0
	pollutant_counter = 0


	for (i in id)
	{

		# read data from csv
		data <-	 read.csv(paste(directory,"/", sprintf("%03d",i), ".csv", sep=""))
		# filter to obtain only data of pollutant
		data <- data[pollutant]
		# remove NA entrances
		data <- data[!is.na(data)]

		# add data and count of lines to the sum
		pollutant_sum <- pollutant_sum + sum(data)
		pollutant_counter <- pollutant_counter + length(data)
	}
	return (pollutant_sum / pollutant_counter)
}
