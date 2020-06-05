fillNAs <- function( df )
{
	#This function receives a dataframe with NAs in the steps column and replaces them with the average number of steps by interval accross all days.
	
	NoMissingData <- na.omit(df)
	avgNumberSteps <- tapply(NoMissingData$steps, NoMissingData$interval, mean)
	tinter = 0
	for(i in 1:dim(df)[1])
	{
		tinter = tinter + 1
		if( is.na(df$steps[i]) )
		{
			df$steps[ i ] <- avgNumberSteps[ tinter ]
		}
		if( tinter == 288 )
		{
			tinter <- 0
		}		
	}

	return(df)	
}