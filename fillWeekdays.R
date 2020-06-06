fillWeekdays <- function(df)
{
	# This function returns a value of weekday or weekend for each row containing a date.
	# The results are stored in a new variable in the same dataframe.

	for(i in 1:dim(df)[1])
	{
		if( weekdays(df$date) == "Saturday" || weekdays(df$date) == "Sunday")
		{
		 	
		}
	}
}