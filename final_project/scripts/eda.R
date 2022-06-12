# Method: Count the number of occurrences of an element in a list of values.
# Parameter values: A list of values.
# Parameter x: The element whose occurrences will be counted.
count_occurrences = function(values, x){
  return (length(which(values==x)))
}

# Method: Displays the five-number summary of a list of values, as a table.
# Parameter values: A list of values.
# Parameter description: A description of the values.
display_fivenum = function(values, description){
  values = fivenum(values)
  df <- matrix(values, ncol=1, byrow=TRUE)
  colnames(df) <- c(description)
  rownames(df) <- c("Minimum", "First Quartile", "Median", "Third Quartile",
                    "Maximum")
  df <- as.table(df)
  head(df)
}

# Method: Displays a histogram corresponding to a list of values.
# Parameter values: A list of values.
# Parameter title: The title of the histogram.
# Parameter xlab: The label of the x-axis.
# Parameter ylab: The label of the y-axis.
# Parameter bins: The number of histogram bins.
display_histogram = function(values, title, xlab, ylab, bins=40){
  ggplot() +
    aes(values) +
    geom_histogram(bins=bins) +
    labs(title=title, x=xlab, y=ylab)
}

# Method: Displays a table summarizing the frequency of the elements in a given
# list, and their corresponding percentage with respect to the number of
# elements. The table is sorted in decreasing order with respect to percentage.
# Parameter values: A list of values.
# Parameter description: A description of the values.
# Parameter counts_description: A description of the counts.
# Parameter top_k: The top k elements with the highest percentage are displayed. 
display_counts_and_percentages = function(values, description,
                                          counts_description, top_k=10){
  df = table(values, dnn=list(description)) %>%
    as.data.frame(responseName=counts_description) %>%
    mutate(Percentage=round(prop.table(Sales)*100, 2)) %>%
    arrange(desc(Percentage))
  
  head(df, top_k)
}

# Method: Displays a barplot corresponding to lists of x-values and y-values.
# Parameter xvalues: A list of values plotted along the x-axis.
# Parameter yvalues: A list of values plotted along the y-axis.
# Parameter xlab: The label of the x-axis.
# Parameter ylab: The label of the y-axis.
display_barplot = function(xvalues, yvalues, title, xlab, ylab, order_vals=NA){
  if(is.na(order_vals)){
    order_vals = 1:length(xvalues)
  }
  
  data.frame(x=reorder(xvalues, -order_vals), y=yvalues) %>%
    ggplot(aes(x=x, y=y)) +
    geom_bar(stat="identity") +
    labs(title=title, x=xlab, y=ylab) +
    coord_flip()
}



# Method: Calculates a list containing the frequency that a day of month occurs
# between two dates (inclusive).
# start_date: The first date to count.
# end_date: The last date to count.
calculate_day_of_month_counts = function(start_date, end_date){
  day_of_month_counts = rep(0, 31)
  
  current_date = start_date
  
  while(current_date <= end_date){
    day_of_month = day(current_date)
    day_of_month_counts[day_of_month] = day_of_month_counts[day_of_month] + 1
    
    current_date = current_date + 1
  }
  
  return (day_of_month_counts)
}

# Method: Calculates a list containing the average volume per day of month.
# counts: A list containing the number of times that a day of month occurred.
# sales: A list containing the number of sales that occurred on a day of month.
calculate_volume_averages_day_of_month = function(counts, sales){
  result = rep(0, 31)
  
  for(day_of_month in 1:31){
    result[day_of_month] = count_occurrences(sales, day_of_month) /
      counts[day_of_month]
  }
  
  return (result)
}

# Method: Calculates a list containing the frequency that a month occurs between
# two dates (inclusive).
# start_date: The first date to count.
# end_date: The last date to count.
calculate_month_counts = function(start_date, end_date){
  month_counts = rep(0, 12)
  
  current_date = start_date
  
  while(current_date <= end_date){
    current_month = month(current_date)
    month_counts[current_month] = month_counts[current_month] + 1
    
    current_date = current_date + 1
  }
  
  return (month_counts)
}

# Method: Calculates a list containing the average monthly volume.
# counts: A list containing the number of times that a month occurred.
# sales: A list containing the number of sales that occurred per month.
calculate_volume_averages_month = function(counts, sales){
  result = rep(0, 12)
  
  for(current_month in 1:12){
    result[current_month] = count_occurrences(sales, current_month) /
      counts[current_month]
  }
  
  return (result)
}

# Method: Calculates a list containing the frequency that a weekday occurs
# between two dates (inclusive).
# start_date: The first date to count.
# end_date: The last date to count.
calculate_wday_counts = function(start_date, end_date){
  wday_counts = rep(0, 7)
  
  current_date = start_date
  
  while(current_date <= end_date){
    current_wday = wday(current_date)
    wday_counts[current_wday] = wday_counts[current_wday] + 1
    
    current_date = current_date + 1
  }
  
  return (wday_counts)
}

# Method: Calculates a list containing the average weekday volume.
# counts: A list containing the number of times that a weekday occurred.
# sales: A list containing the number of sales that occurred per weekday
calculate_volume_averages_wday = function(counts, sales){
  result = rep(0, 7)
  wday_names = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday",
               "Saturday")
  
  for(current_wday in 1:7){
    result[current_wday] = count_occurrences(sales, wday_names[current_wday]) /
      counts[current_wday]
  }
  
  return (result)
}