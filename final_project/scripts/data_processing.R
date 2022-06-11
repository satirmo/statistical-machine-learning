# Method: Removes dollar signs and commas from each element of a list, and
# converts each element to a numeric variable.
# Parameter values: A list of dollar-valued strings that can be converted to
# numeric variables.
parse_dollar_values = function(values){
  comma_string = ","
  dollar_string = "\\$"
  empty_string = ""
  
  return (as.numeric(sub(comma_string, empty_string,
                         sub(dollar_string, empty_string, values))))
}

# Method: Calculates the day of the week corresponding to each date in a list.
# Parameter values: A list of dates in %m/%d/%Y format.
calculate_weekday = function(values){
  return (weekdays(parse_date(values, "%m/%d/%Y")))
}

# Method: Calculates the day of the month corresponding to each date in a list.
# Parameter values: A list of dates in %m/%d/%Y format.
calculate_day = function(values){
  return (day(parse_date(values, "%m/%d/%Y")))
}

# Method: Calculates the month corresponding to each date in a list.
# Parameter values: A list of dates in %m/%d/%Y format.
calculate_month = function(values){
  return (month(parse_date(values, "%m/%d/%Y")))
}

# Method: Calculates the year corresponding to each date in a list.
# Parameter values: A list of dates in %m/%d/%Y format.
calculate_year = function(values){
  return (year(parse_date(values, "%m/%d/%Y")))
}

# Method: Calculates the number of days that have passed since January 1, 1970
# for each date in a list.
# Parameter values: A list of dates in %m/%d/%Y format.
calculate_days_elapsed = function(values){
  return (as.numeric(parse_date(values, "%m/%d/%Y")))
}

# Method: Removes leading white-space characters from a list of strings.
# Parameter values: A list of strings.
wstrip = function(values){
  whitespace_string = " "
  empty_string = ""
  
  return (sub(whitespace_string, empty_string, values))
}
