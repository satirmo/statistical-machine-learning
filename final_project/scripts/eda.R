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
display_barplot = function(xvalues, yvalues, title, xlab, ylab){
  data.frame(x=xvalues, y=yvalues) %>%
    ggplot(aes(x=x, y=y)) +
    geom_bar(stat="identity") +
    labs(title=title, x=xlab, y=ylab)
}
