# Method: Read the processed data set.
# file_name: The file name of processed data set.
read_processed_data_set = function(file_name){
  sales = read_csv(file_name)
  sales$brand = as.factor(sales$brand)
  sales$sneaker_name = as.factor(sales$sneaker_name)
  sales$order_weekday = as.factor(sales$order_weekday)
  sales$order_day = as.factor(sales$order_day)
  sales$order_month = as.factor(sales$order_month)
  sales$order_year = as.factor(sales$order_year)
  sales$buyer_region = as.factor(sales$buyer_region)
  
  return (sales)
}

# Method: Read the modelling data set.
# file_name: The file name of modelling data set.
read_model_data_set = function(file_name){
  sales = read_csv(file_name)
  sales$brand = as.factor(sales$brand)
  sales$sneaker_name = as.factor(sales$sneaker_name)

  return (sales)
}