dubai_house_transaction <- read.csv("Transactions.csv")
dubai_house_transaction <- dubai_house_transaction[,c(1,3:5,7,9,11:19,21)]
dubai_house_transaction$instance_date <- as.Date(dubai_house_transaction$instance_date, "%d-%m-%Y")
dubai_house_transaction$month_yr <- format(dubai_house_transaction$instance_date, "%m-%Y")
avg_price_month = aggregate(dubai_house_transaction[,9], list(dubai_house_transaction$month_yr), mean)

property_type <- dubai_house_transaction %>%
  select(property_type_en) %>%
  unique() 
  
property_type <- c(property_type$property_type_en)

property_type = c("Land")
dubai_plot <- subset(dubai_house_transaction, property_type_en %in% property_type)



for(i in property_type){print(i)}
miao <- c("Land", "Building")


areas_dubai <- dubai_house_transaction %>%
  select(area_name_en) %>%
  unique()

str(dubai_house_transaction)

avg_price_month = aggregate(dubai_house_transaction[,9], list(dubai_house_transaction$month_yr, dubai_house_transaction$area_name_en), mean)

colnames(avg_price_month)
avg_price_month$month_yr <-  avg_price_month[,1]
avg_price_month$area_name_en <- avg_price_month[,2]
avg_price_month$actual_worth <- avg_price_month[,3]
avg_price_month <- subset(avg_price_month, select = c(4:6))