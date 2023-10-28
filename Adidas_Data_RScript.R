# 2020 ADIDAS US SALES EXPLORATORY DATA ANALYSIS #


#Loading Libraries
library(tidyverse)
library(skimr)
library(janitor)
library(scales)

#  DATA PROCESS and CLEANING 

#Importing Dataset
Adidas_data <- readxl::read_excel("AdidasUSDataset.xlsx")

#Overview of Dataset
print(Adidas_data)
skim_without_charts(Adidas_data)
View(Adidas_data)

#Removing Null Rows
Adidas_data1 <- Adidas_data %>% na.omit()
sum(is.na(Adidas_data1))
print(Adidas_data1)

#Converting row to column header
Adidas_data2 <- Adidas_data1 %>% row_to_names(1)
print(Adidas_data2)

#Overview of dataset after having column header
View(Adidas_data2)
str(Adidas_data2)

#Standardizing column names
Adidas_data2 <- Adidas_data2 %>%
                clean_names() 
print(Adidas_data2)

#Changing column datatypes and Creating new columns
Adidas_clean_data <- Adidas_data3 %>% 
                     mutate(retailer_id = as.numeric(retailer_id),
                            price_per_unit = as.numeric(price_per_unit),
                            units_sold = as.numeric(units_sold),
                            total_sales = as.numeric(total_sales),
                            operating_profit = as.numeric(operating_profit),
                            operating_margin = as.double(operating_margin),
                            invoice_date = excel_numeric_to_date(as.numeric(invoice_date)),
                            invoice_year = year(invoice_date),
                            invoice_month_name = month(invoice_date, label = T, abbr = F),
                            invoice_day_name = wday(invoice_date, label = T, abbr = F),
                            invoice_quarter = quarter(invoice_date))
print(Adidas_clean_data)

#Overview of cleaned data
skim_without_charts(Adidas_clean_data)
View(Adidas_clean_data)

#Saving/Exporting cleaned data
getwd()
write_csv(Adidas_clean_data,"Adidas_clean_data.csv")



#  DATA ANALYSIS and VISUALIZATION

#- 1 Display the overall KPI of the data
Primary_KPI <- Adidas_clean_data %>% 
              summarize(Total_Sales = sum(total_sales),
                        Total_profit = sum(operating_profit), 
                        Total_units_sold = sum(units_sold))
print(Primary_KPI)

#- 2 Sales by Retailers
Sales_by_retailers <- Adidas_clean_data %>% 
                      select(retailer,total_sales) %>% 
                      group_by(retailer) %>% 
                      summarize(Total_sales = sum(total_sales)) %>% 
                      arrange(- Total_sales)
print(Sales_by_retailers)
 #---visualization 
 ggplot(Sales_by_retailers,mapping = aes(x = retailer,y = Total_sales)) +
  stat_summary(fun = "sum", geom = "bar") +
   labs(title = "TOTAL SALES BY RETAILERS",x = "Retailers",y = "Total sales") +
    scale_y_continuous(labels = label_number_si(unit = "m",scale = 1e-6)) +
     theme(axis.title = element_text(size = 12, color = "brown",face = "bold"),
           axis.line = element_line(),
           plot.title = element_text(face = "bold"),
           axis.text = element_text(colour = "black", size = 10),
           panel.grid = element_blank())

ggsave("Adidas total sales by retailers.png")

#-3 Profit by Retailers
Profit_by_retailers <- Adidas_clean_data %>% 
                       select(retailer,operating_profit) %>% 
                       group_by(retailer) %>% 
                       summarize(Total_profit = sum(operating_profit)) %>% 
                       arrange(- Total_profit)
print(Profit_by_retailers)
 #---visualization
 ggplot(Profit_by_retailers,mapping = aes(x = retailer,y = Total_profit)) +
  stat_summary(fun = "sum", geom = "bar") +
   labs(title = "TOTAL PROFIT BY RETAILERS",x = "Retailers",y = "Total profit") +
    scale_y_continuous(labels = label_number_si(unit = "m",scale =  1e-6)) +
     theme(axis.title = element_text(size = 12, color = "brown",face = "bold"),
            plot.title = element_text(face = "bold"),
            axis.line = element_line(),
            panel.grid = element_blank(),
            axis.text = element_text(size = 10, colour = "black"))

ggsave("Adidas total profit by retailers.png")

#-4 Sales by Product
Product_sales <- Adidas_clean_data %>% 
                 select(product,total_sales) %>% 
                 group_by(product) %>% 
                 summarize(Product_sales = sum(total_sales)) %>% 
                 arrange(- Product_sales)
print(Product_sales)
 #---visualization
 ggplot(Product_sales,mapping = aes(x = product, y = Product_sales)) +
  stat_summary(fun = "sum", geom = "bar") +
   labs(title = "TOTAL SALES BY PRODUCTS", x = "Product", y = "Total Sales") +
    scale_y_continuous(labels = label_number_si(unit = "m",scale = 1e-6)) +
     theme(axis.title = element_text(size = 12, color = "brown",face = "bold"),
        plot.title = element_text(face = "bold"),
        axis.line = element_line(),
        panel.grid = element_blank(),
        axis.text = element_text(size = 10, colour = "black")) +
      coord_flip()

ggsave("Adidas total sales by products.png")

#-5 Sales by Sales Method
method_sales <- Adidas_clean_data %>% 
                select(sales_method,total_sales) %>% 
                group_by(sales_method) %>% 
                summarize(Total_method_sales = sum(total_sales)) %>% 
                arrange(- Total_method_sales)
print(method_sales)
 #---visualization
 pie(method_sales$Total_method_sales, paste0("$",method_sales$Total_method_sales), 
    col = rainbow(length(method_sales$sales_method)),
     main = "TOTAL SALES BY SALES METHOD")
  legend("topleft",method_sales$sales_method,
         fill = rainbow(length(method_sales$sales_method))) 

ggsave("Adidas total sales by sales method.png")

#-6 Monthly Sales Trend by Retailers
sales_trend <- Adidas_clean_data %>% 
               select(retailer, invoice_month_name,total_sales) %>% 
               group_by(retailer, invoice_month_name) %>% 
               summarize(Total_monthly_sales = sum(total_sales))
print(sales_trend)
 #---visualization
 ggplot(data = sales_trend, mapping = aes(x = invoice_month_name, y = Total_monthly_sales,fill = retailer)) +
  scale_y_continuous(labels = label_number_si(unit = "m",scale = 1e-6)) +
   stat_summary(fun = "sum", geom = "bar" ) +
    labs(title = "MONTHLY SALES BY RETAILERS", x = "Month", y ="Monthly sales") +
     theme(axis.title = element_text(color = "brown",face = "bold", size = 12),
           plot.title = element_text(face = "bold"),
           axis.line = element_line(),
           panel.grid = element_blank(),
           axis.text.x = element_text(angle = 90)) +
      facet_wrap(~retailer) +
       coord_flip()

ggsave("Adidas monthly sales by retailers.png")

#-7 Average Price per Product
product_avg_price <- Adidas_clean_data %>% 
                     select(product,price_per_unit) %>% 
                     group_by(product) %>% 
                     summarize(average_price = mean(price_per_unit)) %>% 
                     arrange(- average_price)
print(product_avg_price)
 #---visualization
 ggplot(data = product_avg_price, mapping = aes(x = product, y = average_price)) +
  stat_summary(fun = "mean", geom = "bar") +
   labs(title = "PRODUCTS AVERAGE PRICE", x = "Products", y ="Average price") +
    theme(axis.text = element_text(size =10, colour = "black"),
          axis.ticks = element_blank(),
          axis.title = element_text(face = "bold", size =12, colour = "brown"),
          plot.title = element_text(face = "bold"),
          axis.line = element_line(),
          panel.grid = element_blank()) +
     coord_flip()

ggsave("Adidas products average price.png")     

#-8 Product Sales by Region
region_product_sales <- Adidas_clean_data %>% 
                        select(region,product,total_sales) %>% 
                        group_by(region,product) %>%
                        summarize(region_sales = sum(total_sales))
print(region_product_sales)
 #---visualization
 ggplot(data = region_product_sales,mapping = aes(x = region, y = region_sales, fill = product)) +
  stat_summary(fun = "sum", geom = "bar", position = position_dodge()) +
   scale_y_continuous(labels = label_number_si(unit = "m",scale = 1e-6)) +
    labs(title = "PRODUCT SALES BY REGION", x = "Region", y ="Sales") +
     theme(axis.title = element_text(face = "bold", size =12, colour = "brown"),
           plot.title = element_text(face = "bold"),
           axis.line = element_line(),
           panel.grid = element_blank(),
           axis.text = element_text(size =10, colour = "black")) +
       coord_flip()

ggsave("Adidas product sales by region.png")  

