#clear environment
rm(list = ls())
#set working directory to where the data stores
setwd("/Users/chloeli/Documents/01. PriceElasticity_CL/001.Data")

#install or/and reuqire neccessary packages
#if (!require("pacman")) install.packages("pacman") #this line of code just need to run once 
pacman::p_load("gridExtra","ggplot2", "dplyr","lubridate","reshape2","data.table","quantmod","lme4","lattice","plyr","broom",'ReporteRs',"knitr","xtable")


#---------------------------------------------------------------------------------------#
#make sure every files in the list are having the same structure first before importing 
#IF NOT:
#run ONLY ONCE:
##source the function created to reformat the date in specific dataset
#source("/Users/chloeli/Documents/R_Reference_CL/ReformaDate_Function_CL.R")
#
#use the function
#ReformaDate("SFOI_160101_160430.csv")
#---------------------------------------------------------------------------------------#


#import data

#getting a list of files in the directory
file_list <- list.files()

#iterate through the list of files in the current working directory and put them together to form a dataframe 
for (file in file_list){
  
  # if the merged dataset doesn't exist, create it
  if (!exists("dataset")){
    dataset <- read.csv(file)
  }
  
  # if the merged dataset does exist, append to it
  if (exists("dataset")){
    temp_dataset <-read.csv(file)
    dataset<-rbind(dataset, temp_dataset)
    rm(temp_dataset)
  }
  
}

#----------------------------------------------------LOAD------------------------------------------------------#



#----------------------------------------------------PREP------------------------------------------------------#

#select vairables that are needed for this analysis
Master <- dplyr:: filter(dataset, post_purchase_item != 1 & price != 0.00) %>% 
  #remove all post_purchase_item is 1, and price of 0.00
  select(product_id,qty_ordered,	
         price, date = created_at,brand_model)


#change some variables to correct type
Master$product_id <- as.character(Master$product_id)
#dealing with the time type 
Master$date <- as.character(Master$date)
Master$date <- as.Date(gsub( " .*$", "", Master$date))
Master <- dplyr::arrange(Master, product_id, date)

#----------------------------------------------------PREP------------------------------------------------------#




#This is to generate the csv for PED analysis on products by brand.


#graph histogram of price
#y-count of price frequency
#x-price range
#more like a gamma distribution
ggplot(Master, aes(x=price)) +
  geom_histogram(colour = "black", fill = "sky blue",bins = 30) +
  xlab("Price") +
  ylab("Count") +
  ggtitle("Histogram of Price") 





#create a dataset to show each price change per products

#group the data by each product and its each price to show each product with different price if changed had been made
by_ProductID <- dplyr::group_by(Master, product_id, price) 

SumData <- dplyr::summarise(by_ProductID, 
#calculate the total quantity sold to customers at each price for each product
#Start_PriceChange_Date = min(date),
Total_Orders = sum(qty_ordered)
) 


#create a dataset to 

#use table() to calculate the frequency of price changes per product_id
#this step is to examine the feasibility of the analysis
CountTimes <- dplyr::select(as.data.frame(table(SumData$product_id)), product_id = Var1, frequency = Freq)

# ####
#use histogram to show the feasibility
#shows how many products that have # of prices changed
ggplot(CountTimes, aes(x=frequency)) +
geom_histogram(colour = "black", fill = "pink", bins = 30) +
xlab("Frequency") +
ylab("Count") +
ggtitle("Histogram of Frequency in Price Change")

# ####

#filter the dataset based on frequency in price change
#in this case, the cut off point is 2
CountTimes_Filtered <- dplyr::filter(CountTimes, CountTimes$frequency > 2)


#-----------------------------------------------------------MAIN DATASET IS READY------------------------------------------------------#

#get a dataset with all data in which products have changed 2 or more times in its price 

# subset product_id if exsit in CountTimes_Filtered which has greater than 1 times of price change
PriceChangedData <- subset(Master[Master$product_id %in% CountTimes_Filtered$product_id,])
#arrange dataset so all same product_id with multiple dates
PriceChangedData <- dplyr::arrange(PriceChangedData, product_id, date)

#-----------------------------------------------------------MAIN DATASET IS READY------------------------------------------------------#



#Count number of orders per brand/model
CountBrand <- as.data.frame(table(PriceChangedData$brand_model))
CountBrand <- dplyr::select(CountBrand, brand_model = Var1, Num_Orders = Freq)




#--------------------------------------------------DATA BY BRAND FOR SHOW IS READY-----------------------------------------------------#

#filter the brand_model with larger frequency/number of orders and remove some NULL/NA
#extract only top sellers brand/model (cutoff - 100 orders and above per brand/model)
filtered_brand <- filter(CountBrand, Num_Orders > 100 & brand_model != "\\t" & brand_model != "" & brand_model != "NULL") %>%
arrange(desc(Num_Orders))
#go back to original dataset and subset the brand/model that we need
brand_Ready <- subset(PriceChangedData[PriceChangedData$brand_model %in% filtered_brand$brand_model,])


by_BrandID <- dplyr::group_by(brand_Ready, brand_model) 
Brand_ID <- dplyr::summarise(by_BrandID, Num_Unique_ID = length(unique(product_id))) %>%
arrange(desc(Num_Unique_ID)) %>%
filter(Num_Unique_ID > 5)
Brand_ID$Num_Unique_ID <- as.numeric(Brand_ID$Num_Unique_ID)

#--------------------------------------------------DATA BY BRAND FOR SHOW IS READY-----------------------------------------------------#




#brand/model analysis is based on product_id whose price had changed more than 2 times
#-----------------------------------------------------SHOW-------------------------------------------------------------------#
#this is histogram to show number of total orders per brand/model (sorted)
#####################################################################################
#set up x & y axis label's theme
bold <- element_text(face = "bold", color = "black")
#add more by adding face = "bold.italics" etc.
#More: http://rstudio-pubs-static.s3.amazonaws.com/3364_d1a578f521174152b46b19d0c83cbe7e.html

Brand <- ggplot(brand_Ready,aes(x=reorder(brand_model,brand_model,function(x)-length(x))))
Brand + 
  geom_bar(fill = "orange") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  ggtitle("Histogram of Brand/Model") +
  xlab("Brand") +
  ylab("Number of Orders") #+
#theme(axis.text = bold)


#####################################################################################

#show table for top 20 brands/models with number of total orders per brands/models
kable(filtered_brand[1:20,])




#The below graph shows the number of unique product_id associated with each brand/model

#####################################################################################              
#show histogram of number of unique product_id per brand/model
#this is  histogram of number of unique_ID associated with each brand/model 
#(exclude the ID with only 1 price changed)                   

ggplot(Brand_ID,aes(x=reorder(brand_model,-Num_Unique_ID),y=Num_Unique_ID)) + 
  geom_bar(fill = "#0072B2", stat='identity') + 
  labs(y='Number of Unique Product_ID',x='Brand/Model') + #coord_flip() 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

##################################################################################### 

#Show table for top 20 brands/models with number of unique product_id associated with 
kable(Brand_ID[1:20,])


#-----------------------------------------------------SHOW END---------------------------------------------------------------#








#--------------------------------------------------DATA BY BRAND FOR SHOW IS READY-----------------------------------------------------#

#cleaning the brand_model --> remove some NULL/NA
brand_All_Ready <- dplyr::filter(PriceChangedData, brand_model != "\\t" & brand_model != "" & brand_model != "NULL")








#--------------------------------------------------DATA BY BRAND FOR SHOW IS READY-----------------------------------------------------#

#this section is to calculate the PED per product'price change


##########  ########## ###########
#        #  #          #          #
#        #  #          #          #
##########  #########  #          #
#           #          #          #
#           #          #          #
#           ########## ###########


# ####
#-----------------------------DataSet Grouping by Product & Brand, mark price_id--------------------------------------#


#source the R function code for Diff_Identifier() which returns a new data frame
source("/Users/chloeli/Documents/R_Functions_CL/Diff_Identifier_byBrand_Fun.R")

Diff_Summary_byBrand <- Diff_Identifier_Brand(brand_All_Ready)


#-----------------------------DataSet Grouping by Product & Brand, mark price_id--------------------------------------#




#combine data by product_id and price but by brand as well
#last step using Diff_Identifier we identify price changes in different peirod
#Now we need to group those prices together for each product and sum it with total qty and total days of diff

by_price <- dplyr::group_by(Diff_Summary_byBrand, product_id, price, brand_model)
priceGrouped <- dplyr::summarise(by_price,
                                 price_id = min(price_id), #min(price_id) allows me to identify which price occurred first
                                 Total_qty_demanded = sum(sum_qty),
                                 Total_sales_days = sum(date_diff))


#order by product_id and then by price_id to get the timeline of price occurred
priceGrouped <- dplyr::arrange(priceGrouped, product_id, brand_model, price_id)

#change the brand_model to character
priceGrouped$brand_model <- as.character(priceGrouped$brand_model)
#change total sales days to numeric 
priceGrouped$Total_sales_days <- as.numeric(priceGrouped$Total_sales_days)
#change the dataframe to data table
priceGrouped <- as.data.table(priceGrouped)




#-----------------------------NEED TO REFORMAT THE DATASET--------------------------------------#

#what do we need NOW?
#we need to make sure that for each product_id, for each kind of brand/model for that id, we have more than 1 price changed
#otherwise we need to eliminate those brand/model that only occurred 1 time per product_id - #1 step
#eliminate the brand/model per product_id that do NOT have price changed at all -#2 step

Brand_PED_Analysis <- data.frame(
  product_id = character(),
  price = double(),
  brand_model = character(),
  price_id = character(),
  Total_qty_demanded = double(),
  Total_sales_days = double(),		                         
  stringsAsFactors=FALSE) 


#grep each product_id

for (id in unique(priceGrouped$product_id)){
  temp_DF <- priceGrouped[priceGrouped$product_id == id]
  
  tempSubset <- select(as.data.frame(table(temp_DF$brand_model)), brand_model = Var1, frequency = Freq)
  tempSubset <- dplyr::filter(tempSubset, frequency > 1)
  # subset product_id if exsit in CountTimes_Filtered which has greater than 1 times of price change
  temp_DF <- subset(temp_DF[temp_DF$brand_model %in% tempSubset$brand_model,])
  
  
  #now we need to add this subset into a new dataframe (append it)
  Brand_PED_Analysis <- rbind(Brand_PED_Analysis,temp_DF)
}





#--------------------------------------------READY FOR PED---------------------------------------------------#
#arrange dataset
Brand_PED_Analysis <- dplyr::arrange(Brand_PED_Analysis, product_id, brand_model, price_id)

Brand_PED_Analysis <- as.data.table(Brand_PED_Analysis)         

#now need to calculate PED for each kind of brand under each product_ID
#in order to do so, we need to create a primary key for data.table to generate the arithmetic percentage change in some values
#in this case, we combine product_id with brand_model
Brand_PED_Analysis$Key <- paste(Brand_PED_Analysis$product_id, Brand_PED_Analysis$brand_model, sep = "")

#mark product_id+brand_model as KEY
setkey(Brand_PED_Analysis,Key)


#calculate the # of orders per price period
Brand_PED_Analysis$AvgDemand_PerDay <- Brand_PED_Analysis$Total_qty_demanded/as.numeric(Brand_PED_Analysis$Total_sales_days)


#calculate percentage change in price and in average qty demanded by product_id for each brand_model per product_id
Brand_PED_Analysis[,Change_price:=c(Delt(price, type='arithmetic')),by=Key]
Brand_PED_Analysis[,Change_Avgqty:=c(Delt(AvgDemand_PerDay, type='arithmetic')),by=Key]


#calculate price elasticity of demand per price change for each product_id
Brand_PED_Analysis$PED <- Brand_PED_Analysis$Change_Avgqty/Brand_PED_Analysis$Change_price


#clean the dataset
Brand_PED_Analysis <- dplyr::select(Brand_PED_Analysis, product_id, brand_model, price, PED, AvgDemand_PerDay, everything(), -price_id,-Key)



#save as .csv
write.csv(Brand_PED_Analysis, "PED_byBrand.csv")









