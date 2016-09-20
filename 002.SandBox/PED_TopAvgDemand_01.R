                  ################################################################################
                  #                 This script is for Output Top Sellers                        #
                  #                     **All product PED**                                      #
                  #                     Created by: CHLOE LI                                     #
                  #                   Date Updated: Sep 06, 2016                                 #
                  ################################################################################
                  
                  
                  
                  #----------------------------------------------------PREP------------------------------------------------------#
                  
                  #clear environment
                  rm(list = ls())
                  #set working directory to where the data stores
                  setwd("/Users/chloeli/Documents/01. PriceElasticity_CL/001.Data")
                  
                  #install or/and reuqire neccessary packages
                  #if (!require("pacman")) install.packages("pacman") #this line of code just need to run once 
                  pacman::p_load("gridExtra","ggplot2", "dplyr","lubridate","reshape2","data.table","quantmod","lme4",
                                 "lattice","plyr","broom",'ReporteRs',"knitr","xtable")
                  
                  
                  #---------------------------------------------------------------------------------------#
                  #this function works for dataset's created_at (date) format is 7/1/15 7:25 
                  #this function will return "2016-01-10"
                  #make sure every files in the list are having the same structure first before importing 
                  #IF NOT:
                  #run ONLY ONCE:
                  ##source the function created to reformat the date in specific dataset
                  #source("/Users/chloeli/Documents/03. R_Functions_CL/ReformaDate_Function_CL.R")
                  #
                  #use the function
                  #ReformaDate("SFOI_150701_151231.csv")
                  #---------------------------------------------------------------------------------------#
                  
                  
                  
                  #----------------------------------------------------LOAD------------------------------------------------------#
                  
                  
                  #import data
                  
                  #getting a list of files in the directory
                  file_list <- list.files()
                  
                  #iterate through the list of files in the current working directory and put them together to form a dataframe 
                  for (file in file_list){
                    
                    # if the merged dataset doesn't exist, create it
                    if (!exists("dataset")){
                      dataset <- read.csv(file) 
                      #make sure every dataset imported as the same structure/column names
                      dataset <- dplyr::select(dataset, product_id, price, qty_ordered, created_at, brand_model, post_purchase_item)
                    }
                    
                    # if the merged dataset does exist, append to it
                    if (exists("dataset")){
                      temp_dataset <-read.csv(file)
                      #make sure every dataset imported as the same structure/column names
                      temp_dataset <- dplyr::select(temp_dataset, product_id, price, qty_ordered, created_at, brand_model, post_purchase_item)
                      dataset<-rbind(dataset, temp_dataset)
                      rm(temp_dataset)
                    }
                    
                  }
                  
                  
                  
                  
                  #--------------------------------------------------DATA TYPE----------------------------------------------------#
                  #change some variables to correct type
                  dataset$product_id <- as.character(dataset$product_id)
                  #dealing with the time type 
                  dataset$created_at <- as.character(dataset$created_at)
                  dataset$created_at <- as.Date(gsub( " .*$", "", dataset$created_at))
                  
                  
                  #select vairables that are needed for this analysis
                  Master <- dplyr::filter(dataset, post_purchase_item != 1 & price != 0.00) %>% 
                    #remove all post_purchase_item is 1, and price of 0.00
                    select(product_id,qty_ordered,	
                           price, date = created_at,brand_model) %>%
                    arrange(product_id, date)
                  
                  
                  #---------------------------------------------HISTOGRAM OF PRICE------------------------------------------------#
                  
                  
                  
                  #graph histogram of price
                  #y-count of price frequency
                  #x-price range
                  #more like a gamma distribution
                  ggplot(Master, aes(x=price)) +
                    geom_histogram(colour = "black", fill = "sky blue",bins = 30) +
                    xlab("Price") +
                    ylab("Count") +
                    ggtitle("Histogram of Price") 
                  
                  
                  
                  #------------------------------------------FREQUENCY OF PRICE CHANGE--------------------------------------------#
                  
                  
                  
                  #create a dataset to show each price change per products
                  
                  #group the data by each product and its each price to show each product with different price if changed had been made
                  by_ProductID <- dplyr::group_by(Master, product_id, price) 
                  
                  SumData <- dplyr::summarise(by_ProductID, 
                                              #calculate the total quantity sold to customers at each price for each product
                                              #Start_PriceChange_Date = min(date),
                                              Total_Orders = sum(qty_ordered)
                  )          
                  
                  
                  #use table() to calculate the frequency of price changes per product_id
                  #this step is to examine the feasibility of the analysis
                  CountTimes <- dplyr::select(as.data.frame(table(SumData$product_id)), product_id = Var1, frequency = Freq)
                  
                  
                  #filter the dataset based on frequency in price change
                  #in this case, the cut off point is 2
                  CountTimes_Filtered <- dplyr::filter(CountTimes, CountTimes$frequency > 2)
                  
                  
                  
                  #get a dataset with all data in which products have changed 2 or more times in its price 
                  
                  # subset product_id if exsit in CountTimes_Filtered which has greater than 1 times of price change
                  PriceChangedData <- subset(Master[Master$product_id %in% CountTimes_Filtered$product_id,])
                  #arrange dataset so all same product_id with multiple dates
                  PriceChangedData <- dplyr::arrange(PriceChangedData, product_id, date)
                  
                  
                  #-------------------------------HISTOGRAM OF FREQUENCY OF PRICE CHANGE----------------------------------#
                  
                  
                  # ####
                  #use histogram to show the feasibility
                  #shows how many products that have # of prices changed
                  ggplot(CountTimes, aes(x=frequency)) +
                    geom_histogram(colour = "black", fill = "pink", bins = 30) +
                    xlab("Frequency") +
                    ylab("Count") +
                    ggtitle("Histogram of Frequency in Price Change")
                  
                  # ####
                  
                  
                  #--------------------------------CALCULATE PED PER PRODUCT PRICE CHANGED--------------------------------#
                  
                  
                  
                  
                  ##########  ########## ###########
                  #        #  #          #          #
                  #        #  #          #          #
                  ##########  #########  #          #
                  #           #          #          #
                  #           #          #          #
                  #           ########## ###########
                  
                  
                  
                  #source the R function code for Diff_Identifier() which returns a new data frame
                  source("/Users/chloeli/Documents/03. R_Functions_CL/Diff_Identifier_Fun.R")
                  
                  #this is to identify each price changed per product 
                  Diff_Summary <- Diff_Identifier(PriceChangedData)
                  
                  
                  
                  #combine data by product_id and price
                  #Now we need to group those prices together for each product and sum it with total qty and total days of diff
                  
                  by_price <- dplyr::group_by(Diff_Summary, product_id, price)
                  priceGrouped <- dplyr::summarise(by_price,
                                                   price_id = min(price_id), #min(price_id) allows me to identify which price occurred first
                                                   Total_qty_demanded = sum(sum_qty),
                                                   Total_sales_days = sum(date_diff))
                  
                  
                  #order by product_id and then by price_id
                  priceGrouped <- dplyr::arrange(priceGrouped, product_id, price_id)
                  #filter the dataset: exclude data points whose Total_sales_days are 1
                  priceGrouped <- dplyr::filter(priceGrouped, Total_sales_days > 1) #might result in more 1 price changed entries..exclude those if possible
                  
                  
                  #---------------------------------EXCLUDE 1 PRICE CHANGED ITEM-CHECK_2--------------------------------------#
                  #use table() to calculate the frequency of price changes per product_id
                  #this step is to examine the feasibility of the analysis
                  CountTimes_2 <- dplyr::select(as.data.frame(table(priceGrouped$product_id)), product_id = Var1, frequency = Freq)
                  
                  #filter the dataset based on frequency in price change
                  #in this case, the cut off point is 2
                  CountTimes_2Filtered <- dplyr::filter(CountTimes_2, CountTimes_2$frequency > 2)
                  
                  
                  
                  # subset product_id if exsit in CountTimes_Filtered which has greater than 1 times of price change
                  priceGrouped <- subset(priceGrouped[priceGrouped$product_id %in% CountTimes_2Filtered$product_id,])
                  #arrange dataset so all same product_id with multiple dates
                  priceGrouped <- dplyr::arrange(priceGrouped, product_id)
                  
                  
                  
                  
                  #transform the dataset into data table for further calcualtion
                  priceGrouped <- as.data.table(priceGrouped)
                  
                  #add new marker to price_id so I know which price occurred first
                  priceGrouped[, price_id_New := cumsum(c(0,diff(price) != 0)), by = product_id] 
                  #reorganize the dataset
                  priceGrouped <- dplyr::select(priceGrouped, product_id, price_id_New, everything()) #rearrange the dataset variable order
                  priceGrouped <- dplyr::select(priceGrouped, -price_id)
                  
                  
                  
                  #--------------------------------------------READY FOR PED ALL---------------------------------------------------#
                  
                  #calculate the # of orders per price period
                  priceGrouped$AvgDemand_PerDay <- priceGrouped$Total_qty_demanded/as.numeric(priceGrouped$Total_sales_days)
                  
                  
                  #sort dataframe by product_id and mark it as KEY
                  setkey(priceGrouped,product_id)
                  
                  #calculate percentage change in price and in average qty demanded by product_id
                  priceGrouped[,Change_price:=c(Delt(price, type='arithmetic')),by=product_id]
                  priceGrouped[,Change_Avgqty:=c(Delt(AvgDemand_PerDay, type='arithmetic')),by=product_id]
                  
                  
                  #calculate price elasticity of demand per price change for each product_id
                  priceGrouped$PED <- priceGrouped$Change_Avgqty/priceGrouped$Change_price
                  
                  
                  #-------------------------------HISTOGRAM OF FREQUENCY OF PRICE CHANGE----------------------------------#
                  
                  CountTimes_3 <- dplyr::select(as.data.frame(table(priceGrouped$product_id)), product_id = Var1, frequency = Freq)
                  # ####
                  #use histogram to show frequency of price changed after cleaning
                  #shows how many products that have # of prices changed
                  ggplot(CountTimes_3, aes(x=frequency)) +
                    geom_histogram(colour = "black", fill = "#CC79A7", bins = 30) +
                    xlab("Frequency") +
                    ylab("Count") +
                    ggtitle("Histogram of Frequency in Price Change")
                  
                  # ####
                  
            
          
                  #------------------------------------Top Sellers' PED Summary--------------------------------------------#
                  
                  #transform back to data frame type 
                  priceGrouped <- as.data.frame(priceGrouped) 
                  
                  
                  #sort the dataset by average order per period
                  priceGrouped <- arrange(priceGrouped, -AvgDemand_PerDay)
                  
                  Top10 <- priceGrouped[1:10,]
                  
                  # subset product_id if exsit 
                  Top10_PED <- subset(priceGrouped[priceGrouped$product_id %in% Top10$product_id,])
                  Top10_PED <- arrange(Top10_PED, product_id, price_id_New)
                  
                  
                  #this chunk is to create a new dataframe to stroe data from top 10 avergae demand per day 
                  
                  #create a new empty dataframe to store data for the loop 
                  
                  NeedTransform <- data.frame(
                    product_id = character(),
                    price_id_New = character(),
                    price = double(),
                    Total_qty_demanded = double(),
                    Total_sales_days = double(),
                    AvgDemand_PerDay = double(),
                    Change_price = double(),
                    Change_Avgqty = double(),
                    PED = double(),			                         
                    stringsAsFactors=FALSE) 
                  
                  
                  #for loop - loops through each element from Top10 dataset which is highest 10 entries with highest average demand per day
                  #note, top 10 NOT means top 10 products
                  
                  n <- 1 #initialize the row index for new dataframe
                  
                  for (index in which(paste(Top10_PED$product_id, Top10_PED$price_id_New, sep = "")
                                      %in% paste(Top10$product_id, Top10$price_id_New, sep = ""))){#make sure every high average demand per day entry are included in new dataset along with the entry "before" it, so it can show the correct entry associated with PED
                    
                    NeedTransform[n:(n+1),] <- Top10_PED[(index-1):index,]
                    n <- n+2
                    
                  }
                  
                  
                  #when loop through, some duplications were created. So here we need to remove those duplicated entries
                  NeedTransform<- NeedTransform[!duplicated(NeedTransform), ]
                  
                  
                  #drop product_id which appears only once
                  Count_ID <- dplyr::select(as.data.frame(table(NeedTransform$product_id)), product_id = Var1, frequency = Freq)
                  Count_ID <- dplyr::filter(Count_ID, frequency>1)
                  
                  #only need product_id that appear mnultiple times
                  NeedTransform <- subset(NeedTransform[NeedTransform$product_id %in% Count_ID$product_id,])
                  
                  
                  NeedTransform <- as.data.table(NeedTransform)
                  NeedTransform[, price_id_2 := cumsum(c(0,diff(price) != 0)), by = product_id] #add new marker to price_id so I know which price occurred first
                  
                  #remove the old price_id_New
                  NeedTransform <- dplyr::select(NeedTransform, -price_id_New)
                  #reorganize the dataset
                  NeedTransform <- dplyr::select(NeedTransform, product_id, price_id = price_id_2, everything())
                  
                  NeedTransform[price_id == "0", c("Change_price","Change_Avgqty","PED")] <- NA
                  
                  
                  
                  #save as csv
                  #write.csv(NeedTransform, "PED_TopSellers.csv")
