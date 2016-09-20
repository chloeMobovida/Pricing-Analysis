                                ################################################################################
                                #                 This script is for all PED analysis                          #
                                #                     **All product PED**                                      #
                                #                     Created by: CHLOE LI                                     #
                                #                   Date Updated: Sep 07, 2016                                 #
                                #     Note: Calculate the PED for EACH price change combination for each       #
                                #     product. **Each unique price combination - no timing consideration       #
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
                    
                    #round the price to 2 decimal places
                    dataset$price <- round(dataset$price, 2)
                    
                    #select vairables that are needed for this analysis
                    Master <- dplyr::filter(dataset, post_purchase_item != 1 & price != 0.00)  %>%
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
                    
                    
                    #change datediff to numeric for better transformation later on
                    priceGrouped$Total_sales_days <- as.numeric(priceGrouped$Total_sales_days)
                    

                    
                    
                    #--------------------------------------------READY FOR PED ALL---------------------------------------------------#
                                                        ##########  ########## ###########
                                                        #        #  #          #          #
                                                        #        #  #          #          #
                                                        ##########  #########  #          #
                                                        #           #          #          #
                                                        #           #          #          #
                                                        #           ########## ###########
                                                        
                    
                    
                    
                   #need to calculate PED for all price changes.
                    #any price changes combination
                    
                    
                    #set up an empty dataframe to store the new pairwise comparison in rows 
                    DT_CombP_PED <- data.frame(
                      product_id = character(),
                      price_id_New = character(),
                      price = double(),
                      Total_qty_demanded = double(),
                      Total_sales_days = double(),
                      stringsAsFactors=FALSE) 
                    
                    #this loop is to loop through each unique product_id, for each id, we use pairwise comparison on each price_id
                    #and rbind each of product_id's pairwise dataset
                    for (id in unique(priceGrouped$product_id)){
                      combined <- apply(priceGrouped[priceGrouped$product_id == id, ], 2, combn, m=2)
                      DT_CombP_PED <- rbind(DT_CombP_PED, combined)
                    }
                    
                    
                    #after combn(), all variables turn to factor type. Need to reformat all data type
                    DT_CombP_PED$product_id <- as.character(DT_CombP_PED$product_id)
                    DT_CombP_PED$price_id_New <- as.character(DT_CombP_PED$price_id_New)
                    
                    #some numeric values might be loose information if transform directly from factor to numeric
                    #here are some special treatment
                    DT_CombP_PED$price <- as.numeric(as.character(DT_CombP_PED$price))
                    DT_CombP_PED$Total_sales_days <- as.numeric(as.character(DT_CombP_PED$Total_sales_days))
                    DT_CombP_PED$Total_qty_demanded <- as.numeric(as.character(DT_CombP_PED$Total_qty_demanded))
                    
                    
                    
                    #calculate the # of orders per price period
                    DT_CombP_PED$AvgDemand_PerDay <- DT_CombP_PED$Total_qty_demanded/DT_CombP_PED$Total_sales_days
                    
                    #set back to data table for calcualting the lag differenes
                    DT_CombP_PED <- as.data.table(DT_CombP_PED)
                    
                    #sort dataframe by product_id and mark it as KEY
                    setkey(DT_CombP_PED,product_id)
                    
                    #calculate percentage change in price and in average qty demanded by product_id
                    #this will return each of consecutive rows differences for each product
                    DT_CombP_PED[,Percent_Change_price:=c(Delt(price, type='arithmetic')),by=product_id]
                    DT_CombP_PED[,Percent_Change_Avgqty:=c(Delt(AvgDemand_PerDay, type='arithmetic')),by=product_id]
                    
                      
                    #but we need only one difference between each of two rows
                    #that is, we need to delete the odd rows
                    
                    #set up odd indexes
                    odd_indexes <- seq(1,length(DT_CombP_PED$product_id), 2)
                    #assign NA to odd rows' percentage change in price and percentage change in Avgqty
                    DT_CombP_PED[odd_indexes, c("Percent_Change_price", "Percent_Change_Avgqty")] <- NA
                    
                    
                    
                    #calculate price elasticity of demand per price change for each product_id
                    DT_CombP_PED$PED <- DT_CombP_PED$Percent_Change_Avgqty/DT_CombP_PED$Percent_Change_price
                    
                    
                    
                    
                    
                    #calcuate revenue
                    DT_CombP_PED$Revenue <- DT_CombP_PED$price * DT_CombP_PED$Total_qty_demanded
                    #rearrange the column names
                    DT_CombP_PED <- dplyr::select(DT_CombP_PED, product_id, price_id_New, price, Total_qty_demanded, Revenue, everything())         
                    

                    
                    #---------------------------------------REGRESSION MODEL BUILDING-------------------------------------#
                    #need to build regression model per each product_id
                    #using y-dependent variable of percentage change in price, x-independent variable of percentage change in qty demanded
                    
                    RegrModel_DT <- na.omit(DT_CombP_PED)
                    
                    
                    fitted_models = RegrModel_DT %>% group_by(product_id) %>% do(model = lm(AvgDemand_PerDay ~ Percent_Change_price, data = .))
                    
                    Coef_Summary <- fitted_models %>% tidy(model) #coefficient and p value
                    Coef_Summary <- as.data.frame(Coef_Summary) #using 0.1 as significant cut off
                    #still need to round the num
                    Coef_Summary[,3:6] <-round(Coef_Summary[,3:6],3) 
                    
                    
                    
                    Model_summary <- fitted_models %>% glance(model) #with r square
                    Model_summary <- as.data.frame(Model_summary)
                    Model_summary[,2:6] <-round(Model_summary[,2:6],3) 
                    
                    #write.csv(Coef_Summary, "Coefficient_Summary.csv")
                    #write.csv(Model_summary, "Model_Summary.csv")
                    
                    
                    
                    
                    #-------------------------------HISTOGRAM OF FREQUENCY OF PRICE CHANGE----------------------------------#
                    
                    CountTimes_3 <- dplyr::select(as.data.frame(table(RegrModel_DT$product_id)), product_id = Var1, frequency = Freq)
                    # ####
                    #use histogram to show frequency of price changed after cleaning
                    #shows how many products that have # of prices changed
                    ggplot(CountTimes_3, aes(x=frequency)) +
                      geom_histogram(colour = "black", fill = "orange", bins = 30) +
                      xlab("Frequency") +
                      ylab("Count") +
                      ggtitle("Histogram of Frequency in Price Change")
                    
                    # ####
                    
                    #save as .csv
                    #write.csv(DT_CombP_PED, "PED_allProduct_PriceCombination.csv")
                    
                    
                    
                    #-------------------------------------------------END GRAPH-------------------------------------------#
                    
                    
                    
                    
                    