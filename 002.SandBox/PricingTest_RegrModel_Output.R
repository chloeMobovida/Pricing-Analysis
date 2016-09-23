################################################################################
#                 This script is for all PED analysis                          #
#                     **All product PED**                                      #
#                     Created by: CHLOE LI                                     #
#                   Date Updated: Sep 19, 2016                                 #
################################################################################

#David changed price on Group A skus. 
#greb all Group A skus and find out the regression model for each skus. which has 3562 skus. 

#DO NOT filter the master dataset..leave all price changes in the dataset for building regression model. For all A sku product, there should be at least 
#2 price per sku.

#need to build regression model based on those. 

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
#ReformaDate("SFOI_160912_160915.csv")
#---------------------------------------------------------------------------------------#



#----------------------------------------------------LOAD------------------------------------------------------#




file_list <- list.files()


dataset <- do.call("rbind",lapply(file_list,
                                  FUN=function(files){read.csv(files)[ ,c("product_id", 
                                                                          "sku","price", "qty_ordered", "created_at", 
                                                                          "brand_model", "post_purchase_item")]}))

setwd("/Users/chloeli/Documents/01. PriceElasticity_CL/006.PricingTest")
#grab dataset that David sent to me. The pricing changed only on Group A sku.
#Note, some of sku had 10% price increase but some of them are not.

GroupASku <- read.csv("Group A SKUs.csv")
#unique # of sku and # of product_id --> 3789
GroupASku$product.id <- as.character(GroupASku$product.id)
GroupASku$sku <- as.character(GroupASku$sku)
GroupASku$category <- as.character(GroupASku$category)





#--------------------------------------------------DATA TYPE----------------------------------------------------#
#change some variables to correct type
dataset$product_id <- as.character(dataset$product_id)
dataset$sku <- as.character(dataset$sku)
#dealing with the time type 
dataset$created_at <- as.character(dataset$created_at)
dataset$created_at <- as.Date(gsub( " .*$", "", dataset$created_at))

#round the price to 2 decimal places
dataset$price <- round(dataset$price, 2)
  

#------------------------------------------------DATASET READY--------------------------------------------------#

#subset the dataset based on Group A sku. 

GroupA_DT <-  subset(dataset[dataset$sku %in% GroupASku$sku,])

#length(unique(GroupA_DT$sku))
#3735

#this is just trying to find the mismatached sku and product id

#by_sku <- dplyr::group_by(GroupA_DT, sku)
#summarise_bysku <- dplyr::summarise(by_sku, 
#                                    count_productID = length(unique(product_id)))

#FindOutMis_match <- filter(summarise_bysku, count_productID >1)


#grep the two mismatched for further investigation
#Mismatched <- filter(GroupA_DT, sku %in% FindOutMis_match$sku)
#Mismatched <- arrange(Mismatched, sku, product_id)
#Mismatched$IDSku <- paste(Mismatched$sku, " & ", Mismatched$product_id, sep = "")

#unique(Mismatched$IDSku)



#test sku not included
#subset(GroupASku, !(sku %in% GroupA_DT$sku))

GroupA_DT_Filtered <- dplyr::filter(GroupA_DT, post_purchase_item != 1 & price != 0.00) 
#length(unique(GroupA_DT_Filtered$sku))
#3733

GroupA_DT_Filtered <- dplyr::select(GroupA_DT_Filtered, product_id, sku, price, qty_ordered, date = created_at)




#------------------------------------------------DATASET READY--------------------------------------------------#
source("/Users/chloeli/Documents/01. PriceElasticity_CL/007.Functions_CL/Diff_Identifier_Fun_bySku.R")

Diff_summary_bySku <- Diff_Identifier_Sku(GroupA_DT_Filtered)

Diff_summary_bySku <- dplyr::arrange(Diff_summary_bySku, sku, price_id)


#check how many of skus are having only one price
by_sku2 <- dplyr::group_by(Diff_summary_bySku, sku)
summarise_freq <- dplyr::summarise(by_sku2,
                                   Price_freq = length(unique(price)))


#length(summarise_freq$sku[summarise_freq$Price_freq != 1])
#2631 sku have more than 1 price listed

#if we remove sku that had only 1 price listed - because they did not sell before (no sales records)

PED_Ready <- subset(Diff_summary_bySku, sku %in% summarise_freq$sku[summarise_freq$Price_freq != 1])


#combine data by product_id and price
#Now we need to group those prices together for each product and sum it with total qty and total days of diff

by_price <- dplyr::group_by(PED_Ready, sku, price)
priceGrouped <- dplyr::summarise(by_price,
                                 price_id = min(price_id), 
                                 #min(price_id) allows me to identify which price occurred first
                                 Total_qty_demanded = sum(sum_qty),
                                 Total_sales_days = sum(date_diff))


#order by product_id and then by price_id
priceGrouped <- dplyr::arrange(priceGrouped, sku, price_id)


#arrange dataset so all same product_id with multiple dates
priceGrouped <- dplyr::arrange(priceGrouped, sku)


#transform the dataset into data table for further calcualtion
priceGrouped <- as.data.table(priceGrouped)

#add new marker to price_id so I know which price occurred first
priceGrouped[, price_id_New := cumsum(c(0,diff(price) != 0)), by = sku] 
#reorganize the dataset
priceGrouped <- dplyr::select(priceGrouped, sku, price_id_New, everything()) 
#rearrange the dataset variable order
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
  sku = character(),
  price_id_New = character(),
  price = double(),
  Total_qty_demanded = double(),
  Total_sales_days = double(),
  stringsAsFactors=FALSE) 

#this loop is to loop through each unique product_id, for each id, we use pairwise comparison on each price_id
#and rbind each of product_id's pairwise dataset
for (id in unique(priceGrouped$sku)){
  combined <- apply(priceGrouped[priceGrouped$sku == id, ], 2, combn, m=2)
  DT_CombP_PED <- rbind(DT_CombP_PED, combined)
}


#after combn(), all variables turn to factor type. Need to reformat all data type
DT_CombP_PED$sku <- as.character(DT_CombP_PED$sku)
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
setkey(DT_CombP_PED,sku)

#calculate percentage change in price and in average qty demanded by product_id
#this will return each of consecutive rows differences for each product
DT_CombP_PED[,Percent_Change_price:=c(Delt(price, type='arithmetic')),by=sku]
DT_CombP_PED[,Percent_Change_Avgqty:=c(Delt(AvgDemand_PerDay, type='arithmetic')),by=sku]


#but we need only one difference between each of two rows
#that is, we need to delete the odd rows

#set up odd indexes
odd_indexes <- seq(1,length(DT_CombP_PED$sku), 2)
#assign NA to odd rows' percentage change in price and percentage change in Avgqty
DT_CombP_PED[odd_indexes, c("Percent_Change_price", "Percent_Change_Avgqty")] <- NA



#calculate price elasticity of demand per price change for each product_id
DT_CombP_PED$PED <- DT_CombP_PED$Percent_Change_Avgqty/DT_CombP_PED$Percent_Change_price


#calcuate revenue
DT_CombP_PED$Revenue <- DT_CombP_PED$price * DT_CombP_PED$Total_qty_demanded
#rearrange the column names
DT_CombP_PED <- dplyr::select(DT_CombP_PED, sku, price_id_New, price, Total_qty_demanded, Revenue, everything())   


setwd("/Users/chloeli/Documents/01. PriceElasticity_CL/005.RelevantDocuments")

#get document to greb all sku category data
Sku_Category <- read.csv("Sku_Category.csv")

#grep the first three letters of sku
DT_CombP_PED$SubSku <- substr(DT_CombP_PED$sku, 0, 3)
#reset the name of columns for later join
DT_CombP_PED <- dplyr::select(DT_CombP_PED, sku = SubSku, original_sku = sku, everything())
#use join function from plyr to do VLOOKUP function in R
#match with categories
DT_CombP_PED <- join(DT_CombP_PED, Sku_Category, by = "sku")







#---------------------------------------REGRESSION MODEL BUILDING-------------------------------------#
#need to build regression model per each product_id
#using y-dependent variable of percentage change in price, x-independent variable of percentage change in qty demanded

RegrModel_DT <- na.omit(DT_CombP_PED)


fitted_models = RegrModel_DT %>% group_by(sku) %>% do(model = lm(AvgDemand_PerDay ~ Percent_Change_price, data = .))

Coef_Summary <- fitted_models %>% tidy(model) #coefficient and p value
Coef_Summary <- as.data.frame(Coef_Summary) #using 0.1 as significant cut off
#still need to round the num
Coef_Summary[,3:6] <-round(Coef_Summary[,3:6],3) 

NaNis <- filter(Coef_Summary, !is.nan(p.value))


Model_Ready <- subset(DT_CombP_PED, sku %in% NaNis$sku)
Model_Ready <- na.omit(Model_Ready)
fitted_models = Model_Ready %>% group_by(sku) %>% do(model = lm(AvgDemand_PerDay ~ Percent_Change_price, data = .))
Coef_Summary <- fitted_models %>% tidy(model) #coefficient and p value
Coef_Summary <- as.data.frame(Coef_Summary) #using 0.1 as significant cut off
#still need to round the num
Coef_Summary[,3:6] <-round(Coef_Summary[,3:6],3) 



Model_summary <- fitted_models %>% glance(model) #with r square
Model_summary <- as.data.frame(Model_summary)
Model_summary[,2:6] <-round(Model_summary[,2:6],3) 


#save to files
#write.csv(Coef_Summary, "Coefficient_TableSummary.csv")
#write.csv(Model_summary, "Regression_TableSummary.csv")

#---------------------------------------REGRESSION MODEL Table-------------------------------------#

Reg_Table <- Model_summary[, c("sku","r.squared","adj.r.squared","p.value")]

source("/Users/chloeli/Documents/01. PriceElasticity_CL/008.Pricing_Regression_Scripts/Fun_createRegressionTable.R")


Reg_Table <- Est.Qty_Demanded_t(Reg_Table, 5,10)


#save it 
write.csv(Reg_Table, "GroupA_EstQtyDemand9192016.csv")




