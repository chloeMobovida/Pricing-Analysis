#David Liu emailed me a list of product_id which he did pricing test on
#he wants to check the PED on those products

#set directory path to where the data is stored
setwd("/Users/chloeli/Documents/01. PriceElasticity_CL")

#save david data
david <- read.csv("DS SKU pricing - BC Cost Updated with New Pricing.csv")
  
setwd("/Users/chloeli/Documents/01. PriceElasticity_CL/004.Reports")

#grab original dataset
DT_CombP_PED <- read.csv("PED_AllPriceComb.csv")


  
Heyo <- subset(DT_CombP_PED[DT_CombP_PED$product_id %in% david$product_id,])
  

write.csv(Heyo, "Heyo.csv")
