product_id <- c("1000","1000","1000","1000","1000","1000", "1002","1002","1002","1002","1002","1002")
qty_ordered <- c(1,2,1,1,1,1,1,2,1,2,1,1)
price <- c(2.49,2.49,2.49,1.743,2.49,2.49,  2.093,2.093,2.11,2.11,2.11, 2.97)
date <- c("2/23/15","2/23/15",  '3/16/15','3/16/15','5/16/15',  "6/18/15",  "2/19/15","3/19/15","3/19/15","3/19/15","3/19/15","4/19/15")
sampleData <- data.frame(product_id,    qty_ordered,    price,  date)


#source the R function code for Diff_Identifier() which returns a new data frame
source("/Users/chloeli/Documents/R_Reference_CL/Diff_Identifier_Fun.R")

sampleData_Summary <- Diff_Identifier(sampleData)

#calculate the # of orders per price period
sampleData_Summary$AvgOrder_PerPeiod <- sampleData_Summary$sum_qty/as.numeric(sampleData_Summary$date_diff)




setkey(sampleData_Summary,product_id)


sampleData_Summary[,Change_price:=c(Delt(price, type='arithmetic')),by=product_id]
sampleData_Summary[,Change_Avgqty:=c(Delt(AvgOrder_PerPeiod, type='arithmetic')),by=product_id]




sampleData_Summary <- as.data.frame(sampleData_Summary) 

#calculate price elasticity of demand per price change for each product_id
sampleData_Summary$PED <- sampleData_Summary$Change_Avgqty/sampleData_Summary$Change_price



library(lme4)


#xyplot(response ~ year, groups=state, data=d, type='l')

#or
fits <- lmList(AvgOrder_PerPeiod ~ Change_Avgqty | product_id, data=sampleData_Summary)
fits

#or

fitted_models = sampleData_Summary %>% group_by(product_id) %>% do(model = lm(AvgOrder_PerPeiod ~ price, data = .))
fitted_models %>% glance(model)
