#this function ONLY takes data frame with the following structure:
#column names: 
# $ product_id : chr  "1002" 
# $ qty_ordered: num  1 
# $ price      : num  1.99 
# $ date       : Date, format: "2015-03-24" 

#NOTE*********** This function will return a data table

Diff_Identifier_Sku <- function(DataFrame){
  #install/load "data.table"
  pacman::p_load("data.table")
  
  # setDT --> Convert lists and data.frames to data.table by reference
  setDT(DataFrame)
  
  # := Assignment by reference --> Fast add, remove and modify subsets of columns, by reference.
  DataFrame[, firstdate := as.Date(date, "%m/%d/%y")]
  #this will add a new column called "firstdate", and copy all original "date" value into the new column in a right format
  
  
  #Based on how you calculate date diff, we are better off creating a range of dates for each row:
  #Original code: DataFrame[, lastdate := shift(firstdate,type = "lead"), by = product_id]
  DataFrame[, lastdate := shift(firstdate, type="lead", fill=firstdate[.N]), by = sku]
  #create a new column named "lastdate"
  #use shift() which --> This function shifts a vector input a certain number of places in the direction desired.
  #in this case, we shift dates by product, so for each product, we extract the last date when a certain price changes
  #this will give us every entry's last date when that price appear before the next price
  
  #this can be removed since we changed the above code 
  #DataFrame[is.na(lastdate), lastdate := firstdate]
  #there are some NA genered by shift()
  #because NA appears when a NEW product_id starts
  #HERE we replace those NA with the same value in first date. Because we assume, for that last price change for that product_id, that is the last date we record sales on that price
  
  
  #Then create a new ID for every change in price:
  DataFrame[, price_id := cumsum(c(0,diff(price) != 0)), by = sku]
  #first we take diff(price), which calculates the difference between all values in a vector. With diff(price) != 0, I convert this to a vector of T/F with TRUE if the price is different (i.e. the start of a new group). I then concatenate zero to the start of this, as diff gave n - 1 values in the return. This step also converts the T/Fs to 1/0. Now I have a vector with 1 at every position of a price change. Taking the cumsum of this creates an autoincrement id for every group. 
  
  #create a new column named "price_id"
  #cumsum --> Returns a vector whose elements are the cumulative sums, products, minima or maxima of the elements of the argument.
  
  
  
  #Then calculate your groupwise functions, by product and price run:
  
DataFrame[,.(
    price = unique(price),
    sum_qty = sum(qty_ordered),
    date_diff = max(lastdate)-min(firstdate)+1
  ),
  by = .(
    sku,
    price_id
  )
  ]
  
  
}
