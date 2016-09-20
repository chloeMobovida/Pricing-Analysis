PCD <- PriceChangedData
PCD$price_id <- as.character(PCD$price_id)
PCD$pk <- paste(PCD$product_id, PCD$price_id, sep = "")

PCD <- as.data.frame(PCD)

#special treatment "24617" and "246" are not unique
#add a character at the end of the pk to make it unique
PCD[PCD$product_id == "246", "pk"] <- paste("a", PCD[PCD$product_id == "246", "pk"], sep = "")

PCD[PCD$product_id == "247", "pk"] <- paste("b", PCD[PCD$product_id == "247", "pk"], sep = "")

PCD[PCD$product_id == "251", "pk"] <- paste("b", PCD[PCD$product_id == "251", "pk"], sep = "")






WHATEVER <- data.frame(
  product_id = character(),
  price_id = character(),
  price = double(),
  first_Date = as.Date(character()),
  last_date = as.Date(character()),
  stringsAsFactors=FALSE) 


i <- 1

  for ( k in unique(PCD$pk)){
    
    temp_dt <- PCD[PCD$pk==k,]
    
    WHATEVER[i,"product_id"] <- unique(temp_dt$product_id)
    WHATEVER[i,"price_id"] <- unique(temp_dt$price_id)
    WHATEVER[i,"price"] <- unique(temp_dt$price)
    WHATEVER[i,"first_Date"] <- min(as.Date(temp_dt$date))
    WHATEVER[i,"last_date"] <- max(as.Date(temp_dt$date))
    
    i <- i+1

  
}




a <- 0

for (i in 1:length(WHATEVER$product_id)){
  Times <- length(which( WHATEVER$first_Date[i+1] == WHATEVER$last_date[i] ))
  
  a <- a + Times
  
  
}

#a 
#[1] 17872
#a/length(WHATEVER$product_id)
#[1] 0.661387
