#this funtion takes the Model_summary[, c("product_id","r.squared","adj.r.squared","p.value")] table and feed the two kinds of percentage change in price, and return a table of regression model summary and coefficient value

Est.Qty_Demanded_t <- function(Reg_Table, percentChangePrice1, percentChangePrice2){
  #Reg_Table contains Model_summary[, c("product_id","r.squared","adj.r.squared","p.value")]
  
  
  
  #according to the input percentage change in price, created four columns 
  #two prices for comparison
  Reg_Table[, paste(percentChangePrice1,"%_est_qty",sep = "")] <- rep(NA, length(Reg_Table$sku))
  Reg_Table[, paste(percentChangePrice1,"%_PED",sep = "")] <- rep(NA, length(Reg_Table$sku))
  Reg_Table[, paste(percentChangePrice2,"%_est_qty",sep = "")] <- rep(NA, length(Reg_Table$sku))
  Reg_Table[, paste(percentChangePrice2,"%_PED",sep = "")] <- rep(NA, length(Reg_Table$sku))
  
  
  for (id in unique(Reg_Table$sku)) {
    #source
    #get the coefficient of error 
    eCoef <- Coef_Summary[Coef_Summary$sku == id,][1,3]
    #get the coefficient of X1 - percentage change in price
    x1Coef <- Coef_Summary[Coef_Summary$sku == id,][2,3]
    
    
    
    Reg_Table[Reg_Table$sku == id, paste(percentChangePrice1,"%_est_qty",sep = "")] <- eCoef + x1Coef*percentChangePrice1
    
    Reg_Table[Reg_Table$sku == id, paste(percentChangePrice1,"%_PED",sep = "")] <- (eCoef + x1Coef*percentChangePrice1)/percentChangePrice1


    Reg_Table[Reg_Table$sku == id, paste(percentChangePrice2,"%_est_qty",sep = "")] <- eCoef + x1Coef*percentChangePrice2
    
    Reg_Table[Reg_Table$sku == id, paste(percentChangePrice2,"%_PED",sep = "")] <- (eCoef + x1Coef*percentChangePrice2)/percentChangePrice2
    




  }
  
  
  return(Reg_Table)
  
  
}


