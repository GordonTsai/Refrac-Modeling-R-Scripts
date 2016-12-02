join_production_header <- function(input_production, input_header) {
##### If no data
ifNone<-data.frame(currentOperator='No Wells!',productionDate='1900-01-01',county='--',geologyZone='--',refracFlag='',api10='--',gasProduction='0',oilProduction='0',waterProduction='0',reservoirAlias='--',state='--',play='--',field='--',wellbore='--',reworkStatus='--',firstProductionDate='1900-01-01')
colnames(ifNone)<-c('currentOperator',"productionDate", 'county', 'geologyZone','refracFlag','api10','gasProduction','oilProduction','waterProduction','reservoirAlias','state','play','field','wellbore','reworkStatus','firstProductionDate') 
if (nrow(input_header)==0) {
  output <- merge(x = input_production, y = input_header, by.x='api10', by.y='api10', all.y = TRUE)
  outputMessage <- 'No wells marked on Initial Completion Analysis'
  return(output)
}

uniqueAPI_Header <- data.frame(unique(input_header$api10))
if (nrow(uniqueAPI_Header)>=1000) {
  outputMessage <- 'Too many wells (1000) marked on Initial Completion Analysis'
  output <- merge(x = input_production, y = input_header, by.x='api10', by.y='api10', all.y = TRUE)	
  return(output)
}

#Subset production data to api10 values found in the marked header table
selectedRows <- (input_production$api10 %in% input_header$api10)
input_production_reduced <- input_production[selectedRows,]

#create date rank column
input_production_reduced$rankDate <- ave(as.numeric(input_production_reduced$productionDate), input_production_reduced$api10, FUN=rank)

#Expand table for missing production dates (for cumulative production curve)
##uniqueAPI <- unique(input_production_reduced$api10)
##uniqueDate <- unique(input_production_reduced$productionDate)
##date_id <- merge(uniqueAPI,uniqueDate, all=TRUE)
##date_id_frame <- data.frame(date_id)
##names(date_id_frame) <- c("api10","productionDate")
##input_production_expanded <- merge(date_id_frame,input_production_reduced,by=c("api10","productionDate"),all=TRUE)



#Replace null production with zeros
##input_production_expanded$oilProduction[is.na(input_production_expanded$oilProduction)] <- 0
##input_production_expanded$gasProduction[is.na(input_production_expanded$gasProduction)] <- 0
##input_production_expanded$waterProduction[is.na(input_production_expanded$waterProduction)] <- 0
input_production_reduced$oilProduction[is.na(input_production_reduced$oilProduction)] <- 0
input_production_reduced$gasProduction[is.na(input_production_reduced$gasProduction)] <- 0
input_production_reduced$waterProduction[is.na(input_production_reduced$waterProduction)] <- 0

#Create cumulative production columns
##input_production_ordered <- input_production_expanded[with(input_production_expanded, order(input_production_expanded$api10,input_production_expanded$productionDate)), ]
##input_production_ordered$oilCumulative <- unlist(tapply(input_production_ordered$oilProduction,input_production_ordered$api10,FUN=cumsum),use.names=TRUE) 
##input_production_ordered$gasCumulative <- unlist(tapply(input_production_ordered$gasProduction,input_production_ordered$api10,FUN=cumsum),use.names=TRUE) 
##input_production_ordered$waterCumulative <- unlist(tapply(input_production_ordered$waterProduction,input_production_ordered$api10,FUN=cumsum),use.names=TRUE) 

#joins header columns to the production table
##output_join <- merge(x = input_production_ordered, y = input_header, by.x='api10', by.y='api10', all.y = TRUE)
output_join <- merge(x = input_production_reduced, y = input_header, by.x='api10', by.y='api10', all.y = TRUE)

#Subset to row where the porduction date is after the first production date
outputMessage <- ''
output <- output_join[ which(output_join$productionDate>=output_join$firstProductionDate), ]
#output <- output_join[ which(output_join$productionDate<=output_join$lastProductionDate), ]
}