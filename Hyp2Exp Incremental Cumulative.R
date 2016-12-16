##########  Data Pull  ###################
library(RODBC)
#{
#  myConn <- odbcDriverConnect('driver={SQL Server};server=AUS2-CIS-DDB02V;trusted_connection=true')
#  #input < sqlFetch(myConn, [esp_stage].[dbo].[RefracIncrementalInput]
#  data <- sqlQuery(myConn,"Select * from [esp_stage].[dbo].[RefracIncrementalInput]")
#  close(myConn)
#}






#                   with permitRank as (
#                   select DISTINCT api
#                   , bottomLatitude
#                   , bottomLongitude
#                   , lateralLength
#                   , row_number()  over (partition by api order by lateralLength desc) as ranked
#                   from esp_stage.dbo.espPermit p
#                   where lateralLength is not null 
#                   ),
#                   
#                   x as 
#                   (
#                   SELECT distinct h.api
#                   , h.hpdiEntityId
#                   , h.play
#                   , h.operator
#                   , h.reservoirAlias 
#                   , h.firstProductionDate
#                   , h.azimuth[az]
#                   , h.latitude27
#                   , h.longitude27
#                   , (h.latitude27 + p.bottomlatitude) / 2[midlat]
#                   , (h.longitude27 + p.bottomlongitude) / 2[midlong]
#                   , p.bottomlatitude[botlat]
#                   , p.bottomlongitude[botlong]
#                   , wellbore
#                   --, row_number()  over (partition by h.api order by firstProductionDate,reservoirAlias) as ranked2
#                   
#                   from esp_stage.dbo.espHeader h
#                   left join permitRank p on h.api = p.api and p.ranked = 1
#                   where h.api <>'0' 
#                   and firstProductionDate is not null
#                   and h.longitude27 is not null
#                   and h.latitude27 is not null
#                   and (p.bottomLatitude is not null or h.wellbore = 'Vertical')
#                   and (p.bottomLongitude is not null or h.wellbore = 'Vertical')
#                   
#                   ),
#                   
#                   y as 
#                   (
#                   SELECT distinct api
#                   , hpdiEntityId
#                   , play
#                   , [az]
#                   , latitude27
#                   , longitude27
#                   , [midlat]
#                   , [midlong]
#                   , [botlat]
#                   , [botlong]
#                   , wellbore
#                   ,reservoirAlias
#                   ,firstProductionDate
#                   ,case
#                   when x.latitude27 = x.botlat and x.longitude27 = x.botlong then x.latitude27 --mistake input
#                   when x.wellbore = 'VERTICAL' and x.botlat IS NOT NULL then x.botlat 
#                   when x.wellbore = 'VERTICAL' and x.botlat IS NULL then x.latitude27
#                   when x.wellbore = 'DIRECTIONAL' and x.botlat IS NOT NULL then x.botlat
#                   when x.wellbore = 'HORIZONTAL' and x.midlat IS NOT NULL then x.midlat
#                   when x.wellbore IS NULL and x.midlat IS NOT NULL then x.midlat
#                   else x.latitude27
#                   end [Y] 
#                   ,case
#                   when x.latitude27 = x.botlat and x.longitude27 = x.botlong then x.longitude27
#                   when x.wellbore = 'VERTICAL' and x.botlong IS NOT NULL then x.botlong
#                   when x.wellbore = 'VERTICAL' and x.botlong IS NULL then x.longitude27
#                   when x.wellbore = 'DIRECTIONAL' and x.botlong IS NOT NULL then x.botlong
#                   when x.wellbore = 'HORIZONTAL' and x.midlong IS NOT NULL then x.midlong
#                   when x.wellbore IS NULL and x.midlong IS NOT NULL then x.midlong
#                   else x.longitude27
#                   end [X]              
#                   from x
#                   where x.longitude27 < 0 and x.latitude27 > 0 
#                   --AND ranked2=1 --Unique api so well is not neighbor to itself. More than 99% of apis have same  or 4th digit same coordinates
#                   AND play<>'Permian Basin' --Permian Basin is currently too complex geologically to accuratly determine nn
#                   )
#                   
#                   SELECT distinct api
#                   , hpdiEntityId
#                   , play
#                   , reservoirAlias
#                   , firstProductionDate
#                   , az
#                   , X
#                   , Y
#                   , wellbore
#                   , latitude27
#                   , longitude27
#                   , botlat
#                   , botlong
#                   FROM y
#                   WHERE play = 'Bakken' AND api LIKE '33053%'--play subset
#                   ")
  count = 0

  input = data
#input <- read.csv("RefracIncrementalInput.csv",header = TRUE, stringsAsFactors =FALSE)
#input_header <- read.csv("Refrac Header Test 2.csv",header = TRUE)
#input_production <- read.csv("Refrac Production Test 2.csv", header = TRUE)

forecast <- function(input) {
  #Import Package arpsDCA package
  source("arpsDCA/R/bestfit.R")
  source("arpsDCA/R/arps.R")
  source("arpsDCA/R/curtail.R")
  source("arpsDCA/R/eur.R")
  source("arpsDCA/R/s3.R")
  
  ##Calculation of Refrac Time
  #input$firstProductionDate = format(as.Date(input$firstProductionDate,"%d/%m/%Y"),"%d/%m/%Y")
  #input$completionDate = format(as.Date(input$completionDate,"%d/%m/%Y"),"%d/%m/%Y")
  #input$productionDate = format(as.Date(input$productionDate,"%d/%m/%Y"),"%d/%m/%Y")
  
  
  ##Create refracTime
  #prodDate = strptime(input$firstProductionDate, format = "%d/%m/%Y")
  #completeDate = strptime(input$completionDate, format = "%d/%m/%Y")
  #diff_in_days = difftime(completeDate, prodDate, units = "days")
  #diff_in_weeks = difftime(completeDate, prodDate, units = "weeks")
  #diff_in_years = as.double(diff_in_days)/365
  #months_diff = as.double(substring(completeDate, 6, 7)) - as.double(substring(prodDate, 6, 7))
  #input$refracTime = floor(diff_in_years)*12 + months_diff
  
  
  ## turn a date into a 'monthnumber' relative to an origin
  #monnb <- function(d) { lt <- as.POSIXlt(as.Date(d, origin="1900-01-01")); lt$year*12 + lt$mon } 
  ## compute a month difference as a difference between two monnb's
  #mondf <- function(d1, d2) { monnb(d2) - monnb(d1) }
  ## take it for a spin
  #mondf(currentProd
  #      , Sys.Date())
  
  
  
  
  
  
  
  
  ##Create rankDate column, same method as join production header
  #firstprodDate = strptime(input$firstProductionDate, format = "%d/%m/%Y")
  #prodDate = strptime(input$productionDate, format = "%d/%m/%Y")
  #diff_in_days = difftime(prodDate, firstprodDate, units = "days")
  #diff_in_weeks = difftime(prodDate, firstprodDate, units = "weeks")
  #diff_in_years = as.double(diff_in_days)/365
  #months_diff = as.double(substring(prodDate, 6, 7)) - as.double(substring(firstprodDate, 6, 7))
  
  #input$rankDate <- months_diff
  
  
  
  
  
  
  
  #Reorder
  input = input[order(input$api),]
  input_reduced = subset(input, input$Time2Refrac > 4)
  
  #Filter out Refrac data
  refracAPI = unique(input_reduced$api)
  ForecastCumOil = numeric(length(refracAPI))
  ForecastCumGas = numeric(length(refracAPI))
  output = data.frame(api = refracAPI,ForecastCumOil, ForecastCumGas)
  
  ##FOR EACH COULD BE USED HERE?
  
  #API <- unique(input_production$api10)
  for(i in 1:length(refracAPI)) {   
    count = count+1
    print(count)
    well = subset(input_reduced, input_reduced$api == refracAPI[i])
    well$ProductionMonth <- well$ProductionMonth +1
    
    #Sort if necessary
    #input_header <- input_header[order(input_header$api10),]
    #well <- well[order(input_production$api10),]
    
    #well <- subset(well, well$api == unique(well$api)[i])
    currentTime = max(well$ProductionMonth)/12
    currentTimeSeries = well$ProductionMonth/12
    well <- well[order(well$ProductionMonth),]
    well = subset(well, well$ProductionMonth <= well$Time2Refrac[1]) 
    well$oilProduction = well$oilProduction/30.4
    well$gasProduction = well$gasProduction/30.4
    well$ProductionMonth <- as.double(well$ProductionMonth)/12
    
    

  
    ############Add section to cancelout duplicates 
    if(length(unique(well$completionDate))>1)
    {
      well= subset(well, well$completionDate==min(well$completionDate))
    }
    
    ####Check if there are two refracs (two Time2Refrac unique numbers)
    if(length(unique(well$Time2Refrac))>1) 
    {
      well = subset(well, well$Time2Refrac==max(well$Time2Refrac))
    }
    
    
    
    
    #Find best fit line
    #Should make changes eventually to iterate through and find the first month of production if q[1] happens to be zero
    #q[1] causes the DCA package to error out because the Di calculation can't divide by 0
    if((length(unique(well$oilProduction)) == 1 & min(unique(well$oilProduction)) == 0) || max(well$ProductionMonth*12)<4 || length(well$ProductionMonth)<=2)
      {
        
        output$ForecastCumOil[output$api == refracAPI[i]] = 0
        
      } else if(well$oilProduction[1] == 0){
      
      well$oilProduction[1] = 1  
      hyp2exp.fit.Oil = best.hyp2exp(well$oilProduction, well$ProductionMonth)
      CumProduction.Oil = arps.Np.hyp2exp(hyp2exp.fit.Oil$decline, currentTime)
      output$ForecastCumOil[output$api == refracAPI[i]] = CumProduction.Oil
      
    } else {
      
      hyp2exp.fit.Oil = best.hyp2exp(well$oilProduction, well$ProductionMonth)
      CumProduction.Oil = arps.Np.hyp2exp(hyp2exp.fit.Oil$decline, currentTime)
      output$ForecastCumOil[output$api == refracAPI[i]] = CumProduction.Oil
      
    }
    
    
    
    if((length(unique(well$gasProduction)) == 1 & min(unique(well$gasProduction)) == 0) || max(well$ProductionMonth*12)<4 ||length(well$ProductionMonth)<=2)
    {
      
      output$ForecastCumGas[output$api == refracAPI[i]] = 0
      
    } else if(well$gasProduction[1] == 0){
      
      well$gasProduction[1] = 1  
      hyp2exp.fit.Gas = best.hyp2exp(well$gasProduction, well$ProductionMonth)
      CumProduction.Gas = arps.Np.hyp2exp(hyp2exp.fit.Gas$decline, currentTime)
      output$ForecastCumGas[output$api == refracAPI[i]] = CumProduction.Gas
      
    } else {
      
      hyp2exp.fit.Gas = best.hyp2exp(well$gasProduction, well$ProductionMonth)
      CumProduction.Gas = arps.Np.hyp2exp(hyp2exp.fit.Gas$decline, currentTime)
      output$ForecastCumGas[output$api == refracAPI[i]] = CumProduction.Gas
      
    }  
    
    
    
    
      
    
    ##Plot fit
    #plot(well$rankDate, well$oilProduction, main="Hyperbolic Fit",col="blue", xlab="Time", ylab="Rate") 
    #lines( well$rankDate,arps.q.hyp2exp(hyp2exp.fit$decline, well$rankDate), col="red")
    #print(arps.Np.hyp2exp(hyp2exp.fit$decline, currentTime))
    
    ##Output
    ##rateTimeForecast = arps.q.hyp2exp(hyp2exp.fit$decline, currentTimeSeries)
    #CumProduction.Oil = arps.Np.hyp2exp(hyp2exp.fit.Oil$decline, currentTime)
    #CumProduction.Gas = arps.Np.hyp2exp(hyp2exp.fit.Gas$decline, currentTime)
    
    ##Merge CumProduction with correct well API
    #input_header$ForecastCumOil[input_header$api10 == refracAPI[i]] <- CumProduction.Oil
    #input_header$ForecastCumGas[input_header$api10 == refracAPI[i]] <- CumProduction.Gas
    
  }
  
  return(output)
}

arps.Np.hyp2exp <- function(decl, t) hyp2exp.Np(decl$qi, decl$Di, decl$b, decl$Df, t)

hyp2exp.Np <- function (qi, Di, b, Df, t)
{
  t.trans <- hyp2exp.transition(Di, b, Df)
  q.trans <- hyperbolic.q(qi, Di, b, t.trans)
  Np.trans <- hyperbolic.Np(qi, Di, b, t.trans)
  
  Np <- hyperbolic.Np(qi, Di, b, t)
  Np[t > t.trans] <- Np.trans +
    exponential.Np(q.trans, Df, t[t > t.trans] - t.trans)
  
  Np
};

output = forecast(input)

write.table(output, "C:/Users/gordon.tsai/Documents/Refracs/output")

#write.csv(output, "C:/Users/gordon.tsai/Documents/Refracs/output.csv") 

#data dump back into SQL ## timeSeries
#myConn <- odbcDriverConnect('driver={SQL Server};server=gis-mssql.prod.aus,60342;trusted_connection=true')
#{
#myConn <- odbcDriverConnect('driver={SQL Server};server=AUS2-CIS-DDB02V;trusted_connection=true')
# sqlQuery(myConn, "use Analytics_Dev
## drop table RefracProductionCumulatives
# CREATE TABLE RefracPredictedCumulatives(
#       [api] [int] NULL,
#       [ForecastCumOil] [decimal](38,0) NULL,
#       [ForecastCumGas] [decimal](38,0) NULL,
# ) ON [PRIMARY]
# 
# BULK INSERT espNearNeighborTime FROM '//di-mssql02.prod.aus/analytics$/nnTime.txt' WITH (FIELDTERMINATOR = ',', ROWTERMINATOR = '\n')
#              
# update esp_stage.dbo.espProduction
# set esp_stage.dbo.espProduction.nearNeighborFt = s.nearNeighborFt 
# from esp_stage.dbo.espProduction p
#        left join esp_data.dbo.espNearNeighborTime s on p.hpdiEntityId = s.hpdiEntityId and p.productionDate = s.prodMon
#              ")
 
 
# close(myConn)
#}