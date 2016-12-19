##########  Data Pull  ###################
library(RODBC)
#{
#  myConn <- odbcDriverConnect('driver={SQL Server};server=AUS2-CIS-DDB02V;trusted_connection=true')
#  #input < sqlFetch(myConn, [esp_stage].[dbo].[RefracIncrementalInput]
#  data <- sqlQuery(myConn,"Select * from [esp_stage].[dbo].[RefracIncrementalInput]")
#  close(myConn)
#}

  count = 0
  input = data
  
forecast <- function(input) {
  #Import Package arpsDCA package
  source("arpsDCA/R/bestfit.R")
  source("arpsDCA/R/arps.R")
  source("arpsDCA/R/curtail.R")
  source("arpsDCA/R/eur.R")
  source("arpsDCA/R/s3.R")
  
  #Reorder
  input = input[order(input$api),]
  input_reduced = subset(input, input$Time2Refrac > 4)
  
  api = 4903728829

  #Filter out Refrac data
  refracAPI = unique(input_reduced$api)
  i = match(api,refracAPI)
  ForecastCumOil = numeric(length(refracAPI))
  #ForecastCumOil_ThreeMonth = numeric(length(refracAPI))
  #ForecastCumOil_SixMonth = numeric(length(refracAPI))
  ForecastCumGas = numeric(length(refracAPI))
  #ForecastCumGas_ThreeMonth = numeric(length(refracAPI))
  #ForecastCumGas_SixMonth = numeric(length(refracAPI))
  
  output = data.frame(api = refracAPI,ForecastCumOil, ForecastCumGas)
  
  #output = data.frame(api = refracAPI,ForecastCumOil, ForecastCumGas,ForecastCumOil_ThreeMonth,ForecastCumGas_ThreeMonth,ForecastCumOil_SixMonth,ForecastCumGas_SixMonth)
  


   #i = 150 #4 is a good one to test
#  for(i in 1:length(refracAPI)) {   
    count = count+1
    print(count)
    well = subset(input_reduced, input_reduced$api == refracAPI[i])
    well$ProductionMonth <- well$ProductionMonth + 1
  
    currentTime = max(well$ProductionMonth)
    well <- well[order(well$ProductionMonth),]
    originalTime_series = well$ProductionMonth
    originalProd_series_Oil = well$oilProduction
    originalProd_series_Gas = well$gasProduction
    
    
    well = subset(well, well$ProductionMonth <= well$Time2Refrac[1]) 
    well$oilProduction = well$oilProduction
    well$gasProduction = well$gasProduction
    well$ProductionMonth <- as.double(well$ProductionMonth)
  
    ############Check if multiple completion dates, then picks earlier one
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
    if((length(unique(well$oilProduction)) == 1 & min(unique(well$oilProduction)) == 0)){
      output$ForecastCumOil[output$api == refracAPI[i]] = 1111111111
    } else if(length(well$ProductionMonth)<=2)
      {
        output$ForecastCumOil[output$api == refracAPI[i]] = 2222222222
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
    
  #  if(well$Time2Refrac[1]+6 <= currentTime) {
      
   # }
    
    if((length(unique(well$gasProduction)) == 1 & min(unique(well$gasProduction)) == 0)){
      output$ForecastCumGas[output$api == refracAPI[i]] = 1111111111
    } else if(length(well$ProductionMonth)<=2){
      output$ForecastCumGas[output$api == refracAPI[i]] = 2222222222
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
    ##Plotting Oil Two cases zoomed and not zoomed
    par(mfrow=c(2,2))
    plot(well$ProductionMonth, well$oilProduction, main="Oil Fit (Zoom)",col="green", xlab="Time", ylab="Rate") 
    lines(well$ProductionMonth,arps.q.hyp2exp(hyp2exp.fit.Oil$decline, well$ProductionMonth), col="blue")
    plot(originalTime_series, originalProd_series_Oil, main="Oil Fit",col="green", xlab="Time", ylab="Rate") 
    lines(well$ProductionMonth,arps.q.hyp2exp(hyp2exp.fit.Oil$decline, well$ProductionMonth), col="blue")
    print(well$Time2Refrac[1])
    
    ##Plotting Gas Two cases zoomed and not zoomed
    plot(well$ProductionMonth, well$gasProduction, main="Gas Fit (Zoom)",col="red", xlab="Time", ylab="Rate") 
    lines(well$ProductionMonth,arps.q.hyp2exp(hyp2exp.fit.Gas$decline, well$ProductionMonth), col="blue")
    plot(originalTime_series, originalProd_series_Gas, main="Gas Fit",col="red", xlab="Time", ylab="Rate") 
    lines(well$ProductionMonth,arps.q.hyp2exp(hyp2exp.fit.Gas$decline, well$ProductionMonth), col="blue")
    
    
    ##Output
    ##rateTimeForecast = arps.q.hyp2exp(hyp2exp.fit$decline, currentTimeSeries)
    #CumProduction.Oil = arps.Np.hyp2exp(hyp2exp.fit.Oil$decline, currentTime)
    #CumProduction.Gas = arps.Np.hyp2exp(hyp2exp.fit.Gas$decline, currentTime)
  
#  }
  return(output)
}

##########Helper Functions
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

#write.table(output, "C:/Users/gordon.tsai/Documents/Refracs/output")

#RefracPredictedCumulatives = write.csv(output, "C:/Users/gordon.tsai/Documents/Refracs/output.csv") 

#data dump back into SQL ## timeSeries
#myConn <- odbcDriverConnect('driver={SQL Server};server=gis-mssql.prod.aus,60342;trusted_connection=true')

# myConn <- odbcDriverConnect('driver={SQL Server};server=AUS2-CIS-DDB02V;trusted_connection=true')
#sqlQuery(myConn, "use Analytics_Dev
## drop table RefracPredictedCumulatives
# CREATE TABLE RefracPredictedCumulatives(
#       [api] [int] NULL,
#       [ForecastCumOil] [decimal](38,0) NULL,
#       [ForecastCumGas] [decimal](38,0) NULL,
# ) ON [PRIMARY]
# 
# BULK INSERT RefracPredictedCumulatives FROM '//di-mssql02.prod.aus/analytics$/nnTime.txt' WITH (FIELDTERMINATOR = ',', ROWTERMINATOR = '\n')
#              
##              ")
# close(myConn)


  #data dump back into SQL ## timeSeries
  # myConn <- odbcDriverConnect('driver={SQL Server};server=gis-mssql.prod.aus,60342;trusted_connection=true')
  # sqlQuery(myConn, "use esp_data
  # drop table espNearNeighborTimef
  # CREATE TABLE espNearNeighborTime(
  #       [hpdiEntityId] [int] NULL,
  #       [nearNeighborFt] [decimal](38,0) NULL,
  #       [prodMon] [date] NULL,
  # ) ON [PRIMARY]
  # 
  # BULK INSERT espNearNeighborTime FROM '//di-mssql02.prod.aus/analytics$/nnTime.txt' WITH (FIELDTERMINATOR = ',', ROWTERMINATOR = '\n')
  #              
  # update esp_stage.dbo.espProduction
  # set esp_stage.dbo.espProduction.nearNeighborFt = s.nearNeighborFt 
  # from esp_stage.dbo.espProduction p
  #        left join esp_data.dbo.espNearNeighborTime s on p.hpdiEntityId = s.hpdiEntityId and p.productionDate = s.prodMon
  #              ")
  # 
  # 
  # close(myConn)
  
  