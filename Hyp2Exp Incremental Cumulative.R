##########  Data Pull  ###################
library(RODBC)
library(aRpsDCA)
#{
#  myConn <- odbcDriverConnect('driver={SQL Server};server=AUS2-CIS-DDB02V;trusted_connection=true')
#  #input < sqlFetch(myConn, [esp_stage].[dbo].[RefracIncrementalInput]
#  data <- sqlQuery(myConn,"Select * from [esp_stage].[dbo].[RefracIncrementalInput]")
#  close(myConn)
#}
input = data

#######################Contact Gordon Tsai for any questions#######################
#Outline of Error codes
#1111111111: The first entry for the rate vector is zero. This means that the stream has a reported production of 0 for the q[1] in the rate vector.
  #This causes an error because the first Di guess is calculated by using (q[2]-q[1])/(q[1]). q[1]=0 causes a divide by zero error.
#2222222222: When there are less than two data point before Time2Refrac. Cannot confidently fit a curve to a dataset with just two points.
#3333333333: When there is less than 3 months of production after Refrac. Or 

forecast <- function(input) {
  #Reorder and filter out all Time2Refracs that happened less than 4 months from start
  input = input[order(input$api),]
  input_reduced = subset(input, input$Time2Refrac > 4)
  
  #Filter out Refrac data
  refracAPI = unique(input_reduced$api)
  #Create columns for the output vector
  ForecastCumOil = numeric(length(refracAPI))
  ForecastCumOil_ThreeMonth = numeric(length(refracAPI))
  ForecastCumOil_SixMonth = numeric(length(refracAPI))
  ForecastCumGas = numeric(length(refracAPI))
  ForecastCumGas_ThreeMonth = numeric(length(refracAPI))
  ForecastCumGas_SixMonth = numeric(length(refracAPI))
  output = data.frame(api = refracAPI,ForecastCumOil, ForecastCumGas,ForecastCumOil_ThreeMonth,ForecastCumGas_ThreeMonth,ForecastCumOil_SixMonth,ForecastCumGas_SixMonth)
  
  #api  = 0504514571
  #i = match(api,refracAPI)
  
  
  
 for(i in 1:length(refracAPI)) {   
    well = subset(input_reduced, input_reduced$api == refracAPI[i])
    well$ProductionMonth <- well$ProductionMonth + 1
    
    currentTime = max(well$ProductionMonth)
    well <- well[order(well$ProductionMonth),]
    originalTime_series = well$ProductionMonth
    originalProd_series_Oil = well$oilProduction
    originalProd_series_Gas = well$gasProduction
    
    well = subset(well, well$ProductionMonth <= well$Time2Refrac[1]) 
    well$ProductionMonth <- as.double(well$ProductionMonth)
    
    ############Check if multiple completion dates, then picks earlier one
    if(length(unique(well$RefraccompletionDate))>1)
    {
      well= subset(well, well$RefraccompletionDate==min(well$RefraccompletionDate))
    }
    
    ####Check if there are two refracs (two Time2Refrac unique numbers)
    #if there are two then take the minimum Tim2Refrac
    if(length(unique(well$Time2Refrac))>1) 
    {
      well = subset(well, well$Time2Refrac==min(well$Time2Refrac))
    }
    
    ####Saving Time2Refrac inside a variable instead of a vector
    Time2Refrac = well$Time2Refrac[1]
    PreRefracCumOil = well$PreRefracCumOil[1]
    PreRefracCumGas = well$PreRefracCumGas[1]
    
    #Find best fit line and compute decline curve parameters
    #The model used was a hyperbolic two exponential decline curve from the aRpsDCA package written by Derrick Turk
    #Should make changes eventually to iterate through and find the first month of production if q[1] happens to be zero
    ##This if statement checks for the first type of error 1111111111. This error happens when the firat q[1] in the production vector
    #is equal to zero. This errors out because q[1] causes the DCA package to error out because the Di calculation can't divide by 0
    if((length(unique(well$oilProduction)) == 1 & min(unique(well$oilProduction)) == 0)){
      output$ForecastCumOil[output$api == refracAPI[i]] = 1111111111
    
    ####Second if statement checks for type 2 error. If there are only two points of production before Refrac time
      #Then function errors out because a decline curve cannot be accurately drawn between just two points.
      ##Sometimes this erorr happens even when the Time2Refrac is a large value. This occurs because of reporting errors which
      #limits the number of production months we have data for to less than 2.
    } else if(length(well$ProductionMonth)<=2)
    {
      output$ForecastCumOil[output$api == refracAPI[i]] = 2222222222
    } else if(well$oilProduction[1] == 0){
      well$oilProduction[1] = 1  
      hyp2exp.fit.Oil = best.hyp2exp(well$oilProduction, well$ProductionMonth)
      CumProduction.Oil = (arps.Np.hyp2exp(hyp2exp.fit.Oil$decline, currentTime) 
          - arps.Np.hyp2exp(hyp2exp.fit.Oil$decline, Time2Refrac)) + PreRefracCumOil
      output$ForecastCumOil[output$api == refracAPI[i]] = CumProduction.Oil
      if(Time2Refrac+6 <= currentTime) {
        ###6 month cum calc
        CumProduction.Oil.6Month = (arps.Np.hyp2exp(hyp2exp.fit.Oil$decline, Time2Refrac+6) 
          - arps.Np.hyp2exp(hyp2exp.fit.Oil$decline, Time2Refrac)) + PreRefracCumOil
        output$ForecastCumOil_SixMonth[output$api == refracAPI[i]] = CumProduction.Oil.6Month
        CumProduction.Oil.3Month = (arps.Np.hyp2exp(hyp2exp.fit.Oil$decline, Time2Refrac+3) 
          - arps.Np.hyp2exp(hyp2exp.fit.Oil$decline, Time2Refrac)) + PreRefracCumOil
        output$ForecastCumOil_ThreeMonth[output$api == refracAPI[i]] = CumProduction.Oil.3Month
      } else if(Time2Refrac+3 <= currentTime){
        ##3 month cum calc
        CumProduction.Oil.6Month = 3333333333
        output$ForecastCumOil_SixMonth[output$api == refracAPI[i]] = CumProduction.Oil.6Month
        CumProduction.Oil.3Month = (arps.Np.hyp2exp(hyp2exp.fit.Oil$decline, Time2Refrac+3) 
          - arps.Np.hyp2exp(hyp2exp.fit.Oil$decline, Time2Refrac)) + PreRefracCumOil
        output$ForecastCumOil_ThreeMonth[output$api == refracAPI[i]] = CumProduction.Oil.3Month
      } else {
        CumProduction.Oil.6Month = 3333333333
        output$ForecastCumOil_SixMonth[output$api == refracAPI[i]] = CumProduction.Oil.6Month
        CumProduction.Oil.3Month = 3333333333
        output$ForecastCumOil_ThreeMonth[output$api == refracAPI[i]] = CumProduction.Oil.3Month
      }
    } else {
      hyp2exp.fit.Oil = best.hyp2exp(well$oilProduction, well$ProductionMonth)
      CumProduction.Oil = (arps.Np.hyp2exp(hyp2exp.fit.Oil$decline, currentTime) 
          - arps.Np.hyp2exp(hyp2exp.fit.Oil$decline, Time2Refrac)) + PreRefracCumOil
      output$ForecastCumOil[output$api == refracAPI[i]] = CumProduction.Oil
      if(Time2Refrac+6 <= currentTime) {
        ###6 month cum calc
        CumProduction.Oil.6Month = (arps.Np.hyp2exp(hyp2exp.fit.Oil$decline, Time2Refrac+6) 
          - arps.Np.hyp2exp(hyp2exp.fit.Oil$decline, Time2Refrac)) + PreRefracCumOil
        output$ForecastCumOil_SixMonth[output$api == refracAPI[i]] = CumProduction.Oil.6Month
        CumProduction.Oil.3Month = (arps.Np.hyp2exp(hyp2exp.fit.Oil$decline, Time2Refrac+3) 
          - arps.Np.hyp2exp(hyp2exp.fit.Oil$decline, Time2Refrac)) + PreRefracCumOil
        output$ForecastCumOil_ThreeMonth[output$api == refracAPI[i]] = CumProduction.Oil.3Month
      } else if(Time2Refrac+3 <= currentTime){
        ##3 month cum calc
        CumProduction.Oil.6Month = 3333333333
        output$ForecastCumOil_SixMonth[output$api == refracAPI[i]] = CumProduction.Oil.6Month
        CumProduction.Oil.3Month = (arps.Np.hyp2exp(hyp2exp.fit.Oil$decline, Time2Refrac+3) 
          - arps.Np.hyp2exp(hyp2exp.fit.Oil$decline, Time2Refrac)) + PreRefracCumOil
        output$ForecastCumOil_ThreeMonth[output$api == refracAPI[i]] = CumProduction.Oil.3Month
      } else {
        CumProduction.Oil.6Month = 3333333333
        output$ForecastCumOil_SixMonth[output$api == refracAPI[i]] = CumProduction.Oil.6Month
        CumProduction.Oil.3Month = 3333333333
        output$ForecastCumOil_ThreeMonth[output$api == refracAPI[i]] = CumProduction.Oil.3Month
      }
    } 
    
    ###Calculations for Gas portions
    if((length(unique(well$gasProduction)) == 1 & min(unique(well$gasProduction)) == 0)){
      output$ForecastCumGas[output$api == refracAPI[i]] = 1111111111
    } else if(length(well$ProductionMonth)<=2){
      output$ForecastCumGas[output$api == refracAPI[i]] = 2222222222
    } else if(well$gasProduction[1] == 0){
      well$gasProduction[1] = 1  
      hyp2exp.fit.Gas = best.hyp2exp(well$gasProduction, well$ProductionMonth)
      CumProduction.Gas = (arps.Np.hyp2exp(hyp2exp.fit.Gas$decline, currentTime)
          - arps.Np.hyp2exp(hyp2exp.fit.Gas$decline, Time2Refrac)) + PreRefracCumGas
      output$ForecastCumGas[output$api == refracAPI[i]] = CumProduction.Gas
      if(Time2Refrac+6 <= currentTime) {
        ###6 month cum calc
        CumProduction.Gas.6Month = (arps.Np.hyp2exp(hyp2exp.fit.Gas$decline, Time2Refrac+6) 
          - arps.Np.hyp2exp(hyp2exp.fit.Gas$decline, Time2Refrac)) + PreRefracCumGas
        output$ForecastCumGas_SixMonth[output$api == refracAPI[i]] = CumProduction.Gas.6Month
        CumProduction.Gas.3Month = (arps.Np.hyp2exp(hyp2exp.fit.Gas$decline, Time2Refrac+3) 
          - arps.Np.hyp2exp(hyp2exp.fit.Gas$decline, Time2Refrac)) + PreRefracCumGas
        output$ForecastCumGas_ThreeMonth[output$api == refracAPI[i]] = CumProduction.Gas.3Month
      } else if(Time2Refrac+3 <= currentTime){
        ##3 month cum calc
        CumProduction.Gas.6Month = 3333333333
        output$ForecastCumGas_SixMonth[output$api == refracAPI[i]] = CumProduction.Gas.6Month
        CumProduction.Gas.3Month = (arps.Np.hyp2exp(hyp2exp.fit.Gas$decline, Time2Refrac+3) 
                                    - arps.Np.hyp2exp(hyp2exp.fit.Gas$decline, Time2Refrac)) + PreRefracCumGas
        output$ForecastCumGas_ThreeMonth[output$api == refracAPI[i]] = CumProduction.Gas.3Month
      } else {
        CumProduction.Gas.6Month = 3333333333
        output$ForecastCumGas_SixMonth[output$api == refracAPI[i]] = CumProduction.Gas.6Month
        CumProduction.Gas.3Month = 3333333333
        output$ForecastCumGas_ThreeMonth[output$api == refracAPI[i]] = CumProduction.Gas.3Month
      }
    } else {
      hyp2exp.fit.Gas = best.hyp2exp(well$gasProduction, well$ProductionMonth)
      CumProduction.Gas = (arps.Np.hyp2exp(hyp2exp.fit.Gas$decline, currentTime)
          - arps.Np.hyp2exp(hyp2exp.fit.Gas$decline, Time2Refrac)) + PreRefracCumGas
      output$ForecastCumGas[output$api == refracAPI[i]] = CumProduction.Gas
      if(Time2Refrac+6 <= currentTime) {
        ###6 month cum calc
        CumProduction.Gas.6Month = (arps.Np.hyp2exp(hyp2exp.fit.Gas$decline, Time2Refrac+6) 
          - arps.Np.hyp2exp(hyp2exp.fit.Gas$decline, Time2Refrac)) + PreRefracCumGas
        output$ForecastCumGas_SixMonth[output$api == refracAPI[i]] = CumProduction.Gas.6Month
        CumProduction.Gas.3Month = (arps.Np.hyp2exp(hyp2exp.fit.Gas$decline, Time2Refrac+3) 
          - arps.Np.hyp2exp(hyp2exp.fit.Gas$decline, Time2Refrac)) + PreRefracCumGas
        output$ForecastCumGas_ThreeMonth[output$api == refracAPI[i]] = CumProduction.Gas.3Month
      } else if(Time2Refrac+3 <= currentTime){
        ##3 month cum calc
        CumProduction.Gas.6Month = 3333333333
        output$ForecastCumGas_SixMonth[output$api == refracAPI[i]] = CumProduction.Gas.6Month
        CumProduction.Gas.3Month = (arps.Np.hyp2exp(hyp2exp.fit.Gas$decline, Time2Refrac+3) 
          - arps.Np.hyp2exp(hyp2exp.fit.Gas$decline, Time2Refrac)) + PreRefracCumGas
        output$ForecastCumGas_ThreeMonth[output$api == refracAPI[i]] = CumProduction.Gas.3Month
      } else {
        CumProduction.Gas.6Month = 3333333333
        output$ForecastCumGas_SixMonth[output$api == refracAPI[i]] = CumProduction.Gas.6Month
        CumProduction.Gas.3Month = 3333333333
        output$ForecastCumGas_ThreeMonth[output$api == refracAPI[i]] = CumProduction.Gas.3Month
      }
    }    
  }
  return(output)
}

#Calling the forecast function and saving it to "output"
output = forecast(input)

#write.table(output, "C:/Users/gordon.tsai/Documents/Refracs/output")

#Writing output to csv. 
RefracPredictedCumulatives = write.csv(output, "C:/Users/gordon.tsai/Documents/Refracs/output.csv", row.names = FALSE) 

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

