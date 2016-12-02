input_header <- read.csv("Refrac Header.csv",header = TRUE)
input_production <- read.csv("Refrac Production.csv", header = TRUE)

forecast <- function(input_header, input_production) {
  
  #Import Package arpsDCA package
  #source("arpsDCA/R/bestfit.R")
  #source("arpsDCA/R/arps.R")
  #source("arpsDCA/R/curtail.R")
  #source("arpsDCA/R/eur.R")
  #source("arpsDCA/R/s3.R")
  
  
  
  #Filter out Refrac data
  refracAPI = unique(input_header$api10[which(is.na(input_header$refracTime) != TRUE)])
  ##FOR EACH COULD BE USED HERE?
  
  #API <- unique(input_production$api10)
  for(i in length(refracAPI)) {
    well = subset(input_production, input_production$api10 == refracAPI & is.na(input_production$refracTime) != TRUE)
    currentTime = max(well$rankDate)/12
    currentTimeSeries = well$rankDate/12
    well = subset(well, input_production$rankDate <= input_production$refracTime[1])
    #input_header <- input_header[order(input_header$api10),]
    well <- well[order(input_production$api10),]
    
    
    well <- subset(input_production, input_production$api10 == unique(input_production$api10)[i])
    well <- well[order(well$rankDate),]
    well$oilProduction = well$oilProduction/30.4
    well$gasProduction = well$gasProduction/30.4
    well$rankDate <- as.double(well$rankDate)/12
    
    
    #Find best fit line
    if(length(well$oilProduction) != 0 ) {
      hyp2exp.fit.Oil = best.hyp2exp(well$oilProduction, well$rankDate)
      hyp2exp.fit.Gas = best.hyp2exp(well$gasProduction, well$rankDate)
    }
    ##Plot fit
    #plot(well$rankDate, well$oilProduction, main="Hyperbolic Fit",col="blue", xlab="Time", ylab="Rate") 
    #lines( well$rankDate,arps.q.hyp2exp(hyp2exp.fit$decline, well$rankDate), col="red")
    #print(arps.Np.hyp2exp(hyp2exp.fit$decline, currentTime))
    
    #Output
    #rateTimeForecast = arps.q.hyp2exp(hyp2exp.fit$decline, currentTimeSeries)
    CumProduction.Oil = arps.Np.hyp2exp(hyp2exp.fit.Oil$decline, currentTime)
    CumProduction.Gas = arps.Np.hyp2exp(hyp2exp.fit.Gas$decline, currentTime)
    
    #Merge CumProduction with correct well API
    input_header$ForecastCumOil[input_header$api10 == API[i]] <- CumProduction.Oil
    input_header$ForecastCumGas[input_header$api10 == API[i]] <- CumProduction.Gas
    
    
    
    
  }
  
  output =  input_header
}

forecast(input_header,input_production)

best.hyp2exp(c,c)
