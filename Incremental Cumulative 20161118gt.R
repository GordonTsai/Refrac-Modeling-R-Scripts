
source("join_production_header.R")
#Import Package arpsDCA package
source("arpsDCA/R/bestfit.R")
source("arpsDCA/R/arps.R")
source("arpsDCA/R/curtail.R")
source("arpsDCA/R/eur.R")
source("arpsDCA/R/s3.R")

#install.packages('arps')
dbHeader <- read.csv("Refrac Header.csv",header = TRUE)
dbProd <- read.csv("Refrac Production.csv", header = TRUE)


dbHeader <- dbHeader[order(dbHeader$api10),]
dbProd <- dbProd[order(dbProd$api10),]
#input_header <- subset(dbHeader, dbHeader$api10 == API_List[1:500])
#input_production <- subset(dbProd, dbProd$api10 == API_List[1:500])
#View(input_production)
#output = join_production_header(input_production, input_header)


wellnumber <- 7
well <- subset(dbProd, dbProd$api10 == unique(dbProd$api10)[wellnumber])
well <- well[order(well$rankDate),]
well$rankDate <- as.double(well$rankDate)
plot(well$rankDate/12,well$oilProduction/30.4,'l')


hyperbolic.fit = best.hyperbolic(well$oilProduction/30.4, well$rankDate/12)
qi = hyperbolic.fit[[1]][1]
Di = hyperbolic.fit[[1]][2]
b = hyperbolic.fit[[1]][3]


#b =arps.q(hyperbolic.fit$decline, well$rankDate)
#Plot fit

plot(well$rankDate/12,well$oilProduction/30.4, main="Hyperbolic Fit",
     col="blue", xlab="Time", ylab="Rate")
lines(well$rankDate/12, arps.q(hyperbolic.fit$decline, well$rankDate/12),
      col="red")


