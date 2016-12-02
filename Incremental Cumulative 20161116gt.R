
source("join_production_header.R")
dbHeader <- read.csv("Refrac Header.csv",header = TRUE)
dbProd <- read.csv("Refrac Production.csv", header = TRUE)


dbHeader <- dbHeader[order(dbHeader$api10),]
dbProd <- dbProd[order(dbProd$api10),]
#input_header <- subset(dbHeader, dbHeader$api10 == API_List[1:500])
#input_production <- subset(dbProd, dbProd$api10 == API_List[1:500])
#View(input_production)
#output = join_production_header(input_production, input_header)


wellnumber <- 1
well <- subset(dbProd, dbProd$api10 == unique(dbProd$api10)[wellnumber])
well <- well[order(well$rankDate),]
plot(well$rankDate,well$oilProduction/30.4,'l')

