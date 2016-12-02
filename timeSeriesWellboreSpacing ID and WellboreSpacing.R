rm(list=ls())
library('zoo')
library("RODBC")
library('spatstat')
library('PBSmapping')
library('cwhmisc')
library('geosphere')
library('rgdal')
library('reshape2')
library("plyr")
library("ggplot2")
library('maps')
library('Hmisc')
library('lubridate')
library('data.table')
library('sp')
library('RColorBrewer')

#setwd("U:/Analysis/Spacing")
#start clock
ptm <- proc.time()
ttime1 <- proc.time()

##########  Data Pull  ################### 
{
  myConn <- odbcDriverConnect('driver={SQL Server};server=gis-mssql.prod.aus,60342;trusted_connection=true')
  data <- sqlQuery(myConn,"
                   with permitRank as (
                   select DISTINCT api
                   , bottomLatitude
                   , bottomLongitude
                   , lateralLength
                   , row_number()  over (partition by api order by lateralLength desc) as ranked
                   from esp_stage.dbo.espPermit p
                   where lateralLength is not null 
                   ),
                   
                   x as 
                   (
                   SELECT distinct h.api
                   , h.hpdiEntityId
                   , h.play
                   , h.operator
                   , h.reservoirAlias 
                   , h.firstProductionDate
                   , h.azimuth[az]
                   , h.latitude27
                   , h.longitude27
                   , (h.latitude27 + p.bottomlatitude) / 2[midlat]
                   , (h.longitude27 + p.bottomlongitude) / 2[midlong]
                   , p.bottomlatitude[botlat]
                   , p.bottomlongitude[botlong]
                   , wellbore
                   --, row_number()  over (partition by h.api order by firstProductionDate,reservoirAlias) as ranked2
                   
                   from esp_stage.dbo.espHeader h
                   left join permitRank p on h.api = p.api and p.ranked = 1
                   where h.api <>'0' 
                   and firstProductionDate is not null
                   and h.longitude27 is not null
                   and h.latitude27 is not null
                   and (p.bottomLatitude is not null or h.wellbore = 'Vertical')
                   and (p.bottomLongitude is not null or h.wellbore = 'Vertical')
                   
                   ),
                   
                   y as 
                   (
                   SELECT distinct api
                   , hpdiEntityId
                   , play
                   , [az]
                   , latitude27
                   , longitude27
                   , [midlat]
                   , [midlong]
                   , [botlat]
                   , [botlong]
                   , wellbore
                   ,reservoirAlias
                   ,firstProductionDate
                   ,case
                   when x.latitude27 = x.botlat and x.longitude27 = x.botlong then x.latitude27 --mistake input
                   when x.wellbore = 'VERTICAL' and x.botlat IS NOT NULL then x.botlat 
                   when x.wellbore = 'VERTICAL' and x.botlat IS NULL then x.latitude27
                   when x.wellbore = 'DIRECTIONAL' and x.botlat IS NOT NULL then x.botlat
                   when x.wellbore = 'HORIZONTAL' and x.midlat IS NOT NULL then x.midlat
                   when x.wellbore IS NULL and x.midlat IS NOT NULL then x.midlat
                   else x.latitude27
                   end [Y] 
                   ,case
                   when x.latitude27 = x.botlat and x.longitude27 = x.botlong then x.longitude27
                   when x.wellbore = 'VERTICAL' and x.botlong IS NOT NULL then x.botlong
                   when x.wellbore = 'VERTICAL' and x.botlong IS NULL then x.longitude27
                   when x.wellbore = 'DIRECTIONAL' and x.botlong IS NOT NULL then x.botlong
                   when x.wellbore = 'HORIZONTAL' and x.midlong IS NOT NULL then x.midlong
                   when x.wellbore IS NULL and x.midlong IS NOT NULL then x.midlong
                   else x.longitude27
                   end [X]              
                   from x
                   where x.longitude27 < 0 and x.latitude27 > 0 
                   --AND ranked2=1 --Unique api so well is not neighbor to itself. More than 99% of apis have same  or 4th digit same coordinates
                   AND play<>'Permian Basin' --Permian Basin is currently too complex geologically to accuratly determine nn
                   )
                   
                   SELECT distinct api
                   , hpdiEntityId
                   , play
                   , reservoirAlias
                   , firstProductionDate
                   , az
                   , X
                   , Y
                   , wellbore
                   , latitude27
                   , longitude27
                   , botlat
                   , botlong
                   FROM y
                   WHERE play = 'Bakken' AND api LIKE '33053%'--play subset
                   ")
  close(myConn)
}

#data$firstProductionDate<-as.Date(data$firstProductionDate)

###############  Set Global Variables  #################
window<-owin(xrange=c(-3000,3000), yrange=c(2000,7000))
mark <- vector()
eldiff <- vector()
nearrow <- vector()
r <- vector()
dist <- 26400
time <- 33
delaz <- 30
maxDate <- max(data$firstProductionDate)
u <- 30
deldate <- integer()
delay <- vector()
totdist <- vector()
count <- 0
mark <- 0
near <- matrix(NA, nrow=nrow(data), ncol = 6 )
colnames(near) <- c("NeighborId","API2", "NeighborApi", "Dist", "Iter", "nnStatus")

#data$botel <- as.numeric(as.character(data$botel))
data$az <- as.numeric(as.character(data$az))
#data$tvd <- as.numeric(as.character(data$tvd))

#for (j in 1:length(names)) {
  year<-as.Date(as.yearmon(2015)) #Start Year
  
  data.all <- data.frame()
  data.filter <- data
  data.cut <- data.filter[rowSums(is.na(data.filter)) != ncol(data.filter),]
  colnames(data.cut) <- c("api","hpdiEntityId","play","reservoirAlias","firstProductionDate","az","X", "Y","wellbore","latitude27","longitude27","botlat","botlong")
  data.cut$firstProductionDate <- as.Date(data.cut$firstProductionDate)

  
  #############  Create nearest neighbor distance function  ###################
  
  NN<-function(mids,window,u) {
    attr(mids, "projection")<-"LL"
    midsUTM<-convUL(mids)
    midsppp<<-as.ppp(midsUTM,window)
    nn<<-nndist(midsppp,k=1:u)
    nn <- data.frame(nn)
    nn.name <<- nnwhich(midsppp, k=1:u)
    nn.name <- data.frame(nn.name)
    nnfeet<<-nn*3280.8399
  }

  
  while (year<=as.Date(as.yearmon(maxDate))) { ##end condition
    temp<-data.cut[(data.cut$firstProductionDate<=year),c("api","hpdiEntityId","play","reservoirAlias","firstProductionDate","az","X", "Y","wellbore","latitude27","longitude27","botlat","botlong")]#

    if (nrow(temp) > 30) {
      window<-owin(xrange=c(-3000,3000), yrange=c(2000,7000))
      mids<-data.frame(temp[,c("X","Y")])
      mids<- data.frame(mids)
      colnames(mids) <- c("X", "Y")
      NN(mids,window,u)
      dr <- nrow(temp)
      dc <- ncol(nnfeet)
      nb <- vector("list", dr)
      nd <- vector("list", dr)
      iter <- vector("list", dr)
      nnApi <- vector("list", dr)
      nnId <- vector("list", dr)
      test <- vector("list", dr)
      near[,"NeighborId"] <- data$hpdiEntityId
      #near[,"API2"] <- data$api
      
      nn[is.infinite(nn)] <- 0 
      nn.name[is.infinite(nn.name)] <- 0
      ##HERE BE CODE
      
      dc <- ncol(nnfeet)
      
      for (d in 1:dr) { #cycle through every row
        ##  print(d)
        p1 <- temp$play[d]
        ra1 <- temp$reservoirAlias[d]
        api1 <- data$api[d]
        
        #print("for loop 1")
        
        for (e in 1:dc) { #cycle through nearest neighbors, starting with the closest
          #report 1st nearest neighbor name and distance
          #print("for loop 2") 
          ra2 <- temp$reservoirAlias[nn.name[d,e]]
          api2 <- data$api[nn.name[d,e]]
          d1 <- nnfeet[d,e]
          ym <- temp$firstProductionDate[nn.name[d,e]]
          ##    print(paste(d, e, d1, p1, ra1, ra2))
          if ((d1 > dist) & (api1 != api2)){ ##spot when distance is too large - this is is disabled bc there isn't a good way to get NONE back into single column with numeric spacings
            nb[[d]] <- NA
            nd[[d]] <- NA
            iter[[d]] <- e
            #nnApi[[d]] <- NA
            nnId[[d]] <- NA
            test[[d]] <- 0
            #break
          }
          else if (d1 > 0 & (p1 != 'Bakken') | (p1 == 'Bakken' & ra1 == ra2)) { ##special case for Bakken to only id near neighbors for same reservoir, not applied to rest of dataset. approved by Chris Smith.
            #nb[[d]] <- temp$hpdiEntityId[nn.name[d,e]]
            #nnApi[[d]] <- temp$api[nn.name[d,e]]
            
            if ((temp$wellbore[d] %in% 'HORIZONTAL' & temp$wellbore[nn.name[d,e]] %in% 'HORIZONTAL') & (!is.na(temp$az[d]) & !is.na(temp$az[nn.name[d,e]]))) { ##special geometry calc if both wells are horizontal to avoid underestimate
              if (((abs(temp$az[d] - temp$az[nn.name[d,e]]) >= 0 & (abs(temp$az[d] - temp$az[nn.name[d,e]]) <= delaz)) | (abs(temp$az[d] - temp$az[nn.name[d,e]]) <= 360 & (abs(temp$az[d] - temp$az[nn.name[d,e]])) >= (360-delaz)))
                  & (((distCosine(c(temp$longitude27[d], temp$latitude27[d]), c(temp$longitude27[nn.name[d,e]], temp$latitude27[nn.name[d,e]])) * 3.2808) <= dist)
                     &((distCosine(c(temp$botlong[d], temp$botlat[d]), c(temp$botlong[nn.name[d,e]], temp$botlat[nn.name[d,e]])) * 3.2808) <= dist)) ){ ##aligned wellbores
                ##centerline
                avgSurfLat <- (temp$latitude27[d] + temp$latitude27[nn.name[d,e]]) / 2
                avgSurfLong <- (temp$longitude27[d] + temp$longitude27[nn.name[d,e]]) / 2
                avgBotLat <- (temp$botlat[d] + temp$botlat[nn.name[d,e]]) / 2
                avgBotLong <- (temp$botlong[d] + temp$botlong[nn.name[d,e]]) / 2
                
                ##combine into vector
                surfCenter <- c(avgSurfLong, avgSurfLat)
                botCenter <- c(avgBotLong, avgBotLat)
                
                ##calc bearings
                bearCenter <- bearing(botCenter, surfCenter)
                bearBot <- bearing(c(temp$botlong[d], temp$botlat[d]), c(temp$botlong[nn.name[d,e]], temp$botlat[nn.name[d,e]]))
                
                theta <- bearCenter - bearBot - 90
                botDist <- distCosine(c(temp$botlong[d], temp$botlat[d]), c(temp$botlong[nn.name[d,e]], temp$botlat[nn.name[d,e]])) * 3.2808
                
                totdist <- abs(botDist * cos(theta / 180 * pi))
                
                #add in depth distance if it is available for both wells - this is disabled bc we don't have reliave TVD, our TVD col is calculated by total_depth - our calcd lateral length
                #     if (is.na(temp$tvd[d]) == 'FALSE' & is.na(temp$tvd[nn.name[d,e]]) == 'FALSE') { 
                #       totdist <- (horizDist^2 + (temp$tvd[d] - temp$tvd[nn.name[d,e]])^2)^.5
                #     } else { totdist <- horizDist }
                nb[[d]] <- temp$hpdiEntityId[nn.name[d,e]]
                #nnApi[[d]] <- temp$api[nn.name[d,e]]
                nnId[[d]] <- temp$hpdiEntityId[nn.name[d,e]]
                if (!is.na(totdist) & totdist >= nnfeet[d,e]){
                  nd[[d]] <- totdist
                } else {
                  nd[[d]] <- nnfeet[d,e]
                }
                #nd[[d]] <- totdist
                iter[[d]] <- e
                test[[d]] <- 1
                break
              } 
              else if (((abs(temp$az[d] - temp$az[nn.name[d,e]]) >= 180 - delaz & abs(temp$az[d] - temp$az[nn.name[d,e]]) <= 180 + delaz))
                       & (((distCosine(c(temp$longitude27[d], temp$latitude27[d]), c(temp$botlong[nn.name[d,e]], temp$botlat[nn.name[d,e]])) * 3.2808) <= dist)
                          &((distCosine(c(temp$botlong[d], temp$botlat[d]), c(temp$longitude27[nn.name[d,e]], temp$latitude27[nn.name[d,e]])) * 3.2808) <= dist))) { ##reversed wellbores
                ##centerline - connecting surf to bottom bc wellbores are reversed
                avgSurfLat <- (temp$latitude27[d] + temp$botlat[nn.name[d,e]]) / 2
                avgSurfLong <- (temp$longitude27[d] + temp$botlong[nn.name[d,e]]) / 2
                avgBotLat <- (temp$botlat[d] + temp$latitude27[nn.name[d,e]]) / 2
                avgBotLong <- (temp$botlong[d] + temp$longitude27[nn.name[d,e]]) / 2
                
                ##combine into vector
                surfCenter <- c(avgSurfLong, avgSurfLat)
                botCenter <- c(avgBotLong, avgBotLat)
                
                ##calc bearings
                bearCenter <- bearing(botCenter, surfCenter)
                bearBot <- bearing(c(temp$botlong[d], temp$botlat[d]), c(temp$botlong[nn.name[d,e]], temp$botlat[nn.name[d,e]]))
                
                theta <- bearCenter - bearBot - 90
                botDist <- distCosine(c(temp$botlong[d], temp$botlat[d]), c(temp$longitude27[nn.name[d,e]], temp$latitude27[nn.name[d,e]])) * 3.2808
                
                totdist <- abs(botDist * cos(theta / 180 * pi))
                
                #add in depth distance if it is available for both wells  - this is disabled bc we don't have reliave TVD, our TVD col is calculated by total_depth - our calcd lateral length
                #       if (is.na(temp$tvd[d]) == 'FALSE' & is.na(temp$tvd[nn.name[d,e]]) == 'FALSE') { 
                #         totdist <- (horizDist^2 + (temp$tvd[d] - temp$tvd[nn.name[d,e]])^2)^.5
                #       
                #       } else { totdist <- horizDist }
                
                nb[[d]] <- temp$hpdiEntityId[nn.name[d,e]]
                #nnApi[[d]] <- temp$api[nn.name[d,e]]
                nnId[[d]] <- temp$hpdiEntityId[nn.name[d,e]]
                if (!is.na(totdist) & totdist >= nnfeet[d,e]){
                  nd[[d]] <- totdist
                } else {
                  nd[[d]] <- nnfeet[d,e]
                }
                #nd[[d]] <- totdist
                iter[[d]] <- e
                test[[d]] <- 2
                break    
              } 
              else {
                nb[[d]] <- NA
                #nnApi[[d]] <- NA
                nnId[[d]] <- NA
                nd[[d]] <- NA
                iter[[d]] <- e
                test[[d]] <- 3
              }
            }
            else if ((temp$wellbore[d] %in% 'HORIZONTAL' & temp$wellbore[nn.name[d,e]] %in% 'HORIZONTAL') & (!is.na(temp$az[d]) & is.na(temp$az[nn.name[d,e]]))){  
              nb[[d]] <- temp$hpdiEntityId[nn.name[d,e]]
              nnApi[[d]] <- temp$api[nn.name[d,e]]
              nd[[d]] <- nnfeet[d,e]
              iter[[d]] <- e
              test[[d]] <- 10  
              
            }
            else if (temp$wellbore[d] %in% 'HORIZONTAL' & is.na(temp$az[d])){  
              nb[[d]] <- NA
              nnApi[[d]] <- NA
              nd[[d]] <- NA
              iter[[d]] <- e
              test[[d]] <- 11  
              
            }
            else if (!temp$wellbore[d] %in% 'HORIZONTAL' & !temp$wellbore[nn.name[d,e]] %in% 'HORIZONTAL'){  
              nb[[d]] <- temp$hpdiEntityId[nn.name[d,e]]
              #nnApi[[d]] <- temp$api[nn.name[d,e]]
              nnId[[d]] <- temp$hpdiEntityId[nn.name[d,e]]
              nd[[d]] <- nnfeet[d,e]
              iter[[d]] <- e
              test[[d]] <- 12
              break
            } 
            else {
              nb[[d]] <- NA
              nd[[d]] <- NA
              #nnApi[[d]] <- NA
              nnId[[d]] <- NA
              iter[[d]] <- e
              test[[d]] <- 13
              
            }
          } 
          else {
            nb[[d]] <- NA
            nd[[d]] <- NA
            #nnApi[[d]] <- NA
            nnId[[d]] <- NA
            iter[[d]] <- e
            test[[d]] <- 21
            
          }
        }
      }
      nd <- as.numeric(nd)
      nnId <- as.numeric(nnId)
      nnId.last <- nnId
      final<-cbind(temp,nd)
      final<-final[,c(2,14)]
      final.last<- cbind(final,nnId.last) 
      rm(list=c("mids","midsppp","nn","nnfeet","window","nd","nnId"))
      data.cut<-merge(data.cut,final, all.x=TRUE,by="hpdiEntityId")
      x1<-as.character(year)
      colnames(data.cut)[length(data.cut)]<-x1
      final.last$prodMon<-factor(year)
      rm(list=c("x1"))
    }
    #print(year)
    year<-as.yearmon(as.yearmon(year)+1/12)
    year<-as.Date(year)
    #ttime2 <- proc.time() - ttime1
    #print(ttime2)
    #ttime1 <- proc.time()
    
  }
  median.dist <- c(sapply(data.cut[,14:ncol(data.cut)], median, na.rm=TRUE))
  median.dist <- data.frame(median.dist)
 # plot(median.dist)
 # title(main = names[j])
 # histo <- hist(data.cut[,ncol(data.cut)], breaks = c(100*(0:50000)), xlim = range(0, 5280))
 # title(main = names[j])

  #mean near neighbor ft - don't use because wildcats heavily skew numbers
  #mean.dist <- c(sapply(data.cut[,5:ncol(data.cut)], mean, na.rm=TRUE))
  #plot(mean.dist)
  data.out <- melt(data.cut, id.vars=c("hpdiEntityId"), measure.vars = c(14:ncol(data.cut)))
  
  colnames(data.out)[colnames(data.out)=="variable"] <- "prodMon"
  colnames(data.out)[colnames(data.out)=="value"] <- "nearNeighborFt"
  #data.out <- data.out[order(data.out$hpdiEntityId, data.out$prodMon),]
  #eliminate NA rows
  data.out <- data.out[!is.na(data.out$nearNeighborFt),]
  
  #combine results
  data.all <- rbind(data.all, data.out)

  
###write back to SQL database###

##nneighbor##
##replace NA and Inf with blanks so SQL doesn't choke  
names(final.last) <- c("hpdiEntityId", "nearNeighborFt","nearNeighborId", "prodMon")
final.last$nearNeighborFt[final.last$nearNeighborFt == 0] <- NA #For SQL
final.last[final.last=="NA"] <- ""
final.last[final.last=="Inf"] <- ""
# write.csv(final.last, "nneighbor.csv", row.names = FALSE) #can be comented for Production
# write.table(as.data.frame(final.last), "//di-mssql02.prod.aus/analytics$/espNearNeighbor.csv", sep=",", row.names = FALSE, col.names = FALSE, quote=FALSE)
# 
# #data dump back into SQL
# myConn <- odbcDriverConnect('driver={SQL Server};server=gis-mssql.prod.aus,60342;trusted_connection=true')
#     sqlQuery(myConn, "use esp_data
# drop table espNearNeighbor
# CREATE TABLE espNearNeighbor(
#       [hpdiEntityId] [int] NULL,
#       [nearNeighborEntityId] [int] NULL,
#       [nearNeighborFt] [decimal](38,0) NULL
# ) ON [PRIMARY]
# 
# BULK INSERT espNearNeighbor FROM '//di-mssql02.prod.aus/analytics$/espNearNeighbor.csv' WITH (FIELDTERMINATOR = ',', ROWTERMINATOR = '\n')
#              
# update esp_stage.dbo.espHeader
# set    esp_stage.dbo.espHeader.nearNeighborEntityID = s.nearNeighborEntityId,
#        esp_stage.dbo.espHeader.nearNeighborFt = s.nearNeighborFt
# from esp_stage.dbo.espHeader h
#        left join esp_data.dbo.espNearNeighbor s on h.hpdiEntityId = s.hpdientityid
#              ")
# close(myConn)



##timeSeries##
##replace NA and Inf with blanks so SQL doesn't choke
combine <- data.all[,c("hpdiEntityId", "nearNeighborFt", "prodMon")]
combine[combine=="NA"] <- ""
combine[combine=="Inf"] <- ""
#write.csv(combine, "nneighborTimeSeries.csv", row.names = FALSE) #can be comented for Production
#write.table(as.data.frame(combine),"//di-mssql02.prod.aus/analytics$/nnTime.txt",quote=FALSE,sep=",",row.names=FALSE,col.names=FALSE,append=FALSE)

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


ttime <- proc.time() - ptm
print(ttime)