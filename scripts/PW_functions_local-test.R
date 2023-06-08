rm(list=ls())
update.packages(c('Rcurl','data.table','ape','picante','vegan','permute'))

source('scripts/PWFunctions_load.R')

### only for debugging
year <- 2020
stump <- F
orig.dead <- F
survival <- F
bsprout <- F
epicormic <- F
apical <- F
canopy <- F
bsprout.height <- F
bsprout.count <- F
tag.pulled <- T
keep.999 <- T
branches <- F
prefix <- 'https://raw.githubusercontent.com/dackerly/PepperwoodVegPlots/master/'
plot.list <- NA
###

get.indv.data <- function(year, stump=F, orig.dead=F, survival=F, bsprout=F, epicormic=F, apical=F, canopy=F, bsprout.height=F, bsprout.count=F, tag.pulled=F, keep.999=F, branches=F, prefix='https://raw.githubusercontent.com/dackerly/PepperwoodVegPlots/master/',plot.list=NA)
{
  require(RCurl)
  options(stringsAsFactors=FALSE) 
  #getURL('https://github.com/dackerly/PepperwoodVegPlots/tree/master/2020/Woody2020/Data/OriginalCSV/Woody')
  #dir.list <- dir(paste(prefix,year,"/Woody",year,"/Data/OriginalCSV/Woody/",sep=''))
  file.list <-(paste(prefix,year,"/Woody",year,"/Data/OriginalCSV/Woody/WoodySurvey",year,"_", sep='')) 
  if (is.na(plot.list[1])) plot.list<-get.plot(year=year)
  
  mega.data<-lapply(paste(file.list, plot.list, ".csv", sep=''), function(x) read.csv(text=getURL(x, followlocation = TRUE, cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl")), na.strings=c("","NA") , skip=3)) 
  names(mega.data) <- plot.list # assigns a name to each item of the list
  #print(names(mega.data[[1]]))
  i=1
  for (i in 1:length(mega.data)) 
  {
    Plot<-plot.list[i]
    mega.data[[i]]<-cbind(Plot=Plot, mega.data[[i]]) 
    #print(c(i,length(colnames(mega.data[[i]]))))
    
    if(year>=2018) 
    {
      # remove any excess columns created by excel
      mega.data[[i]] <- mega.data[[i]][,1:21]
      nm <- colnames(mega.data[[i]])
      #nm$SOD.on.Bay <- NA
      nm <-replace(nm,grep('X..cm',nm),'X_cm')
      nm <-replace(nm,grep('Y..cm',nm),'Y_cm')
      nm <-replace(nm,grep('Tree.Tag.No.',nm),'Num')
      nm <-replace(nm,grep('DBH..cm.',nm),'DBH_cm')
      nm <-replace(nm,which(nm=='Basal'),'bSprout')
      nm <-replace(nm,grep('Epicormic',nm),'Epicormic')
      nm <-replace(nm,grep('Apical',nm),'Apical')
      nm <-replace(nm,grep('X..Living.Canopy',nm),'Canopy.Percent') 
      nm <-replace(nm,grep('SA.or.Stump.Height..cm.',nm),'SA.Height_cm') 
      nm <-replace(nm,grep('SA.or.Stump.BD..cm.',nm),'SA.BD_cm') 
      nm <-replace(nm,grep('SA..Stem.',nm),'SA.Stems') 
      nm <-replace(nm,grep('Basal.resrpout..height.',nm),'Basal.Resprout.Height_cm')
      nm <- replace(nm,grep('Basal.Resprout.count',nm),'Basal.Resprout.Count')
      nm <-replace(nm,grep('Tag.pulled',nm),'Tag.Pulled')
      nm <-replace(nm,grep('SOD.on.Bay',nm),'SOD.on.Bay')
      
      colnames(mega.data[[i]]) <- nm
      rm('nm')
      
      # in 2018 only, newly killed trees were measured in 1851:1854 to recreate pre fire stands - these are numbered with 5 digits: 99###; so they are not removed by keep.999=F
      if (keep.999==F) mega.data[[i]] <- mega.data[[i]][mega.data[[i]]$Num>=1000,]
    } else {
      # THIS SECTION FOR 2013
      mega.data[[i]]<-mega.data[[i]][,1:14] 
      # CLEAN UP COLUMN NAMES - 2013 COLUMNS CAN **NEVER** BE REORDERED, OR THIS LINE NEEDS TO BE EDITED!
      colnames(mega.data[[i]])<-c("Plot", "Quad", "Type", "Num", "Species", "Confidence",
                                  "Dead.Stump", 
                                  "SA.Height_cm", "SA.BD_cm", 
                                  "SA.Stems", "DBH_cm", "X_cm", "Y_cm", "Notes") 
      mega.data[[i]]$Num[which(mega.data[[i]]$Dead.Stump %in% c("D","S"))] <- 999

      # subset no stumps and original dead
      if(stump==T & orig.dead==T) mega.data[[i]]<-subset(mega.data[[i]], subset=(mega.data[[i]]$Dead.Stump=="S" | mega.data[[i]]$Dead.Stump=="D" | is.na(mega.data[[i]]$Dead.Stump)))       
      # subset no stumps and original dead
      if(stump==F & orig.dead==T) mega.data[[i]]<-subset(mega.data[[i]], subset=(mega.data[[i]]$Dead.Stump=="D" | is.na(mega.data[[i]]$Dead.Stump))) 
      # subset original dead individuals
      if(stump==T & orig.dead==F) mega.data[[i]]<-subset(mega.data[[i]], subset=(mega.data[[i]]$Dead.Stump=="S" | is.na(mega.data[[i]]$Dead.Stump)))
      #subset both 
      if(stump==F & orig.dead==F) {mega.data[[i]]<-subset(mega.data[[i]], subset=(is.na(mega.data[[i]]$Dead.Stump)))
      }
    }
  }
  
  # turn list of dataframes into a single large dataframe called indv.data for easier manipulations
  indv.data<-do.call(rbind, mega.data)
  # get rid of extra rows - 2013 only
  if (year==2013) indv.data<-indv.data[!is.na(indv.data$Type),]
  if (year>=2018) indv.data<-indv.data[which(!is.na(indv.data$Num)),]
  
  # change SA.Stump.BD_cm into numeric
  indv.data$SA.BD_cm <- suppressWarnings(as.numeric(indv.data$SA.BD_cm))
  indv.data$DBH_cm <- suppressWarnings(as.numeric(indv.data$DBH_cm))
  # make indv.data$TreeNum numeric
  indv.data$Num<-as.numeric(indv.data$Num)
  # cleaning up Types 
  indv.data$Type[indv.data$Type==" TR"]<-"TR"
  indv.data$Type[indv.data$Type=="SA "]<-"SA"
  indv.data$Type[indv.data$Type=="SA  "]<-"SA"
  indv.data$Type[indv.data$Type=="S"]<-"SA"
  indv.data$Type[indv.data$Type=="AS"]<-"SA"
  
  # fix up Species names for bad data entry, etc.
  indv.data[which(indv.data$Species=="CEOCUN"), "Species"]<-"CEACUN"
  indv.data[which(indv.data$Species=="UNKN27"), "Species"]<-"CEACUN"
  indv.data[which(indv.data$Species=="ARBMEN "), "Species"]<-"ARBMEN"
  indv.data[which(indv.data$Species=="ARCMAN "), "Species"]<-"ARCMAN"
  indv.data[which(indv.data$Species=="CERBET "), "Species"]<-"CERBET"
  indv.data[which(indv.data$Species=="QUEAGR "), "Species"]<-"QUEAGR"
  indv.data[which(indv.data$Species=="QUEAGRI"), "Species"]<-"QUEAGR"
  indv.data[which(indv.data$Species=="QUEAGARI"), "Species"]<-"QUEAGR"
  indv.data[which(indv.data$Species=="QUAGRI"), "Species"]<-"QUEAGR"
  indv.data[which(indv.data$Species=="QUEARG"), "Species"]<-"QUEAGR"
  indv.data[which(indv.data$Species=="QUERAGR"), "Species"]<-"QUEAGR"
  indv.data[which(indv.data$Species=="QUEBERGAR"), "Species"]<-"QUEBEGA"
  indv.data[which(indv.data$Species=="QUEBERG"), "Species"]<-"QUEBER" 
  indv.data[which(indv.data$Species=="QUEBAR"), "Species"]<-"QUEBER" 
  indv.data[which(indv.data$Species=="QUEDEO"), "Species"]<-"QUEDOU"
  indv.data[which(indv.data$Species=="QUEDO"), "Species"]<-"QUEDOU"
  indv.data[which(indv.data$Species=="QUEGAR "), "Species"]<-"QUEGAR"
  indv.data[which(indv.data$Species=="QUEGARI"), "Species"]<-"QUEAGR" 
  indv.data[which(indv.data$Species=="QUEKEL "), "Species"]<-"QUEKEL"
  indv.data[which(indv.data$Species=="QUEKAL"), "Species"]<-"QUEKEL"
  indv.data[which(indv.data$Species=="QUEKELL"), "Species"]<-"QUEKEL"
  indv.data[which(indv.data$Species=="QUEWIZ"), "Species"]<-"QUEWIS"
  indv.data[which(indv.data$Species=="PIEMEN"), "Species"]<-"PSEMEN"
  indv.data[which(indv.data$Species=="PSMEN"), "Species"]<-"PSEMEN"
  indv.data[which(indv.data$Species=="PSEMEM"), "Species"]<-"PSEMEN"
  indv.data[which(indv.data$Species=="SEMEN"), "Species"]<-"PSEMEN"
  indv.data[which(indv.data$Species=="SAL SP."), "Species"]<-"SAL_SP"
  indv.data[which(indv.data$Species=="UMBCAL "), "Species"]<-"UMBCAL"
  indv.data[which(indv.data$Species=="UMCAL"), "Species"]<-"UMBCAL"
  indv.data[which(indv.data$Species=="UMBAL"), "Species"]<-"UMBCAL"
  indv.data[which(indv.data$Species=="UNKN30"), "Species"]<-"UNK"
  indv.data[which(indv.data$Species=="QUEDEC"), "Species"]<-"QUEdec"
  indv.data[which(indv.data$Species=="UNKN47"), "Species"]<-"PRUCER"
  indv.data[which(indv.data$Species=="CEATHY"), "Species"]<-"CEAPAR"
  
  
  ##Used to run these lines in order to replace incorrect species ID's in 2013- changed these in the 2013 CSV files during 2022 QC- no need to use AUG_Species csv anymore- some names in that file are incorrect now anyway
  
  # Change individuals originally identified as "QUEDEC" to species-level identification 
  #if(year==2013) {
  # AUG.ID<-read.csv(text=getURL(paste(prefix, "2013/OakID2013/AUG_Species.csv", sep=''), followlocation = TRUE, cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl")))
  #for(i in 1:dim(AUG.ID)[1]){
  #  indv.data[indv.data$Num %in% AUG.ID$Num[i], "Species"] <-AUG.ID$Species[i]
  #  }
  # }
  
  # change coordinates to all be positive
  indv.data$Y_cm<-suppressWarnings(as.numeric(indv.data$Y_cm))
  indv.data[which(indv.data$X_cm<0),"X_cm"]<-indv.data[which(indv.data$X_cm<0),"X_cm"]+500
  indv.data[which(indv.data$Y_cm<0),"Y_cm"]<-indv.data[which(indv.data$Y_cm<0),"Y_cm"]+500
  
  # condense each individual into a single row 
  #indv.data$Point <- indv.data$Num %% 1
  indv.data$iNum<-floor(indv.data$Num) #strips away decimal point values
  indv.data$Point <- indv.data$Num - indv.data$iNum
  
  # For saplings, change NAs for stem number to 0, and then add 1 to all - CHECK 2013 question for the possibility that main stem was included in stem number
  indv.data$SA.Stems[indv.data$Type=="SA" & is.na(indv.data$SA.Stems)] <- 0
  indv.data$SA.Stems[which(indv.data$Type=="SA")] <- indv.data$SA.Stems[which(indv.data$Type=="SA")] + 1
  
  indv.data$dbh <- NA
  indv.data$dbh[which(indv.data$Type=="SA")] <- indv.data$SA.BD_cm[which(indv.data$Type=="SA")]
  indv.data$dbh[which(indv.data$Type=="TR")] <- indv.data$DBH_cm[which(indv.data$Type=="TR")]
  
  indv.data$Basal.Area <- ((indv.data$dbh/2)^2)*(pi)

  # For saplings adjust basal area by number of stems < 10cm. As done here it overestimates by multiplying by stem number, and the other stems are smaller than main stem
  #indv.data$Basal.Area[which(indv.data$Type=="SA")] <-(indv.data$Basal.Area[which(indv.data$Type=="SA")])*(indv.data$SA.Stems[which(indv.data$Type=="SA")])
  
  # CREATE FOUR STATUS VARIABLES AND ASSIGN
  indv.data$Dead <- NA
  indv.data$Live <- NA
  indv.data$Topkill <- NA
  indv.data$gCrown <- NA
  
  if (year==2013) {
    indv.data$Survival <- NA
    indv.data$Epicormic <- NA
    indv.data$Apical <- NA
  } else 
  {
    indv.data$Epicormic[which(indv.data$Type=='SA')] <- NA
    indv.data$Apical[which(indv.data$Type=='SA')] <- NA
  }
  
  indv.data$NPoints <- NA
  indv.data$NPoints[which(indv.data$Type=='SA')] <- indv.data$SA.Stems[which(indv.data$Type=='SA')]
  indv.data$NPoints[which(indv.data$Type=='TR')] <- 1
  
  if (year==2013) 
  {
    indv.data$Live <- 1
    indv.data$Live[!is.na(indv.data$Dead.Stump)] <- 0
    indv.data$gCrown <- indv.data$Live
    
    ## make list of individuals with TS attached
    indv.data$bSprout[indv.data$Point==0] <- 0
    TSlist <- unique(indv.data$iNum[which(indv.data$Type=='TS')])
    indv.data$bSprout[which(indv.data$Num %in% TSlist)] <- 1
  } else 
  {
    indv.data$Apical[which(indv.data$Type=='TR' & indv.data$Survival==0)] <- 0
    indv.data$Epicormic[which(indv.data$Type=='TR' & indv.data$Survival==0)] <- 0
    indv.data$Live <- apply(indv.data[,c('Survival','bSprout')],1,max,na.rm=T)
    
    indv.data$gCrown[which(indv.data$Type=='TR')] <- apply(indv.data[which(indv.data$Type=='TR'),c('Epicormic','Apical')],1,max,na.rm=T)
    
    indv.data$gCrown[which(indv.data$Type=='SA')] <- indv.data$Survival[which(indv.data$Type=='SA')]
    
    indv.data$Topkill <- 0
    indv.data$Topkill[which(indv.data$Survival==0)] <- 1
  }
  
  # convert TS which were main stems to SA. If not main stem, get collapsed in next step (if branches==F)
  indv.data$Type[which(indv.data$Type == 'TS' & indv.data$Point == 0)] <- "SA"
  
  if (branches==F)
  {
    # get rid of dead points - DIDN'T WORK
    # indv.data <- indv.data[-which(indv.data$Survival==0 & indv.data$bSprout==0  & indv.data$Num%%1==0),]
    
    # Convert to data.table to collapse rows 
    library(data.table)
    indv.data<-data.table(indv.data)
    indv.data[,dbh:=max(dbh),by="iNum"]
    indv.data[,Basal.Area:=sum(Basal.Area),by="iNum"]
    allZero <- function(x) if (all(x==0)) return(0) else return(1)
    allOne <- function(x) if (all(x==1)) return(1) else return(0)
    
    if(year>=2018) {
      indv.data[,Survival:=max(Survival),by="iNum"]
      indv.data[,Live:=max(Live),by="iNum"]
      indv.data[,Topkill:=min(Topkill),by="iNum"]
      indv.data[,bSprout:=max(bSprout),by="iNum"]
      indv.data[,Epicormic:=max(Epicormic),by="iNum"]
      indv.data[,Apical:=max(Apical),by="iNum"]
      indv.data[,gCrown:=max(gCrown),by="iNum"]
      #indv.data[,Canopy.Percent:=max(Canopy.Percent),by="iNum"] #### BIT OF A FUDGE
    }
    # Set Dead to 1-Live (after step above to allow for effects of collapsing branches)
    indv.data$Dead <- 1 - indv.data$Live
    
    # Keep only main stem rows
    indv.data<-indv.data[indv.data$Point==0,] 
    
    # Get rid of the extra columns "DBH_cm", ... 
    indv.data<-as.data.frame(indv.data)
    indv.data<-indv.data[,c("Plot","Quad","Type","Num","Species","X_cm","Y_cm","Basal.Area","dbh","Survival","Dead","Live","bSprout","Topkill","Epicormic","Apical","gCrown")]
  } 
  # else {
  #   indv.data[indv.data$Type=="SA", "Basal.Area" ]<-(((indv.data[indv.data$Type=="SA", "SA.Stump.BD_cm"]/2)^2)*(pi))*(indv.data[indv.data$Type=="SA","SA.Stem.Num"])
  #   indv.data[indv.data$Type=="TR", "Basal.Area"]<- (((indv.data[indv.data$Type=="TR","DBH_cm"]/2)^2)*(pi))
  # }
  # 
  
  if (year=="none"){
    indv.data<-subset(indv.data, subset=(indv.data$Dead.Stump=="D" | indv.data$Dead.Stum=="S"))  
  } else {
    indv.data$Year<-year
  }
  
  row.names(indv.data) <- NULL
  
  # remove PSEMEN trees that were accidentally removed by chainsaw crews in winter 2017 in plot PPW1307 
  indv.data<-indv.data[which(!(indv.data$Plot=="PPW1307" & indv.data$Num==1108 | indv.data$Num==1115 |indv.data$Num==1118 |indv.data$Num==1121 | indv.data$Num==1127 | indv.data$Num==1169 | indv.data$Num==1170 |indv.data$Num==1174)),]
  
  return(indv.data)
}

### local function versions for trouble shooting
plants.by.plot <- function(year, type){
  plot.list<- get.plot()
  if(type=="SEJU"){
    seju<-get.seju.data(year=year)
    seju<-with(seju, aggregate(cbind(Num.Seedlings,Num.Juveniles,Total.Number)~Species+Plot+Year, FUN=sum))        
    seju<-seju[,c(2,1,3:6)]
    seju$Year<-year
    return(seju)                 
  }
  else{
    indv.data<-suppressWarnings(get.indv.data(year=year))
    if(type!="SA.TR") {
      count<-with(indv.data[indv.data$Type==type,], aggregate(Num~Species+Plot+Type, FUN=function(x)  length(unique(x))))
      area<-with(indv.data[indv.data$Type==type,], aggregate(Basal.Area~Species+Plot+Type, FUN=sum))
      output<-merge(area,count, by=c("Plot","Species","Type"))
      output$Year<-year
      colnames(output)<-c("Plot", "Species", "Type", "Basal.Area","Count","Year")}
    else{
      count.sap<-with(indv.data[indv.data$Type=="SA",], aggregate(Num~Species+Plot+Type, FUN=function(x)  length(unique(x))))
      area.sap<-with(indv.data[indv.data$Type=="SA",], aggregate(Basal.Area~Species+Plot+Type, FUN=sum))
      output.sap<-merge(area.sap,count.sap,by=c("Plot","Species","Type"))
      count.tr<-with(indv.data[indv.data$Type=="TR",], aggregate(Num~Species+Plot+Type, FUN=function(x)  length(unique(x))))
      area.tr<-with(indv.data[indv.data$Type=="TR",], aggregate(Basal.Area~Species+Plot+Type, FUN=sum))
      output.tr<-merge(area.tr,count.tr,by=c("Plot","Species","Type"))
      output<-merge(output.sap,output.tr,by=c("Plot","Species"),all=T)
      output<-output[-c(3,6)]
      output$Year<-year
      colnames(output)<-c("Plot","Species","SA.Basal.Area","SA.Count","TR.Basal.Area","TR.Count","Year")
      output[is.na(output)] <- 0
    }
    return(output)
  }   
}
