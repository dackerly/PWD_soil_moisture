# summarize soil moisture to date
rm(list=ls())
#write.csv('x','scripts/test.csv')
#source('scripts/PW_functions_local-test.R')
#plot.list.2020 <- get.plot(2020)

plots <- c(paste('PPW130',1:9,sep=''),paste('PPW13',10:50,sep=''),paste('PPW18',51:54,sep=''))
plots
sm <- data.frame(plot=plots)

infiles <- dir('soil_moisture_data/')
i=7
for (i in 1:length(infiles))
{
  print(i)
  yr <- substr(infiles[i],13,16)
  colname <- paste('sm',yr,sep='')
  sm <- cbind(sm,newcol=NA)
  names(sm)[ncol(sm)] <- colname
  smd <- read.csv(paste('soil_moisture_data/',infiles[i],sep=''))
  head(smd)
  
  dcols <- grep('reading',names(smd))
  if (length(dcols)>1) smd$reading <- apply(smd[,dcols],1,mean,na.rm=T)
  
  smp <- tapply(smd$reading,smd$plot,mean,na.rm=T)
  smp2sm <- match(sm$plot,names(smp))
  sm[,colname] <- smp[smp2sm]
}
rownames(sm) <- sm[,1]
sm <- sm[,-1]
head(sm)
tail(sm)
ctab <- cor(sm,use='pair')
image(ctab,asp=1)
heatmap(ctab)

# GAP FILL 2 values
smat <- sm[1:50,-c(7,8)]
smat
dim(smat)

# iteratively gap fill
for (i in 1:10) {
  pmean <- apply(smat,1,mean,na.rm=T)
  ymean <- apply(smat,2,mean,na.rm=T)
  mean(pmean)
  mean(ymean)
  
  (smat[30,9] <- pmean[30]+ymean[9]-mean(pmean))
  (smat[42,9] <- pmean[42]+ymean[9]-mean(pmean))
}

# replace back into full data set
sm[30,11] <- smat[30,9]
sm[42,11] <- smat[42,9]

#filter by superplots (measured 10 times)
lengthNonNA <- function(x) length(which(!is.na(x)))
Nvals <- apply(sm,1,lengthNonNA)
Nvals
alldates <- which(Nvals==11)
length(alldates)

Nplots <- apply(sm,2,lengthNonNA)
Nplots

# cor tab for complete time series only
ctab10 <- cor(sm,use='pair')
image(ctab10,asp=1)
heatmap(ctab10)

#time series
cols <- rep('black',54)
cols[alldates] <- 'red'

plot(1:11,sm[1,],type='b',ylim=c(0,max(sm,na.rm=T)),xaxt='n',xlab='Year',ylab='Soil Moisture',col=cols[1])
axis(1,1:11,2013:2023)

i=2
for (i in 2:nrow(sm))
{
  points(1:11,sm[i,],type='b',col=cols[i])
}
abline(v=5.5,lty=2)
abline(v=7.5,lty=2)

# look at some scatterplots
plot(sm[,10:11])
plot(sm[,9:10])
plot(sm[,2:3])
pairs(sm)

# principal components - NOT WORKING
head(sm)
tail(sm)
pc <- princomp(sm[1:50,-c(7,8)],cor = T)
biplot(pc)
pc
pc$scores
which.max(pc$scores[,1])
which.min(pc$scores[,1])

## fire effects
d1718 <- sm$sm2017-sm$sm2018
summary(d1718)
# Interpretation: soil moisture declined overall from 2017 to 2018, presumably reflecting change in rainfall; Larger positive values mean a greater decline

library("RCurl")
fs <- read.csv("https://raw.githubusercontent.com/dackerly/PepperwoodFireSeverity/master/data/FSextract/vegplots-54-20m-FS.csv")
head(fs)
tail(fs)
names(fs)
fscols <- 10:19
  
fsmet <- 'Tubbs.MTBS.RDNBR.30'
names(fs)
summary(fs[,fsmet])
fs[,fsmet]
hist(fs[,fsmet])
sort(fs[,fsmet])

# Plot fire severity vs. change in soil moisture
plot(fs[,fsmet],d1718)
cor(fs[,fsmet],d1718,use='pair')
fit=lm(d1718~fs[,fsmet])
anova(fit)
# Interpretation: negative correlation means that SM declined less in high severity plots - but it's not significant!. First order biological interpretation would be there is less root activity, so soils are relatively wetter; as opposed to the effect of more open canopies, which would be expected to dry out the soils and lead to a positive correlation on this analysis

# now try them all
i=10
for (i in fscols) {
  print(cor(fs[,i],d1718,use='pair'))
}
# Interpretation: across fire severity metrics, most are negative and not significant, while a few are weakly positive, and also NS