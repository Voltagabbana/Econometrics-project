library(foreign) #Or you can use require(foreign)
library(xts)
library(ggplot2)
library(stargazer)
library(BatchGetSymbols)

setwd("C:/Users/enduser/OneDrive - Politecnico di Milano/Ingegneria matematica/4.2.Insurance and economics own/Econometrics/Progetto")


# Questo codice di stocco permette di scaricare le quotazioni dei titoli da yahoo finance
# Gold (GC=F), NASDAQ 100 (^NDX), OIL(CL=F)

first.date <- "2008-06-03"
last.date <- "2022-04-25"

#  GSPC

tickers <- c('^GSPC')
l.out <- BatchGetSymbols(tickers = tickers,
                         first.date = first.date,
                         last.date = last.date,
                         cache.folder = file.path(tempdir(),
                                                  'BGS_Cache') )
str(l.out)
x= l.out$df.tickers$ref.date
y = l.out$df.tickers$price.close
GSPC = data.frame(x,y)

#  OVX

tickers <- c('^OVX')
l.out <- BatchGetSymbols(tickers = tickers,
                         first.date = first.date,
                         last.date = last.date,
                         cache.folder = file.path(tempdir(),
                                                  'BGS_Cache') )
str(l.out)
x= l.out$df.tickers$ref.date
y = l.out$df.tickers$price.close
OVX = data.frame(x,y)

#  GVZ

tickers <- c('^GVZ')
l.out <- BatchGetSymbols(tickers = tickers,
                         first.date = first.date,
                         last.date = last.date,
                         cache.folder = file.path(tempdir(),
                                                  'BGS_Cache') )
str(l.out)
x= l.out$df.tickers$ref.date
y = l.out$df.tickers$price.close
GVZ = data.frame(x,y)

# OIL

tickers <- c('CL=F')
l.out <- BatchGetSymbols(tickers = tickers,
                         first.date = first.date,
                         last.date = last.date,
                         cache.folder = file.path(tempdir(),
                                                  'BGS_Cache') )
str(l.out)
x= l.out$df.tickers$ref.date
y = l.out$df.tickers$price.close
CL_F = data.frame(x,y)

# GOLD

tickers <- c('GC=F')
l.out <- BatchGetSymbols(tickers = tickers,
                         first.date = first.date,
                         last.date = last.date,
                         cache.folder = file.path(tempdir(),
                                                  'BGS_Cache') )
str(l.out)
x= l.out$df.tickers$ref.date
y = l.out$df.tickers$price.close
GC_F = data.frame(x,y)

### fine codice Stocco

# Problema dati mancanti


## NB:Carico i dati per non doverli riscaricare ogni volta!!

load("C:/Users/enduser/OneDrive - Politecnico di Milano/Ingegneria matematica/4.2.Insurance and economics own/Econometrics/Progetto/DatiSerieMarci.RData")

j = 0
for (i in 1:3497)
{
if(CL_F$x[i]!=GVZ$x[i])
  j = append(j,i)
}
# ho trovato che il dato mancante è a riga 2105

j = 0

for (i in 2106:3497)
{
  if(GC_F$x[i]!=GVZ$x[i+1])
    j = append(j,i)
}
# ho trovato che il dato mancante è a riga 2105 e 2128

# modifico CL_F
CL_Fnew = CL_F[1:2104,]
y = (CL_F[2104,2]+CL_F[2105,2])/2
x = as.Date("2016-10-10")
CL_F2005 = data.frame(x,y)
CL_Fnew = rbind(CL_Fnew,CL_F2005)
CL_Ftail = CL_F[2105:3498,]
CL_Fnew = rbind(CL_Fnew,CL_Ftail)

rownames(CL_Fnew)<-1:nrow(CL_Fnew)

# modifico GC_F
GC_Fnew = GC_F[1:2104,]
y = (GC_F[2104,2]+GC_F[2105,2])/2
x = as.Date("2016-10-10")
GC_F2005 = data.frame(x,y)
GC_Fnew = rbind(GC_Fnew,GC_F2005)
GC_Ftail = GC_F[2105:2127,]
GC_Fnew = rbind(GC_Fnew,GC_Ftail)

y = (GC_F[2127,2]+GC_F[2128,2])/2
x = as.Date("2016-11-11")
GC_F2128 = data.frame(x,y)
GC_Fnew = rbind(GC_Fnew,GC_F2128)
GC_Ftail = GC_F[2128:3497,]
GC_Fnew = rbind(GC_Fnew,GC_Ftail)

rownames(GC_Fnew)<-1:nrow(GC_Fnew)







