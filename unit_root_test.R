install.packages("xelatex")
install.packages(c("ggplot2",'RColorBrewer','tinytex'))
remotes::install_github("crsh/papaja", dependencies = TRUE)
install.packages('remotes')
install.packages('tinytex')
install.packages(tseries)
install.packages('tseries')
install.packages('normtest')
install.packages('tidyverse')
install.packages('car')
install.packages('mFilter')
install.packages('PMmisc')
install.packages('MTS')
install.packages('tsDyn')
install.packages('bvartools')
library(tidyverse)
library(lubridate)
library(tseries)
library(car)
library(urca)
library(normtest)
library(vars)
library(forecast)
library(nlme)
library(mFilter)
library(MTS)
library(tsDyn)
library(bvartools)
getwd()
Paridad<- read.csv(
  file = 'Datos_Paridad2.csv',
  stringsAsFactors = FALSE, 
  strip.white = TRUE,
  sep = ';'
)
ts(PPP, start =c(2002,1),end=c(2019,12),freq=12)
PPP <- Paridad[,!(names(PPP)== "T")]
head(PPP)
View(PPP)
summary(PPP)
attach(Paridad)
names(PPP)
class(PPP)

#Histogramas
hist(Inflacion_China,main = '',xlab = 'Rangos',
     ylab = 'Frecuencia',col = '#3361FF',breaks = 20,xlim = c(-0.02,0.1),
     ylim=c(0.0,50))
normtest::jb.norm.test(Inflacion_China)

hist(Inflacion_Peru,main = '',xlab = 'Rangos',
     ylab = 'Frecuencia',col = '#3361FF',breaks = 20,xlim = c(-0.02,0.08),
     ylim = c(0.0,40))

#Consultas
?hist
?`urca-class`
?`ur.pp-class`

#plot
niveles<-factor(Paridad$T)
?plot
#plot_tc
plot(Tipo_cambio,type = 'l',xlab = 'Meses',ylab = 'Valores',ylim=c(-0.15,0.2),
     col='#3361FF',lwd=2,xaxt='n',frame=FALSE)
axis(1,at=niveles, labels=niveles,las=2, cex.axis=0.5)
#plot_infp
plot(Inflacion_Peru,type = 'l',xlab = 'Meses',ylab = 'Valores',ylim = c(-0.02,0.08),
     col='#3361FF',lwd=2,xaxt='n',frame=FALSE)
axis(1,at=niveles, labels=niveles,las=2, cex.axis=0.5)
#plot_infch
plot(Inflacion_China,type = 'l',xlab = 'Meses',ylab = 'Valores',
     col='#3361FF',lwd=2,xaxt='n',frame=FALSE)
axis(1,at=niveles, labels=niveles,las=2, cex.axis=0.5)


#PRUEBAS ESTADISTICAS
#Tipo de cambio
##ADF
adf.tc<-ur.df(Tipo_cambio,type = 'drift',lags = 36,selectlags = 'BIC')
summary(adf.tc)
plot(adf.tc)

adf.tc<-ur.df(Tipo_cambio,type='trend',lags = 36,selectlags = 'BIC')
summary(adf.tc)
plot(adf.tc)

#ERS_tipo de cambio
#Constante
ers.tc<-ur.ers(Tipo_cambio,type = 'DF-GLS',model = 'constant',lag.max = 14)
summary(ers.tc)
plot(ers.tc)
#Constante y Tendencia
ers.tc<-ur.ers(Tipo_cambio, type ="P-test", model = c("constant", "trend"),lag.max = 13)
summary(ers.tc)
plot(ers.tc)

#PP_tipo de cambio
pp.tc<-ur.pp(Tipo_cambio,type = 'Z-tau',model = 'constant',lags = 'long')
pp.tc
summary(pp.tc)
pp.tc@cval
plot(pp.tc)

pp.tc<-ur.pp(Tipo_cambio,type = 'Z-tau',model ='trend',lags = 'long')
pp.tc
summary(pp.tc)
pp.tc@cval
plot(pp.tc)

#Primeras diferencias
#Tipo de cambio
#ADF
#Intercepto
adf.tc<-ur.df(diff(Tipo_cambio),type = 'drift',lags = 36,selectlags = 'BIC')
summary(adf.tc)
plot(adf.tc)
#Intercepto y Tendencia
adf.tc<-ur.df(diff(Tipo_cambio),type='trend',lags = 36,selectlags = 'BIC')
summary(adf.tc)
plot(adf.tc)
#URS
#Constante
ers.tc<-ur.ers(diff(Tipo_cambio),type = 'DF-GLS',model = 'constant',lag.max = 12)
summary(ers.tc)
plot(ers.tc)
#Constante y Tendencia
ers.tc<-ur.ers(diff(Tipo_cambio), type = "DF-GLS", model = c("constant", "trend")
               ,lag.max = 12)
summary(ers.tc)
plot(ers.tc)

#PP
#constante
pp.dtc<-ur.pp(diff(Tipo_cambio),type = 'Z-tau',model = 'constant',lags = 'long')
summary(pp.dtc)
pp.dtc@cval
plot(pp.dtc)
#constante e tendencia
pp.dtc<-ur.pp(diff(Tipo_cambio),type = 'Z-tau',model = c('trend'),lags = 'long')
summary(pp.dtc)
pp.dtc@cval
plot(pp.dtc)


#Diferencia Inflación
##ADF
adf.di<-ur.df(Dif_Inflacion,type = 'drift',lags = 36,selectlags = 'BIC')
summary(adf.di)
plot(adf.di)

adf.di<-ur.df(Dif_Inflacion,type='trend',lags=36,selectlags = 'BIC')
summary(adf.di)
plot(adf.di)

##ERS
#Constante
ers.di<-ur.ers(diff(DI),type = 'DF-GLS',model = 'constant')
summary(ers.di)
plot(ers.di)
?ur.ers
#Constante y Tendencia
ers.di<-ur.ers(Dif_Inflacion, type = "P-test", model = c("constant", "trend"))
summary(ers.di)
plot(ers.tc)

##PP
#Constante
pp.di<-ur.pp(Dif_Inflacion,type = 'Z-tau',model = 'constant',lags = 'long')
summary(pp.di)
pp.di@cval
plot(pp.tc)
#Constante y Tendencia
pp.di<-ur.pp(Dif_Inflacion,type = 'Z-tau',model = c('constant','trend'),
             lags = 'long')
summary(pp.di)
pp.di@cval
plot(pp.tc)

#Primeras diferencias
##ADF
adf.ddi<-ur.df(diff(Dif_Inflacion),type = 'drift',lags = 36,selectlags = 'BIC')
summary(adf.ddi)
plot(adf.ddi)

adf.ddi<-ur.df(diff(Dif_Inflacion),type='trend',lags = 36,selectlags = 'BIC')
summary(adf.ddi)
plot(adf.ddi)

##ERS
#Constante
ers.ddi.d<-ur.ers(diff(Dif_Inflacion),type = 'DF-GLS',model = 'constant',
                  lag.max = 24)
summary(ers.ddi.d)
plot(ers.ddi.d)

#Constante y Tendencia #DF-GLS
ers.ddi.d1<-ur.ers(diff(Dif_Inflacion), type = "DF-GLS",
                   model = c("constant", "trend"),lag.max = 24)
summary(ers.ddi.d1)
plot(ers.ddi.d1)


##PP
#Constante
pp.ddi<-ur.pp(diff(Dif_Inflacion),type = 'Z-tau',model = 'constant',
              lags = 'long')
summary(pp.ddi)
pp.ddi@cval
plot(pp.ddi)
#Constante y Tendencia
pp.ddi<-ur.pp(diff(Dif_Inflacion),type = 'Z-tau',
              model = c('trend'),lags = 'long')
summary(pp.ddi)
pp.ddi@cval
plot(pp.ddi)


#Generar Modelo
modelo1=lm(diff(Tipo_cambio)~diff(Dif_Inflacion))
summary(modelo1)
#Modelo1
diff(Tipo_cambio)=3.394+1.765*diff(Dif_Inflacion)
#Residuos
residuales=modelo1$residuals
summary(residuales)
residualPlot(modelo1)
#Con Tendencia
y=ur.df(residuales,type = 'trend',lags = 36,selectlags = 'BIC')
summary(y)
#Con Constante
y2=ur.df(residuales,type = 'drift',selectlags = 'AIC',lags = 36)
summary(y2)
#Con Constante y Tendencia
y3=ur.df(residuales,type = 'none',selectlags = 'AIC',lags = 36)
summary(y3)

adf.test(residuales)

#Prueba de Phillips y Ouliaris para Cointegración
prueba.PO=ca.po(dset,demean = 'none',type = 'Pz',lag = 'long')
summary(prueba.PO)
plot(prueba.PO)

#Genero Modelo de Correción de Errores
dTC<-diff(Tipo_cambio)
dDI<-diff(di_9)
modelo2=lm(dTC~dDI)
summary(modelo2)

#Genero los residuos
res2<-modelo2$residuals
res23<-lag(res2)
MCE=lm(dTC~dDI+res23)
summary(MCE)

vc<-VECM(dset,lag = 42,estim ='2OLS',r=1)
summary(vc)


#Johansen Test

#Load the Dataset
PPP<- read.csv(
  file = 'p2.csv',
  stringsAsFactors = FALSE, 
  strip.white = TRUE,
  sep = ';'
)
attach(PPP)
p1<-ts(PPP,start = c(2002,1),end = c(2019,12),frequency = 12)
p2<-ts(Paridad,start = c(2002,1),end = c(2019,12),frequency = 12)
ts.plot(p1[,'Tipo_cambio'],p1[,'Dif_Inflacion'],type='l',lty=c(1,2)
        ,col=c('#3361FF','#FF3333'),lwd=2)
legend('topleft',border = NULL,legend = c('TC','DI'),lty =c(1,2),
       col = c('#3361FF','#FF3333'))

ts.plot(p1[,'Tipo_cambio'],type='l',xlab = 'Años',ylab='Valores',
        ylim=c(-0.15,0.2),lty=1,col='#3361FF',lwd=2)
ts.plot(p1[,'Dif_Inflacion'],type='l',xlab = 'Años',ylab='Valores',
        ylim=c(-0.1,0.1),lty=1,col='#3361FF',lwd=2)

ts.plot(p2[,'Inflacion_China'],type='l',xlab = 'Años',ylab='Valores',
        ylim=c(-0.02,0.1),lty=1,col='#3361FF',lwd=2)


di_9<-lag(PPP$Dif_Inflacion,9)

TC<-ts(Tipo_cambio, start =c(2002,1),end=c(2019,12),freq=12)
DI<-ts(Dif_Inflacion, start =c(2002,1),end=c(2019,12),freq=12)

#Bind into a system
dset<-cbind(diff(TC),diff(DI))
dset


#Lag Selection criteria
dset1<-dset[1:215,]
lagselect<-VARselect(dset1,lag.max = 100,type = 'trend')
?VARselect
lagselect$selection


#Johansen test (trace)
ctest1<-ca.jo(dset1,K=57,type = 'trace',ecdet = 'trend',spec = 'longrun')
summary(ctest1)
ctest1
?ca.jo
#Johansen test (Maxeigen)
ctest2<-ca.jo(dset1,K=52,type = 'eigen',ecdet = 'trend',spec = 'longrun')
summary(ctest2)

#Estimación VEC
vecm1<-cajorls(ctest1,r=1)
vecm1
summary(vecm1$rlm)
?cajorls
vc<-VECM(dset,lag = 57,estim ='2OLS',r=1)


#Impulse Response Function
var<-vec2var()
