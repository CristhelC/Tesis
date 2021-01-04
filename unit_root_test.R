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
library(tidyverse)
library(lubridate)
library(tseries)
library(car)
library(urca)
library(normtest)
library(vars)
library(forescast)
library(nlme)
library(mFilter)
getwd()
Paridad<- read.csv(
  file = 'Datos_Paridad.csv',
  stringsAsFactors = FALSE, 
  strip.white = TRUE,
  sep = ';'
)
head(Paridad)
View(Paridad)
summary(Paridad)
attach(Paridad)

Paridad2<- read.csv(
  file = 'Datos_Paridad2.csv',
  stringsAsFactors = FALSE, 
  strip.white = TRUE,
  sep = ';'
)
head(Paridad2)
View(Paridad2)
summary(Paridad2)
attach(Paridad2)
names(Paridad2)
class(Paridad2)

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
#instalar papaja
writeLines('PATH="${RTOOLS40_HOME}\\usr\\bin;${PATH}"', con = "~/.Renviron")
Sys.which("make")
## "C:\\rtools40\\usr\\bin\\make.exe"
install.packages("jsonlite", type = "source")
if(!"devtools" %in% rownames(installed.packages())) install.packages("devtools")
devtools::install_github("crsh/papaja")
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
adf.tc<-ur.df(Tipo_cambio,type = 'drift',selectlags = 'AIC')
summary(adf.tc)
plot(adf.tc)

adf.tc<-ur.df(Tipo_cambio,type='none',selectlags = 'AIC')
summary(adf.tc)
plot(adf.tc)

#ERS_tipo de cambio
#Constante
ers.tc<-ur.ers(Tipo_cambio,type = 'DF-GLS',model = 'constant')
summary(ers.tc)
plot(ers.tc)
#Constante y Tendencia
ers.tc<-ur.ers(Tipo_cambio, type = "DF-GLS", model = c("constant", "trend"))
summary(ers.tc)
plot(ers.tc)

#PP_tipo de cambio
pp.tc<-ur.pp(Tipo_cambio,type = 'Z-tau')
pp.tc
pp.tc@cval
plot(pp.tc)

#Primeras diferencias
#Tipo de cambio
#Intercepto
adf.tc<-ur.df(diff(Tipo_cambio),type = 'drift',selectlags = 'AIC')
summary(adf.tc)
plot(adf.tc)
#Intercepto y Tendencia
adf.tc<-ur.df(diff(Tipo_cambio),type='none',selectlags = 'AIC')
summary(adf.tc)
plot(adf.tc)
#Constante
ers.tc<-ur.ers(diff(Tipo_cambio),type = 'DF-GLS',model = 'constant')
summary(ers.tc)
plot(ers.tc)
#Constante y Tendencia
ers.tc<-ur.ers(diff(Tipo_cambio), type = "DF-GLS", model = c("constant", "trend"))
summary(ers.tc)
plot(ers.tc)

#PP
#constante
pp.dtc<-ur.pp(diff(Tipo_cambio),type = 'Z-tau',model = 'constant')
pp.dtc
pp.dtc@cval
plot(pp.dtc)
#constante e tendencia
pp.dtc<-ur.pp(diff(Tipo_cambio),type = 'Z-tau',model = c('constant','trend'))
pp.dtc
pp.dtc@cval
plot(pp.dtc)


#Diferencia Inflación
##ADF
adf.di<-ur.df(Dif_Inflacion,type = 'drift',selectlags = 'AIC')
summary(adf.di)
plot(adf.di)

adf.di<-ur.df(Dif_Inflacion,type='none',selectlags = 'AIC')
summary(adf.di)
plot(adf.di)

##ERS
#Constante #DF-GLS
ers.di<-ur.ers(Dif_Inflacion,type = 'DF-GLS',model = 'constant')
summary(ers.di)
#Constante #P-Test
ers.di<-ur.ers(Dif_Inflacion,type = 'P-test',model = 'constant')
summary(ers.di)

#Constante y Tendencia #DF-GLS
ers.di<-ur.ers(Dif_Inflacion, type = "DF-GLS", model = c("constant", "trend"))
summary(ers.di)
plot(ers.tc)
#Constante y Tendencia #P-Test
ers.di<-ur.ers(Dif_Inflacion, type = "P-test", model = c("constant", "trend"))
summary(ers.di)
plot(ers.tc)

##PP
#Constante
pp.di<-ur.pp(Dif_Inflacion,type = 'Z-tau',model = 'constant')
pp.di
pp.di@cval
plot(pp.tc)
#Constante y Tendencia
pp.di<-ur.pp(Dif_Inflacion,type = 'Z-tau',model = c('constant','trend'))
pp.di
pp.di@cval
plot(pp.tc)

#Primeras diferencias
##ADF
adf.ddi<-ur.df(diff(Dif_Inflacion),type = 'drift',selectlags = 'AIC')
summary(adf.ddi)
plot(adf.ddi)

adf.ddi<-ur.df(diff(Dif_Inflacion),type='none',selectlags = 'AIC')
summary(adf.ddi)
plot(adf.ddi)

##ERS
#Constante #DF-GLS
ers.ddi.d<-ur.ers(diff(Dif_Inflacion),type = 'DF-GLS',model = 'constant')
summary(ers.ddi.d)
plot(ers.ddi.d)

#Constante #P-Test
ers.ddi.p<-ur.ers(diff(Dif_Inflacion),type = 'P-test',model = 'constant')
summary(ers.ddi.p)
plot(ers.ddi.p)

#Constante y Tendencia #DF-GLS
ers.ddi.d1<-ur.ers(diff(Dif_Inflacion), type = "DF-GLS", model = c("constant", "trend"))
summary(ers.ddi.d1)
plot(ers.ddi.d1)
#Constante y Tendencia #P-Test
ers.ddi.p1<-ur.ers(diff(Dif_Inflacion), type = "P-test", model = c("constant", "trend"))
summary(ers.ddi.p1)
plot(ers.ddi.p1)

##PP
#Constante
pp.ddi<-ur.pp(diff(Dif_Inflacion),type = 'Z-tau',model = 'constant')
pp.ddi
pp.ddi@cval
plot(pp.ddi)
#Constante y Tendencia
pp.ddi<-ur.pp(diff(Dif_Inflacion),type = 'Z-tau',model = c('constant','trend'))
pp.ddi
pp.ddi@cval
plot(pp.ddi)



#DFA_inflacion_peru
adf.infpe<-tseries::adf.test(Inflacion_Peru,k=trunc(4))
plot(adf.infpe)
#ERS_inflacion_peru
ers.infpe<-ur.ers(Inflacion_Peru)
summary(ers.infpe)
plot(ers.infpe)
#PP_inflacion_peru
pp.infpe<-ur.pp(Inflacion_Peru,type = 'Z-tau')
pp.infpe
pp.infpe@cval
plot(pp.infpe)


#DFA_inflacion_china
adf.infch<-tseries::adf.test(Inflacion_China,k=trunc(4))
plot(adf.infch)
#ERS_inflacion_china
ers.infch<-ur.ers(Inflacion_China)
summary(ers.infch)
plot(ers.infch)
#PP_inflacion_china
pp.infch<-ur.pp(Inflacion_China,type = 'Z-tau')
pp.infch
pp.infch@cval
plot(pp.infch)

#DFA_Diff_Inflacion
adf.dif.inf<-ur.df(Dif_Inflacion,type = 'drift')
summary(adf.dif.inf)
plot(adf.dif.inf)

adf.dif.inf<-ur.df(Dif_Inflacion,type='trend',selectlags = 'AIC')
summary(adf.dif.inf)
plot(adf.tc)

adf.dif.inf<-ur.df(Dif_Inflacion,type='none',selectlags = 'AIC')
summary(adf.dif.inf)
plot(adf.tc)

#Primeras diferencias para diferencia de inflación
adf.dd.inf<-ur.df(diff(Dif_Inflacion),type = 'drift',selectlags = 'AIC')
summary(adf.dd.inf)
plot(adf.tc)

#ERS_Dif_inflación
ers.dif.inf<-ur.ers(Dif_Inflacion)
summary(ers.tc)

ers.dif.inf<-ur.ers(Dif_Inflacion, type = "DF-GLS", model = c("constant", "trend"))
summary(ers.dif.inf)
plot(ers.tc)

ers.dif.inf<-ur.ers(Dif_Inflacion, type = "P-test", model = c("constant", "trend") )
summary(ers.dif.inf)
plot(ers.tc)

#PP_Diff_Inflacion
pp.dif.inf<-ur.pp(Dif_Inflacion,type = 'Z-tau')
pp.dif.inf
pp.dif.inf@cval
plot(pp.dif.inf)

##Primeras Diferencias
ers.dif.inf<-ur.ers(diff(Dif_Inflacion),type ='DF-GLS')
summary(ers.dif.inf)

ers1.infch<-ur.ers(diff(Inflacion_China))
summary(ers.ch)
ers.dif<-ur.ers(Dif_Inflacion,type ='DF-GLS')
summary(ers.dif)
ers.tipocambio<-ur.ers(diff(Tipo_cambio),type ='DF-GLS',model = 'trend')
summary(ers.tipocambio)
plot(ers.tipocambio)
summary(ers.tipocambio)

#Generar Modelo
modelo1=lm(diff(Tipo_cambio)~diff(Dif_Inflacion))
summary(modelo1)
#Modelo1
Tipo_cambio=3.394+1.765(Dif_Inflacion)
#Residuos
residuales=modelo1$residuals
summary(residuales)
residualPlot(modelo1)
#Con Tendencia
y=ur.df(residuales,type = 'trend',selectlags = 'AIC')
summary(y)
#Con Constante
y2=ur.df(residuales,type = 'drift',selectlags = 'AIC')
summary(y2)
#Con Constante y Tendencia
y3=ur.df(residuales,type = 'none',selectlags = 'AIC')
summary(y3)

#Prueba de Phillips y Oularis para Cointegración
prueba.PO=ca.po(Paridad2,type = 'Pz')
summary(prueba.PO)
plot(prueba.PO)

#Prueba de Cointegración de Johansen
TParidad=cbind(diff(Tipo_cambio),diff(Dif_Inflacion))

#Seleccionar retardos apropiados
lagselect<-VARselect(TParidad,lag.max = 10,type = 'const')
lagselect$selection
#2 retardos es lo óptimo

#Johansen Test
cTest<-ca.jo(TParidad,type = 'trace',ecdet = 'const',K=2)
summary(cTest)

##again
TC=ts(Tipo_cambio,start = c(2002,1),end = c(2019,12),frequency = 12)
DI=ts(Dif_Inflacion,start = c(2002,1),end = c(2019,12),frequency = 12)
Datos.j=cbind(TC,DI)
print(Datos.j)
plot(Datos.j,xlab = 'Años',main = '',
     col='#3361FF',lwd=2)
axis(1,at=niveles, labels=niveles,las=2, cex.axis=0.5)

VARselect(Datos.j,lag.max = 10,type = 'const')
VARselect(Datos.j,lag.max = 10,type = 'const')$selection

#Generamos un modelo de regresión
modelo2=lm(TC~DI,data = Datos.j)
summary(modelo2)

#Modelo2 quedaría
TC=0.006075+1.090768(DI)

residuales=modelo2$residuals
plot(residuales)
residualPlot(modelo2)
