install.packages("xelatex")
install.packages(c("ggplot2",'RColorBrewer','tinytex'))
remotes::install_github("crsh/papaja", dependencies = TRUE)
install.packages('remotes')
install.packages('tinytex')
install.packages(tseries)
install.packages('tseries')
library(tseries)
library(urca)
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
#Histogramas
hist(Inflacion_China,main = '',xlab = 'Rangos',
     ylab = 'Frecuencia',col = '#3361FF',breaks = 20,xlim = c(-0.02,0.1),
     ylim=c(0.0,50))
#Consultas
?hist
?`urca-class`
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
#histogramas
hist(Inflacion_Peru,main = '',xlab = 'Rangos',
     ylab = 'Frecuencia',col = '#3361FF',breaks = 20,xlim = c(-0.02,0.08),
     ylim = c(0.0,40))
#PRUEBAS ESTADISTICAS
#DFA_tipo de cambio
adf.tc<-tseries::adf.test(Tipo_cambio,k=trunc(4))
plot(adf.tc)
#ERS_tipo de cambio
ers.tc<-ur.ers(Tipo_cambio)
summary(ers.tc)
plot(ers.tc)
#PP_tipo de cambio
pp.tc<-ur.pp(Tipo_cambio,type = 'Z-tau')
pp.tc
pp.tc@cval
plot(pp.tc)

ers.infp<-ur.ers(diff(Dif_Inflacion))
summary(ers.infp)
ers.ch<-ur.ers(diff(Inflacion_China))
summary(ers.ch)

ers.dif<-ur.ers(Dif_Inflacion,type ='DF-GLS',model = 'trend')
summary(ers.dif)
ers.tipocambio<-ur.ers(diff(Tipo_cambio),type ='DF-GLS',model = 'trend')
summary(ers.tipocambio)
plot(ers.tipocambio)
summary(ers.tipocambio)
kpss.tc<-ur.kpss(Dif_Inflacion)
summary(kpss.tc)
ur.kpss(Inflacion_China)
ur.kpss(Inflacion_Peru)
ur.kpss(Tipo_cambio,type ='tau')
tseries::adf.test(Tipo_cambio,k=trunc(4))
tseries::adf.test(diff(Tipo_cambio),k=trunc(4))
tseries::adf.test(Inflacion_Peru,k=trunc(4))
tseries::adf.test(diff(Inflacion_Peru),k=trunc(4))
tseries::adf.test(Inflacion_China,k=trunc(4))
tseries::adf.test(diff(Inflacion_China),k=trunc(4))
tseries::adf.test(Dif_Inflacion,k=trunc(4))
tseries::adf.test(diff(Dif_Inflacion),k=trunc(4))
#pp
pp.tc<-ur.pp(Inflacion_China,type = 'Z-tau')
pp.tc
pp.tc@cval
pp.tc<-ur.pp(diff(Dif_Inflacion),type = 'Z-tau')
pp.tc
pp.tc@cval
pp.infp<-ur.pp(diff(Inflacion_Peru))
summary(pp.infp)
pp.infc<-ur.pp(diff(Inflacion_China))
summary(pp.infc)
?ur.pp
`ur.pp-class`