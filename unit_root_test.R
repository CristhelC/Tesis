install.packages("xelatex")
install.packages(c("ggplot2",'RColorBrewer','tinytex'))
remotes::install_github("crsh/papaja", dependencies = TRUE)
install.packages('remotes')
install.packages('tinytex')
install.packages(tseries)
install.packages('tseries')
install.packages('normtest')
library(tseries)
library(urca)
library(normtest)
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
normtest::jb.norm.test(Inflacion_China)

hist(Inflacion_Peru,main = '',xlab = 'Rangos',
     ylab = 'Frecuencia',col = '#3361FF',breaks = 20,xlim = c(-0.02,0.08),
     ylim = c(0.0,40))

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
adf.dif.inf<-tseries::adf.test(Dif_Inflacion,k=trunc(4))
plot(adf.dif.inf)
#ERS_Diff_Inflacion
ers.dif.inf<-ur.ers(Dif_Inflacion)
summary(ers.dif.inf)
plot(ers.dif.inf)
#PP_Diff_Inflacion
pp.dif.inf<-ur.pp(Dif_Inflacion,type = 'Z-tau')
pp.dif.inf
pp.dif.inf@cval
plot(pp.dif.inf)

##Primeras Diferencias
#ERS1_inflacion_china
ers1.infch<-ur.ers(diff(Inflacion_China))
summary(ers1.infch)
plot(ers1.infch)
#PP1_inflacion_china
pp1.infch<-ur.pp(diff(Inflacion_China),type = 'Z-tau')
pp1.infch
pp1.infch@cval
plot(pp1.infch)


ers1.infch<-ur.ers(diff(Inflacion_China))
summary(ers.ch)
ers.dif<-ur.ers(Dif_Inflacion,type ='DF-GLS',model = 'trend')
summary(ers.dif)
ers.tipocambio<-ur.ers(diff(Tipo_cambio),type ='DF-GLS',model = 'trend')
summary(ers.tipocambio)
plot(ers.tipocambio)
summary(ers.tipocambio)


?`ur.pp-class`