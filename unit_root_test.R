install.packages("xelatex")
install.packages(c("ggplot2",'RColorBrewer','tinytex'))
remotes::install_github("crsh/papaja", dependencies = TRUE)
install.packages('remotes')
install.packages('tinytex')
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
hist(Inflacion_China,main = '',xlab = 'Rangos',
     ylab = 'Frecuencia',col = '#3361FF',breaks = 20,xlim = c(-0.02,0.1),
     ylim=c(0.0,50))
?hist
ur.ers(Inflacion_China)
?`urca-class`
hfghuhgjdksgndn
writeLines('PATH="${RTOOLS40_HOME}\\usr\\bin;${PATH}"', con = "~/.Renviron")
Sys.which("make")
## "C:\\rtools40\\usr\\bin\\make.exe"
install.packages("jsonlite", type = "source")
if(!"devtools" %in% rownames(installed.packages())) install.packages("devtools")
devtools::install_github("crsh/papaja")
#plot
plot(Tipo_cambio,type = 'l',xlab = 'Rangos',ylab = 'Valores',col='#3361FF')
plot(Inflacion_China,type = 'l',xlab = 'Rangos',ylab = 'Valores',col='#3361FF')
plot(Inflacion_Peru,type = 'l',xlab = 'Rangos',
     ylab = 'Valores',col='#3361FF',ylim = c(0.00,0.08),xlim = c(0.0,200))
#histogramas
hist(Inflacion_Peru,main = '',xlab = 'Rangos',
     ylab = 'Frecuencia',col = '#3361FF',breaks = 20,xlim = c(-0.02,0.08),
     ylim = c(0.0,40))
ur.ers(Tipo_cambio)
ur.ers(Inflacion_Peru)
ur.ers(Inflacion_China)
ers.tipocambio<-ur.ers(Tipo_cambio,type ='DF-GLS',model = 'trend')
summary(ers.tipocambio)
ur.kpss(Tipo_cambio)
ur.pp(Tipo_cambio)
ers.tipocambio<-ur.ers(Tipo_cambio,type ='DF-GLS',model = 'trend',
                       lag.max = 1)
summary(ers.tipocambio)