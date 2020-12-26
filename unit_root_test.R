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
     ylab = 'Frecuencia',col = '#336BFF',breaks = 30,xlim = c(-0.02,0.1))
?hist
ur.ers(Inflacion_China)
?`urca-class`
hfghuhgjdksgndn