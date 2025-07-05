############################################################################
#
# MODELOS VAR EN R
#
# Macroeconometría, Universidad Externado
# Carlos Velásquez, Federico Medina
#
#---------------------------------------------------------------------------
#
# Encadenamientos entre ramas económicas a 2017 y pronósticos
#
############################################################################


# Instalar y activar paquete que permite leer archivos de EViews y otros formatos (binarios)
library(hexView)

# Importar datos
datos = readEViews(file.choose())
View(datos)
attach(datos)

# Definir como serie de tiempo el PIB de los sectores
Comercio = ts(COMERCIO[-(71:72)],start = c(2000,1),frequency = 4)
Industria = ts(INDUSTRIAS[-(71:72)],start = c(2000,1),frequency = 4)
Financiero = ts(FINANCIEROS[-(71:72)],start = c(2000,1),frequency = 4)
Transporte = ts(TRANSPORTE[-(71:72)],start = c(2000,1),frequency = 4)

# Gráfico de las series
ts.plot(Financiero,Comercio,Industria,Transporte,col=1:4,main="Series en niveles")
legend(legend = c("Financiero","Comercio","Industria","Transporte"),
       "topleft",lty = 1,col = 1:4)

# Gráfico estético con la librería ggplot2
library(ggplot2)
# Reorganizar datos, todos los valores en una sola columna
datos.graf = data.frame(fecha=rep(time(Financiero),4),
                        valor=c(Financiero,Comercio,Industria,Transporte),
                        Sector=c(rep("Financiero",70),rep("Comercio",70),rep("Industria",70),rep("Transporte",70)))
ggplot(datos.graf,mapping = aes(x = fecha,y = valor))+
  geom_line(aes(color = Sector),size=1)+
  scale_color_manual(values = c("#00AFBB","#E7B800","#FC4E07","blue"))+
  ggtitle("Series en niveles")+theme_light()

# Todas las series estÃ¡n en niveles y muestran una tendencia. 
# Estimar un VAR en niveles no es erróneo, pues no se hará inferencia estadística sobre los coeficientes
# Sin embargo,  al obtener los impulsos respuesta la interpretación se hará compleja
# Es mejor convertirlas a estacionarias


## Estacionariedad
# Diferencias logarítmicas de las series
dlComercio = diff(log(Comercio))
dlIndustria = diff(log(Industria))
dlFinanciero = diff(log(Financiero))
dlTransporte = diff(log(Transporte))
plot(cbind(dlFinanciero,dlComercio,dlIndustria,dlTransporte),
     nc=2,main="Diferencia logarítmica de las series")
# aparentemente son estacionarias, corroborarlo para Financiero
library(tseries)
pp.test(dlFinanciero,type = "Z(t_alpha)",lshort = T) # parámetro de truncamiento corto
pp.test(dlFinanciero,type = "Z(t_alpha)",lshort = F) # parámetro de truncamiento largo
# se rechaza hipótesis nula de existencia de raíz unitaria, es estacionaria


## Identificación del VAR, determinación de rezagos óptimos
library(vars)
# Utilizando los criterios de información (parsimonia)
VARselect(y = cbind(dlFinanciero,dlComercio,dlIndustria,dlTransporte),
          type = "const")   # se incluye una constante como exógena
# 3 criterios indican 1 rezago, el AIC sugiere 10


## Estimación
VAR1 = VAR(y = cbind(dlFinanciero,dlComercio,dlIndustria,dlTransporte),
           type = "const",p = 1)
summary(VAR1) # los coeficientes no se interpretan, es el resultado del VAR en forma reducida


## Estabilidad del sistema, raíces del polinomio característico
roots(VAR1,modulus = F) # raíces
roots(VAR1) # módulo
plot(roots(VAR1,modulus = F),xlim=c(-1.1,1.1),ylim=c(-1.1,1.1),asp=1)
lines(sin(seq(-6,6,0.001)),cos(seq(-6,6,0.001)))


## Evaluar autocorrelación serial de los errores (ruido blanco)
serial.test(VAR1,lags.pt = 4,type = "PT.asymptotic")
# Prueba Portmanteau, se crea una función para resumir resultados
Portmanteau.var = function(modelo,rezago.max){
  Estad=c()
  p.valor=c()
  Estad.ajus=c()
  p.valor.=c()
  for (i in 1:rezago.max) {
    Estad[i]=serial.test(x = modelo,lags.pt = i,type = "PT.asymptotic")$serial$statistic
    p.valor[i]=serial.test(x = modelo,lags.pt = i,type = "PT.asymptotic")$serial$p.value
    Estad.ajus[i]=serial.test(x = modelo,lags.pt = i,type = "PT.adjusted")$serial$statistic
    p.valor.[i]=serial.test(x = modelo,lags.pt = i,type = "PT.adjusted")$serial$p.value
  }
  resultado=cbind(Lag=1:rezago.max,Estad,p.valor,Estad.ajus,p.valor.)
  print("Prueba Portmanteau")
  return(resultado)
}  # crear función
Portmanteau.var(modelo = VAR1,rezago.max = 8)
# no se debe observar el resultado del rezago equivalente al orden del modelo (1)
# la prueba sugiere ausencia de autocorrelación serial

# Prueba Breush Godfrey LM, se crea una función para resumir resultados
LMBreush.var = function(modelo,rezago.max){
  Estadistico=c()
  p.valor=c()
  for (i in 1:rezago.max) {
    Estadistico[i]=serial.test(x = modelo,lags.bg = i,type = "BG")$serial$statistic
    p.valor[i]=serial.test(x = modelo,lags.bg = i,type = "BG")$serial$p.value
  }
  resultado=cbind(Lag=1:rezago.max,Estadistico,p.valor)
  print("Prueba LM Breush Godfrey")
  return(resultado)
}  # crear funciÃ³n
LMBreush.var(modelo = VAR1,rezago.max = 8)
# Esta prueba sugiere autocorrelación de orden 3


## Evaluar normalidad
normality.test(VAR1)  # Únicamente resultados multivariados
normality.test(VAR1,multivariate.only = F)  # resultados ubivariados para JB
# no se rechaza la hipótesis nula de normalidad (p.valor del JB=0.89)


## Impulsos respuesta
plot(irf(VAR1,n.ahead = 10))

# Impulsos respuesta; choques en el sector financiero
plot(irf(VAR1,n.ahead = 10,impulse = "dlFinanciero"))
# Impulsos respuesta; choques en el sector comercio
plot(irf(VAR1,n.ahead = 10,impulse = "dlComercio",
         response = c("dlComercio","dlIndustria","dlTransporte")))
# Impulsos respuesta; choques en el sector industria
plot(irf(VAR1,n.ahead = 10,impulse = "dlIndustria",
         response = c("dlIndustria","dlTransporte")))
# Impulsos respuesta; choques en el sector transporte
plot(irf(VAR1,n.ahead = 10,impulse = "dlTransporte",
         response = c("dlTransporte")))

## Impulsos respuesta acumulados; choques en el sector comercio
plot(irf(VAR1,n.ahead = 10,cumulative = T,impulse = "dlComercio",  # se agrega el argumento cumulative=TRUE
         response = c("dlComercio","dlIndustria","dlTransporte")))


## DescomposiciÃ³n de la varianza del error de pronóstico
# "forecast error variance decomposition"
fevd(VAR1,n.ahead = 10)

fevd(VAR1,n.ahead = 10)$dlIndustria*100 # Descomposición de varianza de la industria
fevd(VAR1,n.ahead = 10)$dlTransporte*100 # Descomposición de varianza del transporte

## Descomposición histórica
library(svars)
library(reshape2)
desc=hd(id.dc(VAR1),series = 4)$hidec
plot(hd(id.dc(VAR1),series = 4))

desc=ts(desc1,start=c(2000,3),frequency=4)
desc1 = data.frame(Fecha=time(desc),
                   Financiero=dlTransporte[-1]*desc[,3]/desc[,2],
                   Comercio=dlTransporte[-1]*desc[,4]/desc[,2],
                   Industria=dlTransporte[-1]*desc[,5]/desc[,2],
                   Transporte=dlTransporte[-1]*desc[,6]/desc[,2])
desc1=melt(desc1,id.vars = 1)
names(desc1)=c("Fecha","Sector","Y")
trans=data.frame(Fecha=time(desc),Y=dlTransporte[-1],Sector=rep("(%)dlTransporte",68))
ggplot(desc1,aes(x = Fecha,y = Y,fill=Sector))+
  geom_bar(stat = "identity",position = "stack")+
  geom_line(mapping = aes(x = Fecha,y = Y),data = trans,lwd=1.2)
#Este gráfico es clave para evaluar determinantes de la inflación
#Minuto 17 de la segunda parte

## Causalidad de Granger
# Conjunta
causality(VAR1,cause = c("dlComercio","dlIndustria","dlTransporte"))$Granger
causality(VAR1,cause = c("dlFinanciero","dlIndustria","dlTransporte"))$Granger
causality(VAR1,cause = c("dlFinanciero","dlComercio","dlTransporte"))$Granger
causality(VAR1,cause = c("dlFinanciero","dlComercio","dlIndustria"))$Granger
#estas pruebas son importantes. min 18:27 de la segunda parte
# Individual
grangertest(dlComercio~dlIndustria,order = 1) #Industria causa a comercio es lo que decimos
grangertest(dlIndustria~dlComercio,order = 1)
grangertest(dlTransporte~dlComercio,order = 1)


# Pronósticos
predict(VAR1,n.ahead = 2)
plot(predict(VAR1,n.ahead = 2),nc=2)
x11()
