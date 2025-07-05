# Importar archivo de excel "Parte 1. IPC"

library(readxl)
datosipc = read_excel(file.choose(),sheet = 2)

# Definir un objeto como serie de tiempo con ts
IPC  = ts(datosipc$IPC,start = c(2000,1),frequency = 12)
plot(IPC)

library(aTSA) #Alternative time series analysis
adf.test(IPC,nlag = 15)
pp.test(IPC)
#Para ambas pruebas no rechazamos H0, por tanto hay
#Raiz unitaria

inf12=diff(log(IPC),lag = 12)   # se define inflacion 12 meses
inf12 = ts(inf12[time(inf12)>=2003],start = c(2003,1),frequency = 12)  #acotar serie
plot(inf12)  # no es estacionaria

inflacion = diff(log(IPC)) #primera diferencia logaritmica
plot(inflacion)
adf.test(inflacion,nlag = 15) #Rezago incluidos en la prueba
PP.test(inflacion)  # param. de rezago corto plazoPP.test(inflacion,lshort = F) # param. de rezago largo plazo

library(forecast)
#Funciones de Autocorrelaci?n Simple y Parcial
fas=Acf(inflacion)
cbind(Rezago=fas$lag,FAS=fas$acf) # se observa componente estacional
fap=Pacf(inflacion)
cbind(Rezago=fap$lag,FAP=fap$acf)
cbind(Rezago=fap$lag,FAS=fas$acf[-1],FAP=fap$acf)
corr.serial = function(serie,rezago.max){
        Estadistico=c()
        P.value=c()
        for (i in 1:rezago.max) {
                Estadistico[i]=Box.test(x = serie,type = "Ljung-Box",lag = i)$statistic
                P.value[i]=Box.test(x = serie,type = "Ljung-Box",lag = i)$p.value
        }
        print(cbind(Estadistico,P.value))
}
corr.serial(serie = inflacion,rezago.max = 20)


# Diferencia estacional logaritmica 12 meses
infl = diff(diff(log(IPC),lag = 12)) #Primera diferencia de la inflación
plot(infl)

# Estacionariedad
adf.test(infl)
PP.test(infl)

#FAS
fas=Acf(infl,lag.max = 20)
cbind(fas$lag,fas$acf) # MA: 1,2,3,4,5,12,13
#FAP
fap=Pacf(infl,lag.max = 20)   # AR: 1,12,13
cbind(fap$lag,fap$acf)
corr.serial(serie = infl,rezago.max = 20)

# Limitar serie para el periodo ene-2003/jun-2017
infl = ts(infl[time(infl)>=2003],start = c(2003,1),frequency = 12)
plot(infl)

# Estimaci?n
# 1. AR(1,11,12) MA(1,2,3,4,5,12,13)
modelo1=arima(x = infl,order = c(12,0,13),
              fixed = c(NA,rep(0,9),NA,NA,  # parte AR
                rep(NA,5),rep(0,6),NA,NA,  # parte MA
                NA),    # intercepto
              method="ML")
library(lmtest)
coeftest(modelo1)
summary(modelo1)

# Validaci?n de supuestos sobre los residuos
tsdiag(modelo1,gof.lag = 11)  # ruido blanco; no autocorrelación serial del error
#el 11 son los grados de libertad, coeficientes

Box.test(x = modelo1$residuals,lag = 12,type = "Ljung-Box",fitdf = 10)

autocorr.res = function(modelo,rezago.max,gl){
  Estadistico=c()
  P.value=c()
  P.value[1:gl]=NA
  for (i in 1:rezago.max) {
    Estadistico[i]=Box.test(x = resid(modelo),type = "Ljung-Box",lag = i,fitdf = gl)$statistic
    P.value[gl+i]=Box.test(x = resid(modelo),type = "Ljung-Box",lag = i+gl,fitdf = gl)$p.value
  }
  print("Prueba Ljung-Box sobre los residuos")
  return(cbind(Estadistico,P.value=P.value[1:rezago.max]))
}
autocorr.res(modelo = modelo1,rezago.max = 20,gl = 10)

library(tseries)
jarque.bera.test(x = resid(modelo1)) # Normalidad
ts.plot(infl,fitted(modelo1),col=1:2,lty=1:2)
plot(resid(modelo1))              # Outliers
abline(h = c(0,2*sd(resid(modelo1)),-2*sd(resid(modelo1))),
       lty=c(1,2,2),col=c(1,4,4))
arch.test(modelo1)      # Homocedasticidad, Prueba ARCH 

# Impulsos respuesta
a=modelo1$coef[1:13]
b=modelo1$coef[14:26]
imp.res <- arima.sim(n=24, model=list(ar=a, ma=b), innov=(1:30)==1, start.innov=rep(0,75))
plot(imp.res);abline(h = 0)

# Modelo 2: excluyendo const. y AR(12)
# 1. AR(1,11) MA(1,2,3,4,5,12,13) 
modelo2=arima(x = infl,order = c(11,0,13),include.mean = F,transform.pars = T,
              fixed = c(NA,rep(0,9),NA,  # parte AR
                        rep(NA,5),rep(0,6),NA,NA))  # parte MA
coeftest(modelo2)
summary(modelo2)

ts.plot(infl,fitted(modelo2),col=1:2,lty=c(1,2,2))
plot(resid(modelo2))              # Outliers
abline(h = c(0,2*sd(resid(modelo2)),-2*sd(resid(modelo2))),
       lty=c(1,2,2),col=c(1,4,4))
autocorr.res(modelo = modelo2,rezago.max = 20,gl = 9)


# Modelo 3: se incluye el AR(6)
# AR(1,6,11) MA(1,2,3,4,5,12,13) 
modelo3=arima(x = infl,order = c(11,0,13),include.mean = F,transform.pars = T,
              fixed = c(NA,rep(0,4),NA,rep(0,4),NA,  # parte AR
                        rep(NA,5),rep(0,6),NA,NA))  # parte MA
coeftest(modelo3)
summary(modelo3)
ts.plot(infl,fitted(modelo3),col=1:2,lty=1:2)
plot(resid(modelo3))              # Outliers
abline(h = c(0,2*sd(resid(modelo3)),-2*sd(resid(modelo3))),
       lty=c(1,2,2),col=c(1,4,4))
autocorr.res(modelo = modelo3,rezago.max = 20,gl = 10)

# Modelo 4, a?adiendo dummy para ago-2016 (outlier)
which.max(abs(resid(modelo3)))   # extraer la posici?n del residuo m?s grande....
as.yearmon(time(infl)[164])

d201608 = ifelse(as.yearmon(time(infl))=="Ago. 2016",1,0)
modelo4=arima(x = infl,order = c(11,0,13),include.mean = F,
              fixed = c(NA,rep(0,4),NA,rep(0,4),NA,  # parte AR
                        rep(NA,5),rep(0,6),NA,NA,  # parte MA
                        NA),    # Dummy
              method = "ML",xreg = d201608)
coeftest(modelo4)
summary(modelo4)
ts.plot(infl,fitted(modelo4),col=1:2,lty=1:2)
plot(resid(modelo4))              # Outliers
abline(h = c(0,2*sd(resid(modelo4)),-2*sd(resid(modelo4))),
       lty=c(1,2,2),col=c(1,4,4))
autocorr.res(modelo = modelo4,rezago.max = 20,gl = 10)

# Modelo 5, incorporando m?ltiples choque estructurales
library(strucchange)
f2=Fstats(IPC~1)
plot(f2)
as.yearmon(time(f2$Fstats)[which.max(f2$Fstats)])

bp1=breakpoints(IPC~1)
plot(bp1)
summary(bp1)
breaks=breakfactor(bp1)[37:210]
breaks
break200505=ifelse(breaks=="segment3",1,0)  # may 2005
break200802=ifelse(breaks=="segment4",1,0)  # feb 2008
break201105=ifelse(breaks=="segment5",1,0)  # may 2011
break201412=ifelse(breaks=="segment6",1,0)  # dic 2014
dummies=ts(cbind(d201608,break200505,break200802,break201105,break201412),
           start = 2003,frequency = 12)

modelo5=arima(x = infl,order = c(11,0,13),include.mean = F,
              fixed = c(NA,rep(0,4),NA,rep(0,4),NA,  # parte AR
                        rep(NA,5),rep(0,6),NA,NA,  # parte MA
                        rep(NA,5)),    # Dummy
              method = "ML",xreg = dummies)
modelo5=arima(x = infl,order = c(11,0,13),include.mean = F,
              fixed = c(NA,rep(0,4),NA,rep(0,4),NA,  # parte AR
                        rep(NA,5),rep(0,6),NA,NA,  # parte MA
                        rep(NA,3)),    # Dummy
              method = "ML",xreg = dummies[,-c(2,5)])
coeftest(modelo5)
summary(modelo5)

ts.plot(infl,fitted(modelo5),col=1:2,lty=1:2)
plot(resid(modelo5))              # Outliers
abline(h = c(0,2*sd(resid(modelo5)),-2*sd(resid(modelo5))),
       lty=c(1,2,2),col=c(1,4,4))
autocorr.res(modelo = modelo5,rezago.max = 20,gl = 10)

# Modelo 6: Se incluye SMA(12) 
# AR(11) MA(1,2,3,4) SMA(1)
modelo6=arima(x = infl,order = c(11,0,4),include.mean = F,
              seasonal = c(0,0,1),
              fixed = c(rep(0,10),NA,  # parte AR
                        rep(NA,4), # parte MA
                        NA,        # parte SMA
                        NA),  # parte dummies
              method = "ML",xreg = break200802)
coeftest(modelo6)
summary(modelo6)

ts.plot(infl,fitted(modelo6),col=1:2,lty=1:2)
plot(resid(modelo6))              # Outliers
abline(h = c(0,2*sd(resid(modelo6)),-2*sd(resid(modelo6))),
       lty=c(1,2,2),col=c(1,4,4))
autocorr.res(modelo = modelo6,rezago.max = 20,gl = 6)

# Modelo 7: Autom?tico
modelo7=auto.arima(diff(log(IPC)),max.p = 8,max.q = 8,max.P = 2,allowdrift = T,ic = "bic")
modelo7
coeftest(modelo7)
summary(modelo7)

ts.plot(diff(log(IPC)),fitted(modelo7),col=1:2,lty=1:2)
plot(resid(modelo7))              # Outliers
abline(h = c(0,2*sd(resid(modelo7)),-2*sd(resid(modelo7))),
       lty=c(1,2,2),col=c(1,4,4))
autocorr.res(modelo = modelo7,rezago.max = 20,gl = 5)

#------------------------------------------------

# Evaluaci?n de pron?sticos con promedio (modelo1)
# se pronostican los ?ltimos 6 datos dentro de la muestra

#1.Estimar el modelo excluyendo los ?ltimos 6 datos
m1_test=update(modelo1,x=infl[1:168])
#2.se hace un pron?stico dentro de la muestra de 6 periodos
infl_0=ts(predict(m1_test,n.ahead = 6,se.fit = F),start = c(2017,1),frequency = 12)
infl_0
ts.plot(infl,infl_0,col=1:2)
# el anterior es un pronostico de la Diferencia estacional logaritmica 12 meses
a.var.anual=function(pronostico){
  inf12_0=pronostico[1]+inf12[168]
  for (i in 2:6) {inf12_0[i]=pronostico[i]+inf12_0[i-1]}
  inf12_0=ts(inf12_0,start = c(2017),frequency = 12)
  return(inf12_0)}
ts.plot(inf12[169:174],a.var.anual(infl_0),col=1:2)

IPC=ts(IPC[time(IPC)>=2003],start = c(2003,1),frequency = 12)  #acotar serie
plot(IPC)

a.nivel=function(pronostico){
  inf12_0=pronostico[1]+inf12[168]
  for (i in 2:6) {inf12_0[i]=pronostico[i]+inf12_0[i-1]}
  inf12_0=ts(inf12_0,start = c(2017),frequency = 12)
  IPC_0=ts(exp(inf12_0)*IPC[157:162],start = 2017,frequency = 12)
  return(IPC_0)
}

IPC_0=a.nivel(infl_0)
ts.plot(IPC[169:174],IPC_0,col=1:2)

error1=IPC[169:174]-IPC_0
mean(error1)    # ME
sqrt(mean(error1^2))  #RMSE
mean(abs(error1))    # MAE
mean(error1/IPC[169:174])*100   # MPE
mean(abs(error1/IPC[169:174]))*100   # MAPE

#----
# Evaluaci?n de pron?sticos: Rolling window

# Estimaci?n de  modelos (cada uno con n mayor que el anterior)
which(as.yearmon(time(infl))=="Nov 2012")  # ultima observacion 1era estimacion
which(as.yearmon(time(infl))=="Dec 2016")  # ultima observacion 50va estimacion

evaluacion = function(modelo,ventana_inic,pron_adelante){
        modelos = lapply((nobs(modelo)-pron_adelante-ventana_inic+1):(nobs(modelo)-pron_adelante),
                         FUN = function(n) update(modelo,x=infl[1:n]))
        sesgo = matrix(nrow = ventana_inic,ncol = pron_adelante)
        for(i in 1:ventana_inic){sesgo[i,] = infl[(nobs(modelo)-pron_adelante-ventana_inic+1+i):
                                                          (nobs(modelo)-pron_adelante-ventana_inic+6+i)]-forecast(modelos[[i]],h=pron_adelante)$mean}
        indicadores = rbind(sesgo=colMeans(sesgo),MSE=rep(NA,pron_adelante),RMSE=rep(NA,pron_adelante),MAE=rep(NA,pron_adelante))
        for (i in 1:pron_adelante) {
                indicadores[2,i] = mean((sesgo[,i])^2)
                indicadores[3,i] = sqrt(mean((sesgo[,i])^2))
                indicadores[4,i] = mean(abs(sesgo[,i]))}
        return(indicadores)
}

modelos = lapply(119:168,FUN = function(n) update(modelo1,x=infl[1:n]))

sesgo = matrix(nrow = 50,ncol = 6)
for(i in 1:50){sesgo[i,] = infl[(119+i):(124+i)]-forecast(modelos[[i]],h=6)$mean}
indicadores = rbind(sesgo=colMeans(sesgo),MSE=rep(NA,6),RMSE=rep(NA,6),MAE=rep(NA,6))
for (i in 1:6) {
        indicadores[2,i] = mean((sesgo[,i])^2)
        indicadores[3,i] = sqrt(mean((sesgo[,i])^2))
        indicadores[4,i] = mean(abs(sesgo[,i]))}
print(indicadores,digits = 3)

modelos=update(modelo1,x=infl[1:124])
#----

# Pron?sticos:
# pron?stico de la inflaci?n a 12 meses
a.var.anual=function(pronostico){
  inf12_0=pronostico[1]+inf12[174]
  for (i in 2:6) {inf12_0[i]=pronostico[i]+inf12_0[i-1]}
  inf12_0=ts(inf12_0,start = c(2017,7),frequency = 12)
  return(inf12_0)}



library(forecast)
pron1=a.var.anual(forecast(modelo1,h = 6)$mean)
pron2=a.var.anual(forecast(modelo2,h = 6)$mean)
pron3=a.var.anual(forecast(modelo3,h = 6)$mean)
pron4=a.var.anual(forecast(modelo4,xreg = rep(0,6),h = 6)$mean)
pron5=a.var.anual(forecast(modelo5,xreg = matrix(0,nrow = 6,ncol = 3),h = 6)$mean)
pron6=a.var.anual(forecast(modelo6,xreg = rep(0,6),h = 6)$mean)
pron7=a.var.anual(forecast(modelo7,h = 6)$mean)

ts.plot(ts(inf12[169:174],end = c(2017,6),frequency = 12),
        pron1,pron2,pron3,pron4,pron5,pron6,pron7,
        col=1:8)
cbind(pron1,pron2,pron3,pron4,pron5,pron6,pron7)

# Intervalos de cofianza
# ejemplo para el modelo 3
pron3=a.var.anual(forecast(modelo3,h = 6,level = 0.95)$mean)
up3=a.var.anual(forecast(modelo3,h = 6,level = 0.95)$upper)
low3=a.var.anual(forecast(modelo3,h = 6,level = 0.95)$lower)

ts.plot(ts(inf12[169:174],end = c(2017,6),frequency = 12),
        pron3,up3,low3,
        col=c(1,4,2,2),lty=c(1,2,2,2))

# Fanchart
Pron_M3=forecast(modelo3,h = 6,level = c(0.30,0.60,0.9))
plot(Pron_M3,include = 6,showgap = F)
# el anterior es un fanchart de la primera diferencia de la inflaci?n anual 

# se tienen que pasar a variaciones anuales:
Pron_M3$x = inf12
Pron_M3$upper[,1]=a.var.anual(Pron_M3$upper[,1])
Pron_M3$upper[,2]=a.var.anual(Pron_M3$upper[,2])
Pron_M3$upper[,3]=a.var.anual(Pron_M3$upper[,3])
Pron_M3$mean=a.var.anual(Pron_M3$mean)
Pron_M3$lower[,1]=a.var.anual(Pron_M3$lower[,1])
Pron_M3$lower[,2]=a.var.anual(Pron_M3$lower[,2])
Pron_M3$lower[,3]=a.var.anual(Pron_M3$lower[,3])
plot(Pron_M3,include = 6,showgap = F)

