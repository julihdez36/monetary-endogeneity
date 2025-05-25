
# Librerias ---------------------------------------------------------------

library(tidyverse)
library(lubridate)

# Directorio e importación de base

library(readxl)
Consolidado <- read_excel("C:/Users/Julian/Desktop/Investigación/Datos/Consolidado.xlsx", 
                          sheet = "Consolidado dos")
colnames(Consolidado)<-c("mes","cartera","bm","m1","m2","m3","m","tib","infla_ipc",
                         "infla_ipp","i_activa","dtf","reservas","trm")
library(DescTools)
Abstract(Consolidado)

Consolidado<-Consolidado %>% 
  filter(complete.cases(cartera))
Consolidado<-Consolidado[,c("mes","cartera","m2","infla_ipc","i_activa")]
class(Consolidado$mes)
Consolidado$mes<-as.character(Consolidado$mes)

which(Consolidado$mes=="1990-08-01")
which(Consolidado$mes=="1996-05-01")
which(Consolidado$mes=="2005-06-01")
which(Consolidado$mes=="2011-03-01")
which(Consolidado$mes=="2016-12-01")

Consolidado$numeracion<-1:465
Consolidado$corte1<-ifelse(Consolidado$numeracion>=which(Consolidado$mes=="1990-08-01") &
                           Consolidado$numeracion< which(Consolidado$mes=="1996-05-01"),
                           1,0)
Consolidado$corte2<-ifelse(Consolidado$numeracion>=which(Consolidado$mes=="1996-05-01") &
                             Consolidado$numeracion< which(Consolidado$mes=="2005-08-01"),
                           1,0)
Consolidado$corte3<-ifelse(Consolidado$numeracion>=which(Consolidado$mes=="2005-08-01") &
                             Consolidado$numeracion< which(Consolidado$mes=="2011-03-01"),
                           1,0)
Consolidado$corte4<-ifelse(Consolidado$numeracion>=which(Consolidado$mes=="2011-03-01") &
                             Consolidado$numeracion< which(Consolidado$mes=="2016-12-01"),
                           1,0)
Consolidado$corte5<-ifelse(Consolidado$numeracion>= which(Consolidado$mes=="2016-12-01"),1,0)
                           
Consolidado$corte6<-ifelse(Consolidado$numeracion<= which(Consolidado$mes=="1986-01-01"),1,0)

Consolidado$corte7<-ifelse(Consolidado$numeracion>= which(Consolidado$mes=="2021-01-01"),1,0)

# Definir como serie de tiempo 

cartera<-ts(Consolidado$cartera,start = c(1984,1),frequency = 12)
m2<-ts(Consolidado$m2,start = c(1984,1),frequency = 12)
infla_ipc<-ts(Consolidado$infla_ipc,start = c(1984,1),frequency = 12)
i_activa<-ts(Consolidado$i_activa,start = c(1984,1),frequency = 12)

# Gráfico de las series
options(scipen=999)
ts.plot(cartera)

ts.plot(bm,m2,col=c("purple","red","blue"))
legend(legend = c("Base monetaria","M1","M2"),
       "topleft",lty = 1,col = c("purple","red","blue"))

# Gráfico estético con la librería ggplot2
# Reorganizar datos, todos los valores en una sola columna

#AGREGADOS MONETARIOS
agregados <- data.frame(fecha=rep(time(bm),12),
                        valor=c(bm,m1,m2),
                        Sector=c(rep("bm",466),rep("m1",466),
                                 rep("m2",466)))

ggplot(agregados,mapping = aes(x = fecha,y = valor))+
  geom_line(aes(color = Sector),size=1)+
  ggtitle("Agregados monetarios 1984-2022")+theme_light()

#TASAS DE INTERÉS

tasas<-data.frame(fecha=rep(time(i_activa),12),
                        valor=c(i_activa,tib),
                        tipo=c(rep("Activa",466),
                               rep("TIB",466)))
                  
ggplot(tasas,aes(x=fecha,y=valor))+
  geom_line(aes(color = tipo))+theme_classic()+
  labs(x="Fecha",
       y="Tasa de interés",
       color="Tasa de interés")+
  theme(legend.position=c(.15,.25))

# SALDOS DE CARTERA
carteradf<-data.frame(fecha=rep(time(cartera),12),
                    valor=c(cartera,m2),
                    tipo=c(rep("Cartera Neta",466),
                           rep("M2",466)))

ggplot(carteradf,aes(x=fecha,y=valor))+
  geom_line(aes(color=tipo), size=.8)+theme_classic()+
  labs(x="Fecha",
       y="Saldos netos de cartera y M2",
       color="")+
  theme(legend.position=c(.15,.25))

#INFLACIÓN
infla_agre<-data.frame(fecha=rep(time(infla_ipc),12),
                      valor=c(infla_ipc,infla_ipp),
                      agregado=c(rep("Inflación (IPC)",466),rep("Inflación (IPP)",466)))

                      
ggplot(infla_agre,aes(x=fecha,y=valor))+
  geom_line(aes(color=agregado))+theme_classic()+
  labs(x="Fecha",
       y="Inflación",
       color="Inflación")+
  theme(legend.position=c(.15,.25))

# Cruce inflacion y agregados monetarios
library(patchwork)

p1<-ggplot(data = Consolidado, aes(x = m2 , y = infla_ipc)) + 
  geom_point(color = 'slateblue', size = 1, alpha = 0.6)+
  geom_smooth(color = 'red') +
  xlab('M2') + 
  ylab('Inflación (IPC)') +
  ggtitle('Relación entre M2 e Inflación') + 
  theme_light()

p2<-ggplot(data = Consolidado, aes(x = m1 , y = infla_ipc)) + 
  geom_point(color="#00AFBB", size = 1, alpha = 0.6)+
  geom_smooth(color = 'red') +
  xlab('M1') + 
  ylab('Inflación (IPC)') +
  ggtitle('Relación entre M1 e Inflación') + 
  theme_light()

p3<-ggplot(data = Consolidado, aes(x = bm , y = infla_ipc)) + 
  geom_point(color = "#E7B800", size = 1, alpha = 0.6)+
  geom_smooth(color = 'red') +
  xlab('BM') + 
  ylab('Inflación (IPC)') +
  ggtitle('Relación entre BM e Inflación') + 
  theme_light()

(p1 | p2) / p3


# Pruebas de raiz unitaria ------------------------------------------------

library(urca)
library(tseries)

summary(ur.df(infla_ipc, selectlags = "AIC", lags=12, type="drift"))
summary(ur.df(cartera, selectlags = "AIC", lags=12, type="drift"))
summary(ur.df(i_activa, selectlags = "AIC", lags=12, type="drift"))
summary(ur.df(m2, selectlags = "AIC", lags=12, type="drift"))

summary(ur.pp(infla_ipc, type="Z-tau", model = "constant", lags = "long"))
summary(ur.pp(cartera, type="Z-tau", model = "constant", lags = "long"))
summary(ur.pp(i_activa, type="Z-tau", model = "constant", lags = "long"))
summary(ur.pp(m2, type="Z-tau", model = "constant", lags = "long"))

summary(ur.kpss(infla_ipc, type = "mu", lags = "long"))
summary(ur.kpss(cartera, type = "mu", lags = "long"))
summary(ur.kpss(i_activa, type = "mu", lags = "long"))
summary(ur.kpss(m2, type = "mu", lags = "long"))

# Diferencias logarítmicas de las series

dlinfla = na.omit(diff(log(infla_ipc)))
dli = na.omit(diff(log(i_activa)))
dlm2 = na.omit(diff(log(m2)))
dlcartera<-na.omit(diff(log(cartera)))
plot(dlcartera)
plot(cbind(dlinfla,dli,dlm2,dlcartera),
     nc=2,main="Diferencia logarítmica de las series
     Pruebas de estacionariedad")

# aparentemente son estacionarias, corroborarlo para Financiero

summary(ur.df(dlinfla, selectlags = "AIC", lags=12, type="drift"))
summary(ur.df(dlcartera, selectlags = "AIC", lags=12, type="drift"))
summary(ur.df(dli, selectlags = "AIC", lags=12, type="drift"))
summary(ur.df(dlm2, selectlags = "AIC", lags=12, type="drift"))

summary(ur.pp(dlinfla, type="Z-tau", model = "constant", lags = "long"))
summary(ur.pp(dlcartera, type="Z-tau", model = "constant", lags = "long"))
summary(ur.pp(dli, type="Z-tau", model = "constant", lags = "long"))
summary(ur.pp(dlm2, type="Z-tau", model = "constant", lags = "long"))

summary(ur.kpss(dlinfla, type = "mu", lags = "long"))
summary(ur.kpss(dlcartera, type = "mu", lags = "long"))
summary(ur.kpss(dli, type = "mu", lags = "long"))
summary(ur.kpss(dlm2, type = "mu", lags = "long"))


# Choque estructural ------------------------------------------------------

#Vector de variables exógenas
## corremos primero el modelo y en las pruebas del error valoramos choque

# Choque estructural ------------------------------------------------------
## Cambios en la estructura de nuestra serie

library(strucchange)
fcar=Fstats(cartera~1) #Prueba de Chow H0: ausencia de cambio estructural
plot(fcar)

bpcarte=breakpoints(cartera~1)
plot(bpcarte)

summary(bpcarte)
breaks<-breakfactor(bpcarte)[141:456]


#Ya incorporamos las dummies

#Preparando la base de datos
## Nuestro modelo tendrá variables integradas de orden 1
### Creamos vector de variables endógenas

endoge<-ts.intersect(dlinfla,dlcartera,dli,dlm2)
endoge

dfexo<-Consolidado[-1,c("corte1","corte2","corte3","corte4","corte5","corte6","corte7")]

c1<-ts(dfexo$corte1,start = c(1984,2),frequency = 12)
c2<-ts(dfexo$corte2,start = c(1984,2),frequency = 12)
c3<-ts(dfexo$corte3,start = c(1984,2),frequency = 12)
c4<-ts(dfexo$corte4,start = c(1984,2),frequency = 12)
c5<-ts(dfexo$corte5,start = c(1984,2),frequency = 12)
c6<-ts(dfexo$corte6,start = c(1984,2),frequency = 12)
c7<-ts(dfexo$corte7,start = c(1984,2),frequency = 12)

exo<-ts.intersect(c2,c3,c4,c5)
exo


# DETERMINACION DE REZAGOS OPTIMOS ----------------------------------------
library(vars)
VARselect(y = endoge, type = "const", exogen = exo)  #con variable exogenas o dicotomicas

# Para el modelo sin variables sigamos el criterio HQ de 3 rezagos


# Estimación: p=3 y dummies -----------------------------------------------


VAR_p = VAR(y = endoge,
           type = "const",p = 3,exogen = exo)
summary(VAR_p) 


## Estabilidad del sistema, raíces del polinomio característico--------

plot(VAR_p) # Deiagramas de ajuste y reiduos

roots(VAR_p,modulus = F) # raíces
roots(VAR_p) # módulo
plot(roots(VAR_p,modulus = F),xlim=c(-1.1,1.1),ylim=c(-1.1,1.1),asp=1,
     ylab="",xlab="")
lines(sin(seq(-6,6,0.001)),cos(seq(-6,6,0.001)))


# Diagnostico de los residuos ---------------------------------------------

#Normalidad de los residuos
normality.test(VAR_p) #Jarque Bera multivariada

residuos<-resid(VAR_p)
re.ts<-ts(residuos,start = c(1984,2),frequency = 12)
ts.plot(re.ts)

which(re.ts==max(re.ts))
Consolidado[Consolidado$numeracion==445,1]

which(re.ts==min(re.ts))
Consolidado[Consolidado$numeracion==25,1]

pru<-re.ts[(re.ts<quantile(re.ts,.25)-IQR(re.ts))==T]
length(pru)

quantile(re.ts,.75)

# H0 es que se distribuyen normal. Esta condición no se cumple. La rechazamo


## Evaluar autocorrelación serial de los errores (ruido blanco)

serial.test(VAR_p,lags.pt = 4,type = "PT.asymptotic")

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
Portmanteau.var(modelo = VAR_p,rezago.max = 8)
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
LMBreush.var(modelo = VAR_p,rezago.max = 8)

## Evaluar normalidad
normality.test(VAR_p)  # Únicamente resultados multivariados
normality.test(VAR_p,multivariate.only = F)  # resultados ubivariados para JB
# no se rechaza la hipótesis nula de normalidad (p.valor del JB=0.89)



# LECTURAS DEL MODELO -----------------------------------------------------

# CAUSALIDAD DE GRANGER ---------------------------------------------------

## Causalidad de Granger
# Conjunta

summary(VAR_p)
causality(VAR_p,cause = c("dlcartera"))
causality(VAR_p,cause = c("dli"))
causality(VAR_p,cause = c("dlm2"))
causality(VAR_p,cause = c("dlinfla"))

# Individuales
grangertest(dlcartera~dlm2,order=3) #m2 si causa cartera: monetarista
grangertest(dlm2~dlcartera,order=3) #Cartera causa m2, con mas significancia: endogeneidad

grangertest(dlcartera~dli,order=3) # La tasa no afecta la cartera (Keynes)
grangertest(dli~dlcartera,order=3) # La cartera impacta la tasa (estructuralista)

grangertest(dlinfla~dlm2,order=3) #m2 no mueve la inflacion
grangertest(dlm2~dlinfla,order=3) #Inflacion no causa m2

grangertest(dlinfla~dlcartera,order=3) # cartera no causa inflacion
grangertest(dlcartera~dlinfla,order=3) #inflacion no causa cartera

grangertest(dli~dlm2,order=3) # m2 no causa las tasa
grangertest(dlm2~dli,order=3) # las tasas no causan m2

grangertest(dli~dlinfla,order=3) #inflacion causa tasas
grangertest(dlinfla~dli,order=3) # tasas no causa inflación


## Impulsos respuesta-------------
#dlinfla,dlTIB,dlM2,dlreservas,dlcartera,dlpib

plot(irf(VAR_p,n.ahead = 10), main = )

# Impulsos respuesta; choques en la inflacion
plot(irf(VAR_p,n.ahead = 10,impulse = "dlinfla"))

plot(irf(VAR_p,n.ahead = 10,impulse = "dlcartera"),
     main="")

plot(irf(VAR_p,n.ahead = 10,impulse = "dlm2"),
     main="") 

plot(irf(VAR_p,n.ahead = 10,impulse = "dli"))
     
