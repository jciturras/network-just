# Librerias
library(haven)
library(dplyr)
library(car)
library(likert)
library(polycor)
library(psych)
library(ggplot2)
library(magrittr)
library(ggpubr)
library(MASS)
library(texreg)
library(ggeffects)


#######################################################################################
#Importación Base de datos
# Codigo Importacion GBV
#######################################################################################

root.dir = "C:/Users/gmobe/Dropbox/Clases/Magister/3er semestre/Seminario de Tesis/"
COESW <- read_dta(paste0(root.dir,"Data/ELSOC_W01_v2.00_Stata14.dta"))

##Selección de variables
Bd_tesis<-dplyr::select(COESW, idencuesta, ola, version, muestra, sospechoso, 
                        starts_with("r01"), 
                        starts_with("d02"), 
                        c15,
                        m0_sexo,m0_edad,m01,m03,m36,
                        c18_02,c18_03)

##Pasar a missing las variables
# view_df(Bd_tesis)
###Limpiar los valores fuera de rango (-888, -999) 
for(i in 6:29){
  Bd_tesis[,i][Bd_tesis[,i] == -999]<-NA
  Bd_tesis[,i][Bd_tesis[,i] == -888]<-NA
}

#Se eliminan los casos sospechosos
# Bd_tesis <- subset(Bd_tesis, casos_sospechosos==0) 

#######################################################################################
##Generar Variables independientes (Controles)

#######################################################################################
#Educacion
Bd_tesis$educ <- as.numeric(Bd_tesis$m01) #Nivel Educacional contínuo
Bd_tesis$edcat <- car::recode(Bd_tesis$m01, "c(1,2)=1; c(3,4)=2; c(5,6)=3; c(7,8)=4;c(9)=5; c(10)=6") 

Bd_tesis$edcat <- factor(Bd_tesis$edcat,levels = c(1,2,3,4,5,6), labels=c("Primaria o menos",
                                                                          "Primaria y secundaria baja",
                                                                          "Secundaria alta",
                                                                          "Terciaria ciclo corto",
                                                                          "Terciaria",
                                                                          "Postgrado")) 
#Sexo
Bd_tesis$sexo <- car::recode(Bd_tesis$m0_sexo, "1=0;2=1")
Bd_tesis$sexo <- factor(Bd_tesis$sexo, levels = c(0,1), labels = c("Hombre","Mujer")) 

#Edad
Bd_tesis$edad <- as.numeric(Bd_tesis$m0_edad) #Edad

#Posicion Politica
Bd_tesis$ppolcat  <- car::recode(Bd_tesis$c15, "c(0,1,2,3,4)=1;5=2;c(6,7,8,9,10)=3;11=4;12=5") 
Bd_tesis$ppolcat  <- factor(Bd_tesis$ppolcat, levels = c(1,2,3,4,5), labels = c("Izquierda/Centro Izquierda",
                                                                                "Centro",
                                                                                "Derecha/Centro Derecha",
                                                                                "Independiente",
                                                                                "Ninguno"))

#######################################################################################
##Graficos de controles

#######################################################################################
###Sexo
Ssex<-Bd_tesis %>% group_by(sexo)%>%
  summarise(freq=n()) %>%
  filter(!is.na(sexo))%>%
  mutate(totfreq=sum(freq)) %>%  
  mutate(perc=freq/totfreq)%>%
  mutate(perc=round(perc,2))%>%
  mutate(sumperc=cumsum(perc))%>%
  mutate(perc=(perc*100))%>%
  mutate(sumperc=(sumperc*100))%>%
  mutate(sumfreq=cumsum(freq))

###Educ
Sedu<-Bd_tesis %>% group_by(edcat)%>%
  summarise(freq=n()) %>%
  filter(!is.na(edcat))%>%
  mutate(totfreq=sum(freq)) %>%  
  mutate(perc=freq/totfreq)%>%
  mutate(perc=round(perc,2))%>%
  mutate(sumperc=cumsum(perc))%>%
  mutate(perc=(perc*100))%>%
  mutate(sumperc=(sumperc*100))%>%
  mutate(sumfreq=cumsum(freq))

###Pospol
Spp<-Bd_tesis %>% group_by(ppolcat)%>%
  summarise(freq=n()) %>%
  filter(!is.na(ppolcat))%>%
  mutate(totfreq=sum(freq)) %>%  
  mutate(perc=freq/totfreq)%>%
  mutate(perc=round(perc,2))%>%
  mutate(sumperc=cumsum(perc))%>%
  mutate(perc=(perc*100))%>%
  mutate(sumperc=(sumperc*100))%>%
  mutate(sumfreq=cumsum(freq))
  
# 
Variable<-c("Sexo","")
Ssex<-cbind(Ssex,Variable)
Variable<-c("","","","","","","")
Ssex<-rbind(Ssex,Variable)

Variable<-c("Educacion","","","","","")
Sedu<-cbind(Sedu,Variable)
Variable<-c("","","","","","")
Sedu<-rbind(Sedu,Variable)

Variable<-c("Posicion Politica","","","","")
Spp<-cbind(Spp,Variable)
Variable<-c("","","","","")
Spp<-rbind(Spp,Variable)

colnames(Ssex) <- c("Categoria","Freq","FreqT","Perc","Cumperc","CumFreq","Variable")
colnames(Sedu) <- c("Categoria","Freq","FreqT","Perc","Cumperc","CumFreq","Variable")
colnames(Spp) <- c("Categoria","Freq","FreqT","Perc","Cumperc","CumFreq","Variable")
Caract<-rbind(Ssex,Sedu,Spp)  
Caract <- Caract[, c(7,1,2,3,4,5,6)]


#######################################################################################
##Variables Dependientes
#Seleccionar variables
#######################################################################################

D2<-dplyr::select(Bd_tesis, starts_with("d02_"))

# Se asignan etiquetas a variables
b<-c("Totalmente en desacuerdo","En desacuerdo","Ni en desacuerdo ni de acuerdo","De acuerdo","Totalmente de acuerdo")
D2$d02_01<-ordered(D2$d02_01,levels=c(1,2,3,4,5),labels=c(b))
D2$d02_02<-ordered(D2$d02_02,levels=c(1,2,3,4,5),labels=c(b))
D2$d02_03<-ordered(D2$d02_03,levels=c(1,2,3,4,5),labels=c(b))

# Se agregan labels
DDSSw<-c( "Pensiones",
          "Educacion",
          "Salud")

colnames(D2)<-(DDSSw)

D2<-as.data.frame(D2)
LD2 <- likert(D2)
LD2p<-likert.bar.plot(LD2,
                      low.color = "#08578a",high.color = "#7c1f18", neutral.color = "#151515",
                      text.size = 3.5,legend = "", ordered = FALSE,include.center=TRUE) +
  ggtitle("Es justo que las personas de altos ingresos puedan acceder o tengan mejor acceso a...")+
  # theme_bw()+
  theme(axis.text.y = element_text(size=8,hjust = 0))+
  theme(plot.title = element_text(size=8, hjust=0.5))+
  geom_hline(yintercept = 0, color =c("white"))+
  theme(legend.position ="bottom",
        legend.title=element_text(size=0),
        legend.text=element_text(size=8))

#######################################################################################
# Tabla VI

#######################################################################################
t1Pens <-Bd_tesis %>% group_by(d02_01)%>%
  summarise(freq=n()) %>%
  filter(!is.na(d02_01))%>%
  mutate(totfreq=sum(freq)) %>%  
  mutate(perc=freq/totfreq)%>%
  mutate(perc=round(perc,2))%>%
  mutate(sumperc=cumsum(perc))%>%
  mutate(perc=(perc*100))%>%
  mutate(sumperc=(sumperc*100))%>%
  mutate(sumfreq=cumsum(freq))
t1Pens<-t1Pens[c(1,2,4)]
t1Pens<-t(t1Pens)
Variable<-c("","","","","")
t1Pens<-rbind(t1Pens,Variable)

t1Educ <-Bd_tesis %>% group_by(d02_02)%>%
  summarise(freq=n()) %>%
  filter(!is.na(d02_02))%>%
  mutate(totfreq=sum(freq)) %>%  
  mutate(perc=freq/totfreq)%>%
  mutate(perc=round(perc,2))%>%
  mutate(sumperc=cumsum(perc))%>%
  mutate(perc=(perc*100))%>%
  mutate(sumperc=(sumperc*100))%>%
  mutate(sumfreq=cumsum(freq))
t1Educ<-t1Educ[c(1,2,4)]
t1Educ<-t(t1Educ)
Variable<-c("","","","","")
t1Educ<-rbind(t1Educ,Variable)

t1Salu <-Bd_tesis %>% group_by(d02_03)%>%
  summarise(freq=n()) %>%
  filter(!is.na(d02_03))%>%
  mutate(totfreq=sum(freq)) %>%  
  mutate(perc=freq/totfreq)%>%
  mutate(perc=round(perc,2))%>%
  mutate(sumperc=cumsum(perc))%>%
  mutate(perc=(perc*100))%>%
  mutate(sumperc=(sumperc*100))%>%
  mutate(sumfreq=cumsum(freq))
t1Salu<-t1Salu[c(1,2,4)]
t1Salu<-t(t1Salu)
Variable<-c("","","","","")
t1Salu<-rbind(t1Salu,Variable)

tabla<-rbind(t1Pens,t1Educ,t1Salu)
b<-c("Tot. desacuerdo","Desacuerdo","Ni desac/ni de acuerdo","Acuerdo","Tot. acuerdo")

colnames(tabla)<-b

tabla<-as.data.frame(tabla)
var1<-c("Pensiones","","","","Educacion","","","","Salud","","","")
var2<-c("","Frec.","(%)","","","Frec.","(%)","","","Frec.","(%)","")
var<-cbind(var1,var2)
tabla<-cbind(var,tabla)

#######################################################################################
#Correlaciones

#######################################################################################

cordat<-dplyr::select(Bd_tesis, starts_with("d02_"))
xx<-polychoric(cordat)
xx<-as.data.frame(xx$rho)

b<-c("Pesiones","Educacion","Salud")
colnames(xx)<-b
rownames(xx)<-b
xx$Pesiones<-round(xx$Pesiones,digits = 3)
xx$Educacion<-round(xx$Educacion,digits = 3)
xx$Salud<-round(xx$Salud,digits = 3)
xx[upper.tri(xx,diag=TRUE)]<-""
xx<-as.data.frame(xx)

#######################################################################################
#Cálculo tamaño de la red

#######################################################################################

##OCupaciones (R1)
#Selección de variables
R01<-as.data.frame(dplyr::select(Bd_tesis, idencuesta, starts_with("r01_")))

#Asignar valor medio a cada categoría de respuesta
x<-c(0,1,3,6,9,13,16,0,0) 

for(i in 1:nrow(R01)){
  for(j in 2:ncol(R01)){
    R01[i,j]=x[R01[i,j]]
  }
}

R01[is.na(R01)] <-0

#Sumar el cálculo
R01$size01<-rowSums(R01[,2:14])

#Chequeo
# frequency::freq(R01$size01)

#Hay que eliminar casos con valor 0
R01<-R01%>% 
  filter(size01>0)

Ocupaciones.n<-as.data.frame(cbind(c("Gerente empresa multinacional","Vendedor ambulante", "Secretario",
                                     "Mecanico de autos","Vendedor de tienda",
                                     "Abogado","Aseador","Medico","Parvulario","Chofer de taxi",
                                     "Camarero","Contador","Profesor Universitario",
                                     "Total")))

Mean.Ocupaciones<-as.data.frame(rbind(
  mean(R01$r01_01,na.rm = T),
  mean(R01$r01_02,na.rm = T),
  mean(R01$r01_03,na.rm = T),
  mean(R01$r01_04,na.rm = T),
  mean(R01$r01_05,na.rm = T),
  mean(R01$r01_06,na.rm = T),
  mean(R01$r01_07,na.rm = T),
  mean(R01$r01_08,na.rm = T),
  mean(R01$r01_09,na.rm = T),
  mean(R01$r01_10,na.rm = T),
  mean(R01$r01_11,na.rm = T),
  mean(R01$r01_12,na.rm = T),
  mean(R01$r01_13,na.rm = T),
  mean(R01$size01,na.rm = T)))

sd.Ocupaciones<-as.data.frame(rbind(
  sd(R01$r01_01,na.rm = T),
  sd(R01$r01_02,na.rm = T),
  sd(R01$r01_03,na.rm = T),
  sd(R01$r01_04,na.rm = T),
  sd(R01$r01_05,na.rm = T),
  sd(R01$r01_06,na.rm = T),
  sd(R01$r01_07,na.rm = T),
  sd(R01$r01_08,na.rm = T),
  sd(R01$r01_09,na.rm = T),
  sd(R01$r01_10,na.rm = T),
  sd(R01$r01_11,na.rm = T),
  sd(R01$r01_12,na.rm = T),
  sd(R01$r01_13,na.rm = T),
  sd(R01$size01,na.rm = T)))

median.Ocupaciones<-as.data.frame(rbind(
  median(R01$r01_01,na.rm = T),
  median(R01$r01_02,na.rm = T),
  median(R01$r01_03,na.rm = T),
  median(R01$r01_04,na.rm = T),
  median(R01$r01_05,na.rm = T),
  median(R01$r01_06,na.rm = T),
  median(R01$r01_07,na.rm = T),
  median(R01$r01_08,na.rm = T),
  median(R01$r01_09,na.rm = T),
  median(R01$r01_10,na.rm = T),
  median(R01$r01_11,na.rm = T),
  median(R01$r01_12,na.rm = T),
  median(R01$r01_13,na.rm = T),
  median(R01$size01,na.rm = T)))

Descriptivos.Ocupaciones<-cbind(Ocupaciones.n,Mean.Ocupaciones,sd.Ocupaciones,median.Ocupaciones)
Descriptivos.Ocupaciones$Min<-0
Descriptivos.Ocupaciones$Max<-18
colnames(Descriptivos.Ocupaciones)<-c("Ocupacion","Mean","SD","Median","Min","Max")
Descriptivos.Ocupaciones$Mean<-round(Descriptivos.Ocupaciones$Mean,digits = 3)
Descriptivos.Ocupaciones$SD<-round(Descriptivos.Ocupaciones$SD,digits = 3)

#######################################################################################
##Generar Data Red Extensa
##
#######################################################################################

Red<-cbind(R01)

#Todos los items
Red$sizesum<-Red$size01

hist.D1<-ggplot(Red, aes(x=sizesum)) + 
  geom_histogram()+theme_bw()+
  ggtitle("")+
  ylab("Frecuencia")+xlab("")+
theme(legend.position = "bottom",
      legend.text = element_text(size = 8),
      legend.title = element_text(size = 8))

mean.D1 <- Red %>% 
  filter(!is.na(sizesum)) %>% 
  summarise(means = mean(sizesum), sd=sd(sizesum))

D1hs<-ggplot(Red, aes(x=sizesum)) +
  stat_function(fun = dnorm, color="#a8a8a8", args=list(mean=mean.D1$means, sd=mean.D1$sd))+
  theme_bw()+ylab("")+xlab("")

size.hist<-ggarrange(hist.D1,D1hs, labels = c("",""),ncol = 2,
                     common.legend = TRUE, legend = "bottom",font.label = list(size = 10))

#Calculo Heterogeneidad de la red
##Ocupaciones
Red$p01_01<-(Red$r01_01/Red$size01)*(Red$r01_01/Red$size01)
Red$p01_02<-(Red$r01_02/Red$size01)*(Red$r01_02/Red$size01)
Red$p01_03<-(Red$r01_03/Red$size01)*(Red$r01_03/Red$size01)
Red$p01_04<-(Red$r01_04/Red$size01)*(Red$r01_04/Red$size01)
Red$p01_05<-(Red$r01_05/Red$size01)*(Red$r01_05/Red$size01)
Red$p01_06<-(Red$r01_06/Red$size01)*(Red$r01_06/Red$size01)
Red$p01_07<-(Red$r01_07/Red$size01)*(Red$r01_07/Red$size01)
Red$p01_08<-(Red$r01_08/Red$size01)*(Red$r01_08/Red$size01)
Red$p01_09<-(Red$r01_09/Red$size01)*(Red$r01_09/Red$size01)
Red$p01_10<-(Red$r01_10/Red$size01)*(Red$r01_10/Red$size01)
Red$p01_11<-(Red$r01_11/Red$size01)*(Red$r01_11/Red$size01)
Red$p01_12<-(Red$r01_12/Red$size01)*(Red$r01_12/Red$size01)
Red$p01_13<-(Red$r01_13/Red$size01)*(Red$r01_13/Red$size01)

Red$SimpsonD.1=1-rowSums(Red[,17:29])

Red$Diversidad[Red$SimpsonD.1<=0.3] <- "Baja"
Red$Diversidad[Red$SimpsonD.1>0.3 & Red$SimpsonD.1<=0.7] <- "Media"
Red$Diversidad[Red$SimpsonD.1>0.7 ] <- "Alta"

histsimps.D1<-ggplot(Red, aes(x=SimpsonD.1, fill=Diversidad, color=Diversidad)) + 
  geom_histogram()+
  # theme_bw()+
  ggtitle("")+
  scale_x_continuous(breaks = c(0,0.1,0.2,0.3,0.4,0.5,
                                0.6,0.7,0.8,0.9,1))+
  ylab("Frecuencia")+xlab("")+
theme(legend.position = "bottom",
      legend.text = element_text(size = 8),
      legend.title = element_text(size = 8))+
  ylim(0,600)+
  scale_color_manual(values=c("#999999", "#E69F00", "#56B4E9"))+
  scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9"))

##Suavizado
meanD1 <- Red %>% 
  filter(!is.na(SimpsonD.1)) %>% 
  summarise(means = mean(SimpsonD.1), sd=sd(SimpsonD.1))

D1h<-ggplot(Red, aes(x=SimpsonD.1)) +
  stat_function(fun = dnorm, color="#a8a8a8", args=list(mean=meanD1$means, sd=meanD1$sd))+
  # theme_bw()+
  ylab("")+
  xlab("")+
  scale_x_continuous(breaks = c(0,0.1,0.2,0.3,0.4,0.5,
                                0.6,0.7,0.8,0.9,1))

##Histogramas SimposonD
Het.hist<-ggarrange(histsimps.D1,D1h, labels = c("",""),ncol = 2,
                    common.legend = TRUE, legend = "bottom",font.label = list(size = 10))

##Generar base desiguladad con tamaño red (Merge)
D2<-dplyr::select(Bd_tesis, idencuesta,d02_01,d02_02,d02_03,
                  edcat,sexo,educ,edad,ppolcat,c18_02,c18_03)
Red<-left_join(Red,D2,by = c("idencuesta"))

#Calculo Composicion de la red
##ISEI
Red$SizeA.Isei<-rowSums(Red[,c(2,7,9,13,14)])
Red$SizeB.Isei<-rowSums(Red[,c(3,5,8,11,12,4,6,10)])

# Red<-Red %>%
#     filter(SizeA.Isei !=0 & SizeA.Isei!=0)

Red$SizeA.Isei[Red$SizeA.Isei==0] <-0.1
Red$SizeB.Isei[Red$SizeB.Isei==0] <-0.1

Red$Compocision.Isei<-(Red$SizeB.Isei)/(Red$SizeA.Isei+Red$SizeB.Isei)

Red$Composicion[Red$Compocision.Isei<=0.3] <- "Baja"
Red$Composicion[Red$Compocision.Isei>0.3 & Red$Compocision.Isei<=0.7] <- "Media"
Red$Composicion[Red$Compocision.Isei>0.7 ] <- "Alta"


hist.Isei<-ggplot(Red, aes(x=Compocision.Isei,fill=Composicion, color=Composicion)) + 
  geom_histogram()+
  # theme_bw()+
  scale_x_continuous(breaks = c(0,0.1,0.2,0.3,0.4,0.5,
                                0.6,0.7,0.8,0.9,1))+
  ylab("Frecuencia")+xlab("")+
  ylim(0, 600)+
  scale_color_manual(values=c("#999999", "#E69F00", "#56B4E9"))+
  scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9"))

meanIsei <- Red %>% 
  filter(!is.na(Compocision.Isei)) %>% 
  summarise(means = mean(Compocision.Isei), sd=sd(Compocision.Isei))

Iseih<-ggplot(Red, aes(x=Compocision.Isei)) +
  stat_function(fun = dnorm, color="#a8a8a8", args=list(mean=meanIsei$means, sd=meanIsei$sd))+
  # theme_bw()+
  ylab("")+xlab("")+
  scale_x_continuous(breaks = c(0,0.1,0.2,0.3,0.4,0.5,
                                0.6,0.7,0.8,0.9,1))

Compo.hist<-ggarrange(hist.Isei,Iseih, 
                      labels = c("",""),
                      ncol = 2,
                      common.legend = TRUE, legend = "bottom",font.label = list(size = 10))


#SCALE UP
#opcion 1 = servel reinscritos y estimaciones del registro civil
grpsize1 = cbind(
  grpsize1.1 =23782   , # Gerente de Empresa Grande
  grpsize1.2 =106166  , # Vendedor ambulante
  grpsize1.3 =203951  , # Secretaria(o)
  grpsize1.4 =97512   , # Mecanico de autos
  grpsize1.5 =467594  , # Vendedor de tienda
  grpsize1.6 =24940   , # Abogado(a)
  grpsize1.7 =303848  , # Aseador(a) de oficina
  grpsize1.8 =21329   , # Medico
  grpsize1.9 =71413   , # Parvularia
  grpsize1.10 =166745 , # Chofer de Taxi
  grpsize1.11 =70664  , # Camarero o mozo
  grpsize1.12 =85587  , # Contador(a)
  grpsize1.13 =28961  , # Profesor universitario
  grpsize1.14 =53751  , #Hernan
  grpsize1.15 =489641 , #Ignacio
  grpsize1.16 =19879  , #Ximena
  grpsize1.17 =30280  , #Viviana
  grpsize1.18 =2375   , #Sacerdote
  grpsize1.19 =1710446, #Mapuche
  grpsize1.20 =40990  , #Miembro UDI
  grpsize1.21 =125192 , #Inmigrante peruano
  grpsize1.22 =52356  , #Miembro PC
  grpsize1.23 =29719  , #Miembro DC
  grpsize1.24 =619885 , #Homosexual No actualizado
  grpsize1.25 =527565   #Desempleado
)

grpsum1 = sum(grpsize1)

Red$scaleup1 = (Red$sizesum/grpsum1) * 18191884 #poblacion chilena

meanScaleUp1 <- Red %>% 
  filter(!is.na(scaleup1)) %>% 
  summarise(means = mean(scaleup1), sd=sd(scaleup1))

SUP1<-ggplot(Red, aes(x=scaleup1)) +
  stat_function(fun = dnorm, color="#a8a8a8", args=list(mean=meanScaleUp1$means, sd=meanScaleUp1$sd))+ 
  geom_line(aes(y = ..density.., colour = 'Empirical'), stat = 'density') +
  geom_histogram(aes(y = ..density..), alpha = 0.4) +
  theme_bw()+ylab("")+xlab("")+
  ggtitle("Scale Up")


#########################################################################################
#Igualitarismo
########################################################################################

Red$igualit<-(Red$c18_02+Red$c18_03)/2


#########################################################################################
#Modelos Logit ordinal
##SimpsonD
########################################################################################

Red$d02_01[Red$d02_01>3]<-3
Red$d02_02[Red$d02_02>3]<-3
Red$d02_03[Red$d02_03>3]<-3


#Ola1
M11= polr(factor(d02_01)~scaleup1+
            SimpsonD.1+
            sexo+edcat+ppolcat+edad+igualit, data=Red,Hess = T)
L11=extract(M11,include.thresholds =TRUE)

M12 = polr(factor(d02_02)~scaleup1+
            SimpsonD.1+
            sexo+edcat+ppolcat+edad+igualit, data=Red,Hess = T)
L12=extract(M12,include.thresholds =TRUE)

M13 = polr(factor(d02_03)~scaleup1+
            SimpsonD.1+
            sexo+edcat+ppolcat+edad+igualit, data=Red,Hess = T)
L13=extract(M13,include.thresholds =TRUE)

# texreg(l=list(L11,L12,L13),float.pos ="h!", caption ="Resultado de modelos logísticos ordinales", 
#        caption.above = TRUE, digits =3)


#Pension
TablaM11<-as.data.frame(ctable <- exp(coef(summary(M11))))
TablaM11$N<-c(1:16)
TablaM11<- TablaM11[which(TablaM11$N ==2 | TablaM11$N > 14),]
TablaM11$Value<-round(TablaM11$Value,digits=3)
TablaM11$A<-"Pension"

#Educacion
TablaM12<-as.data.frame(ctable <- exp(coef(summary(M12))))
TablaM12$N<-c(1:16)
TablaM12<- TablaM12[which(TablaM12$N ==2 | TablaM12$N > 14),]
TablaM12$Value<-round(TablaM12$Value,digits=3)
TablaM12$A<-"Educacion"

#Salud
TablaM13<-as.data.frame(ctable <- exp(coef(summary(M13))))
TablaM13$N<-c(1:16)
TablaM13<- TablaM13[which(TablaM13$N ==2 | TablaM13$N > 14),]
TablaM13$Value<-round(TablaM13$Value,digits=3)
TablaM13$A<-"Salud"

Tabla1<-rbind(TablaM11,TablaM12,TablaM13)

a1<-Tabla1[1,1]
a2<-Tabla1[2,1]
a3<-Tabla1[3,1]
a4<-Tabla1[4,1]
a5<-Tabla1[5,1]
a6<-Tabla1[6,1]
a7<-Tabla1[7,1]
a8<-Tabla1[8,1]
a9<-Tabla1[9,1]

########################################################################################
#Modelos Logit ordinal
##Composicion
########################################################################################
#Ola1
M21 = polr(factor(d02_01)~scaleup1+
            Compocision.Isei+
            sexo+edcat+ppolcat+edad+igualit, data=Red,Hess = T)
L21=extract(M21,include.thresholds =TRUE)

M22 = polr(factor(d02_02)~scaleup1+
            Compocision.Isei+
            sexo+edcat+ppolcat+edad+igualit, data=Red,Hess = T)
L22=extract(M22,include.thresholds =TRUE)

M23 = polr(factor(d02_03)~scaleup1+
            Compocision.Isei+
            sexo+edcat+ppolcat+edad+igualit, data=Red,Hess = T)
L23=extract(M23,include.thresholds =TRUE)

#Pension
TablaM21<-as.data.frame(ctable <- exp(coef(summary(M21))))
TablaM21$N<-c(1:16)
TablaM21<- TablaM21[which(TablaM21$N ==2 | TablaM21$N > 14),]
TablaM21$Value<-round(TablaM21$Value,digits=3)
TablaM21$A<-"Pension"

#Educacion
TablaM22<-as.data.frame(ctable <- exp(coef(summary(M22))))
TablaM22$N<-c(1:16)
TablaM22<- TablaM22[which(TablaM22$N ==2 | TablaM22$N > 14),]
TablaM22$Value<-round(TablaM22$Value,digits=3)
TablaM22$A<-"Educacion"

#Salud
TablaM23<-as.data.frame(ctable <- exp(coef(summary(M23))))
TablaM23$N<-c(1:16)
TablaM23<- TablaM23[which(TablaM23$N ==2 | TablaM23$N > 14),]
TablaM23$Value<-round(TablaM23$Value,digits=3)
TablaM23$A<-"Salud"

# texreg(l=list(L21,L22,L23),float.pos ="h!", caption ="Resultado de modelos logísticos ordinales", 
#        caption.above = TRUE, digits =3)

Tabla2<-rbind(TablaM21,TablaM22,TablaM23)

b1<-Tabla2[1,1]
b2<-Tabla2[2,1]
b3<-Tabla2[3,1]
b4<-Tabla2[4,1]
b5<-Tabla2[5,1]
b6<-Tabla2[6,1]
b7<-Tabla2[7,1]
b8<-Tabla2[8,1]
b9<-Tabla2[9,1]

########################################################################################
#Modelos Logit ordinal
##Composicion y Diversidad
########################################################################################
#Ola1
M31 = polr(factor(d02_01)~scaleup1+
            SimpsonD.1+
            Compocision.Isei+
            sexo+edcat+ppolcat+edad+igualit, data=Red,Hess = T)
L31=extract(M31,include.thresholds =TRUE)

M32 = polr(factor(d02_02)~scaleup1+
            SimpsonD.1+
            Compocision.Isei+
            sexo+edcat+ppolcat+edad+igualit, data=Red,Hess = T)
L32=extract(M32,include.thresholds =TRUE)

M33 = polr(factor(d02_03)~scaleup1+
            SimpsonD.1+
            Compocision.Isei+
            sexo+edcat+ppolcat+edad+igualit, data=Red,Hess = T)
L33=extract(M33,include.thresholds =TRUE)

# texreg(l=list(L31,L32,L33),float.pos ="h!", caption ="Resultado de modelos logísticos ordinales", 
#        caption.above = TRUE, digits =3)

#Pension
TablaM31<-as.data.frame(ctable <- exp(coef(summary(M31))))
TablaM31$N<-c(1:17)
TablaM31<- TablaM31[which(TablaM31$N ==2 |TablaM31$N ==3 | TablaM31$N > 15),]
TablaM31$Value<-round(TablaM31$Value,digits=3)
TablaM31$A<-"Pension"

#Educacion
TablaM32<-as.data.frame(ctable <- exp(coef(summary(M32))))
TablaM32$N<-c(1:17)
TablaM32<- TablaM32[which(TablaM32$N ==2 |TablaM32$N ==3 | TablaM32$N > 15),]
TablaM32$Value<-round(TablaM32$Value,digits=3)
TablaM32$A<-"Educacion"

#Salud
TablaM33<-as.data.frame(ctable <- exp(coef(summary(M33))))
TablaM33$N<-c(1:17)
TablaM33<- TablaM33[which(TablaM33$N ==2 |TablaM33$N ==3 | TablaM33$N > 15),]
TablaM33$Value<-round(TablaM33$Value,digits=3)
TablaM33$A<-"Salud"

# texreg(l=list(L31,L32,L33),float.pos ="h!", caption ="Resultado de modelos logísticos ordinales", 
#        caption.above = TRUE, digits =3)

Tabla3<-rbind(TablaM31,TablaM32,TablaM33)

c1<-Tabla3[1,1]
c2<-Tabla3[2,1]
c3<-Tabla3[3,1]
c4<-Tabla3[4,1]
c5<-Tabla3[5,1]
c6<-Tabla3[6,1]
c7<-Tabla3[7,1]
c8<-Tabla3[8,1]
c9<-Tabla3[9,1]
c10<-Tabla3[10,1]
c11<-Tabla3[11,1]
c12<-Tabla3[12,1]

#######################################
#Predichos
#######################################


# MODELO1
# Pensiones
PV11<-as.data.frame(ggpredict(M11, "SimpsonD.1"))
PV11$Opciones_Respuesta<-PV11$response.level

PPV11<-ggplot(data=PV11, aes(x=x, y=predicted, group=Opciones_Respuesta)) +
  geom_line(aes(color=Opciones_Respuesta))+
  scale_color_manual(values = c("black", "blue", "red"),
                     labels = c("Muy en Desacuerdo", "En desacuerdo","Ni de acuerdo ni en desacuero o (+)")) + 
  labs(x = "Opciones respuesta")+
  # theme_minimal()+
  theme(legend.position="bottom")+
  xlim(0,1)+ylim(0,0.6)+  
  ylab("Valores predichos")+xlab("Diversidad Pensiones")

# Salud
PV12<-as.data.frame(ggpredict(M12, "SimpsonD.1"))
PV12$Opciones_Respuesta<-PV12$response.level

PPV12<-ggplot(data=PV12, aes(x=x, y=predicted, group=Opciones_Respuesta)) +
  geom_line(aes(color=Opciones_Respuesta))+
  scale_color_manual(values = c("black", "blue", "red"),
                     labels = c("Muy en Desacuerdo", "En desacuerdo","Ni de acuerdo ni en desacuero o (+)")) + 
  labs(x = "Opciones respuesta")+
  # theme_minimal()+
  theme(legend.position="bottom")+
  xlim(0,1)+ylim(0,0.6)+  
  ylab("")+xlab("Diversidad Educacion")

# Salud
PV13<-as.data.frame(ggpredict(M13, "SimpsonD.1"))
PV13$Opciones_Respuesta<-PV13$response.level

PPV13<-ggplot(data=PV13, aes(x=x, y=predicted, group=Opciones_Respuesta)) +
  geom_line(aes(color=Opciones_Respuesta))+
  scale_color_manual(values = c("black", "blue", "red"),
                     labels = c("Muy en Desacuerdo", "En desacuerdo","Ni de acuerdo ni en desacuero o (+)")) + 
  labs(x = "Opciones respuesta")+
  # theme_minimal()+
  theme(legend.position="bottom")+
  xlim(0,1)+ylim(0,0.6)+  
  ylab("")+xlab("Diversidad Salud")

PV111213<-ggarrange(PPV11,PPV12,PPV13, 
                 labels = c("","",""),
                 ncol = 3,
                 common.legend = TRUE, legend = "bottom",font.label = list(size = 10))

ggsave("C:/Users/gmobe/Dropbox/Redes Sociales/Articulo Redes Y Desigualdad/VPHD.png", PV111213, units ="cm", width = 20,height = 15)


# MODELO2
# Pensiones
PV21<-as.data.frame(ggpredict(M21, "Compocision.Isei"))
PV21$Opciones_Respuesta<-PV21$response.level

PPV21<-ggplot(data=PV21, aes(x=x, y=predicted, group=Opciones_Respuesta)) +
  geom_line(aes(color=Opciones_Respuesta))+
  scale_color_manual(values = c("black", "blue", "red"),
                     labels = c("Muy en Desacuerdo", "En desacuerdo","Ni de acuerdo ni en desacuero o (+)")) + 
  labs(x = "Opciones respuesta")+
  # theme_minimal()+
  theme(legend.position="bottom")+
  xlim(0,1)+ylim(0,0.6)+  
  ylab("Valores predichos")+xlab("Composicion Pensiones")

# Salud
PV22<-as.data.frame(ggpredict(M22, "Compocision.Isei"))
PV22$Opciones_Respuesta<-PV22$response.level

PPV22<-ggplot(data=PV22, aes(x=x, y=predicted, group=Opciones_Respuesta)) +
  geom_line(aes(color=Opciones_Respuesta))+
  scale_color_manual(values = c("black", "blue", "red"),
                     labels = c("Muy en Desacuerdo", "En desacuerdo","Ni de acuerdo ni en desacuero o (+)")) + 
  labs(x = "Opciones respuesta")+
  # theme_minimal()+
  theme(legend.position="bottom")+
  xlim(0,1)+ylim(0,0.6)+  
  ylab("")+xlab("Composicion Educacion")

# Salud
PV23<-as.data.frame(ggpredict(M23, "Compocision.Isei"))
PV23$Opciones_Respuesta<-PV23$response.level

PPV23<-ggplot(data=PV23, aes(x=x, y=predicted, group=Opciones_Respuesta)) +
  geom_line(aes(color=Opciones_Respuesta))+
  scale_color_manual(values = c("black", "blue", "red"),
                     labels = c("Muy en Desacuerdo", "En desacuerdo","Ni de acuerdo ni en desacuero o (+)")) + 
  labs(x = "Opciones respuesta")+
  # theme_minimal()+
  theme(legend.position="bottom")+
  xlim(0,1)+ylim(0,0.6)+  
  ylab("")+xlab("Composicion Salud")

PV212223<-ggarrange(PPV21,PPV22,PPV23, 
                    labels = c("","",""),
                    ncol = 3,
                    common.legend = TRUE, legend = "bottom",font.label = list(size = 10))

ggsave("C:/Users/gmobe/Dropbox/Redes Sociales/Articulo Redes Y Desigualdad/VPCP.png", PV212223, units ="cm", width = 20,height = 15)

# MODELO3
# Pensiones
PV31<-as.data.frame(ggpredict(M31, "SimpsonD.1"))
PV31$Opciones_Respuesta<-PV31$response.level

PPV31<-ggplot(data=PV31, aes(x=x, y=predicted, group=Opciones_Respuesta)) +
  geom_line(aes(color=Opciones_Respuesta))+
  scale_color_manual(values = c("black", "blue", "red"),
                     labels = c("Muy en Desacuerdo", "En desacuerdo","Ni de acuerdo ni en desacuero o (+)")) + 
  labs(x = "Opciones respuesta")+
  # theme_minimal()+
  theme(legend.position="bottom")+
  xlim(0,1)+ylim(0,0.6)+  
  ylab("Valores predichos")+xlab("Diversidad Pensiones")

# Salud
PV32<-as.data.frame(ggpredict(M32, "SimpsonD.1"))
PV32$Opciones_Respuesta<-PV32$response.level

PPV32<-ggplot(data=PV32, aes(x=x, y=predicted, group=Opciones_Respuesta)) +
  geom_line(aes(color=Opciones_Respuesta))+
  scale_color_manual(values = c("black", "blue", "red"),
                     labels = c("Muy en Desacuerdo", "En desacuerdo","Ni de acuerdo ni en desacuero o (+)")) + 
  labs(x = "Opciones respuesta")+
  # theme_minimal()+
  theme(legend.position="bottom")+
  xlim(0,1)+ylim(0,0.6)+  
  ylab("")+xlab("Diversidad Educacion")

# Salud
PV33<-as.data.frame(ggpredict(M33, "SimpsonD.1"))
PV33$Opciones_Respuesta<-PV33$response.level

PPV33<-ggplot(data=PV33, aes(x=x, y=predicted, group=Opciones_Respuesta)) +
  geom_line(aes(color=Opciones_Respuesta))+
  scale_color_manual(values = c("black", "blue", "red"),
                     labels = c("Muy en Desacuerdo", "En desacuerdo","Ni de acuerdo ni en desacuero o (+)")) + 
  labs(x = "Opciones respuesta")+
  # theme_minimal()+
  theme(legend.position="bottom")+
  xlim(0,1)+ylim(0,0.6)+  
  ylab("")+xlab("Diversidad Salud")

PVH313233<-ggarrange(PPV31,PPV32,PPV33, 
                    labels = c("","",""),
                    ncol = 3,
                    common.legend = TRUE, legend = "bottom",font.label = list(size = 10))

ggsave("C:/Users/gmobe/Dropbox/Redes Sociales/Articulo Redes Y Desigualdad/VPMHD.png", PVH313233, units ="cm", width = 20,height = 15)

# Pensiones
PV31<-as.data.frame(ggpredict(M31, "Compocision.Isei"))
PV31$Opciones_Respuesta<-PV31$response.level

PPV31<-ggplot(data=PV31, aes(x=x, y=predicted, group=Opciones_Respuesta)) +
  geom_line(aes(color=Opciones_Respuesta))+
  scale_color_manual(values = c("black", "blue", "red"),
                     labels = c("Muy en Desacuerdo", "En desacuerdo","Ni de acuerdo ni en desacuero o (+)")) + 
  labs(x = "Opciones respuesta")+
  # theme_minimal()+
  theme(legend.position="bottom")+
  xlim(0,1)+ylim(0,0.6)+  
  ylab("Valores predichos")+xlab("Composicion Pensiones")

# Salud
PV32<-as.data.frame(ggpredict(M32, "Compocision.Isei"))
PV32$Opciones_Respuesta<-PV32$response.level

PPV32<-ggplot(data=PV32, aes(x=x, y=predicted, group=Opciones_Respuesta)) +
  geom_line(aes(color=Opciones_Respuesta))+
  scale_color_manual(values = c("black", "blue", "red"),
                     labels = c("Muy en Desacuerdo", "En desacuerdo","Ni de acuerdo ni en desacuero o (+)")) + 
  labs(x = "Opciones respuesta")+
  # theme_minimal()+
  theme(legend.position="bottom")+
  xlim(0,1)+ylim(0,0.6)+  
  ylab("")+xlab("Composicion Educacion")

# Salud
PV33<-as.data.frame(ggpredict(M33, "Compocision.Isei"))
PV33$Opciones_Respuesta<-PV33$response.level

PPV33<-ggplot(data=PV33, aes(x=x, y=predicted, group=Opciones_Respuesta)) +
  geom_line(aes(color=Opciones_Respuesta))+
  scale_color_manual(values = c("black", "blue", "red"),
                     labels = c("Muy en Desacuerdo", "En desacuerdo","Ni de acuerdo ni en desacuero o (+)")) + 
  labs(x = "Opciones respuesta")+
  # theme_minimal()+
  theme(legend.position="bottom")+
  xlim(0,1)+ylim(0,0.6)+  
  ylab("")+xlab("Composicion Salud")

PVC313233<-ggarrange(PPV31,PPV32,PPV33, 
                    labels = c("","",""),
                    ncol = 3,
                    common.legend = TRUE, legend = "bottom",font.label = list(size = 10))

ggsave("C:/Users/gmobe/Dropbox/Redes Sociales/Articulo Redes Y Desigualdad/VPMCP.png", PVC313233, units ="cm", width = 20,height = 15)

# 
# zz<-cbind(as.data.frame(exp(coef(M11,thresholds =TRUE))),
#           (as.data.frame(exp(coef(M12,thresholds =TRUE)))),
#            (as.data.frame(exp(coef(M13,thresholds =TRUE)))))

