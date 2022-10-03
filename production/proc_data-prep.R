rm(list=ls())
if (!require("pacman")) install.packages("pacman") # instalar pacman
# cargar librerias
pacman::p_load(dplyr,       # Manipulacion de datos
               knitr,
               kableExtra,
               summarytools,
               sjlabelled,
               sjPlot,
               sjmisc,
               ggplot2,
               ggpubr,
               MASS,
               texreg
)


# Datos -------------------------------------------------------------------
load("input/data/original/ELSOC_W01_v2.00.RData");   COESW<- elsoc_w01; remove(elsoc_w01)

##Selección de variables
Bd_tesis<-dplyr::select(COESW, idencuesta, ola, version, muestra, 
                        sospechoso,
                        starts_with("r01"), 
                        starts_with("d02"), 
                        c15,
                        m0_sexo,m0_edad,m01,m03,m36,
                        c18_02,c18_03,d01_01)

Bd_tesis[Bd_tesis ==-999] <- NA
Bd_tesis[Bd_tesis ==-888] <- NA
##Pasar a missing las variables
# view_df(Bd_tesis)
###Limpiar los valores fuera de rango (-888, -999) 

#Se eliminan los casos sospechosos
Bd_tesis <- subset(Bd_tesis, casos_sospechosos==0)


## Controles ---------------------------------------------------------------
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
# Igualitarismo
Bd_tesis$igualit<-(Bd_tesis$c18_02+Bd_tesis$c18_03)/2

# Estatus subjetivo
Bd_tesis$ess <- Bd_tesis$d01_01

# variable promedio de justicia
Bd_tesis$justicia <- as.numeric((Bd_tesis$d02_01+Bd_tesis$d02_02+Bd_tesis$d02_03)/3)
sjmisc::frq(Bd_tesis$justicia)

## Tamannio de la Red ------------------------------------------------------

##OCupaciones (R1)
#Selección de variables
R01<-as.data.frame(dplyr::select(Bd_tesis, idencuesta, starts_with("r01_")))

# view_df(R01,show.frq = T)

#Asignar valor medio a cada categoría de respuesta
# x<-c(0,1,3,6,9,13,16,0,0) 

varname<- R01 %>% select(starts_with("r01_")) %>% names()

for (i in varname) {
  R01[[i]] <- car::recode(R01[[i]],"1=0;2=1;3=3;4=6;6=9;6=13;7=16;c(-888,-999)=NA")
}
summary(R01)
R01[is.na(R01)] <-0
summary(R01)
#Sumar el cálculo
R01$size01<-rowSums(R01[,2:14])

#Chequeo
# frequency::freq(R01$size01)

#Hay que eliminar casos con valor 0
R01<-R01%>% 
  filter(size01>0)

Ocupaciones.n <-
  as.data.frame(cbind(
    c(
      "Gerente empresa multinacional",
      "Vendedor ambulante",
      "Secretario",
      "Mecanico de autos",
      "Vendedor de tienda",
      "Abogado",
      "Aseador",
      "Medico",
      "Parvulario",
      "Chofer de taxi",
      "Camarero",
      "Contador",
      "Profesor Universitario",
      "Total"
    )
  ))

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


#Generar Data Red Extensa------------------------------------------------------

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


## Heterogeneidad de la RED ------------------------------------------------
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
D2<-dplyr::select(Bd_tesis, idencuesta,d02_01,d02_02,d02_03,justicia,
                  edcat,sexo,educ,edad,ppolcat,c18_02,c18_03, igualit,ess)
Red<-left_join(Red,D2,by = c("idencuesta"))
Red<- (na.omit(Red))

## Composition de la RED ---------------------------------------------------
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


## SCALE UP (tamannio de la red) ----------------------------------------------------------------
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


# Analysis ----------------------------------------------------------------

## SimpsonD
Red$d02_01[Red$d02_01>3]<-3
Red$d02_02[Red$d02_02>3]<-3
Red$d02_03[Red$d02_03>3]<-3

## Diversidad 
M11= lm(justicia~SimpsonD.1+scaleup1+
            sexo+edcat+ess+ppolcat+edad+igualit, data=Red)
## Composicion
M21 = lm(justicia~Compocision.Isei+scaleup1+ 
           sexo+edcat+ess+ppolcat+edad+igualit, data=Red)

# Composicion y Diversidad
M31 = estimatr::lm_robust(justicia~SimpsonD.1+Compocision.Isei+scaleup1+
             sexo+edcat+ess+ppolcat+edad+igualit, data=Red)

omit <- "((Intercept))|(sexo)|(edcat)|(ppolcat)|(edad)|(igualit)|(scaleup1)|(ess)"
names <- c("Diversidad","Composición")
screenreg(list(M11,M21,M31),
          custom.coef.names = names,
          omit.coef = omit,
          include.ci = FALSE)

sjPlot::plot_model(M31,type = "est",
                   terms = c("SimpsonD.1","Compocision.Isei"),
                   axis.labels = c("Diversidad","Composición"))

sjPlot::plot_model(M31,type = "pred",
                   terms = c("SimpsonD.1"),
                   axis.labels = c("Diversidad"))

sjPlot::plot_model(M31,type = "pred",
                   terms = c("Compocision.Isei"),
                   axis.labels = c("Diversidad"))

