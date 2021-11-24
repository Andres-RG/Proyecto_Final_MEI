library(ggplot2)
library(tidyverse)
library(viridisLite)
library(viridis)
library(deSolve)
source("02_Functions/Functions.R")

################################################################################
#
#     Q U E R E T A R O
#
################################################################################

## 1 Hacer una gráfica apilada de casos positivos a covid por rangos de edades 
##   en (18-, 18-29,30-39,40-49,50-59,60-70, 70+)

###### Base de datos ######

casos_pos <- filter(datos_covid_qro, CLASIFICACION_FINAL == 1 | 
                           CLASIFICACION_FINAL == 2 |
                           CLASIFICACION_FINAL == 3 )

re <- rangos_edades(casos_pos$EDAD)

casos_pos_edad <- mutate(casos_pos, rango_edad = re)

#############################
### Casos por dia

pos <- c()
for (i in 1:length(casos_pos_edad$FECHA_INGRESO) ) {
  pos <- c(pos,1)
}
casos_pos_edad <- mutate(casos_pos_edad, positivos = pos)
p <- aggregate(positivos~FECHA_INGRESO, data = casos_pos_edad,
               FUN = sum)
p[,3] <- c(1:length(p$FECHA_INGRESO))
colnames(p)[3] <- "num.dia"

#############################
#############################


### Graficas

grafica__pos_rangos_edad <- ggplot(casos_pos_edad, 
                              aes(x=FECHA_INGRESO, y=EDAD, 
                                  fill = rango_edad)) + 
  geom_bar(position="stack", stat="identity") + 
  ggtitle("Casos positivos a COVID por rangos de edades 
          para el estado de Queretaro") + 
  labs(x="Tiempo", y="Casos") +
  labs(fill="Rangos de Edad") +
  theme(plot.title = element_text(hjust = 0.5))+
  theme(panel.background = element_rect(fill = "white"), 
        axis.line = element_line(colour = "black", size = 1)) +
  scale_fill_viridis(discrete = T)

jpeg("Casos positivos por rangos de edades.jpeg", width = 750, height = 350)
grafica__pos_rangos_edad
dev.off()

#######################################


## 2 Probabilidad de Hospitalizacion para casos positivos por rangos de edad

### Sin conmorb ###

casos_pos_sin_c <- filter(casos_pos_edad, 
                          DIABETES         == 2 
                          & EPOC           == 2
                          & ASMA           == 2
                          & INMUSUPR       == 2
                          & HIPERTENSION   == 2
                          & OTRA_COM       == 2
                          & CARDIOVASCULAR == 2
                          & OBESIDAD       == 2
                          & RENAL_CRONICA  == 2
                          & TABAQUISMO     == 2 )

casos_pos_sin_c_nh <- filter(casos_pos_sin_c, TIPO_PACIENTE == 1)
casos_pos_sin_c_h <- filter(casos_pos_sin_c, TIPO_PACIENTE == 2)

#### Probabilidades ####

p_casos_pos_sin_c_edad <- probabilidades(casos_pos_sin_c_nh, 
                                                    casos_pos_sin_c)
p_casos_pos_sin_c_edad

p_casos_pos_con_c_h <- probabilidades(casos_pos_sin_c_h, casos_pos_sin_c)

sum(casos_pos_sin_c_h$positivos)/sum(casos_pos_sin_c$positivos)

### Con alguna conmorb ###

casos_pos_con_c <- filter(casos_pos_edad, 
                          DIABETES         == 1 
                          | EPOC           == 1
                          | ASMA           == 1
                          | INMUSUPR       == 1
                          | HIPERTENSION   == 1
                          | OTRA_COM       == 1
                          | CARDIOVASCULAR == 1
                          | OBESIDAD       == 1
                          | RENAL_CRONICA  == 1
                          | TABAQUISMO     == 1 )

casos_pos_con_c_nh <- filter(casos_pos_con_c, TIPO_PACIENTE == 1)
casos_pos_con_c_h <- filter(casos_pos_con_c, TIPO_PACIENTE == 2)

#### Probabilidad ####

p_casos_pos_con_c_edad <- probabilidades(casos_pos_con_c_nh, casos_pos_con_c)
p_casos_pos_con_c_edad

p_casos_pos_sin_c_h <- probabilidades(casos_pos_con_c_h, casos_pos_con_c)

sum(casos_pos_con_c_h$positivos)/sum(casos_pos_con_c$positivos)



casos_pos_h <- filter(casos_pos_edad, TIPO_PACIENTE == 2)

p_casos_pos_h <- probabilidades(casos_pos_h, casos_pos_edad) 
sum(casos_pos_h$)/sum(casos_pos_edad$rango_edad)





## 3 Probabilidad de Intubación para casos positivos por rangos de edad

### Probablilidad de intubados sin conmorbilidades por edades ###

casos_pos_sin_c_i <- filter(casos_pos_sin_c, INTUBADO == 1)

p_casos_pos_sin_c_i <- probabilidades(casos_pos_sin_c_i, casos_pos_sin_c)

sum(casos_pos_sin_c_i$positivos)/sum(casos_pos_sin_c$positivos)

### Probabilidad de intubados con conmorbilidades por edades ###

casos_pos_con_c_i <- filter(casos_pos_con_c, INTUBADO == 1)

p_casos_pos_con_c_i <- probabilidades(casos_pos_con_c_i, casos_pos_con_c)

sum(casos_pos_con_c_i$positivos)/sum(casos_pos_con_c$positivos)


casos_pos_i <- filter(casos_pos_edad, INTUBADO == 1)
p_casos_pos_i <- probabilidades(casos_pos_i, casos_pos_edad)






## 4 Propabidad de muerte para casos positivos por rangos de edad

casos_pos_con_c <- mutate(casos_pos_con_c, muerte = c( ifelse( 
  !is.na( casos_pos_con_c$FECHA_DEF ), "Muerte", "No muerte") ) )

casos_pos_sin_c <- mutate(casos_pos_sin_c, muerte = c( ifelse( 
  !is.na( casos_pos_sin_c$FECHA_DEF ), "Muerte", "No muerte") ) )

### Sin conmorbilidades

casos_pos_sin_c_m <- filter(casos_pos_sin_c, muerte == "Muerte")

p_casos_pos_sin_c_m <- probabilidades(casos_pos_sin_c_m, casos_pos_sin_c)

sum(casos_pos_sin_c_m$positivos)/sum(casos_pos_sin_c$positivos)

### Con conmorbilidades

casos_pos_con_c_m <- filter(casos_pos_con_c, muerte == "Muerte")

p_casos_pos_con_c_m <- probabilidades(casos_pos_con_c_m, casos_pos_con_c)

sum(casos_pos_con_c_m$positivos)/sum(casos_pos_con_c$positivos)



casos_pos_edad <- mutate(casos_pos_edad, muerte = c( ifelse( 
  !is.na( casos_pos_edad$FECHA_DEF ), "Muerte", "No muerte") ) )
casos_pos_m <- filter(casos_pos_edad, muerte == "Muerte")
p_casos_pos_m <- probabilidades(casos_pos_m, casos_pos_edad)


### Tabla conjunta

prob_t <- cbind(p_casos_pos_con_c_h, p_casos_pos_sin_c_h,
      p_casos_pos_con_c_i, p_casos_pos_sin_c_i,
      p_casos_pos_con_c_m, p_casos_pos_sin_c_m)
colnames(prob_t) <- c("Hospitalizado con conmorbilidades",
                      "Hospitalizado sin conmorbilidades",
                      "Intubado con conmorbilidades",
                      "Intubado sin conmorbilidades",
                      "Muerte con conmorbilidades",
                      "Muerte sin conmorbilidades")
prob_t


