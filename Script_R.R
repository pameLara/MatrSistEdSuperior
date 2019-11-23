matriculas <- read.table("csv/detalle_mtr.csv", sep = ",", header = TRUE, encoding = "UTF-8")

#cambio de nombres columnas
names(matriculas) <- c("acreditacion","cruch","programa","anio","comuna_institucion","calculo","nombre_region_1","subclasificacion","matricula_total",
                       "matricula_primer_anio","area_conocimiento","tipo_titulo_1","tipo_titulo_2","horario","estado","gratuidad","nombre_comuna",
                       "nombre_institucion","nombre_region_2","matr_primer_anio_hombres","matr_primer_anio_mujeres","propiedad_admision","tamanio",
                       "tipo_ies","mtr_total_hombres","mtr_total_mujeres","tradicional","anio","tipo_institucion","comuna","nombre_region_3")

#elimino columnas que no voy a usar
matriculas$cruch <- NULL
matriculas$programa <- NULL
matriculas$anio <- NULL
matriculas$calculo <- NULL
matriculas$nombre_region_1 <- NULL
matriculas$subclasificacion <- NULL
matriculas$tipo_titulo_2 <- NULL
matriculas$propiedad_admision <- NULL
matriculas$tamanio <- NULL
matriculas$tipo_ies <- NULL
matriculas$comuna <- NULL
matriculas$nombre_region_3 <- NULL
matriculas$matr_primer_anio_hombres <- NULL
matriculas$matr_primer_anio_mujeres <- NULL

#cambio de nombres columnas
names(matriculas) <- c("acreditacion","comuna_institucion","matricula_total","matr_primer_anio",
                       "area_conocimiento","tipo_titulo","horario","estado","gratuidad","comuna",
                       "institucion","region","mtr_total_hombres","mtr_total_mujeres","tradicional",
                       "anio","tipo_institucion")
#limpiar datos nulos
matriculas$mtr_total_hombres[is.na(matriculas$mtr_total_hombres)] <- 0
matriculas$mtr_total_mujeres[is.na(matriculas$mtr_total_mujeres)] <- 0

summary(matriculas)

#################
#grafico de matriculas por tipo de institucion
library(plyr)
library(dplyr)

mtr_x_inst <- ddply(matriculas, .(tipo_institucion, anio), summarize, total = sum(matricula_total))
mtr_x_inst


install.packages("highcharter")
library(highcharter)

hchart(mtr_x_inst, "line", 
        hcaes(x = anio, y = total, group = tipo_institucion)) %>% 
        hc_title(text = "Matrículas por Tipo de Institución") %>% 
        hc_xAxis(title = list(text = "Años")) %>% 
        hc_yAxis(title = list(text = "Total Matriculados"),
                 labels = list(format = "{value:,.0f}"), max = 80000) %>% 
        hc_add_theme(hc_theme_gridlight())

library(reshape2)
mtr_x_inst_ancho <- dcast(mtr_x_inst, tipo_institucion ~ anio, fun.aggregate = sum, value.var = "total") 
mtr_x_inst_ancho



breaks = seq(0, 70000, 10000)

library(ggplot2) 
ggplot(mtr_x_inst, aes(x=anio, y=breaks)) + 
  geom_line(colour="red")  + 
  geom_point(size=2, shape=21, fill="white", colour="red") + 
  theme_minimal()

ggplot(matriculas, aes(x=matriculas$anio, y=matriculas$matricula_total, group=matriculas$anio, colour=matriculas$tipo_institucion )) + 
  geom_line()  + 
  geom_point(size=2, shape=21, fill="white") + 
  theme_minimal()

sum(matriculas$matricula_total)

seq(0, 70, 5)

breaks=seq(10, 70, 5)







