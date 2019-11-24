matriculas <- read.table("recursos/detalle_mtr.csv", sep = ",", header = TRUE, encoding = "UTF-8")

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

grafico1 <- hchart(mtr_x_inst, "line", 
                   hcaes(x = anio, y = total, group = tipo_institucion)) %>% 
                   hc_xAxis(categories = c(seq(2005,2019)),
                            title = list(text = "")) %>% 
                   hc_yAxis(title = list(text = ""),
                            tickInterval = 10000,
                            labels = list(format = "{value:,.0f}"), max = 80000) %>% 
                   hc_add_theme(hc_theme_538())


grafico1 <- hchart(mtr_x_inst, "scatter", 
                   hcaes(x = anio, y = total, group = tipo_institucion)) %>% 
                    hc_xAxis(categories = c(seq(2005,2019)),
                             title = list(text = "")) %>% 
                    hc_yAxis(title = list(text = ""),
                             tickInterval = 10000,
                             labels = list(format = "{value:,.0f}"), max = 80000)%>% 
                    hc_add_theme(hc_theme_538())

grafico1


library(knitr)
library(reshape2)
mtr_x_inst_ancho <- dcast(mtr_x_inst, tipo_institucion ~ anio, fun.aggregate = sum, value.var = "total") 
names(mtr_x_inst_ancho)  <- c("Tipo Institución", "2005","2006","2007","2008",
                              "2009","2010","2011","2012","2013","2014","2015",
                              "2016","2017","2018","2019")
sep.miles <- function(x){
              format(x,big.mark=".")
             }

mtr_separado <- sep.miles(mtr_x_inst_ancho)

kable(mtr_separado, caption = "") %>%
  kable_styling(bootstrap_options = c("striped", "hover"), full_width = F, font_size = 12)

#################
#grafico de matriculas por sexo
columnas <- c("mtr_total_hombres", "mtr_total_mujeres", "anio")
mtr_por_sexo <- matriculas[columnas]
head(mtr_por_sexo)

mtr_hom <- ddply(mtr_por_sexo, .(anio), summarize, total = sum(mtr_total_hombres))
mtr_hom$sexo <- "Hombres"

mtr_muj <- ddply(mtr_por_sexo, .(anio), summarize, total = sum(mtr_total_mujeres))
mtr_muj$sexo <- "Mujeres"

mtr_hom_muj <- rbind(mtr_hom, mtr_muj)

mtr_hom_muj <- dcast(mtr_hom_muj, sexo ~ anio, fun.aggregate = sum, value.var = "total") 
mtr_hom_muj
mtr_hom_muj <- melt(mtr_hom_muj)
names(mtr_hom_muj) <- c("Sexo", "anio", "total")
head(mtr_hom_muj)

grafico2 <- ggplot(mtr_hom_muj, aes(x=anio, y=total, fill=Sexo)) +
            xlab("Años") + ylab("Total") +
            geom_bar(stat="identity", position=position_dodge()) +
            scale_fill_manual(values=c("#80FF91", "#E6DB91"))
grafico2 <- grafico2 + guides(fill=guide_legend(title=""))


library(plotly)
ggplotly(grafico2)

#################
matriculas_carrera <- read.table("recursos/detalle_mtr_x_carreras.csv", sep = ",", header = TRUE, encoding = "UTF-8")

names(matriculas_carrera) <- c("cruch","programas","anio_eliminar","calculo_eliminar","region_eliminar1","mtr_total",
                               "mtr_primer_anio","area_conocimiento","carrera","tipo_titulo","tipo_titulo_eliminar",
                               "horario","comuna","institucion","region_eliminar2","mtr_primer_hombres","mtr_primer_mujeres",
                               "mtr_total_hombres","mtr_total_mujeres","anio","region")

matriculas_carrera$cruch <- NULL
matriculas_carrera$programas <- NULL
matriculas_carrera$anio_eliminar <- NULL
matriculas_carrera$calculo_eliminar <- NULL
matriculas_carrera$region_eliminar1 <- NULL
matriculas_carrera$tipo_titulo_eliminar <- NULL
matriculas_carrera$region_eliminar2 <- NULL

instituciones <- read.table("recursos/todo_institucionesRM.csv", sep = ",", header = TRUE, encoding = "UTF-8")
instituciones <- instituciones[,c(18,29)]
names(instituciones) <- c("institucion", "tipo_institucion")

matriculas_carrera$mtr_primer_hombres[is.na(matriculas_carrera$mtr_primer_hombres)] <- 0
matriculas_carrera$mtr_primer_mujeres[is.na(matriculas_carrera$mtr_primer_mujeres)] <- 0
matriculas_carrera$mtr_total_hombres[is.na(matriculas_carrera$mtr_total_hombres)]	<- 0
matriculas_carrera$mtr_total_mujeres[is.na(matriculas_carrera$mtr_total_mujeres)]	<- 0

instituciones <- distinct(instituciones, institucion, tipo_institucion)

matriculas_carrera <- merge (matriculas_carrera, instituciones, by = "institucion")

mujer <- ddply(matriculas_carrera, .(carrera), summarize, total_mujeres = sum(mtr_total_mujeres))
hombre <- ddply(matriculas_carrera, .(carrera), summarize, total_hombres = sum(mtr_total_hombres))

total <- merge (mujer, hombre, by = "carrera")
total$total <- total$total_mujeres + total$total_hombres

names(total) <- c("Carreras", "Total Mujeres", "Total Hombres", "Total Matriculados")

kable(total_matr, caption = "") %>%
  kable_styling(bootstrap_options = c("striped", "hover"), full_width = F, font_size = 12, position = "left")

########

#top instituciones
mtr_total_inst <- ddply(matriculas_carrera, .(institucion,carrera,tipo_institucion), summarize, total = sum(mtr_total))

mtr_carr_1 <- mtr_total_inst %>%
              filter(carrera == "Ingeniería Civil en Computación y/o Informática") %>%
              arrange(desc(total)) %>%
              head(3)

mtr_carr_2 <- mtr_total_inst %>%
              filter(carrera == "Ingeniería en Computación e Informática y similares") %>%
              arrange(desc(total)) %>%
              head(3)

mtr_carr_3 <- mtr_total_inst %>%
              filter(carrera == "Técnico en Análisis de Sistemas Informáticos") %>%
              arrange(desc(total)) %>%
              head(3)

mtr_carr_4 <- mtr_total_inst %>%
              filter(carrera == "Técnico en Computación e Informática") %>%
              arrange(desc(total)) %>%
              head(3)

mtr_carreras <- rbind(mtr_carr_1,mtr_carr_2,mtr_carr_3,mtr_carr_4)

mtr_carreras

names(mtr_carreras) <- c("Institución", "Carrera", "Tipo Institución", "Total")

mtr_carreras <- sep.miles(mtr_carreras)

kable(mtr_carreras[, c(1,4)], caption = "Group Rows") %>% 
      kable_styling("striped", full_width = F) %>%
      group_rows("Carrera: Ingeniería Civil en Computación y/o Informática", 1, 3) %>%
      group_rows("Carrera: Ingeniería en Computación e Informática y similares", 4, 6) %>%
      group_rows("Carrera: Técnico en Análisis de Sistemas Informáticos", 7, 9) %>%
      group_rows("Carrera: Técnico en Computación e Informática", 10, 12)

########################

mtr_var <- melt(mtr_x_inst_ancho)
names(mtr_var) <- c("tipo_inst", "anio", "total")
arrange(mtr_var, desc(tipo_inst,anio))

mtr_univ <- mtr_var[mtr_var$tipo_inst == 'Univ.',]
mtr_ip <- mtr_var[mtr_var$tipo_inst == 'I.P.',]
mtr_cft <- mtr_var[mtr_var$tipo_inst == 'C.F.T.',]

v <- function(x)
    {
      argu <- x$total
      x <- argu[1:15]
      y <- c(0,argu[1:14])
      
      argu <- ((x-y)*100)/y
      argu <- round(argu,1)
      argu[is.infinite(argu)] <- 0
      
      x$var <- argu
    }

mtr_univ$var <- v(mtr_univ)
mtr_ip$var <- v(mtr_ip)
mtr_cft$var <- v(mtr_cft)

mtr_var <- rbind(mtr_univ,mtr_ip,mtr_cft)

mtr_var <- dcast(mtr_var, tipo_inst ~ anio, value.var = "var") 

mtr_var <- mtr_var[,c(1,seq(3,16))]
mtr_var <- melt(mtr_var)
names(mtr_var) <- c("tipo_inst", "anio", "var")

grafico3 <- hchart(mtr_var, "areaspline", 
                   hcaes(x = anio, y = var, group = tipo_inst)) %>% 
                    hc_xAxis(categories = c(seq(2006,2019)),
                             title = list(text = "")) %>% 
                    hc_yAxis(title = list(text = ""),
                             tickInterval = 5,
                             labels = list(format = "{value}%"), max = 30) %>% 
                    hc_add_theme(hc_theme_flat())
grafico3
