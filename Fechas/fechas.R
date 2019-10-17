# Agregamos las fechas con una función para después usarla en un data frame

dia_de_semana <- function(inicio,fin) { 
  # coen el argumento "dia" se puede elegir otro día que no sea viernes
  dia <- 5
  completa <- inicio + 0:6 #se generan 7 días hacia adelante para tener una semana
  dia_entero <- format(completa,"%w") # los pasámos a número entero entre 0 y 6
  #debe ser entre 0 y 6 por la forma en cómo R trata la fecha tipo Date, 0 es domingo en este caso
  #si uso entre 1:7 tendríamos que un viernes es el 6...
  dia_de_semana <- completa[as.numeric(dia_entero)==dia] #aquí ya buscamos el quinto día
  seq.Date(dia_de_semana,fin,by="week") 
  # generamos la fecha buscada, seq.Date es solo apra que quede en el formato adecuado
}

data["viernes"] <- as.Date(mapply(dia_de_semana, data$inicio, data$fin),lubridate::origin)


#########################################################################

library(plyr)
data_by_day = ddply(data, .(Product_ID, Price, Org.Price), transform, 
              day=seq(as.Date(Start.Date), as.Date(End.Date), by=1))

data_by_day = ddply(data, .(producto, precio, precio_sp), transform, 
                    day=seq(as.Date(inicio), as.Date(fin), by=1))
data_by_day <- data_by_day %>% arrange(desc(inicio))

data_by_day$dia <- as.numeric(format(data_by_day$day,"%w" ))

test <- data_by_day %>% group_by(day) %>% filter(dia == 6) %>% ungroup()

test %>% 
  group_by(day, producto, precio_sp) %>% dplyr::summarize(media = mean(precio))

#########################################################################



data_by_day = ddply(data, .(Product_ID, Price, Org.Price), transform, 
                    Saturday_Dt=seq(as.Date(inicio), as.Date(fin), by=1))
data_by_day$Day_num <- as.numeric(format(data_by_day$Saturday_Dt,"%w" ))
new_data <- data_by_day %>% 
  arrange(desc(inicio)) %>% 
  group_by(Saturday_Dt) %>% 
  filter(Day_num == 6) %>% 
  ungroup() %>% 
  group_by(Saturday_Dt, Product_ID, Org.Price) %>% 
  dplyr::summarize(media = mean(Price))




#########################################################################

data_by_day$dia <- format(seq(min(data_by_day$day),max(data_by_day$day), by=1),"%w") 

data_by_day %>% filter(dia %in% lista)



dia <- 5
inicio <- as.Date("2019-02-26")
fin <- as.Date("2019-03-27")

prueba <- dia_de_semana(inicio,fin)

format(completa,"%w")

completa <- inicio + 0:6 #se generan 7 en adelante para tener una semana

format(completa,"%w")

primer_dia_de_seamana <- completa[as.numeric(format(completa,"%w"))==dia]
seq.Date(primer_dia_de_seamana,fin,by="week")

seq.Date(inicio,fin,by="week")
