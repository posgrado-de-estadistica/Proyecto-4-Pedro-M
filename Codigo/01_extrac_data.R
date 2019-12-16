# Cargamos la librerías que se utilizarán

library(lubridate)
library(dplyr)
library(ggplot2)
library(RDS)

options(scipen = 999)

# carga de datos crudos

load("E:/nacimientos.Rdata")
censo_11 <- read_excel("Datos/censo_11.xlsx") %>% mutate(cod_pc= as.character(cod_dist)) %>% dplyr::select(-cod_dist)

nacimientos_t <- nacimientos %>% 
    mutate(Fecha_Nacimiento = as.Date(as.character(Fecha_Nacimiento), "%Y%m%d"),
           anio = format(Fecha_Nacimiento,"%Y"),
           Canton_Procedencia_Madre= as.numeric(Canton_Procedencia_Madre),
           cod_pc = ifelse(Canton_Procedencia_Madre>9, 
                           paste0(Provincia_Procedencia_Madre,Canton_Procedencia_Madre),
                           paste0(Provincia_Procedencia_Madre,"0",Canton_Procedencia_Madre)
                           )
           )  
           
pdf("Imagenes/exploratorio_1.pdf")
nacimientos_t %>% filter(cod_pc == "301") %>%
    filter(Provincia_Procedencia_Madre != 0) %>% 
    filter(Provincia_Procedencia_Madre != 8) %>% 
    filter(Provincia_Procedencia_Madre != 9) %>% 
    filter(Fecha_Nacimiento > "1994-01-01") %>% 
    filter(Fecha_Nacimiento < "2012-01-01") %>% 
    group_by(anio, cod_pc) %>% summarise(n=n()) %>% 
    ggplot(aes(x = anio, y = n, group=1)) +
    geom_point() +
    geom_line() +
    labs(x = "Año", y = "Cantidad de nacimientos", 
         title = "Nacimientos registrados en maestro de nacimientos \n provincia de Cartago cantón central  1994 - 2011")
dev.off()

pdf("Imagenes/exploratorio_2.pdf")
nacimientos_t %>% filter(cod_pc == "101") %>% 
    filter(Provincia_Procedencia_Madre != 0) %>% 
    filter(Provincia_Procedencia_Madre != 8) %>% 
    filter(Provincia_Procedencia_Madre != 9) %>% 
    filter(Fecha_Nacimiento > "1994-01-01") %>% 
    filter(Fecha_Nacimiento < "2012-01-01") %>% 
    group_by(anio, cod_pc) %>% summarise(n=n()) %>% 
    ggplot(aes(x = anio, y = n, group=1)) +
    geom_point() +
    geom_line() +
    labs(x = "Año", y = "Cantidad de nacimientos", 
         title = "Nacimientos registrados en maestro de nacimientos \n provincia de San josé cantón San josé  1994 - 2011")
dev.off()

pdf("Imagenes/exploratorio_31.pdf")

nacimientos_t %>%
    filter(Provincia_Procedencia_Madre != 0) %>% 
    filter(Provincia_Procedencia_Madre != 8) %>% 
    filter(Provincia_Procedencia_Madre != 9) %>% 
    filter(Fecha_Nacimiento > "1994-01-01") %>% 
    filter(Fecha_Nacimiento < "2012-01-01") %>% 
    group_by(anio) %>% summarise(n=n()) %>% 
    ggplot(aes(x = anio, y = n, group=1)) +
    geom_point() +
    geom_line() +
    labs(x = "Año", y = "Cantidad de nacimientos", 
         title = "Nacimientos registrados en maestro de nacimientos \n Costa Rica 1994 - 2011")

dev.off()

# Para el modelo de poisson y obtener la tendecia vamos a acotar por años que presenten una distribucción lógica, excluyendo los datos con registros de provincia 8 y 9 que son registros tardidos.


nacimientos_t_spread <- nacimientos_t %>% 
    filter(Fecha_Nacimiento > "2000-01-01" & Fecha_Nacimiento <"2012-01-01") %>% 
    filter(Provincia_Procedencia_Madre != 0) %>% 
    filter(Provincia_Procedencia_Madre != 8) %>% 
    filter(Provincia_Procedencia_Madre != 9) %>% 
    filter(cod_pc != "000") %>% 
    group_by(anio,cod_pc) %>% 
    summarise(nac_t= n()) %>% 
    spread(anio,nac_t) %>% 
    filter(cod_pc %in% c(101,
                         102,
                         103,
                         104,
                         105,
                         106,
                         107,
                         108,
                         109,
                         110,
                         111,
                         112,
                         113,
                         114,
                         115,
                         116,
                         117,
                         118,
                         119,
                         120,
                         201,
                         202,
                         203,
                         204,
                         205,
                         206,
                         207,
                         208,
                         209,
                         210,
                         211,
                         212,
                         213,
                         214,
                         215,
                         301,
                         302,
                         303,
                         304,
                         305,
                         306,
                         307,
                         308,
                         401,
                         402,
                         403,
                         404,
                         405,
                         406,
                         407,
                         408,
                         409,
                         410,
                         501,
                         502,
                         503,
                         504,
                         505,
                         506,
                         507,
                         508,
                         509,
                         510,
                         511,
                         601,
                         602,
                         603,
                         604,
                         605,
                         606,
                         607,
                         608,
                         609,
                         610,
                         611,
                         701,
                         702,
                         703,
                         704,
                         705,
                         706) ) %>% 
    replace(is.na(.), 1) %>%
    mutate(tot_nac = rowSums(.[2:12])) %>% 
    left_join(censo_11, by= c("cod_pc"="cod_pc"))

nacimientos_t_spread<- nacimientos_t_spread %>% 
    mutate(t_11= (`2011`/n11),
           t_10= (`2010`/n10),
           t_9= (`2009`/n9),
           t_8= (`2008`/n8),
           t_7= (`2007`/n7),
           t_6= (`2006`/n6),
           t_5= (`2005`/n5),
           t_4= (`2004`/n4),
           t_3= (`2003`/n3),
           t_2= (`2002`/n2),
           t_1= (`2001`/n1),
           t_0= (`2001`/n0)) %>% 
    group_by(cod_pc) %>% 
    mutate(tvim=mean(c((t_1-t_0)/t_1,
(t_2-t_1)/t_2,
(t_3-t_2)/t_3,
(t_4-t_3)/t_4,
(t_5-t_4)/t_5,
(t_6-t_5)/t_6,
(t_7-t_6)/t_7,
(t_8-t_7)/t_8,
(t_9-t_8)/t_9,
(t_10-t_9)/t_10,
(t_11-t_10)/t_11))) %>%
    mutate(tvim=ifelse(-1 > tvim,(t_11-t_0)/t_11,tvim )) %>% 
    mutate(t_0_1=t_0*1000,
           t_1_1=t_1*1000,
           t_2_1=t_2*1000,
           t_3_1=t_3*1000,
           t_4_1=t_4*1000,
           t_5_1=t_5*1000,
           t_6_1=t_6*1000,
           t_7_1=t_7*1000,
           t_8_1=t_8*1000,
           t_9_1=t_9*1000,
           t_10_1=t_10*1000,
           t_11_1=t_11*1000,
           ti10=(t_11-t_0)/t_11,
           )

nacimientos_t_spread_f <- nacimientos_t_spread %>% 
    dplyr::select(cod_pc,Provincia, Canton,tvim,t_0_1,t_1_1,t_2_1,
                  t_3_1,t_4_1,t_5_1,t_6_1,t_7_1,t_8_1,t_9_1,t_10_1,t_11_1,ti10)

saveRDS(nacimientos_t_spread_f, file="Datos/nac_final.Rds")


# Estimación con un modelo de poisson

nac_t_spread <- nacimientos_t %>% 
    filter(Fecha_Nacimiento > "2000-01-01" & Fecha_Nacimiento <"2011-01-01") %>% 
    filter(Provincia_Procedencia_Madre != 0) %>% 
    filter(Provincia_Procedencia_Madre != 8) %>% 
    filter(Provincia_Procedencia_Madre != 9) %>% 
    filter(cod_pc != "000") %>% 
    group_by(anio,cod_pc) %>% 
    summarise(nac_t= n()) %>% 
    filter(cod_pc %in% c(101,
                         102,
                         103,
                         104,
                         105,
                         106,
                         107,
                         108,
                         109,
                         110,
                         111,
                         112,
                         113,
                         114,
                         115,
                         116,
                         117,
                         118,
                         119,
                         120,
                         201,
                         202,
                         203,
                         204,
                         205,
                         206,
                         207,
                         208,
                         209,
                         210,
                         211,
                         212,
                         213,
                         214,
                         215,
                         301,
                         302,
                         303,
                         304,
                         305,
                         306,
                         307,
                         308,
                         401,
                         402,
                         403,
                         404,
                         405,
                         406,
                         407,
                         408,
                         409,
                         410,
                         501,
                         502,
                         503,
                         504,
                         505,
                         506,
                         507,
                         508,
                         509,
                         510,
                         511,
                         601,
                         602,
                         603,
                         604,
                         605,
                         606,
                         607,
                         608,
                         609,
                         610,
                         611,
                         701,
                         702,
                         703,
                         704,
                         705,
                         706) ) %>% 
    replace(is.na(.), 1) %>%
    arrange(cod_pc) %>%
    group_by(cod_pc) %>% left_join(censo_11 %>% 
    gather("year", "cases", 1:12) %>%
    mutate(anio=ifelse(year=="n0",2000,
                    ifelse(year=="n1",2001,
                    ifelse(year=="n2",2002,
                    ifelse(year=="n3",2003,
                    ifelse(year=="n4",2004,
                    ifelse(year=="n5",2005,
                    ifelse(year=="n6",2006,
                    ifelse(year=="n7",2007,
                    ifelse(year=="n8",2008,
                    ifelse(year=="n9",2009,
                    ifelse(year=="n10",2010,
                    ifelse(year=="n11",2011,0)))))))))))),
    anio= as.character(anio),
    cod_pc= as.character(cod_pc)))
   
p <- list()
dic <- n %>% distinct(cod_pc)
for(i in 1:81){
mod_poisson <- glm(nac_t~cases,nac_t_spread %>% filter(cod_pc %in% dic$cod_pc[[i]]), family = "poisson")

p[[i]] <- mod_poisson$coefficients[2]

}

p_t <- data.frame(estimates=(unlist(p))*100)
p_t_or <- data.frame(estimates=(exp(unlist(p))-1)*100)
