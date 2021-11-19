## 1. Cargar paquetes 

pacman::p_load(tidyverse, haven, sjmisc, sjPlot, magrittr, car)

## 2. Cargar BBDD

ESI2020 <- haven::read_sav("https://www.ine.cl/docs/default-source/encuesta-suplementaria-de-ingresos/bbdd/spss_esi/2020/esi-2020---personas.sav?sfvrsn=a730ce58_4&download=true")

## 3. Selección de variables:

### fact_cal = factor de expansión trimestral con nueva calibración, proyecciones...
### fact_cal_esi = factor de expansi+on ESI con nueva calibración, proyecciones de población (esta es)
### d1_monto = ingresos por sueldos y salarios netos 
### edad = edad
### sexo = sexo
### B8 = En ese empleo ¿tiene contrato escrito?
### B9 = Duración del contrato def o indef
### d1_opcion = el trabajo actual es el mismo del mes anterior

### 3.1 exploración de variables:
 frq(ESI2020$fact_cal_esi)
 frq(ESI2020$idrph)
 frq(ESI2020$sexo)
 frq(ESI2020$edad)
 frq(ESI2020$d1_opcion)
 frq(ESI2020$d1_monto)
 frq(ESI2020$b8)
 frq(ESI2020$b9)
 
 ### 3.2 selección de variables, corte de base de datos:
 
 datos_proc_esi2020 <- select(ESI2020, fact_cal_esi,idrph, edad, sexo, d1_opcion, b8, b9, d1_monto) 
 
 ## 4. Recodificacion  y filtro de las variables a utilizar
 
 ### 4.1 Filtro Personas en edad de trabajar y Edad en tramos
 
 datos_proc_esi2020 <- datos_proc_esi2020 %>%
   filter(edad >= 15) %>% 
   mutate(edad_en_tramos = case_when(edad > 15 & edad <= 29 ~ "joven",
                                     edad >= 30 & edad <= 60 ~ "adulto",
                                     edad >= 61 ~ "adulto mayor",
                                     TRUE ~ NA_character_)) 
 ### 4.2 filtro B8=sí=1, se dejan solo empleos con contrato escrito:
 
 datos_proc_esi2020 <- datos_proc_esi2020 %>% 
   filter(b8 == 1)

 ### 4.3 recodificación ingresos a ingresos en tramo:
 datos_proc_esi2020 <- datos_proc_esi2020 %>%
   mutate(ingreso_en_tramos = case_when(d1_monto == 0 ~ "0",
                                     d1_monto >= 1 & d1_monto <= 150000 ~ "0-150000",
                                     d1_monto >= 150000.1 & d1_monto <= 300000 ~ "150000.1-300000",
                                     d1_monto >= 300000.1 & d1_monto <= 500000 ~ "300000.1-500000",
                                     d1_monto >= 500000.1 & d1_monto <= 1000000 ~ "500000.1~1000000",
                                     d1_monto >= 1000000.1 ~ "+1000000.1",
                                     TRUE ~ NA_character_)) 
 frq(datos_proc_esi2020$ingreso_en_tramos)
 
 
# Idea: factores sociodemográficos que afectan el sueldo y duración de contrato a nivel poblacional
# tengo que filtrar por personas con contrato, seleccionar las variables de sueldo y duración de contrato y evaluarlas según variables sociodemográficas, sexo, edad y nivel educacional.