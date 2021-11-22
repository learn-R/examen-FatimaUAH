## 1. Cargar paquetes 

pacman::p_load(tidyverse,
               haven,
               magrittr,
               sjmisc,
               srvyr, 
               dplyr, 
               tidyr,
               kableExtra,
               sjPlot)

## 2. Cargar BBDD

ESI2020 <- haven::read_sav("https://www.ine.cl/docs/default-source/encuesta-suplementaria-de-ingresos/bbdd/spss_esi/2020/esi-2020---personas.sav?sfvrsn=a730ce58_4&download=true")

## 3. Selección de variables:

### fact_cal_esi = factor de expansi+on ESI con nueva calibración, proyecciones de población.
### d1_monto = ingresos por sueldos y salarios netos 
### edad = edad
### sexo = sexo
### B8 = En ese empleo ¿tiene contrato escrito?
### B9 = Duración del contrato definido o indefinido.

### 3.1 exploración de variables:
 frq(ESI2020$fact_cal_esi)
 frq(ESI2020$idrph)
 frq(ESI2020$sexo)
 frq(ESI2020$edad)
 frq(ESI2020$d1_monto)
 frq(ESI2020$b8)
 frq(ESI2020$b9)
 
 ### 3.2 selección de variables, corte de base de datos:
 
 datos_proc_esi2020 <- select(ESI2020, fact_cal_esi,idrph, edad, sexo, b8, b9, d1_monto) 
 
 ## 4. Recodificacion  y filtro de las variables a utilizar
 
 ### 4.1 recodificación variable sexo:
 
 datos_proc_esi2020 <- datos_proc_esi2020 %>% 
    mutate(sexo = case_when(sexo == 1 ~ "hombre" ,
                            sexo == 2 ~ "mujer",
                            TRUE ~ NA_character_)) 
 
 ### 4.2 Filtro Personas en edad de trabajar y Edad en tramos
 
 datos_proc_esi2020 <- datos_proc_esi2020 %>%
   filter(edad >= 15) %>% 
   mutate(edad_en_tramos = case_when(edad > 15 & edad <= 29 ~ "1= joven 15-29",
                                     edad >= 30 & edad <= 59 ~ "2= adulto 30-59",
                                     edad >= 60 ~ "3= adulto mayor 60+",
                                     TRUE ~ NA_character_)) %>% 
   na.omit()
 
 ### 4.3 filtro B8=sí=1, se dejan solo empleos con contrato escrito:
 
 datos_proc_esi2020 <- datos_proc_esi2020 %>% 
   filter(b8 == 1)

 ### 4.4 recodificación ingresos a ingresos en tramo:
 datos_proc_esi2020 <- datos_proc_esi2020 %>%
   mutate(salario_en_tramos = case_when(d1_monto == 0 ~ "0= 0",
                                     d1_monto >= 1 & d1_monto <= 150000 ~ "1= 1-150000",
                                     d1_monto >= 150000.1 & d1_monto <= 300000 ~ "2= 150000.1-300000",
                                     d1_monto >= 300000.1 & d1_monto <= 500000 ~ "3= 300000.1-500000",
                                     d1_monto >= 500000.1 & d1_monto <= 1000000 ~ "4= 500000.1-1000000",
                                     d1_monto >= 1000000.1 ~ "5= 1000000.1 +",
                                     TRUE ~ NA_character_))
 frq(datos_proc_esi2020$salario_en_tramos)
 
 ### 4.5 recodificación duración de contrato:
 
 datos_proc_esi2020 <- datos_proc_esi2020 %>% 
    mutate(duracion_contrato = case_when(b9 == 1 ~ "definido" ,
                                         b9 == 2 ~ "indefinido",
                                         TRUE ~ NA_character_)) %>% 
   na.omit()

 
 ## 5. Guardar base recortada:
 
 saveRDS(datos_proc_esi2020, file = "output/data/datos_proc_esi2020.rds")
 
 ## 6. Creación objeto encuesta 
 obj_encuesta_esi2020 <- datos_proc_esi2020 %>% 
    as_survey_design(ids = idrph, 
                     weights = fact_cal_esi)
 
## 7. Análisis de datos 
 
### 7.1 Tabla duracion de contrato general, poblacional. 
 duracion_contrato_porcentaje <- obj_encuesta_esi2020 %>%
    group_by(duracion_contrato) %>% 
    summarise(prop = survey_prop(vartype = "ci", level = .95, na.rm = T)) %>% 
    mutate(por = prop*100,
           prop_low = prop_low*100,
           prop_upp = prop_low*100)
 
    kbl(duracion_contrato_porcentaje) %>%
    kable_styling(bootstrap_options = c("striped", "hover"))
    
### 7.2 Tabla salario general, poblacional.
salario_en_tramos_porcentajes <- obj_encuesta_esi2020 %>%
       group_by(salario_en_tramos) %>% 
       summarise(prop = survey_prop(vartype = "ci", level = .95, na.rm = T)) %>% 
       mutate(por = prop*100,
              prop_low = prop_low*100,
              prop_upp = prop_low*100)
    
    kbl(salario_en_tramos_porcentajes) %>%
       kable_styling(bootstrap_options = c("striped", "hover"))

### 7.3 tabla sexo en general. 
    sexo_porcentajes <- obj_encuesta_esi2020 %>%
       group_by(sexo) %>% 
       summarise(prop = survey_prop(vartype = "ci", level = .95, na.rm = T)) %>% 
       mutate(por = prop*100,
              prop_low = prop_low*100,
              prop_upp = prop_low*100)
    
    kbl(sexo_porcentajes) %>%
       kable_styling(bootstrap_options = c("striped", "hover"))  

### 7.4 tabla edad en tramos en general. 
  edad_en_tramos_porcentajes <- obj_encuesta_esi2020 %>%
       group_by(edad_en_tramos) %>% 
       summarise(prop = survey_prop(vartype = "ci", level = .95, na.rm = T)) %>% 
       mutate(por = prop*100,
              prop_low = prop_low*100,
              prop_upp = prop_low*100)
    
    kbl(edad_en_tramos_porcentajes) %>%
       kable_styling(bootstrap_options = c("striped", "hover")) 

### 7.5 ingreso promedio y mediana de personas con contrato
    
    obj_encuesta_esi2020 %>% 
      summarize(salario_promedio = srvyr::survey_mean(d1_monto, vartype = "ci", level = 95, na.rm=T))
    
    salario_promedio <- obj_encuesta_esi2020 %>% 
      summarize(salario_promedio = srvyr::survey_mean(d1_monto, na.rm=T))    
 
    kbl(salario_promedio) %>%
      kable_styling(bootstrap_options = c("striped", "hover")) 
    
    
    obj_encuesta_esi2020 %>% 
      summarize(mediana = srvyr::survey_median(d1_monto, vartype = "ci", level = 95, na.rm=T))
    
### 7.6 ingreso medio por sexo
    
   salario_promedio_sexo <- obj_encuesta_esi2020 %>%
      group_by(sexo) %>% 
      summarise(salario_promedio_sexo = survey_mean(d1_monto, na.rm=T))
   
   kbl(salario_promedio_sexo) %>%
     kable_styling(bootstrap_options = c("striped", "hover")) 

### 7.7 tipo de contrato por sexo:
  
    contrato_duracion_sexo <- obj_encuesta_esi2020 %>% 
     group_by(sexo,duracion_contrato ) %>% 
     summarise(prop = survey_prop(vartype = "ci", na.rm = T), 
               total = survey_total(vartype = "ci", na.rm=T)) %>% 
     mutate(per = prop*100) %>% 
     select(sexo, duracion_contrato, per, total) %>% 
     pivot_wider(names_from = "duracion_contrato", 
                 values_from = c("per", "total")) 
   
     kbl(contrato_duracion_sexo) %>%
      kable_styling(bootstrap_options = c("striped", "hover"))
     
     

### 7.8 edad en tramo por ingreso medio:

   edad_salario_promedio <- obj_encuesta_esi2020 %>%  
     group_by(edad_en_tramos) %>% 
     summarise(salario_promedio = survey_mean(d1_monto, na.rm=T))
   
   kbl(edad_salario_promedio) %>%
     kable_styling(bootstrap_options = c("striped", "hover")) 

   
### 7.9 edad en tramo por tipo de contrato:
   
   duracion_contrato_edad <- obj_encuesta_esi2020 %>% 
     group_by(duracion_contrato, edad_en_tramos) %>% 
     summarise(prop = survey_prop(vartype = "ci", na.rm = T), 
               total = survey_total(vartype = "ci", na.rm=T)) %>% 
     mutate(per = prop*100) %>% 
     select(edad_en_tramos, duracion_contrato, per, total) %>% 
     pivot_wider(names_from = "duracion_contrato", 
                 values_from = c("per", "total")) 
   
   kbl(duracion_contrato_edad) %>%
     kable_styling(bootstrap_options = c("striped", "hover"))
   
### 7.10 duracion de contrato salario en tramos:
   
   duracion_contrato_salario_tramos <- obj_encuesta_esi2020 %>%
     group_by(duracion_contrato, salario_en_tramos) %>% 
     summarise(prop = survey_prop(vartype = "ci", na.rm = T), 
               total = survey_total(vartype = "ci", na.rm=T)) %>% 
     mutate(per = prop*100) %>% 
     select(salario_en_tramos, duracion_contrato, per, total) %>% 
     pivot_wider(names_from = "duracion_contrato", 
                 values_from = c("per", "total")) 
   
   kbl(duracion_contrato_salario_tramos) %>%
     kable_styling(bootstrap_options = c("striped", "hover"))
   
### 7.11 edad en tramos y salario en tramos:
   
   edad_salario_tramos <- obj_encuesta_esi2020 %>% 
     group_by(edad_en_tramos, salario_en_tramos) %>% 
     summarise(prop = survey_prop(vartype = "ci", na.rm = T), 
               total = survey_total(vartype = "ci", na.rm=T)) %>% 
     mutate(per = prop*100) %>% 
     select(salario_en_tramos, edad_en_tramos, per, total) %>% 
     pivot_wider(names_from = "salario_en_tramos", 
                 values_from = c("per", "total"))
   
   kbl(edad_salario_tramos) %>%
     kable_styling(bootstrap_options = c("striped", "hover"))

### 7.12 sexo y salario en tramos
   
   sexo_salario_tramos <- obj_encuesta_esi2020 %>% 
     group_by(sexo, salario_en_tramos ) %>% 
     summarise(prop = survey_prop(vartype = "ci", na.rm = T), 
               total = survey_total(vartype = "ci", na.rm=T)) %>% 
     mutate(per = prop*100) %>% 
     select(sexo,salario_en_tramos , per, total) %>% 
     pivot_wider(names_from = "salario_en_tramos", 
                 values_from = c("per", "total"))
   
   kbl(sexo_salario_tramos) %>%
     kable_styling(bootstrap_options = c("striped", "hover"))
   
   
### 7.13 salario promedio por sexo, intentos gráficos.   

   plot_frq(salario_promedio$salario_promedio)
            type = c("bar"),
            title = "grafico")

datos_proc_esi2020 %>% sjplot (datos_proc_esi2020, edad_en_tramos_porcentajes, fun = "grpfrq",
                  type = "dot", geom.colors = "Set1")

    
# Idea: factores sociodemográficos que afectan el sueldo y duración de contrato a nivel poblacional
# tengo que filtrar por personas con contrato, seleccionar las variables de sueldo y duración de contrato y evaluarlas según variables sociodemográficas, sexo, edad y nivel educacional.