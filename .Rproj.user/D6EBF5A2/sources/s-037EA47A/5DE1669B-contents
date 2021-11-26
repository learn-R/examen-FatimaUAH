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
 frq(ESI2020$b8)
 frq(ESI2020$b89)
 frq(ESI2020$a1)
 frq(ESI2020$b2)
 descr(ESI2020$d1_monto)
 descr(ESI2020$edad)
 ### 3.2 selección de variables, corte de base de datos:
 
 datos_proc_esi2020 <- select(ESI2020, fact_cal_esi,idrph, edad, sexo, contrato_escito = b8, duracion_contrato1 = b9, salario = d1_monto) 
 
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
   filter(contrato_escito == 1)

 ### 4.4 recodificación ingresos a ingresos en tramo:
 datos_proc_esi2020 <- datos_proc_esi2020 %>%
   mutate(salario_en_tramos = case_when(salario == 0 ~ "0= 0",
                                     salario >= 1 & salario <= 150000 ~ "1= 1-150000",
                                     salario >= 150000.1 & salario <= 300000 ~ "2= 150000.1-300000",
                                     salario >= 300000.1 & salario <= 500000 ~ "3= 300000.1-500000",
                                     salario >= 500000.1 & salario <= 1000000 ~ "4= 500000.1-1000000",
                                     salario >= 1000000.1 ~ "5= 1000000.1 +",
                                     TRUE ~ NA_character_))
 frq(datos_proc_esi2020$salario_en_tramos)
 descr(datos_proc_esi2020$salario)
 
 
 ### 4.5 recodificación duración de contrato:
 
 datos_proc_esi2020 <- datos_proc_esi2020 %>% 
    mutate(duracion_contrato = case_when(duracion_contrato1 == 1 ~ "definido" ,
                                         duracion_contrato1 == 2 ~ "indefinido",
                                         TRUE ~ NA_character_)) %>% 
   na.omit()

 
 ## 5. Guardar base recortada:
 
 saveRDS(datos_proc_esi2020, file = "output/data/datos_proc_esi2020.rds")
 
 ## 6. Creación objeto encuesta 
 obj_encuesta_esi2020 <- datos_proc_esi2020 %>% 
    as_survey_design(ids = idrph, 
                     weights = fact_cal_esi)
 
