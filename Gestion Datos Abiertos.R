# SCRIP PARA LA PUBLICACIÓN DE LOS DATOS ABIERTOS DE LA UNIVERSIDAD NACIONAL DE 
# COLOMBIA DERIVADOS DE LAS FUENTES QUE SIRVEN DE BASE PARA LA PUBLICACIÓN DE 
# LAS ESTADÍSTICAS OFICIALES.

# Librerías requeridas

library(UnalData)
library(dplyr)
library(tidyr)

# Pob 1. Aspirantes ----

Aspirantes <- UnalData::Aspirantes %>% 
              filter((YEAR >= 2019 & TIPO_NIVEL == "Postgrado" & MOD_INS == "Regular")| 
                     (YEAR >= 2019 & TIPO_NIVEL == "Pregrado" & !is.na(TIPO_INS))) %>% 
              select(-c(ID, CAT_EDAD, ESTRATO, ADM_SEDE_NOMBRE: FACULTAD_S, 
                        PROGRAMA_S, `SNIES UNIVERSIDAD`:TITULOUNIVERSITARIO, 
                        AÑO_TERMINACION)) %>% 
              rename(EDAD = EDAD_MOD, ESTRATO = ESTRATO_ORIG) %>% 
              relocate(c(TIPO_NIVEL, NIVEL), .after = SEMESTRE) %>% 
              mutate(INS_SEDE_NOMBRE = if_else(is.na(INS_SEDE_NOMBRE), "Universidad", INS_SEDE_NOMBRE)) # Transformación de datos


Pruebas <- Aspirantes %>% group_by(YEAR, SEMESTRE, INS_SEDE_NOMBRE) %>%
           count()
           
# Pob 2. Admitidos ----
# Pob 3. Matriculados ----
# Pob 4. Matriculados Pvez ----
# Pob 5. Graduados ----
# Pob 6. Docentes ----
# Pob 7. Administrativos ----
# Pob 8. SaberPro ----