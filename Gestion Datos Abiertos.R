# SCRIP PARA LA PUBLICACIÓN DE LOS DATOS ABIERTOS DE LA UNIVERSIDAD NACIONAL DE 
# COLOMBIA DERIVADOS DE LAS FUENTES QUE SIRVEN DE BASE PARA LA PUBLICACIÓN DE 
# LAS ESTADÍSTICAS OFICIALES.

# Librerías requeridas

library(UnalData)
library(dplyr)
library(tidyr)

# Pob 1. Programas Académicos ----
# Pob 2. Aspirantes y Admitidos ----

Aspirantes <- UnalData::Aspirantes %>% 
              filter((YEAR >= 2019 & TIPO_NIVEL == "Postgrado" & MOD_INS == "Regular")| 
                     (YEAR >= 2019 & TIPO_NIVEL == "Pregrado" & !is.na(TIPO_INS))) %>% 
              select(-c(ID, CAT_EDAD, ESTRATO, ADM_SEDE_NOMBRE: FACULTAD_S, 
                        PROGRAMA_S, `SNIES UNIVERSIDAD`:TITULOUNIVERSITARIO, 
                        AÑO_TERMINACION)) %>% 
              rename(EDAD = EDAD_MOD, ESTRATO = ESTRATO_ORIG) %>% 
              relocate(c(TIPO_NIVEL, NIVEL), .after = SEMESTRE) %>% 
              mutate(INS_SEDE_NOMBRE = ifelse(is.na(INS_SEDE_NOMBRE), "Universidad", INS_SEDE_NOMBRE),   # Transformación de datos                 
                     NACIONALIDAD = ifelse(NACIONALIDAD == "Colombiana\r\n", "Colombiana", NACIONALIDAD),
                     SNIES_SEDE = ifelse(YEAR == 2021 & SEMESTRE == 2 & INS_SEDE_NOMBRE == "Universidad", NA, SNIES_SEDE),
                     SNIES_SEDE = ifelse(INS_SEDE_NOMBRE == "Bogotá", 1101, SNIES_SEDE),
                     SNIES_SEDE = ifelse(INS_SEDE_NOMBRE == "Medellín", 1102, SNIES_SEDE),
                     SNIES_SEDE = ifelse(INS_SEDE_NOMBRE == "Manizales", 1103, SNIES_SEDE),
                     SNIES_SEDE = ifelse(INS_SEDE_NOMBRE == "Palmira", 1104, SNIES_SEDE),
                     PTOTAL = ifelse(TIPO_NIVEL == "Pregrado" & PTOTAL < 0, 0, PTOTAL),
                     PTOTAL = round(PTOTAL, 3),
                     AREA_CINE = ifelse(AREA_CINE == "Ingeniería, Industria y Construcción", "Ingeniería, industria y construcción", AREA_CINE),
                     MODALIDAD = ifelse(MODALIDAD == "Otra", "Sin información", MODALIDAD),
                     EDAD = ifelse(is.na(EDAD), -88, EDAD), # Llenar celdas vacias
                     TIPO_DISC = ifelse(is.na(TIPO_DISC), "No aplica", TIPO_DISC),
                     PAES = ifelse(is.na(PAES), "No aplica", PAES),
                     PEAMA = ifelse(is.na(PEAMA), "No aplica", PEAMA),
                     SNIES_SEDE = ifelse(is.na(SNIES_SEDE), -88, SNIES_SEDE),
                     PTOTAL = ifelse(is.na(PTOTAL), -89, PTOTAL),
                     SNIES_PROGRA = ifelse(TIPO_NIVEL == "Pregrado" & ADMITIDO == "No", -88, SNIES_PROGRA),
                     SNIES_PROGRA = ifelse(is.na(SNIES_PROGRA), -89, SNIES_PROGRA),
                     PROGRAMA = ifelse(TIPO_NIVEL == "Pregrado" & ADMITIDO == "No", "No aplica", PROGRAMA),
                     PROGRAMA = ifelse(is.na(PROGRAMA), "Sin información", PROGRAMA),
                     AREAC_SNIES = ifelse(TIPO_NIVEL == "Pregrado" & ADMITIDO == "No", "No aplica", AREAC_SNIES),
                     AREAC_SNIES = ifelse(is.na(AREAC_SNIES), "Sin información", AREAC_SNIES),
                     CA_CINE = ifelse(TIPO_NIVEL == "Pregrado" & ADMITIDO == "No", -88, CA_CINE),
                     CA_CINE = ifelse(is.na(CA_CINE), -89, CA_CINE),
                     CD_CINE = ifelse(TIPO_NIVEL == "Pregrado" & ADMITIDO == "No", -88, CD_CINE),
                     CD_CINE = ifelse(is.na(CD_CINE), -89, CD_CINE),
                     AREA_CINE = ifelse(TIPO_NIVEL == "Pregrado" & ADMITIDO == "No", "No aplica", AREA_CINE),
                     AREA_CINE = ifelse(is.na(AREA_CINE), "Sin información", AREA_CINE),
                     MODALIDAD = ifelse(TIPO_NIVEL == "Pregrado", "No aplica", MODALIDAD),
                     MODALIDAD = ifelse(is.na(MODALIDAD), "Sin información", MODALIDAD),
                     RANGO_ANO_TERMINACION = ifelse(TIPO_NIVEL == "Pregrado", "No aplica", RANGO_ANO_TERMINACION),
                     RANGO_ANO_TERMINACION = ifelse(is.na(RANGO_ANO_TERMINACION), "Sin información", RANGO_ANO_TERMINACION))

# Revisar completitud de la base de datos

if(sum(complete.cases(Aspirantes))== nrow(Aspirantes)) {
  message("Base completa")
} else {
  warning("¡Base incompleta! \nAlgunas variables tienen datos faltantes")
}

# Pob 3. Matriculados ----
# Pob 4. Graduados ----
# Pob 5. Docentes ----
# Pob 6. Administrativos ----
# Pob 7. SaberPro ----
