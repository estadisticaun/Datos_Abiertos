# SCRIP PARA LA PUBLICACIÓN DE LOS DATOS ABIERTOS DE LA UNIVERSIDAD NACIONAL DE 
# COLOMBIA DERIVADOS DE LAS FUENTES QUE SIRVEN DE BASE PARA LA PUBLICACIÓN DE 
# LAS ESTADÍSTICAS OFICIALES.

##%######################################################%##
#                                                          #
####                     Librerías                      ####
#                                                          #
##%######################################################%##

library(UnalData)
library(tidyverse)
library(writexl)
library(openxlsx)

##%######################################################%##
#                                                          #
####                   Convenciones                     ####
#                                                          #
##%######################################################%##


# Variables numéricas
# -88 = No aplica
# -89 = Sin información

# Variables cualitativas
# Sin información
# No aplica


##%######################################################%##
#                                                          #
####          Pob 1. Programas Académicos ----          ####
#                                                          #
##%######################################################%##


##%######################################################%##
#                                                          #
####         Pob 2. Aspirantes y Admitidos ----         ####
#                                                          #
##%######################################################%##


Aspirantes <- UnalData::Aspirantes %>% 
              filter((YEAR >= 2019 & TIPO_NIVEL == "Postgrado" & MOD_INS == "Regular")| 
                     (YEAR >= 2019 & TIPO_NIVEL == "Pregrado" & !is.na(TIPO_INS))) %>% 
              select(-c(ID, TIPO_DOC, NOMBRE, CAT_EDAD, ESTRATO, ADM_SEDE_NOMBRE: FACULTAD_S, 
                        PROGRAMA_S, `SNIES UNIVERSIDAD`:TITULOUNIVERSITARIO, 
                        AÑO_TERMINACION)) %>% 
              rename(EDAD = EDAD_MOD, ESTRATO = ESTRATO_ORIG) %>% 
              relocate(c(TIPO_NIVEL, NIVEL), .after = SEMESTRE) %>% 
              mutate(across(.cols = c(COD_DEP_NAC, COD_CIU_NAC:LAT_CIU_NAC, COD_DEP_RES, COD_CIU_RES:LAT_CIU_RES),
                            .fns = ~ifelse(is.na(.x), -89, .x)),
                     across(.cols = c(DEP_NAC, CIU_NAC, DEP_RES, CIU_RES),
                            .fns = ~ifelse(is.na(.x), "Sin información", .x)),
                     CODS_NAC = ifelse(is.na(CODS_NAC), "Sin información", CODS_NAC),
                     CODN_NAC = ifelse(is.na(CODN_NAC), -89, CODN_NAC),
                     NACIONALIDAD = ifelse(NACIONALIDAD == "Colombiana\r\n", "Colombiana", NACIONALIDAD),
                     INS_SEDE_NOMBRE = ifelse(is.na(INS_SEDE_NOMBRE), "Universidad", INS_SEDE_NOMBRE),   # Transformación de datos 
                     SNIES_SEDE = ifelse(YEAR == 2021 & SEMESTRE == 2 & INS_SEDE_NOMBRE == "Universidad", NA, SNIES_SEDE),
                     SNIES_SEDE = ifelse(INS_SEDE_NOMBRE == "Bogotá", 1101, SNIES_SEDE),
                     SNIES_SEDE = ifelse(INS_SEDE_NOMBRE == "Medellín", 1102, SNIES_SEDE),
                     SNIES_SEDE = ifelse(INS_SEDE_NOMBRE == "Manizales", 1103, SNIES_SEDE),
                     SNIES_SEDE = ifelse(INS_SEDE_NOMBRE == "Palmira", 1104, SNIES_SEDE),
                     PTOTAL = ifelse(TIPO_NIVEL == "Pregrado" & PTOTAL < 0, 0, PTOTAL),
                     PTOTAL = round(PTOTAL, 3),
                     AREA_CINE = ifelse(AREA_CINE == "Ingeniería, Industria y Construcción", "Ingeniería, industria y construcción", AREA_CINE),
                     MODALIDAD = ifelse(MODALIDAD == "Otra", "Sin información", MODALIDAD),
                     EDAD = ifelse(is.na(EDAD), -89, EDAD), # Llenar celdas vacias
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
# 
# Aspirantes <- Aspirantes %>% select(-(DEP_NAC:LAT_CIU_RES))

if(sum(complete.cases(Aspirantes))== nrow(Aspirantes)) {
  message("¡Base Completa! \nNo existen campos vacios")
} else {
  warning("¡Base incompleta! \nAlgunas variables tienen datos faltantes")
}

##%######################################################%##
#                                                          #
####              Pob 3. Matriculados ----              ####
#                                                          #
##%######################################################%##


Matriculados <- UnalData::Matriculados %>% 
                filter(YEAR >= 2019) %>% 
                select(-c(ID, TID, NOMBRE, CAT_EDAD, ESTRATO, PBM, DISCAPACIDAD, 
                          TIPO_DISC, SNIESU_CONVENIO, U_CONVENIO, FACULTAD_S, 
                          PROGRAMA_S)) %>%                 
                 rename(EDAD = EDAD_MOD, ESTRATO = ESTRATO_ORIG, 
                        PBM = PBM_ORIG) %>% 
                 mutate(across(.cols = c(COD_DEP_NAC, COD_CIU_NAC:LAT_CIU_NAC, COD_DEP_PROC, COD_CIU_PROC:LAT_CIU_PROC),
                               .fns = ~ifelse(is.na(.x), -89, .x)),
                        across(.cols = c(DEP_NAC, CIU_NAC, DEP_PROC, CIU_PROC),
                               .fns = ~ifelse(is.na(.x), "Sin información", .x)),
                        CODS_NAC = ifelse(is.na(CODS_NAC), "Sin información", CODS_NAC),
                        CODN_NAC = ifelse(is.na(CODN_NAC), -89, CODN_NAC),
                        NACIONALIDAD = ifelse(is.na(NACIONALIDAD), "Sin información", NACIONALIDAD),
                        EDAD = ifelse(is.na(EDAD), -89, EDAD),
                        ESTRATO = ifelse(is.na(ESTRATO), "ND/NE", ESTRATO),
                        TIPO_COL = ifelse(is.na(TIPO_COL), "Sin información", TIPO_COL),
                        PBM = ifelse(TIPO_NIVEL == "Postgrado", -88, PBM),
                        PBM = ifelse(is.na(PBM), -89, PBM),
                        PAES = ifelse(TIPO_ADM == "PAES" & is.na(PAES), "Sin información", PAES),
                        PAES = ifelse(TIPO_ADM != "PAES", "No aplica", PAES),
                        PEAMA = ifelse(TIPO_ADM == "PEAMA" & is.na(PEAMA), "Sin información", PEAMA),
                        PEAMA = ifelse(TIPO_ADM != "PEAMA", "No aplica", PEAMA),
                        MOV_PEAMA = ifelse(TIPO_ADM == "PEAMA" & is.na(MOV_PEAMA), "Sin información", MOV_PEAMA),
                        MOV_PEAMA = ifelse(TIPO_ADM != "PEAMA", "No aplica", MOV_PEAMA),
                        ADM_PEAMA_ANDINA = ifelse(TIPO_ADM == "PEAMA" & is.na(ADM_PEAMA_ANDINA), "Sin información", ADM_PEAMA_ANDINA),
                        ADM_PEAMA_ANDINA = ifelse(TIPO_ADM != "PEAMA", "No aplica", ADM_PEAMA_ANDINA),
                        CONVENIO = ifelse(TIPO_NIVEL == "Postgrado" & is.na(CONVENIO), "Sin información", CONVENIO),
                        CONVENIO = ifelse(TIPO_NIVEL != "Postgrado", "No aplica", CONVENIO),
                        TIP_CONVENIO = ifelse(CONVENIO == "Sí" & is.na(TIP_CONVENIO), "Sin información", TIP_CONVENIO),
                        TIP_CONVENIO = ifelse(CONVENIO != "Sí", "No aplica", TIP_CONVENIO),
                        FACULTAD = ifelse(is.na(FACULTAD), "Sin información", FACULTAD),
                        SNIES_PROGRA = ifelse(is.na(SNIES_PROGRA), -89, SNIES_PROGRA),
                        PROGRAMA = ifelse(is.na(PROGRAMA), "Sin información", PROGRAMA),
                        AREAC_SNIES = ifelse(is.na(AREAC_SNIES), "Sin información", AREAC_SNIES),
                        CA_CINE = ifelse(is.na(CA_CINE), -89, CA_CINE),
                        CD_CINE = ifelse(is.na(CD_CINE), -89, CD_CINE),
                        AREA_CINE = ifelse(AREA_CINE == "Ingeniería, Industria y Construcción", "Ingeniería, industria y construcción", AREA_CINE),
                        AREA_CINE = ifelse(is.na(AREA_CINE), "Sin información", AREA_CINE))

# Revisar completitud de la base de datos

if(sum(complete.cases(Matriculados))== nrow(Matriculados)) {
  message("¡Base Completa! \nNo existen campos vacios")
} else {
  warning("¡Base incompleta! \nAlgunas variables tienen datos faltantes")
}

##%######################################################%##
#                                                          #
####               Pob 4. Graduados ----                ####
#                                                          #
##%######################################################%##


Graduados <- UnalData::Graduados %>% 
  filter(YEAR >= 2018) %>% 
  select(-c(ID, TID, DEP_PROC:LAT_CIU_PROC, CAT_EDAD, ESTRATO, TIPO_COL:TIPO_DISC, 
            ADM_PEAMA_ANDINA, MOV_PEAMA, SNIESU_CONVENIO, U_CONVENIO, FACULTAD_S, 
            PROGRAMA_S)) %>% 
  rename(EDAD = EDAD_MOD, ESTRATO = ESTRATO_ORIG) %>% 
  mutate(across(.cols = c(COD_DEP_NAC, COD_CIU_NAC:LAT_CIU_NAC),
                .fns = ~ifelse(is.na(.x), -89, .x)),
         across(.cols = c(DEP_NAC, CIU_NAC),
                .fns = ~ifelse(is.na(.x), "Sin información", .x)),
         CODN_NAC = as.numeric(ifelse(YEAR == 2022 & SEMESTRE == 1, CODS_NAC, CODN_NAC)),
         CODS_NAC = ifelse(YEAR == 2022 & SEMESTRE == 1, "Sin información", CODS_NAC),
         CODS_NAC = ifelse(is.na(CODS_NAC), "Sin información", CODS_NAC),
         CODN_NAC = ifelse(is.na(CODN_NAC), -89, CODN_NAC),
         NACIONALIDAD = ifelse(is.na(NACIONALIDAD), "Sin información", NACIONALIDAD),
         EDAD = ifelse(is.na(EDAD), -89, EDAD),
         SEXO = ifelse(is.na(SEXO), "Sin información", SEXO),
         ESTRATO = ifelse(is.na(ESTRATO), "ND/NE", ESTRATO),
         across(.cols = c(SNIES_SEDE_ADM, SNIES_SEDE_MAT),
                .fns = ~ifelse(is.na(.x), -89, .x)),
         across(.cols = c(SEDE_NOMBRE_ADM, SEDE_NOMBRE_MAT, MOD_ADM, TIPO_ADM),
                .fns = ~ifelse(is.na(.x), "Sin información", .x)),
         PAES = ifelse(TIPO_ADM == "PAES" & is.na(PAES), "Sin información", PAES),
         PAES = ifelse(TIPO_ADM != "PAES", "No aplica", PAES),
         PEAMA = ifelse(TIPO_ADM == "PEAMA" & is.na(PEAMA), "Sin información", PEAMA),
         PEAMA = ifelse(TIPO_ADM != "PEAMA", "No aplica", PEAMA),
         CONVENIO = ifelse(TIPO_NIVEL == "Postgrado" & is.na(CONVENIO), "Sin información", CONVENIO),
         CONVENIO = ifelse(TIPO_NIVEL != "Postgrado", "No aplica", CONVENIO),
         TIP_CONVENIO = ifelse(CONVENIO == "Sin información", "Sin información", TIP_CONVENIO),
         TIP_CONVENIO = ifelse(CONVENIO == "Sí" & is.na(TIP_CONVENIO), "Sin información", TIP_CONVENIO),
         TIP_CONVENIO = ifelse(CONVENIO %in% c("No", "No aplica"), "No aplica", TIP_CONVENIO),
         FACULTAD = ifelse(is.na(FACULTAD), "Sin información", FACULTAD),
         SNIES_PROGRA = ifelse(is.na(SNIES_PROGRA), -89, SNIES_PROGRA),
         PROGRAMA = ifelse(is.na(PROGRAMA), "Sin información", PROGRAMA),
         AREAC_SNIES = ifelse(is.na(AREAC_SNIES), "Sin información", AREAC_SNIES),
         CA_CINE = ifelse(is.na(CA_CINE), -89, CA_CINE),
         CD_CINE = ifelse(is.na(CD_CINE), -89, CD_CINE),
         AREA_CINE = ifelse(is.na(AREA_CINE), "Sin información", AREA_CINE))

# Revisar completitud de la base de datos

if(sum(complete.cases(Graduados))== nrow(Graduados)) {
  message("¡Base Completa! \nNo existen campos vacios")
} else {
  warning("¡Base incompleta! \nAlgunas variables tienen datos faltantes")
}
   
##%######################################################%##
#                                                          #
####                Pob 5. Docentes ----                ####
#                                                          #
##%######################################################%##

Docentes <- UnalData::Docentes %>% 
  filter(YEAR >= 2018) %>% 
  select(-c(ID, FACULTAD, CAT_EDAD, CAT_SERVICIO, UNIVERSIDAD)) %>% 
  rename(FACULTAD = FACULTAD_O) %>% 
  mutate(across(.cols = where(is.numeric), .fns = ~ifelse(is.na(.x), -89, .x)),
         across(.cols = where(is.character), .fns = ~ifelse(is.na(.x), "Sin información", .x)))
  
# Revisar completitud de la base de datos

if(sum(complete.cases(Docentes))== nrow(Docentes)) {
  message("¡Base Completa! \nNo existen campos vacios")
} else {
  warning("¡Base incompleta! \nAlgunas variables tienen datos faltantes")
}

##%######################################################%##
#                                                          #
####            Pob 6. Administrativos ----             ####
#                                                          #
##%######################################################%##

Administrativos <- UnalData::Administrativos %>% 
  filter(YEAR >= 2018) %>% 
  select(-c(ID, CAT_EDAD, CAT_SERVICIO))%>% 
  mutate(across(.cols = where(is.numeric), .fns = ~ifelse(is.na(.x), -89, .x)),
         across(.cols = where(is.character), .fns = ~ifelse(is.na(.x), "Sin información", .x))) %>% 
  arrange(desc(YEAR), desc(SEMESTRE))

# Revisar completitud de la base de datos

if(sum(complete.cases(Administrativos))== nrow(Administrativos)) {
  message("¡Base Completa! \nNo existen campos vacios")
} else {
  warning("¡Base incompleta! \nAlgunas variables tienen datos faltantes")
}

# Base de datos a publicar

write_xlsx(Administrativos, "Datos/Administrativos.xlsx")


##%######################################################%##
#                                                          #
####                Pob 7. SaberPro ----                ####
#                                                          #
##%######################################################%##

SaberPro <- UnalData::SaberPro %>% 
  filter(YEAR >= 2019) %>% 
  select(-c(ID, TID, SEMESTRE:SEMESTRE_MAT, TIPO_NIVEL, CAT_EDAD, ESTRATO, PBM, 
            ADM_PEAMA_ANDINA, FACULTAD_S, PROGRAMA_S, SNP)) %>% 
  rename(EDAD = EDAD_MOD, ESTRATO = ESTRATO_ORIG, 
         PBM = PBM_ORIG, SEDE_ADMISION = SEDE_NOMBRE_ADM, 
         SEDE_FINALIZACION =SEDE_NOMBRE_MAT) %>% 
  mutate(across(.cols = c(COD_DEP_NAC, COD_CIU_NAC:LAT_CIU_NAC, COD_DEP_PROC, COD_CIU_PROC:LAT_CIU_PROC),
                .fns = ~ifelse(is.na(.x), -89, .x)),
         across(.cols = c(DEP_NAC, CIU_NAC, DEP_PROC, CIU_PROC),
                .fns = ~ifelse(is.na(.x), "Sin información", .x)),
         CODS_NAC = ifelse(is.na(CODS_NAC), "Sin información", CODS_NAC),
         CODN_NAC = ifelse(is.na(CODN_NAC), -89, CODN_NAC),
         EDAD = ifelse(is.na(EDAD), -89, EDAD),
         SEXO = ifelse(is.na(SEXO), "Sin información", SEXO),
         ESTRATO = ifelse(is.na(ESTRATO), "ND/NE", ESTRATO),
         TIPO_COL = ifelse(is.na(TIPO_COL), "Sin información", TIPO_COL),
         PBM = ifelse(is.na(PBM), -89, PBM),
         across(.cols = c(SNIES_SEDE_ADM, SNIES_SEDE_MAT),
                .fns = ~ifelse(is.na(.x), -89, .x)),
         across(.cols = c(SEDE_ADMISION, SEDE_FINALIZACION, MOD_ADM, TIPO_ADM),
                .fns = ~ifelse(is.na(.x), "Sin información", .x)),
         PAES = ifelse(TIPO_ADM == "PAES" & is.na(PAES), "Sin información", PAES),
         PAES = ifelse(TIPO_ADM != "PAES", "No aplica", PAES),
         PEAMA = ifelse(TIPO_ADM == "PEAMA" & is.na(PEAMA), "Sin información", PEAMA),
         PEAMA = ifelse(TIPO_ADM != "PEAMA", "No aplica", PEAMA),
         FACULTAD = ifelse(is.na(FACULTAD), "Sin información", FACULTAD),
         SNIES_PROGRA = ifelse(is.na(SNIES_PROGRA), -89, SNIES_PROGRA),
         PROGRAMA = ifelse(is.na(PROGRAMA), "Sin información", PROGRAMA),
         AREAC_SNIES = ifelse(is.na(AREAC_SNIES), "Sin información", AREAC_SNIES),
         CA_CINE = ifelse(is.na(CA_CINE), -89, CA_CINE),
         CD_CINE = ifelse(is.na(CD_CINE), -89, CD_CINE),
         AREA_CINE = ifelse(is.na(AREA_CINE), "Sin información", AREA_CINE),
         across(.cols = starts_with("PUNT"),
                .fns = ~ifelse(is.na(.x), -89, .x)),
         across(.cols = starts_with("NIVEL"),
                .fns = ~ifelse(is.na(.x), "Sin información", .x)))

# Revisar completitud de la base de datos

if(sum(complete.cases(SaberPro))== nrow(SaberPro)) {
  message("¡Base Completa! \nNo existen campos vacios")
} else {
  warning("¡Base incompleta! \nAlgunas variables tienen datos faltantes")
}


