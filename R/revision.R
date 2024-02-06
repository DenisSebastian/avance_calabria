
library(classInt)
library(sf)
library(mapview)
library(dplyr)
library(readr)


# Parámetros Generales ----------------------------------------------------

paleta_hex <- read_delim("~/Documents/CIT/Calabria/piloto_calabria/data/resources/paleta_hex.csv", 
                         delim = ";", escape_double = FALSE, trim_ws = TRUE)
idep_pal <-  paleta_hex %>% select(IDEP) %>% na.omit() %>% pull()
n_breaks <- length(idep_pal)
max_value <- 500



# Calabria ----------------------------------------------------------------

equip <- st_read("~/Documents/CIT/Calabria/piloto_calabria/data/shapes/equipment/calabria/polygons_IDEP.shp")
local_equip <- equip %>% 
  filter(CATEGORIA == "LOCAL")

metro_equip <- equip %>% 
  filter(CATEGORIA == "INTERCOMUNAL")

red  <- st_read("~/Documents/CIT/Calabria/piloto_calabria/data/shapes/general/unified_network_calabria.shp")

insumos <- 
  # mapview(red, color = "orange", hide = T) +
  mapview(metro_equip, col.regions = "red") +
  mapview(local_equip, col.regions = "blue") 
insumos  

# CL

indicador <- readRDS("~/Documents/CIT/Calabria/presentaciones/acc_calabria_pres/data/calabria/calabria_CL_IDEP.rds")

indicador <- indicador %>% 
  mutate(
    IDEP = ifelse(IDEP > 100,100, IDEP),
    IDEP = round(IDEP, 2))

b <- classInt::classIntervals(na.omit(indicador$IDEP), 
                              n = n_breaks, style = "fisher",
                              intervalClosure = "right")

hist(indicador$IDEP, breaks = 100)


indicador <- indicador %>% 
  select(TIPO_LO, COD_IST, PERSONAS,AREA_KM,IDEP, IDEP_l, IDEP_m) %>% 
  mutate(level = cut(IDEP, breaks = b$brks, right = T, 
                     include.lowest = T), 
         TIPO = ifelse(TIPO_LO %in% c(1,2), "Urbano", "Rural"))
         

tab_all <- indicador %>% st_drop_geometry() %>% 
  mutate(total_personas = sum(PERSONAS, na.rm = T)) %>% 
  mutate(area_total = sum(AREA_KM, na.rm = T)) %>% 
  group_by(level) %>% 
  summarise(n_unidades = n(),
            Personas = sum(PERSONAS, na.rm = T),
            Porc_personas = (Personas/unique(total_personas))*100,
            Area_km2 = sum(AREA_KM, na.rm = T),
            Porc_Area_km2 = (Area_km2/unique(area_total))*100)

tab_rural <- indicador %>% st_drop_geometry() %>% 
  filter(TIPO == "Urbano") %>% 
  mutate(total_personas = sum(PERSONAS, na.rm = T)) %>% 
  mutate(area_total = sum(AREA_KM, na.rm = T)) %>% 
  group_by(level) %>% 
  summarise(n_unidades = n(),
            Personas = sum(PERSONAS, na.rm = T),
            Porc_personas = (Personas/unique(total_personas))*100,
            Area_km2 = sum(AREA_KM, na.rm = T),
            Porc_Area_km2 = (Area_km2/unique(area_total))*100)

tab_rural <- indicador %>% st_drop_geometry() %>% 
  filter(TIPO == "Rural") %>% 
  mutate(total_personas = sum(PERSONAS, na.rm = T)) %>% 
  mutate(area_total = sum(AREA_KM, na.rm = T)) %>% 
  group_by(level) %>% 
  summarise(n_unidades = n(),
            Personas = sum(PERSONAS, na.rm = T),
            Porc_personas = (Personas/unique(total_personas))*100,
            Area_km2 = sum(AREA_KM, na.rm = T),
            Porc_Area_km2 = (Area_km2/unique(area_total))*100)


normal_map <- indicador %>%
  mapview(zcol = "IDEP", at =  b$brks, col.regions = idep_pal, 
          na.color = "transparent", color = NA) 




normal <- mapview(metro_equip, col.regions = "red") +
  mapview(local_equip, col.regions ="blue") +
  normal_map

normal


SA_ind <- readRDS("~/Documents/CIT/Calabria/presentaciones/acc_calabria_pres/data/calabria/calabria_SA_IDEP.rds")

SA_ind <- SA_ind %>% 
  mutate(
    IDEP = ifelse(IDEP > 100,100, IDEP),
    IDEP = round(IDEP, 2))

bSA <- classInt::classIntervals(na.omit(SA_ind$IDEP), 
                              n = n_breaks, style = "fisher",
                              intervalClosure = "right")

SA_ind <- SA_ind %>% 
  select(TIPO_LO, COD_IST, PERSONAS,AREA_KM,IDEP, IDEP_l, IDEP_m) %>% 
  mutate(level = cut(IDEP, breaks = bSA$brks, right = T, 
                     include.lowest = T), 
         TIPO = ifelse(TIPO_LO %in% c(1,2), "Urbano", "Rural"))

SA_map <- mapview(SA_ind, zcol = "IDEP", at =  bSA$brks, 
                  col.regions = idep_pal, 
                 na.color = "transparent", color = NA) 

SA <- mapview(metro_equip, col.regions = "red")+
  mapview(local_equip, col.regions ="blue") +
  SA_map 
SA


indicadorPM <- readRDS("~/Documents/CIT/Calabria/presentaciones/acc_calabria_pres/data/calabria/calabria_PM_IDEP.rds")
indicadorPM <- indicadorPM %>% 
  mutate(
    IDEP = ifelse(IDEP > 100,100, IDEP),
    IDEP = round(IDEP, 2))

bPM <- classInt::classIntervals(na.omit(indicadorPM$IDEP), 
                                n = n_breaks, style = "fisher",
                                intervalClosure = "right")
indicadorPM <- indicadorPM %>% 
  select(TIPO_LO, COD_IST, PERSONAS,AREA_KM,IDEP, IDEP_l, IDEP_m) %>% 
  mutate(level = cut(IDEP, breaks = bPM$brks, right = T, 
                     include.lowest = T), 
         TIPO = ifelse(TIPO_LO %in% c(1,2), "Urbano", "Rural"))

PM_map <- mapview(indicadorPM, zcol = "IDEP", at =  bSA$brks, 
                  col.regions = idep_pal, 
                  na.color = "transparent", color = NA) 



PM <- 
  mapview(metro_equip, col.regions = "red")+
  mapview(local_equip, col.regions ="blue") +
  PM_map 

PM




# Cosenza -----------------------------------------------------------------
equip <- st_read("~/Documents/CIT/Calabria/piloto_calabria/data/shapes/equipment/cosenza/Cosenza_IDEP.shp")
local_equip <- equip %>% 
  filter(CATEGORIA == "LOCAL")

metro_equip <- equip %>% 
  filter(CATEGORIA == "INTERCOMUNAL")

red  <- st_read("~/Documents/CIT/Calabria/piloto_calabria/data/shapes/general/red_of_cosenza.shp")

insumos <- mapview(red, color = "orange") +
  mapview(metro_equip, col.regions = "red") +
  mapview(local_equip, col.regions = "blue") 
insumos  

# CL

indicador <- readRDS("~/Documents/CIT/Calabria/presentaciones/acc_calabria_pres/data/cosenza/Cosenza_CL_IDEP.rds")
b <- classInt::classIntervals(indicador$IDEP, n = 10, style = "fisher")
hist(indicador$IDEP, breaks = 100)


normal_map <- indicador %>% 
  select(TIPO_LO, COD_IST, PERSONAS, IDEP, IDEP_l, IDEP_m) %>%
  mapview(zcol = "IDEP", at =  b$brks) 




normal <- mapview(metro_equip, col.regions = "red") +
  mapview(local_equip, col.regions ="blue") +
  normal_map
  
normal


indicadorSA <- readRDS("~/Documents/CIT/Calabria/presentaciones/acc_calabria_pres/data/cosenza/Cosenza_SA_IDEP.rds")
bSA <- classInt::classIntervals(indicadorSA$IDEP, n = 10, style = "fisher")
hist(indicadorSA$IDEP, breaks = 100)


SA_map <- indicadorSA %>% 
  select(TIPO_LO, COD_IST, PERSONAS, IDEP, IDEP_l, IDEP_m) %>% 
  mapview(zcol = "IDEP", at =  bSA$brks) 

  
  
SA <- mapview(metro_equip, col.regions = "red")+
  mapview(local_equip, col.regions ="blue") +
  SA_map 
SA


indicadorPM <- readRDS("~/Documents/CIT/Calabria/presentaciones/acc_calabria_pres/data/cosenza/Cosenza_PM_IDEP.rds")
bPM <- classInt::classIntervals(indicadorPM$IDEP, n = 10, style = "fisher")
hist(indicadorPM$IDEP, breaks = 100)


PM_map <- indicadorPM %>% 
  select(TIPO_LO, COD_IST, PERSONAS, IDEP, IDEP_l, IDEP_m) %>% 
  mapview(zcol = "IDEP", at =  bPM$brks) 



PM <- 
  mapview(metro_equip, col.regions = "red")+
  mapview(local_equip, col.regions ="blue") +
  PM_map 
  
PM


  
  # Reggio de Calabria ------------------------------------------------------

equip <- st_read("~/Documents/CIT/Calabria/piloto_calabria/data/shapes/equipment/reggio_di_calabria/reggio_di_calabria_IDEP.shp")
  local_equip <- equip %>% 
    filter(CATEGORIA == "LOCAL")
  
  metro_equip <- equip %>% 
    filter(CATEGORIA == "INTERCOMUNAL")
  
  red  <- st_read("~/Documents/CIT/Calabria/piloto_calabria/data/shapes/general/red_of_reggio_di_calabria.shp") %>% 
    filter(COMUNE == "Reggio di Calabria")
  
  insumos <- mapview(red, color = "orange") +
    mapview(metro_equip, col.regions = "red") +
    mapview(local_equip, col.regions = "blue") 
  insumos  
  
  indicador <- readRDS("~/Documents/CIT/Calabria/presentaciones/acc_calabria_pres/data/reggio_di_calabria/reggio_di_calabria_CL_IDEP.rds")
  b <- classInt::classIntervals(indicador$IDEP, n = 10, style = "fisher")
  hist(indicador$IDEP, breaks = 100)
  
  
  normal <- indicador %>% 
    select(TIPO_LO, COD_IST, PERSONAS, IDEP, IDEP_l, IDEP_m) %>%
    mapview(zcol = "IDEP", at =  b$brks) +
    mapview(metro_equip, col.regions = "red")
  mapview(local_equip, col.regions ="blue") +
    
    normal

  
  
  indicadorSA <- readRDS("~/Documents/CIT/Calabria/presentaciones/acc_calabria_pres/data/reggio_di_calabria/reggio_di_calabria_SA_IDEP.rds")
  bSA <- classInt::classIntervals(indicadorSA$IDEP, n = 10, style = "fisher")
  hist(indicadorSA$IDEP, breaks = 100)
  
  
  SA_map <- indicadorSA %>% 
    select(TIPO_LO, COD_IST, PERSONAS, IDEP, IDEP_l, IDEP_m) %>% 
    mapview(zcol = "IDEP", at =  bSA$brks) 
  
  
  
  SA <- mapview(metro_equip, col.regions = "red")+
    mapview(local_equip, col.regions ="blue") +
    SA_map 
  SA
  
  
  indicadorPM <- readRDS("~/Documents/CIT/Calabria/presentaciones/acc_calabria_pres/data/reggio_di_calabria/reggio_di_calabria_PM_IDEP.rds")
  bPM <- classInt::classIntervals(indicadorPM$IDEP, n = 10, style = "fisher")
  hist(indicadorPM$IDEP, breaks = 100)
  
  
  PM_map <- indicadorPM %>% 
    select(TIPO_LO, COD_IST, PERSONAS, IDEP, IDEP_l, IDEP_m) %>% 
    mapview(zcol = "IDEP", at =  bPM$brks) 
  
  
  
  PM <- 
    mapview(metro_equip, col.regions = "red")+
    mapview(local_equip, col.regions ="blue") +
    PM_map 
  
  PM
  
  
#sync

library(leafsync)  
sync(SA_map,PM_map)
PM_map
  
  