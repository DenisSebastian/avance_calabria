
library(classInt)
library(sf)
library(mapview)
library(dplyr)



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
  