#mapas SJC#
library(tidyverse)
library(ggplot2)
library(sf)
library(ggmap)
library(ggspatial)
library(geobr)
library(RColorBrewer)

#importa layers
rodovias_sjc_4326 <- read_sf("G:\\Meu Drive\\Mestrado\\Projeto FGV\\SJC_operacional\\Bases\\Mapa_base\\rodovias_sjc_4326.shp") %>% 
  st_crop(xmin=-46.110 , xmax=-45.717 , ymin=-23.315 , ymax=-22.809)
areas_urbanas_4326 <- read_sf("G:\\Meu Drive\\Mestrado\\Projeto FGV\\SJC_operacional\\Bases\\Mapa_base\\areas_urbanas_4326.shp") %>% 
  st_crop(xmin=-46.110 , xmax=-45.717 , ymin=-23.315 , ymax=-22.809)
SJC_limite_municipal_4326 <- read_sf("G:\\Meu Drive\\Mestrado\\Projeto FGV\\SJC_operacional\\Bases\\Mapa_base\\SJC_limite_municipal_4326.shp")
#viario_4326 <- read_sf("G:\\Meu Drive\\Mestrado\\Projeto FGV\\SJC_operacional\\Bases\\Mapa_base\\viario_4326.shp")
REVAP_4326 <- read_sf("G:\\Meu Drive\\Mestrado\\Projeto FGV\\SJC_operacional\\Bases\\Mapa_base\\REVAP_4326.shp")
rio_paraiba_4326 <- read_sf("G:\\Meu Drive\\Mestrado\\Projeto FGV\\SJC_operacional\\Bases\\Mapa_base\\rio_paraiba_4326.shp")
CTA_4326 <- read_sf("G:\\Meu Drive\\Mestrado\\Projeto FGV\\SJC_operacional\\Bases\\Mapa_base\\CTA_4326.shp")
APAS_limites_4326 <- read_sf("G:\\Meu Drive\\Mestrado\\Projeto FGV\\SJC_operacional\\Bases\\Mapa_base\\APAS_limites_4326.shp")
zoneamento_OD <- read_sf("zoneamento OD\\zoneamento_OD.shp") %>% 
  st_transform(4326)
grade <- read_sf("G:\\Meu Drive\\Mestrado\\Dissertação\\!IBGE\\grid\\grade_id26\\grade_id26.shp")
sp_estado <- read_state("SP")
meso_regi <- read_meso_region("SP")
municipios <- read_municipality("SP")
setores <- read_census_tract(3549904)
macrozonas <- read_sf("G:\\Meu Drive\\Mestrado\\Projeto FGV\\SJC_operacional\\Bases\\Macrozonas\\macrozonas.shp")
dados_censitarios <- read_delim("G:\\Meu Drive\\Mestrado\\Dissertação\\!IBGE\\SP\\sp_setores_censitarios\\SP Exceto a Capital\\Base informaçoes setores2010 universo SP_Exceto_Capital\\CSV\\Basico_SP2-virg.csv",
                                delim = ";")
setores$code_tract2 <- as.numeric(setores$code_tract)  

setores <- setores %>% 
  left_join(dados_censitarios, by=c("code_tract2"="Cod_setor"))

#mapas base####

mapa_base_areas_urbanas  <- ggplot() +
  geom_sf(data=SJC_limite_municipal_4326,  color = "black", fill = "white", size = 0.7, show.legend = "line")+
  geom_sf(data=areas_urbanas_4326, fill = "#d8d9d9", color = "#d8d9d9")+
  geom_sf(data=APAS_limites_4326, aes(alpha=I(0.5)), fill = "#94d180", color = "#94d180")+
  geom_sf(data=rio_paraiba_4326,  color = "#6498d2", size = 0.5, show.legend = "line")+
  geom_sf(data=rodovias_sjc_4326,  color = "black", show.legend = "line")

mapa_base = ggplot() +
  geom_sf(data=SJC_limite_municipal_4326,  color = "black", fill = "white", size = 0.7, show.legend = "line")+
  geom_sf(data=APAS_limites_4326, aes(alpha=I(0.5)), fill = "#94d180", color = "#94d180")+
  geom_sf(data=CTA_4326,  fill = "#e5b636", color = "#e5b636")+
  geom_sf(data=rio_paraiba_4326,  color = "#6498d2", size = 0.5, show.legend = "line")+
  geom_sf(data=REVAP_4326,  fill = "#e5b636", color = "#e5b636")

mapa_base_sem_areas = ggplot() +
  geom_sf(data=SJC_limite_municipal_4326,  color = "black", fill = "white", size = 0.7, show.legend = "line")+
  geom_sf(data=APAS_limites_4326, aes(alpha=I(0.5)), fill = "#94d180", color = "#94d180")+
  geom_sf(data=rio_paraiba_4326,  color = "#6498d2", size = 0.5, show.legend = "line")

#localização sjc####
temp_plot = ggplot() +
  geom_sf(data=municipios,  color = "gray", fill = NA, size = 0.7, show.legend = "line")+
  geom_sf(data=sp_estado,  color = "black", fill = NA, size = 1, show.legend = "line")+
  geom_sf(data=meso_regi %>% filter(name_meso=="Vale Do Paraíba Paulista"),  aes(color = "RMVP", fill = "RMVP"), size = 0.7, show.legend = "polygon")+
  geom_sf(data=municipios %>% filter(name_muni=="São José Dos Campos"),  aes(color = "sjc", fill="sjc"), size = 0.7, show.legend = "polygon")+
  geom_sf(data=municipios %>% filter(name_muni=="São Paulo"),  aes(color = "sp", fill="sp"), size = 0.7, show.legend = "polygon")+
  scale_color_manual(
    values = c("RMVP" = "red", "sjc"="black", "sp"="black"), 
    labels = c("RMVP" = "RMVPLN", "sjc"="São José dos Campos", "sp"="São Paulo"),
    name = ""
  )+
  scale_fill_manual(
    values = c("RMVP" = NA, "sjc"="gray", "sp"="white"), 
    labels = c("RMVP" = "RMVPLN", "sjc"="São José dos Campos", "sp"="São Paulo"),
    name = ""
  )+
  theme_nothing(legend=TRUE) +
  annotation_scale(location = "bl", width_hint = 0.5, pad_x = unit(3.8,"cm"), text_cex = 0.8) + #escala
  annotation_north_arrow(location = "bl", which_north = "true", height = unit(1.3, "cm"), width = unit(1.3, "cm"), #norte 
                         pad_x = unit(0.15, "in"), pad_y = unit(2.0, "in"), #pad_y é a distância vertical até a escala
                         style = north_arrow_fancy_orienteering)

temp_plot

ggsave(temp_plot, file="!figuras/localizacao_sjc.png")

#areas urbanas e rodovias####
rodovias_sjc_4326$aux <- 0
for(i in 1:nrow(rodovias_sjc_4326)){
  rodovias_sjc_4326$aux[i] <- i
}

rodovias_lista <- rodovias_sjc_4326 %>% 
  group_by(CODTRECHOR) %>% 
  summarise(posicao=max(aux))

rodovias_lista$geometry <- NULL

rodovias_lista$posicao

rodovias_sjc_label <- rodovias_sjc_4326 %>% 
  left_join(rodovias_lista) %>% 
  filter(aux==posicao)

temp_plot = mapa_base_areas_urbanas +
  geom_sf_text(data=rodovias_sjc_label %>% filter(CODTRECHOR=="BR-116/SP-060" | CODTRECHOR=="SP-070"), aes(label=CODTRECHOR), color="red", check_overlap=T, angle=30, size=3)+
  geom_sf_text(data=rodovias_sjc_label %>% filter(CODTRECHOR!="BR-116/SP-060" & CODTRECHOR!="SP-070"), aes(label=CODTRECHOR), color="red", check_overlap=T, angle=-45, size=3)+
  theme_nothing(legend=TRUE) +
  annotation_scale(location = "bl", width_hint = 0.5, pad_x = unit(2.7,"cm"), text_cex = 0.8) + #escala
  annotation_north_arrow(location = "bl", which_north = "true", height = unit(1.3, "cm"), width = unit(1.3, "cm"), #norte 
                         pad_x = unit(0.15, "in"), pad_y = unit(3.0, "in"), #pad_y é a distância vertical até a escala
                         style = north_arrow_fancy_orienteering)

temp_plot

ggsave(temp_plot, file="!figuras/areasurbanas_sjc.png")

#densidade pop e macrozonas####
grade_sjc <- grade %>%
  st_transform(4326) %>% 
  st_join(SJC_limite_municipal_4326) %>% 
  filter(CD_GEOCMU=="3549904")

grade_sjc$area <- st_area(grade_sjc)
grade_sjc$densidade <- grade_sjc$POP/grade_sjc$area
grade_sjc$densidade <- as.numeric(grade_sjc$densidade)

APAS <- APAS_limites_4326 %>%
  st_crop(xmin=-45.982 , xmax=-45.755, ymin = -23.297 ,ymax= -23.127)

temp_plot = ggplot() +
  geom_sf(data=grade_sjc, aes(fill=densidade), color=NA)+ #apenas o q tão em sjc
  scale_fill_distiller(palette = "YlOrBr", direction=1)+
  geom_sf(data=macrozonas, fill=NA, size=1)+
  geom_sf(data=CTA_4326, fill=NA)+
  geom_sf_text(data=CTA_4326, aes(label=Name), size=3)+
  geom_sf(data=REVAP_4326, fill=NA)+
  geom_sf_text(data=REVAP_4326, aes(label=Name), size=3)+
  geom_sf(data=APAS, aes(alpha=I(0.5)), fill = "#94d180", color = "#94d180")+
  geom_sf_text(data=macrozonas, aes(label=MacroZona))+
  theme_nothing(legend=TRUE) +
  labs( fill = expression( Densidade(hab/m^2) ) )+
  annotation_scale(location = "bl", width_hint = 0.5, pad_x = unit(2.7,"cm"), text_cex = 0.8) + #escala
  annotation_north_arrow(location = "bl", which_north = "true", height = unit(1.3, "cm"), width = unit(1.3, "cm"), #norte 
                         pad_x = unit(0.15, "in"), pad_y = unit(3.0, "in"), #pad_y é a distância vertical até a escala
                         style = north_arrow_fancy_orienteering)+
  coord_sf(xlim = c(-45.982 , -45.755), ylim = c(-23.297 , -23.127), expand = FALSE)

temp_plot

ggsave(temp_plot, file="!figuras/populacao_sjc.png")

#renda per capita####
setores$renda_media <- as.numeric(setores$V009)

temp_plot = mapa_base_areas_urbanas+
  geom_sf(data=setores %>% filter(renda_media>0), aes(fill=renda_media), color=NA)+ #apenas o q tão em sjc
  scale_fill_distiller(palette = "YlOrBr", direction=1)+
  geom_sf(data=APAS, aes(alpha=I(0.5)), fill = "#94d180", color = "#94d180")+
  geom_sf(data=rio_paraiba_4326,  color = "#6498d2", size = 0.5, show.legend = "line")+
  geom_sf(data=rodovias_sjc_4326,  color = "black", show.legend = "line")+
  theme_nothing(legend=TRUE) +
  labs( fill = "Renda média mensal\ndas pessoas(R$)" )+
  annotation_scale(location = "bl", width_hint = 0.5, pad_x = unit(4.2,"cm"), text_cex = 0.8) + #escala
  annotation_north_arrow(location = "bl", which_north = "true", height = unit(1.3, "cm"), width = unit(1.3, "cm"), #norte 
                         pad_x = unit(0.15, "in"), pad_y = unit(2.8, "in"), #pad_y é a distância vertical até a escala
                         style = north_arrow_fancy_orienteering)+
  coord_sf(xlim = c(-45.982 , -45.755), ylim = c(-23.297 , -23.127), expand = FALSE)

temp_plot

ggsave(temp_plot, file="!figuras/renda_sjc.png")

#zoneamento_OD####
#completo
temp_plot = ggplot() +
  geom_sf(data=zoneamento_OD, aes(fill=MacroZona))+
  geom_sf(data=macrozonas, fill=NA, size=1)+
  theme_nothing(legend=T) +
  labs(fill="Macrozona")+
  annotation_scale(location = "bl", width_hint = 0.5, pad_x = unit(2.9,"cm"), text_cex = 0.8) + #escala
  annotation_north_arrow(location = "bl", which_north = "true", height = unit(1.3, "cm"), width = unit(1.3, "cm"), #norte 
                         pad_x = unit(0.15, "in"), pad_y = unit(4.7, "in"), #pad_y é a distância vertical até a escala
                         style = north_arrow_fancy_orienteering)

temp_plot

ggsave(temp_plot, file="!figuras/zoneamento_od_sjc.png")

#zoom urbano
temp_plot = ggplot() +
  geom_sf(data=zoneamento_OD, aes(fill=MacroZona))+
  geom_sf(data=macrozonas, fill=NA, size=1)+
  theme_nothing(legend=TRUE) +
  labs(fill="Macrozona")+
  annotation_scale(location = "bl", width_hint = 0.5, pad_x = unit(2.7,"cm"), text_cex = 0.8) + #escala
  annotation_north_arrow(location = "bl", which_north = "true", height = unit(1.3, "cm"), width = unit(1.3, "cm"), #norte 
                         pad_x = unit(0.15, "in"), pad_y = unit(3.0, "in"), #pad_y é a distância vertical até a escala
                         style = north_arrow_fancy_orienteering)+
  coord_sf(xlim = c(-45.982 , -45.755), ylim = c(-23.297 , -23.127), expand = FALSE)

temp_plot

ggsave(temp_plot, file="!figuras/zoneamento_od_sjc_zoom.png")