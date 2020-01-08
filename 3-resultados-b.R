library(tidyverse)
library(ggplot2)
library(sf)
library(ggmap)
library(ggspatial)
library(geobr)
library(RColorBrewer)
library(readxl)
library(lubridate)
library(stplanr)
library(BAMMtools)

#importa layers
rodovias_sjc_4326 <- read_sf("G:\\Meu Drive\\Mestrado\\Projeto FGV\\SJC_operacional\\Bases\\Mapa_base\\rodovias_sjc_4326.shp")
areas_urbanas_4326 <- read_sf("G:\\Meu Drive\\Mestrado\\Projeto FGV\\SJC_operacional\\Bases\\Mapa_base\\areas_urbanas_4326.shp")
SJC_limite_municipal_4326 <- read_sf("G:\\Meu Drive\\Mestrado\\Projeto FGV\\SJC_operacional\\Bases\\Mapa_base\\SJC_limite_municipal_4326.shp")
corrego <- read_sf("G:\\Meu Drive\\Mestrado\\Projeto FGV\\SJC_operacional\\Bases\\Mapa_base\\corrego.shp")
area_verde <- read_sf("G:\\Meu Drive\\Mestrado\\Projeto FGV\\SJC_operacional\\Bases\\Mapa_base\\area_verde.shp")
viario_4326 <- read_sf("G:\\Meu Drive\\Mestrado\\Projeto FGV\\SJC_operacional\\Bases\\Mapa_base\\viario_4326.shp")
REVAP_4326 <- read_sf("G:\\Meu Drive\\Mestrado\\Projeto FGV\\SJC_operacional\\Bases\\Mapa_base\\REVAP_4326.shp")
rio_paraiba_4326 <- read_sf("G:\\Meu Drive\\Mestrado\\Projeto FGV\\SJC_operacional\\Bases\\Mapa_base\\rio_paraiba_4326.shp")
CTA_4326 <- read_sf("G:\\Meu Drive\\Mestrado\\Projeto FGV\\SJC_operacional\\Bases\\Mapa_base\\CTA_4326.shp")
APAS_limites_4326 <- read_sf("G:\\Meu Drive\\Mestrado\\Projeto FGV\\SJC_operacional\\Bases\\Mapa_base\\APAS_limites_4326.shp")
SJC_limite_municipal_4326 <- read_sf("G:\\Meu Drive\\Mestrado\\Projeto FGV\\SJC_operacional\\Bases\\Mapa_base\\SJC_limite_municipal_4326.shp")
macrozonas <- read_sf("G:\\Meu Drive\\Mestrado\\Projeto FGV\\SJC_operacional\\Bases\\Macrozonas\\macrozonas.shp") %>%
  st_transform(4326) 
zonas_od <- read_sf("zoneamento OD\\zoneamento_OD.shp") %>%
  st_transform(4326) 

rodovias_sjc_4326 <- st_crop(rodovias_sjc_4326 , xmin=-46.110 , xmax=-45.717 , ymin=-23.315 , ymax=-22.809)
areas_urbanas_4326 <- st_crop(areas_urbanas_4326 , xmin=-46.110 , xmax=-45.717 , ymin=-23.315 , ymax=-22.809)

#0 mapas base####
mapa_base_areas_urbanas  <- ggplot() +
  geom_sf(data=SJC_limite_municipal_4326,  color = "black", fill = "white", size = 0.7, show.legend = "line")+
  geom_sf(data=areas_urbanas_4326, fill = "#d8d9d9", color = "#d8d9d9")+
  geom_sf(data=APAS_limites_4326, aes(alpha=I(0.5)), fill = "#94d180", color = "#94d180")+
  geom_sf(data=area_verde, aes(alpha=I(0.5)), fill = "#94d180", color = "#94d180")+
  geom_sf(data=rio_paraiba_4326,  color = "#6498d2", size = 0.5, show.legend = "line")+
  geom_sf(data=corrego,  color = "#6498d2", size = 1, show.legend = "line")+
  geom_sf(data=rodovias_sjc_4326,  color = "black", show.legend = "line")

mapa_base_areas_urbanas2  <- ggplot() +
  geom_sf(data=SJC_limite_municipal_4326,  color = "black", fill = "white", size = 0.7, show.legend = "line")+
  geom_sf(data=areas_urbanas_4326, fill = "#d8d9d9", color = "#d8d9d9")+
  geom_sf(data=APAS_limites_4326, aes(alpha=I(0.5)), fill = "#94d180", color = "#94d180")+
  geom_sf(data=area_verde, aes(alpha=I(0.5)), fill = "#94d180", color = "#94d180")

mapa_base = ggplot() +
  geom_sf(data=SJC_limite_municipal_4326,  color = "black", fill = "white", size = 0.7, show.legend = "line")+
  geom_sf(data=APAS_limites_4326, aes(alpha=I(0.5)), fill = "#94d180", color = "#94d180")+
  geom_sf(data=area_verde, aes(alpha=I(0.5)), fill = "#94d180", color = "#94d180")+
  geom_sf(data=CTA_4326,  fill = "#e5b636", color = "#e5b636")+
  geom_sf(data=rio_paraiba_4326,  color = "#6498d2", size = 0.5, show.legend = "line")+
  geom_sf(data=corrego,  color = "#6498d2", size = 1, show.legend = "line")+
  geom_sf(data=REVAP_4326,  fill = "#e5b636", color = "#e5b636")

mapa_base_sem_areas = ggplot() +
  geom_sf(data=SJC_limite_municipal_4326,  color = "black", fill = "white", size = 0.7, show.legend = "line")+
  geom_sf(data=APAS_limites_4326, aes(alpha=I(0.5)), fill = "#94d180", color = "#94d180")+
  geom_sf(data=area_verde, aes(alpha=I(0.5)), fill = "#94d180", color = "#94d180")+
  geom_sf(data=rio_paraiba_4326,  color = "#6498d2", size = 0.5, show.legend = "line")+
  geom_sf(data=corrego,  color = "#6498d2", size = 1, show.legend = "line")

#1 avaliação dos fatores expansão####
#1.1 gráfico população x usuários####
grupos_sjc_temp <- grupos %>%
  filter(grupo %in% grupos_sjc_lista$grupo, !is.na(f1))
x <- grupos_sjc_temp$popu
y <- grupos_sjc_temp$moradores_identif
eq <- lm(y ~ x)

temp_plot = ggplot(grupos_sjc_temp, aes(x=popu, y =moradores_identif))+
  geom_point(size=0.5, alpha=0.3)+
  geom_abline(intercept = coef(eq)[1], slope = coef(eq)[2])+
  labs(x="População", y="Moradores identificados",
       title = paste0("y=", round(coef(eq)[2],2),
                      "x+", round(coef(eq)[1],2),
                      ", r2=", round(summary(eq)$r.squared,2) ) )+
  coord_fixed(ratio = 1)+
  theme_light()

temp_plot

ggsave(temp_plot, file="!figuras/moradores_identificados_X_pop.png")

#1.2 comparar amostra cdr x od
sum(grupos_sjc_temp$moradores_identif) #moradores identificados com cdr

OD_pessoas <- read_excel("Banco de Dados OD Dom SJC Entrega_PDDI.xlsx", sheet = "pessoas", range = "a1:r12552")
OD_viagens <- read_excel("Banco de Dados OD Dom SJC Entrega_PDDI.xlsx", sheet = "viagens", range = "a1:z24989")
OD_viagens$ID_pessoa <-  paste0(OD_viagens$ZONA, "_", OD_viagens$DOMIC, "_", OD_viagens$NUM_PESS)

nrow(OD_pessoas) #moradores entrevistados na OD
nrow(OD_viagens %>% group_by(ID_pessoa) %>% summarise()) #moradores entrevistados na OD com viagens

#OBS fatores para igualar totais de viagens da pesquisa e do cdr####
OD_viagens$hora_i_exata <- hour(OD_viagens$`HORA SAIDA`)
matriz_hora_pesquisa_i <- OD_viagens %>%
  group_by(O_ZONA, D_ZONA, hora_i_exata) %>% 
  filter(O_ZONA<=55, D_ZONA<=55) %>% 
  summarise(viagens_exp_pesquisa_i = sum(`FAT_EXP GERAL`, na.rm = T))

matriz_hora_pesquisa_i_OriDom <- OD_viagens %>%
  group_by(O_ZONA, D_ZONA, hora_i_exata) %>% 
  filter(O_ZONA<=55, D_ZONA<=55, O_MOTIVO=="Residência") %>% 
  summarise(viagens_exp_pesquisa_i_OriDom = sum(`FAT_EXP GERAL`, na.rm = T))

matriz_hora_pesquisa_i_DestDom <- OD_viagens %>%
  group_by(O_ZONA, D_ZONA, hora_i_exata) %>% 
  filter(O_ZONA<=55, D_ZONA<=55, D_MOTIVO=="Residência") %>% 
  summarise(viagens_exp_pesquisa_i_DestDom = sum(`FAT_EXP GERAL`, na.rm = T))

matriz_hora_pesquisa_i_SemDom <- OD_viagens %>%
  group_by(O_ZONA, D_ZONA, hora_i_exata) %>% 
  filter(O_ZONA<=55, D_ZONA<=55, O_MOTIVO!="Residência", D_MOTIVO!="Residência") %>% 
  summarise(viagens_exp_pesquisa_i_SemDom = sum(`FAT_EXP GERAL`, na.rm = T))

#consolida todos os resultados no zoneamento da OD em matrizes_z
matrizes_z <- matriz_hora_pesquisa_i %>% 
  full_join(matriz_hora_pesquisa_i_OriDom,
            by=c("O_ZONA", "D_ZONA", "hora_i_exata")) %>% 
  full_join(matriz_hora_pesquisa_i_DestDom,
            by=c("O_ZONA", "D_ZONA", "hora_i_exata")) %>% 
  full_join(matriz_hora_pesquisa_i_SemDom,
            by=c("O_ZONA", "D_ZONA", "hora_i_exata")) %>% 
  full_join(matriz_hora_sjc_z,
            by=c("O_ZONA"="zona_i","D_ZONA"="zona_j", "hora_i_exata"))

#troca NA por 0
matrizes_z[is.na(matrizes_z)] <- 0

#2.0.1 fatores sem criadas
fator_sem_criadas <- sum(matrizes_z$viagens_exp_pesquisa_i)/
  sum(matrizes_z$viagens_exp_sjc_i_SemCriada)
matrizes_z$viagens_exp_sjc_SemCriada_i_ajus <-
  matrizes_z$viagens_exp_sjc_i_SemCriada * fator_sem_criadas

#2.0.2 com criadas (para resolver os dois fatores x1 e x2 , iguala HBpicos e totais)

#c=f*x1+g*x2, referente a viagens HBpicos. c é da OD, f do cdr e g do CDR criada 
#a=d*x1+g*x2, referente a viagens total. a é da OD, d do cdr e g do CDR criada 

#cria antes campo periodos
cortes_periodos <- c(-1,5,9,14,19,25)#picos definidos entre 5h-10h e 15h-20h

matrizes_z$periodos <- cut(matrizes_z$hora_i_exata,
                           breaks = cortes_periodos,
                           labels = c("FP","PM","EP","PT","FP")) 

periodos <- matrizes_z %>% 
  group_by(periodos) %>% 
  summarise()

#calcula fatores
a <- sum(matrizes_z$viagens_exp_pesquisa_i)
c <- sum((matrizes_z %>%
            filter(periodos=="PM"|
                     periodos=="PT"))$viagens_exp_pesquisa_i_OriDom)
d <- sum(matrizes_z$viagens_exp_sjc_i_SemCriada)
f <- sum((matrizes_z %>%
            filter(periodos=="PM"|
                     periodos=="PT"))$viagens_exp_sjc_i_OriDom_SemCriada)
g <- sum(matrizes_z$viagens_exp_sjc_i_Criada)

fator_cdr <- (a-d) / (d-f)

fator_cdrHB <- (c-f*fator_cdr)/ g

#cria campos com totais multiplicados por fatores
matrizes_z$viagens_exp_sjc_i_ComCriada_ajus <- fator_cdr * matrizes_z$viagens_exp_sjc_i_SemCriada +
  fator_cdrHB * matrizes_z$viagens_exp_sjc_i_Criada
matrizes_z$viagens_exp_sjc_i_urb_ComCriada_ajus <- fator_cdr * matrizes_z$viagens_exp_sjc_i_urb_SemCriada +
  fator_cdrHB * matrizes_z$viagens_exp_sjc_i_urb_Criada

matrizes_z$viagens_exp_sjc_i_SemCriada_ajus <- fator_sem_criadas * matrizes_z$viagens_exp_sjc_i_SemCriada
matrizes_z$viagens_exp_sjc_i_urb_SemCriada_ajus <- fator_sem_criadas * matrizes_z$viagens_exp_sjc_i_urb_SemCriada

matrizes_z$viagens_exp_sjc_i_SemCriada_ajus <- fator_sem_criadas * matrizes_z$viagens_exp_sjc_i_SemCriada

matrizes_z$viagens_exp_sjc_i_OriDom_SemCriada_ajus <- fator_sem_criadas * matrizes_z$viagens_exp_sjc_i_OriDom_SemCriada
matrizes_z$viagens_exp_sjc_i_DestDom_SemCriada_ajus <- fator_sem_criadas * matrizes_z$viagens_exp_sjc_i_DestDom_SemCriada
matrizes_z$viagens_exp_sjc_i_SemDom_SemCriada_ajus <- fator_sem_criadas * matrizes_z$viagens_exp_sjc_i_SemDom_SemCriada

matrizes_z$viagens_exp_sjc_i_OriDom_Cviz_SemCriada_ajus <- fator_sem_criadas * matrizes_z$viagens_exp_sjc_i_OriDom_Cviz_SemCriada
matrizes_z$viagens_exp_sjc_i_DestDom_Cviz_SemCriada_ajus <- fator_sem_criadas * matrizes_z$viagens_exp_sjc_i_DestDom_Cviz_SemCriada
matrizes_z$viagens_exp_sjc_i_SemDom_Cviz_SemCriada_ajus <- fator_sem_criadas * matrizes_z$viagens_exp_sjc_i_SemDom_Cviz_SemCriada

#2 Comparação demandas levantadas####
#2.1 total de pares por hora####
pares_preenchidos <- c(nrow(matrizes_z %>% #OD
                              filter(viagens_exp_pesquisa_i>0)
                            )/(55*55*24) ,
                       nrow(matriz_hora_sjc_i %>% #cdr
                              filter(viagens_exp_sjc_i>0, criada==F)
                            )/(24*nrow(grupos_sjc_lista)^2) ,
                       nrow(matrizes_z %>% #cdr convertido
                              filter(viagens_exp_sjc_SemCriada_i>0)
                            )/(55*55*24),
                       nrow(matriz_hora_sjc_i %>% #cdr com domiciliares
                              filter(viagens_exp_sjc_i>0) %>% 
                              group_by(grupo_i, grupo_j, hora_i_exata) %>% 
                              summarise()
                       )/(24*nrow(grupos_sjc_lista)^2) ,
                       nrow(matrizes_z %>% #cdr convertido com domiciliares
                              filter(viagens_exp_sjc_ComCriada_i>0)
                       )/(55*55*24)
                       )
pares_preenchidos <- tibble(pares_preenchidos)
pares_preenchidos$fonte <- c("Pesquisa OD", "CDR","CDR convertido", "CDR com domiciliares extras","CDR convertido com domiciliares extras")
pares_preenchidos$extra <- c(F,F,F,T,T)
pares_preenchidos$fonte <- factor(pares_preenchidos$fonte,
                                  levels = c("CDR convertido com domiciliares extras",
                                             "CDR convertido",
                                             "CDR com domiciliares extras",
                                             "CDR",
                                             "Pesquisa OD"))

temp_plot = ggplot(pares_preenchidos, aes(x=fonte, y = pares_preenchidos))+
  geom_col(aes(fill=extra)) + #fill="#6498d2"
  geom_text(aes(label = paste0(round(pares_preenchidos*100, 1),"%"), y = pares_preenchidos + 0.05), position = position_dodge(0.7), vjust = 0)+
  coord_flip()+
  labs(x="", y = "Combinação de Pares OD x hora preenchidos (%)")+
  guides(fill=FALSE)+
  ylim(0, 1)+
  theme_minimal()

temp_plot

ggsave(temp_plot, file="!figuras/preenchimento_pares_por_fonte.png")

#2.2 correlação entre od e cdr_ajus####
#cria campo macrozona
zona_macrozona <- zonas_od[,c("ZAT_55","MacroZona")]
zona_macrozona$geometry <- NULL

matrizes_z <- matrizes_z %>%
  left_join(zona_macrozona, by=c("O_ZONA"="ZAT_55")) %>% 
  left_join(zona_macrozona, by=c("D_ZONA"="ZAT_55"), suffix=c("_i","_j"))

#função que recebe x e y e retorna gráfico
f_regressao_com_mapa <- function(x,y,M){
  eq <- lm(y ~ x)
  
  temp_plot = ggplot(M, aes(x=x,y=y))+
    geom_point(size=0.5, alpha=0.3)+
    geom_abline(intercept = coef(eq)[1], slope = coef(eq)[2])+
    labs(x="Viagens Pesquisa OD", y="Viagens CDR",
         title = paste0("y=", round(coef(eq)[2],2),
                        "x+", round(coef(eq)[1],2),
                        ", r2=", round(summary(eq)$r.squared,2) ) )+
    coord_fixed(ratio = 1)+
    theme_light()
  
  return(temp_plot)
}

#2.2.1 zonas e períodos####
matriz_temp <- matrizes_z %>%
  filter(O_ZONA <= 55, D_ZONA <= 55) %>%
  group_by(O_ZONA, D_ZONA, periodos, MacroZona_i, MacroZona_j) %>% 
  summarise_all(sum)
cenario <- "periodo_zona"

#2.2.1.1 Sem urb e com criadas
temp_plot <- 
  f_regressao_com_mapa(x = matriz_temp$viagens_exp_pesquisa_i,
                       y = matriz_temp$viagens_exp_sjc_i_ComCriada_ajus,
                       M = matriz_temp)
temp_plot
ggsave(temp_plot, file=paste0("!figuras/cdr_x_od_",cenario,
                              "_semUrb_ComCriada.png"))

#2.2.1.2 Com urb e com criadas
temp_plot <- 
  f_regressao_com_mapa(x = matriz_temp$viagens_exp_pesquisa_i,
                       y = matriz_temp$viagens_exp_sjc_i_urb_ComCriada_ajus,
                       M = matriz_temp)
temp_plot
ggsave(temp_plot, file=paste0("!figuras/cdr_x_od_",cenario,
                              "_comUrb_ComCriada.png"))

#2.2.1.3 Sem urb e sem criadas
temp_plot <- 
  f_regressao_com_mapa(x = matriz_temp$viagens_exp_pesquisa_i,
                       y = matriz_temp$viagens_exp_sjc_i_SemCriada_ajus,
                       M = matriz_temp)
temp_plot
ggsave(temp_plot, file=paste0("!figuras/cdr_x_od_",cenario,
                              "_semUrb_SemCriada.png"))

#2.2.1.4 Com urb e sem criadas
temp_plot <- 
  f_regressao_com_mapa(x = matriz_temp$viagens_exp_pesquisa_i,
                       y = matriz_temp$viagens_exp_sjc_i_urb_SemCriada_ajus,
                       M = matriz_temp)
temp_plot
ggsave(temp_plot, file=paste0("!figuras/cdr_x_od_",cenario,
                              "_comUrb_SemCriada.png"))

#2.2.2 zonas e dia####
matriz_temp <- matrizes_z %>%
  filter(O_ZONA <= 55, D_ZONA <= 55) %>% 
  select(-c("hora_i_exata", "periodos","MacroZona_i", "MacroZona_j")) %>%
  group_by(O_ZONA, D_ZONA) %>% 
  summarise_all(sum)

cenario <- "dia_zona"

#2.2.2.1 Sem urb e com criadas
temp_plot <- 
  f_regressao_com_mapa(x = matriz_temp$viagens_exp_pesquisa_i,
                       y = matriz_temp$viagens_exp_sjc_i_ComCriada_ajus,
                       M = matriz_temp)
temp_plot
ggsave(temp_plot, file=paste0("!figuras/cdr_x_od_",cenario,
                              "_semUrb_ComCriada.png"))

#2.2.2.2 Com urb e com criadas
temp_plot <- 
  f_regressao_com_mapa(x = matriz_temp$viagens_exp_pesquisa_i,
                       y = matriz_temp$viagens_exp_sjc_i_urb_ComCriada_ajus,
                       M = matriz_temp)
temp_plot
ggsave(temp_plot, file=paste0("!figuras/cdr_x_od_",cenario,
                              "_comUrb_ComCriada.png"))

#2.2.2.3 Sem urb e sem criadas
temp_plot <- 
  f_regressao_com_mapa(x = matriz_temp$viagens_exp_pesquisa_i,
                       y = matriz_temp$viagens_exp_sjc_i_SemCriada_ajus,
                       M = matriz_temp)
temp_plot
ggsave(temp_plot, file=paste0("!figuras/cdr_x_od_",cenario,
                              "_semUrb_SemCriada.png"))

#2.2.2.4 Com urb e sem criadas
temp_plot <- 
  f_regressao_com_mapa(x = matriz_temp$viagens_exp_pesquisa_i,
                       y = matriz_temp$viagens_exp_sjc_i_urb_SemCriada_ajus,
                       M = matriz_temp)
temp_plot
ggsave(temp_plot, file=paste0("!figuras/cdr_x_od_",cenario,
                              "_comUrb_SemCriada.png"))

#2.2.3 macrozonas e periodo####
matriz_temp <- matrizes_z %>%
  filter(O_ZONA <= 55, D_ZONA <= 55) %>% 
  select(-c("hora_i_exata")) %>%
  group_by(MacroZona_i, MacroZona_j, periodos) %>% 
  summarise_all(sum)

cenario <- "periodo_macrozona"

#2.2.3.1 Sem urb e com criadas
temp_plot <- 
  f_regressao_com_mapa(x = matriz_temp$viagens_exp_pesquisa_i,
                       y = matriz_temp$viagens_exp_sjc_i_ComCriada_ajus,
                       M = matriz_temp)
temp_plot
ggsave(temp_plot, file=paste0("!figuras/cdr_x_od_",cenario,
                              "_semUrb_ComCriada.png"))

#2.2.3.2 Com urb e com criadas
temp_plot <- 
  f_regressao_com_mapa(x = matriz_temp$viagens_exp_pesquisa_i,
                       y = matriz_temp$viagens_exp_sjc_i_urb_ComCriada_ajus,
                       M = matriz_temp)
temp_plot
ggsave(temp_plot, file=paste0("!figuras/cdr_x_od_",cenario,
                              "_comUrb_ComCriada.png"))

#2.2.3.3 Sem urb e sem criadas
temp_plot <- 
  f_regressao_com_mapa(x = matriz_temp$viagens_exp_pesquisa_i,
                       y = matriz_temp$viagens_exp_sjc_i_SemCriada_ajus,
                       M = matriz_temp)
temp_plot
ggsave(temp_plot, file=paste0("!figuras/cdr_x_od_",cenario,
                              "_semUrb_SemCriada.png"))

#2.2.3.4 Com urb e sem criadas
temp_plot <- 
  f_regressao_com_mapa(x = matriz_temp$viagens_exp_pesquisa_i,
                       y = matriz_temp$viagens_exp_sjc_i_urb_SemCriada_ajus,
                       M = matriz_temp)
temp_plot
ggsave(temp_plot, file=paste0("!figuras/cdr_x_od_",cenario,
                              "_comUrb_SemCriada.png"))

#3 Comparação padrões horários####
#curvas 3 fontes de dados - od, cdr e cdr ajus
matrizes_por_hora <-  matrizes_z %>% 
  group_by(hora_i_exata) %>% 
  select(-c("MacroZona_i","MacroZona_j","periodos")) %>%
  summarise_all(sum)

temp_plot = ggplot(matrizes_por_hora)+
  geom_line(aes(x=hora_i_exata, y=viagens_exp_pesquisa_i,
                                   color="od", size="od"))+
  geom_line(aes(x=hora_i_exata, y=viagens_exp_sjc_i_SemCriada,
                                   color="cdr", size="cdr"))+
  geom_line(aes(x=hora_i_exata, y=viagens_exp_sjc_i_SemCriada_ajus,
                                   color="cdra", size="cdra"))+
  scale_size_manual(
    values = c("od" = 1.5, "cdr" = 1, "cdra"= 1.5), 
    labels = c("CDR", "CDR Ajustado", "OD"),
    name = ""
  )+
  scale_color_manual(
    values = c("od" = "blue", "cdr" = "red", "cdra"= "red"), 
    labels = c("CDR", "CDR Ajustado", "OD"),
    name = ""
  )+
  labs(x="Hora", y = "Viagens Feitas")+
  theme_light()

temp_plot

ggsave(temp_plot, file="!figuras/curvas_horarias_comparacao.png")

#curvas 4 fontes de dados - od, cdr e cdr ajus e cdr ajus com domicilares extras
matrizes_por_periodo <- matrizes_z %>% 
  group_by(periodos) %>% 
  select(-c("MacroZona_i","MacroZona_j")) %>%
  summarise_all(sum)

temp_plot = ggplot(matrizes_por_periodo)+
  geom_line(aes(x=periodos, y=viagens_exp_pesquisa_i,
               color="od", group=1))+
  geom_line(aes(x=periodos, y=viagens_exp_sjc_i_SemCriada,
               color="cdr", group=1))+
  geom_line(aes(x=periodos, y=viagens_exp_sjc_SemCriada_i_ajus,
               color="cdra", group=1))+
  geom_line(aes(x=periodos, y=viagens_exp_sjc_i_Criada*fator_cdrHB+viagens_exp_sjc_i_SemCriada*fator_cdr,
               color="cdra_dom", group=1))+
  scale_color_manual(
    values = c("od" = "blue", "cdr" = "purple",
               "cdra"= "red", "cdra_dom"="black"),
    labels = c("od" = "OD", "cdr"="CDR",
               "cdra"="CDR Ajustado","cdra_dom"="CDR Ajustado + domiciliares adicionadas"),
    name = ""
  )+
  labs(x="Período", y = "Viagens Feitas")+
  theme_light()

temp_plot

ggsave(temp_plot, file="!figuras/curvas_por_periodo_comparacao.png")

#3.1 matrizes separadas entre Ori=DOm, Dest=Dom, e restante####
#3.1.1 sem ajuste de grupos vizinhos####
espessura=1.2

temp_plot = ggplot(matrizes_por_hora)+
  geom_line(aes(x=hora_i_exata, y=viagens_exp_pesquisa_i_OriDom, color="od_OriDom"), size=espessura)+
  geom_line(aes(x=hora_i_exata, y=viagens_exp_sjc_i_OriDom_SemCriada_ajus, color="cdra_OriDom"), size=espessura)+
  geom_line(aes(x=hora_i_exata, y=viagens_exp_pesquisa_i_DestDom, color="od_DestDom"), size=espessura)+
  geom_line(aes(x=hora_i_exata, y=viagens_exp_sjc_i_DestDom_SemCriada_ajus, color="cdra_DestDom"), size=espessura)+
  geom_line(aes(x=hora_i_exata, y=viagens_exp_pesquisa_i_SemDom, color="od_SemDom"), size=espessura)+
  geom_line(aes(x=hora_i_exata, y=viagens_exp_sjc_i_SemDom_SemCriada_ajus, color="cdra_SemDom"), size=espessura)+
  scale_color_manual(
    values = c("od_OriDom" = "#ff8080",
               "cdra_OriDom" = "#809fff",
               "od_DestDom"= "#ff0000",
               "cdra_DestDom" = "#0040ff",
               "od_SemDom" = "#800000",
               "cdra_SemDom"= "#001a66"), 
    labels = c("od_OriDom" = "OD, O=Dom",
               "cdra_OriDom" = "CDR, O=Dom",
               "od_DestDom"= "OD, D=Dom",
               "cdra_DestDom" = "CDR, D=Dom",
               "od_SemDom" = "OD, restante",
               "cdra_SemDom"= "CDR, restante"),
    name = ""
  )+
  labs(x="Hora", y = "Viagens Feitas")+
  theme_light()

temp_plot

ggsave(temp_plot, file="!figuras/curvas_horarias_comparacao_desagregado_pDomicilio.png")

#3.1.2 com ajuste de grupos vizinhos####
temp_plot = ggplot(matrizes_por_hora)+
  geom_line(aes(x=hora_i_exata, y=viagens_exp_pesquisa_i_OriDom, color="od_OriDom"), size=espessura)+
  geom_line(aes(x=hora_i_exata, y=viagens_exp_sjc_i_OriDom_Cviz_SemCriada_ajus, color="cdra_OriDom"), size=espessura)+
  geom_line(aes(x=hora_i_exata, y=viagens_exp_pesquisa_i_DestDom, color="od_DestDom"), size=espessura)+
  geom_line(aes(x=hora_i_exata, y=viagens_exp_sjc_i_DestDom_Cviz_SemCriada_ajus, color="cdra_DestDom"), size=espessura)+
  geom_line(aes(x=hora_i_exata, y=viagens_exp_pesquisa_i_SemDom, color="od_SemDom"), size=espessura)+
  geom_line(aes(x=hora_i_exata, y=viagens_exp_sjc_i_SemDom_Cviz_SemCriada_ajus, color="cdra_SemDom"), size=espessura)+
  scale_color_manual(
    values = c("od_OriDom" = "#ff8080",
               "cdra_OriDom" = "#809fff",
               "od_DestDom"= "#ff0000",
               "cdra_DestDom" = "#0040ff",
               "od_SemDom" = "#800000",
               "cdra_SemDom"= "#001a66"), 
    labels = c("od_OriDom" = "OD, O=Dom",
               "cdra_OriDom" = "CDR, O=Dom",
               "od_DestDom"= "OD, D=Dom",
               "cdra_DestDom" = "CDR, D=Dom",
               "od_SemDom" = "OD, restante",
               "cdra_SemDom"= "CDR, restante"),
    name = ""
  )+
  labs(x="Hora", y = "Viagens Feitas")+
  theme_light()

temp_plot

ggsave(temp_plot, file="!figuras/curvas_horarias_comparacao_desagregado_pDomicilio_comVizinhos.png")


#4 distribuição de distâncias####
#como em alexander et al 2015

#4.1 calcula distancias entre pares OD####
#monta linhas de desejo
od_inter <- matrizes_z %>%
  filter(O_ZONA != D_ZONA, O_ZONA!=999, D_ZONA!=999) %>% 
  group_by(O_ZONA, D_ZONA) %>% 
  summarise()

zonas_temp <- zonas_od %>%
  select(ZAT_55)

l <- od2line(flow = od_inter, zones = zonas_temp)

route = line2route(l, route_fun = route_osrm)

saveRDS(route,"route.rds")

l$route <- st_geometry(route)

l$distance = as.numeric(st_length(l$route))

#4.1.1 exemplo de rotas no mapa, para o centro
temp_plot <- mapa_base_areas_urbanas+
  geom_sf(data=(l %>% filter(D_ZONA==1)), aes(geometry=route), color="red", size=1, show.legend = "line")+
  theme_nothing(legend=TRUE)+
  labs(color=NA)+
  annotation_scale(location = "bl", width_hint = 0.4, pad_x = unit(4,"cm"), text_cex = 1) + #escala
  annotation_north_arrow(location = "bl", which_north = "true", height = unit(1.8, "cm"), width = unit(1.8, "cm"), #norte 
                         pad_x = unit(0.25, "in"), pad_y = unit(4, "in"), #pad_y é a distância vertical até a escala
                         style = north_arrow_fancy_orienteering)

temp_plot

ggsave(temp_plot, file = '!figuras/viagens_Ori_centro_PM.png' )

#4.2 calcula distribuição dist x viagens para as duas fontes####
matriz_temp <- matrizes_z %>% 
  group_by(O_ZONA, D_ZONA) %>% 
  summarise(viagens_exp_pesquisa_i = sum(viagens_exp_pesquisa_i),
            viagens_exp_sjc_i_SemCriada_ajus = sum(viagens_exp_sjc_i_SemCriada_ajus),
            viagens_exp_sjc_i_urb_SemCriada_ajus = sum(viagens_exp_sjc_i_urb_SemCriada_ajus),
            viagens_exp_sjc_i_ComCriada_ajus = sum(viagens_exp_sjc_i_ComCriada_ajus),
            viagens_exp_sjc_i_urb_ComCriada_ajus = sum(viagens_exp_sjc_i_urb_ComCriada_ajus))

l <- l %>% 
  left_join(matriz_temp)

#ordena pela distância
l2 <- l[order(l$distance),]

l2$viagens_exp_pesquisa_i2 <- l2$viagens_exp_pesquisa_i/
  sum(l2$viagens_exp_pesquisa_i)
l2$viagens_exp_sjc_i_SemCriada_ajus2 <- l2$viagens_exp_sjc_i_SemCriada_ajus/
  sum(l2$viagens_exp_sjc_i_SemCriada_ajus)
l2$viagens_exp_sjc_i_urb_SemCriada_ajus2 <- l2$viagens_exp_sjc_i_urb_SemCriada_ajus/
  sum(l2$viagens_exp_sjc_i_urb_SemCriada_ajus)
l2$viagens_exp_sjc_i_ComCriada_ajus2 <- l2$viagens_exp_sjc_i_ComCriada_ajus/
  sum(l2$viagens_exp_sjc_i_ComCriada_ajus)
l2$viagens_exp_sjc_i_urb_ComCriada_ajus2 <- l2$viagens_exp_sjc_i_urb_ComCriada_ajus/
  sum(l2$viagens_exp_sjc_i_urb_ComCriada_ajus)

temp_plot = ggplot(l2)+
  geom_line(aes(x=distance/1000,
                y=cumsum(viagens_exp_pesquisa_i2),
                color="OD")
            )+
  geom_line(aes(x=distance/1000,
                y=cumsum(viagens_exp_sjc_i_SemCriada_ajus2),
                color="CDR semurb semcriada")
            )+
  geom_line(aes(x=distance/1000,
                y=cumsum(viagens_exp_sjc_i_urb_SemCriada_ajus2),
                color="CDR urb semcriada")
  )+
  geom_line(aes(x=distance/1000,
                y=cumsum(viagens_exp_sjc_i_ComCriada_ajus2),
                color="CDR semurb + HBpicos")
  )+
  geom_line(aes(x=distance/1000,
                y=cumsum(viagens_exp_sjc_i_urb_ComCriada_ajus2),
                color="CDR urb+ HBpicos")
  )+
  labs(x="Distância (km)", y="Viagens (%)", color="Fonte")+
  scale_x_continuous(limits = c(0, 70))+
  theme(panel.grid.major.y = element_line(colour = "gray",size=0.5)) + 
  scale_y_continuous(breaks = seq(0, 1, 0.2)) +
  theme_light()

temp_plot

ggsave(temp_plot, file="distribuicao_viagens_distancia.png")

#com zoom
temp_plot = ggplot(l2)+
  geom_line(aes(x=distance/1000,
                y=cumsum(viagens_exp_pesquisa_i2),
                color="OD")
  )+
  geom_line(aes(x=distance/1000,
                y=cumsum(viagens_exp_sjc_i_SemCriada_ajus2),
                color="CDR semurb semcriada")
  )+
  geom_line(aes(x=distance/1000,
                y=cumsum(viagens_exp_sjc_i_urb_SemCriada_ajus2),
                color="CDR urb semcriada")
  )+
  geom_line(aes(x=distance/1000,
                y=cumsum(viagens_exp_sjc_i_ComCriada_ajus2),
                color="CDR semurb + HBpicos")
  )+
  geom_line(aes(x=distance/1000,
                y=cumsum(viagens_exp_sjc_i_urb_ComCriada_ajus2),
                color="CDR urb+ HBpicos")
  )+
  labs(x="Distância (km)", y="Viagens (%)", color="Fonte")+
  scale_x_continuous(limits = c(5, 30))+
  theme(panel.grid.major.y = element_line(colour = "gray",size=0.5)) + 
  scale_y_continuous(limits = c(0.2,1),
                     breaks = seq(0, 1, 0.2)) +
  theme_light()

temp_plot

ggsave(temp_plot, file="distribuicao_viagens_distancia_zoom.png")

#4.3 fluxo entre macrozonas por período####
#agrupa resultado em macrozonas
matriz_temp <- matrizes_z %>% 
  group_by(MacroZona_i, MacroZona_j, periodos) %>% 
  summarise(viagens_exp_pesquisa_i=sum(viagens_exp_pesquisa_i, na.rm=T),
            viagens_exp_sjc_i_SemCriada_ajus=sum(viagens_exp_sjc_i_SemCriada_ajus, na.rm=T),
            viagens_exp_sjc_i_ComCriada_ajus=sum(viagens_exp_sjc_i_ComCriada_ajus, na.rm=T))

#plota linhas de desejo
od_inter <- matriz_temp %>%
  filter(MacroZona_i != MacroZona_j)

l <- od2line(flow = od_inter, zones = macrozonas)

#esquema de cores
#Natural Breaks
n_intervalos <- 5
breaks <- 100*round( getJenksBreaks(l$viagens_exp_pesquisa_i, n_intervalos+1) /100,0) #arredonda na centena
breaks[n_intervalos+1] <- 100*ceiling(max(l$viagens_exp_pesquisa_i)/100) #garante que máximo é arredondado para cima

l$breaks1 <- cut(l$viagens_exp_pesquisa_i, breaks=breaks, include.lowest = TRUE, labels=breaks[2:(n_intervalos+1)])
l$breaks2 <- cut(l$viagens_exp_sjc_i_SemCriada_ajus, breaks=breaks, include.lowest = TRUE, labels=breaks[2:(n_intervalos+1)])
l$breaks3 <- cut(l$viagens_exp_sjc_i_ComCriada_ajus, breaks=breaks, include.lowest = TRUE, labels=breaks[2:(n_intervalos+1)])
breaks_scale <- levels(l$breaks1) %>% as.numeric()
labels <- as.character(rev(breaks_scale))
labels <- paste0(labels," - ",c(labels[ 2 : n_intervalos ] , "0"))

for(i in levels(periodos$periodos)){
  cont_breaks <- 1
  
  for(j in colnames(l)[4:6]){
    total_pico <- sum( (l %>% filter(periodos==i))[[j]])
    br <- paste0("breaks",cont_breaks)
    
    temp_plot = mapa_base_areas_urbanas+
      geom_sf(data = l[(l$periodos==i) & (l[[j]]>total_pico/150), ],
              aes_string(size=br, color=br),
              show.legend = "line")+
      scale_color_manual(
        values=brewer.pal(n_intervalos+1, "YlOrRd")[2:(n_intervalos+1)],
        breaks=rev(breaks_scale),
        drop=F,
        labels=labels
      )+
      scale_size_manual(
        values=c(1:6)/1.2,
        breaks=rev(breaks_scale),
        drop=F,
        labels=labels
      )+
      theme_nothing(legend=TRUE)+
      labs(size="Viagens", color="Viagens")+
      annotation_scale(location = "bl", width_hint = 0.4, pad_x = unit(4,"cm"), text_cex = 1) + #escala
      annotation_north_arrow(location = "bl", which_north = "true", height = unit(1.8, "cm"), width = unit(1.8, "cm"), #norte 
                             pad_x = unit(0.25, "in"), pad_y = unit(3, "in"), #pad_y é a distância vertical até a escala
                             style = north_arrow_fancy_orienteering)
    
    temp_plot
    
    ggsave(temp_plot, file = paste0('!figuras/linhas_desejo_',j,'_',i,'.png') )
    
    cont_breaks <- cont_breaks+1
  }
}

#4.3.2 para cdr
#agrupa resultado em macrozonas
matriz_temp <- matrizes_z %>% 
  group_by(MacroZona_i, MacroZona_j, periodos) %>% 
  summarise(viagens_cdr_i_ajus=sum(viagens_cdr_i_ajus, na.rm=T))

#plota linhas de desejo
od_inter <- matriz_temp %>%
  filter(MacroZona_i != MacroZona_j)

l <- od2line(flow = od_inter, zones = macrozonas)

#esquema de cores
#Natural Breaks
l$breaks <- cut(l$viagens_cdr_i_ajus, breaks=breaks, include.lowest = TRUE, labels=breaks[2:(n_intervalos+1)])

for(i in levels(periodos$periodos)){
  #para cada pico
  total_pico <- sum( (l %>% filter(periodos==i))$viagens_cdr_i_ajus)
  
  temp_plot = mapa_base_areas_urbanas+
    geom_sf(data=l %>% filter(periodos==i, viagens_cdr_i_ajus>total_pico/150), aes(size=breaks, color=breaks), show.legend = "line")+
    scale_color_manual(
      values=brewer.pal(n_intervalos+1, "YlOrRd")[2:(n_intervalos+1)],
      breaks=rev(breaks_scale),
      drop=F,
      labels=labels
    )+
    scale_size_manual(
      values=c(1:6)/1.2,
      breaks=rev(breaks_scale),
      drop=F,
      labels=labels
    )+
    theme_nothing(legend=TRUE)+
    labs(size="Viagens", color="Viagens")+
    annotation_scale(location = "bl", width_hint = 0.4, pad_x = unit(4,"cm"), text_cex = 1) + #escala
    annotation_north_arrow(location = "bl", which_north = "true", height = unit(1.8, "cm"), width = unit(1.8, "cm"), #norte 
                           pad_x = unit(0.25, "in"), pad_y = unit(3, "in"), #pad_y é a distância vertical até a escala
                           style = north_arrow_fancy_orienteering)
  
  temp_plot
  
  ggsave(temp_plot, file = paste0('linhas_desejo_cdr_i_',i,'.png') )
}

#5 visualização matriz####
#5.1 dissolve borda dos polígonos no mesmo grupo####
# apenas uma visualização para validar, n entrou no trabalho

#marca quadrados em sjc
SJC_limite_municipal_4326$NM_MUNICIP <- "sjc"
grade <- st_transform(grade, 4326)

grade <- grade %>%
  st_join(SJC_limite_municipal_4326)

grade$NM_MUNICIP[is.na(grade$NM_MUNICIP)] <- "não sjc"

grade_em_grupos <- grade %>% 
  group_by(grupo,NM_MUNICIP) %>% 
  summarise(POP = sum(POP, na.rm=T))

grade_em_grupos$area <- st_area(grade_em_grupos)
grade_em_grupos$area <- as.numeric(grade_em_grupos$area)
grade_em_grupos$densidade <- grade_em_grupos$POP/grade_em_grupos$area * 1000

#densidade da grade_em_grupos
temp_plot=ggplot()+
  geom_sf(data=grade_em_grupos, aes(fill=-densidade), color=NA)+
  geom_sf(data=viario_4326)+
  geom_sf(data=SJC_limite_municipal_4326, fill=NA, color="red")+
  coord_sf(xlim = c(-45.982 , -45.755), ylim = c(-23.297 , -23.127), expand = FALSE)

temp_plot
ggsave(temp_plot, file = paste0('densidade_grade_zonas.png') )

#densidade da grade
grade_sjc <- grade %>% filter(NM_MUNICIP=="sjc")

grade_sjc$area <- st_area(grade_sjc)
grade_sjc$area <- as.numeric(grade_sjc$area)
grade_sjc$densidade <- grade_sjc$POP/grade_sjc$area * 1000

temp_plot=ggplot()+
  geom_sf(data=grade_sjc, aes(fill=-densidade), color=NA)+
  geom_sf(data=viario_4326)+
  geom_sf(data=SJC_limite_municipal_4326, fill=NA, color="red")+
  coord_sf(xlim = c(-45.982 , -45.755), ylim = c(-23.297 , -23.127), expand = FALSE)

temp_plot
ggsave(temp_plot, file = paste0('densidade_grade.png') ) #resultado - densidades fazem sentido

#5.2 monta tabela com O e D para cada zona em cada hora####
origem <- matriz_hora_sjc_z %>% 
  group_by(hora_i_exata, zona_i) %>% 
  summarise( viagens_o = sum(viagens_exp))

destino <- matriz_hora_sjc_z %>% 
  group_by(hora_i_exata, zona_j) %>% 
  summarise( viagens_d = sum(viagens_exp))

vetores_od <- origem %>%
  full_join(destino, by = c("hora_i_exata"="hora_i_exata", "zona_i"="zona_j"))

colnames(vetores_od)[2] <- "zona"

#gera figura com total de origens para cada hora
for (i in 0:23){
  vetores_temp <- vetores_od %>%
    filter(hora_i_exata == i)
  
  #join grade_em_grupos com vetores da hora
  zonas_temp <- zonas_OD %>% left_join(vetores_temp, by=c("ZAT_55"="zona"))
  zonas_temp$viagens_o <- replace_na(zonas_temp$viagens_o,0)
  zonas_temp$viagens_d <- replace_na(zonas_temp$viagens_d,0)
  pontos_o <- st_sample(zonas_temp, size = round(zonas_temp$viagens_o/100))
  pontos_d <- st_sample(zonas_temp, size = round(zonas_temp$viagens_d/100))
  
  #plota O
  temp_plot = ggplot()+
    geom_sf(data=zonas_temp, aes(fill=viagens_o), color=NA) +
    scale_fill_distiller(palette = "Blues", direction = -1,limits=c(0, 10000), breaks=seq(0, 10000, by=2000)) + #display.brewer.all()
    geom_sf(data=viario_4326, color = "black")+
    geom_sf(data=pontos_o, color = "orange", size=0.6)+
    guides(fill= guide_legend()) +
    labs(fill="Origens Viagens", title = paste0(i,"h")) +
    theme_nothing(legend=TRUE) +
    annotation_scale(location = "bl", width_hint = 0.5, pad_x = unit(8,"cm"), text_cex = 0.8) + #escala
    annotation_north_arrow(location = "bl", which_north = "true", height = unit(1.3, "cm"), width = unit(1.3, "cm"), #norte 
                           pad_x = unit(0.15, "in"), pad_y = unit(4.8, "in"), #pad_y é a distância vertical até a escala
                           style = north_arrow_fancy_orienteering)+
    coord_sf(xlim = c(-45.982 , -45.755), ylim = c(-23.297 , -23.127), expand = FALSE)
  
  temp_plot
  
  if (i<10){ hora <- paste0("0" , i)
  } else{    hora <- as.character(i)
  }
  
  ggsave(temp_plot, file=paste(c("gif_exp\\",'viagens_O_z', hora,'.png'), collapse =""))
  
  #plota D
  temp_plot = ggplot()+
    geom_sf(data=zonas_temp, aes(fill=viagens_d), color=NA) +
    scale_fill_distiller(palette = "Blues", direction = -1,limits=c(0, 10000), breaks=seq(0, 10000, by=2000)) + #display.brewer.all()
    geom_sf(data=viario_4326, color = "black")+
    geom_sf(data=pontos_d, color = "orange", size=0.6)+
    guides(fill= guide_legend()) +
    labs(fill="Destinos de Viagens", title = paste0(i,"h")) +
    theme_nothing(legend=TRUE) +
    annotation_scale(location = "bl", width_hint = 0.5, pad_x = unit(8,"cm"), text_cex = 0.8) + #escala
    annotation_north_arrow(location = "bl", which_north = "true", height = unit(1.3, "cm"), width = unit(1.3, "cm"), #norte 
                           pad_x = unit(0.15, "in"), pad_y = unit(4.8, "in"), #pad_y é a distância vertical até a escala
                           style = north_arrow_fancy_orienteering)+
    coord_sf(xlim = c(-45.982 , -45.755), ylim = c(-23.297 , -23.127), expand = FALSE)
  
  temp_plot
  
  ggsave(temp_plot, file=paste(c("gif_exp\\",'viagens_D_z', hora,'.png'), collapse =""))
  
#5.3 mapa com O e D para PPM e para dia####
origem_pm <- matrizes_z %>% 
  filter(periodos=="PM") %>% 
  group_by(zona=O_ZONA) %>% 
  summarise( viagens_o_pm = sum(viagens_exp_sjc_i_ComCriada_ajus))

origem <- matrizes_z %>% 
  group_by(zona=O_ZONA) %>% 
  summarise( viagens_o = sum(viagens_exp_sjc_i_ComCriada_ajus))

destino_pm <- matrizes_z %>% 
  filter(periodos=="PM") %>% 
  group_by(zona=D_ZONA) %>% 
  summarise( viagens_d_pm = sum(viagens_exp_sjc_i_ComCriada_ajus))

destino <- matrizes_z %>% 
  group_by(zona=D_ZONA) %>% 
  summarise( viagens_d = sum(viagens_exp_sjc_i_ComCriada_ajus))

zonas_temp <- zonas_od
zonas_temp$zona <- zonas_temp$ZAT_55

zonas_temp <- zonas_temp %>% 
  left_join(origem) %>% 
  full_join(origem_pm) %>% 
  full_join(destino) %>% 
  full_join(destino_pm)

zonas_temp$area_calc <- as.numeric(st_area(zonas_temp))

for(i in c("viagens_o_pm","viagens_o","viagens_d_pm","viagens_d") ){
  zonas_temp$densidade <- (zonas_temp[[i]]/zonas_temp$area_calc)
    
  temp_plot = ggplot()+
    geom_sf(data=zonas_temp, aes(fill=densidade), color=NA) +
    geom_sf(data=zonas_temp, fill=NA, color="gray") +
    scale_fill_distiller(palette = "YlOrRd", direction = 1) + #display.brewer.all()
    geom_sf(data=rodovias_sjc_4326, color = "black", size=0.8)+
    guides(fill= guide_legend()) +
    labs(fill="Viagens/m2") +
    theme_nothing(legend=TRUE) +
    annotation_scale(location = "bl", width_hint = 0.5, pad_x = unit(8,"cm"), text_cex = 0.8) + #escala
    annotation_north_arrow(location = "bl", which_north = "true", height = unit(1.3, "cm"), width = unit(1.3, "cm"), #norte 
                           pad_x = unit(0.15, "in"), pad_y = unit(4.5, "in"), #pad_y é a distância vertical até a escala
                           style = north_arrow_fancy_orienteering)+
    coord_sf(xlim = c(-45.982 , -45.755), ylim = c(-23.297 , -23.127), expand = FALSE)
  
  temp_plot
  
  ggsave(temp_plot, file=paste0("!figuras/total_zona_",i,".png"))
}

#5.4 viagens/ pop por macrozona - barras ####
# filtra utilizadas e agrupa por domiciliio, somando fator
viagens_por_grupo_temp <- viagens_sjc %>%
  filter(utilizadas==T) %>% 
  group_by(domicilio) %>% 
  summarise(viagens_exp = sum(f3, na.rm=T))
 
# converte de grupo para zonas
viagens_por_zona_temp <- viagens_por_grupo_temp %>% 
  left_join(zona_grupo, by=c("domicilio"="grupo")) %>% 
  group_by(ZAT_55) %>% 
  summarise(viagens_exp_z = sum(fator_grupo_p_zona * viagens_exp))

sum(viagens_por_grupo_temp$viagens_exp)
sum(viagens_por_zona_temp$viagens_exp_z) #diferença se deve a grupos que têm parte fora de sjc

#calcula pop
viagens_por_zona_temp <- viagens_por_zona_temp %>% 
  left_join(grade_dados %>% 
              group_by(ZAT_55) %>% 
              summarise(pop=sum(POP,na.rm=T)) )

# agrupa em macrozonas
viagens_por_mz_temp <- viagens_por_zona_temp %>% 
  left_join(zona_macrozona) %>% 
  group_by(MacroZona)%>%
  filter(!is.na(MacroZona)) %>% 
  summarise_all(sum)

#gráfico
temp_plot = ggplot(viagens_por_mz_temp, aes(x=MacroZona, y = viagens_exp_z/pop))+
  geom_col(fill="#6498d2") +
  labs(x="Macrozona", y = "Viagem/População")+
  guides(fill=FALSE)+
  theme_minimal()

temp_plot

ggsave(temp_plot, file="!figuras/viagem_por_populacao_paracadaMacrozona.png")

#5.5 O e D para cada Macrozona no PM####
for (i in macrozonas$MacroZona){
   #origens
   origens <- matrizes_z %>% 
    filter(periodos=="PM", MacroZona_j==i) %>% 
    group_by(zona=O_ZONA) %>% 
    summarise( viagens = sum(viagens_exp_sjc_i_ComCriada_ajus))
  
  zonas_temp <- zonas_od %>% 
    left_join(origens, by=c("ZAT_55"="zona"))
  
  zonas_temp$area_calc <- as.numeric(st_area(zonas_temp))
  zonas_temp$densidade <- zonas_temp$viagens/zonas_temp$area_calc
  
  temp_plot = ggplot()+
    geom_sf(data=zonas_temp, aes(fill=densidade), color=NA) +
    geom_sf(data=zonas_temp, fill=NA, color="gray") +
    scale_fill_distiller(palette = "YlOrRd", direction = 1) + #display.brewer.all()
    geom_sf(data=(macrozonas %>% filter(MacroZona==i)), fill=NA, color="black", size=1) +
    guides(fill= guide_legend()) +
    labs(fill="Viagens/m2") +
    theme_nothing(legend=TRUE) +
    annotation_scale(location = "bl", width_hint = 0.5, pad_x = unit(4.5,"cm"), text_cex = 0.8) + #escala
    annotation_north_arrow(location = "bl", which_north = "true", height = unit(1.3, "cm"), width = unit(1.3, "cm"), #norte 
                           pad_x = unit(0.15, "in"), pad_y = unit(3, "in"), #pad_y é a distância vertical até a escala
                           style = north_arrow_fancy_orienteering)+
    coord_sf(xlim = c(-45.982 , -45.755), ylim = c(-23.297 , -23.127), expand = FALSE)
  
  temp_plot
  
  ggsave(temp_plot, file=paste0("!figuras/densidade_origens_",i,".png"))
  
  #destinos
  destinos <- matrizes_z %>% 
    filter(periodos=="PM", MacroZona_i==i) %>% 
    group_by(zona=D_ZONA) %>% 
    summarise( viagens = sum(viagens_exp_sjc_i_ComCriada_ajus))
  
  zonas_temp <- zonas_od %>% 
    left_join(destinos, by=c("ZAT_55"="zona"))
  
  zonas_temp$area_calc <- as.numeric(st_area(zonas_temp))
  zonas_temp$densidade <- zonas_temp$viagens/zonas_temp$area_calc
  
  temp_plot = ggplot()+
    geom_sf(data=zonas_temp, aes(fill=densidade), color=NA) +
    geom_sf(data=zonas_temp, fill=NA, color="gray") +
    scale_fill_distiller(palette = "YlOrRd", direction = 1) + #display.brewer.all()
    geom_sf(data=(macrozonas %>% filter(MacroZona==i)), fill=NA, color="black", size=1) +
    guides(fill= guide_legend()) +
    labs(fill="Viagens/m2") +
    theme_nothing(legend=TRUE) +
    annotation_scale(location = "bl", width_hint = 0.5, pad_x = unit(4.5,"cm"), text_cex = 0.8) + #escala
    annotation_north_arrow(location = "bl", which_north = "true", height = unit(1.3, "cm"), width = unit(1.3, "cm"), #norte 
                           pad_x = unit(0.15, "in"), pad_y = unit(3, "in"), #pad_y é a distância vertical até a escala
                           style = north_arrow_fancy_orienteering)+
    coord_sf(xlim = c(-45.982 , -45.755), ylim = c(-23.297 , -23.127), expand = FALSE)
  
  temp_plot
  
  ggsave(temp_plot, file=paste0("!figuras/densidade_destinos_",i,".png"))
}

#5.6 LD com cor indicando distância####
#filtra PM
matriz_temp <- matrizes_z %>% 
  filter(periodos=="PM") %>% 
  group_by(O_ZONA, D_ZONA) %>% 
  summarise(viagens_exp_sjc_i_ComCriada_ajus=sum(viagens_exp_sjc_i_ComCriada_ajus, na.rm=T))

#plota linhas de desejo
l <- l %>% 
  left_join(matriz_temp)

#total_pico <- sum( (l %>% filter(periodos==i))$viagens_cdr_i_ajus)

temp_plot = ggplot()+
  geom_sf(data=l %>% filter(viagens_exp_sjc_i_ComCriada_ajus>50),
          aes(size=viagens_exp_sjc_i_ComCriada_ajus, color=distance/1000), show.legend = "line")+
  scale_color_distiller(palette = 'RdYlGn', direction=-1)+
  scale_size_continuous(range=c(0.1,4))+
  theme_nothing(legend=TRUE)+
  labs(size="Viagens", color="Distância(km)")+
  annotation_scale(location = "bl", width_hint = 0.4, pad_x = unit(4,"cm"), text_cex = 1) + #escala
  annotation_north_arrow(location = "bl", which_north = "true", height = unit(1.8, "cm"), width = unit(1.8, "cm"), #norte 
                         pad_x = unit(0.25, "in"), pad_y = unit(3.2, "in"), #pad_y é a distância vertical até a escala
                         style = north_arrow_fancy_orienteering)
  #coord_sf(xlim = c(-45.982 , -45.755), ylim = c(-23.297 , -23.127), expand = FALSE)

temp_plot

ggsave(temp_plot, file = '!figuras/linhas_desejo_distancia.png')

#com zoom
temp_plot = ggplot()+
  geom_sf(data=l %>% filter(viagens_exp_sjc_i_ComCriada_ajus>50, O_ZONA!=48, D_ZONA!=48),
          aes(size=viagens_exp_sjc_i_ComCriada_ajus, color=distance/1000), show.legend = "line")+
  scale_color_distiller(palette = 'RdYlGn', direction=-1)+
  scale_size_continuous(range=c(0.1,4))+
  theme_nothing(legend=TRUE)+
  labs(size="Viagens", color="Distância(km)")+
  annotation_scale(location = "bl", width_hint = 0.4, pad_x = unit(4,"cm"), text_cex = 1) + #escala
  annotation_north_arrow(location = "bl", which_north = "true", height = unit(1.8, "cm"), width = unit(1.8, "cm"), #norte 
                         pad_x = unit(0.25, "in"), pad_y = unit(2.5, "in"), #pad_y é a distância vertical até a escala
                         style = north_arrow_fancy_orienteering)+
  coord_sf(xlim = c(-45.982 , -45.755), ylim = c(-23.297 , -23.12), expand = FALSE)

temp_plot

ggsave(temp_plot, file = '!figuras/linhas_desejo_distancia_zoom.png')
