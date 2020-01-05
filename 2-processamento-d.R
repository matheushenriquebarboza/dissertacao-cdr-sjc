library(tidyverse)
library(ggplot2)
library(lubridate)
library(sf)
library(ggmap)
library(ggspatial)
library(geobr)
library(yaImpute)
library(readxl)
library(spdep)
library(stringr)

#0 monta arquivo cdr - elimina colunas e junta linhas####
#todos os arquivos que começam com 0 foram feitos a partir dos dados a seguir, apenas da primeira semana
dados_sjc_semana1 <-rbind(
                  dados_sjc_1[,c(1,2,5,8)],
                  dados_sjc_2[,c(1,2,5,8)],
                  dados_sjc_3[,c(1,2,5,8)],
                  dados_sjc_4[,c(1,2,5,8)],
                  dados_sjc_5[,c(1,2,5,8)],
                  dados_sjc_6[,c(1,2,5,8)],
                  dados_sjc_7[,c(1,2,5,8)])

#arquivo completo
dados_sjc_8 <- dados_sjc_8[,c(1,2,5,8)]
dados_sjc_9 <- dados_sjc_9[,c(1,2,5,8)]
dados_sjc_10<- dados_sjc_10[,c(1,2,5,8)] 
dados_sjc_11<- dados_sjc_11[,c(1,2,5,8)] 
dados_sjc_12<- dados_sjc_12[,c(1,2,5,8)] 
dados_sjc_13<- dados_sjc_13[,c(1,2,5,8)] 
dados_sjc_14<- dados_sjc_14[,c(1,2,5,8)] 
dados_sjc_15<- dados_sjc_15[,c(1,2,5,8)] 
dados_sjc_16<- dados_sjc_16[,c(1,2,5,8)] 
dados_sjc_17<- dados_sjc_17[,c(1,2,5,8)] 
dados_sjc_18<- dados_sjc_18[,c(1,2,5,8)] 
dados_sjc_19<- dados_sjc_19[,c(1,2,5,8)] 
dados_sjc_20<- dados_sjc_20[,c(1,2,5,8)] 
dados_sjc_21<- dados_sjc_21[,c(1,2,5,8)] 
dados_sjc_22<- dados_sjc_22[,c(1,2,5,8)] 
dados_sjc_23<- dados_sjc_23[,c(1,2,5,8)] 
dados_sjc_24<- dados_sjc_24[,c(1,2,5,8)] 
dados_sjc_25<- dados_sjc_25[,c(1,2,5,8)] 
dados_sjc_26<- dados_sjc_26[,c(1,2,5,8)] 
dados_sjc_27<- dados_sjc_27[,c(1,2,5,8)] 
dados_sjc_28<- dados_sjc_28[,c(1,2,5,8)] 
dados_sjc_29<- dados_sjc_29[,c(1,2,5,8)]
dados_sjc_30<- dados_sjc_30[,c(1,2,5,8)] 
  
dados_sjc_semana2 <-rbind(
  dados_sjc_8,
  dados_sjc_9,
  dados_sjc_10,
  dados_sjc_11,
  dados_sjc_12,
  dados_sjc_13,
  dados_sjc_14)
  
dados_sjc_semana3 <-rbind(
  dados_sjc_15,
  dados_sjc_16,
  dados_sjc_17,
  dados_sjc_18,
  dados_sjc_19,
  dados_sjc_20,
  dados_sjc_21)

dados_sjc_semana4 <-rbind(
  dados_sjc_22,
  dados_sjc_23,
  dados_sjc_24,
  dados_sjc_25,
  dados_sjc_26,
  dados_sjc_27,
  dados_sjc_28,
  dados_sjc_29,
  dados_sjc_30)

saveRDS(object = dados_sjc_semana2, file = "dados_sjc_semana2.rds")
saveRDS(object = dados_sjc_semana3, file = "dados_sjc_semana3.rds")
saveRDS(object = dados_sjc_semana4, file = "dados_sjc_semana4.rds")

dados_sjc_semanas <-rbind(
  zdados_sjc_semana1[,c(1,2,4,5)],
  dados_sjc_semana2,
  dados_sjc_semana3,
  dados_sjc_semana4)

colnames(dados_sjc_semanas) <- c("data", "hora", "userID", "CELLID")

#1.1 Tratamento dos dados####
#1.1.1 Tratamento dos dados de antenas####

#le antenas####
antenas_input <- read_csv2("G:\\Meu Drive\\Mestrado\\Dissertação\\!CDR bruto\\TODAS_UFS\\Antenas Brasil.csv")
antenas_input <- antenas_input[,-6] #retira última coluna repetida

#analise consistencia
dim(antenas %>%
      group_by(CELLID,UF,DDD,LAT, LONG) %>%
      summarise(n())) #69281 linhas, ou seja, tem 4 linhas repetidas

#retirando repetidas
antenas <- antenas_input %>%
  group_by(CELLID,UF,DDD,LAT, LONG) %>% 
  summarise()

dim(antenas %>%
      group_by(CELLID) %>%
      summarise(n())) #41285 linhas, ou seja, quase 28000 linhas com ID não único 

dim(antenas %>%
      group_by(CELLID) %>%
      summarise(count=n()) %>% 
      filter(count>1))#15700 IDs aparecem mais de uma vez!

antenas %>% filter(CELLID==1) #exemplo que mostra 7 antenas de estados diferentes com mesmo ID - estado parece fazer parte da chave dos dados

antenas %>%
  group_by(CELLID, UF) %>%
  summarise(count=n()) %>% 
  filter(count>1) #apenas 3 ids aparecem mais de uma vez no mesmo UF, nenhum em sp. Ou seja, UF é parte da
                  #chave do arquivo de antenas, com 7 exceções.

#o arquivo de antenas terá então todas as antneas de sp, além das de fora que não tiverem repetição de ID

IDs_SP <- antenas %>%
  group_by(CELLID) %>%
  filter(UF=="SP") %>% 
  summarise() #ids q aparecem em sp

IDs_repetidos <- antenas %>%
  group_by(CELLID) %>%
  summarise(count=n()) %>% 
  filter(count>1) %>% 
  select(CELLID) #ids q aparecem mais de uma vez

antenas_sp <- antenas %>% inner_join(IDs_SP) %>% filter(UF=='SP')
antenas_fora <- (antenas %>% anti_join(IDs_SP)) %>% anti_join(IDs_repetidos)
antenas <- rbind(antenas_sp, antenas_fora)

#agrupa antenas com mesmo lat long
antenas_grupo <- antenas %>%
  group_by(LAT, LONG) %>%
  summarise()

antenas_grupo <- rowid_to_column(antenas_grupo,"grupo")

antenas <- antenas %>%
  left_join(antenas_grupo)

rm(antenas_estudo); rm(antenas_fora); rm(antenas_sp); rm(antenas_grupo)

saveRDS(object = antenas, file = "antenas.rds")

#join de dados com antenas####
dados <- dados_sjc_semanas %>%
  inner_join(antenas)

saveRDS(object = dados, file = "dados.rds") #39.578.058 registros

#1.1.2 Tratamento dos dados de CDR####
#distribuição usuarios por numero de chamadas####
distribuicao_cham <- dados %>%
  group_by(userID) %>% 
  summarise(countchamada = n()) %>% 
  group_by(countchamada) %>% 
  summarise(countID = n())

sum(distribuicao_cham$countID) #445666 usuários

#ordenando crescente por número de chamadas
distribuicao_cham <- distribuicao_cham[order(distribuicao_cham$countchamada),]
for (i in c(1:nrow(distribuicao_cham))){
  distribuicao_cham$prob_acum[i] <- sum(distribuicao_cham$countID * (distribuicao_cham$countchamada[i] >= distribuicao_cham$countchamada) ) /
                            sum(distribuicao_cham$countID)
}

saveRDS(object = distribuicao_cham, file = "distribuicao_cham.rds")

#grafico distribuicao usuario chamadas
temp_plot = ggplot(distribuicao_cham)+
  geom_point(aes(x = countchamada, y = prob_acum))+
  theme_classic()+
  labs(x="Número de chamadas", y="Número Acumulado de Usuários (%)")+
  theme(
    axis.text.x = element_text(size=13),
    axis.text.y = element_text(size=13),
    axis.title.x = element_text(size=15),
    axis.title.y = element_text(size=15)
  )

temp_plot

ggsave(temp_plot, file='distribuicao_cham.png')

temp_plot = ggplot(distribuicao_cham) +
  geom_point(aes(x = countchamada, y = prob_acum)) +
  theme_classic() +
  labs(x="Número de chamadas", y="Número Acumulado de Usuários (%)") +
  theme(
    axis.text.x = element_text(size=13),
    axis.text.y = element_text(size=13),
    axis.title.x = element_text(size=15),
    axis.title.y = element_text(size=15)
  ) +
  coord_cartesian(ylim = c(0, 0.95), xlim = c(0,500))

temp_plot

ggsave(temp_plot, file='distribuicao_cham_limit.png')

#define limites inferior e superior de número de ligações feitas####
cmin  <- 6 #julio usa 20 em um ano
cmax <- 500  #julio usa usuarios com menos de 5000 lig em 1 ano , 13.7 por dia. em um mês, 411 lig

ligadores_significativos <- dados %>% 
  group_by(userID) %>% 
  summarise(countcham = n()) %>% 
  filter((countcham >= cmin) & (countcham <= cmax)) %>% 
  select(userID)

saveRDS(object = ligadores_significativos, file = "ligadores_significativos.rds")

#reduz dados para os q aparecem no limite de ligações
dados_usuarios <- dados %>%
  inner_join(ligadores_significativos)

#distribuicao locais####
distribuicao_locais <- dados_usuarios %>% 
  group_by(userID) %>% 
  summarise(locais_distintos = n_distinct(grupo))  %>% 
  group_by(locais_distintos) %>% 
  summarise(countID = n())

#grafico distribuicao usuario locais
png('distribuicao_locais.png', width = 700, height = 495)

ggplot(distribuicao_locais)+
  geom_point(aes(x = locais_distintos, y = countID))+
  theme_classic()+
  labs(x="Quantidade de locais visitados", y="Quantidade de usuários")+
  theme(
    axis.text.x = element_text(size=14),
    axis.text.y = element_text(size=14),
    axis.title.x = element_text(size=17),
    axis.title.y = element_text(size=17)
  )

dev.off()

#2.1 Classificação de pontos como casa####
#2.1.1 local permanência####
#2.1.1.1 definição de horario para definir permanencia - a partir da OD####
OD_viagens <- read_excel("Banco de Dados OD Dom SJC Entrega_PDDI.xlsx", sheet = "viagens", range = "a1:z24989")
OD_pessoas <- read_excel("Banco de Dados OD Dom SJC Entrega_PDDI.xlsx", sheet = "pessoas", range = "a1:r12552")
OD_viagens$ID_pessoa = paste0(OD_viagens$ZONA, "_",OD_viagens$DOMIC, "_", OD_viagens$NUM_PESS)

OD_viagens_dom <- OD_viagens %>% 
  filter(O_MOTIVO=="Residência" | D_MOTIVO=="Residência") %>% 
  group_by(ID_pessoa, horai = `HORA SAIDA`, horaj = `HORA CHEGA`, O_MOTIVO, D_MOTIVO) %>% 
  summarise()

x <- OD_viagens_dom %>% 
  group_by(ID_pessoa) %>% 
  summarise(hora_min = min(horai), hora_max = max(horai))

OD_viagens_dom <- OD_viagens_dom %>%
  left_join(x)

#retira as viagens do dia seguinte da análise
hora_limite <- ymd_hms("1900-01-01 00:00:00")
OD_viagens_dom <- OD_viagens_dom %>% filter(horai< hora_limite & horaj< hora_limite) 
rm(hora_limite)

OD_viagens_dom$primeiro <- OD_viagens_dom$horai == OD_viagens_dom$hora_min
OD_viagens_dom$ultimo <- OD_viagens_dom$horai == OD_viagens_dom$hora_max
OD_viagens_dom$horai_ex <- hour(OD_viagens_dom$horai)
OD_viagens_dom$horaj_ex <- hour(OD_viagens_dom$horaj)

m <- as.data.frame(matrix(0, ncol = 24, nrow = nrow(OD_viagens_dom)))
OD_viagens_dom <- cbind.data.frame(OD_viagens_dom, m) #adiciona 24 colunas, correpondentes às horas do dia

#preenche horas em casa com 1
for(i in 1:nrow(OD_viagens_dom)){
  if(OD_viagens_dom$primeiro[i]==T){
    if(OD_viagens_dom$O_MOTIVO[i]=="Residência"){ #se primeira viagem parte de casa, até hora da saída da primeira viagem recebe 1
      OD_viagens_dom[ i , ( (12+0) : (12+OD_viagens_dom$horai_ex[i]) ) ] <- 1
    }
  }else if(OD_viagens_dom$ultimo[i]==T){
    if(OD_viagens_dom$D_MOTIVO[i]=="Residência"){ #se última viagem chega em casa, horário de chegada até fim do dia recebe 1
      OD_viagens_dom[ i , ( (12+OD_viagens_dom$horaj_ex[i]) : (12+23) ) ]<- 1
    }
  }else{
    if(OD_viagens_dom$O_MOTIVO[i]=="Residência"){ #se viagem durante o dia sai de casa, busca horario de chegad da viagem anterior e intervalço recebe 1
      OD_viagens_dom[ i , ( (12 + OD_viagens_dom$horaj_ex[i-1]) : (12+OD_viagens_dom$horai_ex[i]) ) ] <- 1
    }
  }
}

#agrupa com 1 linha para cada pessoa
total_casa_por_hora <- OD_viagens_dom[,c(1,12:35)]
total_casa_por_hora <- total_casa_por_hora %>% 
  group_by(ID_pessoa) %>% 
  summarise_all(list(casa=sum))

#remove linhas com 2
total_casa_por_hora <- total_casa_por_hora %>% 
  filter_all(all_vars(.!=2))

#calcula totais por hora
horario_pessoas_em_casa_OD <-as.data.frame(colSums(total_casa_por_hora[,-1]))
colnames(horario_pessoas_em_casa_OD)[1] <- "pessoas_em_casa"
horario_pessoas_em_casa_OD$hora <- 0:23

png('horario_pessoas_em_casa_OD.png', width = 700, height = 495)

ggplot(horario_pessoas_em_casa_OD,aes(hora,pessoas_em_casa)) +
  geom_col() +
  theme_classic()+
  labs(x="hora", y="Quantidade de pessoas em casa")+
  theme(
    axis.text.x = element_text(size=14),
    axis.text.y = element_text(size=14),
    axis.title.x = element_text(size=17),
    axis.title.y = element_text(size=17)
  )

dev.off()

#2.1.1.2 registra ligacoes em horarios potencialmente domicilio (volta pro cdr)####
dados_usuarios$diasemana <- wday(dados_usuarios$data, label = TRUE)
feriado <- ymd(20130329)
dados_usuarios$hora2 <- hour(dados_usuarios$hora)

dados_usuarios$permanencia <- ( dados_usuarios$diasemana == "dom" ) |
  ( dados_usuarios$data == feriado ) |
  ( dados_usuarios$hora2 >= 22 ) |
  ( dados_usuarios$hora2 <= 6 )

#ITERAÇÃO EXTRA - alguns grupos foram unidos, para que nenhum grupo tivesse menos de 300 usuários detectados com domicilio no grupo. dados_usuarios_antesdeagrupar mostra dados_usuario antes do processo seguinte
dados_usuarios$grupo0 <- dados_usuarios$grupo

dados_usuarios$grupo[dados_usuarios$grupo0==2489] <- 2488
dados_usuarios$grupo[dados_usuarios$grupo0==2976] <- 2619
dados_usuarios$grupo[dados_usuarios$grupo0==2441] <- 2467
dados_usuarios$grupo[dados_usuarios$grupo0==2532] <- 2543
dados_usuarios$grupo[dados_usuarios$grupo0==2541] <- 2525
dados_usuarios$grupo[dados_usuarios$grupo0==2513] <- 2506
dados_usuarios$grupo[dados_usuarios$grupo0==2515] <- 2534

grupo_para_grupo0 <- dados_usuarios %>% 
  group_by(grupo,grupo0) %>% 
  summarise()

saveRDS(object = dados_usuarios, file = "dados_usuarios.rds")

minimo_dias_ligacao <- 3 #minimo de dias em que usuario realizou ligacoes para ser considerado na identificacao de domicilio

#acrescenta-se uma coluna aos ligadores_significativos, com os dias distintos de ligações
ligadores_significativos <- dados_usuarios %>% 
  group_by(userID) %>% 
  summarise(dias_distintos = n_distinct(data))

#coluna atende_min_dias registra se userID atende mínimo de dias com ligações feitas
ligadores_significativos$atende_min_dias <- ( ligadores_significativos$dias_distintos >= minimo_dias_ligacao )

#2.1.1.3 determina local de permanência de cada usuario####
#A local de maior permanencia é unico####
#x avalia se locais de permanencia de cada usuário é o mais visitado
x <- dados_usuarios %>% #calcula número de dias em cada local
  filter(permanencia==T) %>% 
  group_by(userID, grupo) %>% 
  summarise(dias_distintos_local = n_distinct(data))

x2 <- x %>% #calcula numero maximo de dias de visitas de cada usuario a um lugar
  group_by(userID) %>% 
  summarise(max_dias_distintos = max(dias_distintos_local))

x3 <- x %>% #filtra local visitado mais dias por cada usuario (pode ser mais de um)
  inner_join(x2) %>% 
  filter(dias_distintos_local == max_dias_distintos)
  
x4 <- x3 %>% 
  inner_join(ligadores_significativos, by = "userID") %>% 
#  filter(dias_distintos_local >= dias_distintos/2) %>% #17367 linhas com esse filtro
  filter(atende_min_dias == TRUE) #290826 linhas, algumas c mesmo userID

dim(x4 %>% group_by(userID) %>% summarise(n=n())) #234908 userID's únicos

#z1 guarda os ids q só aparecem uma vez e estão resolvidos
z1 <- x4 %>% 
  group_by(userID) %>% 
  summarise(npermanencias = n()) %>% 
  filter(npermanencias==1) %>% 
  select(userID) #197172 ids q só aparecem uma vez e estão resolvidos

a1 <- x4 %>% #a1 guarda os ids e os grupos do domicílio
  inner_join(z1) %>% 
  select(userID,grupo)

#B local de maior permanencia n é unico, dentre eles o de n-permanencia é unico####
#para remover os locais de permanencia sobrando, criterio para escolher casa vai ser os q aparecem mais vezes fora do período desejado
y1 <- dados_usuarios %>% #calcula número de dias em cada local fora do horario de permanencia
  filter(permanencia==FALSE) %>% 
  group_by(userID, grupo) %>% 
  summarise(dias_distintos_perm_false = n_distinct(data))
  
x5 <- x4 %>% #adiciona a x4 o numero de dias_distintos_perm_false e mantem apenas ids n resolvidos
  left_join(y1) %>% 
  anti_join(z1)

y2 <- x5 %>% #calcula para cada usuário max_dias_distintos_perm_false
  group_by(userID) %>% 
  summarise(max_dias_distintos_perm_false = max(dias_distintos_perm_false))

x6 <- x5 %>% #filtra local visitado mais dias por cada usuario (pode ser mais de um)
  inner_join(y2) %>% 
  filter(dias_distintos_perm_false == max_dias_distintos_perm_false)

dim(x6 %>% group_by(userID) %>% summarise(n=n()) %>% 
  filter(n>1)) #ainda sobram 2138 com repetição, para esses, será adotado o grupo com valor mais alto

#z2 guarda os ids resolvidos com máximo fora do período
z2 <- x6 %>% group_by(userID) %>%
  summarise(n=n()) %>% 
  filter(n == 1) %>% 
  select(userID)

a2 <- x6 %>% #a2 guarda os ids e os grupos de domicílio
  inner_join(z2) %>% 
  select(userID,grupo)

#C local de maior permanencia n é unico, dentre eles o de n-permanencia n é unico####
#para os que sobram, casa será a que tiver grupo com maior valor
#x7 são os dados dos que sobram
x7 <- x6 %>%
  anti_join(z2)

y3 <- x7 %>% #calcula para cada usuário max_grupo
  group_by(userID) %>% 
  summarise(max_grupo = max(grupo))

x8 <- x7 %>% #filtra grupo de maior valor de cada usuario
  inner_join(y3) %>% 
  filter(grupo == max_grupo)

#z3 guarda os ids resolvidos com máximo fora do período
z3 <- x8 %>% group_by(userID) %>%
  summarise(n=n()) %>% 
  filter(n == 1) %>% 
  select(userID)

a3 <- x8 %>% #a3 guarda os ids e os grupos de domicílio
  inner_join(z3) %>% 
  select(userID,grupo)

#D acumulo das soluções####
domicilios <- rbind(a1,a2,a3)

colnames(domicilios)[2] <- "domicilio"

ligadores_significativos_E_domicilio <- ligadores_significativos %>% 
  left_join(domicilios) 

saveRDS(object = ligadores_significativos_E_domicilio, file = "ligadores_significativos_E_domicilio.rds")

zona <- ligadores_significativos_E_domicilio %>% 
  group_by(domicilio) %>% 
  summarise(moradores = n())

saveRDS(object = zona, file = "zona.rds")

#2.1.1.4 adicionar população de cada zona para calcular fator de expansão####
grupos <- dados_usuarios %>% 
  group_by(grupo0,LAT,LONG) %>% 
  summarise()

grupos <- st_as_sf(x = grupos, 
                   coords = c("LONG", "LAT"),
                   crs = "+proj=longlat +datum=WGS84")

grupos <- grupos %>%
  left_join(zona, by = c("grupo0"="domicilio"))

#2.3 Adição de viagens de base domiciliar####
#2.3.1 monta viagens####
x <- dados_usuarios %>%
  group_by(userID, data, hora, grupo) %>%
  summarise()

saveRDS(object = x, file = "x.rds")

viagens0 <- cbind(x[-nrow(x),] , x[-1,]) #repete x ao lado, deletando a primeira linha
rm(x)
colnames(viagens0) <- c("userID","data_i","hora_i","grupo_i","userID_j","data_j","hora_j","grupo_j")
viagens0 <- viagens0[(viagens0$userID == viagens0$userID_j) &
                       (viagens0$grupo_i != viagens0$grupo_j),]

viagens0$userID_j <- NULL

viagens0$duracao <- as.numeric( ymd_hms( paste(viagens0$data_j,
                                               seconds_to_period(viagens0$hora_j) ) ) -
                                 ymd_hms( paste(viagens0$data_i,
                                                seconds_to_period(viagens0$hora_i) ) ) )

viagens0$criada <- F

saveRDS(object = viagens0, file = "viagens0.rds")

#2.3.2 PD_saiu lista pessoa-dia que saiu de casa#
dados_usuarios2 <- dados_usuarios %>% 
  left_join(ligadores_significativos_E_domicilio, by="userID")

PD_saiu <- dados_usuarios2 %>% 
  group_by(userID, data) %>%
  filter(grupo != domicilio) %>% 
  summarise()

viagens2 <- viagens0 %>% 
  left_join(ligadores_significativos_E_domicilio)

#2.3.3 L1 lista pessoa-dia com viagem transiente de saída de casa#
viagens2$hora_i_num <- as.numeric(viagens2$hora_i)

temp <- viagens2 %>% 
  group_by(userID, data_i) %>% 
  summarise(hora_min=min(hora_i_num))

L1 <- viagens2 %>% 
  left_join(temp) %>% 
  filter(hora_i_num == hora_min, grupo_i == domicilio) %>% 
  group_by(userID, data_i) %>% 
  summarise()

#2.3.4 L2 lista pessoa-dia com viagem transiente de chegada em casa#
temp <- viagens2 %>% 
  group_by(userID, data_i) %>% 
  summarise(hora_max=max(hora_i_num))

L2 <- viagens2 %>% 
  left_join(temp) %>% 
  filter(hora_i_num == hora_max, grupo_j == domicilio) %>% 
  group_by(userID, data_i) %>% 
  summarise()

#2.3.5 adiciona viagem de saída de casa para PD_saiu-L1
userdia_recebe_viagem_saindo <- PD_saiu %>% 
  anti_join(L1, by=c("userID", "data"="data_i"))

dados_usuarios_recebe_viagem <- dados_usuarios2 %>% 
  inner_join(userdia_recebe_viagem_saindo)

dados_usuarios_recebe_viagem$hora_num <- as.numeric(dados_usuarios_recebe_viagem$hora)

hora_prim_ponto <- dados_usuarios_recebe_viagem %>% 
  group_by(userID, data) %>% 
  filter(grupo != domicilio) %>% 
  summarise(hora_min=min(hora_num))

userdiaponto_recebe_viagem_saindo <- hora_prim_ponto %>% 
  left_join(dados_usuarios_recebe_viagem) %>% 
  group_by(userID, data, grupo,domicilio) %>% 
  summarise()

viagens_base_domiciliar_extras1 <- userdiaponto_recebe_viagem_saindo

colnames(viagens_base_domiciliar_extras1) <- c("userID", "data_i", "grupo_j","grupo_i")

x <- viagens0 %>% #auxiliar para hora desejada ficar no formato certo
  filter(hora_i==hms("07:00:00")) %>%
  group_by(hora_i) %>%
  summarise()

viagens_base_domiciliar_extras1$hora_i <- x$hora_i[1]
viagens_base_domiciliar_extras1$data_j <- viagens_base_domiciliar_extras1$data_i

#2.3.6 adiciona viagem de chegada em casa para PD_saiu-L2
userdia_recebe_viagem_chegando <- PD_saiu %>% 
  anti_join(L2, by=c("userID", "data"="data_i"))

dados_usuarios_recebe_viagem <- dados_usuarios2 %>% 
  inner_join(userdia_recebe_viagem_chegando)

dados_usuarios_recebe_viagem$hora_num <- as.numeric(dados_usuarios_recebe_viagem$hora)

hora_ultimo_ponto <- dados_usuarios_recebe_viagem %>% 
  group_by(userID, data) %>% 
  filter(grupo != domicilio) %>% 
  summarise(hora_max=max(hora_num))

userdiaponto_recebe_viagem_chegando <- hora_ultimo_ponto %>% 
  left_join(dados_usuarios_recebe_viagem) %>% 
  group_by(userID, data, grupo, domicilio) %>% 
  summarise()

viagens_base_domiciliar_extras2 <- userdiaponto_recebe_viagem_chegando

colnames(viagens_base_domiciliar_extras2) <- c("userID", "data_i", "grupo_i","grupo_j")

x <- viagens0 %>% #auxiliar para hora desejada ficar no formato certo
  filter(hora_i==hms("17:00:00")) %>%
  group_by(hora_i) %>%
  summarise()

viagens_base_domiciliar_extras2$hora_i <- x$hora_i[1]
viagens_base_domiciliar_extras2$data_j <- viagens_base_domiciliar_extras2$data_i

#combina viagens criadas
viagens_base_domiciliar_extras <- rbind(viagens_base_domiciliar_extras1[,1:6],
                                        viagens_base_domiciliar_extras2[,1:6])

viagens_base_domiciliar_extras$criada <- T

viagens_base_domiciliar_extras$hora_j <- NA
viagens_base_domiciliar_extras$duracao <- NA
  
saveRDS(object = viagens_base_domiciliar_extras, file = "viagens_base_domiciliar_extras.rds")

#visualizando onde estão os grupos - n precisa rodar####
#importa geo's de sp
mesor <- read_meso_region(code_meso="SP", year=2018)
micro <- read_micro_region(code_micro="SP", year=2018)
munic <- read_municipality(code_muni="SP", year=2018)
#população grade
grade <- read_sf("G:\\Meu Drive\\Mestrado\\Dissertação\\!IBGE\\grid\\grade_id26\\grade_id26.shp")

xmin <- -46.2; xmax <- -44.2; ymin <- -24; ymax <- -22.4
mesor <- st_crop(mesor , xmin=xmin , xmax=xmax , ymin=ymin , ymax=ymax)
micro <- st_crop(micro , xmin=xmin , xmax=xmax , ymin=ymin , ymax=ymax)
munic <- st_crop(munic , xmin=xmin , xmax=xmax , ymin=ymin , ymax=ymax)
grade <- st_crop(grade , xmin=xmin , xmax=xmax , ymin=ymin , ymax=ymax)

ggplot() +
  geom_sf(data=mesor) + 
  geom_sf(data=micro) + 
  geom_sf(data=munic) + 
  geom_sf(data=grupos) + 
  #theme_nothing(legend=TRUE) +
  annotation_scale(location = "bl", width_hint = 0.5, pad_x = unit(10.5,"cm"), text_cex = 1) + #escala
  annotation_north_arrow(location = "bl", which_north = "true", height = unit(1.8, "cm"), width = unit(1.8, "cm"), #norte 
                         pad_x = unit(0.25, "in"), pad_y = unit(6, "in"), #pad_y é a distância vertical até a escala
                         style = north_arrow_fancy_orienteering)

#2.4 Expansão das Viagens####
#2.4.1 associa cada quadrado ao grupo mais proximo####
#2.4.1.1 calcula centroides dos quadrados
grade$centroids <- st_transform(grade, 29101) %>% 
  st_centroid() %>% 
  # this is the crs from d, which has no EPSG code:
  st_transform(., '+proj=longlat +ellps=GRS80 +no_defs') %>%
  # since you want the centroids in a second geometry col:
  st_geometry()


sfc_as_cols <- function(x, names = c("x","y")) {
  stopifnot(inherits(x,"sf") && inherits(sf::st_geometry(x),"sfc_POINT"))
  ret <- sf::st_coordinates(x)
  ret <- tibble::as_tibble(ret)
  stopifnot(length(names) == ncol(ret))
  x <- x[ , !names(x) %in% names]
  ret <- setNames(ret,names)
  dplyr::bind_cols(x,ret)
}

centroides <- grade %>% 
  dplyr::select(ID_UNICO) %>%
  st_transform(4326) %>%
  st_centroid()

centroides <- centroides %>%
  sfc_as_cols(names = c("x", "y"))
centroides <- centroides %>%
  st_set_geometry(NULL) %>%
  as.data.frame()

grade <- grade %>%
  left_join(centroides, by = "ID_UNICO")
rm(centroides)

#2.4.1.2 calcula grupo mais próximo de cada quadrado - equivale a ver em qual voronoi está o centroide de cada grade
gruposxy <- sfc_as_cols(grupos)
gruposxy <- gruposxy[,c("x","y")]
gruposxy <- st_drop_geometry(gruposxy)

gradexy <- grade[,c("x","y")]
gradexy <- st_drop_geometry(gradexy)

knn.out <- ann(as.matrix(gruposxy), as.matrix(gradexy), k=1)
grade$grupo0 <- grupos$grupo0[knn.out$knnIndexDist[,1]]
grade <- grade %>%
  left_join(grupo_para_grupo0)
  
saveRDS(grade,"grade.rds")

#2.4.2 calcula fator de expansão####
#testar se grupo faz sentido
grade_teste <- grade[grade$grupo==834 | grade$grupo==835,]
ggplot()+
  geom_sf(data=grade_teste, aes(color=grupo)) +
  geom_sf(data=grupos)

grupos_teste <- grupos[grupos$grupo==834 | grupos$grupo==835,]
ggplot()+
  geom_sf(data=grupos)+
  geom_sf(data=grupos_teste, color = "red")
#teste ok: estão no mesmo lugar!
rm(grade_teste); rm(gradexy); rm(grupos_teste); rm(gruposxy); rm(knn.out)

#calcula pop de cada grupo
x <- st_drop_geometry(grade) %>% 
  group_by(grupo) %>% 
  summarise(popu = sum(POP))

grupos <- grupos %>% left_join(x)
rm(x)

#calcula fator inicial
grupos$f_exp <- grupos$popu / grupos$moradores

colnames(grupos) <- c("grupo","grupo0","moradores_identif","popu","geometry","f_exp")

#avalia fatores
ggplot(grupos, aes(f_exp))+
  geom_histogram() #tem valores muuuito altos

ggplot(grupos[grupos$f_exp<=1000,], aes(f_exp))+ #histograma para valores menores que 1000
  geom_histogram() 

quantil <- quantile(grupos$f_exp,probs = seq(0, 1, 0.025), na.rm = TRUE)
plot(quantil[1:38]) #10% dos grupos de maior fexp descolam dos outros

#avalia grupos em sjc
SJC_limite_municipal_4326 <- read_sf("G:\\Meu Drive\\Mestrado\\Projeto FGV\\SJC_operacional\\Bases\\Mapa_base\\SJC_limite_municipal_4326.shp")
SJC_limite_municipal_4326$NM_MUNICIP <- "sjc"

grade <- st_transform(grade, 4326)

grade <- grade %>%
  st_join(SJC_limite_municipal_4326)

grade_dados <- grade
grade_dados$geometry <- NULL

grupos_sjc_lista <- grade_dados %>% 
  filter(NM_MUNICIP=="sjc") %>% 
  group_by(grupo, grupo0) %>% 
  summarise()

saveRDS(grupos_sjc_lista, file = "grupos_sjc_lista.rds")

#mapa
grade_agrupada <- grade %>%
  filter(grupo %in% grupos_sjc_lista$grupo) %>% 
  group_by(grupo) %>% 
  summarise()

ggplot()+
  geom_sf(data=SJC_limite_municipal_4326,  color = "black", size=0.7, show.legend = "line")+
  geom_sf(data=viario_4326,  color = "black", size=0.5, show.legend = "line")+
  
  geom_sf(data=grade_agrupada, fill=NA)+
  guides(fill= guide_legend(), size=guide_legend()) +
  labs(color="Moradores Identificados") +
  #theme_nothing(legend=TRUE) +
  annotation_scale(location = "bl", width_hint = 0.5, pad_x = unit(6.5,"cm"), text_cex = 0.8) + #escala
  annotation_north_arrow(location = "bl", which_north = "true", height = unit(1.3, "cm"), width = unit(1.3, "cm"), #norte 
                         pad_x = unit(0.15, "in"), pad_y = unit(3.6, "in"), #pad_y é a distância vertical até a escala
                         style = north_arrow_fancy_orienteering)+
  coord_sf(ylim = c(-23.25, -23), xlim = c(-46.25,-45.8))


saveRDS(object = grupos, file = "grupos.rds")

#2.4.4 expande viagens####
#2.4.4.1 viagens avaliadas####
#define dias de interesse - dias úteis, para comparação com OD
feriado <- ymd(20130329)
viagens <- rbind(viagens0, viagens_base_domiciliar_extras)
viagens$dias_de_interesse <- !(wday(viagens$data_i, label = T) %in% c("sáb", "dom")) & (viagens$data_i != feriado)

#filtra em função da duração das viagens
duracao_max <- 3 * 60*60 #duracao maxima convertida para segundos
viagens$na_duracao <- (viagens$duracao <= duracao_max) | (is.na(viagens$duracao))

#combina filtros
viagens$utilizadas <- viagens$dias_de_interesse & viagens$na_duracao

#2.4.4.2 fator por zona####
grupos_resumo <- grupos[,c("grupo","f_exp")]
grupos_resumo$geometry <- NULL

viagens <- viagens %>%
  left_join( select(ligadores_significativos_E_domicilio, userID, domicilio) ) %>%  #tira duas colunas do meio
  left_join(grupos_resumo, by=c("domicilio"="grupo"))

#2.4.4.3 fator por individuo####
#conta dias de cada usuário
x <- viagens %>%
  filter(utilizadas==T) %>% 
  group_by(userID) %>% 
  summarise(ndias = n_distinct(data_i))

viagens <- viagens %>%
  left_join(x)

viagens$f_dia_u <- 1/viagens$ndias

#expande viagens
viagens$fator <- viagens$f_dia_u * viagens$f_exp

#2.4.5 monta matriz####
matriz <- viagens %>% 
  filter(utilizadas==T) %>%
  group_by(grupo_i, grupo_j, criada) %>%
  summarise( viagens_exp = sum(fator, na.rm=T) )

saveRDS(object = matriz, file = "matriz.rds")

#2.4.5.1 matriz por hora####
viagens$hora_i_exata <- as.numeric( floor(viagens$hora_i/60/60) )
saveRDS(viagens,"viagens.rds")

matriz_hora <- viagens %>% 
  filter(utilizadas==T) %>%
  group_by(grupo_i, grupo_j, hora_i_exata , criada) %>%
  summarise( viagens_exp = sum(fator, na.rm=T) )

#2.4.5.2 converte para zoneamento da OD####

#2.4.5.2.1 filtra viagens de domicílios de sjc
viagens_sjc <- viagens %>%
  semi_join(grupos_sjc_lista, by=c("domicilio"="grupo"))

saveRDS(viagens_sjc,"viagens_sjc.rds")

matriz_hora_sjc <- viagens_sjc %>% 
  filter(utilizadas==T) %>%
  group_by(grupo_i, grupo_j, hora_i_exata, criada ) %>%
  summarise( viagens_exp_sjc = sum(fator, na.rm=T) )

matriz_hora <- matriz_hora %>% 
  left_join(matriz_hora_sjc)
matriz_hora$viagens_exp_sjc <- matriz_hora$viagens_exp_sjc %>%
  replace_na(0)

#2.4.5.2.1.1 remove viagens que não saem de sjc
matriz_hora_sjc_i <- viagens_sjc %>% 
  filter(utilizadas==T, grupo_i %in% grupos_sjc_lista$grupo, grupo_j %in% grupos_sjc_lista$grupo) %>%
  group_by(grupo_i, grupo_j, hora_i_exata, criada ) %>%
  summarise( viagens_exp_sjc_i = sum(fator, na.rm=T) )

matriz_hora <- matriz_hora %>% 
  left_join(matriz_hora_sjc_i)
matriz_hora$viagens_exp_sjc_i <- matriz_hora$viagens_exp_sjc_i %>%
  replace_na(0)

#2.4.5.2.1.1.1 dividindo em 3 partes sem considerar vizinhos
  #A com origem no domicilio
  matriz_hora_sjc_i_OriDom <- viagens_sjc %>% 
    filter(utilizadas==T, grupo_i %in% grupos_sjc_lista$grupo, grupo_j %in% grupos_sjc_lista$grupo, grupo_i==domicilio) %>%
    group_by(grupo_i, grupo_j, hora_i_exata, criada ) %>%
    summarise( viagens_exp_sjc_i_OriDom = sum(fator, na.rm=T) )
  
  #B com destino no domicilio
  matriz_hora_sjc_i_DestDom <- viagens_sjc %>% 
    filter(utilizadas==T, grupo_i %in% grupos_sjc_lista$grupo, grupo_j %in% grupos_sjc_lista$grupo, grupo_j==domicilio) %>%
    group_by(grupo_i, grupo_j, hora_i_exata, criada ) %>%
    summarise( viagens_exp_sjc_i_DestDom = sum(fator, na.rm=T) )
  
  #C com O e D não no domicilio
  matriz_hora_sjc_i_SemDom <- viagens_sjc %>% 
    filter(utilizadas==T, grupo_i %in% grupos_sjc_lista$grupo, grupo_j %in% grupos_sjc_lista$grupo, grupo_i!=domicilio, grupo_j!=domicilio) %>%
    group_by(grupo_i, grupo_j, hora_i_exata, criada ) %>%
    summarise( viagens_exp_sjc_i_SemDom = sum(fator, na.rm=T) )

  matriz_hora <- matriz_hora %>% 
    left_join(matriz_hora_sjc_i_OriDom) %>% 
    left_join(matriz_hora_sjc_i_DestDom) %>% 
    left_join(matriz_hora_sjc_i_SemDom)
  
  matriz_hora$viagens_exp_sjc_i_OriDom <- matriz_hora$viagens_exp_sjc_i_OriDom %>%
    replace_na(0)
  matriz_hora$viagens_exp_sjc_i_DestDom <- matriz_hora$viagens_exp_sjc_i_DestDom %>%
    replace_na(0)
  matriz_hora$viagens_exp_sjc_i_SemDom <- matriz_hora$viagens_exp_sjc_i_SemDom %>%
    replace_na(0)
  
  saveRDS(object = matriz_hora, file = "matriz_hora.rds")
  
#2.4.5.2.1.1.2 dividindo em 3 partes, considerando novo método que agrupa polígonos vizinhos na identificação do domicílio####
#2.4.5.2.1.1.2.1 identifica vizinhos
#gera voronoi
grupos_multi <- st_union(grupos) #voronoi precisa dessa mudança de propriedade para rodar
grupos_vor <- st_voronoi(grupos_multi)
grupos_vor <- st_cast(grupos_vor)

st_write(grupos_vor, "grupos_vor.shp") #precisei salvar fora e importar para transformar em sf
grupos_vor <- read_sf("grupos_vor.shp")

grupos_vor <- grupos_vor %>%
  st_join(grupos)

grupos_vor <- grupos_vor %>% 
  filter(grupo0 %in% grupos_sjc_lista$grupo0)

ggplot()+
  geom_sf(data=grupos_vor, color='black')+
  geom_sf_text(data=grupos_vor, aes(label=grupo), size=2.5)+
  coord_sf(xlim=c(-46.11,-45.7),ylim=c(-23.33,-23))

#identifica voronois vizinhos
grupos_vor_sp <- as(grupos_vor, 'Spatial')

row.names(grupos_vor_sp) <- as.character(grupos_vor_sp$grupo0)
nb <- poly2nb(grupos_vor_sp)
vizinhos <- nb2mat(nb, style="B")
colnames(vizinhos) <- rownames(vizinhos)

#transforma em dataframe
vizinhos_df <-  as.data.frame(as.table(vizinhos))
vizinhos_df <- vizinhos_df %>%
  filter(Freq==1)
vizinhos_df <- vizinhos_df[,1:2]
colnames(vizinhos_df) <- c('grupo1','grupo2')
vizinhos_df$grupo1 <- as.numeric(paste(vizinhos_df$grupo1))
vizinhos_df$grupo2 <- as.numeric(paste(vizinhos_df$grupo2))

saveRDS(object = vizinhos_df, file = "vizinhos_df.rds")

#2.4.5.2.1.1.2.2 reclassifica viagens quanto à base domiciliar considerando vizinhos
  #os que não tem O e D no domicilio são refinados para buscar os que tem
  viagens_sjc_SemDom <- viagens_sjc %>% 
    filter(utilizadas==T, grupo_i %in% grupos_sjc_lista$grupo,
           grupo_j %in% grupos_sjc_lista$grupo,
           grupo_i!=domicilio, grupo_j!=domicilio)
  
  #inicia variaveis
  viagens_sjc_SemDom$grupo_i_vizDom <- F
  viagens_sjc_SemDom$grupo_j_vizDom <- F
  
  for(i in 1:nrow(viagens_sjc_SemDom)){
    vizinhos_dom <- vizinhos_df %>% 
      filter(grupo1==viagens_sjc_SemDom$domicilio[i])
    
    viagens_sjc_SemDom$grupo_i_vizDom[i] <-
      (viagens_sjc_SemDom$grupo_i[i] %in% vizinhos_dom$grupo2)
    viagens_sjc_SemDom$grupo_j_vizDom[i] <-
      (viagens_sjc_SemDom$grupo_j[i] %in% vizinhos_dom$grupo2)
  }
  
  #A- com origem no domicilio - O em vizinho do domicilio e D não
  matriz_hora_sjc_i_OriDom_Cviz <- viagens_sjc_SemDom %>%
    filter(grupo_i_vizDom==T, grupo_j_vizDom==F) %>% 
    group_by(grupo_i, grupo_j, hora_i_exata, criada ) %>%
    summarise( viagens_exp_sjc_i_OriDom_Cviz = sum(fator, na.rm=T) )
  
  matriz_hora <- matriz_hora %>% 
    left_join(matriz_hora_sjc_i_OriDom_Cviz)
  matriz_hora$viagens_exp_sjc_i_OriDom_Cviz <-
    matriz_hora$viagens_exp_sjc_i_OriDom_Cviz %>%
    replace_na(0)
  
  matriz_hora$viagens_exp_sjc_i_OriDom_Cviz <- 
    matriz_hora$viagens_exp_sjc_i_OriDom_Cviz +
    matriz_hora$viagens_exp_sjc_i_OriDom
    
  #B- com destino no domicilio - D em vizinho do domicilio e O não
  matriz_hora_sjc_i_DestDom_Cviz <- viagens_sjc_SemDom %>%
    filter(grupo_j_vizDom==T, grupo_i_vizDom==F) %>% 
    group_by(grupo_i, grupo_j, hora_i_exata, criada ) %>%
    summarise( viagens_exp_sjc_i_DestDom_Cviz = sum(fator, na.rm=T) )
  
  matriz_hora <- matriz_hora %>% 
    left_join(matriz_hora_sjc_i_DestDom_Cviz)
  matriz_hora$viagens_exp_sjc_i_DestDom_Cviz <-
    matriz_hora$viagens_exp_sjc_i_DestDom_Cviz %>%
    replace_na(0)
  
  matriz_hora$viagens_exp_sjc_i_DestDom_Cviz <- 
    matriz_hora$viagens_exp_sjc_i_DestDom_Cviz +
    matriz_hora$viagens_exp_sjc_i_DestDom
  
  #C- sem domicilio - O e D em vizinho do domicilio ou os 2 sem vizinho
  matriz_hora_sjc_i_SemDom_Cviz <- viagens_sjc_SemDom %>%
    filter((grupo_i_vizDom==T & grupo_j_vizDom==T) | (grupo_i_vizDom==F & grupo_j_vizDom==F)) %>% 
    group_by(grupo_i, grupo_j, hora_i_exata, criada ) %>%
    summarise( viagens_exp_sjc_i_SemDom_Cviz = sum(fator, na.rm=T) )
  
  matriz_hora <- matriz_hora %>% 
    left_join(matriz_hora_sjc_i_SemDom_Cviz)
  matriz_hora$viagens_exp_sjc_i_SemDom_Cviz <-
    matriz_hora$viagens_exp_sjc_i_SemDom_Cviz %>%
    replace_na(0)
  
  saveRDS(object = matriz_hora, file = "matriz_hora.rds")
  
#2.4.5.2.2 converte para zoneamento da OD ####
zoneamento_OD <- read_sf("zoneamento OD\\zoneamento_OD.shp") %>% 
    st_transform(4326) %>%
    select(ZAT_55)

#calcula fator zona->grupo
grade <- st_transform(grade, 4326)

grade <- grade %>%
  st_join(zoneamento_OD)

grade_dados <- grade
grade_dados$geometry <- NULL
grade_dados$ZAT_55 <- grade_dados$ZAT_55 %>%
  replace_na(999)

#atribui peso 1 para quadrados pequenos e 25 para grandes
grade_dados$peso_area <- 25 - (((str_sub(grade_dados$ID_UNICO,1,1) %>%
  as.numeric()) -1) *24)

grade_grupo <- grade_dados %>% #pra cada grade, mostra ZAT_55, grupo e fator dado por inverso do número de grades no grupo
  select(ID_UNICO, ZAT_55, grupo) %>% 
  left_join(grade_dados %>%
              group_by(grupo) %>% 
              summarise(fator_grades_no_grupo = 1/sum(peso_area)) ) %>% 
  left_join(grade_dados %>%
              group_by(grupo) %>% 
              filter(peso_area==1) %>% 
              summarise(fator_grades_no_grupo_urb = 1/sum(peso_area)) )

zona_grupo <- grade_grupo %>% 
  group_by(ZAT_55, grupo) %>% 
  summarise(fator_grupo_p_zona = sum(fator_grades_no_grupo, na.rm=T),
            fator_grupo_p_zona_urb = sum(fator_grades_no_grupo_urb, na.rm=T))


saveRDS(zona_grupo, file = "zona_grupo.rds")

#2.4.5.2.2.1 para matriz completa
#faz correspondecia entre O e D com fatores correspondentes
matriz_hora2 <- matriz_hora %>%
  left_join(zona_grupo, by = c("grupo_i" = "grupo"))
matriz_hora2 <- matriz_hora2 %>%
  left_join(zona_grupo, by = c("grupo_j" = "grupo"), suffix=c(".i",".j"))

matriz_hora2$ZAT_55.i <- matriz_hora2$ZAT_55.i %>%
  replace_na(999)
matriz_hora2$ZAT_55.j <- matriz_hora2$ZAT_55.j %>%
  replace_na(999)

#calcula total de viagens *fatores
matriz_hora2$fatores <- matriz_hora2$fator_grupo_p_zona.i *
  matriz_hora2$fator_grupo_p_zona.j
matriz_hora2$fatores_urb <- matriz_hora2$fator_grupo_p_zona_urb.i *
  matriz_hora2$fator_grupo_p_zona_urb.j

matriz_hora2[,paste0(c("viagens_exp","viagens_exp_sjc","viagens_exp_sjc_i",
        "viagens_exp_sjc_i_OriDom","viagens_exp_sjc_i_DestDom",
        "viagens_exp_sjc_i_SemDom","viagens_exp_sjc_i_OriDom_Cviz",
        "viagens_exp_sjc_i_DestDom_Cviz","viagens_exp_sjc_i_SemDom_Cviz"),
        "_urb")] <- 
  matriz_hora2[,c("viagens_exp","viagens_exp_sjc","viagens_exp_sjc_i",
        "viagens_exp_sjc_i_OriDom","viagens_exp_sjc_i_DestDom",
        "viagens_exp_sjc_i_SemDom","viagens_exp_sjc_i_OriDom_Cviz",
        "viagens_exp_sjc_i_DestDom_Cviz","viagens_exp_sjc_i_SemDom_Cviz")]*
  matriz_hora2$fatores_urb
  
matriz_hora2[,c("viagens_exp","viagens_exp_sjc","viagens_exp_sjc_i",
                "viagens_exp_sjc_i_OriDom","viagens_exp_sjc_i_DestDom",
                "viagens_exp_sjc_i_SemDom","viagens_exp_sjc_i_OriDom_Cviz",
                "viagens_exp_sjc_i_DestDom_Cviz","viagens_exp_sjc_i_SemDom_Cviz")] <- 
  matriz_hora2[,c("viagens_exp","viagens_exp_sjc","viagens_exp_sjc_i",
                  "viagens_exp_sjc_i_OriDom","viagens_exp_sjc_i_DestDom",
                  "viagens_exp_sjc_i_SemDom","viagens_exp_sjc_i_OriDom_Cviz",
                  "viagens_exp_sjc_i_DestDom_Cviz","viagens_exp_sjc_i_SemDom_Cviz")]*
  matriz_hora2$fatores

#calcula matriz resultante
matriz_hora_sjc_z <- matriz_hora2 %>% 
  group_by(zona_i = ZAT_55.i, zona_j = ZAT_55.j, hora_i_exata, criada) %>% 
  summarise(# viagens_exp = sum(viagens_exp, na.rm=T),
            viagens_exp_sjc = sum(viagens_exp_sjc, na.rm=T),
            viagens_exp_sjc_i = sum(viagens_exp_sjc_i, na.rm=T),
            viagens_exp_sjc_i_OriDom = sum(viagens_exp_sjc_i_OriDom, na.rm=T),
            viagens_exp_sjc_i_DestDom = sum(viagens_exp_sjc_i_DestDom, na.rm=T),
            viagens_exp_sjc_i_SemDom = sum(viagens_exp_sjc_i_SemDom, na.rm=T),
            viagens_exp_sjc_i_OriDom_Cviz = sum(viagens_exp_sjc_i_OriDom_Cviz, na.rm=T),
            viagens_exp_sjc_i_DestDom_Cviz = sum(viagens_exp_sjc_i_DestDom_Cviz, na.rm=T),
            viagens_exp_sjc_i_SemDom_Cviz = sum(viagens_exp_sjc_i_SemDom_Cviz, na.rm=T),
            # viagens_exp_urb = sum(viagens_exp_urb, na.rm=T),
            viagens_exp_sjc_urb = sum(viagens_exp_sjc_urb, na.rm=T),
            viagens_exp_sjc_i_urb = sum(viagens_exp_sjc_i_urb, na.rm=T),
            viagens_exp_sjc_i_OriDom_urb = sum(viagens_exp_sjc_i_OriDom_urb, na.rm=T),
            viagens_exp_sjc_i_DestDom_urb = sum(viagens_exp_sjc_i_DestDom_urb, na.rm=T),
            viagens_exp_sjc_i_SemDom_urb = sum(viagens_exp_sjc_i_SemDom_urb, na.rm=T),
            viagens_exp_sjc_i_OriDom_Cviz_urb = sum(viagens_exp_sjc_i_OriDom_Cviz_urb, na.rm=T),
            viagens_exp_sjc_i_DestDom_Cviz_urb = sum(viagens_exp_sjc_i_DestDom_Cviz_urb, na.rm=T),
            viagens_exp_sjc_i_SemDom_Cviz_urb = sum(viagens_exp_sjc_i_SemDom_Cviz_urb, na.rm=T),
  )

#substitui NA por 0
matriz_hora_sjc_z$viagens_exp_sjc <- matriz_hora_sjc_z$viagens_exp_sjc %>% 
  replace_na(0)
matriz_hora_sjc_z$viagens_exp_sjc_i <- matriz_hora_sjc_z$viagens_exp_sjc_i %>% 
  replace_na(0)
matriz_hora_sjc_z$viagens_exp_sjc_i_OriDom <- matriz_hora_sjc_z$viagens_exp_sjc_i_OriDom %>% 
  replace_na(0)
matriz_hora_sjc_z$viagens_exp_sjc_i_DestDom <- matriz_hora_sjc_z$viagens_exp_sjc_i_DestDom %>% 
  replace_na(0)
matriz_hora_sjc_z$viagens_exp_sjc_i_SemDom <- matriz_hora_sjc_z$viagens_exp_sjc_i_SemDom %>% 
  replace_na(0)
matriz_hora_sjc_z$viagens_exp_sjc_i_OriDom_Cviz <- matriz_hora_sjc_z$viagens_exp_sjc_i_OriDom_Cviz %>% 
  replace_na(0)
matriz_hora_sjc_z$viagens_exp_sjc_i_DestDom_Cviz <- matriz_hora_sjc_z$viagens_exp_sjc_i_DestDom_Cviz %>% 
  replace_na(0)
matriz_hora_sjc_z$viagens_exp_sjc_i_SemDom_Cviz <- matriz_hora_sjc_z$viagens_exp_sjc_i_SemDom_Cviz %>% 
  replace_na(0)

matriz_hora_sjc_z$viagens_exp_sjc_urb <- matriz_hora_sjc_z$viagens_exp_sjc_urb %>% 
  replace_na(0)
matriz_hora_sjc_z$viagens_exp_sjc_i_urb <- matriz_hora_sjc_z$viagens_exp_sjc_i_urb %>% 
  replace_na(0)
matriz_hora_sjc_z$viagens_exp_sjc_i_OriDom_urb <- matriz_hora_sjc_z$viagens_exp_sjc_i_OriDom_urb %>% 
  replace_na(0)
matriz_hora_sjc_z$viagens_exp_sjc_i_DestDom_urb <- matriz_hora_sjc_z$viagens_exp_sjc_i_DestDom_urb %>% 
  replace_na(0)
matriz_hora_sjc_z$viagens_exp_sjc_i_SemDom_urb <- matriz_hora_sjc_z$viagens_exp_sjc_i_SemDom_urb %>% 
  replace_na(0)
matriz_hora_sjc_z$viagens_exp_sjc_i_OriDom_Cviz_urb <- matriz_hora_sjc_z$viagens_exp_sjc_i_OriDom_Cviz_urb %>% 
  replace_na(0)
matriz_hora_sjc_z$viagens_exp_sjc_i_DestDom_Cviz_urb <- matriz_hora_sjc_z$viagens_exp_sjc_i_DestDom_Cviz_urb %>% 
  replace_na(0)
matriz_hora_sjc_z$viagens_exp_sjc_i_SemDom_Cviz_urb <- matriz_hora_sjc_z$viagens_exp_sjc_i_SemDom_Cviz_urb %>% 
  replace_na(0)  

#passa criada para coluna
temp_criada <- matriz_hora_sjc_z %>% 
  filter(criada==T) %>% 
  group_by(zona_i, zona_j, hora_i_exata) %>% 
  summarise_all(sum)

temp_n_criada <- matriz_hora_sjc_z %>%
  filter(criada==F) %>% 
  group_by(zona_i, zona_j, hora_i_exata) %>% 
  summarise_all(sum)

matriz_hora_sjc_z <- temp_criada %>% 
  full_join(temp_n_criada, by=c("zona_i", "zona_j", "hora_i_exata"),
            suffix=c("_Criada","_SemCriada"))

matriz_hora_sjc_z$criada_Criada <- NULL
matriz_hora_sjc_z$criada_SemCriada <- NULL

saveRDS(object = matriz_hora_sjc_z, file = "matriz_hora_sjc_z.rds")

#extra - mapa voronoi sjc####
grupos_vor <- read_sf("grupos_vor.shp")

grupos_vor <- grupos_vor %>%
  st_join(grupos)

grupos_agregados <- grupos %>% 
  group_by(grupo) %>% 
  summarise(conta=n()) %>% 
  filter(conta>1)

# grupos_vor <- grupos_vor %>% 
#   filter(grupo0 %in% grupos_sjc_lista$grupo0)

munic <- read_municipality(code_muni="SP", year=2018)

munic_prox <- munic %>%
  filter(name_muni %in% c('São José Dos Campos', 'Caçapava', 'Jacareí','Taubaté', 'Monteiro Lobato', 'Jambeiro', 'Igaratá', 'Santa Branca'))
sjc <- munic %>%
  filter(name_muni %in% c('São José Dos Campos'))

temp_plot= ggplot()+
  geom_sf(data=grupos_vor, fill=NA)+
  geom_sf(data=munic_prox, fill=NA, size=1)+
  geom_sf_label(data=munic_prox, aes(label=name_muni))+
  geom_sf(data=sjc, color='red', fill=NA, size=1.3)+
  coord_sf(xlim=c(-46.11,-45.7),ylim=c(-23.31,-22.83))+
  theme_nothing(legend=F)+
  annotation_scale(location = "bl", width_hint = 0.5, pad_x = unit(5.2,"cm"), text_cex = 0.8) + #escala
  annotation_north_arrow(location = "bl", which_north = "true", height = unit(1.3, "cm"), width = unit(1.3, "cm"), #norte 
                         pad_x = unit(0.15, "in"), pad_y = unit(5, "in"), #pad_y é a distância vertical até a escala
                         style = north_arrow_fancy_orienteering)

temp_plot

ggsave(temp_plot, file='mapa_voronois_sjc.png')

#mapa voronoi grupos unidos
temp_plot= ggplot()+
  geom_sf(data=(grupos_vor %>% filter(grupo %in% grupos_agregados$grupo)), aes(fill=factor(grupo)))+
  geom_sf(data=grupos_vor, fill=NA)+
  geom_sf(data=munic_prox, fill=NA, size=1)+
  geom_sf_label(data=munic_prox, aes(label=name_muni))+
  geom_sf(data=sjc, color='red', fill=NA, size=1.3)+
  coord_sf(xlim=c(-46.11,-45.7),ylim=c(-23.31,-22.83))+
  theme_nothing(legend=F)+
  annotation_scale(location = "bl", width_hint = 0.5, pad_x = unit(5.2,"cm"), text_cex = 0.8) + #escala
  annotation_north_arrow(location = "bl", which_north = "true", height = unit(1.3, "cm"), width = unit(1.3, "cm"), #norte 
                         pad_x = unit(0.15, "in"), pad_y = unit(5, "in"), #pad_y é a distância vertical até a escala
                         style = north_arrow_fancy_orienteering)

temp_plot

ggsave(temp_plot, file='mapa_voronois_sjc_gruposUnidos.png')
