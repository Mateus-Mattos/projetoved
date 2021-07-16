library(tidyverse)
library(readxl)
library(magrittr)

# Carregando dados, obs: Os dados de população residente do data sus são dividios em duas fontes diferentes  o ibge e o tcu, por isso há uma ruptura nos arquivos da população



dados <- readxl::read_xls("homicidios.xls")
população_1980_2012 <- read_delim("população_1980_2012.txt",
                                  ";",col_types = cols(.default = col_integer(),
                                                       `Unidade da Federação` = col_character(),
                                                       `Codigo` = col_character()))
                                                       
                                    
população_2012_2018 <-  read_delim("polulação_2013_2018.txt",
                                   ";",col_types = cols(.default = col_integer(),
                                                                    `Unidade da Federação` = col_character(),
                                                                    `Codigo` = col_character()))

#juntando as duas populações e reformatando o dados
                                    
população <- merge.data.frame(população_1980_2012,população_2012_2018[ ,c(1,3:9)]
, by = "Codigo")
população <- população %>% pivot_longer(cols =  3:42,
                                     names_to = "ano",
                                     values_to = "população")


#  Adcionando as regiões geográficas 



dados <- dados %>% 
  mutate(região =  case_when(
    dados$Sigla %in% c("RS","PR","SC") ~ 'sul',
    dados$Sigla %in% c("RJ","SP","MG","ES") ~ 'sudeste',
    dados$Sigla %in% c("GO","MT","MS","DF") ~ 'centro-oeste',
    dados$Sigla %in% c("CE","PI","BA","MA","RN","PB","PE","SE","AL") ~ 'nordeste',
    dados$Sigla %in% c("AM", "AC", "PA", "RO", "RR", "AP", "TO") ~ 'norte')) 
  
               
#Reformatando os dados


dados_mapa <- dados %>% pivot_longer(cols =  4:42,
                                names_to = "ano",
                                values_to = "homicidios")
# Juntando as bases de dados
dados_mapa <- merge.data.frame(dados_mapa,população[ ,c(1,3,4)], by.x =c("ano","Codigo"), by.y =c("ano","Codigo"))


# Não é uma boa ideia colocarmos homicidios per capita devido ao valor muito baixo ( Chegamos a ordem de 10^-7)
dados_mapa <- dados_mapa %>% mutate(homicidios_100khabitantes = round(dados_mapa$homicidios / dados_mapa$população *10^5, digits = 2))
#Arrendado para 2 a.s


# salvar dados para logar em tebleau


write.csv2(dados_mapa, file = "dados_homicidio_100khab.csv")
