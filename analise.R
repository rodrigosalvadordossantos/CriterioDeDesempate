library(openxlsx)
library(dplyr)
library(tibble)

setwd("C:\\Users\\Rodrigo\\DataScience\\Sports\\CriterioDeDesempate\\xG")

#Leitura de arquivos
files = list.files(getwd(), pattern=".csv", full.names=T)
tabela = read.csv(files[1],1,stringsAsFactors = F)

tabela.br20 <- tabela %>%
  filter(league_id == 2105) %>%
  filter(season == 2020) %>%
  select(c(date, team1, team2, proj_score1, proj_score2, score1, score2, xg1, xg2))

Encoding(tabela.br20[,"team1"]) <- "UTF-8"
Encoding(tabela.br20[,"team2"]) <- "UTF-8"

#ajuse temporário
# tabela.br20[1,"score1"]<-tabela.br20[1,"proj_score1"]
# tabela.br20[1,"score2"]<-tabela.br20[1,"proj_score2"]
# tabela.br20[5,"score1"]<-tabela.br20[5,"proj_score1"]
# tabela.br20[5,"score2"]<-tabela.br20[5,"proj_score2"]
# tabela.br20[1,"xg1"]<-tabela.br20[1,"proj_score1"]
# tabela.br20[1,"xg2"]<-tabela.br20[1,"proj_score2"]
# tabela.br20[5,"xg1"]<-tabela.br20[5,"proj_score1"]
# tabela.br20[5,"xg2"]<-tabela.br20[5,"proj_score2"]
# tabela.br20[4,"xg1"]<-tabela.br20[4,"proj_score1"]
# tabela.br20[4,"xg2"]<-tabela.br20[4,"proj_score2"]
# tabela.br20[9,"xg1"]<-tabela.br20[9,"proj_score1"]
# tabela.br20[9,"xg2"]<-tabela.br20[9,"proj_score2"]
#View(tabela.br20)

times <- unique(tabela.br20$team1)
classif.br20 <- tibble(times[order(times)])

#ATAQUE
#----score-proj_score
dif.proj.casa <- tabela.br20 %>%
  filter(!is.na(score1)) %>%
  group_by(team1) %>%
  summarise(n = sum(score1-proj_score1)) %>%
  select(n)
dif.proj.fora <- tabela.br20 %>%
  filter(!is.na(score2)) %>%
  group_by(team2) %>%
  summarise(n = sum(score2-proj_score2)) %>%
  select(n)
dpa <- dif.proj.casa+dif.proj.fora

classif.br20 <- classif.br20 %>%
  add_column(dif.proj.ataque=dpa)

#----score-xg
dif.xg.casa <- tabela.br20 %>%
  filter(!is.na(score1)) %>%
  filter(!is.na(xg1)) %>%
  group_by(team1) %>%
  summarise(n = sum(score1-xg1)) %>%
  select(n)
dif.xg.fora <- tabela.br20 %>%
  filter(!is.na(score2)) %>%
  filter(!is.na(xg2)) %>%
  group_by(team2) %>%
  summarise(n = sum(score2-xg2)) %>%
  select(n)
dxa <- dif.xg.casa+dif.xg.fora

classif.br20 <- classif.br20 %>%
  add_column(dif.xg.ataque=dxa)

#----xg-proj_score
dif.stat.casa <- tabela.br20 %>%
  filter(!is.na(score1)) %>%
  filter(!is.na(xg1)) %>%
  group_by(team1) %>%
  summarise(n = sum(xg1-proj_score1)) %>%
  select(n)
dif.stat.fora <- tabela.br20 %>%
  filter(!is.na(score2)) %>%
  filter(!is.na(xg2)) %>%
  group_by(team2) %>%
  summarise(n = sum(xg2-proj_score2)) %>%
  select(n)
dsa <- dif.stat.casa+dif.stat.fora

classif.br20 <- classif.br20 %>%
  add_column(dif.stat.ataque=dsa)

#----DEFESA
#----score-proj_score
dif.proj.casa <- tabela.br20 %>%
  filter(!is.na(score2)) %>%
  group_by(team1) %>%
  summarise(n = sum(score2-proj_score2)) %>%
  select(n)
dif.proj.fora <- tabela.br20 %>%
  filter(!is.na(score1)) %>%
  group_by(team2) %>%
  summarise(n = sum(score1-proj_score1)) %>%
  select(n)
dpd <- dif.proj.casa+dif.proj.fora

classif.br20 <- classif.br20 %>%
  add_column(dif.proj.defesa=dpd)

#----score-xg
dif.xg.casa <- tabela.br20 %>%
  filter(!is.na(score2)) %>%
  filter(!is.na(xg2)) %>%
  group_by(team1) %>%
  summarise(n = sum(score2-xg2)) %>%
  select(n)
dif.xg.fora <- tabela.br20 %>%
  filter(!is.na(score1)) %>%
  filter(!is.na(xg1)) %>%
  group_by(team2) %>%
  summarise(n = sum(score1-xg1)) %>%
  select(n)
dxd <- dif.xg.casa+dif.xg.fora

classif.br20 <- classif.br20 %>%
  add_column(dif.xg.defesa=dxd)

#----xg-proj_score
dif.stat.casa <- tabela.br20 %>%
  filter(!is.na(score2)) %>%
  filter(!is.na(xg2)) %>%
  group_by(team1) %>%
  summarise(n = sum(xg2-proj_score2)) %>%
  select(n)
dif.stat.fora <- tabela.br20 %>%
  filter(!is.na(score1)) %>%
  filter(!is.na(xg1)) %>%
  group_by(team2) %>%
  summarise(n = sum(xg1-proj_score1)) %>%
  select(n)
dsd <- dif.stat.casa+dif.stat.fora

classif.br20 <- classif.br20 %>%
  add_column(dif.stat.defesa=dsd)

#----DESEMPENHO
dif.des.casa <- tabela.br20 %>%
  filter(!is.na(xg1)) %>%
  filter(!is.na(xg2)) %>%
  group_by(team1) %>%
  summarise(n = sum(xg1-xg2)) %>%
  select(n)
dif.des.fora <- tabela.br20 %>%
  filter(!is.na(xg1)) %>%
  filter(!is.na(xg2)) %>%
  group_by(team2) %>%
  summarise(n = sum(xg2-xg1)) %>%
  select(n)
dsp <- dif.des.casa+dif.des.fora

classif.br20 <- classif.br20 %>%
  add_column(desempenho=dsp)

#----xgAtaque
xg.ataque.casa <- tabela.br20 %>%
  filter(!is.na(xg1)) %>%
  group_by(team1)%>%
  summarise(n=sum(xg1)) %>%
  select(n)
xg.ataque.fora <- tabela.br20 %>%
  filter(!is.na(xg2)) %>%
  group_by(team2)%>%
  summarise(n=sum(xg2)) %>%
  select(n)
xga <- xg.ataque.casa+xg.ataque.fora

classif.br20 <- classif.br20 %>%
  add_column(xg.ataque=xga)

#----xgDefesa
xg.defesa.casa <- tabela.br20 %>%
  filter(!is.na(xg2)) %>%
  group_by(team1)%>%
  summarise(n=sum(xg2)) %>%
  select(n)
xg.defesa.fora <- tabela.br20 %>%
  filter(!is.na(xg1)) %>%
  group_by(team2)%>%
  summarise(n=sum(xg1)) %>%
  select(n)
xgd <- xg.defesa.casa+xg.defesa.fora

classif.br20 <- classif.br20 %>%
  add_column(xg.defesa=xgd)

View(tabela.br20[grepl("Santos",tabela.br20$team1) |
                   grepl("Santos",tabela.br20$team2),])
#View(classif.br20)

#classif.br20.old <- classif.br20


