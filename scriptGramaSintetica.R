library(janitor)
library(openxlsx)
library(Hmisc)
library(stringr)
library(stringi)
library(lubridate)
library(data.table)
library(plyr)

#Importação dos dados
# Obtido de https://www.kaggle.com/adaoduque/campeonato-brasileiro-de-futebol/data
diretorio = "C:\\Users\\Rodrigo\\Downloads\\campeonato-brasileiro-de-futebol\\"
files = list.files(diretorio, pattern=".xlsx", full.names=T)
tabela = read.xlsx(files[1],1)
#times = sort(unique(tabela$Clube.1))

#Ajustes
tabela$Data = excel_numeric_to_date(tabela$Data)
tabela$Clube.1 = capitalize(tolower(tabela$Clube.1))
tabela$Clube.2 = capitalize(tolower(tabela$Clube.2))
tabela$Vencedor = capitalize(tolower(tabela$Vencedor))
#sort(unique(capitalize(tolower(tabela$Clube.1))))
#sort(unique(capitalize(tolower(tabela$Clube.2))))
tabela[tabela$Clube.1=="Atlético-pr",]$Clube.1 = "Athletico.pr"
tabela[tabela$Clube.2=="Atlético-pr",]$Clube.2 = "Athletico.pr"
tabela[tabela$Vencedor=="Atlético-pr",]$Vencedor = "Athletico.pr"
tabela[tabela$Clube.1=="Barueri",]$Clube.1 = "Gremio.prudente"
tabela[tabela$Clube.2=="Barueri",]$Clube.2 = "Gremio.prudente"
tabela[tabela$Vencedor=="Barueri",]$Vencedor = "Gremio.prudente"
tabela[tabela$Vencedor=="Botafogo",]$Vencedor = "Botafogo.rj"
tabela$Clube.1 = str_replace(str_replace(stri_trans_general(tabela$Clube.1,"Latin-ASCII")," ","."),"-",".")
tabela$Clube.2 = str_replace(str_replace(stri_trans_general(tabela$Clube.2,"Latin-ASCII")," ","."),"-",".")
tabela$Vencedor = str_replace(str_replace(stri_trans_general(tabela$Vencedor,"Latin-ASCII")," ","."),"-",".")
tabela <- tabela[-6722,]
times = sort(unique(capitalize(tolower(tabela$Clube.1))))

# vetores = function (tabela){
#   df = list()
#   for (i in 1:length(times)){
#     df[[i]] = rbind(tabela[tabela$Clube.1==times[i],],tabela[tabela$Clube.2==times[i],])
#     df[[i]] = df[[i]][order(df[[i]]$Data),]
#   }
#   return(df)
# }
# 
# todos.jogos.por.time = vetores(tabela)
# names(todos.jogos.por.time) = times

tabela.cap <- tabela[tabela$Clube.1 =="Athletico.pr" | tabela$Clube.2 =="Athletico.pr",]
tabela.sem.cap <- tabela[tabela$Clube.1 !="Athletico.pr" & tabela$Clube.2 !="Athletico.pr",]
tj.cap.sintetica <- tabela.cap[year(tabela.cap$Data)>2015,]
tj.cap.natural <- tabela.cap[year(tabela.cap$Data)<2016,]
#tj.sintetica <- tabela[grepl("Athletico.pr",tabela$Clube.1)
#                         & year(tabela$Data)>2015,]
tj.natural <- tabela[!(grepl("Athletico.pr",tabela$Clube.1)
                       & year(tabela$Data)>2015),]

#nrow(tj.natural)
#nrow(tj.sintetica)
nrow(tabela)
nrow(tabela.cap)
nrow(tabela.sem.cap)
nrow(tj.cap.sintetica)
nrow(tj.cap.natural)
cap = "Athletico.pr"

#Pontos
pontos.sintetica.mandante <- nrow(
  tj.cap.sintetica[tj.cap.sintetica$Vencedor == cap & 
                     tj.cap.sintetica$Clube.1 == cap,])*3 +
  nrow(tj.cap.sintetica[tj.cap.sintetica$Vencedor == "." & 
                          tj.cap.sintetica$Clube.1 == cap,])
pontos.sintetica.visitante <- nrow(
  tj.cap.sintetica[tj.cap.sintetica$Vencedor == cap & 
                     tj.cap.sintetica$Clube.2 == cap,])*3 + 
  nrow(tj.cap.sintetica[tj.cap.sintetica$Vencedor == "." & 
                      tj.cap.sintetica$Clube.2 == cap,])
pontos.natural.mandante <- nrow(tj.natural[tj.natural$Vencedor == tj.natural$Clube.1,])*3 +
  nrow(tj.natural[tj.natural$Vencedor == ".",])
pontos.natural.visitante <- nrow(tj.natural[tj.natural$Vencedor == tj.natural$Clube.2,])*3 +
  nrow(tj.natural[tj.natural$Vencedor == ".",])

tabela.pontos <- data.frame(
  Campo=c("Natural","Sintético","Total"),
  PontosMandante=c(pontos.natural.mandante,pontos.sintetica.mandante,pontos.natural.mandante+pontos.sintetica.mandante),
  PontosVisitante=c(pontos.natural.visitante,pontos.sintetica.visitante,pontos.natural.visitante+pontos.sintetica.visitante),
  Total=c(pontos.natural.mandante+pontos.natural.visitante,
          pontos.sintetica.mandante+pontos.sintetica.visitante,
          pontos.natural.mandante+pontos.natural.visitante+pontos.sintetica.mandante+pontos.sintetica.visitante))

#Gols
gols.sintetica.mandante.pro <- sum(as.numeric(
  tj.cap.sintetica[tj.cap.sintetica$Clube.1 == cap,"p1"]))
gols.sintetica.mandante.contra <- sum(as.numeric(
  tj.cap.sintetica[tj.cap.sintetica$Clube.1 == cap,"p2"]))
gols.sintetica.visitante.pro <- sum(as.numeric(
  tj.cap.sintetica[tj.cap.sintetica$Clube.2 == cap,"p2"]))
gols.sintetica.visitante.contra <- sum(as.numeric(
  tj.cap.sintetica[tj.cap.sintetica$Clube.2 == cap,"p1"]))
gols.natural.mandante.pro <- sum(as.numeric(tj.natural$p1))
gols.natural.mandante.contra <- sum(as.numeric(tj.natural$p2))
gols.natural.visitante.pro <- sum(as.numeric(tabela$p2)) - gols.sintetica.visitante.pro
gols.natural.visitante.contra <- sum(as.numeric(tabela$p1)) - gols.sintetica.visitante.contra

tabela.gols <- data.frame(
  Campo=c("Natural","Sintético","Total"),
  GolsMandantePro=c(gols.natural.mandante.pro,gols.sintetica.mandante.pro,gols.natural.mandante.pro+gols.sintetica.mandante.pro),
  GolsMandanteContra=c(gols.natural.mandante.contra,gols.sintetica.mandante.contra,gols.natural.mandante.contra+gols.sintetica.mandante.contra),
  GolsVisitantePro=c(gols.natural.visitante.pro,gols.sintetica.visitante.pro,gols.natural.visitante.pro+gols.sintetica.visitante.pro),
  GolsVisitanteContra=c(gols.natural.visitante.contra,gols.sintetica.visitante.contra,gols.natural.visitante.contra+gols.sintetica.visitante.contra),
  Total=c(gols.natural.mandante.pro+gols.natural.mandante.contra+
            gols.natural.visitante.pro+gols.natural.visitante.contra,
          gols.sintetica.mandante.pro+gols.sintetica.mandante.contra+
            gols.sintetica.visitante.pro+gols.sintetica.visitante.contra,
          gols.natural.mandante.pro+gols.natural.mandante.contra+
            gols.sintetica.mandante.pro+gols.sintetica.mandante.contra+
            gols.natural.visitante.pro+gols.natural.visitante.contra+
            gols.sintetica.visitante.pro+gols.sintetica.visitante.contra))

#Vitorias
vitorias.mandante.sintetica <- nrow(tj.cap.sintetica[tj.cap.sintetica$Vencedor==cap & tj.cap.sintetica$Clube.1 == cap,])
empates.mandante.sintetica <- nrow(tj.cap.sintetica[tj.cap.sintetica$Vencedor=="." & tj.cap.sintetica$Clube.1 == cap,])
derrotas.mandante.sintetica <- nrow(tj.cap.sintetica[tj.cap.sintetica$Vencedor !=cap & tj.cap.sintetica$Vencedor !="." & tj.cap.sintetica$Clube.1 == cap,])
vitorias.visitante.sintetica <- nrow(tj.cap.sintetica[tj.cap.sintetica$Vencedor ==cap & tj.cap.sintetica$Clube.2 == cap,])
empates.visitante.sintetica <- nrow(tj.cap.sintetica[tj.cap.sintetica$Vencedor == "." & tj.cap.sintetica$Clube.2 == cap,])
derrotas.visitante.sintetica <- nrow(tj.cap.sintetica[tj.cap.sintetica$Vencedor != cap & tj.cap.sintetica$Vencedor != "." & tj.cap.sintetica$Clube.2 == cap,])

vitorias.mandante.natural <- nrow(tj.natural[tj.natural$Clube.1==tj.natural$Vencedor,])
empates.mandante.natural <- nrow(tj.natural["."==tj.natural$Vencedor,])
derrotas.mandante.natural <- nrow(tj.natural[tj.natural$Clube.2==tj.natural$Vencedor,])
vitorias.visitante.natural <- nrow(tabela[tabela$Clube.2==tabela$Vencedor,])-vitorias.visitante.sintetica
empates.visitante.natural <- nrow(tabela["."==tabela$Vencedor,])-empates.visitante.sintetica
derrotas.visitante.natural <- nrow(tabela[tabela$Clube.1==tabela$Vencedor,])-derrotas.visitante.sintetica

tabela.vitorias <- data.frame(
  #Campo=c("Natural","Sintático","Total"),
  MandanteVitoria = c(vitorias.mandante.natural,vitorias.mandante.sintetica,vitorias.mandante.natural+vitorias.mandante.sintetica),
  MandanteEmpate=c(empates.mandante.natural,empates.mandante.sintetica,empates.mandante.natural+empates.mandante.sintetica),
  MandanteDerrota=c(derrotas.mandante.natural,derrotas.mandante.sintetica,derrotas.mandante.natural+derrotas.mandante.sintetica),
  VisitanteVitoria=c(vitorias.visitante.natural,vitorias.visitante.sintetica,vitorias.visitante.natural+vitorias.visitante.sintetica),
  VisitanteEmpate=c(empates.visitante.natural,empates.visitante.sintetica,empates.visitante.natural+empates.visitante.sintetica),
  VisitanteDerrota=c(derrotas.visitante.natural,derrotas.visitante.sintetica,derrotas.visitante.natural+derrotas.visitante.sintetica))
tabela.vitorias <- cbind(tabela.vitorias, Total=rowSums(tabela.vitorias))

#Proporção de pontos e posição
df.grafico.1 <- data.frame(Ano=c(2000:2019))
for (i in 2000:2019){
  df.grafico.1[i-1999,"Pontos"] <- 3*nrow(tabela.cap[year(tabela.cap$Data) == i & tabela.cap$Vencedor=="Athletico.pr",]) +
    nrow(tabela.cap[year(tabela.cap$Data) == i & tabela.cap$Vencedor==".",])
  df.grafico.1[i-1999,"PontosCasa"] <- 3*nrow(tabela.cap[year(tabela.cap$Data) == i & tabela.cap$Vencedor=="Athletico.pr" & tabela.cap$Clube.1 == "Athletico.pr",]) +
    nrow(tabela.cap[year(tabela.cap$Data) == i & tabela.cap$Vencedor=="." & tabela.cap$Clube.1 == "Athletico.pr",])
}
df.grafico.1["Proporcao"] <- df.grafico.1$PontosCasa / df.grafico.1$Pontos
df.grafico.1["Posicao"] <- c(8,1,14,12,2,6,13,12,13,14,5,17,23,3,8,10,6,11,7,5)

ggplot(df.grafico.1[which(df.grafico.1$Ano == c(2000:2005,2016:2019)),], aes(x=Posicao, y=Proporcao)) + geom_point(aes(colour = cut(Ano, c(-Inf,2015,Inf))), size=3) +
  scale_color_manual(name="Campo",values=c("green3","red"),labels=c("Natual","Sintética")) +
  ggtitle("Proporção de pontos versus posição no campeonato") + xlim(c(1,20))

cor(df.grafico.1$Proporcao[-13],df.grafico.1$Posicao[-13])

#Tabelas proporcionais
tabela.gols.p <- NULL
tabela.gols.p <- cbind(tabela.gols.p, (tabela.gols[,2]/tabela.gols[,6])*2)
tabela.gols.p <- cbind(tabela.gols.p, (tabela.gols[,3]/tabela.gols[,6])*2)
tabela.gols.p <- cbind(tabela.gols.p, (tabela.gols[,4]/tabela.gols[,6])*2)
tabela.gols.p <- cbind(tabela.gols.p, (tabela.gols[,5]/tabela.gols[,6])*2)
tabela.gols.p <- tabela.gols.p[-3,]
tabela.gols.p[2,] <- tabela.gols.p[2,]*(-1)
tabela.gols.p <- rbind(tabela.gols.p, abs(colSums(tabela.gols.p)))
tabela.gols.p[2,] <- tabela.gols.p[2,]*(-1)
rownames(tabela.gols.p) <- c("Natural","Sintetica","Diferença")
colnames(tabela.gols.p) <- c("Mandante Pro", "Mandante Contra", "Visitante Pro", "Visitante Contra")

tmn<-sum(tabela.vitorias[1,c(1,2,3)])
tvn<-sum(tabela.vitorias[1,c(4,5,6)])
tms<-sum(tabela.vitorias[2,c(1,2,3)])
tvs<-sum(tabela.vitorias[2,c(4,5,6)])
tabela.vitorias.p <- c(tabela.vitorias[1,c(1,2,3)]/tmn,tabela.vitorias[1,c(4,5,6)]/tvn)
tabela.vitorias.p <- as.data.frame(tabela.vitorias.p)
tabela.vitorias.p <- rbind(tabela.vitorias.p,
                           c(tabela.vitorias[2,c(1,2,3)]/tms,tabela.vitorias[2,c(4,5,6)]/tvs))
tabela.vitorias.p[2,] <- tabela.vitorias.p[2,]*(-1)
tabela.vitorias.p <- rbind(tabela.vitorias.p, abs(colSums(tabela.vitorias.p)))
tabela.vitorias.p[2,] <- tabela.vitorias.p[2,]*(-1)

#Performance relativa
pontos.natural.mandante.r <- (nrow(tj.natural[tj.natural$Vencedor==tj.natural$Clube.1,])*3+
                                nrow(tj.natural[tj.natural$Vencedor==".",]))/nrow(tj.natural)
pontos.sintetica.mandante.r <- (nrow(tj.cap.sintetica[tj.cap.sintetica$Clube.1==cap 
                                                      & tj.cap.sintetica$Vencedor==cap,])*3+
                                  nrow(tj.cap.sintetica[tj.cap.sintetica$Clube.1==cap 
                                                        & tj.cap.sintetica$Vencedor==".",]))/nrow(tj.cap.sintetica[tj.cap.sintetica$Clube.1==cap,])
pontos.natural.visitante.r <-(nrow(tj.natural[tj.natural$Vencedor==tj.natural$Clube.2,])*3+
                                nrow(tj.natural[tj.natural$Vencedor==".",]))/nrow(tj.natural)
pontos.sintetica.visitante.r <- (nrow(tj.cap.sintetica[tj.cap.sintetica$Clube.2==cap 
                                                       & tj.cap.sintetica$Vencedor==cap,])*3+
                                   nrow(tj.cap.sintetica[tj.cap.sintetica$Clube.2==cap 
                                                         & tj.cap.sintetica$Vencedor==".",]))/nrow(tj.cap.sintetica[tj.cap.sintetica$Clube.1==cap,])


tabela.pontos.r <- data.frame(PontosCasa=c(pontos.natural.mandante.r,pontos.sintetica.mandante.r),
                              PontosFora=c(pontos.natural.visitante.r,pontos.sintetica.visitante.r))

#(gfh/gch)/(gfa/gca)
tabela.2016 <- tabela[year(tabela$Data) > 2015,]
times.2016 <- unique(tabela.2016$Clube.1)
times.2016 <- times.2016[order(times.2016)]
gols.r <- NULL
for(i in 1:length(times.2016)){
  gfh <- sum(as.numeric(
    tabela.2016[tabela.2016$Clube.1==times.2016[i],"p1"]
  ))
  gch <- sum(as.numeric(
    tabela.2016[tabela.2016$Clube.1==times.2016[i],"p2"]
  ))
  gfa <- sum(as.numeric(
    tabela.2016[tabela.2016$Clube.2==times.2016[i],"p2"]
  ))
  gca <- sum(as.numeric(
    tabela.2016[tabela.2016$Clube.2==times.2016[i],"p1"]
  ))
  if (gch>0 & gfa>0 & gca>0){
    g<-(gfh/gch)/(gfa/gca)
    gols.r <- c(gols.r,g)
  }
  else {
    gols.r <- c(gols.r,0)
  }
}
gols.relativos<-data.frame(Times=times.2016,GolsRelativos=gols.r)
t<-as.data.frame(table(tabela.2016$Clube.1))
gols.relativos["Cor"] <- rep("B",nrow(gols.relativos))
gols.relativos[which(t$Freq>40),"Cor"] <- "A"
gols.relativos[gols.relativos$Times == cap,"Cor"] <- "C"

ggplot(gols.relativos, aes(x=Times,y=GolsRelativos, color=Cor))+
  geom_point(size=2) + 
  scale_color_manual(name="Tipo", values=c("blue", "green3", "red")) + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  ggtitle("Gols relativos de cada time")

#Médias pré e pós
tabela.sem.cap.2006 <- tabela.sem.cap[year(tabela.sem.cap$Data)>2005,]
media.pontos.natural <- (nrow(tabela.sem.cap.2006[tabela.sem.cap.2006$Vencedor == ".",]) +
  3*nrow(tabela.sem.cap.2006[tabela.sem.cap.2006$Vencedor == tabela.sem.cap.2006$Clube.1,])) /
  nrow(tabela.sem.cap.2006)
media.gols.natual <- sum(as.numeric(tabela.sem.cap.2006[,"p1"])) /
  nrow(tabela.sem.cap.2006)
media.golscontra.natural <- sum(as.numeric(tabela.sem.cap.2006[,"p2"])) /
  nrow(tabela.sem.cap.2006)
media.vitorias.natural <- nrow(tabela.sem.cap.2006[tabela.sem.cap.2006$Vencedor == tabela.sem.cap.2006$Clube.1,])/(16*20)

tj.cap.natural.2006 <- tj.cap.natural[year(tj.cap.natural$Data)>2005,]
media.pontos.antes <- (nrow(tj.cap.natural.2006[
  tj.cap.natural.2006$Clube.1 == cap & tj.cap.natural.2006$Vencedor == ".",]) +
  3*nrow(tj.cap.natural.2006[tj.cap.natural.2006$Clube.1 == cap & tj.cap.natural.2006$Vencedor == cap,])) /
  nrow(tj.cap.natural.2006[tj.cap.natural.2006$Clube.1 == cap,])
media.gols.antes <- sum(as.numeric(tj.cap.natural.2006[tj.cap.natural.2006$Clube.1 == cap,"p1"])) /
  nrow(tj.cap.natural.2006[tj.cap.natural.2006$Clube.1 == cap,])
media.golscontra.antes <- sum(as.numeric(tj.cap.natural.2006[tj.cap.natural.2006$Clube.1 == cap,"p2"])) /
  nrow(tj.cap.natural.2006[tj.cap.natural.2006$Clube.1 == cap,])
media.vitorias.antes <- nrow(tj.cap.natural.2006[tj.cap.natural.2006$Clube.1 == cap & 
  tj.cap.natural.2006$Vencedor == cap,])/10

media.pontos.depois <- (nrow(tj.cap.sintetica[
  tj.cap.sintetica$Clube.1 == cap & tj.cap.sintetica$Vencedor == ".",]) +
    3*nrow(tj.cap.sintetica[tj.cap.sintetica$Clube.1 == cap &
                              tj.cap.sintetica$Vencedor == cap,])) /
  nrow(tj.cap.sintetica[tj.cap.sintetica$Clube.1 == cap,])
media.gols.depois <- sum(as.numeric(tj.cap.sintetica[tj.cap.sintetica$Clube.1 == cap,"p1"])) /
  nrow(tj.cap.sintetica[tj.cap.sintetica$Clube.1 == cap,])
media.golscontra.depois <- sum(as.numeric(tj.cap.sintetica[tj.cap.sintetica$Clube.1 == cap,"p2"])) /
  nrow(tj.cap.sintetica[tj.cap.sintetica$Clube.1 == cap,])
media.vitorias.depois <- nrow(tj.cap.sintetica[tj.cap.sintetica$Clube.1 == cap & 
                                                 tj.cap.sintetica$Vencedor == cap,])/4

data.frame(Pontos=c(media.pontos.natural,media.pontos.antes,media.pontos.depois),
           GolsPro=c(media.gols.natual,media.gols.antes,media.gols.depois),
           GolsContra=c(media.golscontra.natural,media.golscontra.antes,media.golscontra.depois),
           Vitorias=c(media.vitorias.natural,media.vitorias.antes,media.vitorias.depois))

