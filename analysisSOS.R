#Bibliotecas
library(openxlsx)
library(ggplot2)

setwd("C:\\Users\\Rodrigo\\DataScience\\Sports\\StrenghOfSchedule")

#Leitura de arquivos
files = list.files(getwd(), pattern=".xlsx", full.names=T)
tabela2019 = read.xlsx(files[1],"2019")
jogos = read.xlsx(files[1],"Jogos")
resultados = read.xlsx(files[1],"Resultados")
tabela2020 = matrix(0L, nrow=20,ncol=ncol(tabela2019))

View(jogos)

#Ajustes nos arquivos
jogos = jogos[1:20,]
times = jogos$X1
row.names(jogos)=times
jogos$X1 = NULL
names(jogos)=times
cabecalho = c("Posicao","Team","Stand","P.Total","W.Total","D.Total","L.Total","GF.Total","GA.Total","Pts.Total","P.Home","W.Home","D.Home","L.Home","GF.Home","GA.Home","Pts.Home","P.Away","W.Away","D.Away","L.Away","GF.Away","GA.Away","Pts.Away","PPG.Total","PPG.Home","PPG.Away")
names(tabela2019) = cabecalho
tabela2019 = tabela2019[-1,]
row.names(tabela2019) = c(1:24)
resultados = resultados[-21,]
row.names(resultados)=times
resultados$X1=NULL
tabela2020 = as.data.frame(tabela2020)
names(tabela2020) = names(tabela2019)
row.names(tabela2020) = c(1:20)
tabela2020$Team = times

#Metodo de calculo desconsiderando local
forcaDaTabela = function (rodada){
	forca = matrix(ncol=2, nrow=20)
	for(i in 1:20){
		forca[i,1] = times[i]
		soma = 0
		for(j in rodada:(rodada+5)){
			if (length(which(jogos[i,]==j)) > 0){
				nome = names(jogos[which(jogos[i,]==j)])
				soma = soma + tabela2019$PPG.Total[which(tabela2019$Team==nome)]
			} else {
				nome = names(jogos[which(jogos[,i]==j)])
				soma = soma + tabela2019$PPG.Total[which(tabela2019$Team==nome)]
			}
		}
		forca[i,2] = soma/6 
	}
	forca = data.frame(forca)
	forcaOrdenada = forca[order(forca$X2),]
}

#Metodo de calculo considerando local
sos.Home.Away = function (rodada){
	if (rodada > 33){
		return(NULL)
	}
	forca = matrix(ncol=2, nrow=20)
	for(i in 1:20){
		forca[i,1] = times[i]
		soma = 0
		if (rodada <= 6){
			for(j in rodada:(rodada+5)){
				if (length(which(jogos[i,]==j)) > 0){
					nome = names(jogos[which(jogos[i,]==j)])
					soma = soma + tabela2019$PPG.Home[which(tabela2019$Team==nome)]
				} else {
					nome = names(jogos[which(jogos[,i]==j)])
					soma = soma + tabela2019$PPG.Away[which(tabela2019$Team==nome)]
				}
			}
		} else {
			for(j in rodada:(rodada+5)){
				if (length(which(jogos[i,]==j)) > 0){
					nome = names(jogos[which(jogos[i,]==j)])
					soma = soma + tabela2020$PPG.Home[which(tabela2020$Team==nome)]
				} else {
					nome = names(jogos[which(jogos[,i]==j)])
					soma = soma + tabela2020$PPG.Away[which(tabela2020$Team==nome)]
				}
			}
		}
		forca[i,2] = soma/6
	}
	forca = data.frame(forca)
	forcaOrdenada = forca[order(forca$X2),]
	forcaOrdenada$X1 = as.character(forcaOrdenada$X1)
	forcaOrdenada$X2 = round(as.numeric(strsplit(gsub("(^\\[|\\]$)", "", forcaOrdenada$X2), ",")),3)
	names(forcaOrdenada) = c("Team","SOS")
	sos.Home.Away = forcaOrdenada
}

#Matriz de cores para o gráfico
cores = times
cores = cbind(cores, rep("pink",20), rep("orange",20))
cores = as.data.frame(cores)
names(cores)=c("Time","Cor1","Cor2")
cores$Cor1 = as.character(cores$Cor1)
cores$Cor2 = as.character(cores$Cor2)
cores[1,2]="red"
cores[1,3]="black"
cores[2,2]="white"
cores[2,3]="black"
cores[3,2]="green4"
cores[3,3]="white"
cores[4,2]="blue"
cores[4,3]="black"
cores[5,2]="red1"
cores[5,3]="black"
cores[6,2]="darkred"
cores[6,3]="white"
cores[7,2]="red2"
cores[7,3]="white"
cores[8,2]="gray90"
cores[8,3]="black"
cores[9,2]="blue1"
cores[9,3]="red"
cores[10,2]="darkgreen"
cores[10,3]="white"
cores[11,2]="blue2"
cores[11,3]="red"
cores[12,2]="gray10"
cores[12,3]="white"
cores[13,2]="gray80"
cores[13,3]="black"
cores[14,2]="firebrick"
cores[14,3]="white"
cores[15,2]="gray20"
cores[15,3]="white"
cores[16,2]="gray30"
cores[16,3]="white"
cores[17,2]="gray70"
cores[17,3]="black"
cores[18,2]="red3"
cores[18,3]="black"
cores[19,2]="forestgreen"
cores[19,3]="white"
cores[20,2]="red4"
cores[20,3]="black"

#Ordenar a matriz de cores conforme a ordem da forca da tabela
colours.order = function(round.sos, colours){
	colours.order = colours[as.numeric(row.names(round.sos)),]
}

#Chamada dos métodos e do gráfico
graph.sos = function(round){
	sos.all = sos.Home.Away(round)
	colours = colours.order(sos.all,cores)
	x = factor(sos.all$Team, levels = sos.all$Team[order(sos.all$SOS)])
	graph = ggplot(sos.all, aes(x, SOS)) + theme(axis.text.x = element_text(angle=90))
	graph = graph + geom_col(fill=colours$Cor1, colour=colours$Cor2)+coord_cartesian(ylim=c(1,2))
	graph = graph + labs(x="", y="Força da Tabela")
	graph = graph + theme(text = element_text(size=20))
	graph
}

graph.sos(6)
a2 <- sos.Home.Away(2)

#Atualiza apenas o placar
update.score = function(home.team, away.team, score, all.scores){
	all.scores[home.team,away.team]=score
	resultados <<- all.scores
}

#Atualiza a classificação das ult 6 rodadas
update.standings <-function(rodada, standings){
  for (i in (rodada-6):(rodada-1)){
    for (j in 1:20){
      print(paste(i,j))
      placar <- resultados[j, which(jogos[j,]==i)]
      if (length(placar)>0){
        score.Home = as.numeric(strsplit(placar,"-")[[1]][1])
        score.Away = as.numeric(strsplit(placar,"-")[[1]][2])
        home.team <- j
        away.team <- which(jogos[j,]==i)
        if (score.Home > score.Away){
          standings[home.team,]$P.Total = standings[home.team,]$P.Total + 1
          standings[home.team,]$W.Total = standings[home.team,]$W.Total + 1
          standings[home.team,]$GF.Total = standings[home.team,]$GF.Total + score.Home
          standings[home.team,]$GA.Total = standings[home.team,]$GA.Total + score.Away
          standings[home.team,]$Pts.Total = standings[home.team,]$Pts.Total + 3
          standings[home.team,]$P.Home = standings[home.team,]$P.Home + 1
          standings[home.team,]$W.Home = standings[home.team,]$W.Home + 1
          standings[home.team,]$GF.Home = standings[home.team,]$GF.Home + score.Home
          standings[home.team,]$GA.Home = standings[home.team,]$GA.Home + score.Away
          standings[home.team,]$Pts.Home = standings[home.team,]$Pts.Home + 3
          
          standings[away.team,]$P.Total = standings[away.team,]$P.Total + 1
          standings[away.team,]$L.Total = standings[away.team,]$L.Total + 1
          standings[away.team,]$GF.Total = standings[away.team,]$GF.Total + score.Away
          standings[away.team,]$GA.Total = standings[away.team,]$GA.Total + score.Home
          standings[away.team,]$P.Away = standings[away.team,]$P.Away + 1
          standings[away.team,]$L.Away = standings[away.team,]$L.Away + 1
          standings[away.team,]$GF.Away = standings[away.team,]$GF.Away + score.Away
          standings[away.team,]$GA.Away = standings[away.team,]$GA.Away + score.Home
        } else if (score.Home < score.Away) {
          standings[home.team,]$P.Total = standings[home.team,]$P.Total + 1
          standings[home.team,]$L.Total = standings[home.team,]$L.Total + 1
          standings[home.team,]$GF.Total = standings[home.team,]$GF.Total + score.Home
          standings[home.team,]$GA.Total = standings[home.team,]$GA.Total + score.Away
          standings[home.team,]$P.Home = standings[home.team,]$P.Home + 1
          standings[home.team,]$L.Home = standings[home.team,]$L.Home + 1
          standings[home.team,]$GF.Home = standings[home.team,]$GF.Home + score.Home
          standings[home.team,]$GA.Home = standings[home.team,]$GA.Home + score.Away
          
          standings[away.team,]$P.Total = standings[away.team,]$P.Total + 1
          standings[away.team,]$W.Total = standings[away.team,]$W.Total + 1
          standings[away.team,]$GF.Total = standings[away.team,]$GF.Total + score.Away
          standings[away.team,]$GA.Total = standings[away.team,]$GA.Total + score.Home
          standings[away.team,]$Pts.Total = standings[away.team,]$Pts.Total + 3
          standings[away.team,]$P.Away = standings[away.team,]$P.Away + 1
          standings[away.team,]$W.Away = standings[away.team,]$W.Away + 1
          standings[away.team,]$GF.Away = standings[away.team,]$GF.Away + score.Away
          standings[away.team,]$GA.Away = standings[away.team,]$GA.Away + score.Home
          standings[away.team,]$Pts.Away = standings[away.team,]$Pts.Away + 3
        } else {
          standings[home.team,]$P.Total = standings[home.team,]$P.Total + 1
          standings[home.team,]$D.Total = standings[home.team,]$D.Total + 1
          standings[home.team,]$GF.Total = standings[home.team,]$GF.Total + score.Home
          standings[home.team,]$GA.Total = standings[home.team,]$GA.Total + score.Away
          standings[home.team,]$Pts.Total = standings[home.team,]$Pts.Total + 1
          standings[home.team,]$P.Home = standings[home.team,]$P.Home + 1
          standings[home.team,]$D.Home = standings[home.team,]$D.Home + 1
          standings[home.team,]$GF.Home = standings[home.team,]$GF.Home + score.Home
          standings[home.team,]$GA.Home = standings[home.team,]$GA.Home + score.Away
          standings[home.team,]$Pts.Home = standings[home.team,]$Pts.Home + 1
          
          standings[away.team,]$P.Total = standings[away.team,]$P.Total + 1
          standings[away.team,]$D.Total = standings[away.team,]$D.Total + 1
          standings[away.team,]$GF.Total = standings[away.team,]$GF.Total + score.Away
          standings[away.team,]$GA.Total = standings[away.team,]$GA.Total + score.Home
          standings[away.team,]$Pts.Total = standings[away.team,]$Pts.Total + 1
          standings[away.team,]$P.Away = standings[away.team,]$P.Away + 1
          standings[away.team,]$D.Away = standings[away.team,]$D.Away + 1
          standings[away.team,]$GF.Away = standings[away.team,]$GF.Away + score.Away
          standings[away.team,]$GA.Away = standings[away.team,]$GA.Away + score.Home
          standings[away.team,]$Pts.Away = standings[away.team,]$Pts.Away + 1
        }
        standings[home.team,]$PPG.Total = round((standings[home.team,]$Pts.Total / standings[home.team,]$P.Total),3)
        standings[home.team,]$PPG.Home = round((standings[home.team,]$Pts.Home / standings[home.team,]$P.Home),3)
        standings[away.team,]$PPG.Total = round((standings[away.team,]$Pts.Total / standings[away.team,]$P.Total),3)
        standings[away.team,]$PPG.Away = round((standings[away.team,]$Pts.Away / standings[away.team,]$P.Away) ,3)
      }
    }
  }
  return (standings)
}

tabela2020 <- update.standings(7,tabela2020)
tabela2020 <- tabela2020[order(tabela2020$Pts.Total, decreasing = T),]
View(tabela2020)

#salvar.arquivo
salvar <- function(){
  
}

#Apaga a tabela
reset.tabela = function(){
	tabela2020 = matrix(0L, nrow=20,ncol=ncol(tabela2019))
	tabela2020 = as.data.frame(tabela2020)
	names(tabela2020) = names(tabela2019)
	row.names(tabela2020) = c(1:20)
	tabela2020$Team = times
	tabela2020 <<- tabela2020
}

graph.sos(6)

#incluir o maior e o menor valores possiveis OK (padrão entre 1 e 2)
#normalizar o intervalo OK (desnecessário pelo intervalo)
#plotar o grafico de barras OK
#adequar o codigo pra atualizar os resultados OK
#calcular a força retroativa
#comparar estimativa e realizado

View(times)
#Rodada1
update.score(9,5,"0-2",resultados)
update.score(19,7,"0-1",resultados)
update.score(18,16,"3-2",resultados)
update.score(2,17,"1-1",resultados)
update.score(1,13,"0-1",resultados)
update.score(4,14,"1-0",resultados)
update.score(10,6,"9-9",resultados)
update.score(3,12,"9-9",resultados)
update.score(8,20,"9-9",resultados)
update.score(15,11,"9-9",resultados)
#Rodada2
update.score(17,15,"1-1",resultados)
update.score(5,10,"2-1",resultados)
update.score(13,8,"3-2",resultados)
update.score(11,19,"1-0",resultados)
update.score(20,1,"3-0",resultados)
update.score(16,4,"1-1",resultados)
update.score(14,3,"1-1",resultados)
update.score(6,9,"1-0",resultados)
update.score(7,2,"2-0",resultados)
update.score(12,18,"2-0",resultados)
#Rodada3
update.score(4,8,"0-0",resultados)
update.score(19,1,"0-1",resultados)
update.score(3,10,"1-1",resultados)
update.score(13,16,"2-0",resultados)
update.score(11,17,"2-1",resultados)
update.score(12,6,"2-1",resultados)
update.score(14,7,"2-1",resultados)
update.score(20,18,"1-1",resultados)
update.score(9,15,"0-0",resultados)
update.score(2,5,"3-1",resultados)
#Rodada4
update.score(1,4,"1-1",resultados)
update.score(17,14,"2-1",resultados)
update.score(5,3,"0-1",resultados)
update.score(10,9,"1-3",resultados)
update.score(7,20,"3-0",resultados)
update.score(8,19,"3-1",resultados)
update.score(15,13,"2-1",resultados)
update.score(18,2,"0-1",resultados)
update.score(16,12,"0-3",resultados)
update.score(6,11,"1-1",resultados)
#Rodada5
update.score(5,14,"0-1",resultados)
update.score(7,13,"1-0",resultados)
update.score(10,20,"2-0",resultados)
update.score(1,15,"1-1",resultados)
update.score(12,4,"0-0",resultados)
update.score(3,2,"2-1",resultados)
update.score(17,19,"1-2",resultados)
update.score(18,6,"9-9",resultados)
update.score(16,11,"9-9",resultados)
update.score(8,9,"9-9",resultados)
#Rodada6
update.score(13,5,"9-9",resultados)
update.score(15,7,"9-9",resultados)
update.score(14,12,"9-9",resultados)
update.score(11,3,"9-9",resultados)
update.score(9,17,"9-9",resultados)
update.score(6,8,"9-9",resultados)
update.score(19,18,"9-9",resultados)
update.score(2,1,"9-9",resultados)
update.score(20,16,"9-9",resultados)
update.score(4,10,"9-9",resultados)

View(resultados)