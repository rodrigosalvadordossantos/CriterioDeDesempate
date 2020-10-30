library(janitor)
library(openxlsx)
library(Hmisc)
library(plyr)
library(reshape2)
library(stringr)
library(stringi)

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
times = sort(unique(capitalize(tolower(tabela$Clube.1))))

vetores = function (tabela){
	df = list()
	for (i in 1:length(times)){
		df[[i]] = rbind(tabela[tabela$Clube.1==times[i],],tabela[tabela$Clube.2==times[i],])
		df[[i]] = df[[i]][order(df[[i]]$Data),]
	}
	return(df)
}

todos.jogos.por.time = vetores(tabela)
names(todos.jogos.por.time) = times

#Métodos
desempenho.por.ano = function(jogos, ano, time){
	desempenho.ano = 0
	desempenho.ano[1] = 0
	for (i in 1: nrow(jogos)){
		if (unclass(as.POSIXlt(jogos$Data[i]))$year == ano){
			if (jogos$Vencedor[i] == time){
				desempenho.ano = c(desempenho.ano, desempenho.ano[length(desempenho.ano)] + 1)
			} else if (jogos$Vencedor[i] == "."){
				desempenho.ano = c(desempenho.ano, desempenho.ano[length(desempenho.ano)])
			} else {
				desempenho.ano = c(desempenho.ano, desempenho.ano[length(desempenho.ano)] - 1)
			}
		}
	}
	return(desempenho.ano)
}

desempenho.Total = function(jogos, time){
	desempenho = list(1)
	for (j in 100:119){
		des = desempenho.por.ano(jogos, j, time)
		nome.coluna = paste("a",as.character(1900+j),sep="")
		desempenho[[nome.coluna]] = des
	}
	return(desempenho)
}

completar.na = function(df){
	for (i in 1:ncol(df)){
		count = nrow(df)-length(df[,i])
		if (count>0){
			df[,i] = c(df[,i],rep(NA,count))
		}
	}
	return(df)
}

temporadas = function(variable,value){
	anos = 2000:2019
	return(anos[value])
}

#Gráfico Coxa (teste)
jogos.Coxa = todos.jogos.por.time[[14]]
coxa.total = desempenho.Total(jogos.Coxa, "Coritiba")
max.rodadas=max(unlist(lapply(coxa.total,length)))
coxa.total[[1]]=1:max.rodadas
names(coxa.total)[1]="Rodada"
attributes(coxa.total) = list(names = names(coxa.total), row.names=1:max.rodadas, class='data.frame')
coxa.total = completar.na(coxa.total)

#coxa.total.df = lapply(coxa.total,as.data.frame)
#cfc.2000 = desempenho.por.ano(jogos.Coxa,100,"Coritiba")
#cfc.2001 = desempenho.por.ano(jogos.Coxa,101,"Coritiba")
#cfc.2002 = desempenho.por.ano(jogos.Coxa,102,"Coritiba")
#cfc.2017 = desempenho.por.ano(jogos.Coxa,117,"Coritiba")
#cfc.total = data.frame(c(1:length(cfc.2000)),cfc.2000)
#names(cfc.total) = c("Rodada", "a.2000")

#ggplot(cfc.total, aes(x=Rodada, y=a.2000)) + geom_point(aes(color=a.2000>0)) + geom_line(aes(color=Var>0)) + ylim(-10,10) + xlim(0,nrow(cfc.total))

#ggplot(coxa.total, aes(x=Rodada, y=a2000, fill=a2000>0)) + geom_col(aes(color=a2000>0)) + ylim(-10,10) + xlim(0,nrow(coxa.total))

#coxa.total.melt = melt(coxa.total,id.vars="Rodada")

#ggplot(coxa.total.melt, aes(x=Rodada, y=value, fill=value>0)) + geom_col(aes(color=value>0)) + ylim(-10,10) + xlim(0,max.rodadas) + facet_wrap(~variable)

#Gremio (teste)
jogos.gfpa = todos.jogos.por.time[[24]]
gfpa.total = desempenho.Total(jogos.gfpa, "Gremio")
max.rodadas=max(unlist(lapply(gfpa.total,length)))
gfpa.total[[1]]=1:max.rodadas
names(gfpa.total)[1]="Rodada"
attributes(gfpa.total) = list(names = names(gfpa.total), row.names=1:max.rodadas, class='data.frame')
gfpa.total = completar.na(gfpa.total)

#ggplot(sep.total, aes(x=Rodada, y=a2000, fill=a2000>0)) + geom_col(aes(color=a2000>0)) + ylim(-10,10) + xlim(0,nrow(coxa.total))

gfpa.total.melt = melt(gfpa.total,id.vars="Rodada")

ggplot(gfpa.total.melt, aes(x=Rodada, y=value, fill=value>0, width=1)) + geom_col(aes(fill=value>0)) + ylim(-16,20) + xlim(0,max.rodadas) + facet_wrap(~variable, labeller=temporadas) + scale_fill_manual(values=c("red", "forestgreen")) + labs(title = "Grêmio", colour=NULL, y="Saldo de vitórias")+ theme(legend.position = "none")

#Funcao geral
gerar.mosaico = function(id, time){
	jogos.time = todos.jogos.por.time[[id]]
	time.total = desempenho.Total(jogos.time, time)
	max.rodadas=max(unlist(lapply(time.total,length)))
	time.total[[1]]=1:max.rodadas
	names(time.total)[1]="Rodada"
	attributes(time.total) = list(names = names(time.total), row.names=1:max.rodadas, class='data.frame')
	time.total = completar.na(time.total)
	
	time.total.melt = melt(time.total,id.vars="Rodada")
	
	mosaico = ggplot(time.total.melt, aes(x=Rodada, y=value, fill=value>0, width=1)) + geom_col(aes(fill=value>0)) + ylim(-16,20) + xlim(0,max.rodadas) + facet_wrap(~variable,labeller=temporadas)+ scale_fill_manual(values=c("red", "green3")) + labs(title = time, colour=NULL, y="Saldo de vitórias")+ theme(legend.position = "none")
	return(mosaico)
}

#gerar.mosaico(14,"Coritiba")
#gerar.mosaico(3,"Athletico.pr")
#gerar.mosaico(5,"Atletico.mg")
#gerar.mosaico(7,"Bahia")
#gerar.mosaico(8,"Botafogo.rj")
#gerar.mosaico(11,"Ceara")
#gerar.mosaico(13,"Corinthians")
#gerar.mosaico(16,"Cruzeiro")
#gerar.mosaico(19,"Flamengo")
#gerar.mosaico(20,"Fluminense")
#gerar.mosaico(21,"Fortaleza")
#gerar.mosaico(23,"Goias")
#gerar.mosaico(24,"Gremio")
#gerar.mosaico(27,"Internacional")
#gerar.mosaico(33,"Palmeiras")
#gerar.mosaico(41,"Santos")
#gerar.mosaico(43,"Sao.paulo")
#gerar.mosaico(44,"Sport")
#gerar.mosaico(45,"Vasco")
#gerar.mosaico(4,"Atletico.go")
gerar.mosaico(6,"Avai")

gerar.serie = function(id, time){
  jogos.time = todos.jogos.por.time[[id]]
  time.total = desempenho.Total(jogos.time, time)
  max.rodadas=max(unlist(lapply(time.total,length)))
  time.total[[1]]=1:max.rodadas
  names(time.total)[1]="Rodada"
  attributes(time.total) = list(names = names(time.total), row.names=1:max.rodadas, class='data.frame')
  time.total = completar.na(time.total)
  return (time.total)
  
#  time.total.melt = melt(time.total,id.vars="Rodada")

#  mosaico = ggplot(time.total.melt, aes(x=Rodada, y=value, fill=value>0, width=1)) + geom_col(aes(fill=value>0)) + ylim(-16,20) + xlim(0,max.rodadas) + facet_wrap(~variable,labeller=temporadas)+ scale_fill_manual(values=c("red", "green3")) + labs(title = time, colour=NULL, y="Saldo de vitórias")+ theme(legend.position = "none")
#  return(mosaico)
}

#gerar.serie(14,"Coritiba")
#gerar.serie(3,"Athletico.pr")
#gerar.serie(5,"Atletico.mg")
#gerar.serie(7,"Bahia")
#gerar.serie(8,"Botafogo.rj")
#gerar.serie(11,"Ceara")
#gerar.serie(13,"Corinthians")
#gerar.serie(16,"Cruzeiro")
#gerar.serie(19,"Flamengo")
#gerar.serie(20,"Fluminense")
#gerar.serie(21,"Fortaleza")
#gerar.serie(23,"Goias")
#gerar.serie(24,"Gremio")
#gerar.serie(27,"Internacional")
#gerar.serie(33,"Palmeiras")
#gerar.serie(41,"Santos")
#gerar.serie(43,"Sao.paulo")
#gerar.serie(44,"Sport")
#gerar.serie(45,"Vasco")
#gerar.serie(4,"Atletico.go")

arquivo = "C:/Users/Rodrigo/DataScience/Sports/Dashboards/Saldo_de_vitorias.xlsx"
wb <- createWorkbook()

id=43
time="Sao.paulo"
addWorksheet(wb,time)
writeData(wb, time, gerar.serie(id,time), startRow = 1, startCol = 1)

saveWorkbook(wb, file = arquivo, overwrite = FALSE)
#write.xlsx(gerar.serie(id,time), arquivo, sheetName = time)