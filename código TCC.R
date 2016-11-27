require(softImpute)
require(data.table)
require(pryr)
require(dplyr)
require(lattice)
require(xtable)
############################# importando dados de treinamento ######################################
## especifica diretório dos dados do netflix
setwd("C:\\Users\\Juliano\\Desktop\\TCC-2.2016\\Netflix dados tcc\\download\\training_set")

#lista os arquivos com mv
file_list<-list.files(pattern = "mv*")
#le todos os arquivos através de um loop
data_list = lapply(file_list, read.table, sep = ",",skip=1)
####### salvando
save(data_list,file="data_list.Rdata")
load("data_list.Rdata")

dim(data_list)
names(data_list)
summary(data_list)
str(data_list)

### reescrever banco de dados no formato de coordenadas,
### para transformar em uma matriz esparsa
for(a in 1:length(file_list)){
  xx<-data.frame(rep(a,nrow(data_list[[a]])),data_list[[a]][,-3])
  write.table(xx, file="mmm.txt", append=T,col.names = F,row.names = F)
  }


####### agora ler o banco de dados no formato adequado 
setwd("C:\\Users\\Juliano\\Desktop\\TCC-2.2016\\market matrix")

#lista os arquivos com nome mmm.txt
file_list<-list.files(pattern = "mmm.txt*")
### importa o banco de dados em um data.table 
######## li o arquivo no formato de coordenada
importa<-fread("mmm.txt")
####### salvando
save(importa,file="importa.Rdata")
load("importa.Rdata")
##verificando o numero de usuários,
##uma vez que o numero de usuários é menor que o numero de ids.
length(unique(importa$V2))

##ordenando dados com relação aos usuários.
xo<-importa[order(importa$V2),]

##criar um vetor para substituir o vetor de usuários.
xox<-data.frame(table(xo$V2))
usuarios<-rep(1:length(xox$Var1),xox$Freq)
####### salvando
save(usuarios,file="usuarios.Rdata")
load("usuarios.Rdata")

## usando pacote para transformar os dados em uma matriz esparsa.
matespar<-Incomplete(i=usuarios,j=xo$V1,x=xo$V3)
####### salvando
save(matespar,file="matespar.Rdata")
load("matespar.Rdata")



########################################### importando dados de sondagem ################################################
## especifica diretório dos dados do netflix
setwd("C:\\Users\\Juliano\\Desktop\\TCC-2.2016\\Netflix dados tcc\\download\\")
list.files()
avaliados<-readLines("probe.txt")
# Leia o arquivo como um vetor de caracteres que eu vou chamar de avaliados
length(avaliados)
ind.filmes = grep(":", avaliados)
length(ind.filmes)
# Remova o ":"

avaliados[ind.filmes] = sub(":","",avaliados[ind.filmes])

# Converte em numérico

avaliados = as.numeric(avaliados)

# Cria uma matriz com duas colunas com os índices da matriz onde os filmes foram avaliados.
# Para isso adicione um último elemento ao ind.filmes que é o comprimento de avaliados mais 1.
num.filmes = length(ind.filmes)
ind.filmes.m = c(ind.filmes, length(avaliados)+1)

gerar.matriz = function(num.filme) {
  cbind(rep(avaliados[ind.filmes[num.filme]] , ind.filmes.m[num.filme+1] - ind.filmes[num.filme] - 1 ), 
        avaliados[(ind.filmes[num.filme] + 1):(ind.filmes.m[num.filme+1]-1)] )
}

avaliados.mat = sapply( 1:length(ind.filmes), gerar.matriz)


########################## criando banco de dados de sondagem

setwd("C:\\Users\\Juliano\\Desktop\\TCC-2.2016\\market matrix")

load("importa.Rdata")

##ordenando dados com relação aos usuários.
xo<-importa[order(importa$V2),]
####### salvando
save(xo,file="xo.Rdata")
load("xo.Rdata")

user<-xo$V2
####### salvando
save(user,file="user.Rdata")
load("user.Rdata")

#### comparando coluna de usuários para id antigo e id novo
load("usuarios.Rdata")
compara<-cbind(user,usuarios)
head(compara,n=5)


us_fil<-do.call("rbind",avaliados.mat)
us_fil<-us_fil[order(us_fil[,2]),]

compara1<-unique(compara[,1])


##### vetor de usuarios correta
usu.final<-match(us_fil[,2],compara1)

##### criando data.frame com coluna de usuarios e filmes
us_fil[,2]<-usu.final
us_fil<-data.frame(us_fil)
names(us_fil)<-c("filmes","Usuarios")
head(usu.final)
head(us_fil)
#####salvando
save(us_fil,file="us_fil.Rdata")
load("us_fil.Rdata")



#####arrumando banco de dados de treinamento para análise exploratória#########
load("xo.Rdata")
head(xo)
load("usuarios.Rdata")
xo$V2<-usuarios
dados.exp<-xo
colnames(dados.exp)<-c("filmes","Usuarios","notas")
head(dados.exp)
save(dados.exp,file="dados.exp.Rdata")
load("dados.exp.Rdata")
#### conjunto filmes e notas
dados.leng<-dados.exp[,c(1,3)]
save(dados.leng,file="dados.leng.Rdata")
load("dados.leng.Rdata")
#### conjunto usuarios e notas
dados.lengu<-dados.exp[,c(2,3)]
save(dados.lengu,file="dados.lengu.Rdata")
load("dados.lengu.Rdata")
#####arrumando banco de dados de sondagem para análise exploratória#########
us_fil<-data.table(us_fil)
probe<-inner_join(dados.exp,us_fil)
probe<-data.table(probe)


############################## Exemplos metodologia ###############3
####################################################################
a<-matrix(c(1,3,4,5,0,0,0,1,3,4,5,2,0,1,1,3,4,5,
            0,0,0,0,0,0,0,4,5,2,0,0,0,0,4,5,2),nrow=7)
b<-svd(a)
options(digits = 5)
reconstruc<-b$u[,1:2]%*%diag(b$d[1:2])%*%t(b$v)[1:2,]

### previsao exemplo fazer para cada vetor usado no tcc
c(4,1,0,0,0)%*%(b$v)[,1:2]%*%t(b$v)[1:2,]


###########################################################

##########################análise exploratória############################

####medidas de tendencia central :
###media mediana max min
resumo<-summary(dados.exp$notas) 
#moda
moda<-function(x){
  ux<-unique(x)
  ux[which.max(tabulate(match(x,ux)))]
}
moda.notas<-moda(dados.exp$notas)
## numero de observações 
nnotas<-length(dados.exp$notas)
### tabela de frequência
tabe<-table(dados.exp$notas)
xtable(data.frame(tabe))
xtable(tabe)

###### tabelas das primeiras e ultimas 5 observaçõoes de cada conjunto

aa<-rbind(head(probe[,1:2,with=F],n=5),tail(probe[,1:2,with=F],n=5))
xtable(aa)

bb<-rbind(head(dados.exp,n=5),tail(dados.exp,n=5))
xtable(bb)




############# medidas de dispersão das avaliações
#variancia das avaliações
var(dados.exp$notas)
#desvio
desvio<-sd(dados.exp$notas)

########## medidas de dispersão entre os usuarios e dentre os usuarios
############################################## HISTOGRAMAS
################## Podemos avaliar simetria e peso da cauda pelo histograma
##################avaliar distribuição das notas
##################histogramas do treinamento e do probe


load("probe.Rdata")
load("dados.exp.Rdata")
dados.h<-dados.exp$notas
save(dados.h,file="dados.h.Rdata")
load("dados.h.Rdata")
histogram( ~ dados.h, xlab = "Avaliações", type ="percent", col="dodgerblue") 
histogram( ~ notas, data = probe, xlab = "Avaliações", type ="percent", 
           col="dodgerblue")


###########################################################################
################ histogramas das médias
media.u<-aggregate(dados.exp[,c(2,3)],list(dados.exp$Usuarios),mean)
load("media.u.Rdata")
media.f<-aggregate(dados.exp[,c(1,3)],list(dados.exp$filmes),mean)
load("media.f.Rdata")
histogram( ~ notas, data=media.u, xlab = "Avaliações", main="Histograma para as médias das avaliações por usuário",col="dodgerblue") +
  histogram( ~ notas, data = media.f, xlab = "Avaliações", main="Histograma para as médias das avaliações por filme",col="red")


##### estudo dos filmes
leng.filme<-aggregate(dados.leng,list(dados.leng$filmes),length)
load("leng.filme.Rdata")
length(leng.filme$Group.1)
which.max(leng.filme$notas)
leng.filme$notas[5317]
which.min(leng.filme$notas)
leng.filme$notas[13755]
ordenado<-order(leng.filme$notas)
leng.filme<-leng.filme[order(leng.filme$notas),]
####filmes com menor numero de avaliações 
head(leng.filme,n=5)
####filmes com maior numero de avaliações 
tail(leng.filme,n=5)

####em média cada filme recebeu x avaliações, var
mean(leng.filme$notas)
var(leng.filme$notas)

####em média cada usuário avaliou y filmes, var 
leng.usu<-aggregate(dados.lengu,list(dados.lengu$Usuarios),length)
head(dados.lengu)
load("leng.usu.Rdata")
mean(leng.usu$notas)
var(leng.usu$notas)

####usuarios que mais e menos avaliaram
leng.usu<-leng.usu[order(leng.usu$notas),]
head(leng.usu)
tail(leng.usu)


#################################### PREVISÃO ###############################################
#### indices dos dados  de tamanho 100000 para usar deBIAS
sdeb<-sample_n(us_fil,1e+05)
save(sdeb,file = "sdeb.Rdata")
load("sdeb.Rdata")
####

load("probe.Rdata")
###sdeb com notas
sdebn<-inner_join(probe,sdeb)
sdebn<-Incomplete(sdebn$Usuarios,sdebn$filmes,sdebn$notas)
save(sdebn,file="sdebn.Rdata")
#### porção dos dados de tamanho 100000 para deBias
load("sdebn.Rdata")
####

#### porção dos dados restantes
sprev<-anti_join(probe,sdeb)
save(sprev,file="sprev.Rdata")
load("sprev.Rdata")
####

probe.notas<-probe$notas
save(probe.notas,file="probe.notas.Rdata")
load("probe.notas.Rdata")

####### modelos softimpute: modelo genérico####
#setwd("C:\\Users\\aluno\\Desktop\\rodar")
load("matespar.Rdata")

### codigo para todos os modelos, basta mudar os argumentos da sequência 
### e da função softimpute
lamseq=exp(seq(from=log(703),log(236),length=10))

lam5<-lamseq[5]
warm=NULL
limi<-10^(-5)
start.time <- Sys.time()
ajuste1.5=softImpute(matespar,lambda=lam5,rank.max=40,warm=warm,trace.it = T,maxit=200,thresh =limi)
end.time <- Sys.time()
time1.5<- end.time - start.time
save(time1.5,file="time1.5.Rdata")
save(ajuste1.5,file="ajuste1.5.Rdata")


############# depois de gerar os modelos, para cada modelo seguir os passos abaixo
############# substituindo os argumentos conforme o modelo

#####carregar modelo
load("ajuste1.Rdata")
#### ajuste 1, lambda=6253.501, rank.max=40
#### e rank=1.

###### previsao
prev1<-impute(ajuste1,i=sprev$Usuarios,j=sprev$filmes)
save(prev1,file="prev1.Rdata")
load("prev1.Rdata")
###### 

#### raiz do erro quadrático médio
rmse1<-sqrt((sum((sprev$notas-prev1)^2))/length(sprev$notas))
save(rmse1,file="rmse1.Rdata")
load("rmse1.Rdata")
#### 


### tirando o viés 
ajusted1<-deBias(sdebn,ajuste1)
save(ajusted1,file="ajusted1.Rdata")
load("ajusted1.Rdata")
###

###### previsao do sem vies
prev1d<-impute(ajusted1,i=sprev$Usuarios,j=sprev$filmes)
save(prev1d,file="prev1d.Rdata")
load("prev1d.Rdata")
###### 

#### raiz do erro quadrático médio do sem vies
rmse1d<-sqrt((sum((sprev$notas-prev1d)^2))/length(sprev$notas))
save(rmse1d,file="rmse1d.Rdata")
load("rmse1d.Rdata")

#### 




