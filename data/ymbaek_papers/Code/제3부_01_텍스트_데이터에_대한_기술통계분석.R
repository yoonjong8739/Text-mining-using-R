########################################
#제 3부 텍스트 데이터 분석 및 결과제시##
#01 텍스트 데이터에 대한 기술통계분석###
########################################


# setwd("D:/data/code")

#tm 패키지 함수를 설명했던 dtm.e 오브젝트를 사용할 것이다. 
#먼저 단어의 발현빈도를 구해보자. 
library('tm')
library('stringr')
#앞서 작업했던 사전처리 영어 말뭉치와 한국어 말뭉치를 불러오자. 
corpus.e.pp <- readRDS("CorpusE_preprocess.RData")
corpus.k.pp <- readRDS("CorpusK_preprocess.RData")
#앞서 작업했던 영어 DTM과 한국어 DTM을 불러오자. 
dtm.e <- readRDS("dtmE.RData")
dtm.k <- readRDS("dtmK.RData")
word.freq <- apply(dtm.e[,],2,sum)
#아래와 같은 형식의 벡터가 구성, 839개 단어가 총 2968번 등장했다. 
length(word.freq); sum(word.freq)

#빈도가 높은 것부터 낮은 것으로 정렬하자. 
sort.word.freq <- sort(word.freq,decreasing=TRUE)
head(sort.word.freq,20)

#다음과 같이 하면 누적빈도를 계산할 수 있다. 
cumsum.word.freq <- cumsum(sort.word.freq)
head(cumsum.word.freq,20)

#다음과 같이 하면 전체합이 1이 되는 비율을 계산할 수 있다. 
prop.word.freq <- cumsum.word.freq/cumsum.word.freq[length(cumsum.word.freq)]
head(prop.word.freq,20)

#단어빈도에 대한 간단한 분석  
library('ggplot2')  #혹은 library('tidyverse')
#선그래프용 데이터 
myfig <- data.frame(1:length(word.freq),prop.word.freq)
colnames(myfig) <- c('loc','prop')
#점그래프용 데이터 
myfig2 <- myfig[round(83.9*(0:10)),]
myfig2$lblloc <- paste(10*(1:10),"%th\nposition",sep="")
ggplot()+
  geom_line(data=myfig,aes(x=loc,y=prop),color='red')+
  geom_point(data=myfig2,aes(x=loc,y=prop),size=2,color='blue')+
  scale_x_continuous(breaks=myfig2$loc,labels=myfig2$lblloc)+
  scale_y_continuous(breaks=0.20*(0:5),labels=paste(20*(0:5),"%",sep=""))+
  labs(x='\nOrder of word frequency',y='Cumulative proportion\n')+
  theme_classic()
# setwd("D:/data/Fig"); ggsave("Figure_part3_ch01_01.jpg",width=18,height=9,unit='cm')

#wordcloud2 패키지 구동 
#제1판에서는 wordcloud 패키지를 사용하였지만 wordcloud2가 보다 시각적으로 보기 좋다.
library('wordcloud2')
WC_data <- data.frame(names(sort.word.freq),sort.word.freq)
names(WC_data) <- c('word','freq')
wordcloud2(WC_data,
           fontFamily="Times New Roman",  #원하는 글자폰트를 지정함 
           shuffle=FALSE,  #TRUE로 지정하면 그릴 때마다 단어배치가 바뀜
           rotateRatio=0.3,  #0이면 회전없음, 1이면 모두회전 
           shape="circle")
#Figure_part3_ch01_02.jpg 저장 

#빈도수10 이상의 단어들로만 그림 
wordcloud2(WC_data[WC_data$freq>9,],
           fontFamily="Times New Roman",
           shuffle=FALSE,
           rotateRatio=0.3,
           shape="circle")
#Figure_part3_ch01_03.jpg 저장 


#이제는 한국어 학술논문들의 말뭉치를 분석해 보자. 
#동일한 과정을 밟았다. 
word.freq <- apply(dtm.k[,],2,sum)
sort.word.freq <- sort(word.freq,decreasing=TRUE)
cumsum.word.freq <- cumsum(sort.word.freq)
prop.word.freq <- cumsum.word.freq/cumsum.word.freq[length(cumsum.word.freq)]
#선그래프용 데이터 
myfig <- data.frame(1:length(word.freq),prop.word.freq)
colnames(myfig) <- c('loc','prop')
#점그래프용 데이터 
myfig2 <- myfig[round(31.5*(0:10)),]
myfig2$lblloc <- paste(10*(1:10),"%th\nposition",sep="")
ggplot()+
  geom_line(data=myfig,aes(x=loc,y=prop),color='red')+
  geom_point(data=myfig2,aes(x=loc,y=prop),size=2,color='blue')+
  scale_x_continuous(breaks=myfig2$loc,labels=myfig2$lblloc)+
  scale_y_continuous(breaks=0.20*(0:5),labels=paste(20*(0:5),"%",sep=""))+
  labs(x='\n단어빈도 정렬 발생위치',y='누적비율\n')+
  theme_classic()
# setwd("D:/data/Fig"); ggsave("Figure_part3_ch01_04.jpg",width=18,height=9,unit='cm')

#단어빈도수 5회 이상인 경우만 제시함
WC_data <- data.frame(names(sort.word.freq),sort.word.freq)
names(WC_data) <- c('word','freq')
wordcloud2(WC_data[WC_data$freq>4,],
           size=0.7, #글자크기를 조금 줄임(디폴트값은 1)
           shuffle=FALSE,  #TRUE로 지정하면 그릴 때마다 단어배치가 바뀜
           rotateRatio=0.3,  #0이면 회전없음, 1이면 모두회전 
           shape="circle")
#Figure_part3_ch01_05.jpg 저장 


#findAssocs 함수를 사용하면 DTM, TDM에 등장하는 단어가 
#지정된 수치 이상인 값으로 연관된 단어들 목록을 알 수 있다. 
#즉 아래는 dtm.e라는 DTM에 속한 polit라는 단어와 .50이상의 
#상관관계를 갖는 단어들의 목록을 구하는 방법이다. 
findAssocs(dtm.e,"polit",0.50)

#findAssocs는 사실 피어슨 상관계수다. 
#예를 들어 polit와 elect 두 단어의 상관계수를 구해보자. 
var1 <- as.vector(dtm.e[,"polit"])
var2 <- as.vector(dtm.e[,"elect"])
cor.test(var1,var2)

#위와 같은 과정을 개인맞춤형 함수로 지정해 보자. 
my.assoc.func <- function(mydtm,term1,term2){
  myvar1 <- as.vector(mydtm[,term1])
  myvar2 <- as.vector(mydtm[,term2])
  cor.test(myvar1,myvar2)
}
#동일한 결과를 얻을 수 있다. 
my.assoc.func(dtm.e,"polit","elect")

#DTM을 TDM으로 전치시키면 문서와 문서의 상관계수도 구할 수 있다. 
my.assoc.func(t(dtm.e),"p2014a.txt","p2019a.txt")
my.assoc.func(t(dtm.e),"p2014a.txt","p2014g.txt")

#아래와 같이 문서와 문서의 상관계수 행렬을 계산할 수도 있다. 
length.doc <- length(rownames(dtm.e))
my.doc.cor <- matrix(NA,nrow=length.doc,ncol=length.doc)
for (i in 1:length.doc) {
  for (j in 1:length.doc) {
    my.doc.cor[i,j] <- my.assoc.func(t(dtm.e),rownames(dtm.e)[i],rownames(dtm.e)[j])$est
  }
}
rownames(my.doc.cor) <- colnames(my.doc.cor) <- rownames(dtm.e)
diag(my.doc.cor) <- NA 
#상관계수 행렬의 일부만 살펴보자. 
round(my.doc.cor[1:4,1:4],2)

#예를 들어 첫번째 문서, 즉 p2009a.txt는 다른 문서들과 어떤 상관관계를 보이며,
#어느 문서와 가장 비슷하게 연관되어 있을까?
round(my.doc.cor[,1],2)
my.doc.cor[,1][(my.doc.cor[,1]==max(my.doc.cor[,1],na.rm=T))]

#아래와 같은 방법으로 문서간 상관계수의 히스토그램을 살펴보자. 
library('ggplot2') #혹은 tidyverse 패키지 구동 
df_cor <- data.frame(my.doc.cor[lower.tri(my.doc.cor)])
names(df_cor) <- 'cor_coef'
ggplot(df_cor, aes(x=cor_coef))+
  geom_histogram(bins=40)+
  labs(title="Correlations between English journal papers",
       x="correlations", y="frrequency")+
  theme_classic()
summary(df_cor)
# setwd("D:/data/Fig"); ggsave("Figure_part3_ch01_06.jpg",width=18,height=9,unit='cm')

#한국어 학술논문 말뭉치에 대해서도 문서간 유사도를 계산해보자. 
#영어 학술논문 말뭉치를 분석한 경우와 동일한 과정을 밟았다. 
length.doc <- length(rownames(dtm.k))
my.doc.cor <- matrix(NA,nrow=length.doc,ncol=length.doc)
for (i in 1:length.doc) {
  for (j in 1:length.doc) {
    my.doc.cor[i,j] <- my.assoc.func(t(dtm.k),rownames(dtm.k)[i],rownames(dtm.k)[j])$est
  }
}
rownames(my.doc.cor) <- colnames(my.doc.cor) <- rownames(dtm.k)
diag(my.doc.cor) <- NA 
#상관계수의 히스토그램은 다음과 같다. 
df_cor <- data.frame(my.doc.cor[lower.tri(my.doc.cor)])
names(df_cor) <- 'cor_coef'
ggplot(df_cor, aes(x=cor_coef))+
  geom_histogram(bins=40)+
  labs(title="한국어 논문간 상관계수 분포",
       x="상관계수", y="빈도")+
  theme_classic()
summary(df_cor)
# setwd("D:/data/Fig"); ggsave("Figure_part3_ch01_07.jpg",width=18,height=9,unit='cm')


#연관규칙 Association rules 
#간단한 예시사례 
d1 <- c("A","B","C")
d2 <- c("A","C","D","E")
d3 <- c("B","C","D","F")
d4 <- c("A","B","C","D")
myDs <- list(d1,d2,d3,d4)
myDs

library("arules")
#arules 패키지에 맞는 오브젝트 형태로 변환 
myDs <- as(myDs, "transactions")
inspect(myDs)
#모든 가능한 연관규칙 추출
myresult <- apriori(myDs)
#살펴보기 
inspect(myresult)

#문서의 개수만큼 벡터 생성 
words_appear <- rep(NA,length(corpus.e.pp))
#각 문서별로 등장단어 목록을 정리함 
for (i in 1:length(corpus.e.pp)){
  my_appear_words <- names(table(unlist(str_split(corpus.e.pp[[i]]$content," "))))
  words_appear[i] <- str_c(my_appear_words,collapse=" ")
  words_appear[i] <- str_trim(words_appear[i])
}
#정리된 등장단어 목록들을 리스트로 묶음 
data_AR <- str_split(words_appear," ",length(colnames(dtm.e)))
#transaction 형태 오브젝트로 변환
data_AR <- as(data_AR, "transactions")
inspect(head(data_AR))

#지지도>0.30이며 확신도>0.50
trial_01 <- apriori(data_AR, 
                    parameter=list(support=0.30,confidence=0.50))
inspect(head(sort(trial_01, by="support"),10))  #지지도 순서대로 
inspect(head(sort(trial_01, by="confidence"),10))  #확신도 순서대로 
inspect(head(sort(trial_01, by="lift"),10))  #향상도 순서대로 

#지지도>0.20이며 확신도>0.50
trial_02 <- apriori(data=data_AR,
                    parameter=list(support=0.20,confidence=0.50),
                    appearance=list(rhs="network"))
inspect(trial_02)

#지지도>0.10이며 확신도>0.50
#studi,result,effect,examin을 뺄 때, polit,elect,particip 표현등장시 등장 단어는?
trial_03 <- apriori(data=data_AR,
                  parameter=list(support=0.10,confidence=0.50),
                  appearance=list(lhs=c("polit","elect","particip"),
                                  none=c("studi","result","effect","examin")))
inspect(head(sort(trial_03, by="lift"),10))  #향상도 순서대로 


#tm패키지의 inspect 함수를 다시 디폴트로 
inspect <- tm::inspect 




library('factoextra')   #군집분석 시각 
#발현단어빈도에 기반하여 문서들의 유사도/거리를 살펴 보자.  
dist.dtm.e <- dist(dtm.e)
as.matrix(dist.dtm.e)[1:4,1:4]

#Ward’s method 사용 군집분석을 실시함. 
myclusters <- hclust(dist.dtm.e,method="ward.D2")
#군집분석결과를 그래프로 그리면 아래와 같다.
fviz_dend(myclusters)
# setwd("D:/data/Fig"); ggsave("Figure_part3_ch01_07.jpg",width=24,height=14,unit='cm')

#라벨이 별로 보기 좋지 않다. 년도와 년도 뒤의 ab..만 두고 나머지 텍스트는 지우자. 
myclusters$labels <- str_extract(myclusters$labels, "[[:digit:]]{4}[[:alpha:]]{1}")
#군집분석결과를 그래프로 다시 그렸다
fviz_dend(myclusters)
# setwd("D:/data/Fig"); ggsave("Figure_part3_ch01_08.jpg",width=24,height=14,unit='cm')

#13개의 군집을 설정하자
mygroup <- cutree(myclusters,k=13)
table(mygroup)

#각 문서별 소속군집은? 
mygroup

#군집분석결과를 시각적으로 보다 보기 좋게 바꾸었다
fviz_dend(myclusters,k=13)
# setwd("D:/data/Fig"); ggsave("Figure_part3_ch01_09.jpg",width=24,height=14,unit='cm')

#13개의 군집을 설정하고, 각 군집별로 출간년도의 빈도를 구해보자
mytable <- table(str_extract(myclusters$labels,"[[:digit:]]*"),
                 cutree(myclusters,k=13))
mytable


#위의 결과를 막대그래프로 그리면 보다 효율적이다. 
#사전준비작업을 합시다. 응용편을 참조하라. 
library('ggplot2')
cluster.by.year <- data.frame(mytable)
colnames(cluster.by.year) <- c('year','cluster','publish')
cluster.by.year$cluster <- paste('cluster',cluster.by.year$cluster,sep='')
cluster.by.year$year <- as.numeric(as.character(cluster.by.year$year))
#시간에 따른 변화를 살펴보자
cluster.color <- c("red1","blue1","yellow1","brown1","grey1","hotpink1","tan1",
                   "red4","blue4","yellow4","brown4","grey4","hotpink4")
ggplot(data=cluster.by.year, aes(x=year, y=publish, fill=cluster)) + 
  geom_bar(stat="identity")+
  scale_x_continuous(breaks=2009:2019,labels=2009:2019)+
  scale_y_continuous(breaks=0:7,labels=0:7)+
  scale_fill_manual(values=cluster.color,labels=paste('Cluster',1:13,sep=''))+
  labs(x="Publication year",y="Number of published papers",fill='Cluster #')+
  theme_classic()
# setwd("D:/data/Fig"); ggsave("Figure_part3_ch01_10.jpg",width=18,height=14,unit='cm')


#한국어 학술논문 말뭉치에 대한 분석결과는 아래와 같다. 
#위에서 소개한 영문 학술논문 말뭉치에 대한 분석결과에서의 설명을 참조하라. 
dist.dtm.k <- dist(dtm.k)
myclusters <- hclust(dist.dtm.k,method="ward.D2")
#년도와 년도 뒤의 ab..만 두고 나머지 텍스트는 지우자. 
myclusters$labels <- str_extract(myclusters$labels, "[[:digit:]]{4}[[:alpha:]]{1}")
#군집분석결과를 그래프로 그리면 아래와 같다.
fviz_dend(myclusters)
# setwd("D:/data/Fig"); ggsave("Figure_part3_ch01_11.jpg",width=24,height=14,unit='cm')

#군집의 수를 5로 정하거나 8, 심지어 18까지 늘려도 군집이 잘 나누어지지 않는다. 
#적어도 국문 학술논문 말뭉치를 구성하는 문서들을 적절하게 군집화하는 것은 쉽지 않다.
#여기서는 영어 학술논문 말뭉치에서 적용했던 13을 그대로 이용하자. 
mygroup <- cutree(myclusters,k=5)
table(mygroup)
mygroup <- cutree(myclusters,k=8)
table(mygroup)
mygroup <- cutree(myclusters,k=18)
table(mygroup)
mygroup <- cutree(myclusters,k=13)
mygroup

#군집분석결과를 그래프로 그리면 아래와 같다.
fviz_dend(myclusters,k=13)
# setwd("D:/data/Fig"); ggsave("Figure_part3_ch01_12.jpg",width=24,height=14,unit='cm')

mytable <- table(str_extract(myclusters$labels,"[[:digit:]]*"),
                 cutree(myclusters,k=13))
cluster.by.year <- data.frame(mytable)
colnames(cluster.by.year) <- c('year','cluster','publish')
cluster.by.year$cluster <- paste('cluster',cluster.by.year$cluster,sep='')
cluster.by.year$year <- as.numeric(as.character(cluster.by.year$year))
#시간에 따른 변화를 살펴보자 
ggplot(data=cluster.by.year, aes(x=year, y=publish, fill=cluster)) + 
  geom_bar(stat="identity")+
  scale_x_continuous(breaks=2004:2019,labels=2004:2019)+
  scale_y_continuous(breaks=0:7,labels=0:7)+
  scale_fill_manual(values=cluster.color,labels=paste('군집',1:13,sep=''))+
  labs(x="출판년도",y="출간논문수",fill='군집')+
  theme_classic()
# setwd("D:/data/Fig"); ggsave("Figure_part3_ch01_12.jpg",width=18,height=14,unit='cm')

