########################################
#제 3부 텍스트 데이터 분석 및 결과제시##
#02 토픽모형############################
########################################

# setwd("D:/data/code")

#앞서 작업한 말뭉치, DTM불러오고, tm, stringr, ggplot2 패키지 구동
corpus.e.pp <- readRDS("CorpusE_preprocess.RData")
corpus.k.pp <- readRDS("CorpusK_preprocess.RData")
dtm.e <- readRDS("dtmE.RData")
dtm.k <- readRDS("dtmK.RData")
library('tm')
library('stringr')
library('ggplot2')

#LDA를 시작하기 이전에, 다음의 문헌이 어렵더라도 3-4번 정도 훑어보기 바란다.
#Blei, D. M., Ng, A. Y., & Jordan, M. I. (2003). Latent dirichlet allocation. 
#Journal of machine Learning research, 3, 993-1022.
#최적의 잠재토픽 개수 탐색의 경우 별첨자료-2 참조 : 7개가 가장 적절해 보임. 
#영문학술논문 말뭉치에서 7개의 잠재토픽을 추출
#control=list(seed=숫자)옵션을 지정하지 않으면 매번 실행한 결과가 달라진다. 
library('topicmodels')
lda.out <- LDA(dtm.e,control=list(seed=20191116),k=7)

#아래는 문서X토픽 행렬이다. 
dim(lda.out@gamma)
#아래는 토픽X단 행렬이다. 
dim(lda.out@beta)

#7개의 잠재토픽으로 분류된 상위20개 단어들을 살펴보자. 
terms(lda.out,20)
#순서대로 다음과 같이 이름을 붙여보자. 
#Topic 1=Election studies
#Topic 2=Stereotype in communication 
#Topic 3=Political communication
#Topic 4=privacy studies 
#Topic 5=SNS in communication 
#Topic 6=Health communication 
#Topic 7=Inter-culture communication 


#각 문서가 담고있는 잠재토픽의 확률점수를 계산하였다. 
posterior_lda <- posterior(lda.out)
round(posterior_lda$topics,3)

#아래와 같이 향후 분석에 사용할 수 있도록 데이터 프레임으로 저장하였다. 
lda.topics <- data.frame(posterior_lda$topics)
#아래는 문서별 토픽 등장 확률의 총합이다. 
apply(lda.topics,1,sum)

#아래는 문서들 전체에서의 토픽등장 확률의 총합이다. 
apply(lda.topics,2,sum)

#년도별로 각각의 잠재토픽이 어떤 변화를 보이는지 살펴보자. 
tempyear <- rownames(lda.topics)
pubyear <- as.numeric(unlist(str_extract_all(tempyear,"[[:digit:]]{4}")))
#아래의 결과를 보면 년도별 잠재토픽 변화패턴을 살펴볼 수 있다.
topic.by.year <- round(aggregate(as.matrix(lda.topics)~pubyear,lda.topics,sum),5)
topic.by.year

#그래프를 그려보자 
#데이터를 긴 형태로 
fig_LDA_english <- reshape(topic.by.year, idvar = "pubyear",
                   varying = list(2:8),
                   v.names = "X", direction = "long")
colnames(fig_LDA_english) <- c('year','topic_i','score')
#각 토픽별 이름 
fig_LDA_english$topic <- factor(fig_LDA_english$topic,
                        labels=c("Election\nstudies\n",
                                 "Stereotype\nin comm.\n",
                                 "Political\ncomm.\n",
                                 "Privacy\nstudies\n",
                                 "SNS\nin comm\n",
                                 "Health\ncomm\n",
                                 "Inter-culture\ncomm\n"))
#토픽의 알파벳 순서로 정렬 
fig_LDA_english$topic <- as.character(fig_LDA_english$topic)
head(fig_LDA_english)

ggplot(data=fig_LDA_english, aes(x=year, y=score, fill=topic)) + 
  geom_bar(stat="identity")+
  scale_x_continuous(breaks=2009:2019,labels=2009:2019)+
  scale_y_continuous(breaks=2*(0:5),labels=2*(0:5))+
  scale_fill_manual(values=1:7)+
  labs(x="publication year",y="score",fill='latent topic')+
  ggtitle("LDA: English journal papers corpus")+
  theme_classic() 
# setwd("D:/data/Fig"); ggsave("Figure_part3_ch02_01.jpg",width=18,height=14,unit='cm')

#alpha 값이 LDA 결과에 미치는 효과 
#시나리오1
round(posterior(LDA(dtm.e,
                    control=list(seed=20191116,alpha=10),k=7))$topics,3)

#시나리오2
round(posterior(LDA(dtm.e,
                    control=list(seed=20191116,alpha=100),k=7))$topics,3)

#우선 LDA와 마찬가지로 5개의 잠재토픽을 추정하고, 
#마찬가지로 상위 20개까지의 단어들을 점검한다. 
ctm.out <- CTM(dtm.e,control=list(seed=20191119),k=7)
terms(ctm.out,20)
#순서대로 다음과 같이 이름을 붙여보자. 
#Topic 1=SNS in communication 
#Topic 2=Inter-culture communication 
#Topic 3=Election studies
#Topic 4=Health communication 
#Topic 5=Stereotype in communication 
#Topic 6=Political communication
#Topic 7=privacy studies 

#잠재토픽과 단어들에 부여된 가중치를 살펴보고, 문서별 잠재토픽의 확률을 점검하였다. 
posterior_ctm <- posterior(ctm.out)
ctm.topics <- data.frame(posterior_ctm$topics)
round(ctm.topics,3)

#년도에 따라 CTM으로 분류한 잠재토픽의 변화를 살펴보았다. 
tempyear <- rownames(ctm.topics)
pubyear <- as.numeric(unlist(str_extract_all(tempyear,"[[:digit:]]{4}")))
topic.by.year <- round(aggregate(as.matrix(ctm.topics)~pubyear,ctm.topics,sum),3)
topic.by.year

#시간에 따른 변화를 살펴보자. 
fig_CTM_english <- reshape(topic.by.year, idvar = "pubyear", 
                   varying = list(2:8),
                   v.names = "X", direction = "long")
colnames(fig_CTM_english) <- c('year','topic_i','score')
fig_CTM_english$topic <- factor(fig_CTM_english$topic,
                        labels=c("SNS\nin comm\n",
                                 "Inter-culture\ncomm\n",
                                 "Election\nstudies\n",
                                 "Health\ncomm\n",
                                 "Stereotype\nin comm.\n",
                                 "Political\ncomm.\n",
                                 "Privacy\nstudies\n"))
#토픽의 알파벳 순서로 정렬 
fig_CTM_english$topic <- as.character(fig_CTM_english$topic)
#시각화 
ggplot(data=fig_CTM_english, aes(x=year, y=score, fill=topic)) + 
  geom_bar(stat="identity")+
  scale_x_continuous(breaks=2009:2019,labels=2009:2019)+
  scale_y_continuous(breaks=2*(0:5),labels=2*(0:5))+
  scale_fill_manual(values=1:7)+
  labs(x="publication year",y="score",fill='latent topic')+
  ggtitle("CTM: English journal papers corpus")+
  theme_classic() 
# setwd("D:/data/Fig"); ggsave("Figure_part3_ch02_02.jpg",width=18,height=14,unit='cm')

#LDA와 CTM의 추정결과를 비교해 보자. 
#데이터프레임 오브젝트 구성 
lda_topic <- factor(as.vector(topics(lda.out)),
                    labels=c("Election studies","Stereotype in comm","Political comm",
                             "Privacy studies","SNS in comm","Health comm",
                             "Inter-culture comm"))
ctm_topic <- factor(as.vector(topics(ctm.out)),
                    labels=c("SNS in comm","Inter-culture comm","Election studies",
                             "Health comm","Stereotype in comm","Political comm",
                             "Privacy studies"))
LDA_CTM <- data.frame(as.character(lda_topic),as.character(ctm_topic))
colnames(LDA_CTM) <- c("lbl_LDA","lbl_CTM")
#토픽들의 분포비교 
distr_topic <- rbind(prop.table(table(LDA_CTM$lbl_LDA)),
                     prop.table(table(LDA_CTM$lbl_CTM)) )
distr_topic
distr_topic <- data.frame(distr_topic)
distr_topic$model <- c("LDA","CTM")
#그래프를 그려보자 
#데이터를 긴 형태로 
fig_LDA_CTM <- reshape(distr_topic, idvar = "model",
                       varying = list(1:7),
                       v.names = "prop",
                       direction = "long")
colnames(fig_LDA_CTM) <- c('model','topic_i','prop')
fig_LDA_CTM$topic <- factor(fig_LDA_CTM$topic_i,labels=colnames(distr_topic)[1:7])
#시각화 
ggplot(data=fig_LDA_CTM, aes(x=topic,y=prop,fill=model)) + 
  geom_bar(stat="identity",position="dodge")+
  scale_y_continuous(breaks=0.1*(0:3))+
  coord_flip()+
  labs(x="Semantic labels for seven topics",y="proportion",fill="Topic model")+
  ggtitle("Comparison between LDA model and CTM")+
  theme_bw() 
# setwd("D:/data/Fig"); ggsave("Figure_part3_ch02_03.jpg",width=20,height=18,unit='cm')

#라벨이 일치하는 사례
table(substr(LDA_CTM$lbl_LDA,1,8),substr(LDA_CTM$lbl_CTM,1,8))
table(LDA_CTM$lbl_LDA==LDA_CTM$lbl_CTM)
prop.table(table(LDA_CTM$lbl_LDA==LDA_CTM$lbl_CTM))

#한국어 학술논문 말뭉치를 대상으로도 LDA와 CTM을 실시해 보자.
#먼저 LDA를 실시하였다. 
lda.out <- LDA(dtm.k,control=list(seed=20191115),k=7)
terms(lda.out,20)
#topic1=캠페인 연구
#topic2=체감안전도 연구 
#topic3=수용자 연구
#topic4=저널리즘 연구
#topic5=데이터마이닝 활용연구
#topic6=조사방법 연구 
#topic7=공동체 연구 

posterior_lda <- posterior(lda.out)
lda.topics <- data.frame(posterior_lda$topics)
tempyear <- rownames(lda.topics)
pubyear <- as.numeric(unlist(str_extract_all(tempyear,"[[:digit:]]{4}")))
topic.by.year <- round(aggregate(as.matrix(lda.topics)~pubyear,lda.topics,sum),5)

fig_LDA_korean <- reshape(topic.by.year, idvar = "pubyear", 
                          varying = list(2:8),
                          v.names = "X", direction = "long")
colnames(fig_LDA_korean) <- c('year','topic_i','score')
fig_LDA_korean$topic <- factor(fig_LDA_korean$topic,
                               labels=c("캠페인 연구\n",
                                        "체감안전도 연구\n",
                                        "수용자 연구\n",
                                        "저널리즘 연구\n",
                                        "데이터마이닝 활용연구\n",
                                        "조사방법 연구\n",
                                        "공동체 연구\n"))
#토픽의 알파벳 순서로 정렬 
fig_LDA_korean$topic <- as.character(fig_LDA_korean$topic)

#시간에 따른 변화를 살펴보자. 
ggplot(data=fig_LDA_korean, aes(x=year, y=score, fill=topic)) + 
  geom_bar(stat="identity")+
  scale_x_continuous(breaks=2004:2019,labels=2004:2019)+
  scale_y_continuous(breaks=2*(0:5),labels=2*(0:5))+
  scale_fill_manual(values=1:7)+
  labs(x="출판년도",y="점수",fill='잠재토픽')+
  ggtitle("잠재적 디리쉴레 할당모형: 한국어 학술논문 말뭉치")+
  theme_classic() 
# setwd("D:/data/Fig"); ggsave("Figure_part3_ch02_04.jpg",width=18,height=14,unit='cm')

#다음은 CTM을 적용해 보자.  
ctm.out <- CTM(dtm.k,control=list(seed=20191119),k=7)
terms(ctm.out,20)
#topic1=공동체 연구 
#topic2=데이터마이닝 활용연구
#topic3=수용자 연구
#topic4=조사방법 연구
#topic5=체감안전도 연구 
#topic6=캠페인 연구 
#topic7=저널리즘 연구

posterior_ctm <- posterior(ctm.out)
ctm.topics <- data.frame(posterior_ctm$topics)
tempyear <- rownames(ctm.topics)
pubyear <- as.numeric(unlist(str_extract_all(tempyear,"[[:digit:]]{4}")))
topic.by.year <- round(aggregate(as.matrix(ctm.topics)~pubyear,ctm.topics,sum),5)

fig_CTM_korean <- reshape(topic.by.year, idvar = "pubyear", 
                          varying = list(2:8),
                          v.names = "X", direction = "long")
colnames(fig_CTM_korean) <- c('year','topic_i','score')
fig_CTM_korean$topic <- factor(fig_CTM_korean$topic,
                               labels=c("공동체 연구\n",
                                        "데이터마이닝 활용연구\n",
                                        "수용자 연구\n",
                                        "조사방법 연구\n",
                                        "체감안전도 연구\n",
                                        "캠페인 연구\n",
                                        "저널리즘 연구\n"))
#토픽의 알파벳 순서로 정렬 
fig_CTM_korean$topic <- as.character(fig_CTM_korean$topic)

#시간에 따른 변화를 살펴보자. 
ggplot(data=fig_CTM_korean, aes(x=year, y=score, fill=topic)) + 
  geom_bar(stat="identity")+
  scale_x_continuous(breaks=2004:2019,labels=2004:2019)+
  scale_y_continuous(breaks=2*(0:5),labels=2*(0:5))+
  scale_fill_manual(values=1:7)+
  labs(x="출판년도",y="점수",fill='잠재토픽')+
  ggtitle("상관토픽 모형: 한국어 학술논문 말뭉치")+
  theme_classic() 
# setwd("D:/data/Fig"); ggsave("Figure_part3_ch02_05.jpg",width=18,height=14,unit='cm')

#LDA와 CTM의 추정결과를 비교해 보자. 
#데이터프레임 오브젝트 구성 
lda_topic <- factor(as.vector(topics(lda.out)),
                    labels=c("캠페인 연구\n",
                             "체감안전도 연구\n",
                             "수용자 연구\n",
                             "저널리즘 연구\n",
                             "데이터마이닝 활용연구\n",
                             "조사방법 연구\n",
                             "공동체 연구\n"))
ctm_topic <- factor(as.vector(topics(ctm.out)),
                    labels=c("공동체 연구\n",
                             "데이터마이닝 활용연구\n",
                             "수용자 연구\n",
                             "조사방법 연구\n",
                             "체감안전도 연구\n",
                             "캠페인 연구\n",
                             "저널리즘 연구\n"))
LDA_CTM <- data.frame(as.character(lda_topic),as.character(ctm_topic))
colnames(LDA_CTM) <- c("lbl_LDA","lbl_CTM")
#토픽들의 분포비교 
distr_topic <- rbind(prop.table(table(LDA_CTM$lbl_LDA)),
                     prop.table(table(LDA_CTM$lbl_CTM)) )
distr_topic <- data.frame(distr_topic)
distr_topic$model <- c("LDA","CTM")
#그래프를 그려보자 
#데이터를 긴 형태로 
fig_LDA_CTM <- reshape(distr_topic, idvar = "model",
                       varying = list(1:7),
                       v.names = "prop",
                       direction = "long")
colnames(fig_LDA_CTM) <- c('model','topic_i','prop')
fig_LDA_CTM$topic <- factor(fig_LDA_CTM$topic_i,labels=colnames(distr_topic)[1:7])
#시각화 
ggplot(data=fig_LDA_CTM, aes(x=topic,y=prop,fill=model)) + 
  geom_bar(stat="identity",position="dodge")+
  scale_y_continuous(breaks=0.1*(0:3))+
  coord_flip()+
  labs(x="7개 잠재토픽의 의미론적 라벨",
       y="비율",fill="토픽모형")+
  ggtitle("LDA 모형과 CTM 비교 ")+
  theme_bw() 
# setwd("D:/data/Fig"); ggsave("Figure_part3_ch02_06.jpg",width=20,height=18,unit='cm')

#라벨이 일치하는 사례
table(substr(LDA_CTM$lbl_LDA,1,3),substr(LDA_CTM$lbl_CTM,1,3))
table(LDA_CTM$lbl_LDA==LDA_CTM$lbl_CTM)
prop.table(table(LDA_CTM$lbl_LDA==LDA_CTM$lbl_CTM))



# 영문논문초록에 대한 STM 추정
library('stm')
#앞서 작업한 말뭉치, DTM불러오고, tm, stringr, ggplot2 패키지 구동
corpus.e.pp <- readRDS("CorpusE_preprocess.RData")
corpus.k.pp <- readRDS("CorpusK_preprocess.RData")

#STM에서 사용되는 데이터가 어떤 형태인지 살펴보자. 
dim(gadarian)
colnames(gadarian)
summary(gadarian)

#사전처리된 영문 말뭉치를 STM 모형추정에 가능하도록 재구성하자. 
mytxtdf <- data.frame(dtm.e$dimnames$Docs)
colnames(mytxtdf) <- "filename"
#해당 영문논문의 발간년도를 추출하였다. 
mytxtdf$year <- as.numeric(unlist(
  str_extract_all(mytxtdf$filename,"[[:digit:]]{4}")))
head(mytxtdf)
#저자가 미국에 체류할 때를 0, 귀국하였을 때를 1로 하는 가변수 생성
mytxtdf$return.kor <- ifelse(mytxtdf$year>2011,1,0) 
#논문의 초록을 문자형 자료로 입력하였다. 
mytxtdf$abstract <- NA
for (i in 1:dim(mytxtdf)[1]) {
  mytxtdf$abstract[i] <- as.character(str_c(corpus.e.pp[[i]]$content,collapse=" "))
}
#gadarian 데이터와 같은 형식이 된 것을 확인할 수 있다. 
summary(mytxtdf)

#텍스트의 사전처리는 tm 패키지 이용하였기에 별도 지정하지 않음 
mypreprocess <- textProcessor(mytxtdf$abstract,metadata=mytxtdf,
                              lowercase=FALSE,removestopwords=FALSE,
                              removenumbers=FALSE,removepunctuation=FALSE,
                              stem=FALSE)
mypreprocess$documents[1]

#DTM 구성함 
myout <- prepDocuments(mypreprocess$documents, 
                       mypreprocess$vocab, mypreprocess$meta,
                       lower.thresh=0)

#STM 추정 (7개의 토픽)
mystm <- stm(myout$documents, myout$vocab, K=7, 
             prevalence =~ return.kor, data=myout$meta,
             seed=20191120,init.type="Spectral")

#각 토픽이 발현되는 단어들 점검
labelTopics(mystm,topics=1:7)
#순서대로 다음과 같이 이름을 붙여보자. 
#Topic 1=SNS in communication 
#Topic 2=Election studies
#Topic 3=Stereotype in communication
#Topic 4=privacy studies 
#Topic 5=Political communication
#Topic 6=Health communication 
#Topic 7=Inter-culture communication  

#토픽들 사이의 관계
mystm.corr <- topicCorr(mystm)
mystm.corr
#토픽들 사이의 관계의 시각화
library('igraph')
plot(mystm.corr)
# jpeg("D:/data/Fig/Figure_part3_ch02_07.jpg",width=480)
# dev.off()

#각주
mytemp <- stm(myout$documents, myout$vocab, K=50, 
              prevalence =~ return.kor, data=myout$meta,
              seed=20191120,init.type="Spectral")
mystm.corr <- topicCorr(mytemp)
plot(mystm.corr)
# jpeg("D:/data/Fig/Figure_part3_ch02_08.jpg",width=480)
# dev.off()

#메타데이터와 토픽발현가능성의 관계 테스트
set.seed(20191121)
myresult <- estimateEffect(c(1:7) ~ return.kor,
                           mystm,mytxtdf)
summary(myresult)

#모형추정결과 시각화 
par(mfrow=c(2,1))
plot(myresult,covariate="return.kor",
     topics=2,model=mystm,xlim=c(-1.5,1.5),
     main="Topic 2, Election studies, p < .05")
plot(myresult,covariate="return.kor",
     topics=5,model=mystm,xlim=c(-1.5,1.5),
     main="Topic 5, Political communication, p < .10")
# jpeg("D:/data/Fig/Figure_part3_ch02_09.jpg",width=480)
# dev.off()

#tidystm 을 쓰면 훨씬 더 효율적 
#devtools::install_github("mikaelpoul/tidystm")
library("tidystm")
STM_estimate <- extract.estimateEffect(x=myresult,
                       covariate = "return.kor",
                       method = "pointestimate",
                       model = mystm)
STM_estimate$label <- factor(STM_estimate$topic,
                             labels=c("SNS\nin comm",
                                      "Election\nstudies",
                                      "Stereotype\nin comm.",
                                      "Privacy\nstudies",
                                      "Political\ncomm.",
                                      "Health\ncomm",
                                      "Inter-culture\ncomm"))
STM_estimate$return.kor <- factor(STM_estimate$covariate.value,
                             labels=c("In USA\n(Before 2011)",
                                      "In Korea\n(Since 2012)"))
STM_estimate$sig <- ifelse(STM_estimate$topic==2|STM_estimate$topic==5,
                           "Yes","No")
ggplot(data=STM_estimate,aes(x=label,y=estimate,fill=sig))+
  geom_bar(stat='identity')+
  scale_y_continuous(breaks=0.1*(0:5))+
  scale_fill_manual(values=c("grey80","grey30"))+
  labs(x=" ",y="Estimated prevalence",fill="Statistically signficant?")+
  ggtitle("Estimated results, STM")+
  facet_wrap(~return.kor,ncol=2)+
  theme_bw()+
  theme(legend.position="bottom")
# setwd("D:/data/Fig"); ggsave("Figure_part3_ch02_10.jpg",width=22,height=12,unit='cm')



# setwd("D:/data/code")

#한국어논문 STM 
#단독저자 혹은 주저자 여부를 표현하는 화일 
mytxtdf <- read.csv("author_korean_papers.csv")
head(mytxtdf)
#논문의 초록을 문자형 자료로 입력하였다. 
mytxtdf$abstract <- NA
for (i in 1:dim(mytxtdf)[1]) {
  mytxtdf$abstract[i] <- as.character(str_c(corpus.k.pp[[i]]$content,collapse=" "))
}
summary(mytxtdf)

#텍스트의 사전처리는 tm 패키지 이용하였기에 별도 지정하지 않음 
mypreprocess <- textProcessor(mytxtdf$abstract,metadata=mytxtdf,
                              lowercase=FALSE,removestopwords=FALSE,
                              removenumbers=FALSE,removepunctuation=FALSE,
                              stem=FALSE)
#DTM 구성함 
myout <- prepDocuments(mypreprocess$documents, 
                       mypreprocess$vocab, mypreprocess$meta,
                       lower.thresh=0)
#STM 추정 (7개의 토픽)
mystm <- stm(myout$documents, myout$vocab, K=7, 
             prevalence =~ main_author, data=myout$meta,
             seed=20191120,init.type="Spectral")

#각 토픽이 발현되는 단어들 점검
labelTopics(mystm,topics=1:7,n=20)
#순서대로 다음과 같이 이름을 붙여보자. 
#Topic 1=조사방법 연구
#Topic 2=수용자 연구
#Topic 3=저널리즘 연구
#Topic 4=체감안전도 연구
#Topic 5=공동체 연구
#Topic 6=데이터마이닝 활용연구
#Topic 7=캠페인 연구

#메타데이터와 토픽발현가능성의 관계 테스트
set.seed(20191121)
myresult <- estimateEffect(c(1:7) ~ main_author,
                           mystm,mytxtdf)
summary(myresult)

#모형추정결과 시각화 
STM_estimate <- extract.estimateEffect(x=myresult,
                                       covariate = "main_author",
                                       method = "pointestimate",
                                       model = mystm)
STM_estimate$label <- factor(STM_estimate$topic,
                             labels=c("조사방법 연구\n",
                                      "수용자 연구\n",
                                      "저널리즘 연구\n",
                                      "체감안전도 연구\n",
                                      "공동체 연구\n",
                                      "데이터마이닝 활용연구\n",
                                      "캠페인 연구\n"))
STM_estimate$main_author <- factor(STM_estimate$covariate.value,
                                  labels=c("공저자/교신저자",
                                           "주저자/단독저자"))
STM_estimate$sig <- ifelse(STM_estimate$topic==4|STM_estimate$topic==5,
                           "유의함","유의하지 않음")
ggplot(data=STM_estimate,aes(x=label,y=estimate,fill=sig))+
  geom_bar(stat='identity')+
  scale_y_continuous(breaks=0.1*(0:5))+
  scale_fill_manual(values=c("grey80","grey30"))+
  coord_flip()+
  labs(x=" ",y="잠재토픽 발현가능성",fill="통계적 유의도")+
  ggtitle("STM 추정결과 ")+
  facet_wrap(~main_author,ncol=2)+
  theme_bw()+
  theme(legend.position="bottom")
# setwd("D:/data/Fig"); ggsave("Figure_part3_ch02_11.jpg",width=22,height=12,unit='cm')


# setwd("D:/data/code")
##BTM: 짧은 텍스트를 대상으로 한 토픽모형 
library('tidytext')   #unnest_token() 함수 포함 
library("tidyverse")  #stringr, ggplot2 패키지를 별도 구동할 필요없음
library("SnowballC")  #어근동일화 함수 wordStem() 사용을 위함 
#일반적으로는 다음과 같은 명령문 
mytitle <- read.csv("title_english_papers.csv")
#텍스트 데이터를 요인형이 아닌 문자형으로 변환 
mytitle$title <- as.character(mytitle$title)
summary(mytitle)
#타이디버스 접근 %>% 이용 
mytitle <- read.csv("title_english_papers.csv") %>%
  mutate(title = as.character(title))
summary(mytitle)
#타이디텍스트 접근 
#우선 1, 2단계만 
DF_btm12 <- mytitle %>% 
  mutate(title=str_remove_all(title,"[[:digit:]]{1,}"), #숫자제거
         title=str_remove_all(title,"[[:punct:]]{1,}")) #문장기호제거
head(DF_btm12, 3)
#3단계 추가 
DF_btm123 <- mytitle %>% 
  mutate(title=str_remove_all(title,"[[:digit:]]{1,}"), #숫자제거
         title=str_remove_all(title,"[[:punct:]]{1,}")) %>% #문장기호제거
  unnest_tokens(word, title, to_lower=TRUE) 
head(DF_btm123,15)
#4단계 추가 
DF_btm1234 <- mytitle %>% 
  mutate(title=str_remove_all(title,"[[:digit:]]{1,}"), #숫자제거
         title=str_remove_all(title,"[[:punct:]]{1,}")) %>% #문장기호제거
  unnest_tokens(word, title, to_lower=TRUE) %>% #대소문자 통일
  anti_join(stop_words,by='word')  #불용단어 제거 
head(DF_btm1234, 15)
#1-5단계 통합 
DF_btm <- mytitle %>% 
  mutate(title=str_remove_all(title,"[[:digit:]]{1,}"), #숫자제거
         title=str_remove_all(title,"[[:punct:]]{1,}")) %>% #문장기호제거
  unnest_tokens(word, title, to_lower=TRUE) %>% #대소문자 통일
  anti_join(stop_words,by='word') %>%           #불용단어 제거 
  mutate(word = wordStem(word))                 #어근동일화 
head(DF_btm1234, 15)
#문서개수
length(table(DF_btm$id))
#총단어수 
sum(table(DF_btm$word))
#고유단어수 
length(table(DF_btm$word))
#등장빈도 중위수, 최솟값, 최댓값
median(table(DF_btm$word))
min(table(DF_btm$word)) 
max(table(DF_btm$word))

#BTM 실시 
library("BTM")
set.seed(20191122) #저자와 동일한 결과를 얻길 원한다면 
btm.out <- BTM(DF_btm,k=7)
btm.out
#각 토픽별 단어들 목록 점검  
btm_terms <- terms(btm.out, top_n=20)
btm_terms #너무 길어 보기 좋지 않음 
#정리된 결과는 아래와 같음 
btm_terms_summary <- data.frame(matrix(NA,nrow=20,ncol=7))
for (i in 1:7){
  btm_terms_summary[,i] <- btm_terms[[i]][,1]
}
colnames(btm_terms_summary) <- paste0("btm_",1:7)
btm_terms_summary

#각 토픽의 라벨을 다음과 같이 선정함. 
#Topic 1: Privacy studies  
#Topic 2: Stereotype in communication 
#Topic 3: Health communication 
#Topic 4: Election studies 
#Topic 5: Interculture communication 
#Topic 6: SNS in communication 
#Topic 7: Political communication 

#문서별 잠재토픽 등장확률 
topic.by.year <- predict(btm.out,DF_btm)
round(topic.by.year,2)
#시각화 
topic.by.year <- data.frame(topic.by.year)
topic.by.year$pubyear <- rownames(topic.by.year)
topic.by.year$pubyear <- as.numeric(str_extract(topic.by.year$pubyear,
                                                "[[:digit:]]{4}"))
topic.by.year <- aggregate(cbind(X1,X2,X3,X4,X5,X6,X7)~pubyear,
                           topic.by.year,sum)
fig_BTM_english <- reshape(topic.by.year, idvar = "pubyear", 
                          varying = list(2:8),
                          v.names = "X", direction = "long")
colnames(fig_BTM_english) <- c('year','topic_i','score')
fig_BTM_english$topic <- factor(fig_BTM_english$topic_i,
                                labels=c("Privacy\nstudies\n",
                                         "Stereotype\nin comm.\n",
                                         "Health\ncomm\n",
                                         "Election\nstudies\n",
                                         "Inter-culture\ncomm\n",
                                         "SNS\nin comm\n",
                                         "Political\ncomm.\n"))
#토픽의 알파벳 순서로 정렬 
fig_BTM_english$topic <- as.character(fig_BTM_english$topic)

#시간에 따른 변화
ggplot(data=fig_BTM_english, aes(x=year, y=score, fill=topic)) + 
  geom_bar(stat="identity")+
  scale_x_continuous(breaks=2004:2019,labels=2004:2019)+
  scale_y_continuous(breaks=2*(0:5),labels=2*(0:5))+
  scale_fill_manual(values=1:7)+
  labs(x="publication year",y="score",fill='latent topic')+
  ggtitle("BTM: Titles of English journal papers")+
  theme_classic() 
# setwd("D:/data/Fig"); ggsave("Figure_part3_ch02_12.jpg",width=18,height=14,unit='cm')
