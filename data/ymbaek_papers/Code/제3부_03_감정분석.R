########################################
#제 3부 텍스트 데이터 분석 및 결과제시##
#03 감정분석############################
########################################

#감정어휘 사전을 활용한 텍스트 감정분석 
library("tidytext")
library("textdata") #lexicon 다운로드 
library('tidyverse') #stringr, ggplot2 포함 

# setwd("D:/data/code")

#구약(히브리)성서의 <욥기>에 대해 감정분석을 실시해 보자. 
job <- readLines("Job_King_James_version.txt")
head(job,5)
#원텍스트의 줄수는 아래와 같다. 
length(job)
#하나의 텍스트로 묶어보자. 
job <- str_flatten(as.vector(job))
substr(job, 1, 400)
#장, 절을 구분한 표시를 기준으로 텍스트를 분리하자.(18은 욥기의 번호임)
job_text <- str_split(job, "18:[[:digit:]]{3}:[[:digit:]]{3}")[[1]]
#job_text 의 경우 처음은 공란임. 따라서 제거함. 
job_text[1]
job_text <- job_text[2:length(job_text)]

#장, 절을 나타낸 숫자를 뽑아내 보자. 
job_number <- str_extract_all(job,"18:[[:digit:]]{3}:[[:digit:]]{3}")[[1]]
#장(chapter), 절(verse)를 구분해 보자. 
job_chapter <- str_remove(str_extract(job_number,"18:[[:digit:]]{3}"),"18:")
job_verse <- str_remove(str_extract(job_number,"18:[[:digit:]]{3}:[[:digit:]]{3}"),"18:[[:digit:]]{3}:")

length(job_text);length(job_chapter);length(job_verse)

#욥기를 티블 데이터 형식으로 만들고, 대소문자 통일, 문장부호 제거, 불필요공란제거 실시 
tb_job <- tibble(chapter=job_chapter,verse=job_verse,text=job_text) 
tb_job
#사전처리 
tb_job <- tb_job %>% 
  mutate(chapter=as.numeric(chapter),   #장을 텍스트 형식에서 수치형으로 
         verse=as.numeric(verse),       #절을 텍스트 형식에서 수치형으로 
         text=tolower(text),            #대소문자 통일 
         text=str_remove_all(text,"[[:punct:]]{1,}"),  #문장부호 제거
         text=str_squish(text))        #불필요 공란제거 
tb_job
#단어 단위의 데이터로 변환한 후 불용단어 제거 
tb_job_word <- tb_job %>% 
  unnest_tokens(word,text,"words") %>% 
  anti_join(stop_words, by="word")
tb_job_word
#lexicon을 이용 데이터 병합 
#Affin lexicon 경우 
affin_lexicon <- lexicon_afinn()
affin_lexicon
affin_lexicon %>% count(value) %>% 
  mutate(type=ifelse(value<0,"Negative","Positive")) %>% 
  ggplot(aes(x=value,y=n,fill=type))+
  geom_bar(stat='identity')+
  labs(x="Sentiment value (positive score for positive sentiment), Affin lexicon",
       y="Frequency",fill="Direction of sentiment")+
  coord_cartesian(xlim=-6:6)+
  scale_x_continuous(breaks=-6:6)+
  scale_y_continuous(breaks=100*0:10)+
  theme_classic()+
  theme(legend.position="bottom")
# setwd("D:/data/Fig"); ggsave("Figure_part3_ch03_01.jpg",width=16,height=14,unit='cm')

#opinion lexicon 경우 
bing_lexicon <- lexicon_bing()
bing_lexicon
table(bing_lexicon$sentiment)
prop.table(table(bing_lexicon$sentiment))

#NRC lexicon 경우 
NRC_lexicon <- lexicon_nrc()
NRC_lexicon

NRC_lexicon %>% 
  count(sentiment) %>% 
  mutate(type = ifelse(sentiment=="positive"|sentiment=="joy"|sentiment=="trust"|
                         sentiment=="surprise"|sentiment=="anticipation",
                       "Broadly positive","Broadly negative"),
         sentiment = fct_reorder(sentiment,n)
  ) %>% 
  ggplot(aes(x=sentiment,y=n,fill=type))+
  geom_bar(stat='identity')+
  coord_flip()+
  scale_y_continuous(breaks=200*(0:17))+
  labs(x="Sentiment category, NRC lexicon",y="frequency",fill="Broad category")+
  theme_bw()
# setwd("D:/data/Fig"); ggsave("Figure_part3_ch03_02.jpg",width=22,height=15,unit='cm')

#AFFIN이용 감정분석 
tb_job_afinn <- tb_job_word %>% 
  inner_join(affin_lexicon, by="word")
tb_job_afinn
#장별 총 단어수 계산 
tb_job_totalword <- tb_job_word %>% group_by(chapter) %>% 
  mutate(constant=1) %>%
  summarize(total_word=sum(constant))
#감정점수를 "장"별로 집산 
affin_sentiment_chapter <- tb_job_afinn %>% 
  group_by(chapter) %>%    #장들로 데이터를 나눈 후 
  summarize(sentiment_affin = sum(value))  %>%  #각 장에서의 감정점수 합산 
  full_join(tb_job_totalword,by='chapter') %>% #장별 전체단어와 합산 
  mutate(prop = sentiment_affin/total_word)
affin_sentiment_chapter
#장별로 감정점수는 어떻게 바뀌는지 시각화 
ggplot(affin_sentiment_chapter, aes(x=chapter, y=prop)) + 
  geom_point(size=1,color='red')+
  geom_line(lty=2,color='blue')+
  scale_x_continuous(breaks=1:42)+
  labs(x="Chapters in Book of Job in Old Testament (Hebrew Bible)",
       y="sentiment score, averaged per word\n(Affin lexicon)")+
  theme_bw()+
  ggtitle("Sentiment analysis using Affin lexicon")
# setwd("D:/data/Fig"); ggsave("Figure_part3_ch03_03.jpg",width=18,height=14,unit='cm')


#opinion lexicon 이용 감정분석 
tb_job_bing <- tb_job_word %>% 
  inner_join(bing_lexicon, by="word")
tb_job_bing
#감정점수를 "장"별로 합계 집산 
bing_sentiment_chapter <- tb_job_bing %>% 
  mutate(N_pos = ifelse(sentiment=="positive",1,0),
         N_neg = ifelse(sentiment=="negative",1,0)) %>% 
  group_by(chapter) %>% 
  summarize(positive = sum(N_pos),
            negative = sum(N_neg)) %>% 
  full_join(tb_job_totalword,by='chapter') %>% #장별 전체단어와 합산 
  mutate(prop_positive = positive/total_word,
         prop_negative = negative/total_word) %>% 
  select(chapter,prop_positive,prop_negative) %>%  #단어범주별 비율 계산 
  pivot_longer(-chapter,names_to="type",values_to="prop") %>% #gather(type,cnt,-chapter) 동일 
  mutate(type=str_remove(type,"prop_"))
bing_sentiment_chapter
#opinion lexicon을 이용한 경우 
ggplot(bing_sentiment_chapter, aes(x=chapter,y=prop,shape=type,color=type)) + 
  geom_point(size=3)+
  geom_line(lty=2)+
  scale_x_continuous(breaks=1:42)+
  labs(x="Chapters in Book of Job in Old Testament (Hebrew Bible)",
       y="sentiment score, averaged count per word\n(Bing's opinion lexicon)",
       shape="Direction of sentiment",
       color="Direction of sentiment")+
  theme_classic()+
  theme(legend.position="bottom")+
  ggtitle("Sentiment analysis using Bing's opinion lexicon")
# setwd("D:/data/Fig"); ggsave("Figure_part3_ch03_04.jpg",width=18,height=14,unit='cm')


#EmoLex 이용 감정분석 
tb_job_NRC <- tb_job_word %>% 
  inner_join(NRC_lexicon, by="word")
tb_job_NRC
#감정점수를 "장"별로 합계 집산 
NRC_sentiment_chapter <- tb_job_NRC %>% 
  mutate(positive = ifelse(sentiment=="positive",1,0),
         negative = ifelse(sentiment=="negative",1,0),
         anger = ifelse(sentiment=="anger",1,0),
         anticipation = ifelse(sentiment=="anticipation",1,0),
         disgust = ifelse(sentiment=="disgust",1,0),
         fear = ifelse(sentiment=="fear",1,0),
         joy = ifelse(sentiment=="joy",1,0),
         sadness = ifelse(sentiment=="sadness",1,0),
         surprise = ifelse(sentiment=="surprise",1,0),
         trust = ifelse(sentiment=="trust",1,0)) %>% 
  group_by(chapter) %>% 
  summarize_at(
    vars(positive:trust),
    sum
    ) %>% 
  full_join(tb_job_totalword,by='chapter') %>% #장별 전체단어와 합산 
  mutate(prop_positive = positive/total_word,
         prop_negative = negative/total_word,
         prop_anger = anger/total_word,
         prop_anticipation = anticipation/total_word,
         prop_disgust = disgust/total_word,
         prop_fear = fear/total_word,
         prop_joy = joy/total_word,
         prop_sadness = sadness/total_word,
         prop_surprise = surprise/total_word,
         prop_trust = trust/total_word) %>% 
  select(chapter,prop_positive:prop_trust) %>%
  pivot_longer(-chapter,names_to="type",values_to="prop") %>% 
  mutate(type=str_remove(type,"prop_"))
NRC_sentiment_chapter


#Emolex lexicon을 이용한 경우: positive, negative 범주는 삭제
#감정유형은 부정적(anger, disgust,fear, sadness), 긍정적(anticipation, joy, surprise, trust)으로 구분 
NRC_sentiment_chapter2 <- NRC_sentiment_chapter %>% 
  filter(type!="positive"&type!="negative") %>% 
  mutate(upper_cate = ifelse(type=="anger"|type=="disgust"|type=="fear"|type=="sadness",
                             "Negative > ","Positive > "),
         type = str_c(upper_cate,type))
ggplot(NRC_sentiment_chapter2,aes(x=chapter,y=prop)) + 
  geom_point(size=2)+
  geom_line(lty=2)+
  scale_x_continuous(breaks=3*(1:14))+
  labs(x="Chapters in Book of Job in Old Testament (Hebrew Bible)",
       y="sentiment score, averaged per countsummed\n(NRC lexicon)")+
  facet_wrap(~type,ncol=2)+
  theme_bw()+
  theme(legend.position="bottom")+
  ggtitle("Sentiment analysis using NRC lexicon")
# setwd("D:/data/Fig"); ggsave("Figure_part3_ch03_05.jpg",width=16,height=25,unit='cm')

#NRC 감정강도(emotion intensity) lexicon
#각 감정카테고리별 강도 점수 부여(예를 들어 강한 anger감정, 약한 anger 감정)
#NRC VAD(방향, 흥분, 지배; valence, arousal, & dominance) lexicon
#V: 접근-회피 / A: 촉발된 감정의 강렬함 정도 / D: 감정으로 인해 행위자가 보이는 지배-복속 모습 

#Loughran-McDonald lexicon 
#재정과 관련된 이슈를 다루는 텍스트의 경우: 
#negative,positive,litigious,uncertainty,constraining,superfluous 범주제공 


#한국어 감정분석
#여러방법이 있겠으나 구글번역기를 사용하는 것이 가장 손쉬운 듯
#최서해의 <탈출기>를 대상으로: 원문은 novel_탈출기_최서해.txt
esc <- readLines("novel_escape_choi.txt")
head(esc,3)
length(esc)
#장의 구분을 명확하게 
esc <- ifelse(esc=="","__line_break__",esc)
esc <- str_flatten(esc," ")
substr(esc,1,200)
#장을 구분
esc <- str_split(esc, "__line_break__[[:print:]]{1,5}__line_break__")[[1]]
tb_esc <- tibble(text=esc) %>% 
  #텍스트 사전처리 
  mutate(text = str_remove_all(text,"One __line_break__|__line_break__"),
         text = str_squish(text),
         text = str_remove_all(text, "[[:punct:]]{1,}"),
         text = str_remove_all(text, "[[:digit:]]{1,}"),
         chapter = row_number()) %>%  #장번호 부여 
  #단어단위 분해 
  unnest_tokens(word, text, "words", to_lower=TRUE) %>% 
  #불용단어 제거 
  anti_join(stop_words,by="word") %>% 
  #장별 총 단어수 계산
  group_by(chapter) %>% 
  mutate(constant = 1, total_words = sum(constant)) %>% 
  #Emoles lexicon 
  inner_join(NRC_lexicon, by="word")
#장별로 감정점수 평균집산
NRC_sentiment_esc <- tb_esc %>% 
  mutate(positive = ifelse(sentiment=="positive",1,0),
         negative = ifelse(sentiment=="negative",1,0),
         anger = ifelse(sentiment=="anger",1,0),
         anticipation = ifelse(sentiment=="anticipation",1,0),
         disgust = ifelse(sentiment=="disgust",1,0),
         fear = ifelse(sentiment=="fear",1,0),
         joy = ifelse(sentiment=="joy",1,0),
         sadness = ifelse(sentiment=="sadness",1,0),
         surprise = ifelse(sentiment=="surprise",1,0),
         trust = ifelse(sentiment=="trust",1,0)) %>% 
  group_by(chapter) %>% 
  summarize_at(
    vars(positive:trust),
    sum
  ) %>% 
  pivot_longer(-chapter,names_to="type",values_to="cnt")
#장별 총단어수 데이터와 데이터 합치고, 각 범주별 비율계산 
NRC_sentiment_esc <- tb_esc %>% group_by(chapter) %>%
  summarize(total_words=mean(total_words)) %>% 
  full_join(NRC_sentiment_esc, by="chapter") %>% 
  mutate(prop = 100*(cnt/total_words))
NRC_sentiment_esc %>% print(n=20)
#positive / negative 범주를 하나의 데이터로 
#나머지 감정들을 다른 하나의 데이터로
#감정유형은 부정적(anger, disgust,fear, sadness), 긍정적(anticipation, joy, surprise, trust)으로 구분 
NRC_sentiment_esc1 <- NRC_sentiment_esc %>% 
  filter(type=="positive"|type=="negative") 
NRC_sentiment_esc2 <- NRC_sentiment_esc %>% 
  filter(type!="positive"&type!="negative") %>% 
  mutate(upper_cate = ifelse(type=="anger"|type=="disgust"|type=="fear"|type=="sadness",
                             "Negative > ","Positive > "),
         type = str_c(upper_cate,type))

#전반적 긍정, 전반적 부정 범주 시각화 
ggplot(NRC_sentiment_esc1,aes(x=chapter,y=prop,shape=type,color=type)) + 
  geom_point(size=4)+
  geom_line(lty=2)+
  scale_x_continuous(breaks=1:6,labels=str_c(1:6,"장"))+
  scale_y_continuous(breaks=5*(0:4),labels=str_c(5*(0:4),"%"))+
  coord_cartesian(ylim=c(0,20))+
  labs(x="최서해 <탈출기> 장 번호",
       y="해당 범주의 감정단어 비율",
       shape="감정범주",color="감정범주")+
  theme_bw()+
  theme(legend.position="bottom")+
  ggtitle("한국어 텍스트를 구글번역기로 영어 텍스트로 변환후 감정분석\n(EmoLex 감정어휘사전 이용)")
# setwd("D:/data/Fig"); ggsave("Figure_part3_ch03_06.jpg",width=14,height=14,unit='cm')

#나머지 8개 감정범주 시각화 
ggplot(NRC_sentiment_esc2,aes(x=chapter,y=prop)) + 
  geom_point(size=2)+
  geom_line(lty=2)+
  scale_x_continuous(breaks=1:6,labels=str_c(1:6,"장"))+
  scale_y_continuous(breaks=5*(0:5),labels=str_c(5*(0:5),"%"))+
  coord_cartesian(ylim=c(0,25))+
  labs(x="최서해 <탈출기> 장 번호",
       y="해당 범주의 감정단어 비율")+
  facet_wrap(~type,ncol=4)+
  theme_bw()+
  theme(legend.position="bottom")+
  ggtitle("한국어 텍스트를 구글번역기로 영어 텍스트로 변환후 감정분석\n(EmoLex 감정어휘사전 이용)")
# setwd("D:/data/Fig"); ggsave("Figure_part3_ch03_07.jpg",width=16,height=25,unit='cm')

#지도 기계학습을 이용한 감정분석
#제1판에서는 RTextTools를 사용했지만, 안타깝게도 해당 패키지 개발자들이 업데이트를 중지한 
#상태다. 만약 사용을 원한다면 CRAN depository에 업로드된 RTextTools_1.4.2.tar를 다운로드 받아 
#사용하면 되나, 버그가 있을 가능성이 높다. 
#아쉽지만 제2판에서는 caret 패키지를 실습해 보자. 
library("caret")
#https://github.com/victorneo/Twitter-Sentimental-Analysis
#에서 다운로드 받은 데이터를 사용   
setwd("D:/data/Twitter-Sentimental-Analysis-master/")
#각 데이터를 저장
h.train <- readLines("happy.txt")
s.train <- readLines("sad.txt")
h.test <- readLines("happy_test.txt")
s.test <- readLines("sad_test.txt")
#전체 데이터 생성 
Htrain <- tibble(text=h.train) %>% 
  mutate(id=str_c("train_happy_",row_number()))
Htrain
Strain <- tibble(text=s.train) %>% 
  mutate(id=str_c("train_sad_",row_number()))
Htest <- tibble(text=h.test) %>% 
  mutate(id=str_c("test_happy_",row_number()))
Stest <- tibble(text=s.test) %>% 
  mutate(id=str_c("test_sad_",row_number()))

#네 가지 데이터 합치기 
dt <- bind_rows(Htrain,Strain,Htest,Stest) %>% 
  mutate(
    type=str_remove_all(str_extract(id,"[[:alpha:]]{3,}_"),"_"),
    status=str_remove_all(str_extract(id,"_[[:alpha:]]{3,}_"),"_")
  )
dt
#타이디텍스트 맥락에서 텍스트 사전처리  
dt_pp <- dt %>% 
  #사전처리: 문장부호 제거, 숫자제거, 대소문자 통합, 불용문자제거, 불필요공란 제거, 어근동일화 
  mutate(
    text = str_remove_all(text, "[[:punct:]]{1,}"),
    text = str_remove_all(text, "[[:digit:]]{1,}"),
    text = str_squish(text)
  ) %>% 
  unnest_tokens(word,text,"words",to_lower=TRUE) %>% 
  anti_join(stop_words,by='word') %>% 
  mutate(word = SnowballC::wordStem(word))
dt_pp

#훈련데이터의 라벨 생성 
label_train <- factor(dt$status[dt$type=="train"])
#테스트 데이터의 라벨 생성 
label_test <- factor(dt$status[dt$type=="test"])

#훈련데이터의 DTM 생성
dt_train_dtm <- dt_pp %>% filter(type=="train") %>% 
  #문서*단어별 빈도 구하기 
  count(id, word) %>%
  #DTM 생성 
  cast_dtm(document=id, term=word, value=n)
dt_train_dtm
dt_train_dtm <- as.matrix(dt_train_dtm)
#테스트데이터에만 등장하는 단어는 배제된 테스트데이터 DTM 생성 
dt_test_dtm <- dt_pp %>% filter(type=="test") %>% 
  full_join(dt_pp %>% filter(type=="train") %>% select(word),by="word") %>%
  count(id, word) %>% 
  cast_dtm(document=id, term=word, value=n)
#21번째가  NA임에 주목 
rownames(dt_test_dtm) 
dt_test_dtm <- as.matrix(dt_test_dtm)[1:20,]

#Classification tree 
#기계훈련 
set.seed(20191121) #지정하지 않으면 결과가 조금 다를 수 있음
tree_train <- train(x = as.matrix(dt_train_dtm),
                    y = factor(label_train),
                    method="rpart")
pred_tree_train <- predict(tree_train)
table(pred_tree_train,label_train)
#테스트 데이터에 적용 
pred_tree_test <- predict(tree_train,newdata=dt_test_dtm)
table(pred_tree_test,label_test)  #happy를 기준범주로 

#각주: sad를 기준범주로 
table(fct_rev(pred_tree_test),fct_rev(label_test))  

confusionMatrix(table(pred_tree_test,label_test))
confusionMatrix(table(fct_rev(pred_tree_test),fct_rev(label_test)))

precision(table(pred_tree_test,label_test))
recall(table(pred_tree_test,label_test))
F_meas(table(pred_tree_test,label_test))
specificity(table(pred_tree_test,label_test))

#개인함수를 설정하였다. 
quality_index_classification <- function(mytable,myround){
  index_accuracy <- sum(diag(mytable))/sum(mytable)
  index_kappa <- kappa(mytable)$coef
  index_recall <- recall(mytable)
  index_precision <- precision(mytable)
  index_fscore <- F_meas(mytable)
  index_specificity <- specificity(mytable)
  index_name <- c("accuracy","kappa","precision","recall","f-score","specificity")
  index_score <- round(c(index_accuracy,index_kappa,
                         index_precision,index_recall,
                         index_fscore,index_specificity),3)
  data.frame(index_name,index_score)
}
#happy 기준범주 
tab_happy <- table(pred_tree_test,label_test)
quality_index_classification(tab_happy,3)
#sad 기준범주 
tab_sad <- table(fct_rev(pred_tree_test),fct_rev(label_test))
quality_index_classification(tab_sad,3)

#Random forest 
#기계훈련 
set.seed(20191121) #지정하지 않으면 결과가 조금 다를 수 있음
RF_train <- train(x = as.matrix(dt_train_dtm),
                  y = factor(label_train),
                  method = "ranger",   #random forrest 알고리즘 중 상당히 빠름 
                  num.trees = 200)
table(predict(RF_train), label_train)
#테스트 데이터에 적용 
pred_RF_test <- predict(RF_train,newdata=dt_test_dtm)
tab_happy <- table(pred_RF_test,label_test)
tab_happy
#happy 기준범주 
quality_index_classification(tab_happy,3)
#sad 기준범주 
tab_sad <- table(fct_rev(pred_RF_test),fct_rev(label_test))
quality_index_classification(tab_sad,3)

#Neural Network
#기계훈련 
set.seed(20191121) #지정하지 않으면 결과가 조금 다를 수 있음
NN_train <- train(x = as.matrix(dt_train_dtm),
                  y = factor(label_train),
                  method = "nnet")
table(predict(NN_train), label_train)
#테스트 데이터에 적용 
pred_NN_test <- predict(NN_train,newdata=dt_test_dtm)
tab_happy <- table(pred_NN_test,label_test)
tab_happy
#happy 기준범주 
quality_index_classification(tab_happy,3)
#sad 기준범주 
tab_sad <- table(fct_rev(pred_NN_test),fct_rev(label_test))
quality_index_classification(tab_sad,3)


