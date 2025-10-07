## 잠재적 디리클레 할당(LDA)
library(tm)
library(stringr)
library(ggplot2)
library(topicmodels)

# 영어/한국어 말뭉치 불러오기
corpus.e.pp <- readRDS("./2. 텍스트 데이터 사전처리/CorpusE_preprocess.RData")
corpus.k.pp <- readRDS("./2. 텍스트 데이터 사전처리/CorpusK_preprocess.RData")

# 영어/한국어 DTM 불러오기
dtm.e <- readRDS("./2. 텍스트 데이터 사전처리/dtmE.RData")
dtm.k <- readRDS("./2. 텍스트 데이터 사전처리/dtmK.RData")

lda.out <- LDA(dtm.e, k = 7, control = list(seed = 20191116))
lda.out
dim(lda.out@gamma) # 문서x토픽 행렬
dim(lda.out@beta)  # 토픽x단어 행렬
terms(lda.out, 20)  # 토픽x단어 행렬 상위 20개

# 각 문서가 담고있는 잠재토픽의 확률점수를 계산
posterior_lda <- posterior(lda.out)
round(posterior_lda$topics, 3)

lda.topics <- data.frame(posterior_lda$topics)
apply(lda.topics, 1, sum)
apply(lda.topics, 2, sum)  # 전체 말뭉치에서 가장 자주 등장하는 잠재토픽

# 연도별 잠재토픽 변화
tempyear <- rownames(lda.topics)
pubyear <- as.numeric(unlist(str_extract_all(tempyear, "[[:digit:]]{4}")))
topic.by.year <- round(aggregate(as.matrix(lda.topics) ~ pubyear, lda.topics, sum), 5)
topic.by.year

# 시간에 따른 변화 확인
fig_LDA_english <- reshape(topic.by.year, idvar = "pubyear",
                           varying = list(2:8),
                           v.names = "X", direction = "long")
colnames(fig_LDA_english) <- c("year", "topic_i", "score")
fig_LDA_english

# 각 토픽별 이름
fig_LDA_english$topic <- factor(fig_LDA_english$topic, labels = c("Election\nstudies\n", "Stereotype\nin comm\n",
                                                            "Political\ncomm\n", "Privacy\nstudies\n",
                                                            "SNS\nin comm\n", "Health\ncomm\n", "Inter-culture\ncomm\n"))
fig_LDA_english
fig_LDA_english$topic <- as.character(fig_LDA_english$topic)
head(fig_LDA_english)

ggplot(data = fig_LDA_english, aes(x = year, y = score, fill = topic)) +
  geom_bar(stat = "identity") +
  scale_x_continuous(breaks = 2009:2019, labels = 2009:2019) +
  scale_y_continuous(breaks = 2*(0:5), labels = 2*(0:5)) +
  scale_fill_manual(values = 1:7) +
  labs(x = "Publication Year", y = "Score", fill = "Latent Topic") +
  ggtitle("LDA: English Journal Papers Corpus") +
  theme_classic()

# alpha 값이 LDA 결과에 미치는 효과
# alpha = 10
round(posterior(LDA(dtm.e, k = 7, control = list(seed = 20191116, alpha = 10)))$topics, 3)

# alpha = 100
round(posterior(LDA(dtm.e, k = 7, control = list(seed = 20191116, alpha = 100)))$topics, 3)

# alpha값이 작을수록 특정 문서에서 특정 토픽이 주도적으로 드러나는 결과를 얻을 수 있고, alpha값이 증가할수록 특정 문서가 특정 토픽을 강하게 드러낼 확률이 줄어드는 결과를 얻을 수 있다.

## 상관토픽모형(CTM)
# 잠재토픽들 사이의 상관관계 발생도 추정
# k차원의 디리클레 랜덤 분포 추출을 위해 로지스틱 정규분포를 사용
ctm.out <- CTM(dtm.e, k = 7, control = list(seed = 20191119))
terms(ctm.out, 20)  # 상위 20개 단어

# 각 문서가 담고있는 잠재토픽의 확률점수를 계산
posterior_ctm <- posterior(ctm.out)
ctm.topics <- data.frame(posterior_ctm$topics)
round(ctm.topics, 3)

# 연도별 잠재토픽 변화
tempyear <- rownames(ctm.topics)
pubyear <- as.numeric(unlist(str_extract_all(tempyear, "[[:digit:]]{4}")))
topic.by.year <- round(aggregate(as.matrix(ctm.topics) ~ pubyear, ctm.topics, sum), 5)
topic.by.year

# 시간에 따른 변화 확인
fig_CTM_english <- reshape(topic.by.year, idvar = "pubyear",
                           varying = list(2:8),
                           v.names = "X", direction = "long")
colnames(fig_CTM_english) <- c("year", "topic_i", "score")
fig_CTM_english

# 각 토픽별 이름
fig_CTM_english$topic <- factor(fig_CTM_english$topic, labels = c("SNS\nin comm\n", "Inter-culture\ncomm\n",
                                                                  "Election\nstudies\n", "Health\ncomm\n",
                                                                  "Stereotype\nin comm\n", "Political\ncomm\n",
                                                                  "Privacy\nstudies\n"))
fig_CTM_english
fig_CTM_english$topic <- as.character(fig_CTM_english$topic)
head(fig_CTM_english)

ggplot(data = fig_CTM_english, aes(x = year, y = score, fill = topic)) +
  geom_bar(stat = "identity") +
  scale_x_continuous(breaks = 2009:2019, labels = 2009:2019) +
  scale_y_continuous(breaks = 2*(0:5), labels = 2*(0:5)) +
  scale_fill_manual(values = 1:7) +
  labs(x = "Publication Year", y = "Score", fill = "Latent Topic") +
  ggtitle("CTM: English Journal Papers Corpus") +
  theme_classic()

# LDA와 CTM 결과 비교
lda_topic <- factor(as.vector(topics(lda.out)), labels = c("Election studies", "Stereotype in comm", "Political comm", "Privacy studies", "SNS in comm", "Health comm", "Inter-culture comm"))

ctm_topic <- factor(as.vector(topics(ctm.out)), labels = c("SNS in comm", "Inter-culture comm", "Election studies", "Health comm", "Stereotype in comm", "Political comm", "Privacy studies"))

LDA_CTM <- data.frame(as.character(lda_topic), as.character(ctm_topic))
colnames(LDA_CTM) <- c("lbl_LDA", "lbl_CTM")
LDA_CTM

# 토픽 분포비교
distr_topic <- rbind(prop.table(table(LDA_CTM$lbl_LDA)), prop.table(table(LDA_CTM$lbl_CTM)))
distr_topic

distr_topic <- data.frame(distr_topic)
distr_topic$model <- c("LDA", "CTM")
distr_topic

# 시각화
fig_LDA_CTM <- reshape(distr_topic, idvar = "model", varying = list(1:7), v.names = "prop", direction = "long")
fig_LDA_CTM

colnames(fig_LDA_CTM) <- c("model", "topic_i", "prop")
fig_LDA_CTM

fig_LDA_CTM$topic <- factor(fig_LDA_CTM$topic_i, labels = colnames(distr_topic)[1:7])
fig_LDA_CTM

ggplot(data = fig_LDA_CTM, aes(x = topic, y = prop, fill = model)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_y_continuous(breaks = 0.1*(0:3)) +
  coord_flip() +
  labs(x = "Sementic labels for 7 Topics", y = "Proportion", fill = "Topic Model") +
  ggtitle("Comparison between LDA Model and CTM") +
  theme_bw()

# 토픽이 일치하는 사례
table(substr(LDA_CTM$lbl_LDA, 1, 8), substr(LDA_CTM$lbl_CTM, 1, 8))
table(LDA_CTM$lbl_LDA == LDA_CTM$lbl_CTM)
prop.table(table(LDA_CTM$lbl_LDA == LDA_CTM$lbl_CTM))

## 구조적 토픽모형(STM)
# 토픽모델 적합도 검증 방법
# 1. 지속가능성 추정
# 2. 토픽모델 잔차 과분포 검정
# 3. 사후추정 순열검증
library(stm)
dim(gadarian)
colnames(gadarian)
head(gadarian)
summary(gadarian)

# 1. 텍스트 데이터 준비단계
# 말뭉치 데이터를 gadarian와 비슷한 방식으로 구성해야 함.
mytxtdf <- data.frame(dtm.e$dimnames$Docs)
colnames(mytxtdf) <- "filename"

mytxtdf$year <- as.numeric(unlist(str_extract_all(mytxtdf$filename, "[[:digit:]]{4}")))
head(mytxtdf)

# 필자가 미국에 체류할 때 0, 귀국했을 때 1
mytxtdf$return.kor <- ifelse(mytxtdf$year > 2011, 1, 0)

# 논문의 초록을 문자형으로 입력
mytxtdf$abstract <- NA
for (i in 1:dim(mytxtdf)[1]){
  mytxtdf$abstract[i] <- as.character(str_c(corpus.e.pp[[i]]$content, collapse = " "))
}
summary(mytxtdf)

# 2. 사전처리(preprocessing)
# 이미 사전처리되었기 때문에 별도 전처리 기능 비활성화함.
mypreprocess <- textProcessor(mytxtdf$abstract, metadata = mytxtdf, 
                              lowercase = F, removestopwords = F, removenumbers = F, removepunctuation = F,
                              stem = F)
mypreprocess$documents[1]  # 첫 번째 초록의 단어 명목변수값과 빈도 수

# 3. DTM 구성
myout <- prepDocuments(documents = mypreprocess$documents,
                       vocab = mypreprocess$vocab,
                       meta = mypreprocess$meta,
                       lower.thresh = 0)  # 제거 단어 빈도 수 임계값(즉, 모든 단어 포함)

# 4. STM 추정
mystm <- stm(documents = myout$documents,
             vocab = myout$vocab, K = 7,
             prevalence =  ~ return.kor,  # 잠재토픽 발현가능성 비교(귀국 전후)
             data = myout$meta,
             seed = 20191120,
             init.type = "Spectral")  # STM 개발자들이 권장하는 기법(공출현 단어 분해시 비음수 행렬 가정하기 때문)

# 5. 토픽별 단어 확인
labelTopics(model = mystm, topics = 1:7)

# 6. STM 토픽 간의 상관관계
mystm.corr <- topicCorr(mystm)
mystm.corr

# 7. 토픽 간 연관 네트워크 시각화
library(igraph)
plot(mystm.corr)  # 음의 상관관계이므로 연결되지 않음.

# 8. 메타데이터와 토픽 발현 가능성 관계 테스트
set.seed(20191121)
myresult <- estimateEffect(c(1:7) ~ return.kor,  # 귀국 전후로 7개 토픽 발현가능성 통계적 검증
                           stmobj = mystm, 
                           metadata = mytxtdf)
summary(myresult)

# 9. 모형추정 결과 시각화
par(mfrow = c(2,1))
plot(myresult, covariate = "return.kor",
     topics = 3, model = mystm, xlim = c(-1.5, 1.5),
     main = "Topic 3, p < .05")

plot(myresult, covariate = "return.kor",
     topics = 5, model = mystm, xlim = c(-1.5, 1.5),
     main = "Topic 5, p < .05")

# tidtstm을 쓰면 훨씬 효율적
#devtools::install_github("mikaelpoul/tidystm")
library(tidystm)
STM_estimate <- extract.estimateEffect(x = myresult,
                                       covariate = "return.kor",
                                       method = "pointestimate",
                                       model = mystm)
STM_estimate$label <- factor(STM_estimate$topic,
                             labels = c("SNS\nin comm", "Election\nstudies", "Stereotype\nin comm",
                                        "Privacy\nstudies", "Political\ncomm", "Health\ncomm", "Inter-culture\ncomm"))
STM_estimate$return.kor <- factor(STM_estimate$covariate.value,
                                  labels = c("In USA\n(Before 2011)", "In Korea\n(Since 2012)"))
STM_estimate$sig <- ifelse(STM_estimate$topic == 3 | STM_estimate$topic == 5, "Yes", "No")

ggplot(data = STM_estimate, aes(x = label, y = estimate, fill = sig)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(breaks = 0.1 * (0:5)) +
  scale_fill_manual(values = c("grey80", "grey30")) +
  labs(x = " ", y = "Estimated prevalence", fill = "Statistically significant?") +
  ggtitle("Estimated results, STM") +
  facet_wrap(~ return.kor, ncol = 2) +
  theme_bw() +
  theme(legend.position = "bottom")

## 공통등장단어 토픽모형(BTM)
# 단어들의 등장 순서를 고려하지 않음 (BoW 기반).
# tidy text 형태 (엣지 리스트 형태)
library(tidytext)
library(tidyverse)
library(SnowballC)

mytitle <- read.csv("./data/title_english_papers.csv")
mytitle$title <- as.character(mytitle$title)
str(mytitle)
#head(mytitle)
summary(mytitle)

# BTM 함수에 사용할 수 있는 양식으로 사전처리
# 1. 텍스트에 숫자 제거, 2. 문장부호, 특수문자 제거
DF_btm12 <- mytitle %>% 
  mutate(title = str_remove_all(title, "[[:digit:]]{1,}"),  # 숫자 제거
         title = str_remove_all(title, "[[:punct:]]{1,}"))  # 문장부호 제거

head(DF_btm12, 3)

# 3. 알파벳 소문자 변환
DF_btm123 <- mytitle %>% 
  mutate(title = str_remove_all(title, "[[:digit:]]{1,}"),      # 숫자 제거
         title = str_remove_all(title, "[[:punct:]]{1,}")) %>%  # 문장부호 제거
  unnest_tokens(input = title,  # 토큰화할 텍스트
                output = word,  # 출력변수 명
                to_lower = T)   # 소문자 변환
head(DF_btm123)

# 4. 불용어 제거
DF_btm1234 <- mytitle %>% 
  mutate(title = str_remove_all(title, "[[:digit:]]{1,}"),      # 숫자 제거
         title = str_remove_all(title, "[[:punct:]]{1,}")) %>%  # 문장부호 제거
  unnest_tokens(input = title,  # 토큰화할 텍스트
                output = word,  # 출력변수 명
                to_lower = T) %>%    # 소문자 변환
  anti_join(stop_words, by = "word")  # 불용어 제거
head(DF_btm1234)

# 5. 어근 동일화(stem)
DF_btm <- mytitle %>% 
  mutate(title = str_remove_all(title, "[[:digit:]]{1,}"),      # 숫자 제거
         title = str_remove_all(title, "[[:punct:]]{1,}")) %>%  # 문장부호 제거
  unnest_tokens(input = title,  # 토큰화할 텍스트
                output = word,  # 출력변수 명
                to_lower = T) %>%    # 소문자 변환
  anti_join(stop_words, by = "word") %>%   # 불용어 제거
  mutate(word = wordStem(word))  # 어근 동일화
head(DF_btm)

length(DF_btm$id)  # 문서 수
sum(table(DF_btm$word))  # 총 단어 수
length(table(DF_btm$word))  # 고유 단어 수
median(table(DF_btm$word))  # 등장 빈도 중위수
min(table(DF_btm$word))  # 등장 빈도 최솟값
max(table(DF_btm$word))  # 등장 빈도 최빈값

# BTM 실시
library(BTM)
set.seed(20191122)
btm.out <- BTM(data = DF_btm, k = 7)
btm.out

# 각 토픽별 단어 목록
btm_terms <- terms(btm.out, top_n = 20)
btm_terms

btm_terms_summary <- data.frame(matrix(NA, nrow = 20, ncol = 7))
for (i in 1:7){
  btm_terms_summary[,i] <- btm_terms[[i]][, 1]
}
colnames(btm_terms_summary) <- paste0("btm_", 1:7)
btm_terms_summary

# 문서별 잠재토픽 등장 확률
topic.by.year <- predict(btm.out,  # BTM 모델 
                         DF_btm)   # BTM 모델을 적용할 텍스트 데이터
round(topic.by.year, 2)

# 시각화
topic.by.year <- data.frame(topic.by.year)
topic.by.year$pubyear <- rownames(topic.by.year)
topic.by.year$pubyear <- as.numeric(str_extract(topic.by.year$pubyear, "[[:digit:]]{4}"))

topic.by.year <- aggregate(cbind(X1, X2, X3, X4, X5, X6, X7) ~ pubyear, topic.by.year, sum)

fig_BTM_english <- reshape(topic.by.year, idvar = "pubyear",
                           varying = list(2:8),
                           v.names = "X", direction = "long")
colnames(fig_BTM_english) <- c("year", "topic_i", "score")
fig_BTM_english$topic <- factor(fig_BTM_english$topic_i,
                                labels = c("Privacy\nstudies", "Stereotype\nin comm", "Health\ncomm", 
                                           "Election\nstudies", "Inter-culture\ncomm", "SNS\nin comm\n", 
                                           "Political\ncomm"))

fig_BTM_english$topic <- as.character(fig_BTM_english$topic)  # 토픽 알파벳 순으로 정렬
ggplot(data = fig_BTM_english, aes(x = year, y = score, fill = topic)) +
  geom_bar(stat = "identity") +
  scale_x_continuous(breaks = 2004:2019, labels = 2004:2019) +
  scale_y_continuous(breaks = 2*(0:5), labels = 2*(0:5)) +
  scale_fill_manual(values = cluster.color) +
  labs(x = "Publication Year", y = "Score", fill = "latent topic") +
  ggtitle("BTM : Titles of English Journal Papers") +
  theme_classic()
