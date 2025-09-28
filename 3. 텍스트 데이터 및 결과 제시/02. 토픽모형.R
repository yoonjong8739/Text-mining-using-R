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

tempyear <- rownames(lda.topics)
pubyear <- as.numeric(unlist(str_extract_all(tempyear, "[[:digit:]]{4}")))
topic.by.year <- round(aggregate(as.matrix(lda.topics) ~ pubyear, lda.topics, sum), 5)
topic.by.year

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


## 구조적 토픽모형(STM)


## 공통등장단어 토픽모형(BTM)