## 빈도표 계산 및 말뭉치 등을 활용한 시각화
library("tm")
library("stringr")

# 영어/한국어 말뭉치 불러오기
corpus.e.pp <- readRDS("./2. 텍스트 데이터 사전처리/CorpusE_preprocess.RData")
corpus.k.pp <- readRDS("./2. 텍스트 데이터 사전처리/CorpusK_preprocess.RData")

# 영어/한국어 DTM 불러오기
dtm.e <- readRDS("./2. 텍스트 데이터 사전처리/dtmE.RData")
dtm.k <- readRDS("./2. 텍스트 데이터 사전처리/dtmK.RData")
word.freq <- apply(dtm.e[,], 2, sum)
length(word.freq); sum(word.freq)

# 빈도가 높은 상위 20개 단어
sort.word.freq <- sort(word.freq, decreasing = T)
head(sort.word.freq, 20)

# 누적 빈도
cumsum.word.freq <- cumsum(sort.word.freq)
head(sort.word.freq, 20)

prop.word.freq <- cumsum.word.freq / cumsum.word.freq[length(cumsum.word.freq)] # 누적 빈도 비율
head(prop.word.freq, 20)

library(ggplot2)
myfig <- data.frame(1:length(word.freq), prop.word.freq)
colnames(myfig) <- c("loc", "prop")

myfig2 <- myfig[round(83.9*(0:10)), ]
myfig2$lblloc <- paste(10*(1:10), "%th\nposition", sep = "")
ggplot(data = myfig) +
  geom_line(aes(x = loc, y = prop), color = "red") +
  geom_point(data = myfig2, aes(x = loc, y = prop), size = 2, color = "blue") +
  scale_x_continuous(breaks = myfig2$loc, labels = myfig2$lblloc) +
  scale_y_continuous(breaks = 0.2*(0:5), labels = paste(20*(0:5), "%", sep = "")) +
  labs(x = "\nOrder of word frequency", y = "Cumulative proportion\n") +
  theme_classic()

#install.packages("wordcloud2")
library("wordcloud2")
WC_data <- data.frame(names(word.freq), word.freq)
head(WC_data)
names(WC_data) <- c("word", "freq")

wordcloud2(WC_data, size = 1, fontFamily = "Times New Roman",
           shuffle = T, rotateRatio = 0.3)

wordcloud2(WC_data[WC_data$freq > 9, ], size = 1, fontFamily = "Times New Roman",
           shuffle = F, rotateRatio = 0.3, shape = "circle")

# 한국어 말뭉치 분석
word.freq_k <- apply(dtm.k[,], 2, sum)
sort.word.freq_k <- sort(word.freq_k, decreasing = T)
cumsum.word.freq_k <- cumsum(sort.word.freq_k)
head(cumsum.word.freq_k, 20)

prop.word.freq_k <- cumsum.word.freq_k / cumsum.word.freq_k[length(cumsum.word.freq_k)]
head(prop.word.freq_k, 20)

myfig_k <- data.frame(1:length(word.freq_k), prop.word.freq_k)
colnames(myfig_k) <- c("loc", "prop")
head(myfig_k, 20)

myfig2_k <- myfig_k[round(31.5*(0:10)), ]
head(myfig2_k, 20)
myfig2_k$lblloc <- paste(10*(1:10), "%th\nposition", sep = "")
ggplot(data = myfig_k, aes(x = loc, y = prop), color = "red") +
  geom_line() +
  geom_point(data = myfig2_k, aes(x = loc, y = prop), size = 2, color = "blue") +
  scale_x_continuous(breaks = myfig2_k$loc, labels = myfig2_k$lblloc) +
  scale_y_continuous(breaks = 0.2*(0:5), labels = paste(20*(0:5), "%", sep = "")) +
  labs(x = "\n단어 빈도 정렬 발생 위치", y = "누적 비율\n") +
  theme_classic()

WC_data_k <- data.frame(names(sort.word.freq_k), sort.word.freq_k)
names(WC_data_k) <- c("word", "freq")
wordcloud2(data = WC_data_k[WC_data_k$freq > 4, ],
           size = 0.7,
           shuffle = F,
           rotateRatio = 0.3,
           shape = "circle")

## 단어 간 혹은 문서 간 상관관계
findAssocs(dtm.e, "polit", 0.5)  # DTM에 속한 polit라는 단어와 0.5 이상의 피어슨 상관관계를 갖는 단어 목록

var1 <- as.vector(dtm.e[, "polit"])
var2 <- as.vector(dtm.e[, "elect"])
cor.test(var1, var2)

my.assoc.func <- function(mydtm, term1, term2){
  myvar1 <- as.vector(mydtm[, term1])
  myvar2 <- as.vector(mydtm[, term2])
  cor.test(myvar1, myvar2)
}
my.assoc.func(dtm.e, "polit", "elect")

my.assoc.func(t(dtm.e), "p2014a.txt", "p2019a.txt")  # 문서와 문서 간 상관계수
my.assoc.func(t(dtm.e), "p2014a.txt", "p2014g.txt")

# 해외 논문에 등장하는 단어들의 빈도 기반 논문 간의 단어 발현 유사도
length.doc <- length(rownames(dtm.e))
my.doc.cor <- matrix(NA, nrow = length.doc, ncol = length.doc)
for (i in 1:length.doc){
  for (j in 1:length.doc) {
    my.doc.cor[i,j] <- my.assoc.func(t(dtm.e), rownames(dtm.e)[i], rownames(dtm.e)[j])$est
  }
}
rownames(my.doc.cor) <- colnames(my.doc.cor) <- rownames(dtm.e)
diag(my.doc.cor) <- NA
round(my.doc.cor[1:4, 1:4], 2)
round(my.doc.cor[, 1], 2)

# 히스토그램
df_cor <- data.frame(my.doc.cor[lower.tri(my.doc.cor)])
names(df_cor) <- "cor_coef"
ggplot(data = df_cor, aes(x = cor_coef)) +
  geom_histogram(bins = 40) +
  labs(title = "Correlations between English journal papers",
       x = "Correlations", y = "Frequency") +
  theme_classic()

summary(df_cor)

# 국내 논문에 등장하는 단어들의 빈도 기반 논문 간의 단어 발현 유사도
length.doc_k <- length(rownames(dtm.k))
my.doc.cor_k <- matrix(NA, nrow = length.doc_k, ncol = length.doc_k)
for (i in 1:length.doc_k){
  for (j in 1:length.doc_k) {
    my.doc.cor_k[i,j] <- my.assoc.func(t(dtm.k), rownames(dtm.k)[i], rownames(dtm.k)[j])$est
  }
}
rownames(my.doc.cor_k) <- colnames(my.doc.cor_k) <- rownames(dtm.k)
diag(my.doc.cor_k) <- NA
round(my.doc.cor_k[1:4, 1:4], 2)
round(my.doc.cor_k[, 1], 2)

# 히스토그램
df_cor_k <- data.frame(my.doc.cor_k[lower.tri(my.doc.cor_k)])
names(df_cor_k) <- "cor_coef"
ggplot(data = df_cor_k, aes(x = cor_coef)) +
  geom_histogram(bins = 40) +
  labs(title = "한국어 논문 간 상관계수 분포",
       x = "상관계수", y = "빈도") +
  theme_classic()

summary(df_cor_k)

## 연관 규칙 분석
# 각 문서의 단어들 사이의 공통 발생 여부를 근거로 어떤 단어가 어떤 단어들과 연관되는 패턴을 추출하는 기법

# Apriori 알고리즘
# 지지도: 전체 문서(거래내역) 중 부분집합 X와 부분집합 Y를 모두 포함하는 문서의 비율을 의미.
# 확신도: 부분집합 X가 포함된 전체 문서들 중 부분집합 X와 Y를 모두 포함하는 문서의 비율을 의미.
# 향상도: 부분집합 X를 고려하지 않은 채 부분집합 Y를 포함한 문서의 비율과 부분집합 X가 등장할 때 부분집합 X와 부분집합 Y사 모두 등장한 문서의 비율을 의미.

d1 <- c("A", "B", "C")
d2 <- c("A", "C", "D", "E")
d3 <- c("B", "C", "D", "F")
d4 <- c("A", "B", "C", "D")

myDs <- list(d1, d2, d3, d4)
myDs

#install.packages("arules")
library("arules")
myDs <- as(myDs, "transactions")  # arules용 transactions 객체로 변환
myDs
#inspect <- arules::inspect
inspect(myDs)

myresult <- apriori(data = myDs)  # 가능한 모든 연관 규칙 추출
inspect(myresult)

# 영문 학술논문 말뭉치 데이터로 단어들의 연관 규칙 추출
# 문서의 개수만큼 벡터 생성
words_appear <- rep(NA, length(corpus.e.pp))

# 각 문서별로 등장 단어 목록 정리
for (i in 1:length(corpus.e.pp)){
  my_appear_words <- names(table(unlist(str_split(corpus.e.pp[[i]]$content, " "))))
  words_appear[i] <- str_c(my_appear_words, collapse = " ")
}

# 정리된 등장 단어 목록들을 리스트로 묶음
data_AR <- str_split(words_appear, " ", length(colnames(dtm.e)))
data_AR

data_AR <- as(data_AR, "transactions")
inspect(data_AR)

# 지지도 >= 0.3, 확신도 >= 0.5
trial_01 <- apriori(data = data_AR,
                    parameter = list(support = 0.3, confidence = 0.5))
inspect(head(sort(trial_01, by = "support"), 10))
inspect(head(sort(trial_01, by = "confidence"), 10))
inspect(head(sort(trial_01, by = "lift"), 10))

# 지지도 >= 0.2, 확신도 >= 0.5, network 단어와 주로 같이 등장하는 단어
trial_02 <- apriori(data = data_AR,
                    parameter = list(support = 0.2, confidence = 0.5),
                    appearance = list(rhs = "network"))
inspect(trial_02)

# 지지도 >= 0.1, 확신도 >= 0.5
# studi, result, effect, examin을 제외하고 polit, elect, particip 표현 등장 시 등장 단어는?
trial_03 <- apriori(data = data_AR,
                    parameter = list(support = 0.1, confidence = 0.5),
                    appearance = list(lhs = c("polit", "elect", "particip"),
                                      none = c("studi", "result", "effect", "examin")))
inspect(head(sort(trial_03, by = "lift"), 10))

#install.packages("arulesViz")
library(arulesViz)
plot(myresult)
plot(myresult, method="graph", control = list(type = "items"))

plot(trial_01)
plot(trial_01, method="graph", control = list(type = "items"))

plot(trial_02)
plot(trial_02, method="graph", control = list(type = "items"))

plot(trial_03)
plot(trial_03, method="graph", control = list(type = "items"))

inspect <- tm::inspect

## 단어 혹은 문서 간 유사도 행렬을 이용한 위계적 군집분석
library(factoextra)
dist.dtm.e <- dist(dtm.e)
as.matrix(dist.dtm.e)[1:4, 1:4]

myclusters <- hclust(dist.dtm.e, method = "ward.D2")
fviz_dend(myclusters)

myclusters$labels <- str_extract(myclusters$labels, "[[:digit:]]{4}[[:alpha:]]{1}")

mygroup <- cutree(myclusters, k = 13)
table(mygroup)
mygroup

fviz_dend(myclusters, k=13)

mytable <- table(str_extract(myclusters$labels, "[[:digit:]]*"), 
                 cutree(myclusters, k=13))
mytable

cluster.by.year <- data.frame(mytable)
colnames(cluster.by.year) <- c("year", "cluster", "publish")

cluster.by.year$cluster <- paste("cluster", cluster.by.year$cluster, sep = "")
cluster.by.year$year <- as.numeric(as.character(cluster.by.year$year))

cluster.color <- c("red1", "blue1", "yellow1", "brown1", "grey1", "hotpink1", "tan1",
                   "red4", "blue4", "yellow4", "brown4", "grey4", "hotpink4")

ggplot(data = cluster.by.year, aes(x = year, y = publish, fill = cluster)) +
  geom_bar(stat = "identity") +
  scale_x_continuous(breaks = 2009:2019, labels = 2009:2019) +
  scale_y_continuous(breaks = 0:7, labels = 0:7) +
  scale_fill_manual(values = cluster.color, labels = paste("Cluster", 1:13, sep = "")) +
  labs(x = "Publication year", y = "Number of published papers", fill = "Cluster #") +
  theme_classic()

# 한국어 학술논문 말뭉치 분석 결과
dist.dtm.k <- dist(dtm.k)
myclusters_k <- hclust(dist.dtm.k, method = "ward.D2")

myclusters_k$labels <- str_extract(myclusters_k$labels, "[[:digit:]]{4}[[:alpha:]]{1}")
fviz_dend(myclusters_k)

mygroup_k <- cutree(myclusters_k, k = 13)
mygroup_k
table(mygroup_k)

fviz_dend(myclusters_k, k = 13)

mytable_k <- table(str_extract(myclusters_k$labels, "[[:digit:]]*"), 
                 cutree(myclusters_k, k=13))
mytable_k

cluster.by.year_k <- data.frame(mytable_k)
colnames(cluster.by.year_k) <- c("year", "cluster", "publish")

cluster.by.year_k$cluster <- paste("cluster", cluster.by.year_k$cluster, sep = "")
cluster.by.year_k$year <- as.numeric(as.character(cluster.by.year_k$year))

cluster.color_k <- c("red1", "blue1", "yellow1", "brown1", "grey1", "hotpink1", "tan1",
                   "red4", "blue4", "yellow4", "brown4", "grey4", "hotpink4")

ggplot(data = cluster.by.year_k, aes(x = year, y = publish, fill = cluster)) +
  geom_bar(stat = "identity") +
  scale_x_continuous(breaks = 2009:2019, labels = 2009:2019) +
  scale_y_continuous(breaks = 0:7, labels = 0:7) +
  scale_fill_manual(values = cluster.color, labels = paste("Cluster", 1:13, sep = "")) +
  labs(x = "Publication year", y = "Number of published papers", fill = "Cluster #") +
  theme_classic()
