# 감정어휘 사전을 이용한 감정분석
library(tidytext)
library(tidyverse)
library(textdata)  # 감정어휘 사전. lexicon 다운로드

job <- readLines("./data/Job_King_James_version.txt")
head(job)
length(job)  # 3213개 줄로 구성

# 1. '욥기' 텍스트를 장 단위로 구분
job <- str_flatten(job)
substr(job, 1, 400)

job_text <- str_split(job, pattern = "18:[[:digit:]]{3}:[[:digit:]]{3}")[[1]]
job_text

job_text <- job_text[2:length(job_text)]

job_number <- str_extract_all(job, pattern = "18:[[:digit:]]{3}:[[:digit:]]{3}")[[1]]  # 장, 절 숫자 추출
job_chapter <- str_remove(str_extract_all(job_number, "18:[[:digit:]]{3}"),
                          pattern = "18:")
job_verse <- str_remove(str_extract_all(job_number, "18:[[:digit:]]{3}:[[:digit:]]{3}"),
                        pattern = "18:[[:digit:]]{3}:")

length(job_text);length(job_chapter);length(job_verse)

tb_job <- tibble(chapter = job_chapter,
                 verse = job_verse,
                 text = job_text)
tb_job

# 2. 사전처리 실시
tb_job <- tb_job %>% 
  mutate(chapter = as.numeric(chapter),
         verse = as.numeric(verse),
         text = tolower(text),
         text = str_remove_all(text, "[[:punct:]]{1,}"),  # 문장부호 제거
         text = str_squish(text))                         # 불필요한 공란 제거
tb_job

tb_job_word <- tb_job %>%   # 불용어 제거
  unnest_tokens(input = text,
                output = word,
                token = "words") %>% 
  anti_join(stop_words, by = "word")
tb_job_word

# 3. 감정분석 실시
afinn_lexicon <- lexicon_afinn()
afinn_lexicon

# Afinn 감성사전
afinn_lexicon %>% 
  count(value) %>% 
  mutate(type = ifelse(value < 0, "Negative", "Positive")) %>% 
  ggplot(aes(x = value, y = n, fill = type)) +
  geom_bar(stat = "identity") +
  labs(x = "Sentiment value (positive score for positive sentiment), Afinn Lexicon",
       y = "Frequency", fill = "Direction of sentiment") +
  coord_cartesian(xlim = c(-6, 6)) + # <-- Corrected line
  scale_x_continuous(breaks = -6:6) +
  scale_y_continuous(breaks = 100*0:10) +
  theme_classic() +
  theme(legend.position = "bottom")

# Bing 감성사전
bing_lexicon <- lexicon_bing()
bing_lexicon
table(bing_lexicon$sentiment)
prop.table(table(bing_lexicon$sentiment))

bing_lexicon %>% count(sentiment) %>% 
  ggplot(aes(x = sentiment, y = n, fill = sentiment)) +
  geom_bar(stat = "identity") +
  labs(x = "Sentiment", y = "Frequency", fill = "Sentiment", 
       title = "Frequency of Positive and Negative Words in Bing Lexicon") +
  theme_minimal() +
  scale_fill_manual(values = c("negative" = "firebrick", "positive" = "steelblue"))

# EmoLex 감성사전
#NRC_lexicon <- lexicon_nrc()
#NRC_lexicon
NRC_lexicon <- read_tsv("./data/NRC-Emotion-Lexicon.txt", show_col_types = F)
NRC_lexicon

NRC_lexicon %>% 
  filter(score != 0) %>% 
  count(sentiment) %>% 
  mutate(type = ifelse(sentiment == "positive" | sentiment == "joy" | sentiment == "trust" | 
                         sentiment == "surprise" | sentiment == "anticipation",
                       "Broadly Positive", "Broadly Negative"),
         sentiment = fct_reorder(sentiment, n)) %>% 
  ggplot(aes(x = sentiment, y = n, fill = type)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  scale_y_continuous(breaks = 200*(0:17)) +
  labs(x = "Sentiment Category, NRC Lexicon", y = "Frequency", fill = "Broad Category") +
  theme_bw()
  
# AFINN 사전을 이용한 감정분석
tb_job_affin <- tb_job_word %>% 
  inner_join(afinn_lexicon, by = "word")
tb_job_affin

# 장별 총 단어 수 계산
tb_job_totalword <- tb_job_word %>% 
  count(chapter, name = "total_word")
tb_job_totalword

# 감정점수를 장별로 집계
afinn_sentiment_chapter <- tb_job_affin %>% 
  group_by(chapter) %>%  # 장들로 데이터를 나눈 후
  summarise(sentiment_afinn = sum(value)) %>%   # 각 장에서의 감정점수 합산
  full_join(tb_job_totalword, by = "chapter") %>%   # 장별 전체 단어와 합산
  mutate(prop = sentiment_afinn / total_word)
afinn_sentiment_chapter

# 4. 감정분석 결과 시각화
# Afinn Lexicon 이용
# 장별로 감정점수의 변화
ggplot(data = afinn_sentiment_chapter, aes(x = chapter, y = prop)) +
  geom_point(size = 1, color = "red") +
  geom_line(lty = 2, color = "blue") +
  scale_x_continuous(breaks = 1:42) +
  labs(x = "Chapters in Book of Job in Old Testament (Hebrew Bible)",
       y = "sentiment score, averaged per word\n(Afinn lexicon)") +
  theme_bw() +
  ggtitle("Sentiment analysis using Afinn Lexicon")

# Opinion Lexicon 이용한 감정분석
tb_job_bing <- tb_job_word %>% 
  inner_join(bing_lexicon, by = "word")
tb_job_bing

# 감정점수를 장별로 합산
bing_sentiment_chapter <- tb_job_bing %>% 
  count(chapter, sentiment) %>% 
  full_join(tb_job_totalword, by = "chapter") %>%  # 장별 전체 단어와 합산
  mutate(prop = n/total_word) %>%   # 단어 범주별 비율 계산
  select(chapter, sentiment, prop)
bing_sentiment_chapter

ggplot(data = bing_sentiment_chapter, aes(x = chapter, y = prop, shape = sentiment, color = sentiment)) +
  geom_point(size = 3) +
  geom_line(lty = 2) +
  scale_x_continuous(breaks = 1:42) +
  labs(x = "Chapters in Book of Job in Oldest Testament (Hebrew Bible)",
       y = "sentiment score, averaged count per word\n(Bing's opinion lexicon)",
       shape = "Direction of sentiment", color = "Direction of sentiment") +
  theme_classic() +
  theme(legend.position = "botton") +
  ggtitle("Sentitment analysis using Bing's opinion lexicon")

# EmoLex 이용 감정분석
tb_job_NRC <- tb_job_word %>% 
  inner_join(NRC_lexicon, by = "word")
tb_job_NRC

# 감정점수를 장별로 합산
NRC_sentiment_chapter <- tb_job_NRC %>% 
  count(chapter, sentiment) %>% 
  full_join(tb_job_totalword, by = "chapter") %>% 
  mutate(prop = n/total_word) %>% 
  select(chapter, sentiment, prop)
NRC_sentiment_chapter

NRC_sentiment_chapter2 <- NRC_sentiment_chapter %>% 
  filter(sentiment != "positive" & sentiment != "negative") %>% 
  mutate(upper_cate = ifelse(sentiment == "sentiment" | sentiment == "disgust" |
                               sentiment == "fear" | sentiment == "sadness", "Negative > ", "Positive > "),
         type = str_c(upper_cate, sentiment))
NRC_sentiment_chapter2

ggplot(data = NRC_sentiment_chapter2, aes(x = chapter, y = prop)) +
  geom_point(size = 2) +
  geom_line(lty = 2) +
  scale_x_continuous(breaks = 3*(1:14)) +
  labs(x = "Chapters in Book of Job in Old Testament (Hebrew Bible)",
       y = "sentiment score, averaged per countsummed\n(NRC Lexicon)") +
  facet_wrap(~sentiment, ncol = 2) +
  theme_bw() +
  theme(legend.position = "bottom") +
  ggtitle("Sentiment analysis using NRC Lexicon")

# 지도 기계학습을 이용한 감정분석
library(caret)
setwd("./data/Twitter-Sentimental-Analysis-master")

h.train <- readLines("happy.txt")
s.train <- readLines("sad.txt")
h.test <- readLines("happy_test.txt")
s.test <- readLines("sad_test.txt")

library(tibble)
Htrain <- tibble(text = h.train) %>% 
  mutate(id = str_c("train_happy_", row_number()))
Htrain

Strain <- tibble(text = s.train) %>% 
  mutate(id = str_c("train_sad_", row_number()))

Htest <- tibble(text = h.test) %>% 
  mutate(id = str_c("test_happy_", row_number()))

Stest <- tibble(text = s.test) %>% 
  mutate(id = str_c("test_sad_", row_number()))

dt <- bind_rows(Htrain, Strain, Htest, Stest) %>% 
  mutate(
    type = str_remove_all(str_extract(id, "[[:alpha:]]{3,}_"), "_"),
    status = str_remove_all(str_extract(id, "_[[:alpha:]]{3,}_"), "_")
  )
dt

dt_pp <- dt %>% 
  mutate(text = str_remove_all(text, "[[:punct:]]{1,}"),
         text = str_remove_all(text, "[[:digit:]]{1,}"),
         text = str_squish(text)) %>% 
  unnest_tokens(input = text,
                output = word,
                token = "words",
                to_lower = T) %>% 
  anti_join(stop_words, by = 'word') %>%  # 불용어 사전에 포함되지 않은 단어만 조인
  mutate(word = SnowballC::wordStem(word))
dt_pp

# 훈련 데이터 라벨 생성
label_train <- factor(dt$status[dt$type == "train"])
label_test <- factor(dt$status[dt$type == "test"])

# 훈련 데이터 DTM 생성
dt_train_dtm <- dt_pp %>% 
  filter(type == "train") %>% 
  count(id, word) %>%  # 문서X단어별 빈도
  cast_dtm(document = id, term = word, value = n)
dt_train_dtm

dt_train_dtm <- as.matrix(dt_train_dtm)  # 매트릭스 변환을 해야 기계학습 가능

# 훈련 데이터에는 등장했지만 테스트 데이터에 등장하지 않은 단어는 0의 값을 가져야 한다.
dt_test_dtm <- dt_pp %>% 
  filter(type == "test") %>% 
  full_join(dt_pp %>% filter(type == "train") %>% select(word), by="word") %>% 
  count(id, word) %>% 
  cast_dtm(document = id, term = word, value = n)
dt_test_dtm

rownames(dt_test_dtm)

dt_test_dtm <- as.matrix(dt_test_dtm)[1:20, ]
dt_test_dtm

# Classification Tree
set.seed(20191121)
tree_train <- train(x = dt_train_dtm,
                    y = label_train,
                    method = "rpart")

pred_tree_train <- predict(tree_train)
table(pred_tree_train, label_train)

# 테스트 데이터에 적용
pred_tree_test <- predict(tree_train, newdata = dt_test_dtm)
table(pred_tree_test, label_test)

confusionMatrix(table(pred_tree_test, label_test))  # 기준범주: happy
confusionMatrix(table(fct_rev(pred_tree_test), fct_rev(label_test)))  # 기준범주: sad

# Random Forest
set.seed(20191121)
RF_train <- train(x = dt_train_dtm,
                  y = label_train,
                  method = "ranger",
                  num.trees = 200)

pred_RF_train <- predict(RF_train)
table(pred_RF_train, label_train)

# 테스트 데이터에 적용
pred_RF_test <- predict(RF_train, newdata = dt_test_dtm)
table(pred_RF_test, label_test)

confusionMatrix(table(pred_RF_test, label_test))  # 기준범주: happy
confusionMatrix(table(fct_rev(pred_RF_test), fct_rev(label_test)))  # 기준범주: sad

# Neural Network
set.seed(20191121)
NN_train <- train(x = dt_train_dtm,
                  y = label_train,
                  method = "nnet")

pred_NN_train <- predict(NN_train)
table(pred_NN_train, label_train)

# 테스트 데이터에 적용
pred_NN_test <- predict(NN_train, newdata = dt_test_dtm)
table(pred_NN_test, label_test)

confusionMatrix(table(pred_NN_test, label_test))  # 기준범주: happy
confusionMatrix(table(fct_rev(pred_NN_test), fct_rev(label_test)))  # 기준범주: sad
