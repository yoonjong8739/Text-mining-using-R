#install.packages("tm")
# 1. 공란 처리 과정
mytext <- c("software environment", "software  environment", "software\tenvironment")
mytext

# 단어를 ' ' 으로 구분
library(stringr)
str_split(mytext, ' ')

# 각 오브젝트별 단어 수와 문자 수를 세어보자.
sapply(str_split(mytext, ' '), FUN = length)  # 단어 수
sapply(str_split(mytext, ' '), FUN = str_length)  # 문자 수

# 공란 처리 과정
mytext.nowhitespace <- str_replace_all(mytext, "[[:space:]]{1,}"," ") # 스페이스나 탭을 이용한 공란이 최소 1회 이상일 경우, " "으로 교체.
mytext.nowhitespace

sapply(str_split(mytext.nowhitespace, " "), FUN = length)
sapply(str_split(mytext.nowhitespace, " "), FUN = str_length)

# str_squish() 함수 사용
sapply(str_split(str_squish(mytext), " "), length)
sapply(str_split(str_squish(mytext), " "), str_length)

# 2. 대·소문자 통일 과정
mytext <- "The 45th President of the United States, Donald Trump, states that he knows how to play trump with the former president"
myword <- unlist(str_extract_all(mytext, boundary("word")))  # 단어 단위의 텍스트 추출
table(myword)

myword <- str_replace(myword, "Trump", "Trump_unique_")
myword <- str_replace(myword, "States", "States_unique_")

# 소문자 전환 후 빈도표 출력
table(tolower(myword))

# 숫자표현 제거 과정
mytext <- c("He is one of statisticians agreeing that R is the No. 1 statistical software.", 
            "He is one of statisticians agreeing that R is the No. one statistical software.")
str_split(mytext, " ")

mytext2 <- str_split(str_replace_all(mytext, "[[:digit:]]{1,}[[:space:]]{1,}","")," ")  # 숫자 & 공란 최소 1회 이상 등장한 경우 삭제
mytext2
str_c(mytext2[[1]], collapse = " ")  # 띄어쓰기 기준으로 연결
str_c(mytext2[[2]], collapse = " ")

# 3. 문장부호 제거 과정
mytext <- "Baek et al. (2014) argued that the state of default-setting is critical for people to protect their own personal privacy on the Internet."
str_split(mytext, "\\. ")
str_split(mytext, " ")

mytext2 <- str_replace_all(mytext, "-", " ")
mytext2 <- str_replace_all(mytext2, 
                           "[[:upper:]]{1}[[:alpha:]]{1,}[[:space:]](et al\\.)[[:space:]]\\([[:digit:]]{4}\\)",
                           "_reference_")
mytext2 <- str_replace_all(mytext2, "\\.[[:space:]]{0,}", "")
mytext2

# 4. 불용단어 제거 과정
mytext <- c("She is an actor", "She is the actor")
mystopwords <- "(\\ba ) | (\\ban ) | (\\bthe )"  # \\b: 해당 표현으로 시작한다는 의미
str_remove_all(mytext, mystopwords)

# tm 패키지에 포함된 영어 불용단어 목록
library(tm)
length(stopwords("en"))
length(stopwords("SMART"))
stopwords("en")
stopwords("SMART")

# 5. 어근 동일화 처리 과정
various_be <- "(\\b(a|A)m )|(\\b(a|A)re )|(\\b(i|I)s )|(\\b(w|W)as )|(\\b(w|W)ere)|(\\b(w|W)e )"
mystemmer.func <- function(mytextobject){
  mytext <- str_replace_all(mytext, various_be, "be ")
}
mytext <- c("I am a boy. You are a boy. The person might be a boy. Is Jane a boy?")
mytext.stem <- mystemmer.func(mytext)
table(str_split(mytext, " "))
table(str_split(mytext.stem, " "))

# 6. N-gram 적용 과정
mytext <- "The United States comprises fifty states. In the United States, each state has its own laws. However, federal law overrides state law in the United States."
myword <- unlist(str_extract_all(mytext, boundary("word")))
length(table(myword))
sum(table(myword))

mytext.2gram <- str_replace_all(mytext, "\\bUnited States", "United_States")
myword2 <- unlist(str_extract_all(mytext.2gram, boundary("word")))
length(table(myword2))
sum(table(myword2))

mytext.3gram <- str_replace_all(mytext, "\\b(t|T)he United States", "The_United_States")
myword3 <- unlist(str_extract_all(mytext.3gram, boundary("word")))
length(table(myword3))
sum(table(myword3))

# 7. 말뭉치 구성
library(tm)
my.text.location <- "D:/R_project/R_text/Text-mining-using-R/data/ymbaek_papers"
mypaper <- VCorpus(DirSource(my.text.location))  # 해당 폴더에 있는 텍스트 데이터들을 말뭉치로 구성
mypaper
summary(mypaper)

mypaper[[2]]
mypaper[[2]]$content
mypaper[[2]]$meta

meta(mypaper[[2]], tag = "author") <- "Y. M. Baek"
meta(mypaper[[2]])

# 8. 말뭉치 사전처리
# removeNumbers(): 말뭉치에 사용된 숫자표현을 모두 제거
# removePunctuation(): 말뭉치에 사용된 특수문자를 모두 제거
# stripWhitespace(): 2회 이상 연이어 등장하는 공란이나 탭 공란 등을 1개의 공란으로 치환
# removeWords(): 말뭉치에서 사전에 지정된 단어들을 삭제
# stemDocument(): 어근 동일화 알고리즘 적용
# content_transformer(): 이용자가 지정한 함수를 적용. ex) content_transformer(tolower)

myfunc <- function(x){
  str_extract_all(x$content, "[[:alnum:]]{1,}[[:punct:]]{1}?[[:alnum:]]{1,}")  # 특수문자 전후에 등장하는 단어 추출
}
mypuncts <- lapply(mypaper, myfunc)
table(unlist(mypuncts))

myfunc <- function(x){
  str_extract_all(x$content, "[[:graph:]]{0,}[[:digit:]]{1,}[[:graph:]]{0,}")
}
mydigits <- lapply(mypaper, myfunc)
table(unlist(mydigits))

# 숫자 표현 삭제
mycorpus <- tm_map(mypaper, FUN = removeNumbers)

# 특수문자가 들어간 표현 중 하나의 단어로 취급되어야 하는 것들을 처리
mytempfunc <- function(myobject, oldexp, newexp){
  newobject <- tm_map(myobject, 
                      content_transformer(function(x, pattern){
                        str_replace_all(x, pattern, newexp)
                      }),
                      oldexp)
  newobject
}
mycorpus <- mytempfunc(mycorpus, "andmediation-effect", "and mediation effect")
mycorpus <- mytempfunc(mycorpus,"-collar","collar")
mycorpus <- mytempfunc(mycorpus,"\\b((c|C)o-)","co")
mycorpus <- mytempfunc(mycorpus,"\\b((c|C)ross-)","cross")
mycorpus <- mytempfunc(mycorpus,"e\\.g\\.","for example")
mycorpus <- mytempfunc(mycorpus,"i\\.e\\.","that is")
mycorpus <- mytempfunc(mycorpus,"\\'s|\\’s","")
mycorpus <- mytempfunc(mycorpus,"‘|’","")
mycorpus <- mytempfunc(mycorpus,"“|”","")
mycorpus <- mytempfunc(mycorpus,"ICD-","ICD")
mycorpus <- mytempfunc(mycorpus,"\\b((i|I)nter-)","inter")
mycorpus <- mytempfunc(mycorpus,"K-pop","Kpop")
mycorpus <- mytempfunc(mycorpus,"\\b((m|M)eta-)","meta")
mycorpus <- mytempfunc(mycorpus,"\\b((o|O)pt-)","opt")
mycorpus <- mytempfunc(mycorpus,"orga-nizations","organizations")
mycorpus <- mytempfunc(mycorpus,"sci-entific","scientific")
mycorpus <- mytempfunc(mycorpus,"\\b((p|P)ost-)","post")
mycorpus <- mytempfunc(mycorpus,"-end","end")
mycorpus <- mytempfunc(mycorpus,"\\b((w|W)ithin-)","within")
mycorpus <- mytempfunc(mycorpus,"=","is equal to")
mycorpus <- mytempfunc(mycorpus,"and/or","and or")
mycorpus <- mytempfunc(mycorpus,"his/her","his her")
mycorpus <- mytempfunc(mycorpus,"well-being","wellbeing")
mycorpus <- mytempfunc(mycorpus,"settings\\.","settings ")
mycorpus <- mytempfunc(mycorpus,"-|/|\\?|\\)"," ")

# 나머지 특수문자들은 공란으로 처리
mycorpus <- tm_map(mycorpus, removePunctuation)

# 공란 처리 과정
mycorpus <- tm_map(mycorpus, stripWhitespace)

# 대·소문자 통합
mycorpus <- tm_map(mycorpus, content_transformer(tolower))

# SMART 불용어 삭제
mycorpus <- tm_map(mycorpus, removeWords, words=stopwords("SMART"))

# 어근 동일화
mycorpus <- tm_map(mycorpus, stemDocument, language="en")

# 사전처리 이전과 이후 결과 비교
mycharfunc <- function(x){str_extract_all(x$content, ".")}
mywordfunc <- function(x){str_extract_all(x$content, boundary("word"))}

mychar <- lapply(mypaper, mycharfunc)  # 사전 적용 이전
myuniquechar0 <- length(table(unlist(mychar)))
mytotalchar0 <- sum(table(unlist(mychar)))
myword <- lapply(mypaper, mywordfunc)
myuniqueword0 <- length(table(unlist(myword)))
mytotalword0 <- sum(table(unlist(myword)))

mychar <- lapply(mycorpus, mycharfunc)  # 사전 적용 이후
myuniquechar1 <- length(table(unlist(mychar)))
mytotalchar1 <- sum(table(unlist(mychar)))
myword <- lapply(mycorpus, mywordfunc)
myuniqueword1 <- length(table(unlist(myword)))
mytotalword1 <- sum(table(unlist(myword)))

result.comparing <- rbind(
  c(myuniquechar0, myuniquechar1),
  c(mytotalchar0, mytotalchar1),
  c(myuniqueword0, myuniqueword1),
  c(mytotalword0, mytotalword1)
)
colnames(result.comparing) <- c("before", "after")
rownames(result.comparing) <- c("고유문자 수", "총 문자 수", "고유단어 수", "총 단어 수")
result.comparing