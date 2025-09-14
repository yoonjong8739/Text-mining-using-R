#install.packages(c("rJava", "stringr", "hash", "tau", "Sejong", "RSQLite", "devtools"), type = "binary")
library(KoNLP)
#useNIADic()  # 형태소 사전 설치. KoNLP 패키지 설치 후 한 번만 실행하면 된다.
library(tm)
library(stringr)

# 2019년까지 필자의 출간된 한국어 논문 말뭉치 구성
mytextlocation <- "D:/R_project/R_text/Text-mining-using-R/data/ymbaek_논문"
mypaper <- VCorpus(DirSource(mytextlocation, encoding = "cp949"))
mypaper

# 19번째 논문 초록 대상으로 한국어 자연어 처리 진행
mykorean <- mypaper[[19]]$content
mykorean

# 사전처리: 영문 표현 삭제, 괄호 삭제, 가운뎃 점을 쉼표로 교체
mytext <- str_remove_all(mykorean, "[[:lower:]]")
mytext <- str_remove_all(mytext, "\\(")
mytext <- str_remove_all(mytext, "\\)")
mytext <- str_remove_all(mytext, "‘")
mytext <- str_remove_all(mytext, "’")
mytext <- str_replace_all(mytext, " · ", ", ")
mytext

# 명사만 추출
noun.text <- extractNoun(mytext)
noun.text

# 명사들의 빈도 분석
sort(table(noun.text), decreasing = T)

# 어떤 숫자 표현들이 사용되었는지 확인
fun_number <- function(x){
  str_extract_all(x$content, "[[:graph:]]{0,}[[:digit:]]{1,}[[:graph:]]{0,}")
}
mydigits <- lapply(mypaper, FUN = fun_number)
table(unlist(mydigits))

# 어떤 특수문자들이 사용되었고 그 전후 표현 확인
fun_spe_char <- function(x){
  str_extract_all(x$content, "[[:alnum:]]{1,}[[:punct:]]{1,}[[:alnum:]]{1,}")
}
mypuncts <- lapply(mypaper, fun_spe_char)
table(unlist(mypuncts))

# 어떤 영문 표현들이 사용되었고 그 전후 표현 확인
fun_english <- function(x){
  str_extract_all(x$content, "[[:graph:]]{0,}([[:upper:]]{1}|[[:lower:]]{1})[[:lower:]]{0,}[[:graph:]]{0,}")
}
myEnglish <- lapply(mypaper, fun_english)
table(unlist(myEnglish))

# 교체 대항 표현을 아무 것도 지정하지 않는 방식으로 원치 않는 표현 삭제
mytempfunc <- function(myobject, oldexp, newexp){
  newobject <- tm_map(myobject, 
                      content_transformer(function(x, pattern){
                        str_replace_all(x, pattern, newexp)
                      }),
                      oldexp)
  newobject
}

mycorpus <- mypaper
mycorpus <- mytempfunc(mycorpus, "‘","")
mycorpus <- mytempfunc(mycorpus, "’","")
#mycorpus <- mytempfunc(mycorpus, ""","")
#mycorpus <- mytempfunc(mycorpus, ""","")
mycorpus <- mytempfunc(mycorpus, "/","")
mycorpus <- mytempfunc(mycorpus, " · ",", ")
mycorpus <- mytempfunc(mycorpus, "·",", ")
mycorpus <- mytempfunc(mycorpus, " · ",", ")
mycorpus <- mytempfunc(mycorpus, "-","")
mycorpus <- mytempfunc(mycorpus, "－","")
mycorpus <- mytempfunc(mycorpus, "\\?"," ")
mycorpus <- mytempfunc(mycorpus, "\\([[:print:]]{1,}\\)", "") # 괄호 속 표현 삭제

mycorpus <- tm_map(mycorpus, stripWhitespace) # 공란 처리

# 사전처리 수준 점검
mycorpus[[1]]$content
mycorpus[[33]]$content

# 명사 추출 후 문서를 명사들의 나열로 바꿔주는 개인맞춤 함수
myNounFun <- function(mytext){
  myNounList <- paste(extractNoun(mytext), collapse = " ")
  myNounList
}

# 말뭉치의 각 문서들에서 명사들만이 나열된 텍스트 추출
myNounCorpus <- mycorpus
for (i in 1:length(mycorpus)){
  myNounCorpus[[i]]$content <- myNounFun(mycorpus[[i]]$content)
}

# 전체 말뭉치 단어를 확인
words_nouns <- lapply(myNounCorpus, function(x){str_extract_all(x$content, boundary("word"))})
table(unlist(words_nouns))

# 개별 숫자 추가 삭제
myNounCorpus <- mytempfunc(myNounCorpus, "[[:digit:]]{1,}\\,{0,1}[[:digit:]]{0,}", "")
myNounCorpus <- mytempfunc(myNounCorpus, "포퓰리즘[[:alpha:]]{1,}", "포퓰리즘")
myNounCorpus <- mytempfunc(myNounCorpus, "커뮤니케이션[[:alpha:]]{1,}", "커뮤니케이션")
myNounCorpus <- mytempfunc(myNounCorpus, "참여[[:alpha:]]{1,}", "참가")
myNounCorpus <- mytempfunc(myNounCorpus, "참가[[:alpha:]]{1,}", "참가")
myNounCorpus <- mytempfunc(myNounCorpus, "위키리크스[[:alpha:]]{1,}", "위키리스크")
myNounCorpus <- mytempfunc(myNounCorpus, "설치알림판[[:alpha:]]{1,}", "설치알림판")
myNounCorpus <- mytempfunc(myNounCorpus, "콘텐트[[:alpha:]]{1,}", "콘텐트")
myNounCorpus <- mytempfunc(myNounCorpus, "미투[[:alpha:]]{1,}", "미투")

# DTM 구축
dtm.k <- DocumentTermMatrix(myNounCorpus)
dtm.k

# 말뭉치 & DTM 저장
saveRDS(myNounCorpus, "./2. 텍스트 데이터 사전처리/CorpusK_preprocess.RData")
saveRDS(dtm.k, "./2. 텍스트 데이터 사전처리/dtmK.RData")

# 사전을 정의하기 이전
useSejongDic()
populism_text <- "포퓰리즘이 등장하면 사회적 커뮤니케이션의 모습이 왜곡된다. ."
extractNoun(populism_text)

# 새로 사전을 정의한 후 업데이트
buildDictionary(ext_dic = "sejong",
                category_dic_nms = "political", # 추가되는 영역
                user_dic = data.frame(c("포퓰리즘"), c("ncn")), # ncn은 비서술성 명사
                replace_usr_dic = F)

extractNoun(populism_text)
