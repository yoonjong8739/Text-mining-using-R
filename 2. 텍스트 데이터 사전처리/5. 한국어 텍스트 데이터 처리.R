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
