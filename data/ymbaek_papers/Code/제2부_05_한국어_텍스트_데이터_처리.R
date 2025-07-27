############################## 
#제 2부 텍스트 데이터 처리####
#05 한국어 텍스트 데이터 처리#
##############################

#R을 이용한 한국어 자연어처리
#자바의 문제가 있을 수 있다. 
#문제발생시 자신의 PC 사양에 맞는 자바를 재설치한 후 재실행해야 한다(별첨자료-1 참조).
library('KoNLP')
library('tm')
library('stringr')

#2019년까지 출간된 저자의 한국어 논문들의 말뭉치를 구성하였다.
mytextlocation <- "D:/data/ymbaek_논문"
mypaper <- VCorpus(DirSource(mytextlocation))
mypaper

#예를 들어 19번째 논문초록을 대상으로 한국어 자연어 처리를 진행해 보자. 
mykorean <- mypaper[[19]]$content
mykorean

#간단한 사전처리를 실시하자. 
#영문표현들은 모두 삭제하였다(한국어 분석이기 때문에)
mytext <- str_remove_all(mykorean, "[[:lower:]]")
#괄호를 삭제하였다. 
mytext <- str_remove_all(mytext, "\\(")
mytext <- str_remove_all(mytext, "\\)")
#따옴표를 삭제하였다. 
mytext <- str_remove_all(mytext, "‘")
mytext <- str_remove_all(mytext, "’")
#가운뎃점을 삭제하였다. 
mytext <- str_remove_all(mytext, " · ")
mytext


#의미의 핵심이라고 할 수 있는 명사를 추출하였다. 
noun.mytext <- extractNoun(mytext)
noun.mytext
#명사들의 빈도 분석을 해보자. 
sort(table(noun.mytext),decreasing=TRUE)

#띄어쓰기 옵션을 사용할 수도 있다(하지만 완벽하지는 않으며, 개인적으로는 선호하지 않는다)
extractNoun("이문장에는띄어쓰기가없습니다",autoSpacing=FALSE) #디폴트옵션 
extractNoun("이문장에는띄어쓰기가없습니다",autoSpacing=TRUE)


#한국어 말뭉치 텍스트 데이터 분석 
#숫자표현은 어떤 것들이 사용되었는지 확인 
fun_number <- function(x) {
  str_extract_all(x$content,"[[:graph:]]{0,}[[:digit:]]{1,}[[:graph:]]{0,}")
}
mydigits <- lapply(mypaper, fun_number)
table(unlist(mydigits))

#특수기호는 어떤 것들이 사용되었고, 그 전후의 표현은 어떤가?
fun_spe_char <- function(x) {
  str_extract_all(x$content,"[[:alnum:]]{1,}[[:punct:]]{1,}[[:alnum:]]{1,}")
}
mypuncts <- lapply(mypaper, fun_spe_char)
table(unlist(mypuncts))

#영문표현들은 어떤 것들이 사용되었고, 그 전후의 표현은 어떤가?
fun_english <- function(x) {
  str_extract_all(x$content,
                  "[[:graph:]]{0,}([[:upper:]]{1}|[[:lower:]]{1})[[:lower:]]{0,}[[:graph:]]{0,}")
}
myEnglish <- lapply(mypaper, fun_english)
table(unlist(myEnglish))

#지정표현을 교체하는 함수는 영문 말뭉치 처리과정에서 제시한 바 있다. 
#여기서는 교체대상 표현을 아무 것도 지정하지 않는 방식으로 원치 않는 표현을 삭제하였다
mytempfunc <- function(myobject,oldexp,newexp){
  newobject <- tm_map(myobject,
            content_transformer(function(x,pattern){str_replace_all(x,pattern,newexp)}),
            oldexp)
  newobject
}

mycorpus <- mypaper 
#특수기호들 제거 
mycorpus <- mytempfunc(mycorpus, "‘","")
mycorpus <- mytempfunc(mycorpus, "’","")
mycorpus <- mytempfunc(mycorpus, "“","")
mycorpus <- mytempfunc(mycorpus, "”","")
mycorpus <- mytempfunc(mycorpus, "’","")
mycorpus <- mytempfunc(mycorpus, '"',"")
mycorpus <- mytempfunc(mycorpus, "/","")
mycorpus <- mytempfunc(mycorpus, " · ",", ")
mycorpus <- mytempfunc(mycorpus, "·",", ")
mycorpus <- mytempfunc(mycorpus, "ㆍ",", ")
mycorpus <- mytempfunc(mycorpus, "-","")
mycorpus <- mytempfunc(mycorpus, "－","")
#?의 경우 공란 대체 
mycorpus <- mytempfunc(mycorpus, "\\?"," ")
#괄호속의 표현은 모두 삭제 
mycorpus <- mytempfunc(mycorpus, "\\([[:print:]]{1,}\\)","")
#공란 처리 
mycorpus <- tm_map(mycorpus, stripWhitespace)


#사전처리 수준을 점검해 보자. 
mycorpus[[1]]$content
mycorpus[[33]]$content

#명사 추출 후 문서를 명사들의 나열로 바꾸어주는 개인맞춤 함수 
myNounFun <- function(mytext){
  myNounList <- paste(extractNoun(mytext),collapse=' ')
  myNounList
}
#말뭉치의 각 문서들에서 명사들만이 나열된 텍스트 추출 
myNounCorpus <- mycorpus 
for (i in 1:length(mycorpus)) {
  myNounCorpus[[i]]$content <- myNounFun(mycorpus[[i]]$content)
}

#전체 말뭉치 단어를 확인해 보자. 
words_nouns <- lapply(myNounCorpus,
                      function(x){str_extract_all(x$content,boundary("word"))}
)
table(unlist(words_nouns))

#개별숫자들의 경우 추가 삭제하였다.
myNounCorpus <- mytempfunc(myNounCorpus,"[[:digit:]]{1,}\\,{0,1}[[:digit:]]{0,}","")
#추출된 단어들중 몇몇은 문제들을 발견할 수 있다. 
#예를 들면 포퓰리즘, 포퓰리즘과, 포퓰리즘에 등이 모두 개별단어로 처리되어 있다. 
#추가적인 작업을 통해 이런 표현들을 정리하는 것도 괜찮지만, 
#양이 많을 경우 노력이 많이 소요된다. 몇 개만 실시해 보자. 
myNounCorpus <- mytempfunc(myNounCorpus,"포퓰리즘[[:alpha:]]{1,}","포퓰리즘")
myNounCorpus <- mytempfunc(myNounCorpus,"커뮤니케이션[[:alpha:]]{1,}","커뮤니케이션")
myNounCorpus <- mytempfunc(myNounCorpus,"참여[[:alpha:]]{1,}","참가")
myNounCorpus <- mytempfunc(myNounCorpus,"참가[[:alpha:]]{1,}","참가")
myNounCorpus <- mytempfunc(myNounCorpus,"위키리크스[[:alpha:]]{1,}","위키리크스")
myNounCorpus <- mytempfunc(myNounCorpus,"설치알림판[[:alpha:]]{1,}","설치알림판")
myNounCorpus <- mytempfunc(myNounCorpus,"콘텐트[[:alpha:]]{1,}","콘텐트")
myNounCorpus <- mytempfunc(myNounCorpus,"미투[[:alpha:]]{1,}","미투")

#DTM 구축
dtm.k <- DocumentTermMatrix(myNounCorpus)
dtm.k
colnames(dtm.k[,])

# setwd("D:/data/code")
#사전처리한 말뭉치와 DTM을 저장한 후 나중에 다시 호출한 후 사용하겠다. 
saveRDS(myNounCorpus, "CorpusK_preprocess.RData")
saveRDS(dtm.k, "dtmK.RData")
