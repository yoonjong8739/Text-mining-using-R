## 영어 품사 분석 ##
# POS 분석을 위한 패키지 구동
library(NLP)
library(openNLP)
library(tm)
library(stringr)
library(KoNLP)

R_wiki <- "R is a programming language for statistical computing and data visualization. It has been widely adopted in the fields of data mining, bioinformatics, data analysis, and data science.
The core R language is extended by a large number of software packages, which contain reusable code, documentation, and sample data. Some of the most popular R packages are in the tidyverse collection, which enhances functionality for visualizing, transforming, and modelling data, as well as improves the ease of programming (according to the authors and users).
R is free and open-source software distributed under the GNU General Public License. The language is implemented primarily in C, Fortran, and R itself. Precompiled executables are available for the major operating systems (including Linux, MacOS, and Microsoft Windows).
Its core is an interpreted language with a native command line interface. In addition, multiple third-party applications are available as graphical user interfaces; such applications include RStudio (an integrated development environment) and Jupyter (a notebook interface)."

# annotation(): 텍스트 데이터 객체에 대해 주석작업을 실시
R.wiki.sent <- annotate(R_wiki, Maxent_Sent_Token_Annotator()) # 문장 단위로 주석처리
R.wiki.sent

R.wiki.word <- annotate(R_wiki, Maxent_Word_Token_Annotator(), 
                        R.wiki.sent) # 문장 단위로 주석처리 된 것에 추가적으로 단어 단위로 주석처리
R.wiki.word

# 각 단어별 품사분석(POS-tagging) 실시
POStag <- annotate(R_wiki, Maxent_POS_Tag_Annotator(), R.wiki.word)
POStag

word.start <- 1 + length(R.wiki.sent)
word.end <- length(R.wiki.word)
all.POS.tagged <- unlist(POStag$features[word.start:word.end])
all.POS.tagged
table(all.POS.tagged)

# 문장 부호 사용 횟수
my.PUNCT <- str_detect(all.POS.tagged, "[[:punct:]]")
sum(my.PUNCT)

my.NN <- str_detect(all.POS.tagged, "NN$")
sum(my.NN)

my.NNs <- str_detect(all.POS.tagged, "NN")
sum(my.NNs)

# 개인 맟춤 함수 설정
my.POStag.func <- function(mytext){
  sent.annotate <- annotate(mytext, Maxent_Sent_Token_Annotator())
  word.annotate <- annotate(mytext, Maxent_Word_Token_Annotator(), sent.annotate)
  POStag <- annotate(mytext, Maxent_POS_Tag_Annotator(), word.annotate)
  myrange <- (1 + length(sent.annotate)) : length(word.annotate)
  my.POStag <- unlist(POStag$features[myrange])
  my.POStag
}

my.text.location <- "D:/R_project/R_text/Text-mining-using-R/data/ymbaek_papers"
mypaper <- VCorpus(DirSource(my.text.location))
mypaper1.POStag <- my.POStag.func(mypaper[[1]]$content) # 첫 번째 논문 
mypaper1.POStag
sum(str_detect(mypaper1.POStag, "NN"))

# 전체 33개의 영문 초록에서 등장한 명사+대명사 비율 계산
N_mypaper <- length(mypaper)
compare.noun <- rep(NA, N_mypaper)
for (i in 1:N_mypaper){
  my.NN <- sum(str_detect(my.POStag.func(mypaper[[i]]$content), "NN"))
  all.POS <- sum(table(my.POStag.func(mypaper[[i]]$content)))
  compare.noun[i] <- my.NN / all.POS
}
round(compare.noun, 2)

prop.noun <- data.frame(1:N_mypaper, compare.noun)
colnames(prop.noun) <- c("abstract.no", "prop.noun")
head(prop.noun[order(prop.noun$prop.noun), ], 1)
tail(prop.noun[order(prop.noun$prop.noun), ], 1)
prop.noun

## 한국어 품사 분석 ##
my.text.location <- "D:/R_project/R_text/Text-mining-using-R/data/ymbaek_논문"
mypaper <- VCorpus(DirSource(my.text.location, encoding = "cp949"))
mytext <- mypaper[[33]]$content
mytext

mypaper33.pos09 <- SimplePos09(mytext)
mypaper33.pos09

mypaper33.pos22 <- SimplePos22(mytext)
mypaper33.pos22

# 보통명사(NC)만 추출
mypaper33.pos22.pp <- mypaper33.pos22
mytextlength <- length(mypaper33.pos22)
for (i in 1:mytextlength){
  mylocation <- str_locate_all(mypaper33.pos22[i], pattern = "/NC")
  mypaper33.pos22.pp[i] <- str_sub(mypaper33.pos22[i], 1, mylocation[[1]][1]-1)
  mypaper33.pos22.pp[i] <- str_replace_all(mypaper33.pos22.pp[i], "[[:alnum:]]/[[:upper:]]{1,}", "")
}
mypaper33.pos22.pp

mypaper33.pos22.pp <- unlist(mypaper33.pos22.pp)
mypaper33.pos22.pp <- mypaper33.pos22.pp[!is.na(mypaper33.pos22.pp)]
mypaper33.pos22.pp

sum(table(mypaper33.pos22.pp))

my.NC.func <- function(mytext){
  myobject <- SimplePos22(mytext)
  new.myobject <- mytext
  mytextlength <- length(myobject)
  mylocation <- for (i in 1:mytextlength){
    mylocation <- str_locate_all(myobject[i], pattern = "/NC")
    new.myobject[i] <- str_sub(myobject[i], 1, mylocation[[1]][1]-1)
    new.myobject[i] <- str_replace_all(new.myobject[i], "[[:alnum:]]/[[:upper:]]{1,}", "")
  }
  new.myobject <- unlist(new.myobject)
  new.myobject <- new.myobject[!is.na(new.myobject)]
  new.myobject
}
sum(table(my.NC.func(mypaper[[33]]$content)))

size.noun <- rep(NA, length(mypaper))
for (j in 1:length(mypaper)){
  size.noun[j] <- sum(table(my.NC.func(mypaper[[j]]$content)))
}
size.noun

size.noun <- data.frame(1:length(mypaper), size.noun)
colnames(size.noun) <- c("abstract.no", "no.noun")
size.noun[order(size.noun$no.noun),][c(1, length(mypaper)), ]