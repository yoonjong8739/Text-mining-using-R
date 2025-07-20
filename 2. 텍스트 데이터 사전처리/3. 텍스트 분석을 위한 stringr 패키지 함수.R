# str_extract()와 str_extract_all(): 지정된 표현을 추출하는 함수
library(stringr)
R_wiki <- "R is a programming language for statistical computing and data visualization. It has been widely adopted in the fields of data mining, bioinformatics, data analysis, and data science.
The core R language is extended by a large number of software packages, which contain reusable code, documentation, and sample data. Some of the most popular R packages are in the tidyverse collection, which enhances functionality for visualizing, transforming, and modelling data, as well as improves the ease of programming (according to the authors and users).
R is free and open-source software distributed under the GNU General Public License. The language is implemented primarily in C, Fortran, and R itself. Precompiled executables are available for the major operating systems (including Linux, MacOS, and Microsoft Windows).
Its core is an interpreted language with a native command line interface. In addition, multiple third-party applications are available as graphical user interfaces; such applications include RStudio (an integrated development environment) and Jupyter (a notebook interface)."

str_extract(R_wiki, "programming language")
str_extract_all(R_wiki, "programming language")
str_extract_all(R_wiki, "programming language", simplify = T) # 행렬 형태로 출력

# 첫 문자가 대문자로 시작되는 단어들을 추출
myextract <- str_extract_all(R_wiki, "[[:upper:]]{1}[[:alpha:]]{0,}")
myextract
table(myextract)

# str_match()와 str_match_all(): 복잡하게 지정된 표현을 분해하여 추출하는 함수
str_match(R_wiki, "programming language")
str_match_all(R_wiki, "programming language")

str_match(R_wiki, "([[:alpha:]]{1,}) and ([[:alpha:]]{1,})")

university_address <- c("연세대학교 주소는 서울시 서대문구 연세로 50번지다",
                        "서울대 주소: 서울시 관악구 관악로 1번지다",
                        "고려대는 서울시 성묵구 안암로 145번지에 있다",
                        "카이스트 주소, 대전시 유성구 대학로 291번지",
                        "포항시 남구 청암로 77번지는 포항공과 대학교 주소임")
style_address <- "([[:alpha:]]{1,}시) ([[:alpha:]]{1,}구) ([[:alpha:]]{1,}로) ([[:digit:]]{1,}번지)"
str_match(university_address, style_address)

myaddress <- data.frame(str_match(university_address, style_address))
names(myaddress) <- c("full_address", "city", "district", "road", "street")
myaddress
table(myaddress$city)

# str_locate()와 str_locate_all() 함수: 지정된 위치를 출력하는 함수
str_locate(R_wiki, "programming language")
str_locate_all(R_wiki, "programming language")

mylocate <- str_locate_all(R_wiki, "[[:upper:]]{1}[[:alpha:]]{0,}")
mylocate
dim(mylocate[[1]])

mydata <- data.frame(mylocate[[1]])
myextract <- str_extract_all(R_wiki, "[[:upper:]]{1}[[:alpha:]]{0,}")

mydata$myword <- myextract[[1]]
mydata$myword_length <- mydata$end - mydata$start + 1
head(mydata)

# str_split()와 str_split_fixed(): 지정된 표현을 이용하여 텍스트 데이터를 분해하기
R_wiki_para <- str_split(R_wiki, "\n") # 문단으로 분해
R_wiki_para

R_wiki_sent <- str_split(R_wiki_para[[1]], "\\.") # 문장으로 분해
R_wiki_sent

my2sentences <- unlist(R_wiki_sent)[c(4, 7)]
my2sentences

mylength1 <- length(unlist(str_split(my2sentences[1], " ")))
mylength2 <- length(unlist(str_split(my2sentences[2], " ")))
mylength1; mylength2

myfixed_short <- str_split_fixed(my2sentences, " ", 21) # 각 문장에 공란을 기준으로 단어 단위로 구분
myfixed_short

length_sentences <- rep(NA, length(unlist(R_wiki_sent))) # NA 13개
length_sentences

for (i in 1:length(length_sentences)){
  length_sentences[i] <- length(unlist(str_split(unlist(R_wiki_sent[i]), " ")))
}
length_sentences

max_length_sentences <- max(length_sentences) # 58

sent_word_matrix <- str_split_fixed(unlist(R_wiki_sent), " ", max_length_sentences) # 최대 단어 수 기준 문장X단어 행렬
mydata <- data.frame(sent_word_matrix)
rownames(mydata) <- paste('sent', 1:length(unlist(R_wiki_sent)), sep = '.')
colnames(mydata) <- paste('word', 1:max_length_sentences, sep = '.')
mydata
mydata[, 1] # 각 문장의 첫 번째 문자
mydata[3, 1:10] # 세 번째 문장에서 첫 번째부터 열 번째까지의 문자

# str_detect() : 지정된 표현 등장 여부 확인
grepl("language", R_wiki_sent)
str_detect(R_wiki_sent, "language") # 벡터 단위로 입력을 하지 않아 에러 발생

# 각 문장에 적용1
str_detect(unlist(R_wiki_sent), "language")

# 각 문장에 적용2
fun_language_detection <- function(x){str_detect(x, "language")}
lapply(R_wiki_sent, FUN = fun_language_detection)

# 각 문장에 적용3
whether_pattern <- R_wiki_sent
for (i in 1:4) {
  whether_pattern[[i]] <- str_detect(R_wiki_sent[[i]], "language")
}
whether_pattern

# str_subset와 str_which(): 지정된 표현이 등장하는 텍스트 추출 및 위치 확인
str_subset(R_wiki_sent[[1]], "data")
grep("data", R_wiki_sent[[1]]) # 인덱스 출력

R_wiki_sent[[1]][grep("data", R_wiki_sent[[1]])] # str_subset(R_wiki_sent[[1]], "data") 과 동일

str_which(R_wiki_sent[[2]], "data") # grep() 와 동일
grep("data", R_wiki_sent[[2]])

# str_starts()와 str_ends(): 지정된 표현으로 시작 혹은 종료된 표현 추출
str_starts(R_wiki_sent[[1]], "A|An|The")
R_wiki_sent[[2]][str_ends(R_wiki_sent[[2]], "[aeiou][[:graph:]]{1,}")] # 모음으로 시작하는 단어로 종료된 경우

# str_count(): 텍스트 데이터에서 지정된 표현의 빈도 계산
# 'R' 이라는 표현이 등장하는가?
str_count(R_wiki, "R")
str_count(R_wiki_para[[1]], "R")
str_count(unlist(R_wiki_sent), "R")

str_count(unlist(R_wiki_sent), "R.{1,}dat[[:lower:]]{1,}") # R이라는 단어가 등장한 후에 dat로 시작하는 단어가 등장하는 빈도
unlist(R_wiki_sent)
str_count(unlist(R_wiki_sent), "R.{1,}(d|D)at[[:lower:]]{1,}")

str_count(unlist(R_wiki_sent), "R{1}[^R]{1,}(d|D)at[[:alpha:]]{1,}") # R과 dat 사이레는 'R'이 절대 들어가면 안 됨.

# str_sub(): 지정된 위치의 텍스트 출력
str_sub(R_wiki_sent, 1, 10)

fun_substring10 <- function(x){str_sub(x, 1, 10)}
lapply(R_wiki_sent, fun_substring10)

first10chars <- R_wiki_sent
for (i in 1:length(R_wiki_sent)){
  first10chars[[i]] <- str_sub(R_wiki_sent[[i]], 1, 10)
}
first10chars

# str_replace와 str_replace_all(): 지정된 표현을 다른 표현으로 교체
str_replace(R_wiki, "R", "Rstudio") # 처음으로 등장한 지정된 표현만 변경
str_replace_all(R_wiki, "R", "Rstudio") # 등장한 모든 표현들을 변경

temp <- str_replace_all(R_wiki, "R", "Rstudio")
table(str_extract_all(R_wiki, "R|studio|Rstudio"))
table(str_extract_all(temp, "Rstudio"))

temp <- str_replace_all(R_wiki, "R\\b", "R_computer.language_")
temp <- str_replace_all(temp, "C\\b", "C_computer.language_")
table(str_extract_all(temp, "[[:alnum:]]{1}_computer.language_")) # _computer.language_ 표현이 붙은 부분에는 어떤 단어들이 있고, 빈도는 어떠한가?

# str_remove()와 str_remove_all(): 지정된 표현 제거
str_remove_all(R_wiki_sent[[1]],"(\\b(a|A) )|(\\b(a|A)n )|(\\b(t|T)he )") # 관사 제거
str_replace_all(R_wiki_sent[[1]], "((a|A) )|((a|A)n )|((t|T)he )", "")

# str_c()와 str_flatten(): 지정한 표현들 연결
str_c(unlist(R_wiki_sent), collapse = ". ")
str_c(unlist(R_wiki_sent), collapse = ". ") == paste(unlist(R_wiki_sent), collapse = ". ")

str_c(unlist(R_wiki_sent), collapse = ". ") == str_flatten(unlist(R_wiki_sent), collapse = ". ")
str_c(unlist(R_wiki_sent)) == str_flatten(unlist(R_wiki_sent))

str_flatten(R_wiki_sent[[1]])
str_c(R_wiki_sent[[1]])

# str_length(): 지정된 표현의 글자 수 계산
str_length(unlist(R_wiki_sent))
nchar(unlist(R_wiki_sent))

# str_dup(): 지정된 표현 반복하기
str_dup("software", 3)
rep("software", 3)

# str_pad()와 str_trunc(): 지정된 문자 수에 맞도록 공란 투입 혹은 잘라내기
name <- c("Joe", "Jack", "Jackie", "Jefferson")
donation <- c("$1", "$11", "$111", "$1111")
mydata <- data.frame(name, donation)
mydata

name2 <- str_pad(mydata$name, width = 15, side = 'right', pad = " ")
donation2 <- str_pad(mydata$donation, width = 15, side = 'both', pad = "~")
mydata2 <- data.frame(name2, donation2)
mydata2
str_length(mydata$name[1])
str_length(mydata2$name2[1])

str_trunc(R_wiki_sent[[1]], width = 20, side = 'left')
str_trunc(R_wiki_sent[[1]], width = 20, side = 'right')
str_trunc(R_wiki_sent[[1]], width = 20, side = 'center')

# str_trim()와 str_squish(): 불필요 공란 제거
mywhitespaces <- "\n\t In this text, there are too many unnecessary    whitespaces. \n\t"
mywhitespaces

str_trim(mywhitespaces, side = "both")
str_trim(mywhitespaces, side = "right")
str_trim(mywhitespaces, side = "left")

str_squish(mywhitespaces)

# str_glue()와 str_glue_data(): 지정된 양식의 텍스트에 원하는 텍스트 삽입
Name <- "김철수"
Date <- as.Date("2019-10-30")
Product <- "세텍 세제"
str_glue("안녕하세요, {Name} 고객님! 고객님께서는 {format(Date, '%Y년 %m월 %d일')} {Product} 구매하셨습니다. 좋은 상품 빠른 배송으로 고객님의 삶에 도움이 되겠습니다. 감사합니다.")

Name <- c("김철수", "이영희")
Date <- as.Date(c("2019-10-30", "2019-11-3"))
Product <- c("세텍 세제", "도자기 세트")
myDB <- data.frame(Name, Date, Product)
str_glue("안녕하세요, {myDB$Name} 고객님! 고객님께서는 {format(myDB$Date, '%Y년 %m월 %d일')} {myDB$Product} 구매하셨습니다. 좋은 상품 빠른 배송으로 고객님의 삶에 도움이 되겠습니다. 감사합니다.")

# str_order()와 str_sort(): 벡터 형식 텍스트를 알파벳 순서대로 정렬
str_order(unlist(R_wiki_sent))
str_sort(unlist(R_wiki_sent))
