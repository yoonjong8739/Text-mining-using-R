# letters[] 와 LETTERS[]: 알파벳의 대소문자 표현 함수
# 알파벳 출력 함수
letters[3]
LETTERS[3]
letters[1:26]
LETTERS[1:26]

# tolower()와 toupper(): 알파벳의 대소문자 전환
tolower("Eye for eye")
toupper("Eye for eye")

# nchar(): 문자 수와 바이트 수 세기
nchar("Korea")
nchar("Korea", type = "bytes")
nchar("한국")
nchar("한국", type = "bytes")

# 공란이 있으면 다르게 취급
nchar("Korea")
nchar("Korea ")

nchar("Korea\t")
nchar("Korea\t", type = "bytes")
nchar("Korea, Republic of")
nchar("Korea, 
Republic of")
nchar("Korea, \nRepublic of")

# strsplit()와 paste(): 하위 텍스트 오브젝트의 분해와 합치기
# 단어 단위로 문장을 분해하는 방법
mysentense <- "Learning R is so interesting"
strsplit(mysentense, split = " ")

# 문자 단위로 단어를 분해하는 방법
mywords <- strsplit(mysentense, split = " ")
strsplit(mywords[[1]][5], split = "")

# 각 단어들이 어떤 문자들로 구성되었는지 표현
myletters <- list(rep(NA, 5))
for (i in 1:5){
  myletters[i] <- strsplit(mywords[[1]][i], split = "")
}
myletters

# 문자들을 다시 합쳐서 단어로 구성
paste(myletters[[1]], collapse = "")
paste(myletters[[1]], collapse = "-")

mywords2 <- list(rep(NA, 5))
for (i in 1:5){
  mywords2[i] <- paste(myletters[[i]], collapse = "")
}
mywords2

paste(mywords2, collapse = " ")

# 위키피디아 텍스트
R_wiki <- "R is a programming language for statistical computing and data visualization. It has been widely adopted in the fields of data mining, bioinformatics, data analysis, and data science.
The core R language is extended by a large number of software packages, which contain reusable code, documentation, and sample data. Some of the most popular R packages are in the tidyverse collection, which enhances functionality for visualizing, transforming, and modelling data, as well as improves the ease of programming (according to the authors and users).
R is free and open-source software distributed under the GNU General Public License. The language is implemented primarily in C, Fortran, and R itself. Precompiled executables are available for the major operating systems (including Linux, MacOS, and Microsoft Windows).
Its core is an interpreted language with a native command line interface. In addition, multiple third-party applications are available as graphical user interfaces; such applications include RStudio (an integrated development environment) and Jupyter (a notebook interface)."

R_wiki_para <- strsplit(R_wiki, split = "\n")
R_wiki_para

# 문장 단위로 분해
R_wiki_sent <- strsplit(R_wiki_para[[1]], split = "\\.")
R_wiki_sent

# 문단 > 문장 > 단어 단위로 분해
R_wiki_word <- list(rep(NA, 4))
for (i in 1:4){
  R_wiki_word[[i]] <- strsplit(R_wiki_sent[[i]], split = " ")
}
R_wiki_word
R_wiki_word[[3]][[2]][3]

# regexpr(), gregexpr(), regexec(): 텍스트 데이터에서 특정 표현의 위치정보
mysentence <- "Learning R is so interesting"
regexpr("ing", mysentence) # 등장한 문자열 위치, 글자수, 바이트로 표시

loc.begin <- as.vector(regexpr("ing", mysentence))
loc.begin

loc.length <- as.vector(attr(regexpr("ing", mysentence), "match.length"))
loc.length

loc.end <- loc.begin + loc.length - 1
loc.end

# gregexpr()는 여러 번 등장하는 표현을 모두 찾아냄
gregexpr("ing", mysentence)
length(gregexpr("ing", mysentence)[[1]]) # 몇 번 등장하는지

loc.begin <- as.vector(gregexpr("ing", mysentence)[[1]]) # 시작 위치
loc.begin

loc.length <- as.vector(attr(gregexpr("ing", mysentence)[[1]], "match.length")) # 길이
loc.length

loc.end <- loc.begin + loc.length - 1 # 끝 위치
loc.end

# 간단한 표현을 쓸 경우 regexec와 regexpr은 거의 동일
regexpr("interesting", mysentence)
regexec("interestin(g)", mysentence) # 해당 단어 시작점과 끝점 위치를 알려줌

# 원하는 부분이 3개 이상일 경우 유용
regexec("so (interestin(g))", mysentence) # 첫 번째는 전체 표현, 두 번째는 괄호 안의 표현, 세 번째는 괄호 안의 표현의 끝점

mysentences <- unlist(R_wiki_sent)
mysentences

regexpr("software", mysentences) # 문장 단위로 'software'가 등장하는 위치를 찾음
gregexpr("software", mysentences) # 문장 단위로 'software'가 등장하는 위치를 찾음(2회 이상)

# 시작과 종료 위치를 정리하는 방법
mytemp <- regexpr("software", mysentences)
my.begin <- as.vector(mytemp)
#my.begin
my.begin[my.begin == -1] <- NA # -1인 경우는 해당 표현이 없는 경우이므로 NA로 대체
my.end <- my.begin + as.vector(attr(mytemp, "match.length")) - 1 # 시작 위치 + 길이 - 1
mylocs <- matrix(NA, nrow = length(my.begin), ncol = 2) # 시작과 종료 위치를 문장의 수만큼 확정할 수 있는 행렬 데이터
colnames(mylocs) <- c("begin", "end") # 열 이름 지정
rownames(mylocs) <- paste("sentence", seq_along(my.begin), sep = ".") # 행 이름 지정
for (i in seq_along(my.begin)) {
  mylocs[i, ] <- cbind(my.begin[i], my.end[i])
}
mylocs

# grep()와 grepl(): 특정 표현이 텍스트 데이터에서 등장하는지 확인
# grep()은 해당 표현이 등장하는 위치를 알려줌
# grepl()은 해당 표현이 등장하는지 여부를 TRUE/FALSE로 알려줌
grep("software", mysentences) # 'software'가 등장하는 문장 위치
grepl("software", mysentences) # 'software'가 등장하는지 여부
