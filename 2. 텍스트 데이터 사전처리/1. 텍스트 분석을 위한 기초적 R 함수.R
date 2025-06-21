## list(): 리스트 형식의 오브젝트
# 리스트와 벡터의 구분
myvector <- c(1:6, 'a') # 단 하나의 속성으로만 표현 가능(원자적 특성).
myvector

mylist <- list(1:6, 'a') # 여러 형식의 데이터 포함
mylist

# list 형식의 오브젝트 소개
obj1 <- 1:4
obj2 <- 6:10

# 간단한 리스트 형식의 오브젝트는 다음과 같다.
obj3 <- list(obj1, obj2)
obj3

# list 함수는 연이어 사용할 수도 있다.
mylist <- list(obj1, obj2, obj3)
mylist

mylist[[3]][1]
mylist[[3]][[1]]
mylist[[3]][[1]][2]

# unlist 함수는 리스트를 벡터 형식으로 돌려주는 역할을 한다.
myvector <- c(1:6, 'a')
mylist <- list(1:6, 'a')
unlist(mylist)
unlist(mylist) == myvector

mean(mylist[[1]][1:6])
mean(unlist(mylist)[1:6])

# 텍스트형 자료(문자 -> 단어)
name1 <- "Donald"
myspace <- " "
name2 <- "Trump"
list(name1, myspace, name2)
unlist(list(name1, myspace, name2))

## attr(): 오브젝트에 속성값을 입력하거나 추출하기
name <- c('갑', '을', '병', '정')
gender <- c(2,1,1,2)
mydata <- data.frame(name, gender)
attr(mydata$name, "what the variable means") <- "응답자의 이름"
mydata$name

attr(mydata$gender, "what the variable means") <- "응답자의 성별"
myvalues <- gender
for (i in 1:length(gender)) {
  myvalues[i] <- ifelse(gender[i] == 1, "남성", "여성")
}
attr(mydata$gender, "what the value means") <- myvalues
mydata$gender

# 속성값 추출
attr(mydata$gender, "what the value means")

# 속성갑슬 추출한 후 mydata에 새로운 변수로 추가
mydata$gender.charactor <- attr(mydata$gender, "what the value means")
mydata

## lapply(), sapply(), tapply(): 변용된 apply() 함수들
mylist <- list(1:4, 6:10, list(1:4, 6:10))
mylist

lapply(mylist[[3]], mean)
lapply(mylist, mean)
lapply(mylist[c(1, 2, c(1,2))], mean) # 어떤 형식으로 구성되어 있는지 밝히면 됨.

# sapply 함수의 경우 lapply 함수 결과와 유사하지만 결괏값에 unlist 함수를 적용한 것과 동일.
sapply(mylist[c(1,2, c(1,2))], sum)
unlist(lapply(mylist[c(1, 2, c(1,2))], sum))

# tapply는 텍스트 데이터에서 종종 사용됨.
wordlist <- c("the", "is", "a", "the")
doc1freq <- c(3,4,2,4)
doc2freq <- rep(1,4)
tapply(doc1freq, wordlist, length)
tapply(doc2freq, wordlist, length)
tapply(doc1freq, wordlist, sum)
tapply(doc2freq, wordlist, sum)

# 다음과 같은 단어들의 연쇄로 구성된 3문장을 생각해보자.
sent1 <- c("earth", "to", "earth")
sent2 <- c("ashes", "to", "ashes")
sent3 <- c("dust", "to", "dust")

myfreq <- c(rep(1, length(sent1)), rep(1, length(sent2)), rep(1, length(sent3)))
tapply(myfreq, c(sent1, sent2, sent3), sum)
