#################################################
#제 4부 마무리################################### 
#01 R을 활용한 온라인 데이터 수집(scraping) 소개#
#################################################

#Selenium을 R환경에 맞도록 구성한 RSelenium 인스톨 및 구동 
library("RSelenium") #웹페이지의 정보수집 
library('rvest')     #수집된 웹페이지 정보 탐색 및 저장 
library('tidyverse')

#RSelenium에서는 Docker를 설치하고 서버를 구성하라고 하지만, 
#개인이용자에게는 자신의 PC를 서버로 설정해서 사용하는 것이 더 편할 듯 
#rsDriver() 함수를 이용하여 R을 통해 통제가능한 크롬 브라우저를 띄움
#브라우저는 크롬이 아니라 Firefox, Internet Explorer, phantomjs 등도 가능 
#[여기서 phantomjs는 phantom(안보임)+js(자바)를 의미하며, 일반이용자에게는 비추천]
#아래를 설치하기 전에 반드시 확인할 것 2가지 
#첫째, 본인 PC에 Java가 설치되어 있는가?
#(첫째 조건 관련, 자바설치에 대해서는 별첨자료-1 참조)
#둘째, 본인 PC의 크롬 버전이 rsDriver() 함수가 지원하는 버전인가? 
#(둘째 조건 관련 브라우저의 버전을 확인한 후, 아래를 실행하여 확인해 볼 것)
#(버전이 맞지 않는 브라우저를 사용하는 경우 버전에 맞는 브라우저를 먼저 설치)
binman::list_versions("chromedriver")  
rD = rsDriver(browser="chrome",   #
              chromever="78.0.3904.105",  #브라우저 버전 
              port=4444L)  #포트 넘버는 4445 등으로 조금 다를 수 있음(L은 정수표현 의미)

#rD에는 서버, 클라이언트 두 가지 저장. 여기서는 client 를 선택하자. 
names(rD)
remDr = rD[["client"]]

#R을 이용해 웹페이지를 방문해 보자. 
#dbpia.co.kr의 검색란에 저자 이름을 넣은 후 URL의 구조를 살펴보자. 
#크게 다음과 같은 3개 부분으로 구성된 것을 알 수 있다. 
start_chars <- "http://www.dbpia.co.kr/search/topSearch?startCount=0&collection=ALL&range=A&searchField=ALL&sort=RANK&query="
search_term <- "백영민"
end_chars <- "&srchOption=*"
#위의 3가지 문자형들을 통합하면 아래와 같은 URL을 만들 수 있다. 
#필요에 따라 search_term을 다르게 지정하면 바뀐 검색어로 검색이 될 것이다.
#예를 들어 search_term <- "임진왜란" 과 같이 다르게 설정하면 임진왜란을 다루는
#한국어 학술논문들 검색결과를 얻을 수 있다. 
example_dbpia_search <- str_c(start_chars,search_term,end_chars)
#해당 URL을 앞서 이용자 PC에 설치한 서버를 경유하는 크롬 브라우저를 이용해 검색하자 
remDr$navigate(example_dbpia_search)

#여기까지의 검색된 논문들 내역(소스, source) 저장  
search_baek_dbpia = remDr$getPageSource()
class(search_baek_dbpia)
#벡터형태로 
search_baek_dbpia <- search_baek_dbpia[[1]]
class(search_baek_dbpia)
nchar(search_baek_dbpia)
substr(search_baek_dbpia,1,200)

#위의 자료는 텍스트 데이터이기 때문에 다음과 같은 방식도 가능은 하다(절대 권하지 않음)
str_extract_all(search_baek_dbpia,"백영민")
str_extract_all(search_baek_dbpia,
                "([[:alpha:]]|[[:space:]]){1,}팩트체크([[:alpha:]]|[[:space:]]){1,}")

#이 자료는 HTML 형식으로 구성된 자료이기 때문에 rvest 패키지 부속함수들을 이용하면 편함 
html_search_baek_dbpia = read_html(search_baek_dbpia)
html_search_baek_dbpia %>%
  html_node("#dev_search_list") %>% #검색된 목록을 찾아
  head()
html_search_baek_dbpia %>%
  html_node("#dev_search_list") %>% #검색된 목록을 찾아
  html_nodes(".titWrap a")   %>%    #논문제목에 해당되는 내용 
  head()
urls_beak_papers = html_search_baek_dbpia %>%
  html_node("#dev_search_list") %>% #검색된 목록을 찾아
  html_nodes(".titWrap a") %>%      #논문제목에 해당되는 내용 
  html_attr("href") #이동되는 URL만 끌어냄 
head(urls_beak_papers)
#만약 논문제목만을 모으는 것으로 끝내고 싶다면 
title_beak_papers = html_search_baek_dbpia %>%
  html_node("#dev_search_list") %>% #검색된 목록을 찾아
  html_nodes(".titWrap a") %>%      #논문제목에 해당되는 내용 
  html_text() #제시된 텍스트만 추출함 
head(title_beak_papers)

#총 10개의 논문들이 검색된 것을 알 수 있다. 
#해당 URL의 맨 마지막을 보면 <더보기>라는 란이 있다. 이 버튼을 R을 이용해 눌러보자. 
#css selector에 해당되는 요소들 중(using 옵션 부분)
#paginate 라는 이름의 css 버튼의 확장기능을 확정 
button_SeeMore <- remDr$findElement(using="css selector",
                                    ".paginate span") #paginate 앞의 .은 전체선택자 
#아래를 실행하면 “더보기” 클릭을 실행한다. 
button_SeeMore$clickElement() 

#여기까지의 검색된 논문들 내역(소스, source) 저장  
search_baek_dbpia = remDr$getPageSource()
search_baek_dbpia <- search_baek_dbpia[[1]]  #벡터로 
urls_beak_papers = read_html(search_baek_dbpia) %>%
  html_node("#dev_search_list") %>% 
  html_nodes(".titWrap a") %>%  
  html_attr("href") 
length(urls_beak_papers)

#첫번째 검색문서의 URL을 살펴보자 
urls_beak_papers[1]
#URL의 앞부분이 제시되지 않은 것을 발견할 수 있다.
#왜냐하면 "http://www.dbpia.co.kr"을 공유하기 때문이다. 
#따라서 다음과 같이 하면 첫번째 검색문서를 R을 통해 열어볼 수 있다. 
remDr$navigate(str_c("http://www.dbpia.co.kr",urls_beak_papers[1]))

#첫번째 검색된 논문의 소스정보 
baek_dbpia_01 <- remDr$getPageSource()[[1]]
#해당 소스정보에서 필요한 정보들 취합
author <- read_html(baek_dbpia_01) %>%
  html_node("p.author") %>% html_text() 
author
year <- read_html(baek_dbpia_01) %>%
  html_node("li.date") %>% html_text() 
titleK <- read_html(baek_dbpia_01) %>%
  html_node("span.articleTitle") %>% html_text() 
titleE <- read_html(baek_dbpia_01) %>%
  html_node("span.equalTitle") %>% html_text() 
journal <- read_html(baek_dbpia_01) %>%
  html_node("li.journal") %>% html_text() 
vol_no <- read_html(baek_dbpia_01) %>%
  html_node("li.volume") %>% html_text() 
page <- read_html(baek_dbpia_01) %>%
  html_node("li.page") %>% html_text() 
abstract <- read_html(baek_dbpia_01) %>%
  html_node("div#pub_abstract") %>% html_text() 
#사전처리 
author <- str_squish(author)  #불필요 공란 제거 
author
year 
year <- str_sub(year,1,4)  #년도 정보만 추출 
year 
titleK  #사전처리 필요없음 
titleE  #사전처리 필요없음 
journal  #사전처리 필요없음  
vol_no
vol <- str_extract_all(vol_no, "[[:digit:]]{1,}")[[1]][1] #첫 번째로 등장하는 숫자  
no <- str_extract_all(vol_no, "[[:digit:]]{1,}")[[1]][2] #두 번째로 등장하는 숫자  
vol; no
page
page_start <- str_extract_all(page, "[[:digit:]]{1,}")[[1]][1] #첫 번째로 등장하는 숫자  
page_end <- str_extract_all(page, "[[:digit:]]{1,}")[[1]][2]  #두 번째로 등장하는 숫자  
page_start; page_end
abstract  #처음 텍스트는 더보기를 누르기 이전, 두번째 텍스트와 세 번째 텍스트는 더보기를 누른 후 
abstract <- str_split(abstract,"(\t더보기\n)|(\t닫기\n)")[[1]][2] #세 가지 텍스트를 구분 
abstract 
abstract <- str_trim(abstract, "both")  #불필요 공란 제거 
abstract
abstract <- str_remove_all(abstract,"\t|\n") #탭 공란, 줄바꿈 제거 
abstract
abstract <- str_split(abstract,"[[:space:]]{4,}")  #연속된 스페이스 공란 중심으로 텍스트 구분 
abstract
abstractK <- abstract[[1]][1]
abstractE <- abstract[[1]][2]
abstractK
abstractE

mydata <- tibble(author,year,titleK,titleE,journal,
                 vol,no,page_start,page_end,
                 abstractK,abstractE)
mydata

myscraping <- list() 
for (i in 1:30){
  remDr$navigate(str_c("http://www.dbpia.co.kr",urls_beak_papers[i]))
  #i번째 검색된 논문의 소스정보 
  baek_dbpia <- remDr$getPageSource()[[1]]
  #해당 소스정보에서 필요한 정보들 취합
  author <- read_html(baek_dbpia) %>%
    html_node("p.author") %>% html_text() 
  year <- read_html(baek_dbpia) %>%
    html_node("li.date") %>% html_text() 
  titleK <- read_html(baek_dbpia) %>%
    html_node("span.articleTitle") %>% html_text() 
  titleE <- read_html(baek_dbpia) %>%
    html_node("span.equalTitle") %>% html_text() 
  journal <- read_html(baek_dbpia) %>%
    html_node("li.journal") %>% html_text() 
  vol_no <- read_html(baek_dbpia) %>%
    html_node("li.volume") %>% html_text() 
  page <- read_html(baek_dbpia) %>%
    html_node("li.page") %>% html_text() 
  abstract <- read_html(baek_dbpia) %>%
    html_node("div#pub_abstract") %>% html_text() 
  #사전처리 
  abstract <- str_split(abstract,"(\t더보기\n)|(\t닫기\n)")[[1]][2] 
  abstract <- str_trim(str_split(abstract,"(\t더보기\n)|(\t닫기\n)"),"both")
  abstract <- str_remove_all(abstract,"\t|\n")
  abstract <- str_split(abstract,"[[:space:]]{4,}")
  mydata <- tibble(authors=str_squish(author),
                   pubyear=str_sub(year,1,4),
                   titleK=titleK,titleE=titleE,journal=journal,
                   vol=str_extract_all(vol_no, "[[:digit:]]{1,}")[[1]][1],
                   no=str_extract_all(vol_no, "[[:digit:]]{1,}")[[1]][2],
                   page_start=str_extract_all(page, "[[:digit:]]{1,}")[[1]][1],
                   page_end=str_extract_all(page, "[[:digit:]]{1,}")[[1]][2],
                   abstractK=abstract[[1]][1],abstractE=abstract[[1]][2])
  myscraping = bind_rows(myscraping,mydata) #계속 추가함
  myscraping
}
myscraping
#이제 R로 통제하는 크롬 브라우저를 닫아보자. 
remDr$close()
#또한 Rselenium 도 종료시키자. 
rD$server$stop()
rm(rD,remDr)
#이제 원하는 분석을 실시하면 된다. 예를 들어 등장학술지의 빈도표는 아래와 같다. 
myscraping %>% count(journal)
#학술지를 기준으로 동명이인의 연구업적은 데이터에서 배제 
mydata <- myscraping %>% 
  filter(str_detect(journal,"경찰|광고|커뮤니케이션|언론|치안"))
mydata 
#주저자 혹은 단독저자인 논문의 빈도는? 
mydata %>% 
  mutate(role = ifelse(substr(authors,1,3)=="백영민","main","sub")) %>% 
  count(role)
           

