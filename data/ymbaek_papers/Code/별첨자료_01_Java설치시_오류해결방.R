#별첨자료-1: Java 설치방법들

#rJava 패키지 설치후 error: JAVA_HOME cannot be determined 에러 발생시 

#현재 R에서의 설정환경 먼저 확인
Sys.getenv("JAVA_HOME")
#Java가 어디 설치되어 있는지 확인한 후, 해당경로에 맞도록 변경 설정 
Sys.setenv("JAVA_HOME"="C:/Program Files/Java/jdk-11.0.5")
#설정된 경로가 맞는지 확인 
Sys.getenv("JAVA_HOME")

#향후의 설정을 계속 유지하고자 한다면 
profiled = "Sys.setenv(JAVA_HOME='C:/Program Files/Java/jdk-11.0.5')"
write(profiled, "~/.Rprofile")
readLines("~/.Rprofile", warn = FALSE)


#installr 패키지 이용하여 설치 
library("installr") #install.packages("installr")를 먼저 실행할 것 
install.java()
Sys.getenv("JAVA_HOME")


#multilinguer 패키지 이용하여 설치
remotes::install_github("mrchypark/multilinguer") #install.packages("remotes")를 먼저 실행할 것

library("multilinguer")
has_java()
#java version "11.0.5" 2019-10-15 LTS
#Java(TM) SE Runtime Environment 18.9 (build 11.0.5+10-LTS)
#Java HotSpot(TM) 64-Bit Server VM 18.9 (build 11.0.5+10-LTS, mixed mode)
#[1] TRUE
