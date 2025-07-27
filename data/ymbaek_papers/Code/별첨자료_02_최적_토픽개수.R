#######################
#최적의 토픽개수 탐색##
#######################

# setwd("D:/data/code")

#앞서 작업한 DTM불러오고, tm, stringr, ggplot2 패키지 구동
dtm.e <- readRDS("dtmE.RData")
dtm.k <- readRDS("dtmK.RData")
library('tm')
library('stringr')
library('ggplot2')

#최적의 잠재토픽 개수 탐색
library("ldatuning")
English_K_3_21_by3 = FindTopicsNumber(dtm.e,topics=seq(from=3,to=21,by=3),
                                      metrics=c("Griffiths2004", "CaoJuan2009", "Arun2010", "Deveaud2014"),
                                      method="Gibbs",
                                      control=list(seed=20191115))
English_K_3_21_by3
#5-10 범위 사이에 최적 토픽개수가 존재할 듯 
FindTopicsNumber_plot(English_K_3_21_by3)
# setwd("D:/data/Fig"); ggsave("Figure_appendix_02_01.jpg",width=18,height=14,unit='cm')

English_K_5_10_by1 = FindTopicsNumber(dtm.e,topics=seq(from=5,to=10,by=1),
                                      metrics=c("Griffiths2004", "CaoJuan2009", "Arun2010", "Deveaud2014"),
                                      method="Gibbs",
                                      control=list(seed=20191115))
English_K_5_10_by1
#7개가 가장 적절해 보임. 
FindTopicsNumber_plot(English_K_5_10_by1)
# setwd("D:/data/Fig"); ggsave("Figure_appendix_02_02.jpg",width=18,height=14,unit='cm')

Korean_K_3_21_by3 = FindTopicsNumber(dtm.k,topics=seq(from=3,to=21,by=3),
                                     metrics=c("Griffiths2004", "CaoJuan2009", "Arun2010", "Deveaud2014"),
                                     method="Gibbs",
                                     control=list(seed=20191115))
#5-10 범위 사이에 최적 토픽개수가 존재할 듯 
FindTopicsNumber_plot(Korean_K_3_21_by3)
# setwd("D:/data/Fig"); ggsave("Figure_appendix_02_03.jpg",width=18,height=14,unit='cm')

Korean_K_5_10_by1 = FindTopicsNumber(dtm.k,topics=seq(from=5,to=10,by=1),
                                     metrics=c("Griffiths2004", "CaoJuan2009", "Arun2010", "Deveaud2014"),
                                     method="Gibbs",
                                     control=list(seed=20191115))
#7개가 가장 적절해 보임. 
FindTopicsNumber_plot(Korean_K_5_10_by1)
# setwd("D:/data/Fig"); ggsave("Figure_appendix_02_04.jpg",width=18,height=14,unit='cm')
