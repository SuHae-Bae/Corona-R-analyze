install.packages(c("RCurl", "XML"))
library(RCurl)
library(XML)

t <- readLines('https://en.wikipedia.org/wiki/Coronavirus_disease_2019')

d <- htmlParse(t, asText = TRUE)
clean_doc <- xpathSApply(d,"//p",xmlValue)

# 전처리
install.packages(c("tm", "SnowballC"))
library(tm)
library(SnowballC)

doc <- Corpus(VectorSource(clean_doc))
inspect(doc)

doc <- tm_map(doc, content_transformer(tolower))
doc <- tm_map(doc, removeNumbers)
doc <- tm_map(doc, removeWords, stopwords('english'))
doc <- tm_map(doc, removePunctuation)
doc <- tm_map(doc, stripWhitespace)

dtm = DocumentTermMatrix(doc)
dim(dtm)
inspect(dtm)


install.packages("wordcloud")
library(wordcloud)
m <- as.matrix(dtm)
v <- sort(colSums(m), decreasing = TRUE)
d <- data.frame(word = names(v), freq = v)
wordcloud(words = d$word, freq = d$freq, min.freq = 1, max.words = 100, random.order = FALSE, rot.per = 0.35)

library(RColorBrewer)
pal <- brewer.pal(11, "Spectral")
wordcloud(words = d$word, freq = d$freq, min.freq = 1, max.words = 50, random.order = FALSE,
          rot.per = 0.50, colors = pal)
wordcloud(words = d$word, freq = d$freq, min.freq = 1, max.words = 50, random.order = FALSE,
          rot.per = 0.50, colors = pal, family = "mono", font = 2)

install.packages('wordcloud2')
library(wordcloud2)
wordcloud2(d)
# 별모양으로 만들기
d1 <- d[1:200,]
wordcloud2(d1, shape = 'star')
# 단어 방향 범위 지정
wordcloud2(d1, minRotation = pi/4, maxRotation = pi/4, rotateRatio = 1.0)

# 데이터 분석을 위해 데이터 로드
kr_regional_daily = read.csv('./kr_regional_daily.csv', fileEncoding = "utf-8")
kr_regional_daily
kr_daily = read.csv("./kr_daily.csv", fileEncoding = "utf-8")
kr_daily

# 데이터 전처리
kr_daily$date<-as.Date(as.character(kr_daily$date),format="%Y%m%d")
kr_regional_daily$date<-as.Date(as.character(kr_regional_daily$date), format="%Y%m%d")
# kr_daily <- rename(kr_daily, "month" = "date")

# 수치 요약
str(kr_daily)
summary(kr_daily)
str(kr_regional_daily)
summary(kr_regional_daily)

install.packages('ggplot2')
library('ggplot2')
# 전체 누적 확진자 그래프
ggplot(data = kr_daily, aes(x = date, y = confirmed)) + geom_col() + xlab("month")+ ylab("누적 확진자")+ ggtitle("누적 확진자수")+ theme(plot.title = element_text(
  face="bold",          # bold=굵게, italic=기울임
  hjust=0.5,            # 0=왼쪽, 0.5=가운데, 1=오른쪽
  vjust=1,              # 기본위치 0을 기준으로 0보다 작으면 아래쪽, 0보다 크면 위쪽으로 이동
  size=20,              # 글자크기
  color="#ff6600"))

# 누적 확진자 그래프_선
ggplot(data = kr_daily, aes(x = date, y = confirmed)) + geom_line() + xlab("month")+ ylab("누적 확진자")+ ggtitle("누적 확진자수")+ theme(plot.title = element_text(
  face="bold",          # bold=굵게, italic=기울임
  hjust=0.5,            # 0=왼쪽, 0.5=가운데, 1=오른쪽
  vjust=1,              # 기본위치 0을 기준으로 0보다 작으면 아래쪽, 0보다 크면 위쪽으로 이동
  size=20,              # 글자크기
  color="#ff6600"))


# 일별 확진자 그래프
i <- length(kr_daily$date)
while(i > 1){
  kr_daily$confirmed[i] <- kr_daily$confirmed[i] - kr_daily$confirmed[i-1]
  i <- i-1
}


ggplot(data = kr_daily, aes(x = date, y = confirmed)) + geom_col() + xlab("month")+ ylab("확진자 수")+ ggtitle("일일 확진자수")+ theme(plot.title = element_text(
  face="bold",          # bold=굵게, italic=기울임
  hjust=0.5,            # 0=왼쪽, 0.5=가운데, 1=오른쪽
  vjust=1,              # 기본위치 0을 기준으로 0보다 작으면 아래쪽, 0보다 크면 위쪽으로 이동
  size=20,              # 글자크기
  color="#ff6600"))


# 누적 사망 그래프_막대
ggplot(data = kr_daily, aes(x = date, y = death)) + geom_col() + xlab("month")+ ylab("사망자 수")+ ggtitle("누적 사망자수")+ theme(plot.title = element_text(
  face="bold",          # bold=굵게, italic=기울임
  hjust=0.5,            # 0=왼쪽, 0.5=가운데, 1=오른쪽
  vjust=1,              # 기본위치 0을 기준으로 0보다 작으면 아래쪽, 0보다 크면 위쪽으로 이동
  size=20,              # 글자크기
  color="#ff6600"))

# 누적 사망 그래프_선
ggplot(data = kr_daily, aes(x = date, y = death)) + geom_line() + xlab("month")+ ylab("사망자 수")+ ggtitle("누적 사망자수")+ theme(plot.title = element_text(
  face="bold",          # bold=굵게, italic=기울임
  hjust=0.5,            # 0=왼쪽, 0.5=가운데, 1=오른쪽
  vjust=1,              # 기본위치 0을 기준으로 0보다 작으면 아래쪽, 0보다 크면 위쪽으로 이동
  size=20,              # 글자크기
  color="#ff6600"))

# 지역별 누적 사망자수 그래프
library("reshape2")
library("dplyr")
kr_regional_daily %>% group_by(date, region)
regional_1 = kr_regional_daily %>% group_by(date, region) %>% filter(date >= as.Date("2020-06-25")) %>% filter(region != "검역")
regional_1

ggplot(data = regional_1, aes(x = region, y = confirmed)) + geom_col() + xlab("지역")+ ylab("누적 사망자")+ ggtitle("지역별 누적 사망자수")+ theme(plot.title = element_text(
  face="bold",          # bold=굵게, italic=기울임
  hjust=0.5,            # 0=왼쪽, 0.5=가운데, 1=오른쪽
  vjust=1,              # 기본위치 0을 기준으로 0보다 작으면 아래쪽, 0보다 크면 위쪽으로 이동
  size=20,              # 글자크기
  color="#ff6600"))

# 지역별 확진자 그래프
# 데이터 추출
regional_1 = kr_regional_daily %>% group_by(date, region) %>% filter(date >= as.Date("2020-06-25")) %>% filter(region != "검역")
# 패키지 설치 및 로드
install.packages("ggiraphExtra")
install.packages("stringi")
install.packages("devtools")
install.packages("stringi")
install.packages("devtools")
update.packages(checkBuilt=TRUE, ask=FALSE)
devtools::install_github("cardiomoon/kormaps2014")
library(kormaps2014)
library(stringi)
library(devtools)
library(ggiraphExtra)
library(ggplot2)
install.packages("mapproj")
library(mapproj)

# 데이터 체크(생략가능 내가 보려고 하는거임)
str(changeCode(kormap1))
str(changeCode(korpop1))
korpop1$code
korpop1$행정구역별_읍면동
regional_1
# 지역 코드 행 추가
regional_1 <- cbind(regional_1, code = korpop1$code)
regional_1

#지도 만들기
ggChoropleth(data = regional_1,
             aes(fill = confirmed,
                 map_id = code,
                 tooltip = region),
             map = kormap1,
             interactive = T)



