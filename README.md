# 코로나 발생 현황 분석 및 시각화😷

- [깃허브 주소](https://github.com/SuHae-Bae/Corona-R-analyze)



## 개요

- 진행기간: 2020년 6월
- 코로나에 대한 사회적 관심이 높아진 가운데, 관련 데이터를 간단하게 분석하고 시각화해 나타내보려고 함
- 위키피디아 영문서에 나와있는 코로나 관련 단어들을 워드 클라우드로 나타냄
- 국내 코로나 발생과 관련된 데이터를 수집, 분석
- 마지막으로 구글 클라우드와 구글 데이터 스튜디오를 이용하여 현황을 파악할 수 있도록 보고서를 작성함
- 사용 기술: R, 구글 클라우드 플랫폼, 구글 데이터 스튜디오



## 1. COVID-19 위키피디아 영문문서로 워드클라우드 만들기

- [위키피디아 페이지](https://en.wikipedia.org/wiki/Coronavirus_disease_2019)



```R
install.packages(c("RCurl", "XML"))
library(RCurl)
library(XML)

t <- readLines('https://en.wikipedia.org/wiki/Coronavirus_disease_2019')

d <- htmlParse(t, asText = TRUE)
clean_doc <- xpathSApply(d,"//p",xmlValue)
```

- RCurl 라이브러리로 웹 서버에 접속

- XML 라이브러리로 웹 문서 처리

- readLines 함수는 지정된 URL에서 html 파일을 읽어 옴

- htmlParse와 xpathSApply 함수는 웹 문서를 R의 데이터 형으로 변환해 줌



데이터의 전처리 과정은 다음과 같다.

```R
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
```

- tm 라이브러리는 데이터 마이닝 함수 제공

- SnowballC 라이브러리는 어간을 추출하는 함수 제공

- tm_map 함수는 지정된 매개변수 값에 따라 전처리 수행



다음으로 DocumentTermMatrix 함수로 DTM을 구축한다.

```R
dtm = DocumentTermMatrix(doc)
dim(dtm)
inspect(dtm)
```

![image](https://user-images.githubusercontent.com/33304926/149641096-97817501-03eb-49c6-9bcc-117c28f1f316.png)

![image](https://user-images.githubusercontent.com/33304926/149641105-1da02ffa-52dc-47c9-9c13-864abdb001b6.png)



### 워드클라우드 만들기

워드클라우드로 만드는 코드는 다음과 같다. 그런데 이렇게만 진행하면 약간 조잡하고, 흑백의 워드클라우드가 나온다. 만약 패키지가 설치되어 있다면, 설치하지 않아도 된다.

```R
install.packages("wordcloud")
library(wordcloud)
m <- as.matrix(dtm)
v <- sort(colSums(m), decreasing = TRUE)
d <- data.frame(word = names(v), freq = v)
wordcloud(words = d$word, freq = d$freq, min.freq = 1, max.words = 100, random.order = FALSE, rot.per = 0.35)
```

![image](https://user-images.githubusercontent.com/33304926/149641145-81abffcf-a768-49a0-806b-c968b3cf4f47.png)

워드클라우드의 단어 개수를 조절하고, 색을 입혀 다시 만들어보았다.

```R
library(RColorBrewer)
pal <- brewer.pal(11, "Spectral")
wordcloud(words = d$word, freq = d$freq, min.freq = 1, max.words = 50, random.order = FALSE,
          rot.per = 0.50, colors = pal)
```

![image](https://user-images.githubusercontent.com/33304926/149641166-644e107d-3ca6-4e80-90cc-877f9e3f830d.png)

혹시나 했는데 폰트도 조절할 수 있어 폰트도 바꿔보았다.

```R
wordcloud(words = d$word, freq = d$freq, min.freq = 1, max.words = 50, random.order = FALSE,
          rot.per = 0.50, colors = pal, family = "mono", font = 2)

```

![image](https://user-images.githubusercontent.com/33304926/149641189-184c18c1-f521-4e44-bc43-94c95d4494e1.png)

R의 워드클라우드 패키지에는 워드클라우드2도 있어, 워드클라우드2로도 만들어 보았다.

```R
install.packages('wordcloud2')
library(wordcloud2)
wordcloud2(d)
```

![image](https://user-images.githubusercontent.com/33304926/149641243-edd59f8e-c7b5-4bbc-a2fd-bad2423fa2c2.png)

워드클라우드2는 모양을 바꿀수도 있고, 단어 방향의 범위를 지정해 줄 수도 있다.

```R
# 별모양으로 만들기
d1 <- d[1:200,]
wordcloud2(d1, shape = 'star')
# 단어 방향 범위 지정
wordcloud2(d1, minRotation = pi/4, maxRotation = pi/4, rotateRatio = 1.0)
```

![image](https://user-images.githubusercontent.com/33304926/149641291-38ffd5f0-9d59-40df-af72-ce1072aaadf2.png)

![image](https://user-images.githubusercontent.com/33304926/149641294-9a6e97f8-cfba-4046-8b9c-2486555375a5.png)



## 2. 국내 COVID-19 발생과 관련된 데이터 수집, 분석

- [데이터 소스](https://github.com/jooeungen/coronaboard_kr)

위 깃허브(코로나보드)에서 국내 COVID-19발생 데이터를 다운받아 분석하였다.

```R
# 데이터 분석을 위해 데이터 로드
kr_regional_daily = read.csv('./kr_regional_daily.csv', fileEncoding = "utf-8")
kr_regional_daily
kr_daily = read.csv("./kr_daily.csv", fileEncoding = "utf-8")
kr_daily
```



데이터 전처리 실행(date를 date형으로 포맷을 바꿔줌)

```R
# 데이터 전처리
kr_daily$date<-as.Date(as.character(kr_daily$date),format="%Y%m%d")
kr_regional_daily$date<-as.Date(as.character(kr_regional_daily$date), format="%Y%m%d")
```

각 수치를 요약하면 다음과 같다.

![image](https://user-images.githubusercontent.com/33304926/149641452-d6e4c62d-cef8-43b1-812b-fb35b713ec44.png)

![image](https://user-images.githubusercontent.com/33304926/149641456-e1c0520f-401d-42d4-a705-98651b869f3e.png)



### 2-1. 누적 확진자수 그래프

```R
# 전체 누적 확진자수 그래프
ggplot(data = kr_daily, aes(x = date, y = confirmed)) + geom_col() + xlab("month")+ ylab("누적 확진자")+ ggtitle("누적 확진자수")+ theme(plot.title = element_text(
  face="bold",          # bold=굵게, italic=기울임
  hjust=0.5,            # 0=왼쪽, 0.5=가운데, 1=오른쪽
  vjust=1,              # 기본위치 0을 기준으로 0보다 작으면 아래쪽, 0보다 크면 위쪽으로 이동
  size=20,              # 글자크기
  color="#ff6600"))
```

![image](https://user-images.githubusercontent.com/33304926/149641868-df7ec9cf-060e-4c61-a914-040c9a19c9cb.png)

그런데 생각해보니 확진자수의 변화 추이를 보기 위해선 막대 그래프보다 선 그래프가 나을 것 같다는 생각이 들었고, 이에 선 그래프로 다시 나타내보았다.

```R
ggplot(data = kr_daily, aes(x = date, y = confirmed)) + geom_line() + xlab("month")+ ylab("누적 확진자")+ ggtitle("누적 확진자수")+ theme(plot.title = element_text(
  face="bold",          # bold=굵게, italic=기울임
  hjust=0.5,            # 0=왼쪽, 0.5=가운데, 1=오른쪽
  vjust=1,              # 기본위치 0을 기준으로 0보다 작으면 아래쪽, 0보다 크면 위쪽으로 이동
  size=20,              # 글자크기
  color="#ff6600"))
```

![image](https://user-images.githubusercontent.com/33304926/149641991-4766d92e-af66-4c31-b048-e958c9a841ba.png)

선 그래프로 누적 확진자수를 나타내보니, 2월에는 확진자수의 증가가 더뎠지만 2월 말~4월에 확진자수가 급격히 증가했음을 알 수 있었다.



### 2-2. 일일 확진자수 그래프

```R
# 일일 확진자 그래프
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
```

![image](https://user-images.githubusercontent.com/33304926/149642265-75dc391f-03fa-4f7c-93c3-97edd4f7f8d8.png)

그래프를 보면, 3월 초에 일일 확진자수가 최대를 기록했음을 알 수 있으며, 2월 후반~3월 중순에 일일 확진자수가 대체로 많았음을 알 수 있다.



### 2-3. 누적 사망자수 그래프

```R
# 누적 사망 그래프_막대
ggplot(data = kr_daily, aes(x = date, y = death)) + geom_col() + xlab("month")+ ylab("사망자 수")+ ggtitle("누적 사망자수")+ theme(plot.title = element_text(
  face="bold",          # bold=굵게, italic=기울임
  hjust=0.5,            # 0=왼쪽, 0.5=가운데, 1=오른쪽
  vjust=1,              # 기본위치 0을 기준으로 0보다 작으면 아래쪽, 0보다 크면 위쪽으로 이동
  size=20,              # 글자크기
  color="#ff6600"))
```

![image](https://user-images.githubusercontent.com/33304926/149642319-0c94c899-b50a-4c75-a82f-8886cd5889c6.png)

누적 사망자수를 막대 그래프로 나타내면 다음과 같다. 하지만, 이번에도 변화 추세를 살펴보기 위해 이를 다시 선그래프로 나타내었다.

```R
# 누적 사망 그래프_선
ggplot(data = kr_daily, aes(x = date, y = death)) + geom_line() + xlab("month")+ ylab("사망자 수")+ ggtitle("누적 사망자수")+ theme(plot.title = element_text(
  face="bold",          # bold=굵게, italic=기울임
  hjust=0.5,            # 0=왼쪽, 0.5=가운데, 1=오른쪽
  vjust=1,              # 기본위치 0을 기준으로 0보다 작으면 아래쪽, 0보다 크면 위쪽으로 이동
  size=20,              # 글자크기
  color="#ff6600"))
```

![image](https://user-images.githubusercontent.com/33304926/149642335-506e9d13-9b39-4727-91f9-a289fd5e5b51.png)

누적 사망자수를 선 그래프로 보니, 위의 확진자수와 비슷하게 3월~4월에 사망자수가 증가함을 알 수 있다. 단, 확진자수와 다른점은, 확진자수의 증가추세는 4월~5월에는 줄어들었으나, 사망자수는 4월 중순까지 증가추세가 이어졌다 4월 말부터 줄어들었다는 점이다.



### 2-4. 지역별 누적 사망자 그래프

```R
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
```

![image](https://user-images.githubusercontent.com/33304926/149642436-074e9849-8235-453c-bd49-7a9afdf7f84f.png)

다음은 2020년 6월까지의 지역별 코로나 누적 사망자수를 나타낸 그래프이다. 이 당시까지는 대구에서 코로나로 인한 사망자가 가장 많이 발생한 것으로 나타나며, 그 뒤를 경북, 서울, 경기가 이었다.



### 2-5. 지도에 지역별 누적 확진자수 나타내기

먼저 지도에 나타낼 데이터를 뽑아내주고, 필요한 패키지들을 설치 및 로드해준다.

```R
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
```

이 중 kormaps2014는 한국 지도를 그리는 데 도움을 주는 패키지이다.



데이터에 지역 코드 행을 추가해주고, 지도를 만들어준다.

```R
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
```

지도는 히트맵 형태로 출력되며, 지도 위에 마우스를 올리면 해당 지역의 색이 강조되며 데이터가 출력된다.

![image](https://user-images.githubusercontent.com/33304926/149642716-167d0201-f54c-4204-bb96-c2fb9d7038dc.png)

![image](https://user-images.githubusercontent.com/33304926/149642724-78161a64-ac6b-40d5-b990-0fa28e36315e.png)



## 3. Google Data Studio 사용해서 보고서 만들기

먼저 버킷을 생성하고 버킷에 데이터를 넣어준다. 이건 내가 만든 버킷이다.

![image](https://user-images.githubusercontent.com/33304926/149643214-c4a07700-3b76-4322-a498-95cfd89edd6d.png)

![image](https://user-images.githubusercontent.com/33304926/149643439-591c96d4-e219-4c31-b543-08f70b518947.png)

![image](https://user-images.githubusercontent.com/33304926/149643445-2ace9581-f0e8-4251-a656-78f982408bc9.png)



그 뒤 SQL 인스턴스를 생성해준다. ip주소는 안보이게 가렸다.

![image](https://user-images.githubusercontent.com/33304926/149643414-c1837606-c5f8-4a6c-8c1b-55582981cf02.png)

이후 SQL인스턴스를 연결해준다. 먼저 터미널에서 sql을 열어 데이터베이스 및 테이블을 생성해준다.

![image](https://user-images.githubusercontent.com/33304926/149644038-e2088202-3bb8-4bba-ad10-fb147b1cec8a.png)

![image](https://user-images.githubusercontent.com/33304926/149644049-494da135-5a49-43a2-b834-4e604897eedd.png)

db에 테이블을 삽입한다.

![image](https://user-images.githubusercontent.com/33304926/149644656-b039ac48-37e1-4c90-b342-d6a9449f1df2.png)



Google Data Studio 이용하여 현황을 파악할 수 있도록 보고서를 작성한다

다음은 그 예시이다.

![image](https://user-images.githubusercontent.com/33304926/149644683-943d1b0a-6459-4f8b-a6cb-b47e7915c41b.png)





