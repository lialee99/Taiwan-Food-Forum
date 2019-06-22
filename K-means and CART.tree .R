library(jsonlite)
library(jiebaR)

# get dcard_area from github
dcard_area <- dcard_area[,-1]

dim(dcard_area)
# select content, id, gender, area
dcard_content <- dcard_area[,c(1,8,12,15)]
# delete na
dcard_content <- dcard_content %>% 
  filter(area %in% c(1:5))

sapply(dcard_content, class)
library(dplyr)

# repeat 8 round (north, middle, south, east X female, male)
# F_E = female plus east
M_S <- dcard_content %>% 
  filter(gender == "M") %>% 
  filter(area == 3)

# 檢查是否重複
M_M <- M_M %>% 
  distinct(content, id, .keep_all = TRUE) 

# 指定停用詞和分詞詞庫
stop_word <- '/Users/lialee/Desktop/Programming/TextMining/Data/stop.txt'
user_dic <- "/Users/lialee/Desktop/III Final Project/foodclean.csv"

# 設定斷詞器
mixseg <- worker(stop_word = stop_word, user = user_dic, type = "tag")

# - - - 開始跑斷詞
df_FN <- data.frame()
df_FM <- data.frame()
df_FE <- data.frame()
df_FS <- data.frame()
df_MN <- data.frame()
df_MM <- data.frame()
df_ME <- data.frame()
df_MS <- data.frame()

seq_doc <- NULL # Word Segmentation Results
seq_tag <- NULL # POS Tagging Results

k <- M_S$content

k <- gsub('[0-9]+', "", k)
k <- gsub('[[:space:]]', "", k)
k <- gsub('[a-zA-Z]', "", k)
k <- gsub('#', "", k)
k <- gsub('[ ️   ︎   ﹏ ︵ ︶ ︿ ﹃ ꒳]',"",k)
k <- gsub('[︴ ︹ ︺ ꒦ ꒪ ꒫"]' ,"",k)
k <- gsub('[a-zA-Z]', "", k)
k <- gsub('[-+/.─◆○~=,「」▲:～※_★$、?│【】（）()]' ,"", k)

# 中文分詞
w <- segment(as.vector(k), mixseg)
seq_doc <- c(seq_doc, w)

# 詞性標注
t <- names(tagging(as.vector(k), mixseg))
seq_tag <- c(seq_tag , t) 

seq <- data.frame(seq_doc, seq_tag) 
seq <- seq[seq$seq_tag %in% c('n','nr','nrt','ns','nt','nz'),]
seq_doc <- table(as.character(seq$seq_doc))
# give area tags
seq_doc <- data.frame(seq_doc, clas = 'Male South')
df_MS <- rbind(df_MS, seq_doc)

# combine eight data frames to df_area
df_area <- rbind(df_area, df_MS)
table(df_area$clas)

names(df_area)[1] <- 'Keywords'
names(df_area)[2] <- 'Frequency'
names(df_area)[3] <- 'Type'

DF <- c(table(df_area$Keywords))
FM <- unique(df_area$Type)

df_area <- df_school
library(reshape2)

TCM <- acast(df_area, Keywords ~ Type, value.var='Frequency', fill = 0, drop = FALSE, sum)
TCB <- ifelse(TCM > 0, 1, 0)

# 共出現超過5次才選
selectedKW <- rowSums(TCM) > 5

TCM <- as.data.frame(TCM[selectedKW,])
TCB <- as.data.frame(TCB[selectedKW,])

DF <- DF[selectedKW]
#文章總篇數
counter <- 31038 
IDF <- log10(counter / DF)
cbind(rownames(TCM), IDF)

TTF <- colSums(TCM)

TCM_IDF <- t(t(TCM) / TTF) * IDF

TCM <- data.frame(Keywords = rownames(TCM), TCM)
rownames(TCM) <- NULL

TCM_IDF <- data.frame(Keywords = rownames(TCM_IDF), TCM_IDF)
rownames(TCM_IDF) <- NULL

TCB <- data.frame(Keywords = rownames(TCB), TCB)
rownames(TCB) <- NULL

colnam <- TCM$Keywords
TCM$Keywords <- NULL

# 轉向
t_TCM <- as.data.frame(t(TCM))
colnames(t_TCM) <- colnam
rownames(t_TCM) <- FM
# 這個步驟是為了決策樹跟分類無關
cart_TCM <- t_TCM
cart_TCM$Type <- FM

# K-means
library(cluster)

# Decide K
result <- list()

for (i in 2:6){
  kmd <- kmeans(t_TCM, centers=i)
  sil <- silhouette(kmd$cluster, dist(t_TCM))
  result[[paste('k=',i,sep='')]] <- mean(sil[,'sil_width'])
}

result 

# K = 3
kmd <- kmeans(t_TCM, centers=3)
kmd$cluster

# 看輪廓係數
sil <- silhouette(kmd$cluster, dist(t_TCM))
mean(sil[,'sil_width'])

# Cluster Descriptions
kmd$centers

# Display Clustering Results
# 以下都是畫文字雲用的
library(wordcloud2)
install.packages("webshot")
library(webshot)
webshot::install_phantomjs()
library("htmlwidgets")

for(i in 1:3) {
  Clus_i <- t_TCM[kmd$cluster==i,]
  Clus_n <- colnames(t_TCM)
  Clus_f <- colSums(Clus_i)
  Word_Tab <- data.frame(Clus_n, Clus_f)
  rownames(Word_Tab) <- NULL
  Word_Tab <- Word_Tab[Word_Tab$Clus_f!=0,]
  
  my_graph <- wordcloud2(Word_Tab, size = 0.5, minSize = 0, gridSize =  3,color = "random-light", backgroundColor = "white")
  
  saveWidget(my_graph,paste0("/Users/lialee/Desktop/",i,".html"),selfcontained = F)
  
  webshot(paste0("/Users/lialee/Desktop/",i,".html"),
          paste0("/Users/lialee/Desktop/",i,".png"),
          vwidth = 600, vheight=350)
}

# - - - - - cart_TCM 畫決策樹
library(rpart)
library(rpart.plot)

CART.tree <- rpart(Type ~ ., data=cart_TCM, 
                   control=rpart.control(minsplit=2, cp=0))
rpart.plot(CART.tree)
