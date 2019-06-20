library(jsonlite)
library(shiny)
library(rsconnect)
library(plotly)
library(dplyr)
library(markdown)
library(DBI)
library(RMySQL)

host <- "192.168.35.119" # 輸入自己的 AWS RDS Enpoint 位址
port <- 3306
dbname <- "dcard_db" # 輸入自己設定的資料庫名稱
user <- "lia" # 輸入自己設定的使用者名稱
password <- "lia" # 輸入自己設定的使用者密碼

con <- dbConnect(RMySQL::MySQL(),
                 host = host,
                 port = port,
                 dbname = dbname,
                 user = user,
                 password = password
)

dbSendQuery(con, "SET NAMES utf8mb4;")
dcard_area <- dbReadTable(con, name = 'dcard_content', header= FALSE)
#View(ptt_content)

#dcard_area <- read.csv("/Users/lialee/Desktop/dcard_final.csv")

clean_data <- dcard_area[dcard_area$area %in% c(1:5),]
clean_data <- clean_data %>% 
  distinct(content ,id, .keep_all = TRUE) 

a <- c("北","中","南","東","離島")
b <- 1:5
follow <- data.frame(cbind(a, b))
clean_data <- merge(follow, clean_data, by.x = "b", by.y = "area", all.x = T)
clean_data <- clean_data[,-1]
clean_data <- clean_data %>% 
  select(a, school, year, id, heart, gender, content) %>% 
  group_by(a, school, year) %>% 
  summarise(total_post = length(id),
            total_like = sum(heart)) 

colnames(clean_data) <- c('地區','學校','年度','發文總數','愛心總數')
clean_data$地區 <- factor(clean_data$地區, levels = c("北","中","南","東","離島"))
clean_data <- clean_data[order(clean_data$年度, decreasing = F), ]
clean_data <- clean_data[-1,]
# - - - - - clean_data over

data <- dcard_area[dcard_area$area %in% c(1:5),]
data <- data[data$area != "others",]
data <- data[data$gender %in% c("M","F"),]
data <- data %>% 
  distinct(content ,id, .keep_all = TRUE) 
#unique(data$school)

data <- data %>% 
  group_by(area, school, year, gender, city, weekday) %>% 
  summarise(total_post = length(id),
            total_like = sum(heart)) 

a <- c("北","中","南","東","離島")
b <- 1:5
follow <- data.frame(cbind(a, b))
data <- merge(follow, data, by.x = "b", by.y = "area", all.x = T)
data <- data[,-1]

colnames(data) <- c('地區','學校','年度','性別','城市','星期','發文總數','愛心總數')
sapply(data,class)
data <- data[order(data$年度, decreasing = F), ]
data <- data[-1,]
data$地區 <- factor(data$地區, levels = c("北","中","南","東","離島"))
data$城市 <- factor(data$城市, levels = c("台北市","新北市","基隆市","桃園市","新竹市","新竹縣",
                                      "苗栗縣","台中市","彰化縣","雲林縣","南投縣","嘉義市","嘉義縣","台南市",
                                      "高雄市","屏東縣","宜蘭縣","花蓮縣","台東縣","金門縣","澎湖縣"))
unique(data$城市)
# 2014-2019各執行一次
df_2014 <- data %>% 
  filter(年度 == 2014)
df_2015 <- data %>% 
  filter(年度 == 2015)
df_2016 <- data %>% 
  filter(年度 == 2016)
df_2017 <- data %>% 
  filter(年度 == 2017)
df_2018 <- data %>% 
  filter(年度 == 2018)
df_2019 <- data %>% 
  filter(年度 == 2019)
# - - - - - data over

bubble_radius <- (clean_data$發文總數)*2
unique_area <- unique(clean_data$地區)

ui <- fluidPage( 
  navbarPage(title = "台灣美食論壇文字探勘",
    tabPanel("關於我們",HTML('
                           <div class="col-md-12 text-center">
                           <img src="https://upload.cc/i1/2019/06/20/Rrq8OB.png" width="200px" alt="welcome" align="auto">
                           </div>
                           <header id="header" class="jumbotron jumbotron-fluid text-center text-white">
                           <div class="container">
                           <img src="https://upload.cc/i1/2019/06/19/eZMWqk.jpg" width="600px" alt="head" align="auto">
                           <h2 class="display-4" style="font-family:Haettenschweiler">專案精神</h2>
                           <div class="col-md-12 text-center">
                           <p style="font-family:Haettenschweiler">商機來自於討論熱度。以食物而言，透過我們的所建置的產品分析，預測未來趨勢，判斷哪一類型的網路文章<br>會成為哈燒新話題，帶動網路討論風潮，為商家創造新商機。</p>
                           </div>
                           </div>
                           </header>')),
    tabPanel("年度分類",HTML('
                           <h2 class="text-center text-info mb-5" style="font-family:Haettenschweiler">Dcard年度發文總量</h2>
                           <div class="container">
                           <div class="row text-center">
                           <div class="col-md-4">
                           <h3 class="text-primary" style="font-family:Haettenschweiler">年度選擇</h3>
                           <p>2014年至2019年</p>
                           </div>
                           <div class="col-md-4">
                           <br><img src="https://upload.cc/i1/2019/06/19/xKcr0k.png" width="50px" alt="post" align="auto">
                           </div>
                           <div class="col-md-4">
                           <h3 class="text-primary" style="font-family:Haettenschweiler">區域剖析</h3>
                           <p>劃分全台灣縣市</p>
                           </div>
                           </div>
                           </div><br><br>
                           '),  sidebarLayout(
                             sidebarPanel(
                               selectInput(inputId = "年度",
                                           label = "請選擇年度:",
                                           choices = unique(data$年度))
                             ),
                             mainPanel(
                               tabsetPanel(type = "tabs",
                                           tabPanel("圖表",plotOutput("Food_Forum_Plot")),
                                           tabPanel("年度摘要",verbatimTextOutput("summary")),
                                           tabPanel("表格",tableOutput("table"))
                               )
                             )
                           )
             ),
    tabPanel("美食熱搜", HTML('
                            <h2 class="text-center text-info mb-5" style="font-family:Haettenschweiler">即時連結資料庫</h2>
                            <div class="container">
                            <div class="row text-center">
                            <div class="col-md-4">
                            <h3 class="text-primary" style="font-family:Haettenschweiler">資料的家</h3>
                            <p>MySQL 關聯式資料庫</p>
                            </div>
                            <div class="col-md-4">
                            <br><img src="https://upload.cc/i1/2019/06/19/2Bni4d.png" width="50px" alt="post" align="auto">
                            </div>
                            <div class="col-md-4">
                            <h3 class="text-primary" style="font-family:Haettenschweiler">文章推薦</h3>
                            <p>最新最in的美食話題</p>
                            </div>
                            </div>
                            </div><br><br>
                            '),
             textInput("key", "輸入關鍵字:", "抹茶"), textInput("key2", "輸入區域:", "大安區"),
             #numericInput("nrows", "Enter the number of rows to display:", 5),
             #textInput("ID", "Enter your ID:", "34838"),
             tableOutput("tb1")),
    tabPanel("熱度動圖", HTML('
                            <h2 class="text-center text-info mb-5" style="font-family:Haettenschweiler">校園美食趨勢圖</h2>
                            <div class="container">
                            <div class="row text-center">
                            <div class="col-md-4">
                            <h3 class="text-primary" style="font-family:Haettenschweiler">討論熱度</h3>
                            <p>全台灣學校排排站</p>
                            </div>
                            <div class="col-md-4">
                            <br><img src="https://upload.cc/i1/2019/06/19/VjHBRv.png" width="50px" alt="post" align="auto">
                            </div>
                            <div class="col-md-4">
                            <h3 class="text-primary" style="font-family:Haettenschweiler">數據發聲</h3>
                            <p>年度變化一覽無遺</p>
                            </div>
                            </div>
                            </div><br><br>
                           '),
             sidebarLayout(
               sidebarPanel(
                 checkboxGroupInput(
                   "地區",
                   "地區:",
                   choices = unique_area,
                   selected = unique_area
                 )
               ),
               mainPanel(
                 plotlyOutput("food_forum")
               )
             ))
    ),
  HTML('
       <br><br>
       <footer id="footer" class="pt-5 bg-secondary text-white">
       <div class="container">
       <div class="row text-center">
       <div class="col-md-4 footer-list">
       <h3 class="text-uppercase">Project</h3>
       <h5 class="text-primary">Dcard & PTT Food Forum</h5>
       </div>
       <div class="col-md-4 footer-list">
       <h3 class="text-uppercase">Team</h3>
       <h4 class="text-primary">Raymond / Derek / June / Henry / Lia</h4>
       </div>
       <div class="col-md-4 footer-list">
       <h3>Contact Us</h3>
       <a href="mailto:town_village@brand.com">town_village@brand.com</a>
       </div>
       <div class="col-md-12">
       <p class="text-center">&copy; Copy right 2019</p>
       </div>
       </div>
       </footer>
       ')
)

server <- function(input, output) {
  output$tb1 <- renderTable({
    con = dbConnect(drv = RMySQL::MySQL(),
                    user = "derek", 
                    password = "derek",
                    dbname = "dcard_db",
                    host = "192.168.35.119")
    on.exit(dbDisconnect(con), add = TRUE)
    dbSendQuery(con, "SET NAMES utf8mb4;")
    #query <- paste0("SELECT id,school,area,title,content,heart FROM dcard_db.dcard_content WHERE id = '", input$ID, "';")
    #dbGetQuery(con, query)
    query <- paste0("SELECT date,city,content,title,heart FROM dcard_db.dcard_content WHERE content Like '%", input$key , "%' AND content Like '%", input$key2 , "%' ORDER BY date DESC LIMIT 30;")
    dbGetQuery(con, query)
    #dbGetQuery(con, paste0("SELECT id,school,content,city,area FROM dcard_db.dcard_content Limit ", input$nrows, ";"))
  })
  
  output$Food_Forum_Plot <- renderPlot({
    
    if(input$年度 =="2014"){
      ggplot(df_2014 ,aes( x =城市 ,y=發文總數, group = 1, fill=地區))+
        geom_bar(stat = "identity")+
        theme_minimal()+
        theme(text = element_text(family = "Heiti TC Light"), legend.position = 'none')+
        labs(title = paste("時間為",input$年度,"年"),x="城市",y="發文總數")+
        scale_fill_hue(h = c(60,250))+
        theme(axis.title.x = element_text(size = 10, face = "bold", vjust = 0.5, hjust = 0.5))+
        theme(axis.title.y = element_text(size = 10, face = "bold", vjust = 0.5, hjust = 0.5, angle = 360))
    }else if(input$年度 =="2015"){
      ggplot(df_2015 ,aes( x =城市 ,y=發文總數, group = 1, fill = 地區))+
        geom_bar(stat = "identity")+
        theme_minimal()+
        theme(text = element_text(family = "Heiti TC Light"), legend.position = 'none')+
        labs(title = paste("時間為",input$年度,"年"),x="城市",y="發文總數")+
        scale_fill_hue(h = c(60,250))+
        theme(axis.title.x = element_text(size = 10, face = "bold", vjust = 0.5, hjust = 0.5))+
        theme(axis.title.y = element_text(size = 10, face = "bold", vjust = 0.5, hjust = 0.5, angle = 360))
    }else if(input$年度 =="2016"){
      ggplot(df_2016 ,aes( x =城市 ,y=發文總數, group = 1, fill = 地區))+
        geom_bar(stat = "identity")+
        theme_minimal()+
        theme(text = element_text(family = "Heiti TC Light"), legend.position = 'none')+
        labs(title = paste("時間為",input$年度,"年"),x="城市",y="發文總數")+
        scale_fill_hue(h = c(60,250))+
        theme(axis.title.x = element_text(size = 10, face = "bold", vjust = 0.5, hjust = 0.5))+
        theme(axis.title.y = element_text(size = 10, face = "bold", vjust = 0.5, hjust = 0.5, angle = 360))
    }else if(input$年度 =="2017"){
      ggplot(df_2017 ,aes( x =城市 ,y=發文總數, group = 1, fill = 地區))+
        geom_bar(stat = "identity")+
        theme_minimal()+
        theme(text = element_text(family = "Heiti TC Light"), legend.position = 'none')+
        labs(title = paste("時間為",input$年度,"年"),x="城市",y="發文總數")+
        scale_fill_hue(h = c(60,250))+
        theme(axis.title.x = element_text(size = 10, face = "bold", vjust = 0.5, hjust = 0.5))+
        theme(axis.title.y = element_text(size = 10, face = "bold", vjust = 0.5, hjust = 0.5, angle = 360))
    }else if(input$年度 =="2018"){
      ggplot(df_2018 ,aes( x =城市 ,y=發文總數, group = 1, fill = 地區))+
        geom_bar(stat = "identity")+
        theme_minimal()+
        theme(text = element_text(family = "Heiti TC Light"), legend.position = 'none')+
        labs(title = paste("時間為",input$年度,"年"),x="城市",y="發文總數")+
        scale_fill_hue(h = c(60,250))+
        theme(axis.title.x = element_text(size = 10, face = "bold", vjust = 0.5, hjust = 0.5))+
        theme(axis.title.y = element_text(size = 10, face = "bold", vjust = 0.5, hjust = 0.5, angle = 360))
    }else{
      ggplot(df_2019 ,aes( x =城市 ,y=發文總數, group = 1, fill = 地區))+
        geom_bar(stat = "identity")+
        theme_minimal()+
        theme(text = element_text(family = "Heiti TC Light"), legend.position = 'none')+
        labs(title = paste("時間為",input$年度,"年"),x="城市",y="發文總數")+
        scale_fill_hue(h = c(60,250))+
        theme(axis.title.x = element_text(size = 10, face = "bold", vjust = 0.5, hjust = 0.5))+
        theme(axis.title.y = element_text(size = 10, face = "bold", vjust = 0.5, hjust = 0.5, angle = 360))
    }
  })
  output$summary<-renderPrint({
    if(input$年度=="2014"){
      df_2014 %>% 
        select(地區,城市,發文總數,愛心總數) %>%
        group_by(地區) %>% 
        summary()
    }else if(input$年度=="2015"){
      df_2015 %>% 
        select(地區,城市,發文總數,愛心總數) %>%
        group_by(地區) %>% 
        summary()
    }else if(input$年度=="2016"){
      df_2016 %>% 
        select(地區,城市,發文總數,愛心總數) %>%
        group_by(地區) %>% 
        summary()
    }else if(input$年度=="2017"){
      df_2017 %>% 
        select(地區,城市,發文總數,愛心總數) %>%
        group_by(地區) %>% 
        summary()
    }else if(input$年度=="2018"){
      df_2018 %>% 
        select(地區,城市,發文總數,愛心總數) %>%
        group_by(地區) %>% 
        summary()
    }else if(input$年度=="2019"){
      df_2019 %>% 
        select(地區,城市,發文總數,愛心總數) %>%
        group_by(地區) %>% 
        summary()
    }
  })
  
  output$table <- renderTable({
    if(input$年度=="2014"){
      df_2014[-1] %>% 
        head(20)
    }else if(input$年度=="2015"){
      df_2015[-1] %>% 
        head(20)
    }else if(input$年度=="2016"){
      df_2016[-1] %>% 
        head(20)
    }else if(input$年度=="2017"){
      df_2017[-1] %>% 
        head(20)
    }else if(input$年度=="2018"){
      df_2018[-1] %>% 
        head(20)
    }else if(input$年度=="2019"){
      df_2019[-1] %>% 
        head(20)
    }
  })
  # - - - - -
  reactive_food_forum <- reactive(
    clean_data %>%
      filter(地區 %in% input$地區)
  )
  output$food_forum <- renderPlotly({
    shiny::validate(
      need(input$地區, '請至少選擇一個地區!')
    )
    reactive_food_forum() %>% 
      plot_ly(x = ~愛心總數, y = ~發文總數, color = ~地區, size = ~發文總數, 
              type = "scatter", mode = "markers", frame = ~年度,
              text = ~學校, hoverinfo = "text",
              sizes = c(min(bubble_radius), max(bubble_radius)))
  })
}

shinyApp(ui = ui, server = server)

# 一定要取名為app.R, 或切開存成ui.R跟server.R
# shiny server (library(shinyapps))
# dashboard page (library(shinydashboard))
# http://shiny.rstudio.com/articles/html-ui.html
      