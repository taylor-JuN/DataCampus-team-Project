#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(plotly)
library(xts)
library(rvest)
library(shinyalert)
source("ui.R")
library(zoo)

library(plyr)
library(dplyr)

totalData <- readRDS("data/yesu.rds")
region_pie <- readRDS("data/region_pie.rds")


dat <- read.csv("data/test_tot.csv")
dat2 <- read.csv("data/pos_tot.csv")
datDaily <- read.csv("data/daily.csv")


Logged = FALSE
x =0
y =0
z =0

day = 1

my_username <- "1"
my_password <- "1"


res <- c()
 for (i in c("busan", "sejong", "chungnam")) {
     path <- paste0("data/", i, ".txt")
     datOflocally <- read.delim(path, fileEncoding = "cp949")

     total.person <- sum(datOflocally$person, na.rm = T)
     step <- c(i, total.person)
     res <- rbind(res, step)
 }
###################################################### 지도시각화############
mapdata <- get_data_from_map(download_map_data("countries/kr/kr-all"))

local_code <- data.frame(
    code = c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17),
    name = c("Korean","Seoul","GyungGi")
)

abc <- readRDS("data/abc.rds") %>%
    `[[`(2) %>%
    `[[`(2) %>%
    `[`(,-1) %>%
    colSums(,na.rm = T) %>%
    melt() %>%
    mutate(name = rownames(.)) %>%
    `rownames<-`(1:17)


hc_key <- mapdata %>%
    select("hc-key") %>%
    mutate(name = c(
        NA, "경기", "전북", "경남", "전남", "부산", "경북", "세종", "대전", "울산",
        "인천", "강원", "충남", "제주", "충북", "서울", "대구", "광주")) %>%
    join(.,abc)

########## 이동 평균 그래프##########
coach_dygraph <- function(region, day) {

    path <- paste0("data/dygraph/est_", region, ".txt")
    dat <- read.delim(path)
    dat$method <- as.character(dat$method)
    
    method2 <- c("positive.rate", paste0("mv", day))
    
    dat2 <- dat[which(dat$method %in% method2), ]
    
    pr <- dat2[dat2$method == unique(dat2$method)[1], ]
    mv <- dat2[dat2$method == unique(dat2$method)[2], ]
    
    pr$date <- as.Date(pr$date, "%Y-%m-%d")
    mv$date <- as.Date(mv$date, "%Y-%m-%d")
    
    xt_pr <- xts(x = pr$kits, order.by = pr$date)
    xt_mv <- xts(x = mv$kits, order.by = mv$date)
    
    total <- cbind(xt_pr, xt_mv)
    colnames(total) <- c("기존 키트수 ", "이동 평균 적용 키트수")
    
    graph <- dygraph(total) %>%
        dyRangeSelector() %>%
        dyOptions(drawPoints = T, colors = c("#164068", "black"), drawGrid = F)
    
    return(graph)
}

#################파이그래프#######
pie_by_region <- function(region_name){
    
    gradient <- colorRampPalette(c('lightblue', 'darkblue'))
    
    unique(region_pie$sido)
    
    colors <- gradient(dim(region_pie)[1])[
        as.numeric(cut(100*region_pie[region_pie$sido==region_name,"perc"], breaks = dim(region_pie)[1]))]
    
    plot_ly(region_pie[region_pie$sido==region_name,], labels = ~geo_name, values = ~value, type = 'pie',
            textposition = 'inside',
            textinfo = 'label+percent',
            insidetextfont = list(color = '#FFFFFF'),
            hoverinfo = 'text',
            marker = list(colors = colors),
            showlegend = FALSE) %>%
        layout(paper_bgcolor = '#e8f6f9')
}


############## 그룹 수 최적화######
group_min <- function(region,mv) {
    
    coach.mv <- function(dataframe, column, day) {
        
        if (day == 1) {
            one_nrow <- length(dataframe[, column]) - 1
            oneday <- c(0, dataframe[1:one_nrow, column])
            
            return(oneday)
            
        } else if (day == 7) {
            week <- data.frame()
            for (i in unique(dataframe[, "week"])) {
                
                dat <- dataframe[dataframe$week == i, ]
                
                n <- length(dat[, column])
                
                week.mean <- mean(dat[, column])
                
                week.rep <- rep(week.mean, n)
                
                res <- data.frame(week = i, p = week.rep)
                
                week <- rbind(week, res)
                
            }
            
            one <- rep(0, nrow(week[week$week == unique(week$week)[1],]))
            
            summ <- c()
            
            ind_a <- unique(week$week)[2:length(unique(week$week))] %>%
                as.numeric()
            
            for (a in ind_a) {
                
                b <- a-1
                
                first <- week[week$week == b, "p"][1]
                
                second <- rep(first, nrow(week[week$week == a,]))
                
                summ <- c(summ, second)
            }
            
            summ <- c(one, summ)
            
            return(summ)
            
        } else {
            over.day <- rep(0, day)
            day2 <- day + 1
            for (mv in day2:nrow(dataframe)) {
                
                low <- mv - day
                high <- mv - 1
                
                test.mean <- dataframe[low:high, column]
                mean2 <- mean(test.mean)
                
                over.day <- c(over.day, mean2)
                
                
            }
            return(over.day)
        }
    }
    
    
    if(!exists("yesu")){
        
        stop("check yesu.rds object")
        
    } else{ 
        
        selected_yesu <- yesu %>%
            `[`(yesu$local1 == region,) %>%
            na.omit()
        
        p <- coach.mv(selected_yesu,6,mv) %>%
            `[`(length(.))
        
        n=1000
        
        ind_vec <- n
        
        for (g in 2:60) {
            p_prime <- 1-(1-p)^g
            
            exp_R <- n/g+n*p_prime
            
            ind_vec <- c(ind_vec,exp_R)
            
        }
        
        ind_group <- which(ind_vec==min(ind_vec))
        
        ind_final <- c(ind_group,round(p,4)*100)
        
        ind_final
        
    }
    
}


server <- shinyServer(function(input, output, session) {
    observeEvent(input$btn, {
        output$xxxPlot <-
            if(input$btn == 1){
                renderDygraph({coach_dygraph("jg",1)})
            }else if(input$btn == 2){
                renderDygraph({coach_dygraph("jg",2)})
            }else if(input$btn == 3){
                renderDygraph({coach_dygraph("jg",3)})
            }else if(input$btn == 4){
                renderDygraph({coach_dygraph("jg",4)})
            }else if(input$btn == 5){
                renderDygraph({coach_dygraph("jg",5)})
            }else if(input$btn == 6){
                renderDygraph({coach_dygraph("jg",6)})
            }else{
                renderDygraph({coach_dygraph("jg",7)})
            }
    })
    observeEvent(input$btn, {
        output$numOfGroup <-
            if(input$btn == 1){
                renderText({group_min("korea", 1)[1]})
            }else if(input$btn == 2){
                renderText({group_min("korea", 2)[1]})
            }else if(input$btn == 3){
                renderText({group_min("korea", 3)[1]})
            }else if(input$btn == 4){
                renderText({group_min("korea", 4)[1]})
            }else if(input$btn == 5){
                renderText({group_min("korea", 5)[1]})
            }else if(input$btn == 6){
                renderText({group_min("korea", 6)[1]})
            }else{
                renderText({group_min("korea", 7)[1]})
            }
    })
    observeEvent(input$btn, {
        output$prob <-
            if(input$btn == 1){
                renderText({group_min("korea", 1)[2]})
            }else if(input$btn == 2){
                renderText({group_min("korea", 2)[2]})
            }else if(input$btn == 3){
                renderText({group_min("korea", 3)[2]})
            }else if(input$btn == 4){
                renderText({group_min("korea", 4)[2]})
            }else if(input$btn == 5){
                renderText({group_min("korea", 5)[2]})
            }else if(input$btn == 6){
                renderText({group_min("korea", 6)[2]})
            }else{
                renderText({group_min("korea", 7)[2]})
            }
    })
    observeEvent(input$btn2, {
        output$xxxPlot2 <-
            if(input$btn2 == 1){
                renderDygraph({coach_dygraph("busan",1)})
            }else if(input$btn2 == 2){
                renderDygraph({coach_dygraph("busan",2)})
            }else if(input$btn2 == 3){
                renderDygraph({coach_dygraph("busan",3)})
            }else if(input$btn2 == 4){
                renderDygraph({coach_dygraph("busan",4)})
            }else if(input$btn2 == 5){
                renderDygraph({coach_dygraph("busan",5)})
            }else if(input$btn2 == 6){
                renderDygraph({coach_dygraph("busan",6)})
            }else{
                renderDygraph({coach_dygraph("busan",7)})
            }
    })
    
    observeEvent(input$btn2, {
        output$numOfGroup2 <-
            if(input$btn2 == 1){
                renderText({group_min("pusan", 1)[1]})
            }else if(input$btn2 == 2){
                renderText({group_min("pusan", 2)[1]})
            }else if(input$btn2 == 3){
                renderText({group_min("pusan", 3)[1]})
            }else if(input$btn2 == 4){
                renderText({group_min("pusan", 4)[1]})
            }else if(input$btn2 == 5){
                renderText({group_min("pusan", 5)[1]})
            }else if(input$btn2 == 6){
                renderText({group_min("pusan", 6)[1]})
            }else{
                renderText({group_min("pusan", 7)[1]})
            }
    })
    observeEvent(input$btn2, {
        output$prob2 <-
            if(input$btn2 == 1){
                renderText({group_min("pusan", 1)[2]})
            }else if(input$btn2 == 2){
                renderText({group_min("pusan", 2)[2]})
            }else if(input$btn2 == 3){
                renderText({group_min("pusan", 3)[2]})
            }else if(input$btn2 == 4){
                renderText({group_min("pusan", 4)[2]})
            }else if(input$btn2 == 5){
                renderText({group_min("pusan", 5)[2]})
            }else if(input$btn2 == 6){
                renderText({group_min("pusan", 6)[2]})
            }else{
                renderText({group_min("pusan", 7)[2]})
            }
    })
    observeEvent(input$btn2, {
        output$xxxPlot2 <-
            if(input$btn2 == 1){
                renderDygraph({coach_dygraph("busan",1)})
            }else if(input$btn2 == 2){
                renderDygraph({coach_dygraph("busan",2)})
            }else if(input$btn2 == 3){
                renderDygraph({coach_dygraph("busan",3)})
            }else if(input$btn2 == 4){
                renderDygraph({coach_dygraph("busan",4)})
            }else if(input$btn2 == 5){
                renderDygraph({coach_dygraph("busan",5)})
            }else if(input$btn2 == 6){
                renderDygraph({coach_dygraph("busan",6)})
            }else{
                renderDygraph({coach_dygraph("busan",7)})
            }
    })
    
    observeEvent(input$btn2, {
        output$numOfGroup2 <-
            if(input$btn2 == 1){
                renderText({group_min("pusan", 1)[1]})
            }else if(input$btn2 == 2){
                renderText({group_min("pusan", 2)[1]})
            }else if(input$btn2 == 3){
                renderText({group_min("pusan", 3)[1]})
            }else if(input$btn2 == 4){
                renderText({group_min("pusan", 4)[1]})
            }else if(input$btn2 == 5){
                renderText({group_min("pusan", 5)[1]})
            }else if(input$btn2 == 6){
                renderText({group_min("pusan", 6)[1]})
            }else{
                renderText({group_min("pusan", 7)[1]})
            }
    })
    observeEvent(input$btn2, {
        output$prob2 <-
            if(input$btn2 == 1){
                renderText({group_min("pusan", 1)[2]})
            }else if(input$btn2 == 2){
                renderText({group_min("pusan", 2)[2]})
            }else if(input$btn2 == 3){
                renderText({group_min("pusan", 3)[2]})
            }else if(input$btn2 == 4){
                renderText({group_min("pusan", 4)[2]})
            }else if(input$btn2 == 5){
                renderText({group_min("pusan", 5)[2]})
            }else if(input$btn2 == 6){
                renderText({group_min("pusan", 6)[2]})
            }else{
                renderText({group_min("pusan", 7)[2]})
            }
    })
    ###################################################
    observeEvent(input$btn12, {
        output$xxxPlot12 <-
            if(input$btn12 == 1){
                renderDygraph({coach_dygraph("chungnam",1)})
            }else if(input$btn12 == 2){
                renderDygraph({coach_dygraph("chungnam",2)})
            }else if(input$btn12 == 3){
                renderDygraph({coach_dygraph("chungnam",3)})
            }else if(input$btn12 == 4){
                renderDygraph({coach_dygraph("chungnam",4)})
            }else if(input$btn12 == 5){
                renderDygraph({coach_dygraph("chungnam",5)})
            }else if(input$btn12 == 6){
                renderDygraph({coach_dygraph("chungnam",6)})
            }else{
                renderDygraph({coach_dygraph("chungnam",7)})
            }
    })
    
    observeEvent(input$btn12, {
        output$numOfGroup12 <-
            if(input$btn12 == 1){
                renderText({group_min("chungnam", 1)[1]})
            }else if(input$btn12 == 2){
                renderText({group_min("chungnam", 2)[1]}) 
            }else if(input$btn12 == 3){
                renderText({group_min("chungnam", 3)[1]})
            }else if(input$btn12 == 4){
                renderText({group_min("chungnam", 4)[1]})
            }else if(input$btn12 == 5){
                renderText({group_min("chungnam", 5)[1]})
            }else if(input$btn12 == 6){
                renderText({group_min("chungnam", 6)[1]})
            }else{
                renderText({group_min("chungnam", 7)[1]})
            }
    })
    observeEvent(input$btn12, {
        output$prob12 <-
            if(input$btn12 == 1){
                renderText({group_min("chungnam", 1)[2]})
            }else if(input$btn12 == 2){
                renderText({group_min("chungnam", 2)[2]})
            }else if(input$btn12 == 3){
                renderText({group_min("chungnam", 3)[2]})
            }else if(input$btn12 == 4){
                renderText({group_min("chungnam", 4)[2]})
            }else if(input$btn12 == 5){
                renderText({group_min("chungnam", 5)[2]})
            }else if(input$btn12 == 6){
                renderText({group_min("chungnam", 6)[2]})
            }else{
                renderText({group_min("chungnam", 7)[2]})
            }
    })
    observeEvent(input$btn16, {
        output$xxxPlot16 <-
            if(input$btn16 == 1){
                renderDygraph({coach_dygraph("gyeongnam",1)})
            }else if(input$btn16 == 2){
                renderDygraph({coach_dygraph("gyeongnam",2)})
            }else if(input$btn16 == 3){
                renderDygraph({coach_dygraph("gyeongnam",3)})
            }else if(input$btn16 == 4){
                renderDygraph({coach_dygraph("gyeongnam",4)})
            }else if(input$btn16 == 5){
                renderDygraph({coach_dygraph("gyeongnam",5)})
            }else if(input$btn16 == 6){
                renderDygraph({coach_dygraph("gyeongnam",6)})
            }else{
                renderDygraph({coach_dygraph("gyeongnam",7)})
            }
    })
    
    observeEvent(input$btn16, {
        output$numOfGroup16 <-
            if(input$btn16 == 1){
                renderText({group_min("kyeongnam", 1)[1]})
            }else if(input$btn16 == 2){
                renderText({group_min("kyeongnam", 2)[1]})
            }else if(input$btn16 == 3){
                renderText({group_min("kyeongnam", 3)[1]})
            }else if(input$btn16 == 4){
                renderText({group_min("kyeongnam", 4)[1]})
            }else if(input$btn16 == 5){
                renderText({group_min("kyeongnam", 5)[1]})
            }else if(input$btn16 == 6){
                renderText({group_min("kyeongnam", 6)[1]})
            }else{
                renderText({group_min("kyeongnam", 7)[1]})
            }
    })
    observeEvent(input$btn16, {
        output$prob16 <-
            if(input$btn16 == 1){
                renderText({group_min("kyeongnam", 1)[2]})
            }else if(input$btn16 == 2){
                renderText({group_min("kyeongnam", 2)[2]})
            }else if(input$btn16 == 3){
                renderText({group_min("kyeongnam", 3)[2]})
            }else if(input$btn16 == 4){
                renderText({group_min("kyeongnam", 4)[2]})
            }else if(input$btn16 == 5){
                renderText({group_min("kyeongnam", 5)[2]})
            }else if(input$btn16 == 6){
                renderText({group_min("kyeongnam", 6)[2]})
            }else{
                renderText({group_min("kyeongnam", 7)[2]})
            }
    })
    observeEvent(input$btn13, {
        output$xxxPlot13 <-
            if(input$btn13 == 1){
                renderDygraph({coach_dygraph("jeonbuk",1)})
            }else if(input$btn13 == 2){
                renderDygraph({coach_dygraph("jeonbuk",2)})
            }else if(input$btn13 == 3){
                renderDygraph({coach_dygraph("jeonbuk",3)})
            }else if(input$btn13 == 4){
                renderDygraph({coach_dygraph("jeonbuk",4)})
            }else if(input$btn13 == 5){
                renderDygraph({coach_dygraph("jeonbuk",5)})
            }else if(input$btn13 == 6){
                renderDygraph({coach_dygraph("jeonbuk",6)})
            }else{
                renderDygraph({coach_dygraph("jeonbuk",7)})
            }
    })
    
    observeEvent(input$btn13, {
        output$numOfGroup13 <-
            if(input$btn13 == 1){
                renderText({group_min("jeonbuk", 1)[1]})
            }else if(input$btn13 == 2){
                renderText({group_min("jeonbuk", 2)[1]})
            }else if(input$btn13 == 3){
                renderText({group_min("jeonbuk", 3)[1]})
            }else if(input$btn13 == 4){
                renderText({group_min("jeonbuk", 4)[1]})
            }else if(input$btn13 == 5){
                renderText({group_min("jeonbuk", 5)[1]})
            }else if(input$btn13 == 6){
                renderText({group_min("jeonbuk", 6)[1]})
            }else{
                renderText({group_min("jeonbuk", 7)[1]})
            }
    })
    observeEvent(input$btn13, {
        output$prob13 <-
            if(input$btn13 == 1){
                renderText({group_min("jeonbuk", 1)[2]})
            }else if(input$btn13 == 2){
                renderText({group_min("jeonbuk", 2)[2]})
            }else if(input$btn13 == 3){
                renderText({group_min("jeonbuk", 3)[2]})
            }else if(input$btn13 == 4){
                renderText({group_min("jeonbuk", 4)[2]})
            }else if(input$btn13 == 5){
                renderText({group_min("jeonbuk", 5)[2]})
            }else if(input$btn13 == 6){
                renderText({group_min("jeonbuk", 6)[2]})
            }else{
                renderText({group_min("jeonbuk", 7)[2]})
            }
    })
    
    observeEvent(input$btn14, {
        output$xxxPlot14 <-
            if(input$btn14 == 1){
                renderDygraph({coach_dygraph("jeonnam",1)})
            }else if(input$btn14 == 2){
                renderDygraph({coach_dygraph("jeonnam",2)})
            }else if(input$btn14 == 3){
                renderDygraph({coach_dygraph("jeonnam",3)})
            }else if(input$btn14 == 4){
                renderDygraph({coach_dygraph("jeonnam",4)})
            }else if(input$btn14 == 5){
                renderDygraph({coach_dygraph("jeonnam",5)})
            }else if(input$btn14 == 6){
                renderDygraph({coach_dygraph("jeonnam",6)})
            }else{
                renderDygraph({coach_dygraph("jeonnam",7)})
            }
    })
    
    observeEvent(input$btn14, {
        output$numOfGroup14 <-
            if(input$btn14 == 1){
                renderText({group_min("jeonnam", 1)[1]})
            }else if(input$btn14 == 2){
                renderText({group_min("jeonnam", 2)[1]})
            }else if(input$btn14 == 3){
                renderText({group_min("jeonnam", 3)[1]})
            }else if(input$btn14 == 4){
                renderText({group_min("jeonnam", 4)[1]})
            }else if(input$btn14 == 5){
                renderText({group_min("jeonnam", 5)[1]})
            }else if(input$btn14 == 6){
                renderText({group_min("jeonnam", 6)[1]})
            }else{
                renderText({group_min("jeonnam", 7)[1]})
            }
    })
    observeEvent(input$btn14, {
        output$prob14 <-
            if(input$btn14 == 1){
                renderText({group_min("jeonnam", 1)[2]})
            }else if(input$btn14 == 2){
                renderText({group_min("jeonnam", 2)[2]})
            }else if(input$btn14 == 3){
                renderText({group_min("jeonnam", 3)[2]})
            }else if(input$btn14 == 4){
                renderText({group_min("jeonnam", 4)[2]})
            }else if(input$btn14 == 5){
                renderText({group_min("jeonnam", 5)[2]})
            }else if(input$btn14 == 6){
                renderText({group_min("jeonnam", 6)[2]})
            }else{
                renderText({group_min("jeonnam", 7)[2]})
            }
    })
    observeEvent(input$btn8, {
        output$xxxPlot8 <-
            if(input$btn8 == 1){
                renderDygraph({coach_dygraph("sejong",1)})
            }else if(input$btn8 == 2){
                renderDygraph({coach_dygraph("sejong",2)})
            }else if(input$btn8 == 3){
                renderDygraph({coach_dygraph("sejong",3)})
            }else if(input$btn8 == 4){
                renderDygraph({coach_dygraph("sejong",4)})
            }else if(input$btn8 == 5){
                renderDygraph({coach_dygraph("sejong",5)})
            }else if(input$btn8 == 6){
                renderDygraph({coach_dygraph("sejong",6)})
            }else{
                renderDygraph({coach_dygraph("sejong",7)})
            }
    })
    
    observeEvent(input$btn8, {
        output$numOfGroup8 <-
            if(input$btn8 == 1){
                renderText({group_min("sejong", 1)[1]})
            }else if(input$btn8 == 2){
                renderText({group_min("sejong", 2)[1]})
            }else if(input$btn8 == 3){
                renderText({group_min("sejong", 3)[1]})
            }else if(input$btn8 == 4){
                renderText({group_min("sejong", 4)[1]})
            }else if(input$btn8 == 5){
                renderText({group_min("sejong", 5)[1]})
            }else if(input$btn8 == 6){
                renderText({group_min("sejong", 6)[1]})
            }else{
                renderText({group_min("sejong", 7)[1]})
            }
    })
    observeEvent(input$btn8, {
        output$prob8 <-
            if(input$btn8 == 1){
                renderText({group_min("sejong", 1)[2]})
            }else if(input$btn8 == 2){
                renderText({group_min("sejong", 2)[2]})
            }else if(input$btn8 == 3){
                renderText({group_min("sejong", 3)[2]})
            }else if(input$btn8 == 4){
                renderText({group_min("sejong", 4)[2]})
            }else if(input$btn8 == 5){
                renderText({group_min("sejong", 5)[2]})
            }else if(input$btn8 == 6){
                renderText({group_min("sejong", 6)[2]})
            }else{
                renderText({group_min("sejong", 7)[2]})
            }
    })
    renderData <- ddply(totalData,~local1,summarise, totaltest = sum(totaltest,na.rm = T),
          positive = sum(positive,na.rm = T))%>%
        cbind(.,rate = paste0(round((.$positive/.$totaltest)*100, 2),"%"))
    
    ##############    1   ################
    output$Test1 <- renderText({
        renderData$totaltest[1]
    })
    output$Posi1 <- renderText({
        renderData$positive[1]
    })
    output$Rate1 <- renderText({
        renderData$rate[1]
    })
    ##############    2   ################
    output$Test2 <- renderText({
        renderData$totaltest[2]
    })
    output$Posi2 <- renderText({
        renderData$positive[2]
    })
    output$Rate2 <- renderText({
        renderData$rate[2]
    })
    ##############    3   ################
    output$Test3 <- renderText({
        renderData$totaltest[3]
    })
    output$Posi3 <- renderText({
        renderData$positive[3]
    })
    output$Rate3 <- renderText({
        renderData$rate[3]
    })
    ##############    4   ################
    output$Test4 <- renderText({
        renderData$totaltest[4]
    })
    output$Posi4 <- renderText({
        renderData$positive[4]
    })
    output$Rate4 <- renderText({
        renderData$rate[4]
    })
    ##############    5   ################
    output$Test5 <- renderText({
        renderData$totaltest[5]
    })
    output$Posi5 <- renderText({
        renderData$positive[5]
    })
    output$Rate5 <- renderText({
        renderData$rate[5]
    })
    ##############    6   ################
    output$Test6 <- renderText({
        renderData$totaltest[6]
    })
    output$Posi6 <- renderText({
        renderData$positive[6]
    })
    output$Rate1 <- renderText({
        renderData$rate[6]
    })
    ##############    7   ################
    output$Test7 <- renderText({
        renderData$totaltest[7]
    })
    output$Posi7 <- renderText({
        renderData$positive[7]
    })
    output$Rate7 <- renderText({
        renderData$rate[7]
    })
    ##############    8   ################
    output$Test8 <- renderText({
        renderData$totaltest[8]
    })
    output$Posi8 <- renderText({
        renderData$positive[8]
    })
    output$Rate8 <- renderText({
        renderData$rate[8]
    })
    ##############    9   ################
    output$Test9 <- renderText({
        renderData$totaltest[9]
    })
    output$Posi9 <- renderText({
        renderData$positive[9]
    })
    output$Rate9 <- renderText({
        renderData$rate[9]
    })
    ##############    10   ################
    output$Test10 <- renderText({
        renderData$totaltest[10]
    })
    output$Posi10 <- renderText({
        renderData$positive[10]
    })
    output$Rate10 <- renderText({
        renderData$rate[10]
    })
    ##############    11   ################
    output$Test11 <- renderText({
        renderData$totaltest[11]
    })
    output$Posi11 <- renderText({
        renderData$positive[11]
    })
    output$Rate11 <- renderText({
        renderData$rate[11]
    })
    ##############    12   ################
    output$Test12 <- renderText({
        renderData$totaltest[12]
    })
    output$Posi12 <- renderText({
        renderData$positive[12]
    })
    output$Rate12 <- renderText({
        renderData$rate[12]
    })
    ##############    13   ################
    output$Test13 <- renderText({
        renderData$totaltest[13]
    })
    output$Posi13 <- renderText({
        renderData$positive[13]
    })
    output$Rate13 <- renderText({
        renderData$rate[13]
    })
    ##############    14   ################
    output$Test14 <- renderText({
        renderData$totaltest[14]
    })
    output$Posi14 <- renderText({
        renderData$positive[14]
    })
    output$Rate14 <- renderText({
        renderData$rate[14]
    })
    ##############    15   ################
    output$Test15 <- renderText({
        renderData$totaltest[15]
    })
    output$Posi15 <- renderText({
        renderData$positive[15]
    })
    output$Rate15 <- renderText({
        renderData$rate[15]
    })
    ##############    16   ################
    output$Test16 <- renderText({
        renderData$totaltest[16]
    })
    output$Posi16 <- renderText({
        renderData$positive[16]
    })
    output$Rate16 <- renderText({
        renderData$rate[16]
    })
    ##############    17   ################
    output$Test17 <- renderText({
        renderData$totaltest[17]
    })
    output$Posi17 <- renderText({
        renderData$positive[17]
    })
    output$Rate17 <- renderText({
        renderData$rate[17]
    })
    
    
        output$localTestPlot2 <- renderDygraph({
            eco3<-xts(totalData[totalData$local1=="pusan",4], order.by = as.Date(totalData[totalData$local1=="pusan",1]))
            colnames(eco3)<- c("검사자")
            result5 <- dygraph(eco3, group="temp_rain", main="부산 날짜별 검사자 수")
            result5%>%dyRangeSelector()
        })
    
    output$localPositivePlot2 <- renderDygraph({
        eco4<-xts(totalData[totalData$local1=="pusan",5], order.by = as.Date(totalData[totalData$local1=="pusan",1]))
        colnames(eco4)<- c("확진자")
        result6 <- dygraph(eco4, group="temp_rain", main="부산 날짜별 확진자 수")
        result6 %>% dyRangeSelector()
    })
    
        output$localTestPlot3 <- renderDygraph({
            eco5<-xts(totalData[totalData$local1=="daegu",4], order.by = as.Date(totalData[totalData$local1=="daegu",1]))
            colnames(eco5)<- c("검사자")
            result7 <- dygraph(eco5, group="temp_rain", main="대구 날짜별 검사자 수")
            result7%>%dyRangeSelector()
        })
    
    output$localPositivePlot3 <- renderDygraph({
        eco5<-xts(totalData[totalData$local1=="daegu",5], order.by = as.Date(totalData[totalData$local1=="daegu",1]))
        colnames(eco5)<- c("확진자")
        result8 <- dygraph(eco5, group="temp_rain", main="대구 날짜별 확진자 수")
        result8 %>% dyRangeSelector()
    })
    
        output$localTestPlot4 <- renderDygraph({
            eco6<-xts(totalData[totalData$local1=="incheon",4], order.by = as.Date(totalData[totalData$local1=="incheon",1]))
            colnames(eco6)<- c("검사자")
            result9 <- dygraph(eco6, group="temp_rain", main="인천 날짜별 검사자 수")
            result9%>%dyRangeSelector()
        })
    
    output$localPositivePlot4 <- renderDygraph({
        eco7<-xts(totalData[totalData$local1=="incheon",5], order.by = as.Date(totalData[totalData$local1=="incheon",1]))
        colnames(eco7)<- c("확진자")
        result10 <- dygraph(eco7, group="temp_rain", main="인천 날짜별 확진자 수")
        result10 %>% dyRangeSelector()
    })
    
        output$localTestPlot5 <- renderDygraph({
            eco8<-xts(totalData[totalData$local1=="gwangju",4], order.by = as.Date(totalData[totalData$local1=="gwangju",1]))
            colnames(eco8)<- c("검사자")
            result11 <- dygraph(eco8, group="temp_rain", main="광주 날짜별 검사자 수")
            result11 %>%dyRangeSelector()
        })
    
    output$localPositivePlot5 <- renderDygraph({
        eco9<-xts(totalData[totalData$local1=="gwangju",5], order.by = as.Date(totalData[totalData$local1=="gwangju",1]))
        colnames(eco9)<- c("확진자")
        result12 <- dygraph(eco9, group="temp_rain", main="광주 날짜별 확진자 수")
        result12 %>% dyRangeSelector()
    })
    
        output$localTestPlot6 <- renderDygraph({
            eco10<-xts(totalData[totalData$local1=="daejeon",4], order.by = as.Date(totalData[totalData$local1=="daejeon",1]))
            colnames(eco10)<- c("검사자")
            result13 <- dygraph(eco10, group="temp_rain", main="대전 날짜별 검사자 수")
            result13%>%dyRangeSelector()
        })
    
    output$localPositivePlot6 <- renderDygraph({
        eco11<-xts(totalData[totalData$local1=="daejeon",5], order.by = as.Date(totalData[totalData$local1=="daejeon",1]))
        colnames(eco11)<- c("확진자")
        result14 <- dygraph(eco11, group="temp_rain", main="대전 날짜별 확진자 수")
        result14 %>% dyRangeSelector()
    })
    
        output$localTestPlot7 <- renderDygraph({
            eco12<-xts(totalData[totalData$local1=="ulsan",4], order.by = as.Date(totalData[totalData$local1=="ulsan",1]))
            colnames(eco12)<- c("검사자")
            result15 <- dygraph(eco12, group="temp_rain", main="울산 날짜별 검사자 수")
            result15%>%dyRangeSelector()
        })
    
    output$localPositivePlot7 <- renderDygraph({
        eco13<-xts(totalData[totalData$local1=="ulsan",5], order.by = as.Date(totalData[totalData$local1=="ulsan",1]))
        colnames(eco13)<- c("확진자")
        result16 <- dygraph(eco13, group="temp_rain", main="울산 날짜별 확진자 수")
        result16 %>% dyRangeSelector()
    })
    
        output$localTestPlot8 <- renderDygraph({
            eco14<-xts(totalData[totalData$local1=="sejong",4], order.by = as.Date(totalData[totalData$local1=="sejong",1]))
            colnames(eco14)<- c("검사자")
            result17 <- dygraph(eco14, group="temp_rain", main="세종 날짜별 검사자 수")
            result17%>%dyRangeSelector()
        })
    
    output$localPositivePlot8 <- renderDygraph({
        eco15<-xts(totalData[totalData$local1=="sejong",5], order.by = as.Date(totalData[totalData$local1=="sejong",1]))
        colnames(eco15)<- c("확진자")
        result18 <- dygraph(eco15, group="temp_rain", main="세종 날짜별 확진자 수")
        result18 %>% dyRangeSelector()
    })
    
        output$localTestPlot9 <- renderDygraph({
            eco16<-xts(totalData[totalData$local1=="gyeonggi",4], order.by = as.Date(totalData[totalData$local1=="gyeonggi",1]))
            colnames(eco16)<- c("검사자")
            result19 <- dygraph(eco16, group="temp_rain", main="경기 날짜별 검사자 수")
            result19%>%dyRangeSelector()
        })
    
    output$localPositivePlot9 <- renderDygraph({
        eco17<-xts(totalData[totalData$local1=="gyeonggi",5], order.by = as.Date(totalData[totalData$local1=="gyeonggi",1]))
        colnames(eco17)<- c("확진자")
        result20 <- dygraph(eco17, group="temp_rain", main="경기 날짜별 확진자 수")
        result20 %>% dyRangeSelector()
    })
    
        output$localTestPlot10 <- renderDygraph({
            eco18<-xts(totalData[totalData$local1=="gangwon",4], order.by = as.Date(totalData[totalData$local1=="gangwon",1]))
            colnames(eco18)<- c("검사자")
            result20 <- dygraph(eco18, group="temp_rain", main="강원 날짜별 검사자 수")
            result20%>%dyRangeSelector()
        })
    
    output$localPositivePlot10 <- renderDygraph({
        eco19<-xts(totalData[totalData$local1=="gangwon",5], order.by = as.Date(totalData[totalData$local1=="gangwon",1]))
        colnames(eco19)<- c("확진자")
        result21 <- dygraph(eco19, group="temp_rain", main="강원 날짜별 확진자 수")
        result21 %>% dyRangeSelector()
    })
    
        output$localTestPlot11 <- renderDygraph({
            eco20<-xts(totalData[totalData$local1=="chungbuk",4], order.by = as.Date(totalData[totalData$local1=="chungbuk",1]))
            colnames(eco20)<- c("검사자")
            result22 <- dygraph(eco20, group="temp_rain", main="충북 날짜별 검사자 수")
            result22%>%dyRangeSelector()
        })
    
    output$localPositivePlot11 <- renderDygraph({
        eco21<-xts(totalData[totalData$local1=="chungbuk",5], order.by = as.Date(totalData[totalData$local1=="chungbuk",1]))
        colnames(eco21)<- c("확진자")
        result23 <- dygraph(eco21, group="temp_rain", main="충북 날짜별 확진자 수")
        result23 %>% dyRangeSelector()
    })
    
        output$localTestPlot12 <- renderDygraph({
            eco22<-xts(totalData[totalData$local1=="chungnam",4], order.by = as.Date(totalData[totalData$local1=="chungnam",1]))
            colnames(eco22)<- c("검사자")
            result24 <- dygraph(eco22, group="temp_rain", main="충남 날짜별 검사자 수")
            result24%>%dyRangeSelector()
        })
    
    output$localPositivePlot12 <- renderDygraph({
        eco23<-xts(totalData[totalData$local1=="chungnam",5], order.by = as.Date(totalData[totalData$local1=="chungnam",1]))
        colnames(eco23)<- c("확진자")
        result25 <- dygraph(eco23, group="temp_rain", main="충남 날짜별 확진자 수")
        result25 %>% dyRangeSelector()
    })
    
        output$localTestPlot13 <- renderDygraph({
            eco24<-xts(totalData[totalData$local1=="jeonbuk",4], order.by = as.Date(totalData[totalData$local1=="jeonbuk",1]))
            colnames(eco24)<- c("검사자")
            result26 <- dygraph(eco24, group="temp_rain", main="전북 날짜별 검사자 수")
            result26%>%dyRangeSelector()
        })
    
    output$localPositivePlot13 <- renderDygraph({
        eco25<-xts(totalData[totalData$local1=="jeonbuk",5], order.by = as.Date(totalData[totalData$local1=="jeonbuk",1]))
        colnames(eco25)<- c("확진자")
        result27 <- dygraph(eco25, group="temp_rain", main="전북 날짜별 확진자 수")
        result27 %>% dyRangeSelector()
    })
    
        output$localTestPlot14 <- renderDygraph({
            eco26<-xts(totalData[totalData$local1=="jeonnam",4], order.by = as.Date(totalData[totalData$local1=="jeonnam",1]))
            colnames(eco26)<- c("검사자")
            result28 <- dygraph(eco26, group="temp_rain", main="전남 날짜별 검사자 수")
            result28%>%dyRangeSelector()
        })
    
    output$localPositivePlot14 <- renderDygraph({
        eco4<-xts(totalData[totalData$local1=="jeonnam",5], order.by = as.Date(totalData[totalData$local1=="jeonnam",1]))
        colnames(eco4)<- c("확진자")
        result29 <- dygraph(eco4, group="temp_rain", main="전남 날짜별 확진자 수")
        result29 %>% dyRangeSelector()
    })
    
        output$localTestPlot15 <- renderDygraph({
            eco27<-xts(totalData[totalData$local1=="kyeongbuk",4], order.by = as.Date(totalData[totalData$local1=="kyeongbuk",1]))
            colnames(eco27)<- c("검사자")
            result30 <- dygraph(eco27, group="temp_rain", main="경북 날짜별 검사자 수")
            result30%>%dyRangeSelector()
        })
    
    output$localPositivePlot15<- renderDygraph({
        eco28<-xts(totalData[totalData$local1=="kyeongbuk",5], order.by = as.Date(totalData[totalData$local1=="kyeongbuk",1]))
        colnames(eco28)<- c("확진자")
        result31 <- dygraph(eco28, group="temp_rain", main="경북 날짜별 확진자 수")
        result31 %>% dyRangeSelector()
    })
    
        output$localTestPlot16 <- renderDygraph({
            eco29<-xts(totalData[totalData$local1=="kyeongnam",4], order.by = as.Date(totalData[totalData$local1=="kyeongnam",1]))
            colnames(eco29)<- c("검사자")
            result32 <- dygraph(eco29, group="temp_rain", main="경남 날짜별 검사자 수")
            result32%>%dyRangeSelector()
        })
    
    output$localPositivePlot16 <- renderDygraph({
        eco30<-xts(totalData[totalData$local1=="kyeongnam",5], order.by = as.Date(totalData[totalData$local1=="kyeongnam",1]))
        colnames(eco30)<- c("확진자")
        result33 <- dygraph(eco30, group="temp_rain", main="경남 날짜별 확진자 수")
        result33 %>% dyRangeSelector()
    })
    
        output$localTestPlot17 <- renderDygraph({
            eco31<-xts(totalData[totalData$local1=="jeju",4], order.by = as.Date(totalData[totalData$local1=="jeju",1]))
            colnames(eco31)<- c("검사자")
            result34 <- dygraph(eco31, group="temp_rain", main="제주 날짜별 검사자 수")
            result34%>%dyRangeSelector()
        })
    
    output$localPositivePlot17 <- renderDygraph({
        eco32<-xts(totalData[totalData$local1=="jeju",5], order.by = as.Date(totalData[totalData$local1=="jeju",1]))
        colnames(eco32)<- c("확진자")
        result35 <- dygraph(eco32, group="temp_rain", main="제주 날짜별 확진자 수")
        result35 %>% dyRangeSelector()
    })
    
    
    USER <- reactiveValues(Logged = Logged)
    
    observe({
        if (USER$Logged == FALSE) {
            if (!is.null(input$Login)) {
                if (length(input$Login) > 0) {
                    Username <- isolate(input$userName)
                    Password <- isolate(input$passwd)
                    Id.username <- which(my_username == Username)
                    Id.password <- which(my_password == Password)
                    if (length(Id.username) > 0 &
                        length(Id.password) > 0) {
                        if (Id.username == Id.password) {
                            USER$Logged <- TRUE
                        }
                    }
                }
            }
        }
    })
    
    output$page <- renderUI({
        if (USER$Logged == FALSE) {
            do.call(bootstrapPage, ui1())
        } else {
            do.call(bootstrapPage, ui2())
        }
    })

    output$acumDetect <- renderText({
        url <- "http://ncov.mohw.go.kr"
        usedDetect <- read_html(url)
        detectInfo <- html_nodes(usedDetect, css='.suminfo')
        y<- detectInfo[1] %>% html_nodes('.num') %>%  html_text()
        y <- substr(y[2], 1,9)
        y
    })
    
    output$acumPositive <- renderText({
        url <- "http://ncov.mohw.go.kr"
        usedPositive <- read_html(url)
        PositiveInfo <- html_nodes(usedPositive, css='.liveNum')
        x<- PositiveInfo[1] %>% html_nodes('.num') %>%  html_text()
        x <- substr(x[1], 5,10)
        x
    })

    output$test <- renderText({
        url <- "http://ncov.mohw.go.kr"
        usedRatio <- read_html(url)
        ratioInfo <- html_nodes(usedRatio, css='.suminfo')
        z<- ratioInfo[1] %>% html_nodes('.num') %>%  html_text()
        z <- (z[3])
        z
    })
    
    output$positivePlot <- renderDygraph({
        eco1<-xts(datDaily$confirmed, order.by = as.Date(datDaily$date))
        colnames(eco1)<- c("확진자")
        result1 <- dygraph(eco1, group="temp_rain", main="전국 날짜별 확진자 수")
        result1%>%dyRangeSelector()
    })
    
    
    
    output$testingPlot <- renderDygraph({
        eco2<-xts(datDaily$tested, order.by = as.Date(datDaily$date))
        colnames(eco2)<- c("검사자")
        result2 <- dygraph(eco2, group="temp_rain", main="전국 날짜별 검사자 수")
        result2%>%dyRangeSelector()
    })
    
    output$localTestPlot1 <- renderDygraph({
        eco3<-xts(totalData[totalData$local1=="seoul",3], order.by = as.Date(totalData[totalData$local1=="seoul",1]))
        colnames(eco3)<- c("검사자")
        result3 <- dygraph(eco3, group="temp_rain", main="서울 날짜별 검사자 수")
        result3%>%dyRangeSelector()
    })
    
    output$localPositivePlot1 <- renderDygraph({
        eco4<-xts(totalData[totalData$local1=="seoul",4], order.by = as.Date(totalData[totalData$local1=="seoul",1]))
        colnames(eco4)<- c("확진자")
        result4 <- dygraph(eco4, group="temp_rain", main="서울 날짜별 확진자 수")
        result4 %>% dyRangeSelector()
    })
    
    observeEvent(input$jumpToNation, {
        updateNavbarPage(session, "tabs",
                          selected = "2")
    })
    
    observeEvent(input$jumpToLocal, {
        updateNavbarPage(session, "tabs",
                          selected = "3")
    })
    observeEvent(input$backBtn, {
         updateNavbarPage(session, "tabs",
                          selected = "1")
    })
    
    output$Plot1 <-renderPlotly({
        pie_by_region("서울특별시")
    })
    output$Plot2 <-renderPlotly({
        pie_by_region("부산광역시")
    })
    output$Plot3 <-renderPlotly({
        pie_by_region("대구광역시")
    })
    output$Plot4 <-renderPlotly({
        pie_by_region("인천광역시")
    })
    output$Plot5 <-renderPlotly({
        pie_by_region("광주광역시")
    })
    output$Plot6 <-renderPlotly({
        pie_by_region("대전광역시")
    })
    output$Plot7 <-renderPlotly({
        pie_by_region("울산광역시")
    })
    output$Plot8 <-renderPlotly({
        pie_by_region("세종특별자치시")
    })
    output$Plot9 <-renderPlotly({
        pie_by_region("경기도")
    })
    output$Plot10 <-renderPlotly({
        pie_by_region("강원도")
    })
    output$Plot11 <-renderPlotly({
        pie_by_region("충청북도")
    })
    output$Plot12 <-renderPlotly({
        pie_by_region("충청남도")
    })
    output$Plot13 <-renderPlotly({
        pie_by_region("전라북도")
    })
    output$Plot14 <-renderPlotly({
        pie_by_region("전라남도")
    })
    output$Plot15 <-renderPlotly({
        pie_by_region("경상북도")
    })
    output$Plot16 <-renderPlotly({
        pie_by_region("경상남도")
    })
    output$Plot17 <-renderPlotly({
        pie_by_region("제주도")
    })

    output$locallyPlot <- renderPlotly({
        gradient <- colorRampPalette(c('lightblue', 'darkblue'))
        
        colors <- gradient(dim(abc)[1])[
            as.numeric(cut(100*abc$value, breaks = dim(abc)[1]))]
        
        plot_ly(abc, labels = ~name, values = ~value, type = 'pie',
                textposition = 'inside',
                textinfo = 'label+percent',
                insidetextfont = list(color = '#FFFFFF'),
                text = ~paste0("누적 확진자 수 : "),
                hoverinfo = 'text',
                marker = list(colors = colors),
                showlegend = FALSE) %>%
            layout(paper_bgcolor = '#e8f6f9')
    })

    ########지도 시각화
    output$hc_global <- renderHighchart({
        hcmap(
            map = "countries/kr/kr-all",
            data = hc_key,
            value = "value",
            dataLabels = list(enabled = F)
        )
    })
    
    ################ 데이터 넣기
    observeEvent(input$inputBtn, {
        date <- input$date
        local1 <- local_code[which(local_code$code == input$regionSelect), 2]
        local2 <- input$clinicSelect
        totaltest <- as.numeric(input$testedPeople)
        positive <- as.numeric(input$positivePeople)
        
        if (is.na(positive) | is.na(totaltest)) {
            shinyalert("실패!", "데이터 입력에 실패 하였습니다!", type = "error")
        } else if (any(totaltest < 0, positive < 0)) {
            shinyalert("실패!", "데이터 입력에 실패 하였습니다!", type = "error")
        } else {
            results <- data.frame(
                `date` = date,
                `local1` = local1,
                `local2` = local2,
                `totaltest` = totaltest,
                `positive` = positive,
                `postiive_rate` = round(positive / totaltest, 3)
            )
            
            ## --- CHECK .RDS -> SAVE .RDS
            if (file.exists(paste0("data/",local1, "table.rds"))) {
                check_rds <- readRDS(paste0("data/",local1, "table.rds"))
                check_rds <- rbind(check_rds, results)
                saveRDS(check_rds, paste0("data/",local1, "table.rds"))
                shinyalert("성공", "데이터 입력에 성공 하였습니다!", type = "success")
            } else {
                saveRDS(results, paste0("data/",local1, "table.rds"))
                shinyalert("성공", "데이터 입력에 성공 하였습니다!", type = "success")
            }
        }
        
    })

})


shinyApp(ui, server)


