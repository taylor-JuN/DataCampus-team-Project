coach_dygraph <- function(region, day) {
  library(zoo)
  library(dygraphs)
  library(xts)
  
  path <- paste0("/Users/taylor/Desktop/DC/test/data/dygraph/est_", region, ".txt")
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
  colnames(total) <- c("실제값", "이동평균값")
  
  graph <- dygraph(total) %>%
    dyRangeSelector() %>%
    dyOptions(drawPoints = T, colors = c("#164068", "black"), drawGrid = F)
  
  return(graph)
}

coach_dygraph("busan",2)


pie_by_region <- function(region_name){
  region_name = "서울특별시"
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
          showlegend = FALSE)
}
pie_by_region("대구광역시")


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
      for (a in unique(week$week)[2:length(unique(week$week))]) {
        b <- a - 1
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
    
    min(ind_vec)
    
    ind_group <- which(ind_vec==min(ind_vec))
    
    ind_final <- c(ind_group,round(p,4)*100)
    
    ind_final
    
  }
  
}





group_min("pusan", 6)
