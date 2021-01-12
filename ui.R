library(shiny)
library(shinythemes)

ui1 <- function() {
  bootstrapPage(
    style = 'font-family: "Lato", sans-serif;',
    includeCSS("www/assets/css/asdf.css"),
    div(
      class = "sidenav",
      id = "login",
      div(
        class = "login-main-text",
        img(src = "images/coach_logo2.png", class = "img-fluid", style = "width:100%;")
      ),
      div(
        class = "login-form",
        wellPanel(
          HTML('<div class="form-group shiny-input-container">
  <label class="control-label" for="userName">User ID</label>
  <input id="userName" type="text" class="form-control" placeholder="User ID" value=""/>
</div>'),
          HTML('<div class="form-group shiny-input-container">
  <label class="control-label" for="passwd">Password</label>
  <input id="passwd" type="password" class="form-control" placeholder="Password" value=""/>
</div>'),
          HTML('<button id="Login" type="button" class="btn btn-black action-button">Log in</button>'),
          style = "background-color: #164068; border-color: #164068;"
        )
      )
    ),
    HTML('<div class="main">
        <img src="images/team_coach.png" class="img-team" alt="">
        <img src="images/team_comment1.png" class="img-comment" alt="">
 </div>')
  )
}

ui2 <- function(){
  
    navbarPage("Team coach",
               id = "tabs",
               
               tabPanel("MAIN",
                        value = "1",
                        htmlTemplate(
                            filename = "www/index.html",
                            btnToNation = actionButton('jumpToNation', '보러가기'),
                            btnToLocal = actionButton('jumpToLocal', '보러가기')
                        )),
               tabPanel("전국",
                        value = "2",
                        htmlTemplate(
                            filename= "www/nation.html",
                            
                            abcdef = dygraphOutput("positivePlot"),
                            abcd = dygraphOutput("testingPlot"),
                            
                            mapPlot = highchartOutput("hc_global", width = "100%", height = "100%"),
                            acumPositive = textOutput("acumPositive"),
                            acumDetect = textOutput("acumDetect"),
                            acumRatio = textOutput("test"),
                            
                            day1Btn = actionButton("day1", "1day"),
                            day3Btn = actionButton("day3", "3day"),
                            day7Btn = actionButton("day7", "7day"),
                            locallyPlot = plotlyOutput("locallyPlot"),
                            day1Btn = sliderInput("btn", "단위를 선택하세요",min = 1, max = 7, value = 1),
                            xxx = dygraphOutput("xxxPlot"),
                            zzz = textOutput("numOfGroup"),
                            qqq = textOutput("prob")
                        )),
               tabPanel("지역",
                        value = "3",
                        selected = 2,
                        tabsetPanel(
                          tabPanel("부산",
                                   htmlTemplate(
                                     filename= "www/local_busan.html",
                                     seoulTest = textOutput("Test2"),
                                     seoulPosi = textOutput("Posi2"),
                                     seoulRate = textOutput("Rate2"),
                                     a = dygraphOutput("localTestPlot2"),
                                     b = dygraphOutput("localPositivePlot2"),
                                     yyy = plotlyOutput("Plot2"),
                                     day1Btn = sliderInput("btn2", "단위를 선택하세요",min = 1, max = 7, value = 1),
                                     xxx = dygraphOutput("xxxPlot2"),
                                     zzz = textOutput("numOfGroup2"),
                                     qqq = textOutput("prob2")
                                     
                                   )),
                          tabPanel("서울",
                                   htmlTemplate(
                                     filename= "www/local_seoul.html",
                                     seoulTest = textOutput("Test1"),
                                     seoulPosi = textOutput("Posi1"),
                                     seoulRate = textOutput("Rate1"),
                                     day1Btn = actionButton("day1", "1day"),
                                     day3Btn = actionButton("day3", "3day"),
                                     day7Btn = actionButton("day7", "7day"),
                                     a = dygraphOutput("localTestPlot1"),
                                     b = dygraphOutput("localPositivePlot1"),
                                     yyy = plotlyOutput("Plot1")
                                   )),

                          tabPanel("대구",
                                   htmlTemplate(
                                     filename= "www/local_daegu.html",
                                     seoulTest = textOutput("Test3"),
                                     seoulPosi = textOutput("Posi3"),
                                     seoulRate = textOutput("Rate3"),
                                     day1Btn = actionButton("day1", "1day"),
                                     day3Btn = actionButton("day3", "3day"),
                                     day7Btn = actionButton("day7", "7day"),
                                     a = dygraphOutput("localTestPlot3"),
                                     b = dygraphOutput("localPositivePlot3"),
                                     yyy = plotlyOutput("Plot3")
                                   )),
                          tabPanel("인천",
                                   htmlTemplate(
                                     filename= "www/local_inchoen.html",
                                     seoulTest = textOutput("Test4"),
                                     seoulPosi = textOutput("Posi4"),
                                     seoulRate = textOutput("Rate4"),
                                     day1Btn = actionButton("day1", "1day"),
                                     day3Btn = actionButton("day3", "3day"),
                                     day7Btn = actionButton("day7", "7day"),
                                     a = dygraphOutput("localTestPlot4"),
                                     b = dygraphOutput("localPositivePlot4"),
                                     yyy = plotlyOutput("Plot4")
                                   )),
                          tabPanel("광주",
                                   htmlTemplate(
                                     filename= "www/local_gwangju.html",
                                     seoulTest = textOutput("Test5"),
                                     seoulPosi = textOutput("Posi5"),
                                     seoulRate = textOutput("Rate5"),
                                     day1Btn = actionButton("day1", "1day"),
                                     day3Btn = actionButton("day3", "3day"),
                                     day7Btn = actionButton("day7", "7day"),
                                     a = dygraphOutput("localTestPlot5"),
                                     b = dygraphOutput("localPositivePlot5"),
                                     yyy = plotlyOutput("Plot5")
                                   )),
                          tabPanel("대전",
                                   htmlTemplate(
                                     filename= "www/local_daejeon.html",
                                     seoulTest = textOutput("Test6"),
                                     seoulPosi = textOutput("Posi6"),
                                     seoulRate = textOutput("Rate6"),
                                     day1Btn = actionButton("day1", "1day"),
                                     day3Btn = actionButton("day3", "3day"),
                                     day7Btn = actionButton("day7", "7day"),
                                     a = dygraphOutput("localTestPlot6"),
                                     b = dygraphOutput("localPositivePlot6"),
                                     yyy = plotlyOutput("Plot6")
                                   )),
                          tabPanel("울산",
                                   htmlTemplate(
                                     filename= "www/local_ulsan.html",
                                     seoulTest = textOutput("Test7"),
                                     seoulPosi = textOutput("Posi7"),
                                     seoulRate = textOutput("Rate7"),
                                     day1Btn = actionButton("day1", "1day"),
                                     day3Btn = actionButton("day3", "3day"),
                                     day7Btn = actionButton("day7", "7day"),
                                     a = dygraphOutput("localTestPlot7"),
                                     b = dygraphOutput("localPositivePlot7"),
                                     yyy = plotlyOutput("Plot7")
                                   )),
                          tabPanel("세종",
                                   htmlTemplate(
                                     filename= "www/local_sejong.html",
                                     seoulTest = textOutput("Test8"),
                                     seoulPosi = textOutput("Posi8"),
                                     seoulRate = textOutput("Rate8"),
                                     day1Btn = actionButton("day1", "1day"),
                                     day3Btn = actionButton("day3", "3day"),
                                     day7Btn = actionButton("day7", "7day"),
                                     a = dygraphOutput("localTestPlot8"),
                                     b = dygraphOutput("localPositivePlot8"),
                                     yyy = plotlyOutput("Plot8"),
                                     day1Btn = sliderInput("btn8", "단위를 선택하세요",min = 1, max = 7, value = 1),
                                     xxx = dygraphOutput("xxxPlot8"),
                                     zzz = textOutput("numOfGroup8"),
                                     qqq = textOutput("prob8")
                                   )),
                          tabPanel("경기",
                                   htmlTemplate(
                                     filename= "www/local_gunggi.html",
                                     seoulTest = textOutput("Test9"),
                                     seoulPosi = textOutput("Posi9"),
                                     seoulRate = textOutput("Rate9"),
                                     day1Btn = actionButton("day1", "1day"),
                                     day3Btn = actionButton("day3", "3day"),
                                     day7Btn = actionButton("day7", "7day"),
                                     a = dygraphOutput("localTestPlot9"),
                                     b = dygraphOutput("localPositivePlot9"),
                                     yyy = plotlyOutput("Plot9")
                                   )),
                          tabPanel("강원",
                                   htmlTemplate(
                                     filename= "www/local_gangwon.html",
                                     seoulTest = textOutput("Test10"),
                                     seoulPosi = textOutput("Posi10"),
                                     seoulRate = textOutput("Rate10"),
                                     day1Btn = actionButton("day1", "1day"),
                                     day3Btn = actionButton("day3", "3day"),
                                     day7Btn = actionButton("day7", "7day"),
                                     a = dygraphOutput("localTestPlot10"),
                                     b = dygraphOutput("localPositivePlot10"),
                                     yyy = plotlyOutput("Plot10")
                                   )),
                          tabPanel("충북",
                                   htmlTemplate(
                                     filename= "www/local_cb.html",
                                     seoulTest = textOutput("Test11"),
                                     seoulPosi = textOutput("Posi11"),
                                     seoulRate = textOutput("Rate11"),
                                     day1Btn = actionButton("day1", "1day"),
                                     day3Btn = actionButton("day3", "3day"),
                                     day7Btn = actionButton("day7", "7day"),
                                     a = dygraphOutput("localTestPlot11"),
                                     b = dygraphOutput("localPositivePlot11"),
                                     yyy = plotlyOutput("Plot11")
                                   )),
                          tabPanel("충남",
                                   htmlTemplate(
                                     filename= "www/local_cn.html",
                                     seoulTest = textOutput("Test12"),
                                     seoulPosi = textOutput("Posi12"),
                                     seoulRate = textOutput("Rate12"),
                                     day1Btn = actionButton("day1", "1day"),
                                     day3Btn = actionButton("day3", "3day"),
                                     day7Btn = actionButton("day7", "7day"),
                                     a = dygraphOutput("localTestPlot12"),
                                     b = dygraphOutput("localPositivePlot12"),
                                     yyy = plotlyOutput("Plot12"),
                                     day1Btn = sliderInput("btn12", "단위를 선택하세요",min = 1, max = 7, value = 1),
                                     xxx = dygraphOutput("xxxPlot12"),
                                     zzz = textOutput("numOfGroup12"),
                                     qqq = textOutput("prob12")
                                   )),
                          tabPanel("전북",
                                   htmlTemplate(
                                     filename= "www/local_jb.html",
                                     seoulTest = textOutput("Test13"),
                                     seoulPosi = textOutput("Posi13"),
                                     seoulRate = textOutput("Rate13"),
                                     day1Btn = actionButton("day1", "1day"),
                                     day3Btn = actionButton("day3", "3day"),
                                     day7Btn = actionButton("day7", "7day"),
                                     a = dygraphOutput("localTestPlot13"),
                                     b = dygraphOutput("localPositivePlot13"),
                                     yyy = plotlyOutput("Plot13"),
                                     day1Btn = sliderInput("btn13", "단위를 선택하세요",min = 1, max = 7, value = 1),
                                     xxx = dygraphOutput("xxxPlot13"),
                                     zzz = textOutput("numOfGroup13"),
                                     qqq = textOutput("prob13")
                                   )),
                          tabPanel("전남",
                                   htmlTemplate(
                                     filename= "www/local_jn.html",
                                     seoulTest = textOutput("Test14"),
                                     seoulPosi = textOutput("Posi14"),
                                     seoulRate = textOutput("Rate14"),
                                     day1Btn = actionButton("day1", "1day"),
                                     day3Btn = actionButton("day3", "3day"),
                                     day7Btn = actionButton("day7", "7day"),
                                     a = dygraphOutput("localTestPlot14"),
                                     b = dygraphOutput("localPositivePlot14"),
                                     yyy = plotlyOutput("Plot14"),
                                     day1Btn = sliderInput("btn14", "단위를 선택하세요",min = 1, max = 7, value = 1),
                                     xxx = dygraphOutput("xxxPlot14"),
                                     zzz = textOutput("numOfGroup14"),
                                     qqq = textOutput("prob14")
                                   )),
                          tabPanel("경북",
                                   htmlTemplate(
                                     filename= "www/local_gb.html",
                                     seoulTest = textOutput("Test15"),
                                     seoulPosi = textOutput("Posi15"),
                                     seoulRate = textOutput("Rate15"),
                                     day1Btn = actionButton("day1", "1day"),
                                     day3Btn = actionButton("day3", "3day"),
                                     day7Btn = actionButton("day7", "7day"),
                                     a = dygraphOutput("localTestPlot15"),
                                     b = dygraphOutput("localPositivePlot15"),
                                     yyy = plotlyOutput("Plot15")
                                   )),
                          tabPanel("경남",
                                   htmlTemplate(
                                     filename= "www/local_gn.html",
                                     seoulTest = textOutput("Test16"),
                                     seoulPosi = textOutput("Posi16"),
                                     seoulRate = textOutput("Rate16"),
                                     day1Btn = actionButton("day1", "1day"),
                                     day3Btn = actionButton("day3", "3day"),
                                     day7Btn = actionButton("day7", "7day"),
                                     a = dygraphOutput("localTestPlot16"),
                                     b = dygraphOutput("localPositivePlot16"),
                                     yyy = plotlyOutput("Plot16"),
                                     day1Btn = sliderInput("btn16", "단위를 선택하세요",min = 1, max = 7, value = 1),
                                     xxx = dygraphOutput("xxxPlot16"),
                                     zzz = textOutput("numOfGroup16"),
                                     qqq = textOutput("prob16")
                                   )),
                          tabPanel("제주",
                                   htmlTemplate(
                                     filename= "www/local_jeju.html",
                                     seoulTest = textOutput("Test17"),
                                     seoulPosi = textOutput("Posi17"),
                                     seoulRate = textOutput("Rate17"),
                                     day1Btn = actionButton("day1", "1day"),
                                     day3Btn = actionButton("day3", "3day"),
                                     day7Btn = actionButton("day7", "7day"),
                                     a = dygraphOutput("localTestPlot17"),
                                     b = dygraphOutput("localPositivePlot17"),
                                     yyy = plotlyOutput("Plot17")
                                   ))
                        )),
               tabPanel("데이터 입력",
                        value = "4",
                        useShinyalert(),
                        
                        
                        htmlTemplate(
                          filename = "www/inputdata.html",
                          region = selectInput("regionSelect", "지역을 선택하세요", 
                                               choices = list("전국" =0,"서울" = 1, "부산" = 2, "대구" = 3, "인천" = 4, "광주" = 5, "대전" = 6, "울산" = 7, "세종" = 8, "경기" = 9, "강원" = 10, "충북" = 11, "충남" = 12, "전북" = 13, "전남" = 14, "경북" = 15, "경남" = 16, "제주" = 17 ), 
                                               selected = 0),
                          
                          clinic =selectInput("clinicSelect", "선별진료소를 선택하세요", 
                                              choices = list("default" = 1, "선별진료소1" = 2, "선별진료소2" = 3), 
                                              selected = 1),

                          date =  dateInput("date", "날짜를 선택하세요", value = Sys.Date()),

                          testedPeople = numericInput("testedPeople", "검사자수 입력하세요", value = 0),

                          positivePeople =  numericInput("positivePeople", "확진자수 입력하세요", value = 0),
                          
                          dataInputBtn = actionButton("inputBtn","입력", style = "background-color : #4895C5; float : right"),
                          
                          backBtn = actionButton("backBtn", "메인으로", style = "float : left")
                          
                          )
                        )
    )

}

ui <- htmlOutput("page")

