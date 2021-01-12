observeEvent(input$btn, {
        output$xxxPlot <-
            if(input$btn == 1){
                renderDygraph({coach_dygraph("busan",1)})
            }else if(input$btn == 2){
                renderDygraph({coach_dygraph("busan",2)})
            }else if(input$btn == 3){
                renderDygraph({coach_dygraph("busan",3)})
            }else if(input$btn == 4){
                renderDygraph({coach_dygraph("busan",4)})
            }else if(input$btn == 5){
                renderDygraph({coach_dygraph("busan",5)})
            }else if(input$btn == 6){
                renderDygraph({coach_dygraph("busan",6)})
            }else{
                renderDygraph({coach_dygraph("busan",7)})
            }
    })
    
    observeEvent(input$btn, {
        output$numOfGroup <-
            if(input$btn == 1){
                renderText({group_min("korea", 1)[1]})
            }else if(input$btn == 2){
                renderText({group_min("korea", )[1]})
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