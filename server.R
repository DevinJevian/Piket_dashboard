# server
server <- function(input,output,session){
    
    
    output$member_act <- renderUI({
        nrowm <- nrow(member)
        if(nrowm >=7){
            box(status = "info",width = 12,align="center",
                div(h2("Active Mentor"),align = "center"),
                div(h3(paste(member$start_date[1],"-",member$end_date[1])),align = "center"),
                userList(
                userListItem(src = member$pict[1],
                             user_name = member$Nama[1],
                             description = member$desc[1]
                             ),
                userListItem(src = member$pict[2],
                             user_name = member$Nama[2],
                             description = member$desc[2]
                             ),
                userListItem(src = member$pict[3],
                             user_name = member$Nama[3],
                             description = member$desc[3]
                             ),
                userListItem(src = member$pict[4],
                             user_name = member$Nama[4],
                             description = member$desc[4]
                             ),
                userListItem(src = member$pict[5],
                             user_name = member$Nama[5],
                             description = member$desc[5]
                             ),
                userListItem(src = member$pict[6],
                             user_name = member$Nama[6],
                             description = member$desc[6]
                             ),
                userListItem(src = member$pict[7],
                             user_name = member$Nama[7],
                             description = member$desc[7]
                             )
                )
            )
        }else{
            box(title = "",status = "info",width = 12,
                div(h2("Active Mentor"),align = "center"), tags$br(),
                div(h3(paste(member$start_date[1],"-",member$end_date[1])),align = "center"),
                userList(
                userListItem(src = member$pict[1],
                             user_name = member$Nama[1],
                             description = member$desc[1]
                ),
                userListItem(src = member$pict[2],
                             user_name = member$Nama[2],
                             description = member$desc[2]
                ),
                userListItem(src = member$pict[3],
                             user_name = member$Nama[3],
                             description = member$desc[3]
                ),
                userListItem(src = member$pict[4],
                             user_name = member$Nama[4],
                             description = member$desc[4]
                ),
                userListItem(src = member$pict[5],
                             user_name = member$Nama[5],
                             description = member$desc[5]
                ),
                userListItem(src = member$pict[6],
                             user_name = member$Nama[6],
                             description = member$desc[6])
                )
            )
        }
        
    })
    
    
    # output$periodic <- renderUI({
    #     text <- paste("Periode: " ,prd,"<br/>",datep[1]," - ",datep[2],sep = "")
    #     div(h4(HTML(text)))
    #     
    # })
    # 
    # output$sum_1 <- renderUI({
    #     last <- piket_sum %>% 
    #         filter(Timestamp == max(Timestamp))
    #     period <- last$Periode[1]
    #     temp <- piket_sum %>% 
    #         filter(Periode == period) %>% 
    #         mutate(print = paste("-",Summary,"<br/>"))
    #     summ <- temp$print
    #     p(HTML(paste(summ,collapse = " ")))
    # })
    
    output$plot1 <- renderPlotly({
        men_df <- mentoring %>% 
            filter(!Status %in% c("Missed","Canceled")) %>%
            mutate(Class = as.factor(Class),
                   date_only = date(`Date Created`)) %>% 
            filter(date_only >= input$filterer[1] & date_only <= input$filterer[2]) %>% 
            group_by(Class) %>% 
            summarise(n = n())
        
        men_df %>% plot_ly(labels= ~Class, values = ~n, textinfo = "label+percent",
                           insidetextorientation='radial',textposition="inside") %>% 
            add_pie(hole=0.6) %>% 
            layout(showlegend = F,
                   xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                   yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE)) %>% 
            config(displayModeBar = F)
    })
    
    output$plot2 <- renderPlotly({
        plot2x <- mentoring %>% 
            filter(!Status %in% c("Missed","Canceled")) %>%
            mutate(date_only = date(`Date Created`)) %>% 
            filter(date_only >= input$filterer[1] & date_only <= input$filterer[2]) %>% 
            group_by(Mentor, Team) %>% 
            summarise(n = n()) %>%
            na.omit() %>% 
            arrange(-n) %>% 
            mutate(text = paste(Mentor,"\n Team:",Team,"\n Freq:",n)) %>% 
            ggplot(aes(x = reorder(Mentor,n), y = n, text = text)) +
            geom_col(aes(fill = Team),width = 0.8) +
            theme_minimal() +
            labs(x = "", y = "Frequency")
        
        ggplotly(plot2x,tooltip = "text") %>% 
            config(displayModeBar = F)
    })
    
    output$plot3 <- renderPlotly({
        plot3x <- mentoring %>% 
            filter(!Status %in% c("Missed","Canceled")) %>%
            mutate(date_only = date(`Date Created`)) %>% 
            filter(date_only >= input$filterer[1] & date_only <= input$filterer[2]) %>%
            group_by(Material) %>%
            summarise(n = n()) %>% 
            mutate(text = paste(Material,": ",n,sep = "")) %>% 
            arrange(-n) %>% head(6) %>% 
            ggplot(aes(x = n, y = reorder(Material,n),text = text)) +
            geom_col(aes(fill = n),show.legend = F) +
            theme_minimal() +
            labs(x = "Frequency", y = "")
        
        ggplotly(plot3x,tooltip = "text") %>% 
            config(displayModeBar = F)
    })
    
    output$dt1 <- renderDataTable({
        table1 <- mentoring %>% 
            filter(!Status %in% c("Missed","Canceled")) %>%
            mutate(date_only = date(`Date Created`)) %>% 
            filter(date_only >= input$filterer[1] & date_only <= input$filterer[2]) %>%
            group_by(Name) %>% 
            mutate(last_request = max(`Date Created`)) %>% 
            group_by(Name,last_request,Email) %>% 
            summarise(Frequency = n()) %>% 
            arrange(-Frequency)
        
        datatable(data = table1,rownames = F,
                  options = list(scrollX = T,"pageLength" = 5,lengthChange = FALSE))
    })
    
    output$plot4 <- renderPlotly({
        plot4x <- mentoring %>% 
            filter(!Status %in% c("Missed","Canceled")) %>%
            # filter(Period == input$sel_4) %>%
            mutate(dayy = date(`Date Created`)) %>% 
            filter(dayy >= input$date_4[1] & dayy <= input$date_4[2]) %>% 
            group_by(dayy) %>% 
            summarise(n = n()) %>% 
            padr::pad() %>% 
            mutate(n = ifelse(is.na(n),0,n),
                   text = paste(dayy,"\n Mentoring:",n)) %>% 
            arrange(dayy) %>% 
            ggplot(aes(x = dayy, y = n,text=text)) +
            geom_point(aes(col = n)) +
            geom_line(aes(group = 1)) +
            theme_minimal()+
            labs(x = "Date", y = "") +
            theme(legend.position = "none")
        
        ggplotly(plot4x,tooltip = "text")
    })
    
    output$plot5 <- renderPlotly({
        
        timelinex <- if(input$team_sch == "A"){
            timelinex <- timeline %>% 
                filter(Nama %in% tim_a) %>% 
                filter(as.character(Date) == input$date_sch) %>% 
                mutate(Nama = factor(Nama,levels = tim_a))
        }else{
            timelinex <- timeline %>% 
                filter(Nama %in% tim_b) %>% 
                filter(as.character(Date) == input$date_sch) %>% 
                mutate(Nama = factor(Nama,levels = tim_b))
        }
        
        plot5x <- timelinex %>% 
            ggplot(aes(x = Nama, y = Waktu,text = Notes)) +
            geom_point(aes(color = Nama),show.legend = F) + 
            scale_y_continuous(limits = c(07,22),
                               breaks = 7:22,
                               labels = paste(7:22,":00",sep = "")) +
            scale_x_discrete(breaks = levels(timelinex$Nama),
                             labels = levels(timelinex$Nama),
                             drop = F) +
            theme_minimal()
        
        ggplotly(plot5x,tooltip = "text")%>% 
            config(displayModeBar = F)
    })
    
    
    
}
