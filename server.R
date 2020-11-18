#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(readxl)
library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)
library(treemap)

# Define server logic required to draw a histogram
shinyServer(function(input, output,session) {
values <- reactiveValues(data=NULL)

### data input
    observeEvent(input$data, {
        # load data
        values$data <- as.data.frame(read_xlsx(input$data$datapath))
        # read datetime
        values$data <- values$data %>%
            mutate(date = dmy(date),
                   time = hm(time),
                   saved = dmy_hm(saved))
        
        
        #### COUNT SUMMARY
        daysn <- length(unique(values$data$date))
        period <- paste(min(unique(values$data$date)), " - ", max(unique(values$data$date)), sep = "")
        totaln <- nrow(values$data)
        # count total tones
        tone <- as.data.frame(table(values$data$tone))
        colnames(tone) <- c("tone", "freq")
        tone <- mutate(tone, prop = freq/totaln*100)  %>% 
            arrange(desc(tone)) %>%
            mutate(ypos = cumsum(prop)- 0.5*prop ) %>%
            mutate(label = paste(freq, " / ", round(prop,0), "%", sep = ""))
        # total tones plot
        output$total_tone <- renderPlot({
            ggplot(tone, aes(x="", y=prop, fill=tone)) +
                geom_bar(stat="identity", width=1, color="white") +
                coord_polar("y", start=0) +
                theme_minimal() + 
                theme_void() + 
                scale_fill_manual("Tonal structure",
                                  values = c("#383632",
                                             "#cc0808",
                                             "#bbc9bc",
                                             "#0a8f13")) +
                geom_text(aes(y = ypos, label = label), color = "white", size=6)
        }) 
        # show summary
        output$summary <- renderUI({
            tagList(
                h3(paste("Report for ", period, " (", daysn, " days)", sep = "")),
                h4(paste("Total entries: ", totaln, sep = "")),
                plotOutput("total_tone",
                           width = "600px",
                           height = "400px")
            )
            
        })
    }) # observe dataInput end

### show period stats
    observeEvent(input$daystat, {
        # count days stats
        days <- split(data, data$date)
        days_stat <- lapply(days, function(y) {
            day <- unique(y$date)
            number <- nrow(y)
            tonal_table <- table(y$tone)
            if (length(tonal_table) < 4) {
                tones <- c("Не определено", "Негативная", "Нейтральная", "Позитивная")
                lack <- tones[which(!(tones %in% names(tonal_table)))] 
                zeros <- rep(0, times = length(lack))            
                names(zeros) <- lack
                tone <- append(tonal_table, zeros)
                tone <- as.data.frame(rbind(tone))
                colnames(tone) <- names(tone)
                tone <- select(tone, "Не определено", "Негативная", "Нейтральная", "Позитивная") # table
            } else {
                tone <- as.data.frame(rbind(tonal_table)) # table
                colnames(tone) <- names(tonal_table)
            }
            row <- cbind(day, number, tone)
        })
        days_stat <- bind_rows(days_stat) 
        days_tonal <- days_stat %>%
            select(-number) %>%
            gather(key = "tone", value = "number", -day) 
        
        # plot days number
        output$daysnum <- renderPlot({
            ggplot(days_stat, aes(x = day, y = number)) +
                geom_bar(stat = "identity", fill='#A5DAFA', color="#A5DAFA") +
                theme_minimal() +
                coord_flip() +
                labs(
                    title = "",
                    y = "Entries number",
                    x = ""
                ) +
                scale_x_date(breaks = days_stat$day,
                             labels = days_stat$day
                ) +
                scale_y_continuous(limits = c(0, max(days_stat$number) + max(days_stat$number)*0.15),
                                   labels = scales::number) +
                geom_text(aes(x = day, y = number + (max(number, na.rm = TRUE)*0.05),label = ifelse(is.na(number),"",number)),
                          vjust = 0.5, hjust = 0.5,
                          color = "#5B6E79",
                          size = 4) +
                theme(axis.text.x = element_text(vjust = 0.5),
                      axis.text.y = element_text(vjust = 0.5, hjust = 0.5),
                      panel.grid.major.x = element_line(color = "#d3d3d3", size = 0.1, linetype = "dashed"),
                      panel.grid.major.y = element_blank(),
                      panel.grid.minor = element_blank(),
                      text = element_text(color = "#5B6E79",
                                          size = 12))
        })
        
        # plot days tonal
        output$daystone <- renderPlot({
            ggplot(days_tonal, aes(x = day, y = number, fill = tone)) +
                geom_bar(stat = "identity") +
                theme_minimal() +
                labs(
                    title = "",
                    y = "Количество отзывов",
                    x = "",
                    fill = ""
                ) +
                scale_x_date(breaks = days_tonal$day,
                             labels = days_tonal$day
                ) +
                scale_y_continuous(labels = scales::number) +
                scale_fill_manual("Tonal structure",
                                  values = c("#383632",
                                             "#cc0808",
                                             "#bbc9bc",
                                             "#0a8f13")) +
                geom_text(aes(label=ifelse(!is.na(number) & number == 0, "", number)),
                          position = position_stack(0.5),
                          vjust = 0.5, hjust = 0.5,
                          color = "white",
                          size = 4) +
                theme(axis.text.x = element_text(vjust = 0.5),
                      axis.text.y = element_text(vjust = 0.5, hjust = 0.5),
                      panel.grid.major.x = element_line(color = "#d3d3d3", size = 0.1, linetype = "dashed"),
                      panel.grid.major.y = element_blank(),
                      panel.grid.minor = element_blank(),
                      text = element_text(color = "#5B6E79",
                                          size = 12),
                      legend.position = "top") +
                coord_flip()
        })
        
        # show days
        output$days <- renderUI({
            tagList(
                plotOutput("daysnum",
                           width = "480px",
                           height = "720px"),
                plotOutput("daystone",
                           width = "480px",
                           height = "720px")
            )
        })
        
        # show themes
        output$themes <- renderUI({
            data_long <- gather(values$data, key = "theme", value = "theme_value", -colnames(values$data)[1:37]) 
            data_long <- data_long[!is.na(data_long$theme_value),]   
            
            themes_list <- split(data_long, data_long$theme_value) 
            themes_t <- lapply(themes_list, function(x) {
                theme <- unique(x$theme_value)
                n <- nrow(x)
                tonal_table <- table(x$tone)
                if (length(tonal_table) < 4) {
                    tones <- c("Не определено", "Негативная", "Нейтральная", "Позитивная")
                    lack <- tones[which(!(tones %in% names(tonal_table)))] 
                    zeros <- rep(0, times = length(lack))            
                    names(zeros) <- lack
                    tone <- append(tonal_table, zeros)
                    tone <- as.data.frame(rbind(tone))
                    colnames(tone) <- names(tone)
                    tone <- select(tone, "Не определено", "Негативная", "Нейтральная", "Позитивная") # table
                } else {
                    tone <- as.data.frame(rbind(tonal_table)) # table
                    colnames(tone) <- names(tonal_table)
                }
                row <- cbind.data.frame(theme,n, tone, stringsAsFactors = FALSE)
                row
            })
            themes_table <- bind_rows(themes_t) %>%
                arrange(n) %>%
                mutate(position = 1:length(themes_t)) %>%
                gather(key = "tone", value = "number", -c(theme, n, position)) 
            
            #plot themes
            output$themes_n <- renderPlot({
                ggplot(themes_table, aes(x = position, y = number, fill = tone)) +
                    geom_bar(stat = "identity") +
                    theme_minimal() +
                    labs(
                        title = "",
                        y = "Количество отзывов",
                        x = "",
                        fill = ""
                    ) +
                    scale_x_discrete(breaks = themes_table$position,
                                     limits = themes_table$position,
                                     labels = themes_table$theme) +
                    scale_y_continuous(labels = scales::number) +
                    scale_fill_manual("Tonal structure",
                                      values = c("#383632",
                                                 "#cc0808",
                                                 "#bbc9bc",
                                                 "#0a8f13")) +
                    geom_text(aes(label=ifelse(!is.na(number) & number == 0, "", number)),
                              position = position_stack(0.5),
                              vjust = 0.5, hjust = 0.5,
                              color = "white",
                              size = 4) +
                    geom_text(aes(y = n*1.075, label=n),
                              color = "black",
                              size = 4) +
                    theme(axis.text.x = element_text(vjust = 0.5),
                          axis.text.y = element_text(vjust = 0.5, hjust = 0.5),
                          panel.grid.major.x = element_line(color = "#d3d3d3", size = 0.1, linetype = "dashed"),
                          panel.grid.major.y = element_blank(),
                          panel.grid.minor = element_blank(),
                          text = element_text(color = "#5B6E79",
                                              size = 12),
                          legend.position = "top") +
                    coord_flip()
            })
            
            # count treemap
            th_gr <- lapply(themes_list, function(x) {
                y <- split(x, x$source)
                ts <-  lapply(y, function(z) {
                    theme <- unique(z$theme_value)
                    ground <- unique(z$source)
                    n <- nrow(z)
                    views <- sum(z$view[!is.na(z$view)])
                    row <- cbind.data.frame(theme, ground, n, views, stringsAsFactors = FALSE)
                    row
                })
                ts <- bind_rows(ts) 
            })
            th_gr <- bind_rows(th_gr)
            
            
            th_grC <- filter(th_gr, n > max(th_gr$n)*0.05)
            
            # plot groundmap
            output$groundmap <- renderPlot({
                treemap(th_grC,
                        index = c("ground", "theme"),
                        vSize = "n",
                        type = "index")
            })
 
            
            #### THEMES UI
            tagList(
                plotOutput("themes_n"),
                h3("Themes by entries"),
                plotOutput("groundmap")
            )
        })
        
        #show sources
        output$sources <- renderUI({
            data_long <- gather(values$data, key = "theme", value = "theme_value", -colnames(values$data)[1:37]) 
            data_long <- data_long[!is.na(data_long$theme_value),]   
            
            ist <- split(data_long, data_long$source_name)
            ist_count <- bind_rows(ist_count) %>%
                arrange(desc(n))
            ist_top <- ist_count[1:15,]
            ist_top <- ist_top %>%
                arrange(n) %>% 
                mutate(position = 1:15) %>% 
                gather(key = "tone", value = "number", -c(source, n, position)) %>% arrange(desc(n))
            
            # show summary:
            output$scount <- renderText({
                paste("Total", nrow(ist_count), "sources for period", sep = " ")
            })
            #plot sources
            output$sourcetop <- renderPlot({
                ggplot(ist_top, aes(x = position, y = number, fill = tone)) +
                    geom_bar(stat = "identity") +
                    theme_minimal() +
                    labs(
                        title = "",
                        y = "Количество отзывов",
                        x = "",
                        fill = ""
                    ) +
                    scale_x_discrete(breaks = ist_top$position, 
                                     limits = ist_top$position, 
                                     labels = ist_top$source)+
                    scale_y_continuous(labels = scales::number) +
                    scale_fill_manual("Tonal structure",
                                      values = c("#383632",
                                                 "#cc0808",
                                                 "#bbc9bc",
                                                 "#0a8f13")) +
                    geom_text(aes(label=ifelse(!is.na(number) & number == 0, "", number)),
                              position = position_stack(0.5),
                              vjust = 0.5, hjust = 0.5,
                              color = "white",
                              size = 4) +
                    geom_text(aes(y = n*1.15, label=n),
                              color = "black",
                              size = 4) +
                    theme(axis.text.x = element_text(vjust = 0.5),
                          axis.text.y = element_text(vjust = 0.5, hjust = 0.5),
                          panel.grid.major.x = element_line(color = "#d3d3d3", size = 0.1, linetype = "dashed"),
                          panel.grid.major.y = element_blank(),
                          panel.grid.minor = element_blank(),
                          text = element_text(color = "#5B6E79",
                                              size = 12),
                          legend.position = "top") +
                    coord_flip()
            })
            
            # count top1 themes
            top1ist <- data_long[data_long$source_name == ist_top[1, "source"] & !is.na(data_long$source_name),]
            
            ist_t <- split(top1ist, top1ist$theme_value) 
            themes_i_t <- lapply(ist_t, function(x) {
                theme <- unique(x$theme_value)
                n <- nrow(x)
                tonal_table <- table(x$tone)
                if (length(tonal_table) < 4) {
                    tones <- c("Не определено", "Негативная", "Нейтральная", "Позитивная")
                    lack <- tones[which(!(tones %in% names(tonal_table)))] 
                    zeros <- rep(0, times = length(lack))            
                    names(zeros) <- lack
                    tone <- append(tonal_table, zeros)
                    tone <- as.data.frame(rbind(tone))
                    colnames(tone) <- names(tone)
                    tone <- select(tone, "Не определено", "Негативная", "Нейтральная", "Позитивная") # table
                } else {
                    tone <- as.data.frame(rbind(tonal_table)) # table
                    colnames(tone) <- names(tonal_table)
                }
                row <- cbind.data.frame(theme,n, tone, stringsAsFactors = FALSE)
                row
            })
            themes_i_table <- bind_rows(themes_i_t) %>%
                arrange(n) %>%
                mutate(position = 1:length(themes_i_t)) %>%
                gather(key = "tone", value = "number", -c(theme, n, position)) 
            
            # plot top1 themes
            output$top1themes <- renderPlot({
                ggplot(themes_i_table, aes(x = position, y = number, fill = tone)) +
                    geom_bar(stat = "identity") +
                    theme_minimal() +
                    labs(
                        title = "",
                        y = "Количество отзывов",
                        x = "",
                        fill = ""
                    ) +
                    scale_x_discrete(breaks = themes_i_table$position,
                                     limits = themes_i_table$position,
                                     labels = themes_i_table$theme)+
                    scale_y_continuous(labels = scales::number) +
                    scale_fill_manual("Tonal structure",
                                      values = c("#383632",
                                                 "#cc0808",
                                                 "#bbc9bc",
                                                 "#0a8f13")) +
                    geom_text(aes(label=ifelse(!is.na(number) & number == 0, "", number)),
                              position = position_stack(0.5),
                              vjust = 0.5, hjust = 0.5,
                              color = "white",
                              size = 4) +
                    geom_text(aes(y = n*1.075, label=n),
                              color = "black",
                              size = 4) +
                    theme(axis.text.x = element_text(vjust = 0.5),
                          axis.text.y = element_text(vjust = 0.5, hjust = 0.5),
                          panel.grid.major.x = element_line(color = "#d3d3d3", size = 0.1, linetype = "dashed"),
                          panel.grid.major.y = element_blank(),
                          panel.grid.minor = element_blank(),
                          text = element_text(color = "#5B6E79",
                                              size = 12),
                          legend.position = "top") +
                    coord_flip()
            })
            
            
            # count treemap
            tree <- split(values$data, values$data$source_name)
            tree <- lapply(tree, function(x) {
                source <- unique(x$source_name)
                ground <- unique(x$source)
                n <- nrow(x)
                views <- sum(x$view[!is.na(x$view)])
                row <- cbind.data.frame(ground, source, n, views, stringsAsFactors = FALSE)
                row
            })
            tree <- bind_rows(tree) %>% filter(n > 1)
            
            # render treemap
            output$entmap <- renderPlot({
                treemap(tree,
                        index = c("ground", "source"),
                        vSize = "n",
                        type = "index")
            })
            
            # views treemap
            treeV <- filter(tree, views > 0)

            # render views treemap
            output$vmap <- renderPlot({
                treemap(treeV,
                        index = c("ground", "source"),
                        vSize = "views",
                        type = "index")
            })
            #### SOURCE UI
            tagList(
                textOutput("scount"),
                plotOutput("sourcetop"),
                h3("Top1 source themes"),
                plotOutput('top1themes'),
                h3("Source map by entries"),
                plotOutput("entmap"),
                h3("Source map by views"),
                plotOutput("vmap")
            )
        })
        
        
        
        
        
        

        
        
        
        
        
        
    }) # statcount end
    
    
}) # server end
