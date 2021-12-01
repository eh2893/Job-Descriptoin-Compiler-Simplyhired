library(rvest)
library(httr)
library(XML)

library(shiny)
library(DT)
library(tidyverse)
library(wordcloud2)
library(tidytext)

server <- function(input, output, session) {
  df <- reactive({input$act
    df <- isolate({
      max_pages <- input$mp
      main_url <- 'https://www.simplyhired.com'
      df <- data.frame()
      status <- TRUE
      i = 1
      while(status){
        if(i == 1){
          response <- GET(url = main_url, path = "search", 
                          query = list(q = input$q, l = input$l))
        }else{
          response <- GET(url = next_url)
        }
        i <- i + 1
        response %>% 
          read_html(encoding = 'UTF-8') %>% 
          html_elements(xpath = '//ul[@id = "job-list"]/li') %>%
          as.character -> lis
        df_temp <- do.call(rbind, Filter(NROW, lapply(lis, FUN = function(li){
          position <- li %>% read_html(encoding = 'UTF-8') %>% 
            html_node(xpath = '//h3[@class = "jobposting-title"]/a') %>%
            html_text()
          
          url <- li %>% read_html(encoding = 'UTF-8') %>% 
            html_node(xpath = '//h3[@class = "jobposting-title"]/a') %>%
            html_attr(name = 'href')
          
          company <- li %>% read_html(encoding = 'UTF-8') %>% 
            html_node(xpath = '//div/span[@class = "JobPosting-labelWithIcon jobposting-company"]') %>%
            html_text()
          
          location <- li %>% read_html(encoding = 'UTF-8') %>% 
            html_node(xpath = '//div//span[@class = "jobposting-location"]') %>%
            html_text()
          
          description <- li %>% read_html(encoding = 'UTF-8') %>% 
            html_node(xpath = '//div[@class = "SerpJob-snippetContainer"]/p') %>%
            html_text()
          
          salary <- li %>% read_html(encoding = 'UTF-8') %>% 
            html_node(xpath = '//div[@class = "jobposting-salary SerpJob-salary"]') %>%
            html_text()
          
          rating <- li %>% read_html(encoding = 'UTF-8') %>% 
            html_node(xpath = '//span[@class = "CompanyRatings-serp"]') %>% 
            html_text()
          data.frame(position = position, company = company, location = location, 
                     description = description, salary = salary, rating = rating, 
                     url = url) 
        })))
        
        response %>% 
          read_html(encoding = 'UTF-8') %>% 
          html_elements(xpath = '//li[@class = "next-pagination"]/a') %>%
          html_attr('href') -> next_url
        
        next_url <- paste0(main_url, next_url)
        status <- ifelse(next_url == main_url, FALSE, TRUE)
        showNotification(next_url, duration = 2, closeButton = F, type = 'message')
        df <- rbind(df, df_temp)
        if(!is.null(max_pages)&i>max_pages) break
      }
      df %>% as.data.frame %>% 
        mutate(salary = ifelse(is.na(salary), 'Unknown', salary),
               rating = as.numeric(rating),
               url = paste0(main_url, url),
               url = paste0("<a href='", url,"'>", url, "</a>"))
    })
    as.data.frame(df)
  })
  
  output$p1 <- renderPlot({
    ggplot(df(), aes(x = rating, y = ..density..)) +
      geom_histogram(fill = 'tomato') +
      geom_density(col = 'tomato') +
      theme_bw()
  })
  
  output$p2 <- renderWordcloud2({
    df() %>% unnest_tokens(word, description) %>%
      anti_join(stop_words) %>%
      group_by(word) %>% count %>% 
      mutate(n = log(n)) %>%
      wordcloud2()
  })
  
  output$dt1 <- renderDT({
    df()
  }, escape = FALSE)
  
  output$downloadData <- downloadHandler(
    filename = function(){
      if(nchar(input$l)<1){
        aa <- paste('/srv/shiny-server/apps/files/', input$q, ".csv", sep="")
      }else{
        aa <- paste('/srv/shiny-server/apps/files/', input$q, '_', input$l, ".csv", sep="")
      }
      return(aa)
    },
    content = function(file) {
      write.csv(df(), file = file, row.names = FALSE)
    }, contentType = "text/csv")

}



