library(shiny)
library(DT)
library(tidyverse)
library(tidytext)
library(jsonlite)
library(httr)
library(wordcloud)


# get job descriptions
getJobDesc <- function(jobId){
  url <- str_glue("https://jobs.github.com/positions/{id}.json", id=jobId)
  r <- GET(url)
  json <- content(r, as = "text")
  dat <- fromJSON(json)
  dat$description
}

# search for jobs
searchJobs <- function(description=NA, location=NA, full_time=NA){
  query = list()
  if(!is.na(description)){
    query$description <- description
  }
  if(!is.na(location)){
    query$location <- location
  }
  if(!is.na(full_time)){
    query$full_time <- full_time
  }
  r <- GET(
    "https://jobs.github.com/positions.json",
    query = query
  )
  json <- content(r, as = "text")
  dat <- fromJSON(json)
  dat
}


drawWordCloud <- function(jobs){
  text <- jobs$description
  text <- str_replace_all(text, "<.*>", " ")
  text <- str_replace_all(text, "\n", " ")
  jobs_df <- tibble(id=1:length(text), text=text)
  job_tokens <- jobs_df %>% 
    unnest_tokens(word, text) %>% 
    anti_join(stop_words)
  job_tokens %>% group_by(word) %>% summarise(n=n()) %>% 
    with(wordcloud(
      word, n, min.freq = 2, max.words = 100, random.order = FALSE,
      colors = brewer.pal(8, "Dark2"))) 
}

top10Word <- function(jobs){
  text <- jobs$description
  text <- str_replace_all(text, "<.*>", " ")
  text <- str_replace_all(text, "\n", " ")
  jobs_df <- tibble(id=1:length(text), text=text)
  job_tokens <- jobs_df %>% 
    unnest_tokens(word, text) %>% 
    anti_join(stop_words)
  job_tokens %>% group_by(word) %>% summarise(n=n()) %>% 
    arrange(desc(n)) %>% head(10) %>%
    ggplot(aes(x=reorder(word, n), y=n, fill=word)) + geom_bar(stat="identity") + coord_flip() +
    labs(x="Word", y="Count", title="Top 10 tops in descriptions")
}



getTFIDF <- function(jobs){
  text <- jobs$description
  text <- str_replace_all(text, "<.*>", " ")
  text <- str_replace_all(text, "\n", " ")
  jobs_df <- tibble(id=1:length(text), text=text)
  jobs_df %>% 
    unnest_tokens(word, text) %>% 
    anti_join(stop_words) %>% group_by(id, word) %>% summarise(n=n()) %>% filter(n>1) %>%
    bind_tf_idf(word, id, n) %>%
    top_n(3, tf_idf) %>%
    select(id, word, n, tf_idf) %>% ungroup()
}



ui <- fluidPage(
  titlePanel("Github jobs analysis"),
    sidebarLayout(
    sidebarPanel(
        textInput("desc", "Description", placeholder = "Python"),
        textInput("location", "Location", placeholder = "New York"),
        checkboxInput("fulltime", "FullTime", value=1),
        uiOutput("ui")
      
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Job Description", htmlOutput("desc")),
        tabPanel("Search Result", DTOutput("search_result")),
        tabPanel("Text Mining", 
                 h2("Wordcloud of Job Descriptions"),
                 plotOutput("wc"),
                 hr(),
                 h2("Top 10 words"),
                 plotOutput("top")
                 ),
        tabPanel("TF-IDF", DTOutput("tf"))
      )
        
    )
  )
)

server <- function(input, output) {
  data <- reactive({
    dat <- searchJobs(input$desc, input$location, input$fulltime)
  })
  
  output$ui <- renderUI({
    df <- data()
    selectInput("jobs", "Select a job", choices = df$title)
  })
  
  output$desc <- renderUI({
    req(input$jobs)
    df <- data()
    job_id <- df$id[df$title==input$jobs]
    desc <- getJobDesc(job_id)
    HTML(desc)
  })
  
  output$search_result <- renderDataTable({
    dat <- data()
    if(length(dat)==0){
      data.frame(text="No Result")
    }else{
      dat %>% select(-description, -how_to_apply, -company_logo)
    }
  })
  output$wc <- renderPlot({
    dat <- data()
    if(length(dat)==0){
    }else{
      drawWordCloud(dat)
    }
  })
  
  output$top <- renderPlot({
    dat <- data()
    if(length(dat)==0){
    }else{
      top10Word(dat)
    }
  })
  
  output$tf <- renderDataTable({
    dat <- data()
    if(length(dat)==0){
      data.frame(text="No Result")
    }else{
      getTFIDF(dat)
    }
  })
  
}

shinyApp(ui = ui, server = server)
