#========================================
# Script: Obtain news from the Verge.
# Author: Xuanquan Zhuang
# Date: 11/3/2018
# Description: The "rvest" package is a powerful and
# convenient tool for systematically browsing the web
# and obtaining data with R. This script is an example
# about how to scrape data from the web using parallel
# computation. We can do this using "parallel" package.
#========================================

# function for get information in a single page and manipulate it
get.page <- function(n){
  library(rvest)
  library(stringr)
  library(parallel)
  
  page <- read_html(paste("https://www.theverge.com/tech/archives/",n,sep = "")) # url for the i-th page
  
  title.node <- page %>%
    html_nodes("h2.c-entry-box--compact__title") %>%
    html_nodes("a")
  
  info <- page %>%
    html_nodes("div.c-byline") # nodes contain authors, date, comments
  
  is.complete <- sapply(info,function(x){
    length(html_nodes(x,"span.c-byline__item")) == 3
    }) # drop some news which contain incomplete information
  
  title <- title.node[is.complete] %>%
    html_text()
  
  info <- info[is.complete] %>%
    html_nodes("span.c-byline__item") %>%
    html_text() %>%
    str_remove_all("\\n") %>%
    str_remove_all("^\\s+") %>%
    str_remove_all("\\s+$") %>%
    matrix(byrow = TRUE,nrow = sum(is.complete))
  
  author <- info[,1]
  date <- info[,2]
  comment <- str_extract(info[,3],"^\\d+")
  
  link.url <- title.node[is.complete] %>%
    html_attr("href")
  
  content <- sapply(link.url, function(x){
    x %>% read_html() %>%
      html_nodes("div.c-entry-content") %>%
      html_text() %>%
      str_remove_all("^\\n")
  })
  
  return(data.frame(title = title,
                    author = author,
                    date = date,
                    comment = comment,
                    link = link.url,
                    content = content,
                    stringsAsFactors = FALSE))
}

the.verge <- data.frame(stringsAsFactors = FALSE)
system.time(
  {
    for(i in 1:15){
      the.verge <- rbind(the.verge,get.page(i))
    }
  }
) # it takes a long time to browse all news on that 15 pages

# Getting all the news content needs to open many links which
# takes lots of time. Using parallel computation may save some time.
system.time(
  {
    pages <- 1:15
    cl <- makeCluster(4)
    the.verge <- parLapply(cl,pages,get.page)
    the.verge <- do.call("rbind",the.verge)
  }
) # it takes much shorter time than the operation above and
  # it also doesn't cost too much memory, which will allow you
  # to watch a movie or play steam while the program is running
