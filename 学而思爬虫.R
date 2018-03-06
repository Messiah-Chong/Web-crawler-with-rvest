#========================================
# Script: Obtain and manipulate lesson information in xue-er-si.
# Author: Xuanquan Zhuang
# Date: 6/3/2018
# Description: The "rvest" package is a powerful and
# convenient tool for systematically browsing the web
# and obtaining data with R. This script is an example
# about how to obtain data from a "simple" static
# page (without any javascripts methods) and manipulate
# it. Here uses a website "http://sbj.speiyou.com/shouye/"
# as an example.
#========================================
library(rvest)

# Here we search lesson information in Guangzhou. First,
# we input the search page for lessons in Guangzhou. Then
# we get the nodes for pages, grades, and use them to obtain
# urls we need.
search.url <- "http://sgz.speiyou.com/search/index/grade:-7/subject:/level:bx/term:/gtype:time"

search.page <- read_html(search.url) # analyze the html

search.node <- search.page %>% html_nodes("dl")
grade.node <- search.node[[10]] # node for grades

# extract names of available grades
grade.name <- grade.node %>%
  html_nodes("dd") %>%
  html_nodes("ul") %>%
  html_nodes("li") %>%
  html_nodes("a") %>%
  html_text()

grade.url <- grade.node %>%
  html_nodes("dd") %>%
  html_nodes("ul") %>%
  html_nodes("li") %>%
  html_nodes("a")
grade.url <- sapply(1:length(grade.url),
                    function(x) html_attr(grade.url[[x]],"href"))

grade.url <- paste("http://sgz.speiyou.com",
                   grade.url,
                   sep = "") # complete the url

grade.table <- data.frame(grade = grade.name,
                          url = grade.url,
                          stringsAsFactors = FALSE) # store grades and corresponding url

# function for obtaining informations we need in one page
get.info <- function(url){
  read.page <- read_html(url) # analyze the html
  
  class.name <- read.page %>%
    html_nodes("div.s-r-list-info") %>%
    html_nodes("h3") %>%
    html_text() # lessons' names
  
  class.teacher <- read.page %>%
    html_nodes("div.s-r-list-photo") %>%
    html_nodes("p") %>%
    html_text() # teachers' names
  
  class.price <- read.page %>%
    html_nodes("div.price") %>%
    html_text() # prices
  
  class.details <- read.page %>%
    html_nodes("div.s-r-list-info") %>%
    html_nodes("p") %>%
    html_nodes("span") %>%
    html_text() # lessons' info
  
  item.num <- length(class.details)/4 # count number of lessons
  
  class.subject <- class.details[seq(1,(item.num-1)*4+1,by=4)] # subject of lessons
  class.grade <- class.details[seq(2,(item.num-1)*4+2,by=4)] # grade of lessons
  class.period <- class.details[seq(3,(item.num-1)*4+3,by=4)] # start time of lessons
  class.time <- class.details[seq(4,(item.num-1)*4+4,by=4)] # time of lessons
  
  class.place <- read.page %>%
    html_nodes("div.s-r-list-info") %>%
    html_nodes("p") %>%
    html_text()
  
  class.place <- class.place[seq(3,(item.num-1)*3+3,by=3)] # place for lessons
  
  class.status <- read.page %>% html_nodes("p.mtop20") %>% html_text() # remaining seats
  
  class.info <- data.frame(teacher = class.teacher,
                           lesson = class.name,
                           price = class.price,
                           subject = class.subject,
                           grade = class.grade,
                           period = class.period,
                           time = class.time,
                           place = class.place,
                           status = class.status,
                           stringsAsFactors = FALSE) # manipulate lesson information
  
  return(class.info)
}

# function for obtaining all informations we need in a subject
grade.info <- function(grade.url){
  target.page <- read_html(grade.url)
  
  page.num <- target.page %>% html_nodes("div.mtop40") %>% html_nodes("a") %>% html_text() # names of pages
  page.end <- target.page %>% html_nodes("div.mtop40") %>% html_nodes("a")
  
  # extract urls for each page
  page.end <- page.end[length(page.num)]
  page.end.url <- as.character(page.end)
  page.end.url.split <- strsplit(page.end.url,"\"")
  page.end.url <- page.end.url.split[[1]][2]
  
  page.end.num <- strsplit(page.end.url,"curpage:")
  page.end.num <- page.end.num[[1]][2] # get the number of pages
  page.end.num <- as.numeric(page.end.num)
  
  page.all.url <- strsplit(page.end.url,"curpage:")
  page.all.url <- page.all.url[[1]][1] # common part of each url
  page.all.url <- paste(page.all.url,"curpage:",sep = "")
  page.all.url <- paste(page.all.url,2:page.end.num,sep = "")
  page.all.url <- paste("http://sgz.speiyou.com",page.all.url,sep = "") # complete urls
  page.all.url <- as.vector(page.all.url)
  page.all.url <- c(grade.url,page.all.url)
  
  grade.table.info <- data.frame(stringsAsFactors = FALSE)
  for(i in 1:length(page.all.url)){
    grade.table.info <- rbind(grade.table.info,get.info(page.all.url[i]))
  }
  return(grade.table.info)
}

# get all informations of lessons in Guangzhou
xes.table <- data.frame(stringsAsFactors = FALSE)

for(i in 1: nrow(grade.table)){
  xes.table <- rbind(xes.table,grade.info(grade.table[i,2]))
}
rm(i)

# save data as a csv table
write.csv(xes.table,"学而思广州.csv")

# save data as an excel table
library(xlsx)

write.xlsx2(xes.table,"学而思广州.xlsx","学而思广州")

# save data into mysql database
library(RMySQL)

con <- dbConnect(MySQL(),dbname = "mysql",user = "root",password = "password")
