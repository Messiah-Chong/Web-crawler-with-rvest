#========================================
# Script: Downloading images from the web using rvest.
# Author: Xuanquan Zhuang
# Date: 6/3/2018
# Description: The "rvest" package is a powerful and
# convenient tool for systematically browsing the web
# and obtaining data with R. This script is an example
# about how to download images from a "simple" static
# page (without any javascripts methods). Here uses a
# website "http://www.58pic.com/" as an example.
#========================================
library(rvest)
library(stringr)
library(downloader)

# function for extracting the url of the next page
get.next.page <- function(current.page){
  pages.url <- current.page %>%
    html_nodes("div.qt-pagination") %>%
    html_nodes("a") %>%
    as.character() %>%
    str_subset("下一页") %>%
    str_extract("http(.)*(html)")
  return(pages.url)
}

# function for extracting the url of each images in current page
get.images <- function(from){
  image.url <- from %>%
    html_nodes("div.card-img") %>%
    html_nodes("a") %>%
    html_nodes("img") %>%
    as.character() %>%
    str_extract("http(.)*(0x400a0a0)")
  return(image.url[!is.na(image.url)])
}

# input the url (first page of searching result of images you want)
start.page.url <- "http://www.58pic.com/tupian/40069643-0-0-default-5-0-花-0_2_0_0_0_0_0-.html"

save.address <- "D:\\Pictures\\" # fold to save downloaded images

# download images from page 1 to page 10
current.page.url <- start.page.url

for (page in 1:10) {
  current.page <- read_html(current.page.url) # analyze the html
  images <- get.images(current.page)
  if(length(images) > 0){
    sapply(1:length(images),
           function(x) download(images[x],
                                paste(save.address,"flower",page,"_",x,".jpg",sep = ""),mode = "wb"))
  }
  current.page.url <- get.next.page(current.page)
}
