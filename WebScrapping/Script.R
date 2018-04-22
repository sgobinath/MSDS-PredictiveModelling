install.packages("rvest")
library(rvest)

url <- "https://www.imdb.com/search/title?release_date=2017-01-01,2017-12-31&count=250"

web_page <- read_html(url)

class(web_page)
head(web_page)
str(web_page)

rank_data_html <- html_nodes(web_page, '.text-primary')
class(rank_data_html)
head(rank_data_html)

rank_data <- html_text(rank_data_html)
class(rank_data)
head(rank_data, 10)
length(rank_data)

rank_data <- as.numeric(rank_data)

title_data_html <- html_nodes(web_page, '.lister-item-header a')
title_data<-html_text(title_data_html)
head(title_data)

year_data_html <- html_nodes(web_page, '.lister-item-year')
year_data <- html_text(year_data_html)
head(year_data)

certificate_data_html <- html_nodes(web_page, 'span.certificate')
certificate_data <- html_text(certificate_data_html)
head(certificate_data)

runtime_data_html <- html_nodes(web_page, 'span.runtime')
runtime_data <- html_text(runtime_data_html)
runtime_data <- trimws(gsub("* min", "", runtime_data))
runtime_data <- as.numeric(runtime_data)
head(runtime_data)

genre_data_html <- html_nodes(web_page, 'span.genre')
genre_data <- html_text(genre_data_html)
genre_data <- trimws(gsub("\n *", "", genre_data))
head(genre_data)
genre_data <- gsub(",.*", "", genre_data)
head(genre_data)
length(genre_data)

rating_data_html <- html_nodes(web_page, 'div.inline-block.ratings-imdb-rating > strong')
rating_data <- html_text(rating_data_html)
rating_data <- as.numeric(rating_data)
head(rating_data)
length(rating_data)

metascore_data_html <- html_nodes(web_page, 'span.metascore')
metascore_data <- html_text(metascore_data_html)
metascore_data <- as.numeric(trimws(metascore_data))
head(metascore_data)
length(metascore_data)

desc_data_html <- html_nodes(web_page, 'div.lister-item-content > p:nth-child(4)')
desc_data <- html_text(desc_data_html)
desc_data <- gsub("\n *", "", desc_data)
head(desc_data)
length(desc_data)

director_data_html <- html_nodes(web_page, xpath = '//p/a[contains(@href, "_li_dr_")]')
director_data <- html_text(director_data_html)
head(director_data)
length(director_data)

ds_data_html <- html_nodes(web_page, 'div.lister-item-content > p:nth-child(5)')
ds_data <- html_text(ds_data_html)
head(ds_data)
length(ds_data)



