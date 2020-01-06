library(httr)
library(jsonlite)
library(plyr)

features <- function() {
  url <- modify_url("https://delphi.cs.uni-paderborn.de/", path = "/api/features")
  resp <- GET(url)
  if (http_type(resp) != "application/json") {
    stop("API did not return json", call. = FALSE)
  }
  
  fromJSON(content(resp, "text", encoding = "UTF-8")) %>% select(name, description)
}

version <- function() {
  url <- modify_url("https://delphi.cs.uni-paderborn.de/", path = "/api/version")
  content(GET(url), "text", encoding = "UTF-8")
}

retrieve <- function(id) {
  url <- modify_url("https://delphi.cs.uni-paderborn.de", path = paste("/api/retrieve/",id, sep=""))
  resp <- GET(url)
  if (http_type(resp) != "application/json") {
    stop("API did not return json", call. = FALSE)
  }
  
  fromJSON(content(resp, "text", encoding = "UTF-8")) # %>% select(name, description)
}

search <- function(query) {
  url <- modify_url("https://delphi.cs.uni-paderborn.de/", path = "/api/search")
  resp <- POST(url, body=list(query = query), encode = "json")
  if (http_type(resp) != "application/json") {
    stop("API did not return json", call. = FALSE)
  }
  fromJSON(content(resp, "text", encoding = "UTF-8"))
}