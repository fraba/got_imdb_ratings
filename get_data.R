library(rvest)
library(dplyr)
library(stringr)

base_url0 <- 
  "https://www.imdb.com/title/tt0944947/episodes?season=%s&ref_=tt_eps_sn_1"
base_url1 <- 
  "https://www.imdb.com/title/%s/ratings?demo=%s"

demos1 <- 
  c("imdb_users", "males", "females")
demos2 <- 
  c("aged_under_18", "aged_18_29", "aged_30_44", "aged_45_plus")
demos3 <- character()
for (i in 2:length(demos1)) {
  for (j in 1:length(demos2)) {
    demos3 <- 
      c(demos3, paste0(demos1[i], "_", demos2[j]))
  }
}
demographics <-
  c(demos1, demos2, demos3)


# Functions
scrapeSeasonEpisodeCodes <- function(season_number) {
  
  webpage <- read_html(sprintf(base_url0, season_number))
  
  res <-
    webpage %>%
    html_nodes(xpath = '//*[@id="episodes_content"]//a[@itemprop="name"]')
  
  codes <-
    res %>% 
    html_attr(name = "href") %>% 
    str_extract("tt[0-9]+")
  
  titles <-
    res %>% 
    html_attr(name = "title")
  
  return(data.frame(code = codes,
                    title = titles, 
                    stringsAsFactors = F))
  
}

scrapeHist <- function(episode_code, demo) {
  
  webpage <- 
    read_html(sprintf(base_url1, episode_code, demo))
  res <-
    webpage %>% 
    html_node("table") %>%
    html_table() %>%
    select(Rating, Votes) %>%
    mutate(Votes = as.numeric(gsub(",", "", Votes)),
           Rating = factor(Rating))
  return(res)
}

# First, the episodes
episodes <- data.frame()
for (i in 1:8) {
  print(i)
  this_season <- scrapeSeasonEpisodeCodes(i)
  this_season$season <- i
  episodes <- rbind(episodes, this_season)
  Sys.sleep(10)
}
episodes$season <- as.factor(episodes$season)
save(episodes, file = "data/episodes.RData")

# Then, the ratings
ratings <- data.frame()
for (i in 1:(nrow(episodes)-1)) { # One episode is missing
  print(episodes$code[i])
  for (demo in demographics) {
    print(demo)
    this_episode <- 
      scrapeHist(episodes$code[i], demo)
    this_episode$Demographics <- demo
    this_episode$Episode <- episodes$code[i]
    ratings <- rbind(ratings, this_episode)
    Sys.sleep(sample(1:5, 1))
  }
}
ratings$Demographics <- factor(ratings$Demographics)
ratings$Episode <- factor(ratings$Episode)
save(ratings, file = "data/ratings.RData")
