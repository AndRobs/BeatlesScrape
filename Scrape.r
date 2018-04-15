library(rvest)
library(tidyverse)




GetSongLinks <- function(pagenum){
  
  # Takes in a page number (should validate it's between 1 and 6) and scrapes links
  
  link <- paste0('http://www.metrolyrics.com/beatles-alpage-', pagenum,'.html')
  
  links <- read_html(link)     %>%
    html_nodes('#popular')     %>%
    html_nodes('.songs-table') %>%
    html_nodes('td')           %>%
    html_nodes('a')            %>%
    html_attr('href')
}


# Get all song links 

song.links <- do.call(c, lapply(1:6, GetSongLinks))




GetLyrics <- function(song.link) {
  
  # Takes the links we previous made and cleans them
  
  Sys.sleep(1)  # Be kind to their servers
  
  song.link %>%
    read_html()          %>%
    html_nodes('.verse') %>%
    html_text() %>%
    tbl_df() %>%
    mutate(value = gsub(pattern = "\n", replacement = " \n ", value)) %>%
    summarise(value = paste(value, "\n", collapse = "" ))
  
}


LyricsDf <- do.call(rbind, lapply(song.links, GetLyrics))


# Checking data quals -----------------------------------------------------

# A lot of the songs are insturmental and the lyrics are just '[Instrumental'
# We'll use a histogram to decide on a sensible cut off for the minimum lyric length

LyricsDf %>%
  mutate(len = nchar(value)) %>%
  ggplot() +
  geom_histogram(aes(x = len), binwidth = 50, fill = '#A0C2CC') +
  geom_segment(x= 500, xend = 500, yend = 100, y = 0, size = 1.5, lty = 2) +
  theme_minimal(20) +
  labs(x = 'Number of characters',
       y = 'Frequency',
       title = 'Length of Beatles songs (characters)')



# Filtering data ----------------------------------------------------------
# Get rid of everything below 500 characters

LyricsDf %>%
  filter(nchar(value) > 500) %>%
  distinct(value) %>%
  mutate(value = gsub(pattern = "\n ", replacement = "\n", value)) %>%
  unlist(use.names=FALSE) %>%
  writeLines("outfile.txt")







