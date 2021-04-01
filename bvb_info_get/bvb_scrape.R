library(tidyverse)
library(rvest)

circuit <- function(url){
url %>%
  read_html(url) %>% 
  html_nodes(xpath = "//*[@id='lblTournament']/text()[1]") %>% 
    html_text()
}

tournament <- function(url){
  url %>% 
    read_html(url) %>% 
    html_nodes(xpath = '//*[@id="lblTournament"]/text()[2]') %>% 
    html_text()
}

date <- function(url){
  url %>% 
    read_html(url) %>% 
    html_nodes(xpath = '//*[@id="lblTournament"]/text()[3]') %>% 
    html_text()
}

score_duration <- function(url){
  url %>% 
    read_html(url) %>% 
    html_nodes(xpath = '//*[@id="lblScore"]') %>% 
    html_text()
}

winning_team <- function(url){
  url %>% 
    read_html(url) %>% 
    html_nodes(xpath = '//*[@id="lblTeam1Match"]') %>% 
    html_text()
}

losing_team <- function(url){
  url %>% 
    read_html(url) %>% 
    html_nodes(xpath = '//*[@id="lblTeam2Match"]') %>% 
    html_text()
}

w_team_set_1 <- function(url){
  url %>% 
    read_html(url) %>% 
    html_nodes(xpath = '//*[@id="lblTeam1Score1"]') %>% 
    html_text()
}

w_team_set_2 <- function(url){
  url %>% 
    read_html(url) %>% 
    html_nodes(xpath = '//*[@id="lblTeam1Score2"]') %>% 
    html_text()
}

w_team_set_3 <- function(url){
  url %>% 
    read_html(url) %>% 
    html_nodes(xpath = '//*[@id="lblTeam1Score3"]') %>% 
    html_text()
}

l_team_set_1 <- function(url){
  url %>% 
    read_html(url) %>% 
    html_nodes(xpath = '//*[@id="lblTeam2Score1"]') %>% 
    html_text()
}

l_team_set_2 <- function(url){
  url %>% 
    read_html(url) %>% 
    html_nodes(xpath = '//*[@id="lblTeam2Score2"]') %>% 
    html_text()
}

l_team_set_3 <- function(url){
  url %>% 
    read_html(url) %>% 
    html_nodes(xpath = '//*[@id="lblTeam2Score3"]') %>% 
    html_text()
}

total_stats <- function(url){
ifelse(length(read_html(url) %>%
  html_nodes("#MatchStatsControl1_ctl13_StatisticsTable") %>% 
  html_table() == 0), read_html(url) %>%
    html_nodes("#MatchStatsControl1_ctl13_StatisticsTable") %>% 
    html_table(), read_html(url) %>%
    html_nodes("#MatchStatsControl1_ctl10_StatisticsTable") %>% 
    html_table())[[1]] %>% 
    slice(-1) %>% 
    mutate(X1 = ifelse(X1 == 'Errors' & lag(X1,3) == 'ATTACK', 'Attack_Errors', X1),
           X1 = ifelse(X1 == 'Errors' & lag(X1,2) == 'SERVE', 'Serve_Errors', X1)) %>% 
    filter(!X1 %in% c('ATTACK', 'SERVE', 'DEFENSE')) %>% 
    rename(w_ply_1 = X2, w_ply_2 = X3, l_ply_1 = X4, l_ply_2 = X5) %>% 
    mutate(X1 = tolower(X1)) %>% 
    pivot_wider(names_from = X1, values_from = c(w_ply_1:l_ply_2))
}

page_num <- 51452:61455
url <- paste0('http://96.43.215.74/MatchRecap?matchid=', page_num)

get_data_list <- lapply(url, function(i) {
  
  tibble(circuit = circuit(i),
         tournament = tournament(i),
         date = date(i),
         score_duration = score_duration(i),
         winning_team = winning_team(i),
         losing_team = losing_team(i), 
         w_team_set_1 = w_team_set_1(i), 
         w_team_set_2 = w_team_set_2(i), 
         w_team_set_3 = w_team_set_3(i), 
         l_team_set_1 = l_team_set_1(i), 
         l_team_set_2 = l_team_set_2(i), 
         l_team_set_3 = l_team_set_3(i)
         #total_stats = total_stats(i)
  )
})

df <- do.call(rbind, get_data_list)
