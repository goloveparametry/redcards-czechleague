# libs
require(dplyr)
require(rvest)
require(tidyr)
require(ggplot2)
require(plotly)
require(RColorBrewer)


# global config
options(stringsAsFactors = FALSE)

# url1 http://en.fortunaliga.cz/statistiky?unit=1&status=0&parameter=10&season=2022&club=0&game_limit=0&nationality=&age=0&list_number=0&order=8&order_dir=2
# url2 http://en.fortunaliga.cz/statistiky?unit=1&status=0&parameter=10&season=2022&club=0&game_limit=0&nationality=&age=0&list_number=0&order=8&order_dir=2&list_number=50

datalist = list()

for (i in 2022:2022) {

  df <- data.frame(
  )
  
  j <- 0
  while (j < 400) {
    
    url_base <- paste("http://en.fortunaliga.cz/statistiky?unit=1&status=0&parameter=10&season=", i ,"&club=0&game_limit=0&nationality=&age=0&list_number=",j,"&order=8&order_dir=2", sep = "")
    dt <- url_base %>% read_html() %>% html_nodes(xpath='//*/table[1]')%>% html_table(fill=T) #xml_node
    df <- rbind(df, data.frame(bind_rows(dt[[2]])))
    
    
    # loop until no red card
    if (sapply(list(df$RC), function(x) any(x == 0))) {
      break
    }
    j <- j + 50
  }
  
  df$season <- i
  dat <- assign(paste("df", i, sep = ""), df)
  
  #TODO: fix the datalist positioning
  datalist[[i]] <- dat

}

# Dataset metrics
# Club - Club
# Po - Position
# GP - Games Played
# YC - Yellow Cards
# YCG - Yellow Cards per Game
# RC - Red Cards
# RCG - Red Cards per Game
# SC - Streak without card (in minutes)
# C1H - Cards in 1st half
# C2H - Cards in 2nd half
# F - Fouls
# OF - Offsides
# season - Season

# merge all seasons
df1 <- do.call(rbind, datalist)

# cards trend for all clubs
aggregate(df1$RC, by=list(Category=df1$season), FUN=sum)

# cards for a specific club
df1$RC[df1$Club == "OLO"]

# TODO: output to csv
# write.csv(df1, "data/redcards-seasons.csv")


# TODO: fix for consistent levels in Po Levels: - D DM F M OM


## trend of red cards each season
ggplot(df1, aes(season, RC)) + 
  geom_col() +
  labs(title = "Trend of red cards in Czech League 2010-2022") +
  xlab("Season") +
  ylab("Number of red cards")

## trend of red cards by player position
ggplot(df1[df1$Po %in% c("D", "M", "F"),], aes(season, RC, fill = Po)) + 
  geom_col()


# aggre for players
# http://en.fortunaliga.cz/statistiky?unit=1&status=0&parameter=10&season=1&club=0&game_limit=0&nationality=&age=0&list_number=0&order=5&order_dir=2#stats

url_base <- "http://en.fortunaliga.cz/statistiky?unit=1&status=0&parameter=10&season=1&club=0&game_limit=0&nationality=&age=0&list_number=0&order=5&order_dir=2#stats"
dt2 <- url_base %>% read_html() %>% html_nodes(xpath='//*/table[1]')%>% html_table(fill=T) #xml_node
df2 <- data.frame(bind_rows(dt2[[2]]))

# write.csv(df2, "data/redcards-alltime.csv")


#  and scatter plot red and yellow cards
ggplot(df2, aes(x=GP, y=YC, color = RC, label = Player)) + 
  geom_point() + geom_text(hjust=0, vjust=0, size=3
                           
                           )
