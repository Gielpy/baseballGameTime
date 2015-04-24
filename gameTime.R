# gameTime.R
# downloads the massive MLB Gameday data.
# Author: grepo, based off work by apeecape
# Email: gregoryllpowell at gmail dot com
# Updated: April 7, 2015
# Version 0.1
# Version History
# # 0.1 ~ pull down game times from MLB Gameday website

#far and away the majority of this work was done by apeecape for his Gameday Pitching Matchups Script
#I have merely adjusted a few XML search parameters to pull down game times
#To use, run the entire script in R, specifying the start and end dates beforehand
#Then run GameTime() to start pulling data and save it in a text file
#My AMD FX-4100, 8 GB RAM desktop was able to pull down a whole year of GameTime in about 5-10 minutes
#Did run into memory issues if I tried to do longer pulls

# required libraries:
library(XML)

# code for <game type> in game.xml (input game.type in code)
# "S" ~ spring training, "R" ~ regular season, "D" ~ Division Series
# "L" ~ League Championship Series "W" ~ World Series

# code for <game gameday_sw> in game.xml
# http://sports.dir.groups.yahoo.com/group/RetroSQL/message/320
# "N" ~ missing, no pitch info
# "Y" ~ standard w/ pitch locations
# "E" ~ w/ pitch f/x
# "P" ~ for 2010, whatever that's supposed to mean


# -----------------------------------------------------------

GameTime <- function(fileloc = "./elapsedTime2015.txt",
                            start.date = "2015-04-15", end.date = "2015-05-01",
                            URL.base = "http://gd2.mlb.com/components/game/mlb/",
                            game.type = "R",
                            elapsed.Time = "elapsedTime"
                            ) {
  # write initial variables on file
  meta <- c("Year", "Month", "Day", "Home", "Away")
  write(c(meta, elapsed.Time), file = fileloc,
        ncol = length(c(elapsed.Time)) + length(meta), sep = " ")
  
  # transfer date info
  start.date <- as.POSIXlt(start.date); end.date <- as.POSIXlt(end.date);
  diff.date <- as.numeric(difftime(end.date, start.date))
  date.range <- as.POSIXlt(seq(start.date, by = "days",
                               length = 1 + diff.date))
  
  for (i in 1:(diff.date+1)) {
    year <- date.range[i]$year + 1900
    month <- date.range[i]$mon + 1
    day <- date.range[i]$mday
    URL.date <- paste(URL.base, "year_", year, "/",
                      ifelse(month >= 10, "month_", "month_0"), month, "/",
                      ifelse(day >= 10, "day_", "day_0"), day, "/", sep = "")
    
    # grab matchups for today
    HTML.day <- htmlParse(URL.date)
    parse.day <- xpathSApply(HTML.day, "//a[@*]", xmlGetAttr, "href")
    parse.day <- parse.day[grep("^gid_*", parse.day)]
    
    # if games exists today
    if (length(parse.day) >= 1) {
      
      # for each game
      for (game in 1:length(parse.day)) {
        print(game)
        URL.game <- paste(URL.date, parse.day[game], sep = "")
        HTML.game <- htmlParse(URL.game)
        parse.game.exists <- xpathSApply(HTML.game, "//a[@*]", xmlGetAttr, "href")
        
        # if rawboxscore.xml exists
        if (sum(match(parse.game.exists, "rawboxscore.xml"), na.rm = T) > 0) {
          
          # grab game type (regular season, etc.) and gameday type (pitch f/x, etc.)
          XML.game <- xmlInternalTreeParse(paste(URL.game, "rawboxscore.xml", sep = ""))
          parse.game <- sapply(c("game_type", "gameday_sw"), function (x)
            xpathSApply(XML.game, "//boxscore[@*]", xmlGetAttr, x))
          
          # if proper game type: "R" ~ regular season, "S" ~ spring, "D" ~ divison series
          # "L" ~ league chamption series, "W" ~ world series
          if (parse.game['game_type'] == game.type) {
            # grab team names
            parse.teams <- sapply(c("team_code"), function (x)
              xpathSApply(XML.game, "//team[@*]", xmlGetAttr, x))
            home <- parse.teams[1]; away <- parse.teams[2]
            
            # if pitch f/x data exists
            if (parse.game["gameday_sw"] == "E" | parse.game["gameday_sw"] == "P") {
              
              # grab number of innings played
              HTML.Ninnings <- htmlParse(paste(URL.game, "inning/", sep = ""))
              parse.Ninnings <- xpathSApply(HTML.Ninnings, "//a[@*]", xmlGetAttr, "href")
              parse.elapsedTime <- sapply(c("elapsed_time"), function(x)
                  xpathSApply(XML.game, "//boxscore[@*]", xmlGetAttr, x))
              
           
              # check to see if atbat exists
              if (length(parse.Ninnings) > 0) {
               
                  
                    
                    # write results
                    write(t(cbind(year, month, day, home, away,
                                  parse.elapsedTime)), file = fileloc,
                          ncol = length(c(elapsed.Time)) + length(meta),
                          append = T, sep = " ")
                  }
                }
              }
            }
          }
        }
      }
    }
                            