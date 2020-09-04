library(RMySQL)
pwd <- readline(prompt="Enter root password:")
mydb <- dbConnect(MySQL(), user = 'root', password = pwd, 
                  dbname = 'lahmansbaseballdb', host = 'localhost')
rm(pwd)

allTables <- dbListTables(mydb)

df <- dbGetQuery(mydb, "select franchID, 
                        yearID, 
                        W, 
                        L, 
                        case when W>=L then 1 else 0 end above_500 
                        from teams")

teams <- dbGetQuery(mydb, "select distinct franchID
                           from teams")

nteams <- nrow(teams)

library(zoo)

df$rollW <- NA
df$rollL <- NA
df$rollAbove500 <- NA

for(i in 1:nteams) {
    t <- teams$franchID[i]
    df2 <- df[which(df$franchID==t),]
    wins <- rollsum(df2$W, 30, fill=NA, align='right')
    losses <- rollsum(df2$L, 30, fill=NA, align='right')
    ab500 <- rollsum(df2$above_500, 30, fill=NA, align='right')
    df[which(df$franchID==t),]$rollW <- wins
    df[which(df$franchID==t),]$rollL <- losses
    df[which(df$franchID==t),]$rollAbove500 <- ab500
}

df$rollWPct <- df$rollW/(df$rollW+df$rollL)

df[which(df$rollWPct == max(df$rollWPct,na.rm=TRUE)),]

library(dplyr)

# Which teams in history have had the best 20-year rolling winning percentages? (Wow Yankees)
slice_max(df, order_by = rollWPct, n = 20)

# Which teams had the worst stretches?
slice_min(df[which(df$yearID>1900),], order_by = rollWPct, n = 20)

# What do teams' last 20 years look like as of 2019?
df[which(df$yearID == 2019),]

# See the distribution of rolling years above .500
library(ggplot2)
ggplot(data = df, aes(x = rollAbove500)) + 
    geom_histogram(bins = 30,
                   na.rm = TRUE) +
    scale_x_continuous(breaks = c(0,5,10,15,20,25,30))

# Create a new dataframe aggregated by franchise with summary stats
teamPerformance <- df %>%
    group_by(franchID) %>%
    summarize(totalW = sum(W),
              totalL = sum(L),
              winPct = sum(W)/(sum(W) + sum(L)),
              avgAbove500 = mean(above_500),
              avgRoll500 = mean(rollAbove500, na.rm=TRUE),
              bestRoll500 = max(rollAbove500, na.rm=TRUE),
              worstRoll500 = min(rollAbove500, na.rm=TRUE),
              bestRollWPct = max(rollWPct, na.rm=TRUE),
              worstRollWPct = min(rollWPct, na.rm=TRUE))

activeTeamPerformance <- 
    teamPerformance[teamPerformance$franchID %in% df[which(df$yearID == 2019), 'franchID'], ]


