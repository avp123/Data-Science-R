library(Lahman)

# table w/ basic player info
playerInfo <- Fielding %>%
  group_by(playerID) %>%
  arrange(desc(G)) %>%
  slice(1) %>%
  ungroup %>%
  left_join(Master, by="playerID") %>%
  select(playerID, nameFirst, nameLast, POS)

# table of all rookie of the year winners + their stats
ROY <- AwardsPlayers %>%
  filter(awardID == "Rookie of the Year") %>%
  left_join(playerInfo, by="playerID") %>%
  rename(rookie_year = yearID) %>%
  right_join(Batting, by="playerID") %>%
  mutate(AVG = H/AB) %>%
  filter(POS != "P")

# table w/ rookie and sophmore seasons of ROY award winners
ROY <- ROY %>%
  filter(yearID == rookie_year | yearID == rookie_year+1) %>%
  group_by(playerID) %>%
  mutate(rookie = ifelse(yearID == min(yearID), "rookie", "sophomore")) %>%
  filter(n() == 2) %>%
  ungroup %>%
  select(playerID, rookie_year, rookie, nameFirst, nameLast, AVG)

# create rookie and sophmore Batting Average columns
ROY <- ROY %>% spread(rookie, AVG) %>% arrange(desc(rookie))
ROY

# proportion of players who played worse in sophmore season compared to rookie season
mean(ROY$sophomore - ROY$rookie <= 0)

# same analysis on all players in 2013 and 2014 seasons that batted 130+ times (minimum to win rookie of the year award)
two_years <- Batting %>%
  filter(yearID %in% 2013:2014) %>%
  group_by(playerID, yearID) %>%
  filter(sum(AB) >= 130) %>%
  summarize(AVG = sum(H)/sum(AB)) %>%
  ungroup %>%
  spread(yearID, AVG) %>%
  filter(!is.na(`2013`) & !is.na(`2014`)) %>%
  left_join(playerInfo, by="playerID") %>%
  filter(POS!="P") %>%
  select(-POS) %>%
  arrange(desc(`2013`)) %>%
  select(nameFirst, nameLast, `2013`, `2014`)
two_years

# what happened to worst performers of 2013
arrange(two_years, `2013`)

# correlation between 2013 and 2014
qplot(`2013`, `2014`, data = two_years)

summarize(two_years, cor(`2013`,`2014`))

# Conclusion:
#   There is no such thing as a rookie slump. 
#   All rookies are likely to regress to the mean in their second seasons.
#   This holds true for all baseball players in any given 2 year stretch as well.
