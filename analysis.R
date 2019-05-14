caption <- "Design: @FrBailo | Source: IMDB.com | GitHub: fraba/got_imdb_ratings"

load("data/ratings.RData")
load("data/episodes.RData")

library(ggplot2)
library(dplyr)
library(scales)

episodes <-
  episodes %>%
  mutate(episode_number_global = 1:n()) %>%
  group_by(season) %>%
  mutate(episode_number = 1:n())

ratings <- 
  merge(ratings, episodes, 
        by.x = "Episode", by.y = "code")

theme_set(theme_bw())

myPercent <- function(x) paste0(sprintf("%.0f", round(x*100,0)), "%")

ggsave("img/01.png", width = 8, height = 6,
       ratings %>%
         filter(Demographics == "imdb_users") %>%
         group_by(season) %>%
         mutate(season_mean = 
                  weighted.mean(as.numeric(as.character(Rating)), 
                                Votes)) %>%
         group_by(Episode) %>%
         mutate(`Distribution of Votes` = Votes / max(Votes),
                episode_mean = 
                  weighted.mean(as.numeric(as.character(Rating)), 
                                Votes)) %>%
         ggplot(aes(x=Rating, y=`Distribution of Votes`)) +
         geom_vline(aes(xintercept=episode_mean), colour = "red", 
                    size = 1.8, alpha = .5) +
         # geom_vline(aes(xintercept=season_mean), colour = "blue", 
         #            size = 1.5, alpha = .5) +
         geom_col() +
         facet_grid(season ~ episode_number) +
         scale_x_discrete(breaks = c("1", rep("",8), "10")) +
         labs(x="Rating (1 to 10)", y="Distribution of votes", 
              title = sprintf("Game of Thrones ratings: All IMDB users (tot. votes = %s)", 
                              format(sum(ratings$Votes[
                                ratings$Demographics=="imdb_users"]),
                                format="f", big.mark=",")),
              caption = caption) +
         theme(axis.text = element_blank(),
               axis.ticks = element_blank(),
               panel.grid = element_blank())
)

ggsave("img/02.png", width = 8, height = 6,
ratings %>%
  filter(Demographics == "females") %>%
  group_by(season) %>%
  mutate(season_mean = 
           weighted.mean(as.numeric(as.character(Rating)), 
                         Votes)) %>%
  group_by(Episode) %>%
  mutate(`Distribution of Votes` = Votes / max(Votes),
         episode_mean = 
           weighted.mean(as.numeric(as.character(Rating)), 
                         Votes)) %>%
  ggplot(aes(x=Rating, y=`Distribution of Votes`)) +
  geom_vline(aes(xintercept=episode_mean), colour = "red", 
             size = 1.8, alpha = .5) +
  # geom_vline(aes(xintercept=season_mean), colour = "blue", 
  #            size = 1.5, alpha = .5) +
  geom_col() +
  facet_grid(season ~ episode_number) +
  scale_x_discrete(breaks = c("1", rep("",8), "10")) +
  labs(x="Rating (1 to 10)", y="Distribution of votes", 
       title = sprintf("Game of Thrones ratings: Female IMDB users (tot. votes = %s)", 
                       format(sum(ratings$Votes[
                         ratings$Demographics=="females"]),
                         format="f", big.mark=",")),
       caption = caption) +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank())
)

ggsave("img/03.png", width = 8, height = 6,
ratings %>%
  filter(Demographics == "aged_45_plus") %>%
  group_by(season) %>%
  mutate(season_mean = 
           weighted.mean(as.numeric(as.character(Rating)), 
                         Votes)) %>%
  group_by(Episode) %>%
  mutate(`Distribution of Votes` = Votes / max(Votes),
         episode_mean = 
           weighted.mean(as.numeric(as.character(Rating)), 
                         Votes)) %>%
  ggplot(aes(x=Rating, y=`Distribution of Votes`)) +
  geom_vline(aes(xintercept=episode_mean), colour = "red", 
             size = 1.8, alpha = .5) +
  # geom_vline(aes(xintercept=season_mean), colour = "blue", 
  #            size = 1.5, alpha = .5) +
  geom_col() +
  facet_grid(season ~ episode_number) +
  scale_x_discrete(breaks = c("1", rep("",8), "10")) +
  labs(x="Rating (1 to 10)", y="Distribution of votes", 
       title = sprintf("Game of Thrones ratings: Over 45yo IMDB users (tot. votes = %s)", 
                       format(sum(ratings$Votes[
                         ratings$Demographics=="aged_45_plus"]),
                         format="f", big.mark=",")),
       caption = caption) +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank())
)

ggsave("img/04.png", width = 8, height = 6,
ratings %>%
  filter(Demographics == "females_aged_45_plus") %>%
  group_by(season) %>%
  mutate(season_mean = 
           weighted.mean(as.numeric(as.character(Rating)), 
                         Votes)) %>%
  group_by(Episode) %>%
  mutate(`Distribution of Votes` = Votes / max(Votes),
         episode_mean = 
           weighted.mean(as.numeric(as.character(Rating)), 
                         Votes)) %>%
  ggplot(aes(x=Rating, y=`Distribution of Votes`)) +
  geom_vline(aes(xintercept=episode_mean), colour = "red", 
             size = 1.8, alpha = .5) +
  # geom_vline(aes(xintercept=season_mean), colour = "blue", 
  #            size = 1.5, alpha = .5) +
  geom_col() +
  facet_grid(season ~ episode_number) +
  scale_x_discrete(breaks = c("1", rep("",8), "10")) +
  labs(x="Rating (1 to 10)", y="Distribution of votes", 
       title = sprintf("Game of Thrones ratings: Females over 45yo IMDB users (tot. votes = %s)", 
                       format(sum(ratings$Votes[
                         ratings$Demographics=="females_aged_45_plus"]),
                         format="f", big.mark=",")),
       caption = caption) +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank())
)

ggsave("img/05.png", width = 8, height = 6,
ratings %>%
  filter(Demographics == "imdb_users") %>%
  ggplot(aes(x=episode_number_global,
             y=as.numeric(as.character(Rating)))) +
  geom_point(aes(size=Votes, colour=season)) +
  labs(title = "Game of Thrones ratings: All IMDB users", 
       x="Episode", y="Rating", colour="Season",
       caption = caption) +
  scale_y_continuous(breaks = 1:10) +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank())
)

ggsave("img/06.png", width = 8, height = 6,
ratings %>%
  filter(Demographics %in% c("males", "females")) %>%
  group_by(episode_number_global, season) %>%
  summarize(female_to_male = 
           sum(Votes[Demographics=="females"]) / 
           sum(Votes)) %>%
  ggplot(aes(x=episode_number_global,y=female_to_male)) +
  geom_line() +
  geom_point(aes(colour=season), size = 3) +
  geom_smooth(se=FALSE, colour = 'red') +
  scale_y_continuous(label = myPercent) +
  labs(title = "Game of Thrones ratings: All IMDB users", 
       x="Episode", y="Female users", colour="Season",
       caption = caption) +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank())
)
  

  

