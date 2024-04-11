install.packages("tidyverse")
library(tidyverse)
install.packages("ggplot2")
library(ggplot2)
install.packages("janitor")
library(janitor)

## Importing csv file
test_matches <- read_csv("test_matches_records.csv")
View(test_matches)

## Data Cleaning process
colnames(test_matches)
glimpse(test_matches)

test_matches_records <- rename(test_matches, "sr_no" = "...1")
clean_names(test_matches_records)
test_matches_records$sr_no <- test_matches_records$sr_no + 1 
head(test_matches_records)

## Removing special characters "*,+"
testmatches <- test_matches_records %>% 
  mutate(
    matches=as.numeric(str_remove(Matches, "[*+]")),
    fours=as.numeric(str_remove(Fours, "[*+]")),
    sixes=as.numeric(str_remove(ixes, "[*+]")),
    balls_faced=as.numeric(str_remove(balls_faced, "[*+]"))
    )
View(testmatches)

## Finding duplicates
get_dupes(testmatches)

## Organizing data/Transform data

test_matches_clean <- separate(testmatches, col = Span, into = c("From", "To"), sep = "-")
View(test_matches_clean)

## Data Analysis

### Total matches by country
matches_played_by_country <- test_matches_clean %>% 
  group_by(country) %>% 
  summarise(total_matches = sum(matches)) %>% 
  arrange(desc(total_matches)) 
print(matches_played_by_country)

my_pie_chart <- matches_played_by_country %>% 
  ggplot(aes(x = '', y = reorder(total_matches, Country ), 
             fill = factor(reorder(Country, total_matches)) )) +
  geom_bar(stat = "identity", width = 1, color = 'white') +
  geom_text(aes(x = 1.4,label = total_matches), 
            position =  position_stack(vjust= 0.5), color = 'black') +
  theme_void() +
  theme_classic() +
  theme(legend.position = "right") +
  coord_polar("y", start = 0)  +
  theme(plot.title = element_text(hjust = 0.5, size = 15)) +
  theme(plot.subtitle = element_text(hjust = 0.5)) +
  theme(axis.line = element_blank()) +
  theme(axis.text = element_blank()) +
  theme(axis.ticks = element_blank()) +
  guides(fill = guide_legend(reverse = TRUE)) +
  labs(x = NULL, y = NULL,
       title = 'Pie chart of Test Matches Record', 
       subtitle = 'Total matches played by Country') +
  scale_fill_brewer(palette = "Blues", name = "Country") 
print(my_pie_chart)

### Total players in each country
players_in_country <- test_matches_clean %>%
  group_by(country) %>% 
  summarise(players_count = n()) %>% 
  arrange(desc(players_count))
print(players_in_country)

players_bar_chart <-players_in_country %>% 
  ggplot(aes(x = reorder(country, desc(players_count)), 
             y = players_count,
             fill = factor(reorder(country, players_count)))) + 
  geom_bar (stat="identity", width = 0.8) +
  geom_text(aes(label = players_count), 
            vjust = -0.3, size = 4) +
  labs(title = "Players in each country",  
       x= NULL, y="No. of players") +
  theme_get() +
  theme(plot.title = element_text(hjust = 0.5, size = 20)) +
  guides(fill = guide_legend(reverse = TRUE)) +
  scale_fill_brewer(palette = "Blues", 
                    name = "Country") 
print(players_bar_chart)

### Top 10 players with max runs and there highest scores
test_matches_clean %>% 
  select(names, total_runs, highest_score) %>% 
  arrange(desc(total_runs)) %>% 
  head(10) 

### Players from IND have max avg
test_matches_clean %>% 
  select(names, average) %>% 
  filter(test_matches_clean$country == "IND") %>% 
  arrange(desc(average))

### Players with max strike rate
test_matches_clean %>% 
  select(names, strike_rate) %>% 
  arrange(desc(strike_rate)) %>% 
  head(10)

### Players with highest score 200+
test_matches_clean %>% 
  select(country, names, highest_score) %>% 
  filter(highest_score >= 300) %>% 
  arrange(desc(highest_score))

### Finding relation between Total Runs and Total Matches
runs_matches <- test_matches_clean %>% 
  select(matches, total_runs, balls_faced) %>% 
  ggplot(mapping = aes(x = total_runs, y = matches, 
                       color = -total_runs)) +
  geom_smooth(formula = y ~ x, method = "loess", 
              linetype = 'dashed', color = 'navy') +
  geom_point(size = 2.5) +
  labs(title = "Matches vs Total Runs",
       x = 'Runs', y = 'Matches')+
  theme_light() +
  theme(plot.title = element_text(hjust = 0.5, size = 20)) +
  theme(legend.position = "") 
print(runs_matches)

### Players from India with max hundreds
test_matches_clean %>% 
  select(names, hundreds) %>% 
  filter(test_matches_clean$country == "IND") %>% 
  arrange(desc(hundreds))

### The player with earliest debut
test_matches_clean %>% 
  select(names, from) %>% 
  arrange(from) %>% 
  head(5)

##Players with long career 
test_matches_clean %>% 
  select(names, from, to) %>% 
  mutate(played_years = test_matches_clean$to - test_matches_clean$from) %>% 
  arrange(desc(played_years))



  














  
