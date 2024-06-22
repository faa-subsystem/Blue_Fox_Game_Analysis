# Blue_Fox_Game_Analysis
Project manager suggest to use public data sets with game sales and game rating data. Data collected from: [*Video Game Sales with Ratings*](https://www.kaggle.com/datasets/rush4ratio/video-game-sales-with-ratings) and [*Video Game Sales*](https://www.kaggle.com/datasets/gregorut/videogamesales).

*Metadata:*

- Video Game Sales with Ratings:

  - Critic_score - Aggregate score compiled by Metacritic staff
  - Critic_count - The number of critics used in coming up with the Critic_score
  - User_score - Score by Metacritic's subscribers
  - User_count - Number of users who gave the user_score
  - Developer - Party responsible for creating the game
  - Rating - The ESRB ratings.

- Video Game Sales:

  - Rank - Ranking of overall sales
  - Name - The games name
  - Platform - Platform of the games release (i.e. PC,PS4, etc.)
  - Year - Year of the game's release
  - Genre - Genre of the game
  - Publisher - Publisher of the game
  - NA_Sales - Sales in North America (in millions)
  - EU_Sales - Sales in Europe (in millions)
  - JP_Sales - Sales in Japan (in millions)
  - Other_Sales - Sales in the rest of the world (in millions)
  - Global_Sales - Total worldwide sales.


```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)

```

# Business task

## <span style="color:#35F7bb">Summary</span>

Blue Fox Games, a game development company is planning to develop a catalog of various games. They want to discover and define industry trends and user preferences to ensure the most potential players and better sales.

*Objectives:*

- Define user trends and preferences by analyzing game sales data.

- Find insights in order to get better game critics.

- How could the company comply with better game standards?

- How to apply the insights?


# Prepare

## <span style="color:#35F7bb">Data used for this analysis</span>

Project manager suggest to use public data sets with game sales and game rating data. Data collected from: [*Video Game Sales with Ratings*](https://www.kaggle.com/datasets/rush4ratio/video-game-sales-with-ratings) and [*Video Game Sales*](https://www.kaggle.com/datasets/gregorut/videogamesales).

*Metadata:*

- Video Game Sales with Ratings:

  - Critic_score - Aggregate score compiled by Metacritic staff
  - Critic_count - The number of critics used in coming up with the Critic_score
  - User_score - Score by Metacritic's subscribers
  - User_count - Number of users who gave the user_score
  - Developer - Party responsible for creating the game
  - Rating - The ESRB ratings.

- Video Game Sales:

  - Rank - Ranking of overall sales
  - Name - The games name
  - Platform - Platform of the games release (i.e. PC,PS4, etc.)
  - Year - Year of the game's release
  - Genre - Genre of the game
  - Publisher - Publisher of the game
  - NA_Sales - Sales in North America (in millions)
  - EU_Sales - Sales in Europe (in millions)
  - JP_Sales - Sales in Japan (in millions)
  - Other_Sales - Sales in the rest of the world (in millions)
  - Global_Sales - Total worldwide sales.


## <span style="color:#35F7bb"> Load Packages</span>

```{r load packages, results='hide', warning=FALSE, error=FALSE, message=FALSE}
library(lubridate)
library(ggplot2)
gctorture(on = FALSE)
library(tidyverse)
```

## <span style="color:#35F7bb"> Import Data</span>

```{r import data, results='hide'}
vgsales <- read.csv("VG_Data/vgsales.csv")
vgscore <- read.csv("VG_Data/Video_Games_Sales_as_at_22_Dec_2016.csv")

```

## <span style="color:#35F7bb">Observe data sets</span>
```{r Observe data sets, results='hide'}
str(vgsales)
str(vgscore)

```

*Observations:*
Data sets are in wide format. There are N/A entries for Critic_score, Critic_count, User_score and User_count, this will have to be filtered and clean in order to get only the relevant data. Also, the User_Score column is in character format, it should be in numeric format.


# Process

## <span style="color:#35F7bb">Check and Cleaning data</span>

```{r cleaning repeated}
# check for duplicates:
sum(duplicated(vgsales))
sum(duplicated(vgscore))

#check for n/a values:
colSums(is.na(vgsales))
colSums(is.na(vgscore))

```

The variables Critic_Score, Critic_Count, User_Score, and User_Count of vgscore have N/A values, in order to be effective in the analysis, it is necessary to take only the data that contains that information and eliminate the N/A.

## <span style="color:#35F7bb">Eliminating N/A values</span>

```{r filter any N/A rows}
# filter any N/A rows:
vgscore_v2 <- vgscore %>% 
  drop_na()
```

## <span style="color:#35F7bb">Fixing variable types </span>

```{r variable types}
vgscore_v2$User_Score <- as.numeric(vgscore_v2$User_Score)
```

## <span style="color:#35F7bb"> Summary statistics</span>

```{r summary statistics}

vgsales %>% 
  mutate(NA_Sales_dollar = NA_Sales*1000000 ,
         EU_Sales_dollar = EU_Sales*1000000,
         JP_Sales_dollar = JP_Sales*1000000,
         Other_Sales_dollar = Other_Sales*1000000,
         Global_Sales_dollar = Global_Sales*1000000) %>% 
  select(NA_Sales_dollar, EU_Sales_dollar, JP_Sales_dollar, Other_Sales_dollar, Global_Sales_dollar) %>%
  summary()

vgscore_v2 %>% 
  select(Critic_Score, Critic_Count, User_Count,User_Score) %>% 
  summary()

```
*Observations:*

- Median in JP_Sales (sales in japan) is 0, this could be a signal that a lot of games listed in this data where not sale in Japan, or the sales data in Japan was not entirely collected. 

- By the max sales we can see that he most lucrative region for sales is North America.

## <span style="color:#35F7bb"> Merge Score and Sales data</span>

```{r merge data}
score_sales <- merge(vgscore_v2, vgsales, by = c("Name", "Platform", "Genre", "NA_Sales","EU_Sales", "JP_Sales", "Other_Sales", "Global_Sales"))


```

# Analysis

## <span style="color:#35F7bb"> Best-selling games </span>

Even if vgscore table has N/A values in score columns i will use it since i just need sales and names to do this analysis and it has more data about game sales. 
#### Top 6 best-selling games in North America
```{r best selling NA}

vgsales %>% 
  select(Name, NA_Sales) %>% 
  group_by(Name) %>% 
  summarise(total_sales = sum(NA_Sales)) %>% 
  arrange(desc(total_sales)) %>% 
  head()

```
The Best-selling game in NA was Wii Sports, a sports game.

#### TOP 6 best-selling games in Europe
```{r bs Europe}
vgsales %>% 
  select(Name, EU_Sales) %>% 
  group_by(Name) %>% 
  summarise(total_sales = sum(EU_Sales)) %>% 
  arrange(desc(total_sales)) %>% 
  head()

```
The Best-selling game in Europe was Wii Sports, a sports game.

#### TOP 6 best-selling games in Japan
```{r bs Japan}
vgsales %>% 
  select(Name, JP_Sales) %>% 
  group_by(Name) %>% 
  summarise(total_sales = sum(JP_Sales)) %>% 
  arrange(desc(total_sales)) %>% 
  head()

```
The Best-selling game in Japan was Pokemon Red/Pokemon Blue, a Role-Playing.

#### TOP 6 best-selling games in Other regions
```{r Other regions}
vgsales %>% 
  select(Name, Other_Sales) %>% 
  group_by(Name) %>% 
  summarise(total_sales = sum(Other_Sales)) %>% 
  arrange(desc(total_sales)) %>% 
  head()
```

The Best-selling game in Japan was Grand Theft Auto: San Andreas, an action game.

#### TOP 6 best-selling games Globaly 
```{r bs Globaly}
vgsales %>% 
  select(Name, Global_Sales) %>% 
  group_by(Name) %>% 
  summarise(total_sales = sum(Global_Sales)) %>% 
  arrange(desc(total_sales)) %>% 
  head()

```

The Best-selling game in global was Wii Sports, a sports game.

*Observations: *

- Nintendo wii had the highest sales for their game Wii Sports.

- Sport games had a sale boom, that is noticeable in the top 6 sport games appear more than once


## <span style="color:#35F7bb"> Best Critically-acclaimed Games </span>
```{r critic}
vgscore %>% 
  select(Name, Genre, Critic_Score) %>% 
  distinct(Name, .keep_all = TRUE) %>% 
  arrange(desc(Critic_Score)) %>% 
  head()
```

*Observations: *

- The three most critically acclaimed games are of different genres.

- Grant theft Auto series had the best critic score in the top.

- Action, sports, and fighting games had the best critic score.

## <span style="color:#35F7bb"> Best Games by User Score </span>
```{r user_score}
vgscore_v2 %>% 
  select(Name, Genre, User_Score) %>% 
  distinct(Name, .keep_all = TRUE) %>% 
  arrange(desc(User_Score)) %>% 
  head()

```

*Observations: *

- The two most scored games are of different genres.

- Role-Playing and Sports are the predominant genres in the top.

## <span style="color:#35F7bb"> Best-selling Genres by Global Regions</span>
First, create a data table with the sales information summarized by genre. in order to make it understandable, it is necessary to calculate the percentage of each genre in total sales for each region. Then visualize the results.
```{r sales persent table}
col_p <- rainbow(12)
NA_sales_persent <- vgsales %>% 
  group_by(Genre) %>% 
  summarise(total_NA = sum(NA_Sales),
            total_EU = sum(EU_Sales),
            total_JP = sum(JP_Sales),
            total_other = sum(Other_Sales),
            total_global = sum(Global_Sales)) %>% 
  mutate(NA_div = total_NA/sum(total_NA)) %>% 
  mutate(NA_persent = scales::percent(NA_div)) %>% 
  mutate(EU_div = total_EU/sum(total_EU)) %>% 
  mutate(EU_persent = scales::percent(EU_div)) %>% 
  mutate(JP_div = total_JP/sum(total_JP)) %>% 
  mutate(JP_persent = scales::percent(JP_div)) %>% 
  mutate(other_div = total_other/sum(total_other)) %>% 
  mutate(other_persent = scales::percent(other_div)) %>% 
  mutate(global_div = total_global/sum(total_global)) %>% 
  mutate(global_persent = scales::percent(global_div))

ggplot(NA_sales_persent)+geom_col(aes(x=Genre, y=total_NA,fill="North America"), position = "dodge", width=0.9)+geom_col(aes(x=Genre, y=total_EU,fill="Total Europe"), width=0.7)+geom_col(aes(x=Genre, y=total_JP,fill="Total Japan"), width=0.5)+geom_col(aes(x=Genre, y=total_other,fill="Other Regions"), width=0.3)+labs(y="Sales")+theme(axis.text.x = element_text(angle = 25))
```


- North America has the most registered sales in all genres except in Role-Playing games, which japan has the most sales.

- Europe had the fewest sales in genres strategy, Role-Playing, and Puzzle.
```{r Best selling genregraph}


pie(NA_sales_persent$NA_div, clockwise = TRUE, labels = NA_sales_persent$NA_persent, col=col_p, main="Best-selling genre in North America") 
legend("left",legend = NA_sales_persent$Genre, fill = col_p,bty = "n")
pie(NA_sales_persent$EU_div, clockwise = TRUE, labels = NA_sales_persent$EU_persent, col=col_p, main="Best-selling genre in Europe")
legend("left",legend = NA_sales_persent$Genre, fill = col_p,bty = "n")
pie(NA_sales_persent$JP_div, clockwise = TRUE, labels = NA_sales_persent$JP_persent, col=col_p, main="Best-selling genre in Japan") 
legend("left",legend = NA_sales_persent$Genre, fill = col_p,bty = "n")
pie(NA_sales_persent$other_div, clockwise = TRUE, labels = NA_sales_persent$other_persent, col=col_p, main="Best-selling genre in Other Regions")
legend("left",legend = NA_sales_persent$Genre, fill = col_p,bty = "n")
pie(NA_sales_persent$global_div, clockwise = TRUE, labels = NA_sales_persent$global_persent, col=col_p, main="Best-selling genre in Global")
legend("left",legend = NA_sales_persent$Genre, fill = col_p,bty = "n")
```

*Observations: *

- Best-selling genre in North America is Action

- Best-selling genre in Europe is Action

- Best-selling genre in Japan is Role-playing.

- Best-selling genre in Other Regions is Action.

- Global Best-selling genre is Action.

## <span style="color:#35F7bb"> Genres with most User Score</span>

```{r Genres with most User Score, warning=FALSE, error=FALSE, message=FALSE}
vgscore_v2$User_Score <- as.numeric(vgscore_v2$User_Score)
vgscore_v2 %>% 
  select(Name, Genre, User_Score) %>% 
  group_by(Name) %>% 
  summarise(mean_score_user = mean(User_Score), Genre) %>% 
  distinct(Name, .keep_all = TRUE) %>% 
  group_by(Genre) %>% 
  summarise(mean_Critic_score = mean(mean_score_user)) %>% 
  ggplot(aes(x=mean_Critic_score,y=Genre, fill = mean_Critic_score))+geom_col(position = "dodge")+labs(y="Mean User Score")+theme_dark()+theme(axis.text.x = element_text(angle = 25))+coord_cartesian(xlim = c(6,8))+scale_fill_gradient(low = "green", high = "red")
```

*Observations:*

- Most user scored game genre is Role-playing.

## <span style="color:#35F7bb"> Genres with better Critics</span>

```{r Genres with better Critics, warning=FALSE, error=FALSE, message=FALSE}
vgscore_v2 %>% 
  select(Name, Genre, Critic_Score) %>% 
  group_by(Name) %>% 
  summarise(mean_score_critic = mean(Critic_Score), Genre) %>% 
  distinct(Name, .keep_all = TRUE) %>% 
  group_by(Genre) %>% 
  summarise(mean_Critic_score = mean(mean_score_critic)) %>% 
  ggplot(aes(x=mean_Critic_score,y=Genre, fill = mean_Critic_score))+geom_col(position = "dodge")+labs(y="Mean Critic Score")+scale_fill_gradient(low = "green", high = "red")+coord_cartesian(xlim = c(60,75))+theme_dark()
  

```

*Observations:*

- Most critic scored game genre is Strategy.

## <span style="color:#35F7bb"> Correlation between Critic score and User score</span>

```{r Critic score and User score, warning=FALSE}
vgscore_v2 %>% 
  ggplot(aes(x=User_Score, y=Critic_Score))+geom_point(position = "dodge")+geom_smooth()+labs(title = "Correlation between Critic score and User score")

```

*Observations:*

- Positive correlation between Scores.

- User rates less than 5.0 present a inconsistent correlation with critic score

## <span style="color:#35F7bb"> Correlation between Critic score and Global sales</span>
For this graph I'll take the games with 10 million or less in sales and see how does critics and global sales are related.
```{r Critic score and Global sales, warning=FALSE, error=FALSE, message=FALSE}
vgscore_v2 %>% 
  ggplot(aes(x=Critic_Score, y=Global_Sales))+geom_point(position = "dodge")+geom_smooth()+labs(title = "Correlation between Critic score and Global Sales")+coord_cartesian(ylim = c(0,10))
```

*Observations:*

- Positive correlation between games with good Critic and good sales.

## <span style="color:#35F7bb"> Correlation between User score and Global sales</span>

```{r User score and Global sales, warning=FALSE, error=FALSE, message=FALSE}
vgscore_v2 %>% 
  ggplot(aes(x=User_Score, y=Global_Sales))+geom_point(position = "dodge")+geom_smooth()+labs(title = "Correlation between User score and Global Sales")+coord_cartesian(ylim = c(0,10))
```

*Observations:*

- Positive correlation between good user rated games and good games.

- Even though there is a positive correlation it is not that sloped, meaning that the correlation is not that positive as global sales and critic score.


## <span style="color:#35F7bb"> Publisher with most sales </span>
In this graph will take the publishers with more than 20 million on sales, then it will sum the total sales by each one and then the results will be [resented in a bar graph.
```{r Publisher with most sales}
vgscore_v2 %>% 
  group_by(Publisher) %>% 
  summarise(total_sales_millions = sum(Global_Sales)) %>% 
  filter(total_sales_millions > 20) %>% 
  #arrange(desc(total_sales)) %>% 
  ggplot(aes(x=total_sales_millions, y=Publisher, fill=total_sales_millions))+geom_col(position = "stack")+geom_text(aes(label=total_sales_millions, vjust=0.5))+theme(axis.text.x.bottom = element_text(angle = 90))+scale_fill_gradient(low = "green", high = "red")
```

*Observations:*

- As expected, Electronic arts and Nintendo had most sales with 885.5 millions in sales.

# Exploratory Analysis

## <span style="color:#35F7bb"> What game-genre was the most published by the best-selling Publisher </span>
Now lets discover which was the most published game genre by the Best selling publisher.
```{r most published by the best-selling Publisher}
psales<-vgscore_v2 %>% 
  filter(Publisher == "Electronic Arts") %>% 
  select(Genre) %>% 
  group_by(Genre) %>% 
  summarise(total=n()) %>% 
  mutate(p_n = total/sum(total)) %>% 
  mutate(percent = scales::percent(p_n))
  
pie(as.numeric(psales$p_n), clockwise = TRUE, labels = psales$percent, col = col_p, main ="Most published game-genre by the best-selling Publisher" )
legend("left",legend = psales$Genre, fill = col_p,bty = "n")

```

*Observations: *

- The most published genre was Sports.

## <span style="color:#35F7bb"> Best-selling game Rating </span>

```{r rating sales}
vgscore_v2 %>% 
  select(Rating, Global_Sales) %>% 
  group_by(Rating) %>% 
  summarise(total_Sales = sum(Global_Sales)) %>%
  ggplot(aes(x=total_Sales, y=Rating, fill=total_Sales))+geom_col(position = "dodge")+scale_fill_gradient(low = "green", high = "red")+labs(title = "Best-selling game Rating")
```

- T (Teen): Games rated for teen players.

- RP (RATING PENDING): Titles listed as RP (Rating Pending) have been submitted to the ESRB and are awaiting final rating. 

- M (MATURE): Titles rated M (Mature) have content that may be suitable for persons ages 17 and older. 

- K-A (Kids to Adult): Have content that may be suitable for persons ages six and older.

- E10+ (EVERYONE 10+): Titles rated E10+ (Everyone 10+) have content that may be suitable for ages 10 and older.

- E (EVERYONE): Titles rated E (Everyone) have content that may be suitable for persons ages 6 and older.

- AO (ADULTS ONLY): Titles rated AO (Adults Only) have content that should only be played by persons 18 years and older.

As expected, the most selling rating was E (Everyone) since the most selling game was Wii sports, which is a E rated game, and the second most selling genre was Sports, a mostly E rated genre.

# Conclusions

- The amount of sales of a game does not determine the criticism of this, nor does it ensure a good reception by users.

- The fact that a game has a boom in sales does not guarantee an increase in sales of that genre of games.

- The most selling rating was E (for Everyone), which means that the wider the age range, the greater the number of sales.

- The best-selling Genres in North America: Action, sport and shooter. In Europe: Action, Sports, and shooter. in Japan: Role-Playing, Action, and Sports.

- By number of total sales, the most global lucrative game genre is  Action.

- Most user scored game genre is Role-Playing. Most critic scored game genre is sports.

- Critic score has more influence in global sales than user score.

- Publisher with most sales: Electronic Arts, which 41.8% of published games were sport games.

- Critic has correlation with user scores.

- Europe trends in game sales and genre are similar to North America's trends.

# Recommendations

When developing a video game, it is necessary to take into account which market is going to be a priority.

- If the game is going to be published with an objective in the American market, it must include action scenes, include gameplay with active interaction and variability of decisions, which influence the main story of the game.

- The three best critic rated games are designed to be in third person camera view. Its recommendable an action game with third person view.

- Include strategy in the gameplay, this will increase good critic score.

The party in charge of marketing is the publisher. Nevertheless, as an independent game development company it is necessary to make clear and effective marketing recommendations:

- For marketing in North America and Europe use a more action-oriented game cover.

- For marketing in japan use a more cartoony game cover.
