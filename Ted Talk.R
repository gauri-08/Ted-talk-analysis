library(data.table)
library(reshape2)
library(plotly)
library(dplyr)
library(jsonlite)
library(stringr)
library(wordcloud)
library(DT)
library(lubridate)
library(tidytext)
library(tidyverse)

ted_data=ted_main
dim(ted_data)
## Checking for missing values in the table
grep("[NA]",ted_data)

table(is.na(ted_data))

datatable(ted_data,extensions = 'Buttons', options = list(dom = 'Bfrtip', buttons = I('colvis')))

######################## Data Preparation ############################################
# keeping the variables to be used in the analysis
ted1_data <- ted_data[c("comments","duration","languages","main_speaker","num_speaker","published_date","ratings","speaker_occupation","tags","views","title")]

#checking the colnames and dimeantions of the new table
colnames(ted1_data)

dim(ted1_data)

sum(is.na(ted1_data))

#####  Checking outliers ###
ggplot(aes(x = "",y = comments),data = ted1_data) +
  geom_boxplot() +
  scale_y_log10(labels = scales::comma)+
  labs(title = "Number of Comments") +
  theme_minimal()

ggplot(aes(x = "",y = views),data = ted1_data) + 
  geom_boxplot() +
  scale_y_log10(labels = scales::comma) +
  labs(title = "Number of Views") +
  theme_minimal()

par(mfrow = c(1,3))
hist(ted1_data$num_speaker)
boxplot(ted1_data$languages, main = "No of languages")
boxplot(ted1_data$duration,main = "Duration (seconds)")


#4.Converting the Published date to a normal date format and creating Month and Year column to be used later in the analysis
ted1_data$published_date <- as.Date(as.character(ymd_hms(as.POSIXct(as.numeric(ted1_data$published_date),origin = '1970-01-01', tz = "GMT"))),format = "%Y-%m-%d")
ted1_data$published_month <- factor(month.abb[month(ted1_data$published_date)])
ted1_data$published_year <- year(ted1_data$published_date)


#5. Adding a ‘serial number’ column in the datset
tt <- ted1_data %>% summarise(serial_no = n())
ted1_data$serial_no <- seq(1,as.numeric(tt))
view(ted1_data)
##6. Cleaning the Ratings column and transforming it to number of positive, Negative and Neutral ratings

#reading json values to get the values in rows
df1 <- c()
for (i in 1:2550)
{
  df <- fromJSON(str_replace_all(ted1_data$rating[i],"'",'"'))
  df$serial_no<- i 
  df1 <- rbind(df,df1)
}
#Creating a table with the ratings
ted1_ratings <- df1

view(ted1_ratings)


#To check the distinct rating
df %>% distinct(name)

#Classified rating as positive, negative and neutral ratings
neg_words <- c('Unconvincing','Confusing','Obnoxious','Longwinded')
pos_words <- c('Informative','Inspiring','Fascinating','Ingenious','Beautiful','Persuasive','Jaw-dropping','Courageous','Funny')

df1$ratings_type <- ifelse(df1$name %in% unlist(neg_words),'negative_ratings',ifelse(df1$name %in% unlist(pos_words),'positive_ratings',ifelse(df1$name == 'OK','neutral_ratings',' ')))

ted2_data <- df1 %>% group_by(serial_no,ratings_type) %>% 
  summarise(count_rating_type = sum(count)) %>% spread(ratings_type,count_rating_type) %>% ungroup() %>%
  inner_join(ted1_data,by = "serial_no")


#### Removing unnecesary things from speaker occupation

ted1_data$speaker_occupation[1:5]

#replacing all the ;,/ to blanks
ted2_data$speaker_occupation <- ted2_data$speaker_occupation %>% str_replace_all('/',' ') %>% str_replace_all(',',' ')   %>% str_replace_all(';',' ') %>% str_replace_all('\\+',' ') %>% tolower()

ted2_data$speaker_occupation


#Unnesting each occupation
dt <- unnest_tokens(ted2_data,occupation1,speaker_occupation) %>% select(serial_no,occupation1)

stp_words <-  c('and','of','in','expert','social','the','for')


dt <- dt %>% subset(!occupation1 %in% stop_words) %>% mutate(occupation1 = str_replace_all(occupation1, 
                                                                                             c("writer" = "author","scientists" = "scientist","researcher" = "scientist","neuroscientist" = "scientist", "professor" = "educator", "scholar" = "educator", "education" = "educator", "teacher" = "educator", "songauthor" = "author","editor" = "author","data" = "data related","analyst" = "data related","statistician" = "data related", "musician" = "artist","singer" = "artist","sing" = "artist","poet" = "artist","actor" = "artist", "comedian" = "artist","playwright" = "artist","media" = "artist","performance" = "artist","guitarist" = "artist", "dancer" = " artist","humorist" = "artist","pianist" = "artist", "violinist" = "artist","magician" = "artist","artists" = "artist","band" = "artist", "director" = "filmmaker", "producer" = "filmmaker", "entrepreneur" = "business","ceo" = "business", "founder" = "business", "psychology" = "psychologist", "physician" = "health", "medical" = "health", "doctor" = "health", "design" = "designer", "designerer" = "designer", "reporter" = "journalist"))) 
### list of top 20 words
occupation_rank <- dt %>% group_by(occupation1) %>% summarise(n = n_distinct(serial_no)) %>% arrange(desc(n))
top_20_occ <- occupation_rank[1:20,1]
datatable(head(occupation_rank,20))


ted3_data <- dt %>%  mutate(rank = ifelse(occupation1 %in% unlist(top_20_occ),1,0)) %>% arrange(serial_no,desc(rank)) %>%
  subset(!duplicated(serial_no)) %>% right_join(ted2_data,by = "serial_no") %>% 
  mutate(speaker_occupation = ifelse(is.na(occupation1),"others",occupation1)) %>% 
  select(-(occupation1))


## Tag's field cleaning


ted3_data$tags <- ted3_data$tags %>% str_replace_all('\\[','') %>% str_replace_all('\\]','')   %>% str_replace_all("\\'",' ') %>% str_replace_all(',',' ') %>% tolower()

talk_tags <- unnest_tokens(ted3_data,tags1,tags) %>% select(serial_no,tags1)
datatable(head(talk_tags,10))

## Final Dataset

final_data <- ted3_data %>%
  select(c("serial_no","main_speaker","title","num_speaker","comments","positive_ratings","negative_ratings","neutral_ratings","duration","languages","speaker_occupation","views","published_month","published_year","published_date")) %>%
  mutate(ratings = positive_ratings + negative_ratings + neutral_ratings)




final_data %>%
  group_by(published_year) %>%
  summarise(n = n()) %>%
  ggplot(aes(x = factor(published_year),y = n,group = 1)) + 
  geom_line(color = "Blue") + geom_point(lwd = 2, color = "blue") + 
  labs(title = "No. of talks over the years", x = "Published Year", y = "no.of Talks") +
  geom_hline(aes(yintercept = mean(n)), linetype = "dashed", alpha = .5) +
  annotate("text", x = '2007', y = 210, label = "Average: 212.5", size = 3) +
  theme_minimal()


final_data %>%
  group_by(published_year) %>%
  summarise(avg_views = mean(views/100000)) %>% 
  ggplot(aes(x = factor(published_year),y = avg_views,group = 1)) +
  geom_line(color = "red") +
  geom_point(lwd = 2, color = "red") + 
  labs(title = "Views by Published Year", x = "Published Year" , y = "Average no. of views(in hundred thousands)") +
  geom_hline(aes(yintercept = mean(avg_views)), linetype = "dashed", alpha = .5) +
  annotate("text", x = '2007', y = 19, label = "Average: 1,838,604", size = 3) +
  annotate("text", x = '2007', y = 42, label = "Max: 4,130,967", size = 3) + 
  theme_minimal()

#Chart for avg no.of comments

final_data %>% 
  mutate(published_year1 = as.factor(published_year)) %>%
  group_by(published_year1) %>%
  summarise(avg_comments = mean(comments)) %>%
  ggplot(aes(x = published_year1, y = avg_comments)) + 
  geom_point(col = "tomato2", size = 3) +   
  geom_segment(aes(x = published_year1,xend = published_year1,y = min(avg_comments),yend = max(avg_comments)),linetype = "dashed",size = 0.05) +
  coord_flip() + 
  labs(title = "Number of Comments by Published Year", x = "Published year", y = "Average no. of Comments") +
  theme_minimal()

#
final_data %>% 
  mutate(published_year1 = as.factor(published_year)) %>%
  group_by(published_year1) %>%
  summarise(avg_ratings = mean(ratings)) %>%
  ggplot(aes(x = published_year1, y = avg_ratings)) + 
  geom_point(col = "tomato2", size = 3) +   
  geom_segment(aes(x = published_year1,xend = published_year1,y = min(avg_ratings),yend = max(avg_ratings)),linetype = "dashed",size = 0.05) +
  coord_flip() + 
  labs(title = "Number of Comments by Published Year", x = "Published year", y = "Average no of Ratings")+
  theme_minimal()


# bar chart showing % of positive, neutral and negative ratings by the published year
final_data %>%
  group_by(published_year) %>%
  summarise(Perc_Positive_Ratings= sum(positive_ratings)/sum(ratings), Perc_Negative_Ratings = sum(negative_ratings)/sum(ratings), Perc_Neutral_Ratings = sum(neutral_ratings)/sum(ratings)) %>%
  gather(Type, Perc_rating ,-published_year) %>%
  ggplot(aes(x = published_year, y = Perc_rating, fill = Type)) + geom_bar(stat = "identity") +
  labs(title = "% of Positive, Negative & Neutral Ratings by Published Year", x = "Published year", y = "% of Ratings") +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal()

#getting the top 10 talks of all times by number of views
datatable(final_data %>%
            arrange(desc(views)) %>%
            select( title, main_speaker, views, published_date,comments,ratings) %>%
            head(10))

final_data$view_category <- 
  ifelse(between(final_data$views,quantile(final_data$views,0),quantile(final_data$views,0.20)),'worst_talk',
         ifelse(between(final_data$views,quantile(final_data$views,0.20),quantile(final_data$views,0.40)),'bad_talk',
                ifelse(between(final_data$views,quantile(final_data$views,0.40),quantile(final_data$views,0.60)),'okay', 
                       ifelse(between(final_data$views,quantile(final_data$views,0.60),quantile(final_data$views,0.80)),'good_talk',
                              ifelse(final_data$views > quantile(final_data$views,0.80),'best_talk','NA')))))

#adding levels
view_order<- c('best_talk','good_talk','okay','bad_talk','worst_talk')
final_data$view_category <- factor(final_data$view_category, levels = view_order)

category <- final_data %>%
  group_by(view_category) %>%
  summarise(Min_Views = min(views),Max_Views = max(views)) %>%
  arrange(desc(Min_Views))

datatable(category)


### What makes best ted talk?
# Do publishing month affect TED talk

month<- c('Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec')
final_data$published_month <- factor(final_data$published_month, levels = month)

#Plotting Views by published month 
final_data %>%
  filter(published_year >= 2010) %>%
  group_by(published_year,published_month) %>%
  summarise(monthly_views = sum(views)) %>%
  inner_join(final_data %>%
               filter(published_year >= 2010) %>%
               group_by(published_year) %>%
               summarise(yearly_views = sum(views)),by = "published_year") %>%
  mutate(perc_views = monthly_views/yearly_views) %>%
  ggplot(aes(x = published_month,y = perc_views,group = 1, color = published_year)) +
  geom_point() + geom_line() + facet_wrap(~published_year,ncol = 1) + 
  scale_y_continuous(labels = scales::percent) +
  labs(x = "Published Month", y = "% contribution in that year", title = "2010-2017 monthly percentage views - Seasonality") +
  theme_minimal()


#creating a function to create a wordcloud and the frequency chart by the view category used in the function call

generate_cloud_grph <- function(v_cat){
  df_wc <- as.data.frame(final_data %>% 
                           subset(view_category == v_cat,select = c(speaker_occupation,view_category)) %>% 
                           count(speaker_occupation, sort = TRUE))
  
  wordcloud(words = df_wc$speaker_occupation, freq = df_wc$n, min.freq = 1,
            max.words = 100, random.order = FALSE, rot.per = 0.35, 
            colors = brewer.pal(8, "Dark2"))
  
  final_data %>%
    filter(view_category == v_cat) %>%
    group_by(speaker_occupation) %>%
    summarise(n = n()) %>% 
    arrange(desc(n)) %>% 
    head(10) %>%
    ggplot(aes(x = reorder(speaker_occupation,n), y = n, label = n)) + 
    geom_point(size = 6) + 
    geom_segment(aes(x = speaker_occupation, 
                     xend = speaker_occupation, 
                     y = 0, 
                     yend = n)) + 
    geom_text(color = "white", size = 3) + coord_flip() +
    labs(x = "Frequency",y = "Speaker Occupation") +
    theme_classic()
}


generate_cloud_grph("best_talk")

generate_cloud_grph("worst_talk")


set.seed(1234)
final_data %>% select(speaker_occupation,view_category) %>% 
  subset(view_category %in% c('best_talk','worst_talk')) %>%
  group_by(speaker_occupation,view_category) %>%
  summarise(n = n()) %>% 
  acast(speaker_occupation ~ view_category, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("#F8766D", "#00BFC4"),
                   max.words = 100)


final_data %>%
  plot_ly(y = ~duration, color = ~view_category, type = "box")

cor(final_data$views,final_data$duration)



####
#creating a variable to club  more than one speaker as co-speaker talks
datatable(final_data%>% 
            mutate(No_of_Speakers = ifelse(num_speaker == 1 , '1','>1')) %>%
            group_by(No_of_Speakers) %>%
            summarise(count = n()))



final_data %>% 
  mutate(No_of_Speakers = ifelse(num_speaker == 1 , '1','>1')) %>%
  ggplot(aes(x = No_of_Speakers, y = views, fill = No_of_Speakers)) + 
  geom_boxplot() +
  scale_y_log10(labels = scales::comma) +
  theme_minimal()


#Creating a boxplot of number of languages by view catgory
final_data %>%
  ggplot(aes(x = view_category, y = languages)) + 
  geom_boxplot(width = 0.3, fill = "plum") + coord_flip() +
  labs( x = "Views by category", y = "no of Languages", title = "Languages vs View_category") +
  theme_minimal()

cor(final_data$views,final_data$languages)



#Creating a boxplot for no. of languages 
final_data %>%
  ggplot(aes(x = view_category, y = languages)) + 
  geom_boxplot(width = 0.3, fill = "plum") + coord_flip() +
  labs( x = "View Category", y = "no of Languages", title = "Languages vs View_category") +
  theme_minimal()

cor(final_data$views,final_data$languages)



# cloud for tags 

set.seed(1234)
talk_tags %>%
  inner_join(final_data, by = "serial_no") %>%
  select(view_category, tags1) %>%
  filter(!(tags1 %in% c('global','tedx'))) %>%
  subset(view_category %in% c('best_talk','worst_talk')) %>%
  group_by(tags1,view_category) %>%
  summarise(n = n())  %>%
  acast(tags1 ~ view_category, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("#F8766D", "#00BFC4"), max.words = 100)


view(final_data)
















