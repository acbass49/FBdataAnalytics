install.packages("rjson")
install.packages("data.table")
library(rjson)
library(data.table)
library(tidyverse)
library(lubridate)

#------------------------------------
#Comments data
#converting JSON to dataframe
#---------------------------------#

a <- as.data.frame(rjson::fromJSON(file = "files/comments/comments.json"))

#deleting the group variables
b <- a[,!(grepl("group", names(a)))]

#renaming columns to ease iteration
list_to_rename <- names(b)[!grepl("\\.[0-9]+" ,names(b))]
for (x in list_to_rename) {
  b[[paste0(x,".0")]] <- b[[x]]
  b[[x]] <- NULL
}

#creating list
binding_list <- lapply(sprintf(".+\\.%s$", 0:103), function(x){
  c <- b[,names(b)[grepl(x ,names(b))]]
  names(c) <- gsub("\\.[0-9]+", "",names(c))
  c[,order(names(c))]
  })

df <- data.table::rbindlist(binding_list, fill = T)

df$commentsOn <- gsub("Alex Bass replied to |Alex Bass commented on |'s.+","",df$comments.title)
df$commentsOn <- gsub("his own post.|Alex Bass replied to his own comment.|his own photo.|his own comment.|his own GIF.", "My post", df$commentsOn)
df$date <- as_date(as_datetime(df$comments.timestamp))

df %>% 
  count(commentsOn) %>% 
  arrange(-n)

comments <- df
df <- NULL

comments$type <- "comment"

comments <- comments[,-c(1,3,4,6)]

friends$friends.name %in% comments$comments.data.comment.comment

a <- sapply(friends$friends.name, 
            function(x)grep(x,comments$comments.data.comment.comment, value = T))

b <- split.data.frame(comments,1:nrow(comments))

c <- lapply(b, function(x)sapply(friends$friends.name,
                                 function(e)grepl(e,x$comments.data.comment.comment, 
                                                  ignore.case = T)))

unlist(sapply(c, function(x)names(x)[x]))


#------------------------------------
#reactions data
#---------------------------------#

b <- as.data.frame(rjson::fromJSON(file = "files/likes_and_reactions/posts_and_comments.json"))

list_to_rename <- names(b)[!grepl("\\.[0-9]+" ,names(b))]
for (x in list_to_rename) {
  b[[paste0(x,".0")]] <- b[[x]]
  b[[x]] <- NULL
}

binding_list <- lapply(sprintf(".+\\.%s$", 0:695), function(x){
  c <- b[,names(b)[grepl(x ,names(b))]]
  names(c) <- gsub("\\.[0-9]+", "",names(c))
  c[,order(names(c))]
})

df2 <- data.table::rbindlist(binding_list, fill = T)

df2$reactsTo <- gsub("Alex Bass reacted to |Alex Bass liked |Alex Bass likes |'s.+","",df2$reactions.title)
df2$reactsTo <- gsub("his own post.|Alex Bass replied to his own comment.|his own photo.|his own comment.|his own GIF.", "My post", df2$reactsTo)
df2$reactsTo <- gsub("a post.", "Unspecified", df2$reactsTo)

df2$date <- as_date(as_datetime(df2$reactions.timestamp))

df2 %>% 
  count(reactsTo) %>% 
  arrange(-n) %>% 
  head(20)

df2 %>% 
  count(reactions.data.reaction.reaction) %>% 
  arrange(-n)

reactions <- df2
df2 <- NULL

reactions$type <- "reaction"

reactions <- reactions[,-c("reactions.data.reaction.actor","reactions.timestamp")]


#------------------------------------
#friends data
#---------------------------------#

a <- as.data.frame(rjson::fromJSON(file = "files/friends/friends.json"))

#deleting the group variables
b <- a[,!(grepl("group", names(a)))]

#renaming columns to ease iteration
list_to_rename <- names(b)[!grepl("\\.[0-9]+" ,names(b))]
for (x in list_to_rename) {
  b[[paste0(x,".0")]] <- b[[x]]
  b[[x]] <- NULL
}

#creating list
binding_list <- lapply(sprintf(".+\\.%s$", 0:549), function(x){
  c <- b[,names(b)[grepl(x ,names(b))]]
  names(c) <- gsub("\\.[0-9]+", "",names(c))
  c[,order(names(c))]
})

friends <- data.table::rbindlist(binding_list, fill = T)

friends$type <- "friend"

friends$date <- as_date(as_datetime(friends$friends.timestamp))

#------------------------------------
#Posts data
#---------------------------------#
a <- rjson::fromJSON(file = "files/posts/your_posts_1.json")

a <- lapply(a, function(x){
  tryCatch(as.data.frame(x), 
           error = function(e)
           lapply(x, as.data.frame))
  
})

for (x in 1:length(a)) {
  if(!is.data.frame(a[[x]])&!is.null(a[[x]][["data"]][1,1])){
    a[[x]] <- as.data.frame(a[[x]]["data"])
  }
  if(!is.data.frame(a[[x]])&!is.null(a[[x]][["attachments"]][1,1])){
    a[[x]] <- as.data.frame(a[[x]]["attachments"])
  }
}

posts <- rbindlist(a, fill = T)

a <- posts[,grepl("timestamp",names(posts)),with = F]

b <- split.data.frame(a,1:nrow(a))

posts$date <- as.numeric(sapply(b,function(x)grep("[0-9]+",x, value = T)[1]))

posts <- posts[!is.na(date)]



a <- posts[,grepl("post|description",names(posts)),with = F]

b <- split.data.frame(a,1:nrow(a))

posts$posts <- sapply(b,function(x)grep(".+",x, value = T)[1])

posts <- posts[!is.na(posts)]

posts$date <- as_date(as_datetime(posts$date))

posts$type <- "post"

posts <- posts[,c("posts","date","type")]

#------------------------------------
#Combining data
#---------------------------------#
full_dataset <- rbindlist(list(comments,posts,friends,reactions),fill = T)


#------------------------------------
#Initial Analysis
#---------------------------------#
#fonts
library(showtext)
font_add_google("Cairo", family = "Cairo")
showtext_auto()

#Facebook Usage Over Time
full_dataset %>% 
  mutate(year = year(date)) %>% 
  count(type, year) %>% 
  ggplot(aes(year,n))+
  geom_point(aes(color = type))+
  geom_line(aes(color = type))+
  theme_minimal()+
  theme(axis.ticks = element_blank(),
        axis.text.y = element_text(size = 14, hjust = 1.1),
        panel.background = element_blank(),
        panel.grid.minor.x = element_blank(),
        text = element_text(face = "bold", family = "Cairo"),
        plot.title = element_text(hjust = .5, size = 20, face = "bold"),
        legend.title = element_blank(),
        plot.caption = element_text(hjust = 1.43, size = 8, face = "italic"))+
  labs(caption = "Source: My Personal Data Downloaded From Facebook",
       title = "My Facebook Usage Over Time")+
  scale_x_continuous(breaks = c(2008:2021))+
  xlab("Year")+
  ylab("Count")+
  ggsave("graphOutput/trends.png", device = "png", width = 18, height = 10, units = "cm")




b <- full_dataset %>% 
  filter(type=="reaction", date>(Sys.Date()-lubridate::years(1)))%>% 
  count(reactions.data.reaction.reaction) %>% 
  arrange(-n) %>% 
  as.data.frame()

if(length(b$reactions.data.reaction.reaction)<7){
  a <- c("SORRY","WOW","HAHA","LOVE","LIKE","CARE","ANGRY") 
  c <- data.frame(reactions.data.reaction.reaction = a[!(a %in% b[,1])],
             n = rep(0,length(a[!(a %in% b[,1])])))
  b <- rbind(b,c)
}

b <- b %>% arrange(reactions.data.reaction.reaction)
b$image <- sort(paste0("files/images/", list.files("files/images")))

fav_reaction_data <- b

library(ggimage)
font_add_google("Cairo", family = "Cairo")
showtext_auto()
fav_reaction_data %>% 
  mutate(reaction = reorder(factor(reactions.data.reaction.reaction),n,sum)) %>% 
  ggplot()+
  geom_bar(aes(reaction,n),stat="identity", fill = "skyblue3", width = .8)+
  geom_image(aes(reaction,-15 ,image = image), size = .05)+
  coord_flip()+
  theme_minimal()+
  theme(panel.grid.major.y = element_blank(),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = 14),
        axis.title.x = element_text(size = 16),
        text = element_text(face = "bold", family = "Cairo"),
        plot.caption = element_text(size = 8, face = "italic"),
        plot.title = element_text(hjust = .5, size = 20, face = "bold"))+
  ylab("Count")+
  xlab("Reaction")+
  labs(caption = "Source: My Personal Data Downloaded From Facebook",
       title = "My Favorite Reactions")+
  ggsave("graphOutput/emoji.png", device = "png", width = 15, height = 13, units = "cm")

#Figure with inner and outer circle of friends
#Top 3 friends


#make list of friends
#match names in these two table of list of friends
#sort by recency of reactions / or weight by recency
#try and calculate a recency score in these tables

#I should search comments and posts for friends names as a third table

list_of_friends <- as_vector(full_dataset %>% 
  filter(type == "friend") %>% 
  select(friends.name))
names(list_of_friends) <- NULL



in_post_interactions <- sapply(list_of_friends, function(x){
  comment <- sum(grepl(x,full_dataset$comments.data.comment.comment))
  post <- sum(grepl(x,full_dataset$posts))
  sum(comment,post)
})

in_post_interactions <- sort(in_post_interactions[in_post_interactions>0], decreasing = T)

in_post_interactions <- data.frame(friend_name = names(in_post_interactions),
                                   n = in_post_interactions) %>% head(10)

row.names(in_post_interactions) <- NULL

full_dataset$date_count <- 1/log(as.numeric(Sys.Date() - full_dataset$date))

comments_interactions <- full_dataset %>% 
  filter(type == "comment") %>% 
  count(commentsOn, wt=date_count) %>% 
  arrange(-n) %>% 
  rename(friend_name = commentsOn)

reactions_interactions <- full_dataset %>% 
  filter(type == "reaction") %>% 
  count(reactsTo, wt=date_count) %>% 
  arrange(-n) %>% 
  rename(friend_name = reactsTo)

in_post_interactions %>% 
  full_join(comments_interactions, by = "friend_name") %>% 
  full_join(reactions_interactions, by = "friend_name") %>% 
  mutate(count = rowSums(.[,2:4],na.rm =T)) %>% 
  arrange(-count) %>% 
  filter(friend_name %in% list_of_friends) %>% 
  select(friend_name, count)

#for the word cloud
library(wordcloud)
library(RColorBrewer)
wc <- in_post_interactions %>% 
  full_join(comments_interactions, by = "friend_name") %>% 
  full_join(reactions_interactions, by = "friend_name") %>% 
  mutate(count = round((rowSums(.[,2:4],na.rm =T))*5)) %>% 
  arrange(-count) %>% 
  filter(friend_name %in% list_of_friends) %>% 
  select(friend_name, count) %>% 
  head(100)

#save this manually as a png
wordcloud(words = wc$friend_name, freq = wc$count, min.freq = 1,
          max.words=100, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"), scale = c(1.6,.3))

#Exploring Sentiment Analysis
library(SentimentAnalysis)

sent <- analyzeSentiment(full_dataset$posts)

sent <- sent[!is.na(sent$SentimentGI),]

#start of figure
library(scales)
data.frame(a=factor(convertToDirection(sent$SentimentQDAP))) %>% 
  mutate(b = "b") %>% 
  ggplot(aes(x = b, fill = a, label = a))+
  geom_bar(position = "fill", width = .7) +
  scale_fill_manual(values = c("brown3", "grey87", "seagreen4") )+
  coord_flip()+
  theme_minimal()+
  theme(panel.grid.major.y = element_blank(),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = 14),
        axis.title.x = element_text(size = 16),
        text = element_text(face = "bold", family = "Cairo"),
        plot.caption = element_text(size = 8, face = "italic"),
        plot.title = element_text(hjust = .5, size = 20, face = "bold"),
        legend.title = element_blank())+
  labs(caption = "Source: My Personal Data Downloaded From Facebook",
       title = "More Of My Comments/Posts Are Positive")+
  scale_y_continuous(labels = scales::percent)+
  ylab("Percent")+
  ggsave("graphOutput/sentiment.png", device = "png", width = 18, height = 8, units = "cm")

font_add_google("Cairo", family = "Cairo")
showtext_auto()
fav_reaction_data %>% 
  mutate(reaction = reorder(factor(reactions.data.reaction.reaction),n,sum)) %>% 
  ggplot()+
  geom_bar(aes(reaction,n),stat="identity", fill = "skyblue3", width = .8)+
  geom_image(aes(reaction,-15 ,image = image), size = .05)+
  coord_flip()+
  theme_minimal()+
  theme(panel.grid.major.y = element_blank(),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = 14),
        axis.title.x = element_text(size = 16),
        text = element_text(face = "bold", family = "Cairo"),
        plot.caption = element_text(size = 8, face = "italic"),
        plot.title = element_text(hjust = .5, size = 20, face = "bold"))+
  ylab("Count")+
  xlab("Reaction")+
  labs(caption = "Source: My Personal Data Downloaded From Facebook",
       title = "My Favorite Reactions")+
  ggsave("graphOutput/emoji.png", device = "png", width = 10, height = 10, units = "cm")


  
#numbers for proportion lables
data.frame(a=convertToDirection(sent$SentimentQDAP)) %>% 
  mutate(b = "b") %>% 
  count(a) %>% 
  mutate(prop = n/sum(n))

#Guess top 3 or 5 friends / 5 more notable mentions

#Give a favorite page?

#How do I create a friend score?
#Reactions + Comments + Posts

#Goal: Build a monetized app that gives people analytics of their FB data

## PIPELINE PROCESS:
#Download data
#Connect account to get friends profile pictures
#Create image using ggplot of friends circle

## PREMIUM FEATURES:
#It would be awesome if I had...
#FB emojis you use the most
#Figure with inner and outer circle of friends
#Sentiment analysis of posts
#List of people you sent friend requests but didn't accept

#List of best facebook friends - word cloud
#Activity on FB over time
#Friends over time
#Sentiment analysis of facebook posts (words)
##Are you posting happy, sad, excited, scared, unsure?
#Most used emoji

#need to make sure to visualize this in exciting ways

#I should create a master dataset with friends, comments and interactions
