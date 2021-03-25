#references 1-https://github.com/cosmoduende/r-youtube-personal-history-analysis/blob/master/main.R
#2-http://www.danimadrid.net/coding/scraping_youtube_without_api.html
#3-https://www.storybench.org/how-to-download-youtube-data-in-r-using-tuber-and-purrr/
library(SentimentAnalysis)

#INteract with Youtube API
library(stringr)
library(rvest) 
library(tidyverse) 
library(jsonlite) 
library(tidytext)
library(lubridate) 
library(wordcloud)
library(httr)
library(ggplot2)
library(wordcloud2)
library(RCurl)
library(curl)
library(pbapply)
library(ggthemes)

#https://developers.google.com/youtube/v3/getting-started
#key= AIzaSyCIbzRxoTgnbfd4soC6mjLb3SEGqSST9O4


############################################
#Get all the playlists in a channel
#Hebbars Kitchen
library(tuber)
library(magrittr) # Pipes %>%, %T>% and equals(), extract().
library(tidyverse) # all tidyverse packages
library(purrr) # package for iterating/extracting data
#Every day this creates a .httr.oauth file
#Delete it and run it to authenticate
#when running this, I gave #1 save local
#and will need to delete every day.else give #2.
#check the reference links to see how to get the below values
client_id <- "XX"
client_sec <- "XX"
yt_oauth(client_id, client_sec,token='')

s_text="Paneer Butter Masala"
res <- yt_search(term=s_text)


#res <- head(res,50)
#print("iam here 1")

#https://cran.r-project.org/web/packages/tuber/vignettes/tuber-ex.html
my_df<- data.frame(vid=character(),
                   viewcount=integer(),
                   likecount=integer(),
                   dislikecount=integer(),
                   favouritecount=integer(),
                   commentcount=integer())

my_df["title"] <- as.character()
my_df["description"] <- as.character()
my_df["publishTime"] <- as.character()
my_df["channelTitle"] <- as.character()
#my_df["thumbnails.high.url"] <- as.character()
my_df["link"] <- as.character()
my_df["overall_positive_score"]<- as.integer()
my_df["overall_negative_score"]<- as.integer()
my_df["overall_neutral_score"]<- as.integer()
my_df["viewcount"]<- as.integer()
my_df["likecount"]<- as.integer()
my_df["dislikecount"]<- as.integer()

#print("after my_df")


# url = paste(URL_base, allids[[i]], URL_details, URL_key, sep = "")  
#nrow(res)

for (i in 1:nrow(res)) {
  #For each video, get the rating,view count
  # print(paste(i,"in loop"))
  
  v_id <-  res[i,"video_id"]
  video_stats <- get_stats(video_id=v_id)
  
  my_df[i,1:6] <- data.frame(matrix(unlist(video_stats), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
  
  my_df[i,"title"] <- res[i,"title"]  
  my_df[i,"description"] <- res[i,"description"]  
  my_df[i,"channelId"] <- res[i,"channelId"]  
  my_df[i,"publishTime"] <- res[i,"publishTime"]  
  my_df[i,"channelTitle"] <- res[i,"channelTitle"]
  #my_df[i,"thumbnails.high.url"] <- res[i,"thumbnails.high.url"]
  my_df[i,"link"] <-paste("https://www.youtube.com/watch?v=",v_id,sep="")
  
}
#Convert thumbnail to vid links
#https://www.youtube.com/watch?v=pjJgCXx_FRk


#convert to numeric
my_df <- transform(my_df, viewcount = as.numeric(viewcount), 
                   likecount = as.numeric(likecount),
                   dislikecount=as.numeric(dislikecount))
#Order desc by viewcount,likecount, asc dislikecount
my_df <-my_df[order(-my_df[,2],-my_df[,3],my_df[,4]),]
#my_df <- my_df[,-c(8,9,10)]
# 
# v_id<-  res[1,"video_id"]
# video_stats <- get_stats(video_id=v_id)
# 
# my_df[1,] <- data.frame(matrix(unlist(video_stats), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
# 
# video_stats
#View(my_df)
# 
# #nrow(res)
# for ( i in 1:nrow(my_df)) {
#   v_id <- my_df[i,"vid"]
#   my_df[i,"link"] <-paste("https://www.youtube.com/watch?v=",v_id,sep="")
#   
#   
#   
# }

#save this in a file 
setDT(my_df)
saveRDS(my_df, "data/dt.RDS")




my_yt_search <- function(s_text="Panner Butter Masala") {
  
  #res <- yt_search("Panner Butter Masala")
  
  if(length(s_text)> 0 )
{ 
  #  print("iam here")
    res <- yt_search(s_text)
  
  #head(res[, 1:3])
  #View(res)
  #nrow(res)
  res <- head(res,20)
  #print("iam here 1")
  
    #https://cran.r-project.org/web/packages/tuber/vignettes/tuber-ex.html
  my_df<- data.frame(vid=character(),
                     viewcount=integer(),
                     likecount=integer(),
                     dislikecount=integer(),
                     favouritecount=integer(),
                     commentcount=integer())
  
  my_df["title"] <- as.character()
  my_df["description"] <- as.character()
  my_df["publishTime"] <- as.character()
  my_df["channelTitle"] <- as.character()
  my_df["link"] <- as.character()
  my_df["overall_positive_score"]<- as.integer()
  my_df["overall_negative_score"]<- as.integer()
  my_df["overall_neutral_score"]<- as.integer()
  my_df["viewcount"]<- as.integer()
  my_df["likecount"]<- as.integer()
  my_df["dislikecount"]<- as.integer()
  
  #print("after my_df")
  
  
  # url = paste(URL_base, allids[[i]], URL_details, URL_key, sep = "")  
  
  for (i in 1:nrow(res)) {
    #For each video, get the rating,view count
   # print(paste(i,"in loop"))
    
      v_id<-  res[i,"video_id"]
    video_stats <- get_stats(video_id=v_id)
    
    my_df[i,1:6] <- data.frame(matrix(unlist(video_stats), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
    
    my_df[i,"title"] <- res[i,"title"]  
    my_df[i,"description"] <- res[i,"description"]  
    my_df[i,"channelId"] <- res[i,"channelId"]  
    my_df[i,"publishTime"] <- res[i,"publishTime"]  
    my_df[i,"channelTitle"] <- res[i,"channelTitle"]
#    my_df[i,"thumbnails.high.url"] <- res[i,"thumbnails.high.url"]
    my_df[i,"link"] <-paste("https://www.youtube.com/watch?v=",v_id,sep="")
    
    
  }
  #convert to numeric
  my_df <- transform(my_df, viewcount = as.numeric(viewcount), 
                     likecount = as.numeric(likecount),
                     dislikecount=as.numeric(dislikecount))
  #Order desc by viewcount,likecount, asc dislikecount
  my_df <-my_df[order(-my_df[,2],-my_df[,3],my_df[,4]),]
  #my_df <- my_df[,-c(8,9,10)]
  # 
  # v_id<-  res[1,"video_id"]
  # video_stats <- get_stats(video_id=v_id)
  # 
  # my_df[1,] <- data.frame(matrix(unlist(video_stats), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
  # 
  # video_stats
  #View(my_df)
  
  #nrow(res)
  
  
  } #end if 
  my_df
} #end function my_yt_search


#ENd of Program
#Comment analysis of each video in my_df
#nrow(my_df)
#Comment analysis for top 10 and not nrow(my_df)

 #3/17/2021 commenting comment analysis for this
#  
#  for ( i in 1 : 10){
#   print(paste(i,Sys.time()))
#   
#   #my_df[1,]
# #For first video
# comments <- get_all_comments(my_df[i,"vid"])
# #head(comments)
# #View(comments)
# #setwd("~/R/Mini Projects/Youtube Videos Search Ranking")
# #write.csv(comments,"video1_comments.csv")
# 
# #Pick only the text column
# v<- comments[[2]] #2259 rows
# #Put all of them into 1 row with , separated
# a<- paste(v,collapse=",") #convert vector to string
# #separae the 1 sentence to individuals 2659 
# a <- unlist(strsplit(a,",")) #convert to list
# sentiment <- analyzeSentiment(a)
# #str(sentiment)
# #View(sentiment)
# my_comments <- cbind(a,sentiment)
# #View(my_comments)
# c<-convertToDirection(my_comments$SentimentQDAP)
# #View(a)
# #View(c)
# 
# neu <- length(grep("neutral",c))
# pos <- length(grep("positive",c))
# neg <- length(grep("negative",c))
# #sl <- c(nec,pos,neg)
# #l <- c("neutral","positive","negative")
# #pie(sl,labels=l,main="Pie chart of first video comment analysis")
# my_df[i,"overall_positive_score"]<- round(pos/(neu+pos+neg)* 100, 2)
# my_df[i,"overall_negative_score"]<- round(neg/(neu+pos+neg)* 100, 2)
# my_df[i,"overall_neutral_score"]<- round(neu/(neu+pos+neg)* 100, 2)
# 
# #1001+1421+186
# }
 
 
 
#https://cran.r-project.org/web/packages/SentimentAnalysis/vignettes/SentimentAnalysis.html
##############33

