doubles.sentiments <- valens.database[which(valens.database$TYPE=="double"),]


# Get in useful format
doubles.sentiments.df <- doubles.sentiments %>% group_by(PLANETS, SENTIMENT) %>%
  summarise(count=n()) %>%
  mutate(total=sum(count),
         prop=count/total)
# remove 'total' variable
doubles.sentiments.df$total <- NULL
# Make sure list is complete (some sentiments are missing where none listed in input data)
full_data <- expand.grid(PLANETS=doubles.sentiments.df$PLANETS, SENTIMENT=doubles.sentiments.df$SENTIMENT)
doubles.sentiments.df <- unique(left_join(tbl_df(full_data),doubles.sentiments.df))
doubles.sentiments.df$count[is.na(doubles.sentiments.df$count)] <- 0
doubles.sentiments.df$prop[is.na(doubles.sentiments.df$prop)] <- 0


# Order planets and sentiments
doubles.sentiments.df$body.1 <- sapply(doubles.sentiments.df$PLANETS, 
                                       function(x) stringr::str_split(x, pattern=" ")[[1]][1])
#doubles.sentiments.df$body.1 <- ordered(ORDER_OF_BODIES[doubles.sentiments.df$body.1], 
#                                        levels=ORDER_OF_BODIES)
doubles.sentiments.df$body.2 <- sapply(doubles.sentiments.df$PLANETS, 
                                       function(x) stringr::str_split(x, pattern=" ")[[1]][2])
#doubles.sentiments.df$body.2 <- ordered(ORDER_OF_BODIES[doubles.sentiments.df$body.2],
#                                        levels=ORDER_OF_BODIES)
doubles.sentiments.df$bodies.sorted <- sapply(1:nrow(doubles.sentiments.df),
                                              function(x)
                                                paste(as.character(sort(unlist(c(doubles.sentiments.df[x,"body.1"], 
                                                                                 doubles.sentiments.df[x,"body.2"])))),
                                                      collapse=" "))
# Make body 1 and 2 be in order of bodies as expected
#doubles.sentiments.df$body.1 <- gsub(" .*", "", doubles.sentiments.df$bodies.sorted)
#doubles.sentiments.df$body.2 <- gsub(".* ", "", doubles.sentiments.df$bodies.sorted)
#doubles.sentiments.df$body.1 <- ordered(doubles.sentiments.df$body.1,
#                                        levels=ORDER_OF_BODIES)
#doubles.sentiments.df$body.2 <- ordered(doubles.sentiments.df$body.2,
#                                        levels=ORDER_OF_BODIES)
doubles.sentiments.df$order.body.string <- paste0(as.numeric(doubles.sentiments.df$body.1),
                                                  as.numeric(doubles.sentiments.df$body.2))
# Order bodies again (this is hacky but it works)
#doubles.sentiments.df$bodies.sorted <- ordered(doubles.sentiments.df$bodies.sorted,
#                                               levels=unique(doubles.sentiments.df$bodies.sorted[order(doubles.sentiments.df$order.body.string)]))

doubles.sentiments.df$SENTIMENT <- ordered(doubles.sentiments.df$SENTIMENT,
                                           levels=c("bad", "good"))

# Also make a dataframe with an overall sentiment index
doubles.overall.sentiments <- doubles.sentiments.df %>% group_by(body.1, body.2, bodies.sorted, order.body.string) %>% 
  summarise(sentiment=prop[which(SENTIMENT=="good")]-prop[which(SENTIMENT=="bad")])
# Add mean single sentiment
doubles.overall.sentiments$mean.single.sentiment <- sapply(doubles.overall.sentiments$bodies.sorted, function(x) getSingleScores(x))
# Dummy variables
doubles.overall.sentiments$Saturn <- ifelse(grepl("Saturn", doubles.overall.sentiments$bodies.sorted), 1, 0)
doubles.overall.sentiments$Jupiter <- ifelse(grepl("Jupiter", doubles.overall.sentiments$bodies.sorted), 1, 0)
doubles.overall.sentiments$Mars <- ifelse(grepl("Mars", doubles.overall.sentiments$bodies.sorted), 1, 0)
doubles.overall.sentiments$Sun <- ifelse(grepl("Sun", doubles.overall.sentiments$bodies.sorted), 1, 0)
doubles.overall.sentiments$Venus <- ifelse(grepl("Venus", doubles.overall.sentiments$bodies.sorted), 1, 0)
doubles.overall.sentiments$Mercury <- ifelse(grepl("Mercury", doubles.overall.sentiments$bodies.sorted), 1, 0)
doubles.overall.sentiments$Moon <- ifelse(grepl("Moon", doubles.overall.sentiments$bodies.sorted), 1, 0)

doubles.overall.sentiments$length <- sapply(gsub(" ", "-", doubles.overall.sentiments$bodies.sorted), function(x) getLengthPassage(x))

write.csv(doubles.overall.sentiments, '../intermediate-files/doubles.csv', row.names = F)

# Table
# Table of sentiments
double.sentiment.tab <- doubles.sentiments.df %>% select(SENTIMENT, count, bodies.sorted) %>% pivot_wider(names_from=SENTIMENT, values_from=count) 
double.sentiment.tab$bodies.sorted <- ordered(double.sentiment.tab$bodies.sorted,
                                       levels=levels(doubles.overall.sentiments$bodies.sorted))
double.sentiment.tab <- double.sentiment.tab[order(double.sentiment.tab$bodies.sorted, decreasing = FALSE),]
double.sentiment.tab$Greek.description.length <- doubles.overall.sentiments$length
double.sentiment.tab$good.bad.difference <- double.sentiment.tab$good -double.sentiment.tab$bad 
double.sentiment.tab$sum <- double.sentiment.tab$good +double.sentiment.tab$bad # No neutral 

double.sentiment.tab$sentiment <- paste0(double.sentiment.tab$good.bad.difference, "/", double.sentiment.tab$sum, " (", myround(double.sentiment.tab$good.bad.difference/double.sentiment.tab$sum, 2), ")")
double.sentiment.tab <- double.sentiment.tab[,c("bodies.sorted", "Greek.description.length", "good", "bad",  "sum", "sentiment")]
colnames(double.sentiment.tab) <- c("Combination", "No. of words (Greek)", "p", "n", "Total terms", "Sentiment")
double.sentiment.tab$`No. of words (Greek)` <- myround(double.sentiment.tab$`No. of words (Greek)`, 1)
xtable(double.sentiment.tab)

