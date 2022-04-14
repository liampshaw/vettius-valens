triples.sentiments <- valens.database[which(valens.database$TYPE=="triple"),]


# Get in useful format
triples.sentiments.df <- triples.sentiments %>% group_by(PLANETS, SENTIMENT) %>%
  summarise(count=n()) %>%
  mutate(total=sum(count),
         prop=count/total)
# remove 'total' variable
triples.sentiments.df$total <- NULL
# Make sure list is complete (some sentiments are missing where none listed in input data)
full_data <- expand.grid(PLANETS=triples.sentiments.df$PLANETS, SENTIMENT=triples.sentiments.df$SENTIMENT)
triples.sentiments.df <- unique(left_join(tbl_df(full_data),triples.sentiments.df))
triples.sentiments.df$count[is.na(triples.sentiments.df$count)] <- 0
triples.sentiments.df$prop[is.na(triples.sentiments.df$prop)] <- 0


# Order planets and sentiments
triples.sentiments.df$body.1  <- sapply(triples.sentiments.df$PLANETS, 
       function(x) stringr::str_split(x, pattern=" ")[[1]][1])
triples.sentiments.df$body.2  <- sapply(triples.sentiments.df$PLANETS, 
                                        function(x) stringr::str_split(x, pattern=" ")[[1]][2])
triples.sentiments.df$body.3  <- sapply(triples.sentiments.df$PLANETS, 
                                        function(x) stringr::str_split(x, pattern=" ")[[1]][3])
#triples.sentiments.df$body.1 <- sapply(stringr::str_split(triples.sentiments.df$TRIPLE, pattern=" "),
#                                       function(x) x[1])
triples.sentiments.df$body.1 <- ordered(triples.sentiments.df$body.1, 
                                        levels=ORDER_OF_BODIES)
#triples.sentiments.df$body.2 <- sapply(stringr::str_split(triples.sentiments.df$TRIPLE, pattern=" "),
#                                       function(x) x[2])
triples.sentiments.df$body.2 <- ordered(triples.sentiments.df$body.2,
                                        levels=ORDER_OF_BODIES)
#triples.sentiments.df$body.3 <- sapply(stringr::str_split(triples.sentiments.df$TRIPLE, pattern=" "),
#                                       function(x) x[3])
triples.sentiments.df$body.3 <- ordered(triples.sentiments.df$body.3,
                                      levels=ORDER_OF_BODIES)
#triples.sentiments.df$bodies.sorted <- sapply(1:nrow(triples.sentiments.df),
#                                              function(x)
#                                                paste(as.character(sort(unlist(c(triples.sentiments.df[x,"body.1"], 
 #                                                                                triples.sentiments.df[x,"body.2"], triples.sentiments.df[x,"body.3"])))),
  #                                                    collapse=" "))
# Make body 1 and 2 be in order of bodies as expected
#triples.sentiments.df$body.1 <- sapply(stringr::str_split(triples.sentiments.df$bodies.sorted, pattern=" "),
#                                       function(x) x[1])
#triples.sentiments.df$body.2 <- sapply(stringr::str_split(triples.sentiments.df$bodies.sorted, pattern=" "),
#                                       function(x) x[2])
#triples.sentiments.df$body.3 <- sapply(stringr::str_split(triples.sentiments.df$bodies.sorted, pattern=" "),
#                                       function(x) x[3])
#triples.sentiments.df$body.1 <- ordered(triples.sentiments.df$body.1,
#                                        levels=ORDER_OF_BODIES)
#triples.sentiments.df$body.2 <- ordered(triples.sentiments.df$body.2,
#                                        levels=ORDER_OF_BODIES)
#triples.sentiments.df$body.3 <- ordered(triples.sentiments.df$body.3,
#                                        levels=ORDER_OF_BODIES)

triples.sentiments.df$order.body.string <- paste0(as.numeric(triples.sentiments.df$body.1),
                                                  as.numeric(triples.sentiments.df$body.2), 
                                                  as.numeric(triples.sentiments.df$body.3))
# Order bodies again (this is hacky but it works)
triples.sentiments.df$bodies.sorted <- ordered(triples.sentiments.df$PLANETS,
                                               levels=unique(triples.sentiments.df$PLANETS[order(triples.sentiments.df$order.body.string)]))

triples.sentiments.df$SENTIMENT <- ordered(triples.sentiments.df$SENTIMENT,
                                           levels=c("bad", "neutral", "good"))

# Also make a dataframe with an overall sentiment index
triples.overall.sentiments <- triples.sentiments.df %>% group_by(body.1, body.2,body.3, bodies.sorted, order.body.string) %>% 
  summarise(sentiment=prop[which(SENTIMENT=="good")]-prop[which(SENTIMENT=="bad")])
# Add mean double sentiment
triples.overall.sentiments$mean.double.sentiment <- sapply(triples.overall.sentiments$bodies.sorted, function(x) getDoubleScoresForTriple(x))
# Add mean single sentiment
triples.overall.sentiments$mean.single.sentiment <- sapply(triples.overall.sentiments$bodies.sorted, function(x) getSingleScores(x))
# Mean double+single sentiment
triples.overall.sentiments$mean.double.plus.single.sentiment <- sapply(triples.overall.sentiments$bodies.sorted, function(x) getDoubleSingleScoresForTriple(x))

# Dummy variables
triples.overall.sentiments$Saturn <- ifelse(grepl("Saturn", triples.overall.sentiments$bodies.sorted), 1, 0)
triples.overall.sentiments$Jupiter <- ifelse(grepl("Jupiter", triples.overall.sentiments$bodies.sorted), 1, 0)
triples.overall.sentiments$Mars <- ifelse(grepl("Mars", triples.overall.sentiments$bodies.sorted), 1, 0)
triples.overall.sentiments$Sun <- ifelse(grepl("Sun", triples.overall.sentiments$bodies.sorted), 1, 0)
triples.overall.sentiments$Venus <- ifelse(grepl("Venus", triples.overall.sentiments$bodies.sorted), 1, 0)
triples.overall.sentiments$Mercury <- ifelse(grepl("Mercury", triples.overall.sentiments$bodies.sorted), 1, 0)
triples.overall.sentiments$Moon <- ifelse(grepl("Moon", triples.overall.sentiments$bodies.sorted), 1, 0)


# Add lengths
triples.overall.sentiments$length <- sapply(gsub(" ", "-", triples.overall.sentiments$bodies.sorted), function(x) getLengthPassage(x))

write.csv(triples.overall.sentiments, '../intermediate-files/triples.csv', row.names = F)


# Table of sentiments
triple.sentiment.tab <- triples.sentiments.df %>% select(SENTIMENT, count, bodies.sorted) %>% pivot_wider(names_from=SENTIMENT, values_from=count) 
triple.sentiment.tab$bodies.sorted <- ordered(triple.sentiment.tab$bodies.sorted,
                                              levels=levels(triples.overall.sentiments$bodies.sorted))
triple.sentiment.tab <- triple.sentiment.tab[order(triple.sentiment.tab$bodies.sorted, decreasing = FALSE),]
triple.sentiment.tab$Greek.description.length <- triples.overall.sentiments$length
triple.sentiment.tab$good.bad.difference <- triple.sentiment.tab$good -triple.sentiment.tab$bad 
triple.sentiment.tab$sum <- triple.sentiment.tab$good +triple.sentiment.tab$bad +triple.sentiment.tab$neutral

triple.sentiment.tab$sentiment <- paste0(triple.sentiment.tab$good.bad.difference, "/", triple.sentiment.tab$sum, " (", myround(triple.sentiment.tab$good.bad.difference/triple.sentiment.tab$sum, 2), ")")
triple.sentiment.tab <- triple.sentiment.tab[,c("bodies.sorted", "Greek.description.length", "good", "neutral", "bad",  "sum", "sentiment")]
colnames(triple.sentiment.tab) <- c("Combination", "No. of words (Greek)", "p", "m", "n", "Total terms", "Sentiment")
print(xtable(triple.sentiment.tab), include.rownames = FALSE)
