singles.sentiments <- valens.database[which(valens.database$TYPE=="single"),]

# Get in useful format
singles.sentiments.df <- singles.sentiments %>% group_by(PLANETS, SENTIMENT) %>%
  summarise(count=n()) %>%
  mutate(total=sum(count),
         prop=count/total)
# remove 'total' variable
singles.sentiments.df$total <- NULL
# Make sure list is complete (some sentiments are missing where none listed in input data)
full_data <- expand.grid(PLANETS=singles.sentiments.df$PLANETS, SENTIMENT=singles.sentiments.df$SENTIMENT)
singles.sentiments.df <- unique(left_join(tbl_df(full_data),singles.sentiments.df))
singles.sentiments.df$count[is.na(singles.sentiments.df$count)] <- 0
singles.sentiments.df$prop[is.na(singles.sentiments.df$prop)] <- 0
# Order planets and sentiments
singles.sentiments.df$PLANETS <- ordered(singles.sentiments.df$PLANETS, 
                                        levels=ORDER_OF_BODIES)
singles.sentiments.df$SENTIMENT <- ordered(singles.sentiments.df$SENTIMENT,
                                           levels=c("bad", "neutral", "good"))
# Overall sentiment index
singles.overall.sentiments <- singles.sentiments.df %>% group_by(PLANETS) %>% 
  summarise(sentiment=prop[which(SENTIMENT=="good")]-prop[which(SENTIMENT=="bad")])

singles.overall.sentiments$length <- sapply(gsub(" ", "-", singles.overall.sentiments$PLANETS), function(x) getLengthPassage(x))

# Write to file
write.csv(singles.overall.sentiments, file='../intermediate-files/singles.csv', row.names = F)

# Table of sentiments
single.lengths <- sapply(ORDER_OF_BODIES, function(x) getLengthPassage(x))
names(single.lengths) <- ORDER_OF_BODIES
single.sentiment.tab <- singles.sentiments.df %>% select(SENTIMENT, count, PLANETS) %>% pivot_wider(names_from=SENTIMENT, values_from=count) 
single.sentiment.tab$PLANET <- ordered(single.sentiment.tab$PLANETS,
                                       levels=ORDER_OF_BODIES)
single.sentiment.tab <- single.sentiment.tab[order(single.sentiment.tab$PLANETS, decreasing = FALSE),]
single.sentiment.tab$Greek.description.length <- single.lengths[single.sentiment.tab$PLANETS]
single.sentiment.tab$good.bad.difference <- single.sentiment.tab$good -single.sentiment.tab$bad 
single.sentiment.tab$sum <- single.sentiment.tab$good +single.sentiment.tab$bad + single.sentiment.tab$neutral

single.sentiment.tab$sentiment <- paste0(single.sentiment.tab$good.bad.difference, "/", single.sentiment.tab$sum, " (", myround(single.sentiment.tab$good.bad.difference/single.sentiment.tab$sum, 2), ")")
single.sentiment.tab <- single.sentiment.tab[,c("PLANET", "Greek.description.length", "good","neutral", "bad",  "sum", "sentiment")]
colnames(single.sentiment.tab) <- c("Planet", "No. of words (Greek)", "p", "m", "n", "Total terms", "Sentiment")
xtable(single.sentiment.tab)
