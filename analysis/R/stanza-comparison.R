# Check assessed sentiments against those from Stanza

singles.overall.sentiments <- read.csv('../intermediate-files/singles.csv', header=T, stringsAsFactors = F)
singles.overall.sentiments$bodies.sorted <- singles.overall.sentiments$PLANET # just for consistency

doubles.overall.sentiments <- read.csv('../intermediate-files/doubles.csv', header=T, stringsAsFactors = F)
triples.overall.sentiments <- read.csv('../intermediate-files/triples.csv', header=T, stringsAsFactors = F)

merged.sentiments <- dplyr::bind_rows(singles.overall.sentiments[,c("bodies.sorted", "sentiment")], 
      doubles.overall.sentiments[,c("bodies.sorted", "sentiment")], 
      triples.overall.sentiments[,c("bodies.sorted", "sentiment")])

# Read in stanza sentiments
stanza.sentiments <- read.csv('../intermediate-files/stanza-sentiments.csv', header=F, stringsAsFactors = F, row.names = 1)

# Add to the merged sentiments
merged.sentiments$stanza.sentiment <- stanza.sentiments[merged.sentiments$bodies.sorted, "V3"]
# Add single/double/triple
merged.sentiments$combination <- sapply(merged.sentiments$bodies.sorted, 
                                        function(x) length(stringr::str_split(x, " ")[[1]]))

merged.sentiments$combination <- ifelse(merged.sentiments$combination==1, 
                                        "Single", 
                                        ifelse(merged.sentiments$combination==2, "Double", "Triple"))
merged.sentiments$combination <- ordered(merged.sentiments$combination, 
                                         levels=c("Single", "Double", "Triple"))
# WRite this to file
write.csv(merged.sentiments, file='../intermediate-files/stanza-sentiment-comparison.csv', row.names = F)

corr.label.single <- paste0("italic(r)==", myround(cor.test(merged.sentiments[which(merged.sentiments$combination=="Single"),"sentiment"], 
                                                     merged.sentiments[which(merged.sentiments$combination=="Single"),"stanza.sentiment"])$estimate, 2))
corr.label.double <- paste0("italic(r)==", myround(cor.test(merged.sentiments[which(merged.sentiments$combination=="Double"),"sentiment"], 
                                                            merged.sentiments[which(merged.sentiments$combination=="Double"),"stanza.sentiment"])$estimate, 2))
corr.label.triple <- paste0("italic(r)==", myround(cor.test(merged.sentiments[which(merged.sentiments$combination=="Triple"),"sentiment"], 
                                                            merged.sentiments[which(merged.sentiments$combination=="Triple"),"stanza.sentiment"])$estimate, 2))



p.single <- ggplot(merged.sentiments[which(merged.sentiments$combination=="Single"),], aes(sentiment, stanza.sentiment))+
  geom_point()+
  geom_abline(slope=1, intercept=0, linetype='dashed')+
  theme_basic()+
  coord_fixed()+
  ggrepel::geom_text_repel(aes(label=bodies.sorted), family="Times", size=2)+
  facet_wrap(~combination)+
  ylab("Sentiment (from Stanza)")+
  xlab("Sentiment (author-assessed)")+
  annotate('text', -0.4, 0.8,
           label=corr.label.single, parse=TRUE, 
           hjust=1, size=5, family='Times')+
  xlim(c(-1,1))+
  ylim(c(-1,1))
p.double <- ggplot(merged.sentiments[which(merged.sentiments$combination=="Double"),], aes(sentiment, stanza.sentiment))+
  geom_point()+
  geom_abline(slope=1, intercept=0, linetype='dashed')+
  theme_basic()+
  coord_fixed()+
  ggrepel::geom_text_repel(aes(label=bodies.sorted), family="Times", size=2)+
  facet_wrap(~combination)+
  ylab("Sentiment (from Stanza)")+
  xlab("Sentiment (author-assessed)")+
  annotate('text', -0.4, 0.8,
           label=corr.label.double, parse=TRUE, 
           hjust=1, size=5, family='Times')+
  xlim(c(-1,1))+
  ylim(c(-1,1))

p.triple <- ggplot(merged.sentiments[which(merged.sentiments$combination=="Triple"),], aes(sentiment, stanza.sentiment))+
  geom_point()+
  geom_abline(slope=1, intercept=0, linetype='dashed')+
  theme_basic()+
  coord_fixed()+
  ggrepel::geom_text_repel(aes(label=bodies.sorted), family="Times", size=2)+
  facet_wrap(~combination)+
  ylab("Sentiment (from Stanza)")+
  xlab("Sentiment (author-assessed)")+
  annotate('text', -0.4, 0.8,
           label=corr.label.triple, parse=TRUE, 
           hjust=1, size=5, family='Times')+
  xlim(c(-1,1))+
  ylim(c(-1,1))

pdf('../figures/Appendix-figure-correlation-with-Stanza.pdf', width=12, height=6)
cowplot::plot_grid(p.single, p.double, p.triple, nrow=1)
dev.off()