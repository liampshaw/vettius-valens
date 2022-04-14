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

# Makes version of Figure 5 with this stanza data

library(ggplot2)
library(dplyr)
library(cowplot)
library(tidyr)
library(broman)
library(xtable)

# Functions
source('functions.R')

# Just for triples 
merged.sentiments <- read.csv('../intermediate-files/stanza-sentiment-comparison.csv', header=T, stringsAsFactors = F, row.names = 1)

singles.overall.sentiments <- read.csv('../intermediate-files/singles.csv', header=T, stringsAsFactors = F)
doubles.overall.sentiments <- read.csv('../intermediate-files/doubles.csv', header=T, stringsAsFactors = F)
triples.overall.sentiments <- read.csv('../intermediate-files/triples.csv', header=T, stringsAsFactors = F)

singles.sentiments$sentiment <- merged.sentiments[singles.overall.sentiments$PLANET, "stanza.sentiment"]
doubles.overall.sentiments$sentiment <- merged.sentiments[doubles.overall.sentiments$bodies.sorted, "stanza.sentiment"]
triples.overall.sentiments$sentiment <- merged.sentiments[triples.overall.sentiments$bodies.sorted, "stanza.sentiment"]

triples.overall.sentiments$mean.double.sentiment <- sapply(triples.overall.sentiments$bodies.sorted, function(x) getDoubleScoresForTriple(x))
# Add mean single sentiment
triples.overall.sentiments$mean.single.sentiment <- sapply(triples.overall.sentiments$bodies.sorted, function(x) getSingleScores(x))
# Mean double+single sentiment
triples.overall.sentiments$mean.double.plus.single.sentiment <- sapply(triples.overall.sentiments$bodies.sorted, function(x) getDoubleSingleScoresForTriple(x))


corr.label <- paste0("italic(r)==", myround(cor.test(triples.overall.sentiments$mean.single.sentiment, triples.overall.sentiments$sentiment, method="pearson")$estimate, 2))
p.triple.single <- ggplot(triples.overall.sentiments, aes(mean.single.sentiment, sentiment))+
  stat_smooth(method="lm", se=FALSE, colour="red")+
  geom_point(colour="black")+
  theme_basic()+
  ggrepel::geom_text_repel(aes(label=bodies.sorted), size=2, family="Times")+
  coord_fixed()+
  xlim(c(-1, 1))+
  ylim(c(-1,1))+
  geom_hline(yintercept = 0, linetype='dashed')+
  geom_vline(xintercept = 0, linetype='dashed')+
  xlab("Predicted sentiment index")+
  ylab("Actual sentiment index")+
  annotate('text', -0.6, 0.8,
           label=corr.label, parse=TRUE, 
           hjust=1, size=5, family='Times')

corr.label <- paste0("italic(r)==", myround(cor.test(triples.overall.sentiments$mean.double.sentiment, triples.overall.sentiments$sentiment, method="pearson")$estimate, 2))
p.triple.double <- ggplot(triples.overall.sentiments, aes(mean.double.sentiment, sentiment))+
  stat_smooth(method="lm", se=FALSE, colour="red")+
  geom_point(colour="black")+
  theme_basic()+
  ggrepel::geom_text_repel(aes(label=bodies.sorted), size=2, family="Times")+
  coord_fixed()+
  xlim(c(-1, 1))+
  ylim(c(-1,1))+
  geom_hline(yintercept = 0, linetype='dashed')+
  geom_vline(xintercept = 0, linetype='dashed')+
  xlab("Predicted sentiment index")+
  ylab("Actual sentiment index")+
  annotate('text', -0.6, 0.8,
           label=corr.label, parse=TRUE, 
           hjust=1, size=5, family='Times')

corr.label <- paste0("italic(r)==", myround(cor.test(triples.overall.sentiments$mean.double.plus.single.sentiment, triples.overall.sentiments$sentiment, method="pearson")$estimate, 2))
p.triple.double.single <- ggplot(triples.overall.sentiments, aes(mean.double.plus.single.sentiment, sentiment))+
  stat_smooth(method="lm", se=FALSE, colour="red")+
  geom_point(colour="black")+
  theme_basic()+
  ggrepel::geom_text_repel(aes(label=bodies.sorted), size=2, family="Times")+
  coord_fixed()+
  xlim(c(-1, 1))+
  ylim(c(-1,1))+
  geom_hline(yintercept = 0, linetype='dashed')+
  geom_vline(xintercept = 0, linetype='dashed')+
  xlab("Predicted sentiment index")+
  ylab("Actual sentiment index")+
  annotate('text', -0.6, 0.8,
           label=corr.label, parse=TRUE, 
           hjust=1, size=5, family='Times')
p.together <- cowplot::plot_grid(p.triple.single+ggtitle("(a) Component singles"), p.triple.double+ggtitle("(b) Component doubles"),
                                 p.triple.double.single+ggtitle("(c) Component doubles and singles"), nrow=1)
ggsave(p.together, file='../figures/Appendix-Figure-5-triples-modelled-using-stanza.pdf', width=14, height=6)
