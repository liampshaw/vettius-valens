# Makes the ../figures for the associated paper.

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
