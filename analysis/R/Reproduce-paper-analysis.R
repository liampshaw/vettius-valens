# Makes the ../figures for the associated paper.

library(ggplot2)
library(dplyr)
library(cowplot)
library(tidyr)
library(broman)
library(xtable)

# Functions
source('functions.R')

# Generate the tables of sentiments. 
# Saves outputs from raw input data into analysis/intermediate-files
# Read in main database
valens.database <- read.csv('../../Valens-database.csv', header=T, stringsAsFactors = F)
# SINGLE SENTIMENTS
source('prepare-single-sentiments.R')
# DOUBLE SENTIMENTS
source('prepare-double-sentiments.R')
# TRIPLES SENTIMENTS
source('triples-sentiments-load.R')


# Figure 1 and 2
double.occurrences <- read.csv('../../data/0-CE-200-CE-double-occurrence-per-year.csv', header=F,stringsAsFactors = F)
colnames(double.occurrences) <- c("year", "double", "occurrence")
double.occurrences.averages <- double.occurrences %>% group_by(double) %>%
  summarise(median=median(occurrence), mean=mean(occurrence))
double.occurrences.averages$bodies.sorted <- gsub("-", " ", double.occurrences.averages$double)
double.occurrences.averages$bodies.sorted  <- ordered(double.occurrences.averages$bodies.sorted,
                                                      levels=levels(doubles.sentiments.df$bodies.sorted))
plot.double.occurrence.df <- right_join(double.occurrences.averages, doubles.overall.sentiments, by="bodies.sorted")
# Make plot
doubles.occurrences.plot <- ggplot(plot.double.occurrence.df, aes(mean, sentiment))+
  geom_point()+
  theme_basic()+ylim(c(-1,1))+
  ylab("Overall sentiment of conjunction")+xlab("Mean occurrence of conjunction (% of Z-codes 1-200 CE)")+
  ggrepel::geom_text_repel(aes(label=bodies.sorted), family="Times", size=2)+
  geom_hline(linetype='dashed', yintercept = 0)

triple.occurrences <- read.csv('../../data/0-CE-200-CE-triple-occurrence-per-year.csv', header=F,stringsAsFactors = F)
colnames(triple.occurrences) <- c("year", "triple", "occurrence")
triple.occurrences.averages <- triple.occurrences %>% group_by(triple) %>%
  summarise(median=median(occurrence), mean=mean(occurrence))
triple.occurrences.averages$bodies.sorted <- gsub("-", " ", triple.occurrences.averages$triple)
triple.occurrences.averages$bodies.sorted  <- ordered(triple.occurrences.averages$bodies.sorted,
                                                      levels=levels(triples.sentiments.df$bodies.sorted))
plot.triple.occurrence.df <- right_join(triple.occurrences.averages, triples.overall.sentiments, by="bodies.sorted")
# Make plot
plot.double.triple.occurrence.df <- rbind(plot.double.occurrence.df[,c("bodies.sorted", "mean", "sentiment")], plot.triple.occurrence.df[,c("bodies.sorted", "mean", "sentiment")])
plot.double.triple.occurrence.df$type <- c(rep("double", nrow(plot.double.occurrence.df)), 
                                           rep("triple", nrow(plot.triple.occurrence.df)))
occurrences.plot <- ggplot(plot.double.triple.occurrence.df, aes(mean, sentiment, colour=type))+
  geom_hline(linetype='dashed', yintercept = 0)+
  geom_point()+
  theme_basic()+ylim(c(-1,1))+
  ylab("Overall sentiment of conjunction")+xlab("Mean occurrence of conjunction (% of Z-codes 1-200 CE)")+
  ggrepel::geom_text_repel(aes(label=bodies.sorted), family="Times", size=2)+
  scale_color_manual(values=c("red", "black"))+
  theme(legend.position = c(0.85, 0.1), 
        legend.text=element_text(family="Times"),
        legend.title = element_blank())

# Make Figure 2
ggsave(occurrences.plot, file='../figures/Figure-2-occurrences-length.pdf', width = 6, height=6)

# Length of descriptions
plot.double.triple.occurrence.df$length <- sapply(gsub(" ", "-", plot.double.triple.occurrence.df$bodies.sorted), function(x) getLengthPassage(x))
length.occurrences.plot <-  ggplot(plot.double.triple.occurrence.df, aes(mean, length, colour=type))+
  geom_point()+
  theme_basic()+
  ylab("Number of words in description")+xlab("Mean occurrence of conjunction (% of Z-codes 1-200 CE)")+
  scale_y_continuous(breaks=seq(0, 125, 25), limits=c(0, 125))+
  ggrepel::geom_text_repel(aes(label=bodies.sorted), family="Times", size=2)+
  scale_color_manual(values=c("red", "black"))+
  theme(legend.position = c(0.85, 0.1), 
        legend.text=element_text(family="Times"),
        legend.title = element_blank())
# Add single lengths
single.lengths <- sapply(ORDER_OF_BODIES, function(x) getLengthPassage(x))
names(single.lengths) <- ORDER_OF_BODIES
lengths.df <- rbind(plot.double.triple.occurrence.df[,c("type", "length")], data.frame(type=rep("single", 7), length=single.lengths))
lengths.df$type <- ordered(lengths.df$type,
                           levels=c("single", "double", "triple"))
length.boxplot <- ggplot(lengths.df, aes(type, length, colour=type))+
  geom_boxplot(width=0.5, outlier.shape = NA)+
  geom_jitter(height=0, width=0.2)+
  theme_basic()+
  scale_color_manual(values=c("blue", "red","black"))+
  theme(legend.position = "none")+
  scale_y_continuous(breaks=seq(0, 275, 25), limits=c(0, 275))+
  ylab("Number of words in description")+
  xlab("")

length.plot <- cowplot::plot_grid(plotlist=list(length.boxplot, length.occurrences.plot),
                                  rel_widths = c(0.4, 0.6))
# Make Figure 1
ggsave(length.plot, file='../figures/Figure-1-lengths.pdf', width = 8, height=6)



# MODELLING
# Doubles: dummy variables
lm.doubles.dummy <- lm(data=doubles.overall.sentiments, sentiment ~ 0 + Saturn + Jupiter + Mars + Sun + Venus + Mercury + Moon) 
# Doubles: from mean of single sentiments
lm.doubles.mean <- lm(data=doubles.overall.sentiments, sentiment ~ 0 + mean.single.sentiment)
# Doubles: sum of singles
doubles.overall.sentiments$sum.single.sentiment <- sapply(doubles.overall.sentiments$bodies.sorted, function(x) sum(getSingleScores(x)))
lm.doubles.singles <- lm(data=doubles.overall.sentiments, sentiment ~ 0 + 1*sum.single.sentiment)
logLik()


AIC(lm.doubles.dummy, lm.doubles.mean)

# Triples: dummy variables
lm.triples.dummy <- lm(data=triples.overall.sentiments, sentiment ~ 0 + Saturn + Jupiter + Mars + Sun + Venus + Mercury + Moon)
# Triples: from mean of single sentiments
lm.triples.mean.single <- lm(data=triples.overall.sentiments, sentiment ~ 0 + mean.single.sentiment)
# Triples: from mean of double sentiments
lm.triples.mean.double <- lm(data=triples.overall.sentiments, sentiment ~ 0 + mean.double.sentiment)
# Triples: mean of (double + single) sentiments
lm.triples.mean.double.plus.single <- lm(data=triples.overall.sentiments, sentiment ~ 0 + mean.double.plus.single.sentiment)

# We prefer the minimum AIC
AIC(lm.triples.dummy, lm.triples.mean.single, lm.triples.mean.double, lm.triples.mean.double.plus.single)
BIC(lm.triples.dummy, lm.triples.mean.single, lm.triples.mean.double, lm.triples.mean.double.plus.single)

# All doubles and triples with dummy model
col.names <- c("Saturn", "Jupiter", "Mars", "Sun", "Venus", "Mercury", "Moon", "sentiment")
lm.doubles.and.triples.dummy <- lm(data=rbind(triples.overall.sentiments[,col.names], 
                                              doubles.overall.sentiments[,col.names]), sentiment ~ 0 + Saturn + Jupiter + Mars + Sun + Venus + Mercury + Moon)
# All doubles and triples with mean single sentiment model
lm.doubles.and.triples.mean.single <- lm(data=rbind(triples.overall.sentiments[,c("sentiment", "mean.single.sentiment")], 
                                                    doubles.overall.sentiments[,c("sentiment", "mean.single.sentiment")]), sentiment ~ 0 + mean.single.sentiment)

# Plot results
double.dummy.df <- data.frame(summary(lm.doubles.dummy)$coef)
double.dummy.df$min <- double.dummy.df$Estimate-double.dummy.df$Std..Error
double.dummy.df$max <- double.dummy.df$Estimate+double.dummy.df$Std..Error
double.dummy.df$body <- rownames(double.dummy.df)

triple.dummy.df <- data.frame(summary(lm.triples.dummy)$coef)
triple.dummy.df$min <- triple.dummy.df$Estimate-triple.dummy.df$Std..Error
triple.dummy.df$max <- triple.dummy.df$Estimate+triple.dummy.df$Std..Error
triple.dummy.df$body <- rownames(triple.dummy.df)

sentiment.df <- data.frame(body=names(coef(lm.doubles.dummy)),
                           double.model=coef(lm.doubles.dummy), 
                           triple.model=coef(lm.triples.dummy),
                           sentiment=singles.overall.sentiments$sentiment)
# Plot the results
dummy.df <- rbind(double.dummy.df, triple.dummy.df)
dummy.df$type <- c(rep("effect of body in double model", 7), rep("effect of body in triple model", 7))
actual.sentiment.df <- data.frame(body=singles.overall.sentiments$PLANET, 
                                  Estimate=singles.overall.sentiments$sentiment, 
                                  type="sentiment of individual body", min=0, max=0)
sentiment.effect.plot <- ggplot(dummy.df, aes(body, group=type, ymin=min, ymax=max, y=Estimate, colour=type))+
  geom_hline(yintercept = 0, linetype='dashed')+
  geom_point(data=actual.sentiment.df, aes(body, Estimate), size=5)+
  geom_point(position=position_dodge(width=0.2), size=2)+
  geom_errorbar(width=0, position=position_dodge(width=0.2), size=1)+
  theme_basic()+
  scale_colour_manual(values=c( "red", "black", "grey"))+
  xlab("")+
  ylab("Sentiment effect")+
  ylim(c(-1,1.1))+
  theme(legend.position = c(0.8, 0.2), 
        legend.text=element_text(family="Times", size=8),
        legend.key.size = unit(0.05, "in"),
        legend.title = element_blank())
ggsave(sentiment.effect.plot, file='../figures/Figure-3-sentiment-effect.pdf', width=6, height=4)





# Plot them together
# 
corr.label <- paste0("italic(r)==", myround(cor.test(doubles.overall.sentiments$mean.single.sentiment, doubles.overall.sentiments$sentiment, method="pearson")$estimate, 2))
p.double.single <- ggplot(doubles.overall.sentiments, aes(mean.single.sentiment, sentiment))+
  stat_smooth(method="lm", se=FALSE, colour="red")+
  geom_point(colour="red")+
  theme_basic()+
  ggrepel::geom_text_repel(aes(label=bodies.sorted), size=2, family='Times')+
  coord_fixed()+
  xlim(c(-1, 1))+
  ylim(c(-1,1))+
  geom_hline(yintercept = 0, linetype='dashed')+
  geom_vline(xintercept = 0, linetype='dashed')+
  xlab("Mean sentiment index of component planets")+
  ylab("Sentiment index of double")+
  annotate('text', -0.6, 0.8,
           label=corr.label, parse=TRUE, 
           hjust=1, size=5, family='Times')
ggsave(p.double.single, file='../figures/Figure-4-doubles-modelled-by-singles.pdf', width=5, height=6)

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
ggsave(p.together, file='../figures/Figure-5-triples-modelled.pdf', width=14, height=6)

## Appendix
source('functions.R')
rand <- read.csv('../python/random_aspects.csv', header=T, stringsAsFactors = F)
d <- read.csv('../python/aspects.csv', header=T, stringsAsFactors = F)

N <- 7
cumulativeTable <- function(vec){
  return(sapply(seq(0, N), function(x) ifelse(length(table(vec>x))==1, NA, table(vec>x)[2]/length(vec) * 100)))
}
actual.df <- data.frame(value=c(cumulativeTable(d$conjunction),
                   cumulativeTable(d$opposition),
                   cumulativeTable(d$trine),
                   cumulativeTable(d$quartile),
                   cumulativeTable(d$sextile)),
                   number=rep(seq(1,N+1), 5),
           type=c(rep("conjunction", N+1),
                  rep("opposition", N+1),
                  rep("trine", N+1),
                  rep("quartile", N+1),
                  rep("sextile", N+1)),
           observations="real")
random.df <- data.frame(value=c(cumulativeTable(rand$conjunction),
                                cumulativeTable(rand$opposition),
                                cumulativeTable(rand$trine),
                                cumulativeTable(rand$quartile),
                                cumulativeTable(rand$sextile)),
                        number=rep(seq(1,N+1), 5),
                        type=c(rep("conjunction", N+1),
                               rep("opposition", N+1),
                               rep("trine", N+1),
                               rep("quartile", N+1),
                               rep("sextile", N+1)),
                        observations="random")
merge.df <- rbind(actual.df, random.df)
library(ggplot2)
aspects.plot <- ggplot(merge.df, aes(number, value, group=observations, linetype=observations))+
  geom_line()+
  scale_colour_manual(values=c("dashed", "full"))+
  facet_wrap(~type, nrow=1)+
  theme_basic()+
  theme(legend.position = "none")+
  theme(strip.text = element_text(family="Times"))+
  xlab("Minimum number of aspectual relationships in Z-code")+
  ylab("Percentage of Z-codes")
ggsave(aspects.plot, file='../figures/Appendix-aspects.pdf', width=8, height=3)
