# 
library(ggplot2)
library(dplyr)
library(cowplot)
library(tidyr)
library(broman)

# Functions
source('functions.R')

# SINGLE SENTIMENTS
source('singles-sentiments-load.R')

# DOUBLE SENTIMENTS
source('doubles-sentiments-load.R')

# TRIPLES SENTIMENTS
source('triples-sentiments-load.R')

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
ggsave(sentiment.effect.plot, file='sentiment-effect.pdf', width=6, height=4)
ggsave(sentiment.effect.plot, file='sentiment-effect.png', width=6, height=5, unit="in", dpi=300)

ggplot(sentiment.df, aes(sentiment))+
  geom_point(aes(y=triple.model))+
  geom_point(aes(y=double.model), colour="red")+
  theme_basic()+
  ylab("Predicted effect")+
  ggrepel::geom_text_repel(aes(y=double.model, label=body))


# Lengths
ggplot(triples.overall.sentiments, aes(sentiment<0, length))+
  geom_boxplot()+
  theme_basic()
ggplot(doubles.overall.sentiments, aes(sentiment<0, length))+
  geom_boxplot()+
  theme_basic()

summary(lm(length ~ Saturn + Jupiter+ Mars + Sun + Venus + Mercury, data=triples.overall.sentiments))


summary(lm(length ~ Saturn + Jupiter + Mars + Sun + Venus + Mercury, data=doubles.overall.sentiments))

# Almost as if: when Saturn and Jupiter are in doubles, there is not much to say that has not already been said, because their influence dominates. 
# But there is much more to say about them when they are in triples, because there is more to modify.
# Give the 


triples.overall.sentiments$order.sum <- 1*triples.overall.sentiments$Saturn + 2*triples.overall.sentiments$Jupiter + 3*triples.overall.sentiments$Mars + 4*triples.overall.sentiments$Sun + 5*triples.overall.sentiments$Venus + 6*triples.overall.sentiments$Mercury + 7*triples.overall.sentiments$Moon
ggplot(triples.overall.sentiments, aes(order.sum, length))+
  geom_point()+
  stat_smooth(method="lm")+
  theme_basic()
# This is a nice relationship if it holds

# Does this exist for doubles?
doubles.overall.sentiments$order.sum <- 1*doubles.overall.sentiments$Saturn + 2*doubles.overall.sentiments$Jupiter + 3*doubles.overall.sentiments$Mars + 4*doubles.overall.sentiments$Sun + 5*doubles.overall.sentiments$Venus + 6*doubles.overall.sentiments$Mercury + 7*doubles.overall.sentiments$Moon
ggplot(doubles.overall.sentiments, aes(order.sum, length))+
  geom_point()+
  stat_smooth(method="lm")
# Seems in opposite direction, hmm
# Not very convincing that the relationship is different between the two
ggplot(doubles.overall.sentiments, aes(order.sum, length))+
  geom_point(colour="red")+stat_smooth(colour="red", se=FALSE, method="lm")+
  geom_point(data=triples.overall.sentiments)+stat_smooth(data=triples.overall.sentiments, colour="black", se=FALSE, method="lm")+theme_basic()+xlab("Importance metric (lower=more important)")+ylab("Length of description")

# 
cor.test(triples.overall.sentiments$order.sum, doubles.overall.sentiments$length)
# 
cor.test(doubles.overall.sentiments$order.sum, doubles.overall.sentiments$length)
# They don't overlap zero at least!

# change the order of bodies
triples.overall.sentiments$order.sum.new <- 1*triples.overall.sentiments$Saturn + 2*triples.overall.sentiments$Jupiter + 3*triples.overall.sentiments$Mars +  4*triples.overall.sentiments$Venus + 6*triples.overall.sentiments$Mercury + 6*triples.overall.sentiments$Sun +7*triples.overall.sentiments$Moon
doubles.overall.sentiments$order.sum.new <- 1*doubles.overall.sentiments$Saturn + 2*doubles.overall.sentiments$Jupiter + 3*doubles.overall.sentiments$Mars + 4*doubles.overall.sentiments$Venus + 5*doubles.overall.sentiments$Mercury + 6*doubles.overall.sentiments$Sun +7*doubles.overall.sentiments$Moon
# Make 'seniority' (rank)
doubles.overall.sentiments$seniority <- order(doubles.overall.sentiments$order.sum.new, decreasing =FALSE)
triples.overall.sentiments$seniority <- order(triples.overall.sentiments$order.sum.new, decreasing =FALSE)

p.double.seniority <- ggplot(doubles.overall.sentiments, aes(seniority, length))+
  stat_smooth(colour="red", method="lm")+
  geom_point(colour="red")+
  theme_basic()+
  ylim(c(0,125))+
  ylab("Length of description (Greek words)")+
  xlab("Seniority (1 = most senior)")
p.triple.seniority <- ggplot(triples.overall.sentiments, aes(seniority, length))+
  stat_smooth(colour="black", method="lm")+
  geom_point(colour="black")+
  theme_basic()+
  ylim(c(0,125))+
  ylab("Length of description (Greek words)")+
  xlab("Seniority (1 = most senior)")
p.seniority <- cowplot::plot_grid(p.double.seniority+ggtitle("(a)"), p.triple.seniority+ggtitle("(b)"))
ggsave(p.seniority, file='seniority-plot.pdf', width=8, height=6)

# Redo seniority without Sun and Moon
newOrder <- function(df){
  df$order.sum <- 1*df$Saturn + 2*df$Jupiter + 3*df$Mars + 4*df$Venus + 5*df$Mercury
  return(df)
}
doubles.overall.sentiments.no.luminaries <- newOrder(doubles.overall.sentiments[which(doubles.overall.sentiments$Sun==0 & doubles.overall.sentiments$Moon==0),])
doubles.overall.sentiments.no.sun <- newOrder(doubles.overall.sentiments[which(doubles.overall.sentiments$Sun==0 & doubles.overall.sentiments$Moon!=0),])
doubles.overall.sentiments.no.moon <- newOrder(doubles.overall.sentiments[which(doubles.overall.sentiments$Sun!=0 & doubles.overall.sentiments$Moon==0),])

ggplot(doubles.overall.sentiments.no.luminaries, aes(order.sum, length))+
  geom_point()
ggplot(doubles.overall.sentiments.no.sun, aes(order.sum, length))+
  geom_point()
ggplot(doubles.overall.sentiments.no.moon, aes(order.sum, length))+
  geom_point()


triples.overall.sentiments.no.luminaries <- newOrder(triples.overall.sentiments[which(triples.overall.sentiments$Sun==0 & triples.overall.sentiments$Moon==0),])
ggplot(triples.overall.sentiments.no.luminaries, aes(order.sum, length))+
  geom_point()

triples.overall.sentiments$order.sum.new <- 1
triples.overall.sentiments.no.luminaries <- triples.overall.sentiments[which(triples.overall.sentiments$Sun==0 & triples.overall.sentiments$Moon==0),]
doubles.overall.sentiments$order.sum.new <- 1*doubles.overall.sentiments$Sun + 2*doubles.overall.sentiments$Moon+3*doubles.overall.sentiments$Saturn + 4*doubles.overall.sentiments$Jupiter + 5*doubles.overall.sentiments$Mars + 6*doubles.overall.sentiments$Venus + 7*doubles.overall.sentiments$Mercury 
doubles.overall.sentiments$seniority <- order(doubles.overall.sentiments$order.sum.new, decreasing =FALSE)
ggplot(doubles.overall.sentiments, aes(seniority, length))+
  stat_smooth(colour="red", method="lm")+
  geom_point(colour="red")+
  theme_basic()+
  ylim(c(0,125))+
  ylab("Length of description (Greek words)")+
  xlab("Seniority (1 = most senior)")
triples.overall.sentiments$order.sum.new <- 3*triples.overall.sentiments$Saturn + 4*triples.overall.sentiments$Jupiter + 5*triples.overall.sentiments$Mars + 6*triples.overall.sentiments$Venus + 7*triples.overall.sentiments$Mercury 
triples.overall.sentiments$seniority <- order(triples.overall.sentiments$order.sum.new, decreasing =FALSE)

# How does this correspond to Ptolemy's listings?
# in English translation (from Tetrabiblos 83 onwards)
# Saturn 240
# Jupiter 169
# Mars 206
# Venus 144
# Mercury 225
# Order is thus: 
#      Saturn, Mercury, Mars, Jupiter, Venus
# cf. Mercury, Saturn, Venus, Mars, Jupiter in Valens
# In both, Mercury gets longer than you would think



# Plot them together
# 
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
  ylab("Sentiment index of double")
ggsave(p.double.single, file='single-doubles.pdf', width=5, height=6)

  
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
  xlab("Mean sentiment index of component planets")+
  ylab("Sentiment index of triple")
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
  xlab("Mean sentiment index of component doubles")+
  ylab("Sentiment index of triple")
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
  xlab("Mean sentiment index of component doubles")+
  ylab("Sentiment index of triple")
p.together <- cowplot::plot_grid(p.triple.single+ggtitle("(a)"), p.triple.double+ggtitle("(b)"),
                                 p.triple.double.single+ggtitle("(c)"), nrow=3)
ggsave(p.together, file='double-triples.pdf', width=6, height=18)

