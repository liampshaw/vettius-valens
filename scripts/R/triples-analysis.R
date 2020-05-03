# Libraries
library(reshape2)
library(ggsignif)
library(tidyr)
library(dplyr)
library(ggplot2)
library(ggrepel)

# Read in occurrence data 
# N.B. needs generating again probably
# analyse occurrences of triples
v <- read.csv('data/vettius-occurrences.txt', header=F, stringsAsFactors = F, sep=' ')
v$triple <- paste(v$V1, v$V2, v$V3)
# Does it contain each planet
order_of_bodies = c('Saturn',
                    'Jupiter',
                    'Mars',
                    'Sun',
                    'Venus',
                    'Mercury',
                    'Moon')
v$Saturn <- ifelse(grepl('Saturn', v$triple), "yes", "no")
v$Jupiter <- ifelse(grepl('Jupiter', v$triple), "yes", "no")
v$Mars <- ifelse(grepl('Mars', v$triple), "yes", "no")
v$Sun <- ifelse(grepl('Sun', v$triple), "yes", "no")
v$Venus <- ifelse(grepl('Venus', v$triple), "yes", "no")
v$Mercury <- ifelse(grepl('Mercury', v$triple), "yes", "no")
v$Moon <- ifelse(grepl('Moon', v$triple), "yes", "no")
rownames(v) <- v$triple


v$V1 <- NULL
v$V2 <- NULL
v$V3 <- NULL
v.melt <- melt(v, value.name = "frequency", id.vars = c("triple", order_of_bodies),
               measure.vars = "V4")
v.melt <- melt(v, measure.vars = order_of_bodies)

# Read in triple qualities
qualities <- read.csv('data/triples-qualities.csv', header = T, stringsAsFactors = F)

qualities.df <- qualities %>% group_by(TRIPLE, RATING) %>%
  summarise(count=n()) %>%
  mutate(total=sum(count),
         prop=count/total)

unique(v.melt[order(v.melt$V4, decreasing = TRUE), c("triple", "V4")])

order_of_bodies = c('Saturn',
                    'Jupiter',
                    'Mars',
                    'Sun',
                    'Venus',
                    'Mercury',
                    'Moon')
names(order_of_bodies) <- c("Sa", "J", "Mar", "Su", "V", "Mer", "Moo")
gsub("J ", "Jupiter ", qualities.df$TRIPLE)

qualities.df$body.1 <- sapply(stringr::str_split(qualities.df$TRIPLE, pattern=" "),
                              function(x) x[1])
qualities.df$body.1 <- ordered(order_of_bodies[qualities.df$body.1], 
                               levels=order_of_bodies)
qualities.df$body.2 <- sapply(stringr::str_split(qualities.df$TRIPLE, pattern=" "),
                              function(x) x[2])
qualities.df$body.2 <- ordered(order_of_bodies[qualities.df$body.2],
                               levels=order_of_bodies)

qualities.df$body.3 <- sapply(stringr::str_split(qualities.df$TRIPLE, pattern=" "),
                              function(x) x[3])
qualities.df$body.3 <- ordered(order_of_bodies[qualities.df$body.3],
                               levels=order_of_bodies)

qualities.df$bodies.sorted <- sapply(1:nrow(qualities.df),
                                     function(x) paste(as.character(sort(unlist(c(qualities.df[x,"body.1"], 
                                                                                  qualities.df[x,"body.2"], 
                                                                                  qualities.df[x,"body.3"])))), collapse=" "))

qualities.df.good <- qualities.df[which(qualities.df$RATING=="good"),]
qualities.df.good$occurrence <- v[qualities.df.good$bodies.sorted, "V4"]

ggplot(qualities.df.good, aes(occurrence, prop))+
  geom_point(size=3)+
  ggrepel::geom_label_repel(aes(label=bodies.sorted), size=4)+
  scale_x_log10()+
  geom_smooth(method="lm")+
  theme_bw()+
  xlab("% of days which contain triple (log-scale)")+
  ylab("Proportion of associations which are 'good'")


qualities.df.bad <- qualities.df[which(qualities.df$RATING=="bad"),]
qualities.df.bad$occurrence <- v[qualities.df.bad$bodies.sorted, "V4"]
ggplot(qualities.df.bad, aes(occurrence, prop))+
  geom_point(size=3)+
  ggrepel::geom_label_repel(aes(label=bodies.sorted), size=4)+
  scale_x_log10()+
  geom_smooth(method="lm")+
  theme_bw()+
  xlab("% of days which contain triple (log-scale)")+
  ylab("Proportion of associations which are 'bad'")


cor.test(qualities.df.bad$occurrence, qualities.df.bad$prop, method="spearman")


# Do multivariate analysis of variance with involvement of body as a binary variable
qualities.df.good$Saturn <- grepl("Saturn", qualities.df.good$bodies.sorted)
ggplot(qualities.df.good, aes(Saturn, prop))+
  geom_boxplot(outlier.shape = NA)+
  geom_jitter(width=0.25)

# Jupiter
qualities.df.good$Jupiter <- grepl("Jupiter", qualities.df.good$bodies.sorted)
ggplot(qualities.df.good, aes(Jupiter, prop))+
  geom_boxplot(outlier.shape = NA)+
  geom_jitter(width=0.25)

# Mercury
# Jupiter
qualities.df.good$Mercury <- grepl("Mercury", qualities.df.good$bodies.sorted)
ggplot(qualities.df.good, aes(Jupiter, prop))+
  geom_boxplot(outlier.shape = NA)+
  geom_jitter(width=0.25)

# Build a model with X = binary presence of planets i.e. seven-element binary vector
# proportion of good predictions ~ beta*X + b*occurrence
# The question would then be, what is b?
# What we've analysed in terms of frequencies alone above doesn't take into account the properties of the planets

#### MODELLING
# Can consider four models:
# 1. Planet  
# 2. Frequency of combination
# 3. Planet and Frequency (no interaction)
# 4. Planet and Frequency (interaction) 
# (and then a group depending on how much interaction term)
# The ultimate question which would be nice to answer is: do different planets have different proportions of good/bad / specific/general associations because of their actual orbit speeds or because of cultural reasons
# qualities.df.good
# prop ~ occurrence + 
modelling.df <- qualities.df.good %>% select(prop, bodies.sorted, occurrence)
modelling.df %>% 
  mutate(Saturn=as.numeric(grepl("Saturn", bodies.sorted)),)
addbody <- function(df, body) {
  mutate(df, !!body := as.numeric(grepl(body,bodies.sorted )))
}
for (body in order_of_bodies){
  modelling.df <- addbody(modelling.df, body)
}
# Run model
LM.planet.occurrence <- lm(prop ~ Saturn + Jupiter + Mars + Sun + Venus + Mercury  + occurrence, 
   data=modelling.df )
LM.planet <- lm(prop ~ Saturn + Jupiter + Mars + Sun + Venus + Mercury, 
                           data=modelling.df)
LM.occurrence <- lm(prop ~ occurrence, 
                data=modelling.df)
LM.planet.occurrence.interaction <- lm(prop ~ Saturn*occurrence + Jupiter*occurrence + 
                                         Mars*occurrence + Sun*occurrence + 
                                         Venus*occurrence + Mercury*occurrence, 
                    data=modelling.df)
anova(LM.planet, LM.planet.occurrence, LM.occurrence)
anova(LM.occurrence, LM.planet)
anova(LM.planet, LM.occurrence)
anova(LM.planet.occurrence, LM.occurrence)

# AICs
AIC(LM.planet, LM.planet.occurrence, LM.planet.occurrence.interaction, LM.occurrence)

# The AICs are clear; AIC_min = -18.6 (most complex model)
# And the deltaAIC with planet model is 6.5 which is "considerably less support" (https://stats.stackexchange.com/questions/232465/how-to-compare-models-on-the-basis-of-aic)
# However, I'm a bit concerned by possible co-linearity with planet and frequency.
# There's an overall positive impact of frequency - but this becomes negative by varying
# amounts when combined with a planet. This basically moves it back to zero.
# Can we do half and half cross-validation?
random.sample <- sample(nrow(modelling.df), nrow(modelling.df)/2)
train.df <- modelling.df[random.sample,]
test.df <- modelling.df[which(!rownames(modelling.df) %in% random.sample),]
LM.planet.occurrence.interaction.half <- lm(prop ~ Saturn*occurrence + Jupiter*occurrence + 
                                         Mars*occurrence + Sun*occurrence + 
                                         Venus*occurrence + Mercury*occurrence, 
                                       data=train.df)
train.predictions <- predict(LM.planet.occurrence.interaction.half, train.df)
sum((train.predictions-train.df$prop)^2)
test.predictions <- predict(LM.planet.occurrence.interaction.half, test.df)
sum((test.predictions-test.df$prop)^2)

# Sun-Venus-Mercury is a big outlier - it's what causes this gigantic residual sum of squares!
# So exclude it?
modelling.df.2 <- modelling.df[which(modelling.df$bodies.sorted!=c("Sun Venus Mercury")),]
random.sample <- sample(nrow(modelling.df.2), nrow(modelling.df.2)/2)
train.df <- modelling.df.2[random.sample,]
test.df <- modelling.df.2[which(!rownames(modelling.df.2) %in% random.sample),]
LM.planet.occurrence.interaction.half <- lm(prop ~ Saturn*occurrence + Jupiter*occurrence + 
                                              Mars*occurrence + Sun*occurrence + 
                                              Venus*occurrence + Mercury*occurrence, 
                                            data=train.df)
train.predictions <- predict(LM.planet.occurrence.interaction.half, train.df)
sum((train.predictions-train.df$prop)^2)
test.predictions <- predict(LM.planet.occurrence.interaction.half, test.df)
sum((test.predictions-test.df$prop)^2)
test.df[names(test.predictions)[which(test.predictions^2 > 10)],]
# In fact, get rid of all the triples including Sun-Mercury
modelling.df.3 <- modelling.df[which(!(modelling.df$Sun==1 & modelling.df$Mercury==1)),]
random.sample <- sample(nrow(modelling.df.3), nrow(modelling.df.3)/2)
train.df <- modelling.df.3[random.sample,]
test.df <- modelling.df.3[which(!rownames(modelling.df.3) %in% random.sample),]
LM.planet.occurrence.interaction.half <- lm(prop ~ Saturn*occurrence + Jupiter*occurrence + 
                                              Mars*occurrence + Sun*occurrence + 
                                              Venus*occurrence + Mercury*occurrence, 
                                            data=train.df)
train.predictions <- predict(LM.planet.occurrence.interaction.half, train.df)
sum((train.predictions-train.df$prop)^2)
test.predictions <- predict(LM.planet.occurrence.interaction.half, test.df)
sum((test.predictions-test.df$prop)^2)
test.df[names(test.predictions)[which(test.predictions^2 > 10)],]
# Now Sun and Venus are the problem...
# SO I think no half cross-validation like this will work

# Using the double proportion as 'independent' input to 
# build a 'mechanistic' model which uses other data to decide the 
# coefficients is probably better. 

# 
# We should use beta regression!
# https://hansjoerg.me/2019/05/10/regression-modeling-with-proportion-data-part-1/
library(betareg)
BETA.planet.occurrence <- betareg(prop ~ Saturn + Jupiter + Mars + Sun + Venus + Mercury  + occurrence, 
                           data=modelling.df)
BETA.planet <- betareg(prop ~ Saturn + Jupiter + Mars + Sun + Venus + Mercury, 
                data=modelling.df)
BETA.occurrence <- betareg(prop ~ occurrence, 
                    data=modelling.df)
BETA.planet.occurrence.interaction <- betareg(prop ~ Saturn*occurrence + Jupiter*occurrence + 
                                         Mars*occurrence + Sun*occurrence + 
                                         Venus*occurrence + Mercury*occurrence, 
                                       data=modelling.df)
BETA.planet.occurrence.Saturn <- betareg(prop ~ Saturn + Jupiter + Mars + Sun + Venus + Mercury+
                                                occurrence | Saturn, 
                                              data=modelling.df)
BETA.planet.occurrence.interaction.only <- betareg(prop ~ Saturn:occurrence + Jupiter:occurrence + 
                                                Mars:occurrence + Sun:occurrence + 
                                                Venus:occurrence + Mercury:occurrence, 
                                              data=modelling.df)
# AICs
AIC(BETA.planet, BETA.occurrence, BETA.planet.occurrence.interaction, BETA.planet.occurrence,
    BETA.planet.occurrence.interaction.only)

# Again, best model is the one with planet and interaction.
# Using all of the interaction terms feels wrong. 
# What's the right way to account for a planet-level effect of frequency?



# Whether triple contains a planet
# or also whether triple contains a double (e.g. 5 triples contain Mars and Jupiter)
# Do goodness scores of component doubles correlate with the goodness score of a triple?

# Add doubles
doubles.qualities <- read.csv('data/doubles-qualities.csv', header = T, stringsAsFactors = F)

doubles.qualities.df <- doubles.qualities %>% group_by(DOUBLE, RATING) %>%
  summarise(count=n()) %>%
  mutate(total=sum(count),
         prop=count/total)

doubles.qualities.df$body.1 <- sapply(stringr::str_split(doubles.qualities.df$DOUBLE, pattern=" "),
                              function(x) x[1])
doubles.qualities.df$body.1 <- ordered(order_of_bodies[doubles.qualities.df$body.1], 
                               levels=order_of_bodies)
doubles.qualities.df$body.2 <- sapply(stringr::str_split(doubles.qualities.df$DOUBLE, pattern=" "),
                              function(x) x[2])
doubles.qualities.df$body.2 <- ordered(order_of_bodies[doubles.qualities.df$body.2],
                               levels=order_of_bodies)
doubles.qualities.df$bodies.sorted <- sapply(1:nrow(doubles.qualities.df),
                                     function(x) paste(as.character(sort(unlist(c(doubles.qualities.df[x,"body.1"], 
                                                                                  doubles.qualities.df[x,"body.2"])))), collapse=" "))

# This kind of plot is worth including as well
ggplot(doubles.qualities.df, aes(bodies.sorted, prop, fill=RATING))+
  geom_bar(stat="identity")+
  theme_bw()+
  theme(axis.text.x=element_text(angle=45, hjust=1))







# Is there a correlation between doubles and triples?
# i.e. for a given triple, is the median score of the 3 choose 2 doubles 
# correlated with its score?
doubles.qualities.df.good <- doubles.qualities.df[which(doubles.qualities.df$RATING=="good"),]
# Add missing scores
getDoubleScores <- function(triple, double.data=doubles.qualities.df, maximum=FALSE){
  triple.bodies <- ordered(unlist(stringr::str_split(triple, pattern=" ")), 
                           levels=order_of_bodies)
  
  # combinations
  doubles <- ordered(combn(triple.bodies, 2), levels=order_of_bodies)
  doubles <- sapply(c(1, 3, 5), function(x) paste(c(doubles[x], doubles[x+1]), collapse= ' '))
  
  doubles.ordered <- sapply( doubles,  
                   function(x) paste(as.character(sort(unlist(strsplit(fixed = TRUE, split=" ", x))), collapse = " ")))
  doubles.ordered <- as.data.frame(doubles.ordered)
  set.of.doubles <- sapply(c(1,2, 3), 
         function(x) paste(order_of_bodies[as.numeric(as.character(doubles.ordered[1,x]))],
                           order_of_bodies[as.numeric(as.character(doubles.ordered[2,x]))]))
  
  prop <- 0
  i <- 0
  props <- c()
  for (double in set.of.doubles){
    if (!double  %in% c("Mars Sun", "Mars Moon")){
      new.prop <- double.data[which(double.data$bodies.sorted==double & 
                                      double.data$RATING=="good"), "prop"]
      prop <- prop +  new.prop
      props <- c(props, as.numeric(new.prop))
      i <- i+1
    }
 
  }
  mean.prop <- prop/i
  if (maximum==FALSE){
    return(as.numeric(mean.prop))
  }
  else{
    return(max(props))
  }
}

# Add this to triples df
# Note that for now Mars Sun, and Mars Moon are missing. 

qualities.df.good$double.mean.prop <- sapply(qualities.df.good$bodies.sorted, 
                                             function(x) getDoubleScores(x))
ggplot(qualities.df.good, aes(double.mean.prop, prop))+
  geom_point()+
  theme_bw()+
  stat_smooth(method="lm", colour="red")+
  coord_cartesian(xlim=c(0,1), ylim=c(0,1))+
  scale_x_continuous(expand=c(0,0),)+
  scale_y_continuous(expand=c(0,0))+
  xlab("Mean good proportion for triple's component doubles")+
  ylab("Good proportion for triple")+
  theme(axis.line=element_line(colour="black"),
        panel.background = element_blank(),
        plot.background = element_blank(),
        panel.border = element_blank(),
        panel.grid = element_blank(),
        axis.text=element_text(colour="black"))
ggsave("figures/doubles-predict-triples.pdf", width=8, height=6)


# Check maximum as well?
qualities.df.good$double.max.prop <- sapply(qualities.df.good$bodies.sorted, 
                                             function(x) getDoubleScores(x, maximum = TRUE))
ggplot(qualities.df.good, aes(double.max.prop, prop))+
  geom_point()+
  theme_bw()+
  stat_smooth(method="lm", colour="red")+
  coord_cartesian(xlim=c(0,1), ylim=c(0,1))+
  scale_x_continuous(expand=c(0,0),)+
  scale_y_continuous(expand=c(0,0))+
  xlab("Maximum good proportion for triple's component doubles")+
  ylab("Good proportion for triple")+
  theme(axis.line=element_line(colour="black"),
        panel.background = element_blank(),
        plot.background = element_blank(),
        panel.border = element_blank(),
        panel.grid = element_blank(),
        axis.text=element_text(colour="black"))
ggsave("figures/doubles-predict-triples-maximum.pdf", width=8, height=6)
