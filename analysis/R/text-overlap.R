# 
library(ggplot2)
library(dplyr)
library(cowplot)
library(tidyr)
library(broman)
library(tidytext)
library(ggrepel)

# Functions
source('functions.R')

# SINGLE SENTIMENTS
source('singles-sentiments-load.R')

# DOUBLE SENTIMENTS
source('doubles-sentiments-load.R')

# TRIPLES SENTIMENTS
source('triples-sentiments-load.R')

# Text analysis
for (conjunction in as.character(triples.overall.sentiments$bodies.sorted)){
  df <- tibble(word=scan(paste0('../../../Texts for analysis/names-removed-Riley English Translation (2010)/', gsub(" ", "-", conjunction), '.txt'),what="character", sep=NULL)) %>%
    mutate(line=row_number(), conjunction=conjunction)
  if (exists("full.df")){
    full.df <- rbind(full.df, df)
  }
  else{
    full.df <- df
  }
}
full.df$word <- gsub("[,.\'\"”“;’]", "", full.df$word)
full.df$word <- tolower(full.df$word)
# Remove stopwords
library(stopwords)
full.df.stop <- full.df %>% 
  anti_join(get_stopwords())
full.df.stop <- full.df.stop %>%
  filter(!word %in% c("they", "men", "make", "bring", "cause", "these", "stars", "become", "men", "however", "also", "configuration", ""))

library(reshape2)
word.matrix <- acast(data=full.df.stop,word~conjunction, fill=0)
word.matrix[which(word.matrix>1)] <- 1 # there are some descriptions where the same word crops up more than once, but set these to one
word.matrix <- word.matrix[which(rowSums(word.matrix)!=1),] # remove those that only appear in a single description

dist.mat <- dist(t(word.matrix))
library(ggdendro)
word.dist <- dist(t(word.matrix), method = "euclidean") # Maybe binary would be best? but 
word.dist.mat <- as.matrix(word.dist)
hc <- hclust(word.dist, method = "ward.D2")
ggdendrogram(hc)
ddata <- dendro_data(hc, type = "triangle")
ggplot(segment(ddata)) + 
  geom_segment(aes(x = x, y = y, xend = xend, yend = yend)) + 
  coord_flip() + 
  scale_y_reverse(expand = c(0.2, 0)) +
  theme_dendro()

p <- ggdendro::ggdendrogram(hc, rotate=TRUE, )
p + scale_y_reverse()+
  theme_dendro()

# Could perhaps look at any possible correlation between dist and the components of combination?
# 0 - no bodies shared
# 1 - 1 body shared
# 2 - 2 bodies shared
bodyVector <- function(combination){
  return(as.numeric(sapply(ORDER_OF_BODIES, function(x) ifelse(grepl(x, combination), 1, 0))))
}
bodyOverlap <- function(combination1, combination2){
  v1 <- bodyVector(combination1)
  v2 <- bodyVector(combination2)
  return(length(which(v1+v2==2)))
}
# Make a matrix storing these overlap scores
overlap.mat <- matrix(nrow=nrow(word.dist.mat), 
                      ncol=ncol(word.dist.mat))
rownames(overlap.mat) <- rownames(word.dist.mat)
colnames(overlap.mat) <- colnames(word.dist.mat)
for (i in rownames(word.dist.mat)){
  for (j in colnames(word.dist.mat)){
    overlap.mat[i,j] <- bodyOverlap(i, j)
  }
}
# Now use these to look at distribution of the word overlaps
quantile(word.dist.mat[which(overlap.mat==2)])
quantile(word.dist.mat[which(overlap.mat==1)])
quantile(word.dist.mat[which(overlap.mat==0)])

# Maybe actually just do shared words, not a distance
sharedWords <- function(combination1, combination2){
  return(length(which(word.matrix[,combination1]+word.matrix[,combination2]==2)))
}
shared.word.mat <-  matrix(nrow=nrow(word.dist.mat), 
                           ncol=ncol(word.dist.mat))
rownames(shared.word.mat) <- rownames(word.dist.mat)
colnames(shared.word.mat) <- colnames(word.dist.mat)
for (i in rownames(word.dist.mat)){
  for (j in colnames(word.dist.mat)){
    if (i!=j){
      shared.word.mat[i,j] <- sharedWords(i, j)
    }
  }
}
overlap.two <- shared.word.mat[which(overlap.mat==2)]
overlap.one <- shared.word.mat[which(overlap.mat==1)]
overlap.zero <- shared.word.mat[which(overlap.mat==0)]
overlap.df <- data.frame(shared.words=c(overlap.two, overlap.one, overlap.zero),
      body.overlap=as.factor(c(rep(2, length(overlap.two)), 
        rep(1, length(overlap.one)),
        rep(0, length(overlap.zero)))))
word.plot <- ggplot(overlap.df, aes(body.overlap, shared.words))+
  geom_boxplot(outlier.shape = NA)+
  geom_jitter(alpha=0.25, width=0.2, height=0.1)+
  theme_basic()+
  xlab("Bodies in common between triples")+
  ylab("Shared words in descriptions")+
  scale_y_continuous(breaks=seq(0, 14, 2))
ggsave(file='shared-words-increase-with-body-composition.pdf', word.plot)



overlap <- (word.matrix[,c("Saturn Mars Venus")]+word.matrix[,c("Saturn Venus Moon")])
overlap <- (word.matrix[,c("Jupiter Mars Moon")]+word.matrix[,c("Jupiter Mars Sun")])
overlap <- (word.matrix[,c("Mars Sun Venus")]+word.matrix[,c("Mars Sun Moon")])

mds.df <- data.frame(metaMDS(binary.dist)$points)
mds.df$Saturn <- ifelse(grepl("Saturn", rownames(mds.df)), 1, 0)
mds.df$Jupiter <- ifelse(grepl("Jupiter", rownames(mds.df)), 1, 0)
mds.df$Mars <- ifelse(grepl("Mars", rownames(mds.df)), 1, 0)
mds.df$combination <- rownames(mds.df)
               

#                                   
                        
ggplot(mds.df, aes(MDS1, MDS2))+
  geom_point()+
  theme_basic()+
  geom_text_repel(aes(label=combination), size=2)

library(umap)
words.umap <- umap(t(word.matrix))
umap.df <- data.frame(words.umap$layout)
umap.df$combination <- rownames(umap.df)
ggplot(umap.df, aes(X1, X2))+
  geom_point()+
  theme_basic()+
  geom_text_repel(aes(label=combination), size=2)
