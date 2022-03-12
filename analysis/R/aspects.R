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
