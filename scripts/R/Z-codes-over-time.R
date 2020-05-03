# Plot occurrences over time. 
library(ggplot2)
library(broman)

occurrences <- read.csv('../scheme-of-heaven/0-CE-200-CE-occurrence-50-year-sliding-window.csv', header=F,stringsAsFactors = F)
colnames(occurrences) <- c("year", "triple", "occurrence")



# Order by median value
library(dplyr)
occurrences.median <- occurrences %>% group_by(triple) %>%
  summarise(median=median(occurrence))
order.of.triples <- occurrences.median$triple[order(occurrences.median$median, decreasing = TRUE)]
occurrences.median$year <- 175
occurrences.median$y <- 11
occurrences.median$triple <- ordered(occurrences.median$triple,
                                     levels=order.of.triples)
occurrences.median$median.plot <- myround(occurrences.median$median, 2)

# Transform ordering
occurrences$triple <- ordered(occurrences$triple,
                              levels=order.of.triples)

occurrences.plot <- ggplot(occurrences, aes(year, occurrence, group=triple))+
  geom_line()+
  theme_bw()+
  xlab("Year (CE)")+
  ylab("Z-codes in 50 year period containing triple (%)")+
  facet_wrap(~triple, ncol=7)+
  theme(panel.grid = element_blank())+
  scale_y_continuous(breaks=c(0, 5, 10, 15))+
  ylim(c(0,15))+
  theme(strip.text = element_text(size=8))+
  geom_text(data=occurrences.median, 
            aes(year, median+1.75, label=median.plot, group=triple))

pdf('occurrences-fifty-year-sliding-window.pdf', width=10, height=6)
occurrences.plot
dev.off()

# TO DO: annotate those which are missing from Valens