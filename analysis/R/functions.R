# USEFUL FUNCTIONS
ORDER_OF_BODIES = c('Saturn',
                    'Jupiter',
                    'Mars',
                    'Sun',
                    'Venus',
                    'Mercury',
                    'Moon')
names(ORDER_OF_BODIES) <- c("Sa", "J", "Mar", "Su", "V", "Mer", "Moo")
# For plots
theme_basic <- function () { 
  theme_bw(base_size=12) %+replace% 
    theme(
      axis.text=element_text(colour="black", family = "Times"),
      axis.title=element_text(colour="black", family = "Times"),
      panel.background = element_blank(),
      panel.border = element_blank(),
      axis.line = element_line()
    ) %+replace% 
    theme(
      panel.grid=element_blank()
    )  %+replace% 
    theme(title=element_text(colour="black", family="Times"))
}

# LENGTH OF DESCRIPTIONS
getLengthPassage <- function(conjunction){
  df <- tibble(word=scan(paste0('../../data/texts/Greek-Pingree/', conjunction, '.txt'), what="character", sep=NULL)) %>%
    mutate(line=row_number())
  return(as.numeric(nrow(df)))
}

# SINGLE SCORES FUNCTION
getSingleScores <- function(conjunction, mean=TRUE, singles=singles.overall.sentiments){
  conjunction.bodies <- ordered(unlist(stringr::str_split(conjunction, pattern=" ")), 
                                levels=ORDER_OF_BODIES)
  prop <- 0
  props <- c()
  for (body in conjunction.bodies){
    new.prop <- as.numeric(singles[which(singles$PLANETS==body), "sentiment"])
    prop <- prop +  new.prop
    props <- c(props, new.prop)
  }
  if (mean==FALSE){
    return(props)
  }
  else{
    return(prop/length(props))
  }
}
# Double scores
getDoubleScoresForTriple <- function(triple, double.data=doubles.overall.sentiments, mean=TRUE){
  triple.bodies <- ordered(unlist(stringr::str_split(triple, pattern=" ")), 
                           levels=ORDER_OF_BODIES)
  
  # combinations
  doubles <- ordered(combn(triple.bodies, 2), levels=ORDER_OF_BODIES)
  doubles <- sapply(c(1, 3, 5), function(x) paste(c(doubles[x], doubles[x+1]), collapse= ' '))
  
  doubles.ordered <- sapply( doubles,  
                             function(x) paste(as.character(unlist(strsplit(fixed = TRUE, split=" ", x)), collapse = " ")))
  doubles.ordered <- as.data.frame(doubles.ordered)
  
  set.of.doubles <- sapply(c(1,2, 3), 
                           function(x) paste(ORDER_OF_BODIES[as.numeric(ordered(doubles.ordered[1,x], levels=ORDER_OF_BODIES))],
                                             ORDER_OF_BODIES[as.numeric(ordered(doubles.ordered[2,x], levels=ORDER_OF_BODIES))]))
  
  prop <- 0
  i <- 0
  props <- c()
  for (double in set.of.doubles){
    if (!double  %in% c("Mars Sun", "Mars Moon")){
      new.prop <- double.data[which(double.data$bodies.sorted==double),"sentiment"]
      prop <- prop +  new.prop
      props <- c(props, as.numeric(new.prop))
      i <- i+1
    }
    
  }
  mean.prop <- prop/i
  if (mean==TRUE){
    return(as.numeric(mean.prop))
  }
  else{
    return(props)
  }
}
getDoubleSingleScoresForTriple <- function(triple, double.data=doubles.overall.sentiments, mean=TRUE){
  triple.bodies <- ordered(unlist(stringr::str_split(triple, pattern=" ")), 
                           levels=ORDER_OF_BODIES)
  
  # combinations
  doubles <- ordered(combn(triple.bodies, 2), levels=ORDER_OF_BODIES)
  doubles <- sapply(c(1, 3, 5), function(x) paste(c(doubles[x], doubles[x+1]), collapse= ' '))
  
  doubles.ordered <- sapply( doubles,  
                             function(x) paste(as.character(unlist(strsplit(fixed = TRUE, split=" ", x)), collapse = " ")))
  doubles.ordered <- as.data.frame(doubles.ordered)
  set.of.doubles <- sapply(c(1,2, 3), 
                           function(x) paste(ORDER_OF_BODIES[as.numeric(ordered(doubles.ordered[1,x], levels=ORDER_OF_BODIES))],
                                             ORDER_OF_BODIES[as.numeric(ordered(doubles.ordered[2,x], levels=ORDER_OF_BODIES))]))
  
  # Get the single scores
  single.bodies <- unique(unlist(doubles.ordered))
  
  #single.body.scores <- apply(doubles.ordered, MARGIN=2, function(x) single.bodies[which(!single.bodies %in% x]))
  #single.bodies <- as.character(ORDER_OF_BODIES[as.numeric(apply(doubles.ordered, MARGIN=2, function(x) single.bodies[which(!single.bodies %in% x)]))])
  prop <- 0
  i <- 0
  props <- c()
  for (double in set.of.doubles){
    if (!double  %in% c("Mars Sun", "Mars Moon")){
      double.score <- as.numeric(double.data[which(double.data$bodies.sorted==double),"sentiment"])
      # single that is not in double
      left.out.single <- single.bodies[which(!single.bodies %in% stringr::str_split(set.of.doubles[1], " ")[[1]])]
      # proportion is 2/3 double + 1/3 left out single
      new.prop <- 2/3 * double.score + 1/3 *getSingleScores(left.out.single)
      prop <- prop +  new.prop
      props <- c(props, as.numeric(new.prop))
      i <- i+1
    }
    
  }
  mean.prop <- prop/i
  if (mean==TRUE){
    return(as.numeric(mean.prop))
  }
  else{
    return(props)
  }
}
