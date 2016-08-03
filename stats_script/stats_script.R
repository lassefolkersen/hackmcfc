#The purpose of this module is to quantify probabilities of pass based on all parameters input. In the presented analysis numbers from Ben Blacmore's script "Hack MCFC Data Playground.ipynb" was used, which is basically a logistic regression as described here-under:
#Alternative modes of action after that could be random-forest or even some of Paolo Mercantilli's algorithms.


rm(list=ls())
stats_file<-"C:/Users/FOLK/Documents/Work/Analysis/2016-07-26 manchester/2016-07-30_213_for_merged_stats.txt"
stats<-read.table(stats_file,header=T,row.names=1,sep="\t")


#remove the variables that are mostly NA
stats<-stats[,apply(is.na(stats),2,sum)<5000]


formula <- as.formula("outcome ~ qualifier_140 + qualifier_141 + qualifier_213 + qualifier_56 + qualifier_212 + opportunities + friendly_players_narrowly_in_front + opposing_players_narrowly_in_front + friendly_players_widely_in_front + opposing_players_widely_in_front")
model <- glm(formula   ,family=binomial(link='logit'),data=stats)


summary(model)

#End of analysis
#other options to be explored: random forest, linear mixed models
#other covariates to be included, match ID