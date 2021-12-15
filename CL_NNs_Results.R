rm(list=ls())
library(scales)
library(beeswarm)

path <- "/Users/joemorford/Desktop/CL_NNs/Sim_data/"


file_list <- list.files(path=path)

df_list <- list()
for (i in 1:length(file_list)){
  df_list[[i]] <- read.csv(paste(path, file_list[i], sep=""))
}


#Pairs of agents with Pberaging decision-making rules and leadership weightings of 0.5:0.5, 0.7:0.3 and 0.9:0.1
#Democratic decision-making
Av_0.5 <- df_list[[1]]
Av_0.7 <- df_list[[2]]
Av_0.9 <- df_list[[3]]
#Pairs of agents with probabilistic decision-making rules and leadership weightings of 0.5:0.5, 0.7:0.3 and 0.9:0.1
#Despotic decision-making
Pb_0.5 <- df_list[[4]]
Pb_0.7 <- df_list[[5]]
Pb_0.9 <- df_list[[6]]
#Solo agents
Solo <- df_list[[7]]
Solo$Pair_ID <- NULL


######### Solo agent learning results

Trials <- unique(Solo$Trial_n)
#Trials is number of test trials
Solo_meds <- c()
for (i in Trials){
  Trial_i <- subset(Solo, Solo$Trial_n==i)
  Solo_meds[i+1] <- median(Trial_i$Individual)*360
}
#Medians


plot(Solo_meds~Trials, type="l", ylim = c(0, 180), lwd=3.5, col="black", bty='l',
     ylab="Absolute angular error", xlab="Number of learning trials")
better_worse <- c()
for (i in unique(Solo$Individual_n)){
  ind <- unique(Solo$Individual_n)[i]
  all_ind <- subset(Solo, Solo$Individual_n==ind)
  lines((Individual*360)~Trial_n, data=all_ind, col=alpha("black", 0.3), lwd=0.8)
  if (all_ind$Individual[1]>all_ind$Individual[nrow(all_ind)]){
    better_worse[i] <- "B"
  }else{
    better_worse[i] <- "W"
  }
}
lines(Solo_meds~Trials, type="l", ylim = c(0, 180), lwd=3.5, col="black", ylab="Absolute angular error", xlab="Number of learning trials")


pdf("/Users/joemorford/Desktop/CL_NNs/14122021_figs/Solo_learners.pdf", width=2, height=2,
    pointsize=6)
plot(Solo_meds~Trials, type="l", ylim = c(0, 180), lwd=1.2, col="black", bty='l',
     ylab="Absolute angular error", xlab="Number of learning trials")
dev.off()

better_worse
length(better_worse[better_worse=="W"])
#How many agents did not improve in performance?


#############Pairs of agents with Averaging decision-making rule - performance


######Averaging pairs - collective performance


#0.5:0.5 leadership weighting
Av_0.5_meds <- c()
for (i in Trials){
  Trial_i <- subset(Av_0.5, Av_0.5$Trial_n==i)
  Av_0.5_meds[i+1] <- median(Trial_i$Collective)*360
}
plot(Av_0.5_meds~Trials, type="l", ylim = c(0, 180), lwd=3, col="red", 
     ylab="Collective absolute angular error", xlab="Number of learning trials")
better_worse <- c()
for (i in unique(Av_0.5$Pair_n)){
  pair <- unique(Av_0.5$Pair_n)[i]
  all_pair <- subset(Av_0.5, Av_0.5$Pair_n==pair)
  lines((Collective*360)~Trial_n, data=all_pair, col=alpha("black", 0.4), lwd=1.5)
  if (all_pair$Collective[1]>all_pair$Collective[nrow(all_pair)]){
    better_worse[i] <- "B"
  }else{
    better_worse[i] <- "W"
  }
}
lines(Av_0.5_meds~Trials, type="l", ylim = c(0, 180), lwd=3, col="red", ylab="Collective absolute angular error", xlab="Number of learning trials")

better_worse
length(better_worse[better_worse=="W"])
#How many pairs of agents did not improve in performance?


#0.7:0.3 leadership weighting
Av_0.7_meds <- c()
for (i in Trials){
  Trial_i <- subset(Av_0.7, Av_0.7$Trial_n==i)
  Av_0.7_meds[i+1] <- median(Trial_i$Collective)*360
}

better_worse <- c()
plot(Av_0.7_meds~Trials, type="l", ylim = c(0, 180), lwd=3, col="purple", ylab="Collective absolute angular error", xlab="Number of learning trials")
for (i in unique(Av_0.7$Pair_n)){
  pair <- unique(Av_0.7$Pair_n)[i]
  all_pair <- subset(Av_0.7, Av_0.7$Pair_n==pair)
  lines((Collective*360)~Trial_n, data=all_pair, col=alpha("black", 0.4), lwd=1.5)
  if (all_pair$Collective[1]>all_pair$Collective[nrow(all_pair)]){
    better_worse[i] <- "B"
  }else{
    better_worse[i] <- "W"
  }
}
lines(Av_0.7_meds~Trials, type="l", ylim = c(0, 180), lwd=3, col="purple", ylab="Collective absolute angular error", xlab="Number of learning trials")
better_worse
length(better_worse[better_worse=="W"])
#How many pairs of agents did not improve in performance?


#0.9:0.1 leadership weighting

Av_0.9_meds <- c()
for (i in Trials){
  Trial_i <- subset(Av_0.9, Av_0.9$Trial_n==i)
  Av_0.9_meds[i+1] <- median(Trial_i$Collective)*360
}

better_worse <- c()
plot(Av_0.9_meds~Trials, type="l", ylim = c(0, 180), lwd=3, col="blue", ylab="Collective absolute angular error", xlab="Number of learning trials")
for (i in unique(Av_0.9$Pair_n)){
  pair <- unique(Av_0.9$Pair_n)[i]
  all_pair <- subset(Av_0.9, Av_0.7$Pair_n==pair)
  lines((Collective*360)~Trial_n, data=all_pair, col=alpha("black", 0.4), lwd=1.5)
  if (all_pair$Collective[1]>all_pair$Collective[nrow(all_pair)]){
    better_worse[i] <- "B"
  }else{
    better_worse[i] <- "W"
  }
}
lines(Av_0.9_meds~Trials, type="l", ylim = c(0, 180), lwd=3, col="blue", ylab="Collective absolute angular error", xlab="Number of learning trials")

better_worse
length(better_worse[better_worse=="W"])
#How many pairs of agents did not improve in performance?




plot(Av_0.5_meds~Trials, type="l", ylim = c(0, 180), lwd=3,  bty='l',
     col="red", ylab="Collective absolute angular error", xlab="Number of learning trials")
lines(Av_0.7_meds~Trials, type="l", ylim = c(0, 180), lwd=3, col="purple", ylab="Collective absolute angular error")
lines(Av_0.9_meds~Trials, type="l", ylim = c(0, 180), lwd=3, col="blue", ylab="Collective absolute angular error")
legend(14, 180, title="Decision-making\n weightings", legend=c("0.5:0.5", "0.7:0.3", "0.9:0.1"),
       col=c("red", "purple", "blue"), lty=1, cex=1.2, box.lty=0, lwd=2)



pdf("/Users/joemorford/Desktop/CL_NNs/14122021_figs/Collective_Averaging.pdf", width=2, height=2,
    pointsize=6)
plot(Av_0.5_meds~Trials, type="l", ylim = c(0, 180), lwd=1.2,  bty='l',
     col="red", ylab="Collective absolute angular error", xlab="Number of learning trials")
lines(Av_0.7_meds~Trials, type="l", ylim = c(0, 180), lwd=1.2, col="purple", ylab="Collective absolute angular error")
lines(Av_0.9_meds~Trials, type="l", ylim = c(0, 180), lwd=1.2, col="blue", ylab="Collective absolute angular error")
legend(12, 160, title="Decision-making\n weightings", legend=c("0.5:0.5", "0.7:0.3", "0.9:0.1"),
       col=c("red", "purple", "blue"), lty=1, cex=1, box.lty=0, lwd=1.2)

dev.off()




Av_0.5$LF <- factor(0.5)
Av_0.7$LF <- factor(0.7)
Av_0.9$LF <- factor(0.9)

Avs <- rbind(Av_0.5, Av_0.7, Av_0.9)
Avs$performance_pair_ID <- NA
for (i in 1:nrow(Avs)){
  if (i==1){
    pair <- 1
    counter <- 1
  }
  if (Avs$Pair_n[i] != pair){
    pair <- Avs$Pair_n[i]
    counter <- counter + 1
  }
  Avs$performance_pair_ID[i] <- counter
}
Avs_5s <- subset(Avs, Avs$Trial_n==5)
Avs_5s$Performance <- Avs_5s$Collective
Avs_25s <- subset(Avs, Avs$Trial_n==25)
Avs_25s$Performance <- Avs_25s$Collective
#Dataframes for 5th and 25th test trial


Solo$LF <- factor("Solo")
Solo$Performance <- Solo$Individual
Solo_5s <- subset(Solo, Solo$Trial_n==5)
Solo_25s <- subset(Solo, Solo$Trial_n==25)
#Dataframes for 5th and 25th test trial


performance_Avs_5s <- rbind(Solo_5s[,c(4,5)], Avs_5s[,c(9,11)])
performance_Avs_25s <- rbind(Solo_25s[,c(4,5)], Avs_25s[,c(9,11)])
#Dataframe with Solo agents and collective pairs of agents at 5th test trial and 25th test trial


test_performance_Avs_5s <- pairwise.wilcox.test(performance_Avs_5s$Performance, performance_Avs_5s$LF, p.adjust.method = "none")
test_performance_Avs_5s
#Pairwise MWU tests between different leadership weightings pairs of agents plus Solo agents
#At 6th test trial


test_performance_Avs_25s <- pairwise.wilcox.test(performance_Avs_25s$Performance, performance_Avs_25s$LF, p.adjust.method = "none")
test_performance_Avs_25s
#Pairwise MWU tests between different leadership weightings pairs of agents plus Solo agents
#At 25th test trial



######## Inidividuals within pairs with Averaging decision-making

#0.5:0.5 leadership weighting
Av_0.5_meds_lf <- c()
for (i in Trials){
  Trial_i <- subset(Av_0.5, Av_0.5$Trial_n==i)
  Av_0.5_meds_lf[i+1] <- median(abs(c(Trial_i$Leader, Trial_i$Follower))*360)
}
#Leader and follower is abitrary when weighting = 0.5
plot(Av_0.5_meds_lf~Trials, type="l", ylim = c(0, 180), lty="dotdash", lwd=3, col="red", ylab="Solo absolute angular error", xlab="Number of learning trials")
for (i in unique(Av_0.5$Pair_n)){
  pair <- unique(Av_0.5$Pair_n)[i]
  all_pair <- subset(Av_0.5, Av_0.5$Pair_n==pair)
  lines((abs(Leader)*360)~Trial_n, data=all_pair, col=alpha("black", 0.4))
  lines((abs(Follower)*360)~Trial_n, data=all_pair, col=alpha("black", 0.4))
}
lines(Av_0.5_meds_lf~Trials, type="l", ylim = c(0, 180), lty="dotdash", lwd=3, col="red", ylab="Solo absolute angular error", xlab="Number of learning trials")


#0.7:0.3 leadership weighting

Av_0.7_meds_l <- c()
#Leader
Av_0.7_meds_f <- c()
#Follower
for (i in Trials){
  Trial_i <- subset(Av_0.7, Av_0.7$Trial_n==i)
  Av_0.7_meds_l[i+1] <- median(abs(Trial_i$Leader)*360)
  Av_0.7_meds_f[i+1] <- median(abs(Trial_i$Follower)*360)  
}

plot(Av_0.7_meds_l~Trials, type="l", ylim = c(0, 180), lty="dashed", lwd=3, col="purple", ylab="Solo absolute angular error", xlab="Number of learning trials")
for (i in unique(Av_0.7$Pair_n)){
  pair <- unique(Av_0.7$Pair_n)[i]
  all_pair <- subset(Av_0.7, Av_0.7$Pair_n==pair)
  lines((abs(Leader)*360)~Trial_n, data=all_pair, lty="dashed", col=alpha("black", 0.4))
}
lines(Av_0.7_meds_l~Trials, type="l", ylim = c(0, 180), lty="dashed", lwd=3, col="purple", ylab="Solo absolute angular error", xlab="Number of learning trials")

plot(Av_0.7_meds_f~Trials, type="l", ylim = c(0, 180), lty="dotted", lwd=3, col="purple", ylab="Solo absolute angular error", xlab="Number of learning trials")
for (i in unique(Av_0.7$Pair_n)){
  pair <- unique(Av_0.7$Pair_n)[i]
  all_pair <- subset(Av_0.7, Av_0.7$Pair_n==pair)
  lines((abs(Follower)*360)~Trial_n, data=all_pair, lty="dotted", col=alpha("black", 0.4))
}
lines(Av_0.7_meds_f~Trials, type="l", ylim = c(0, 180), lty="dotted", lwd=3, col="purple", ylab="Solo absolute angular error", xlab="Number of learning trials")


#0.9:0.1 leadership weighting

Av_0.9_meds_l <- c()
Av_0.9_meds_f <- c()
for (i in Trials){
  Trial_i <- subset(Av_0.9, Av_0.9$Trial_n==i)
  Av_0.9_meds_l[i+1] <- median(abs(Trial_i$Leader)*360)
  Av_0.9_meds_f[i+1] <- median(abs(Trial_i$Follower)*360)  
}
plot(Av_0.9_meds_l~Trials, type="l", ylim = c(0, 180), lty="dashed", lwd=3, col="blue", ylab="Solo absolute angular error", xlab="Number of learning trials")
for (i in unique(Av_0.9$Pair_n)){
  pair <- unique(Av_0.9$Pair_n)[i]
  all_pair <- subset(Av_0.7, Av_0.7$Pair_n==pair)
  lines((abs(Leader)*360)~Trial_n, data=all_pair, lty="dashed", col=alpha("black", 0.4))
}
lines(Av_0.9_meds_l~Trials, type="l", ylim = c(0, 180), lty="dashed", lwd=3, col="blue", ylab="Solo absolute angular error", xlab="Number of learning trials")

plot(Av_0.9_meds_f~Trials, type="l", ylim = c(0, 180), lty="dotted", lwd=3, col="blue", ylab="Solo absolute angular error", xlab="Number of learning trials")
for (i in unique(Av_0.9$Pair_n)){
  pair <- unique(Av_0.9$Pair_n)[i]
  all_pair <- subset(Av_0.7, Av_0.7$Pair_n==pair)
  lines((abs(Follower)*360)~Trial_n, data=all_pair, lty="dotted", col=alpha("black", 0.4))
}
lines(Av_0.9_meds_f~Trials, type="l", ylim = c(0, 180), lty="dotted", lwd=3, col="blue", ylab="Solo absolute angular error", xlab="Number of learning trials")




plot(Av_0.5_meds_lf~Trials, type="l", ylim = c(0, 180), lty="dotdash", lwd=3, col="red", ylab="Solo absolute angular error", xlab="Number of learning trials")
lines(Av_0.7_meds_l~Trials, type="l", ylim = c(0, 180), lty="dashed", lwd=3, col="purple", ylab="Solo absolute angular error", xlab="Number of learning trials")
lines(Av_0.7_meds_f~Trials, type="l", ylim = c(0, 180), lty="dotted", lwd=3, col="purple", ylab="Solo absolute angular error", xlab="Number of learning trials")
lines(Av_0.9_meds_l~Trials, type="l", ylim = c(0, 180), lty="dashed", lwd=3, col="blue", ylab="Solo absolute angular error", xlab="Number of learning trials")
lines(Av_0.9_meds_f~Trials, type="l", ylim = c(0, 180), lty="dotted", lwd=3, col="blue", ylab="Solo absolute angular error", xlab="Number of learning trials")


pdf("/Users/joemorford/Desktop/CL_NNs/14122021_figs/Solo_Averaging.pdf", width=2.5, height=2.5,
    pointsize=7)
plot(Av_0.5_meds_lf~Trials, type="l", ylim = c(0, 180), lty="dotdash", lwd=1.2, col="red", bty="l",
     ylab="Solo absolute angular error", xlab="Number of learning trials")
lines(Solo_meds~Trials, type="l", ylim = c(0, 180), lwd=1.2, col="black", ylab="Absolute angular error", xlab="Number of learning trials")
lines(Av_0.7_meds_l~Trials, type="l", ylim = c(0, 180), lty="dashed", lwd=1.2, col="purple", ylab="Solo absolute angular error", xlab="Number of learning trials")
lines(Av_0.7_meds_f~Trials, type="l", ylim = c(0, 180), lty="dotted", lwd=1.2, col="purple", ylab="Solo absolute angular error", xlab="Number of learning trials")
lines(Av_0.9_meds_l~Trials, type="l", ylim = c(0, 180), lty="dashed", lwd=1.2, col="blue", ylab="Solo absolute angular error", xlab="Number of learning trials")
lines(Av_0.9_meds_f~Trials, type="l", ylim = c(0, 180), lty="dotted", lwd=1.2, col="blue", ylab="Solo absolute angular error", xlab="Number of learning trials")
legend("topright", legend=c("0.5:0.5", "0.7:0.3 leaders", "0.7:0.3 followers",
                                 "0.9:0.1 leaders", "0.9:0.1 followers", 
                                 "Solo learners"),
       col=c("red", "purple", "purple", "blue", "blue", "black"), 
       lty=c(4, 2, 3, 2, 3, 1), cex=0.85, 
       box.lty=0, lwd=1.2)

dev.off()

Avs_Solos_trial_last <- c(
  Avs_25s$Leader[which(Avs_25s$LF==0.5)],
  Avs_25s$Follower[which(Avs_25s$LF==0.5)],
  Avs_25s$Leader[which(Avs_25s$LF==0.7)],
  Avs_25s$Follower[which(Avs_25s$LF==0.7)],
  Avs_25s$Leader[which(Avs_25s$LF==0.9)],
  Avs_25s$Follower[which(Avs_25s$LF==0.9)],
  Solo_25s$Performance
)
Avs_Solos_trial_last <- abs(Avs_Solos_trial_last)
Category <- c(rep("0.5", 500), rep("0.7_l", 250), rep("0.7_f", 250), 
              rep("0.9_l", 250), rep("0.9_f", 250), rep("Solo", 250))

pairwise.wilcox.test(Avs_Solos_trial_last, Category)


#####  Pair difference (with Averaging decision-making rule)

#0.5:0.5 leadership weighting

Av_0.5_meds_pd <- c()
for (i in Trials){
  Trial_i <- subset(Av_0.5, Av_0.5$Trial_n==i)
  Av_0.5_meds_pd[i+1] <- median(abs(Trial_i$Pair_diff)*360)
}

plot(Av_0.5_meds_pd~Trials, type="l", ylim = c(0, 180), lwd=3, col="red", ylab="Pair difference", xlab="Number of learning trials")
for (i in unique(Av_0.5$Pair_n)){
  pair <- unique(Av_0.5$Pair_n)[i]
  all_pair <- subset(Av_0.5, Av_0.5$Pair_n==pair)
  lines((Pair_diff*360)~Trial_n, data=all_pair, col=alpha("black", 0.4), lwd=1.5)
}
lines(Av_0.5_meds_pd~Trials, type="l", ylim = c(0, 180), lwd=3, col="red", ylab="Pair difference", xlab="Number of learning trials")

Av_0.5_trial0 <- Av_0.5[which(Av_0.5$Trial_n==0),]
Av_0.5_trial_last <- Av_0.5[which(Av_0.5$Trial_n==25),]

wilcox.test(Av_0.5_trial0$Pair_diff, Av_0.5_trial_last$Pair_diff)

#0.7:0.3 leadership weighting

Av_0.7_meds_pd <- c()
for (i in Trials){
  Trial_i <- subset(Av_0.7, Av_0.7$Trial_n==i)
  Av_0.7_meds_pd[i+1] <- median(abs(Trial_i$Pair_diff)*360)
}
plot(Av_0.7_meds_pd~Trials, type="l", ylim = c(0, 180), lwd=3, col="purple", ylab="Pair difference", xlab="Number of learning trials")
for (i in unique(Av_0.7$Pair_n)){
  pair <- unique(Av_0.7$Pair_n)[i]
  all_pair <- subset(Av_0.7, Av_0.5$Pair_n==pair)
  lines((Pair_diff*360)~Trial_n, data=all_pair, col=alpha("black", 0.4), lwd=1.5)
}
lines(Av_0.7_meds_pd~Trials, type="l", ylim = c(0, 180), lwd=3, col="purple", ylab="Pair difference", xlab="Number of learning trials")

Av_0.7_trial0 <- Av_0.7[which(Av_0.7$Trial_n==0),]
Av_0.7_trial_last <- Av_0.7[which(Av_0.7$Trial_n==25),]

wilcox.test(Av_0.7_trial0$Pair_diff, Av_0.7_trial_last$Pair_diff)


#0.9:0.1 leadership weighting

Av_0.9_meds_pd <- c()
for (i in Trials){
  Trial_i <- subset(Av_0.9, Av_0.9$Trial_n==i)
  Av_0.9_meds_pd[i+1] <- median(abs(Trial_i$Pair_diff)*360)
}
plot(Av_0.9_meds_pd~Trials, type="l", ylim = c(0, 180), lwd=3, col="blue", ylab="Pair difference", xlab="Number of learning trials")
for (i in unique(Av_0.9$Pair_n)){
  pair <- unique(Av_0.9$Pair_n)[i]
  all_pair <- subset(Av_0.9, Av_0.9$Pair_n==pair)
  lines((Pair_diff*360)~Trial_n, data=all_pair, col=alpha("black", 0.4), lwd=1.5)
}
lines(Av_0.9_meds_pd~Trials, type="l", ylim = c(0, 180), lwd=3, col="blue", ylab="Pair difference", xlab="Number of learning trials")

Av_0.9_trial0 <- Av_0.9[which(Av_0.9$Trial_n==0),]
Av_0.9_trial_last <- Av_0.9[which(Av_0.9$Trial_n==25),]

wilcox.test(Av_0.9_trial0$Pair_diff, Av_0.9_trial_last$Pair_diff)


plot(Av_0.5_meds_pd~Trials, type="l", ylim = c(0, 180), lwd=3, col="red", bty="l",
     ylab="Pair difference", xlab="Number of learning trials")
lines(Av_0.7_meds_pd~Trials, type="l", ylim = c(0, 180), lwd=3, col="purple", ylab="Pair difference", xlab="Number of learning trials")
lines(Av_0.9_meds_pd~Trials, type="l", ylim = c(0, 180), lwd=3, col="blue", ylab="Pair difference", xlab="Number of learning trials")


pdf("/Users/joemorford/Desktop/CL_NNs/14122021_figs/Pair_diff_Averaging.pdf", width=2.5, height=2.5,
    pointsize=7)
plot(Av_0.5_meds_pd~Trials, type="l", ylim = c(0, 180), lwd=1.2, col="red", bty="l",
     ylab="Pair difference", xlab="Number of learning trials")
lines(Av_0.7_meds_pd~Trials, type="l", ylim = c(0, 180), lwd=1.2, col="purple", ylab="Pair difference", xlab="Number of learning trials")
lines(Av_0.9_meds_pd~Trials, type="l", ylim = c(0, 180), lwd=1.2, col="blue", ylab="Pair difference", xlab="Number of learning trials")
legend("topright", legend=c("0.5:0.5", "0.7:0.3", "0.9:0.1"),
       col=c("red", "purple", "blue"), lty=1, cex=1, box.lty=0, lwd=1.2)

dev.off()

#####  Group membership benefit (with Averaging decision-making rule)
#The difference between performance of a pair compared to a constituent Solo of a pair

#0.5:0.5 leadership weighting

Av_0.5_meds_gmb <- c()
for (i in Trials){
  Trial_i <- subset(Av_0.5, Av_0.5$Trial_n==i)
  Av_0.5_meds_gmb[i+1] <- median(c(Trial_i$Leader_collective_membership_benefit, Trial_i$Follower_collective_membership_benefit)*360)
}
plot(Av_0.5_meds_gmb~Trials, type="l", ylim = c(-180, 180), lty="dotdash", lwd=3, col="red", ylab="Group benefit", xlab="Number of learning trials")
for (i in unique(Av_0.5$Pair_n)){
  pair <- unique(Av_0.5$Pair_n)[i]
  all_pair <- subset(Av_0.5, Av_0.5$Pair_n==pair)
  lines((Leader_collective_membership_benefit*360)~Trial_n, data=all_pair, lty="dotdash", col=alpha("black", 0.4))
  lines((Follower_collective_membership_benefit*360)~Trial_n, data=all_pair, lty="dotdash", col=alpha("black", 0.4))
}
lines(Av_0.5_meds_gmb~Trials, type="l", ylim = c(-180, 180), lty="dotdash", lwd=3, col="red", ylab="Group benefit", xlab="Number of learning trials")

gmb_0.5_all_Solos_trial0 <- c(Av_0.5_trial0$Leader_collective_membership_benefit, Av_0.5_trial0$Follower_collective_membership_benefit)
gmb_0.5_all_Solos_trial_last <- c(Av_0.5_trial_last$Leader_collective_membership_benefit, Av_0.5_trial_last$Follower_collective_membership_benefit)
wilcox.test(gmb_0.5_all_Solos_trial_last, gmb_0.5_all_Solos_trial0)

#0.7:0.3 leadership weighting

Av_0.7_meds_lgmb <- c()
#Leader group benefit
Av_0.7_meds_fgmb <- c()
#Follower group benefit
for (i in Trials){
  Trial_i <- subset(Av_0.7, Av_0.7$Trial_n==i)
  Av_0.7_meds_lgmb[i+1] <- median(Trial_i$Leader_collective_membership_benefit*360)
  Av_0.7_meds_fgmb[i+1] <- median(Trial_i$Follower_collective_membership_benefit*360)  
}
plot(Av_0.7_meds_lgmb~Trials, type="l", ylim = c(-180, 180), lty="dashed", lwd=3, col="purple", ylab="Group benefit", xlab="Number of learning trials")

for (i in unique(Av_0.7$Pair_n)){
  pair <- unique(Av_0.7$Pair_n)[i]
  all_pair <- subset(Av_0.7, Av_0.7$Pair_n==pair)
  lines((Leader_collective_membership_benefit*360)~Trial_n, data=all_pair, lty="dashed", col=alpha("black", 0.4))
}
lines(Av_0.7_meds_lgmb~Trials, type="l", ylim = c(-180, 180), lty="dashed", lwd=3, col="purple", ylab="Group benefit", xlab="Number of learning trials")

plot(Av_0.7_meds_fgmb~Trials, type="l", ylim = c(-180, 180), lty="dotted", lwd=3, col="purple", ylab="Group benefit", xlab="Number of learning trials")
for (i in unique(Av_0.7$Pair_n)){
  pair <- unique(Av_0.7$Pair_n)[i]
  all_pair <- subset(Av_0.7, Av_0.7$Pair_n==pair)
  lines((Follower_collective_membership_benefit*360)~Trial_n, data=all_pair, lty="dotted", col=alpha("black", 0.4))
}
lines(Av_0.7_meds_fgmb~Trials, type="l", ylim = c(-180, 180), lty="dotted", lwd=3, col="purple", ylab="Group benefit", xlab="Number of learning trials")

gmb_0.7_leader_trial0 <- Av_0.7_trial0$Leader_collective_membership_benefit
gmb_0.7_leader_trial_last <- Av_0.7_trial_last$Leader_collective_membership_benefit
wilcox.test(gmb_0.7_leader_trial0, gmb_0.7_leader_trial_last)

gmb_0.7_follower_trial0 <- Av_0.7_trial0$Follower_collective_membership_benefit
gmb_0.7_follower_trial_last <- Av_0.7_trial_last$Follower_collective_membership_benefit
wilcox.test(gmb_0.7_follower_trial0, gmb_0.7_follower_trial_last)


#0.9:0.1 leadership weighting

Av_0.9_meds_lgmb <- c()
#leader group benefit
Av_0.9_meds_fgmb <- c()
#Follower group benefit
for (i in Trials){
  Trial_i <- subset(Av_0.9, Av_0.9$Trial_n==i)
  Av_0.9_meds_lgmb[i+1] <- median(Trial_i$Leader_collective_membership_benefit*360)
  Av_0.9_meds_fgmb[i+1] <- median(Trial_i$Follower_collective_membership_benefit*360)  
}

plot(Av_0.9_meds_lgmb~Trials, type="l", ylim = c(-180, 180), lty="dashed", lwd=3, col="blue", ylab="Group benefit", xlab="Number of learning trials")
for (i in unique(Av_0.9$Pair_n)){
  pair <- unique(Av_0.9$Pair_n)[i]
  all_pair <- subset(Av_0.9, Av_0.9$Pair_n==pair)
  lines((Leader_collective_membership_benefit*360)~Trial_n, data=all_pair, lty="dashed", col=alpha("black", 0.4))
}
lines(Av_0.9_meds_lgmb~Trials, type="l", ylim = c(-180, 180), lty="dashed", lwd=3, col="blue", ylab="Group benefit", xlab="Number of learning trials")

plot(Av_0.9_meds_fgmb~Trials, type="l", ylim = c(-180, 180), lty="dotted", lwd=3, col="blue", ylab="Group benefit", xlab="Number of learning trials")
for (i in unique(Av_0.9$Pair_n)){
  pair <- unique(Av_0.9$Pair_n)[i]
  all_pair <- subset(Av_0.9, Av_0.9$Pair_n==pair)
  lines((Follower_collective_membership_benefit*360)~Trial_n, data=all_pair, lty="dotted", col=alpha("black", 0.4))
}
lines(Av_0.9_meds_fgmb~Trials, type="l", ylim = c(-180, 180), lty="dotted", lwd=3, col="blue", ylab="Group benefit", xlab="Number of learning trials")


gmb_0.9_leader_trial0 <- Av_0.9_trial0$Leader_collective_membership_benefit
gmb_0.9_leader_trial_last <- Av_0.9_trial_last$Leader_collective_membership_benefit
wilcox.test(gmb_0.9_leader_trial0, gmb_0.9_leader_trial_last)
hist(gmb_0.9_follower_trial0*360)
gmb_0.9_follower_trial0 <- Av_0.9_trial0$Follower_collective_membership_benefit
gmb_0.9_follower_trial_last <- Av_0.9_trial_last$Follower_collective_membership_benefit
wilcox.test(gmb_0.9_follower_trial0, gmb_0.9_follower_trial_last)


plot(Av_0.5_meds_gmb~Trials, type="l", ylim = c(-0.55*360,0.55*360), lty="dotdash", lwd=3, col="red", ylab="Collective membership gain", xlab="Number of learning trials", yaxs="i")
lines(Av_0.7_meds_lgmb~Trials, type="l", ylim = c(-0.55*360,0.55*360), lty="dashed", lwd=3, col="purple")
lines(Av_0.7_meds_fgmb~Trials, type="l", ylim = c(-0.55*360,0.55*360), lty="dotted", lwd=3, col="purple")
lines(Av_0.9_meds_lgmb~Trials, type="l", ylim = c(-0.55*360,0.55*360), lty="dashed", lwd=3, col="blue")
lines(Av_0.9_meds_fgmb~Trials, type="l", ylim = c(-0.55*360,0.55*360), lty="dotted", lwd=3, col="blue")
abline(h=0)

pdf("/Users/joemorford/Desktop/CL_NNs/14122021_figs/Averaging_collective_membership_benefit_change.pdf", width=2.5, height=2.5,
    pointsize=7)
plot(NA, ylim = c(-120, 120), xlim=c(0,24),
     ylab="Collective membership gain", xlab="Number of learning trials", yaxs="i", bty="l")
abline(h=0, col="grey")
axis(2)
lines(Av_0.5_meds_gmb~Trials, type="l", ylim = c(-120, 120), lty="dotdash", lwd=1.2, col="red", 
      ylab="Collective membership gain", xlab="Number of learning trials", yaxs="i", bty="l")
lines(Av_0.7_meds_lgmb~Trials, type="l", lty="dashed", lwd=1.2, col="purple")
lines(Av_0.7_meds_fgmb~Trials, type="l",  lty="dotted", lwd=1.2, col="purple")
lines(Av_0.9_meds_lgmb~Trials, type="l",  lty="dashed", lwd=1.2, col="blue")
lines(Av_0.9_meds_fgmb~Trials, type="l", lty="dotted", lwd=1.2, col="blue")
legend("bottomright", legend=c("0.5:0.5", "0.7:0.3 leaders", "0.7:0.3 followers",
                           "0.9:0.1 leaders", "0.9:0.1 followers"),
       col=c("red", "purple", "purple", "blue", "blue"), 
       lty=c(4, 2, 3, 2, 3), cex=0.85, 
       box.lty=0, lwd=1.2)

dev.off()




########

beeswarm(list(c(Av_0.5$Leader_collective_membership_benefit[Av_0.5$Trial_n==25]*360, Av_0.5$Follower_collective_membership_benefit[Av_0.5$Trial_n==25]*360),
              Av_0.7$Leader_collective_membership_benefit[Av_0.7$Trial_n==25]*360, Av_0.7$Follower_collective_membership_benefit[Av_0.7$Trial_n==25]*360,
              Av_0.9$Leader_collective_membership_benefit[Av_0.9$Trial_n==25]*360, Av_0.9$Follower_collective_membership_benefit[Av_0.9$Trial_n==25]*360),
         corral="wrap", method="swarm", col = "black", bg = alpha(c("red", "purple", "purple", "blue", "blue"), 0.5), 
         pch=21, cex=1,ylim=c(-0.55*360,0.55*360), xlim=c(0.5,5.5),
         axes=F, ylab="Collective membership gain", xaxs="i", yaxs="i")
box()
axis(2, tck=-0.027, lwd=0, lwd.ticks=1)
axis(1, at=c(1,2,3,4,5), labels=c("0.5 : 0.5\nAll","0.7 : 0.3\nLeaders","0.7 : 0.3\nFollowers",
                                  "0.9 : 0.1\nLeaders","0.9 : 0.1\nFollowers"), tck=0)
abline(h=0)
axis(1, at=c(0.5,1.5,2.5,3.5,4.5,5.5), tck=-0.027, labels=FALSE)
axis(3, at=c(1.5,2.5,3.5,4.5), tck=1, labels=FALSE, lwd=0, lwd.ticks=1)

###############

collective_membership_benefits_last_trial <- c(gmb_0.5_all_Solos_trial_last, gmb_0.7_leader_trial_last, gmb_0.7_follower_trial_last,
                               gmb_0.9_leader_trial_last, gmb_0.9_follower_trial_last)
lead_ratios <- c(rep("0.5", 500), rep("0.7_l", 250), rep("0.7_f", 250), 
                 rep("0.9_l", 250), rep("0.9_f", 250))

pairwise.wilcox.test(collective_membership_benefits_last_trial, lead_ratios, p.adjust.method = "none")

#######  Error correlation between Solos with Averaging decision-making rule
#Correlation between leader and follower - or randomly assigned for 0.5:0.5 weighting pairs


#0.5:0.5 leadership weighting
#Leader and follower abitrarily determined
Av_l_0.5_25 <- (360*Avs_25s$Leader[Avs_25s$LF==0.5])
#Leader error at test trial 25
Av_f_0.5_25 <- (360*Avs_25s$Follower[Avs_25s$LF==0.5])
#Follower error at test trial 25

Av_m1 <- lm(Av_l_0.5_25 ~ Av_f_0.5_25)
summary(Av_m1)

#0.7:0.3 leadership weighting

Av_l_0.7_25 <- (360*Avs_25s$Leader[Avs_25s$LF==0.7])
Av_f_0.7_25 <- (360*Avs_25s$Follower[Avs_25s$LF==0.7])
Av_m2 <- lm(Av_l_0.7_25 ~ Av_f_0.7_25)
summary(Av_m2)

#0.9:0.1 leadership weighting
Av_l_0.9_25 <- (360*Avs_25s$Leader[Avs_25s$LF==0.9])
Av_f_0.9_25 <- (360*Avs_25s$Follower[Avs_25s$LF==0.9])
Av_m3 <- lm(Av_l_0.9_25 ~ Av_f_0.9_25)
summary(Av_m3)

plot_data <- data.frame("Leader"=c(Av_l_0.5_25, Av_l_0.7_25, Av_l_0.9_25), 
                        "Follower"=c(Av_f_0.5_25, Av_f_0.7_25, Av_f_0.9_25),
                        "Ratio"=c(rep(0.5, length(Av_l_0.5_25)), rep(0.7, length(Av_l_0.7_25)),
                                  rep(0.9, length(Av_l_0.9_25)))
)
plot_data <- plot_data[sample(c(1:nrow(plot_data)), replace=F), ]                        

plot(Leader ~ Follower, data=plot_data, xlab="Agent 2 (Follower) Error", ylab="Agent 1 (Leader) Error", 
     xlim=c(-180, 180), ylim=c(-180, 180), col=ifelse(as.factor(Ratio)==0.5, "red", ifelse(as.factor(Ratio)==0.7, "purple", "blue")), pch=16, axes=FALSE)
#Theoretical expectations
lines((0+-1*seq(-200,200))~seq(-200,200), col="red")
lines((0+-(3/7)*seq(-200,200))~seq(-200,200), col="purple")
lines((0+-(1/9)*seq(-200,200))~seq(-200,200), col="blue")
box()
axis(1, at=seq(-180, 180, 90))
axis(2, at=seq(-180, 180, 90))
legend("topright", legend=c("0.5:0.5", "0.7:0.3", "0.9:0.1"),
       col=c("red", "purple", "blue"), cex=1, pt.cex=0.7, pch=16)


pdf("/Users/joemorford/Desktop/CL_NNs/14122021_figs/Averaging_error_correlation.pdf", width=2.5, height=2.5,
    pointsize=7)
plot(Leader ~ Follower, data=plot_data, xlab="Agent 2 (Follower) Error", ylab="Agent 1 (Leader) Error", 
     xlim=c(-180, 180), ylim=c(-180, 180), col=ifelse(as.factor(Ratio)==0.5, "red", ifelse(as.factor(Ratio)==0.7, "purple", "blue")), pch=16, axes=FALSE)
#Theoretical expectations
lines((0+-1*seq(-200,200))~seq(-200,200), col="red")
lines((0+-(3/7)*seq(-200,200))~seq(-200,200), col="purple")
lines((0+-(1/9)*seq(-200,200))~seq(-200,200), col="blue")
box()
axis(1, at=seq(-180, 180, 90))
axis(2, at=seq(-180, 180, 90))
legend("topright", legend=c("0.5:0.5", "0.7:0.3", "0.9:0.1"),
       col=c("red", "purple", "blue"), cex=1, pt.cex=0.7, pch=16)

dev.off()

########################


####### #############Pairs of agents with probabilistic decision-making rule - performance
#Despotic pairs



#### Collective performance - probabilistic pairs

#0.5:0.5 leadership weighting

Pb_0.5_meds <- c()
for (i in Trials){
  Trial_i <- subset(Pb_0.5, Pb_0.5$Trial_n==i)
  Pb_0.5_meds[i+1] <- median(Trial_i$Collective)*360
}

better_worse <- c()
plot(Pb_0.5_meds~Trials, type="l", ylim = c(0, 180), lwd=3, col="red", ylab="Collective absolute angular error", xlab="Number of learning trials")
for (i in unique(Pb_0.5$Pair_n)){
  pair <- unique(Pb_0.5$Pair_n)[i]
  all_pair <- subset(Pb_0.5, Pb_0.5$Pair_n==pair)
  lines((Collective*360)~Trial_n, data=all_pair, col=alpha("black", 0.4), lwd=1.5)
  if (all_pair$Collective[1]>all_pair$Collective[nrow(all_pair)]){
    better_worse[i] <- "B"
  }else{
    better_worse[i] <- "W"
  }
}
lines(Pb_0.5_meds~Trials, type="l", ylim = c(0, 180), lwd=3, col="red", ylab="Collective absolute angular error", xlab="Number of learning trials")
better_worse
length(better_worse[better_worse=="W"])
#How many pairs of agents did not improve in performance?

#0.7:0.3 leadership weighting

Pb_0.7_meds <- c()
for (i in Trials){
  Trial_i <- subset(Pb_0.7, Pb_0.7$Trial_n==i)
  Pb_0.7_meds[i+1] <- median(Trial_i$Collective)*360
}
better_worse <- c()
plot(Pb_0.7_meds~Trials, type="l", ylim = c(0, 180), lwd=3, col="purple", ylab="Collective absolute angular error", xlab="Number of learning trials")
for (i in unique(Pb_0.7$Pair_n)){
  pair <- unique(Pb_0.7$Pair_n)[i]
  all_pair <- subset(Pb_0.7, Pb_0.7$Pair_n==pair)
  lines((Collective*360)~Trial_n, data=all_pair, col=alpha("black", 0.4), lwd=1.5)
  if (all_pair$Collective[1]>all_pair$Collective[nrow(all_pair)]){
    better_worse[i] <- "B"
  }else{
    better_worse[i] <- "W"
  }
}
lines(Pb_0.7_meds~Trials, type="l", ylim = c(0, 180), lwd=3, col="purple", ylab="Collective absolute angular error", xlab="Number of learning trials")
better_worse
length(better_worse[better_worse=="W"])
#How many pairs of agents did not improve in performance?

#0.9:0.1 leadership weighting

Pb_0.9_meds <- c()
for (i in Trials){
  Trial_i <- subset(Pb_0.9, Pb_0.9$Trial_n==i)
  Pb_0.9_meds[i+1] <- median(Trial_i$Collective)*360
}
better_worse <- c()
plot(Pb_0.9_meds~Trials, type="l", ylim = c(0, 180), lwd=3, col="blue", ylab="Collective absolute angular error", xlab="Number of learning trials")
for (i in unique(Pb_0.9$Pair_n)){
  pair <- unique(Pb_0.9$Pair_n)[i]
  all_pair <- subset(Pb_0.9, Pb_0.7$Pair_n==pair)
  lines((Collective*360)~Trial_n, data=all_pair, col=alpha("black", 0.4), lwd=1.5)
  if (all_pair$Collective[1]>all_pair$Collective[nrow(all_pair)]){
    better_worse[i] <- "B"
  }else{
    better_worse[i] <- "W"
  }
}
lines(Pb_0.9_meds~Trials, type="l", ylim = c(0, 180), lwd=3, col="blue", ylab="Collective absolute angular error", xlab="Number of learning trials")
better_worse
length(better_worse[better_worse=="W"])
#How many pairs of agents did not improve in performance?


plot(Pb_0.5_meds~Trials, type="l", ylim = c(0, 180), lwd=3, col="red", bty="l",
     ylab="Collective absolute angular error", xlab="Number of learning trials")
lines(Pb_0.7_meds~Trials, type="l", ylim = c(0, 180), lwd=3, col="purple", ylab="Collective absolute angular error", xlab="Number of learning trials")
lines(Pb_0.9_meds~Trials, type="l", ylim = c(0, 180), lwd=3, col="blue", ylab="Collective absolute angular error", xlab="Number of learning trials")


pdf("/Users/joemorford/Desktop/CL_NNs/14122021_figs/Collective_probabilistic.pdf", width=2, height=2,
    pointsize=6)
plot(Pb_0.5_meds~Trials, type="l", ylim = c(0, 180), lwd=1.2, col="red", bty="l",
     ylab="Collective absolute angular error", xlab="Number of learning trials")
lines(Pb_0.7_meds~Trials, type="l", ylim = c(0, 180), lwd=1.2, col="purple", ylab="Collective absolute angular error", xlab="Number of learning trials")
lines(Pb_0.9_meds~Trials, type="l", ylim = c(0, 180), lwd=1.2, col="blue", ylab="Collective absolute angular error", xlab="Number of learning trials")
legend(12, 160, title="Decision-making\n weightings", legend=c("0.5:0.5", "0.7:0.3", "0.9:0.1"),
       col=c("red", "purple", "blue"), lty=1, cex=1, box.lty=0, lwd=1.2)
dev.off()

Pb_0.5$LF <- factor(0.5)
Pb_0.7$LF <- factor(0.7)
Pb_0.9$LF <- factor(0.9)

Pbs <- rbind(Pb_0.5, Pb_0.7, Pb_0.9)
Pbs$performance_pair_ID <- NA
for (i in 1:nrow(Pbs)){
  if (i==1){
    pair <- 1
    counter <- 1
  }
  if (Pbs$Pair_n[i] != pair){
    pair <- Pbs$Pair_n[i]
    counter <- counter + 1
  }
  Pbs$performance_pair_ID[i] <- counter
}
Pbs_5s <- subset(Pbs, Pbs$Trial_n==5)
Pbs_5s$Performance <- Pbs_5s$Collective
Pbs_25s <- subset(Pbs, Pbs$Trial_n==25)
Pbs_25s$Performance <- Pbs_25s$Collective


Solo$LF <- factor("Solo")
Solo$Performance <- Solo$Individual

Solo_5s <- subset(Solo, Solo$Trial_n==5)
Solo_Pbs_5s <- rbind(Solo_5s[,c(4,5)], Pbs_5s[,c(9,11)])
Solo_25s <- subset(Solo, Solo$Trial_n==25)
Solo_Pbs_25s <- rbind(Solo_25s[,c(4,5)], Pbs_25s[,c(9,11)])


test_Solo_Pbs_5s <- pairwise.wilcox.test(Solo_Pbs_5s$Performance, Solo_Pbs_5s$LF, p.adjust.method = "none")
test_Solo_Pbs_5s
#Pairwise MWU tests between different leadership weightings pairs of agents plus Solo agents
#At 25th test trial


test_Solo_Pbs_25s <- pairwise.wilcox.test(Solo_Pbs_25s$Performance, Solo_Pbs_25s$LF, p.adjust.method = "none")
test_Solo_Pbs_25s
#Pairwise MWU tests between different leadership weightings pairs of agents plus Solo agents
#At 25th test trial






#####  Solos performance within pairs with probabilistic decision-making

#0.5:0.5 leadership weighting

#Leader/follower is arbitrary with 0.5:0.5 weighting
Pb_0.5_meds_lf <- c()
for (i in Trials){
  Trial_i <- subset(Pb_0.5, Pb_0.5$Trial_n==i)
  Pb_0.5_meds_lf[i+1] <- median(abs(c(Trial_i$Leader, Trial_i$Follower))*360)
}
plot(Pb_0.5_meds_lf~Trials, type="l", ylim = c(0, 180), lty="dotdash", lwd=3, col="red", ylab="Solo absolute angular error", xlab="Number of learning trials")
for (i in unique(Pb_0.5$Pair_n)){
  pair <- unique(Pb_0.5$Pair_n)[i]
  all_pair <- subset(Pb_0.5, Pb_0.5$Pair_n==pair)
  lines((abs(Leader)*360)~Trial_n, data=all_pair, col=alpha("black", 0.4))
  lines((abs(Follower)*360)~Trial_n, data=all_pair, col=alpha("black", 0.4))
}
lines(Pb_0.5_meds_lf~Trials, type="l", ylim = c(0, 180), lty="dotdash", lwd=3, col="red", ylab="Solo absolute angular error", xlab="Number of learning trials")

#0.7:0.3 leadership weighting

Pb_0.7_meds_l <- c()
#leader
Pb_0.7_meds_f <- c()
#follower
for (i in Trials){
  Trial_i <- subset(Pb_0.7, Pb_0.7$Trial_n==i)
  Pb_0.7_meds_l[i+1] <- median(abs(Trial_i$Leader)*360)
  Pb_0.7_meds_f[i+1] <- median(abs(Trial_i$Follower)*360)  
}
plot(Pb_0.7_meds_l~Trials, type="l", ylim = c(0, 180), lty="dashed", lwd=3, col="purple", ylab="Solo absolute angular error", xlab="Number of learning trials")
for (i in unique(Pb_0.7$Pair_n)){
  pair <- unique(Pb_0.7$Pair_n)[i]
  all_pair <- subset(Pb_0.7, Pb_0.7$Pair_n==pair)
  lines((abs(Leader)*360)~Trial_n, data=all_pair, lty="dashed", col=alpha("black", 0.4))
}
lines(Pb_0.7_meds_l~Trials, type="l", ylim = c(0, 180), lty="dashed", lwd=3, col="purple", ylab="Solo absolute angular error", xlab="Number of learning trials")
plot(Pb_0.7_meds_f~Trials, type="l", ylim = c(0, 180), lty="dotted", lwd=3, col="purple", ylab="Solo absolute angular error", xlab="Number of learning trials")
for (i in unique(Pb_0.7$Pair_n)){
  pair <- unique(Pb_0.7$Pair_n)[i]
  all_pair <- subset(Pb_0.7, Pb_0.7$Pair_n==pair)
  lines((abs(Follower)*360)~Trial_n, data=all_pair, lty="dotted", col=alpha("black", 0.4))
}
lines(Pb_0.7_meds_f~Trials, type="l", ylim = c(0, 180), lty="dotted", lwd=3, col="purple", ylab="Solo absolute angular error", xlab="Number of learning trials")


#0.9:0.1 leadership weighting

Pb_0.9_meds_l <- c()
Pb_0.9_meds_f <- c()
for (i in Trials){
  Trial_i <- subset(Pb_0.9, Pb_0.9$Trial_n==i)
  Pb_0.9_meds_l[i+1] <- median(abs(Trial_i$Leader)*360)
  Pb_0.9_meds_f[i+1] <- median(abs(Trial_i$Follower)*360)  
}
plot(Pb_0.9_meds_l~Trials, type="l", ylim = c(0, 180), lty="dashed", lwd=3, col="blue", ylab="Solo absolute angular error", xlab="Number of learning trials")
for (i in unique(Pb_0.9$Pair_n)){
  pair <- unique(Pb_0.9$Pair_n)[i]
  all_pair <- subset(Pb_0.7, Pb_0.7$Pair_n==pair)
  lines((abs(Leader)*360)~Trial_n, data=all_pair, lty="dashed", col=alpha("black", 0.4))
}
lines(Pb_0.9_meds_l~Trials, type="l", ylim = c(0, 180), lty="dashed", lwd=3, col="blue", ylab="Solo absolute angular error", xlab="Number of learning trials")
plot(Pb_0.9_meds_f~Trials, type="l", ylim = c(0, 180), lty="dotted", lwd=3, col="blue", ylab="Solo absolute angular error", xlab="Number of learning trials")
for (i in unique(Pb_0.9$Pair_n)){
  pair <- unique(Pb_0.9$Pair_n)[i]
  all_pair <- subset(Pb_0.9, Pb_0.9$Pair_n==pair)
  lines((abs(Follower)*360)~Trial_n, data=all_pair, lty="dotted", col=alpha("black", 0.4))
}
lines(Pb_0.9_meds_f~Trials, type="l", ylim = c(0, 180), lty="dotted", lwd=3, col="blue", ylab="Solo absolute angular error", xlab="Number of learning trials")


plot(Pb_0.5_meds_lf~Trials, type="l", ylim = c(0, 180), lty="dotdash", lwd=3, col="red", ylab="Solo absolute angular error", xlab="Number of learning trials")
lines(Pb_0.7_meds_l~Trials, type="l", ylim = c(0, 180), lty="dashed", lwd=3, col="purple", ylab="Solo absolute angular error", xlab="Number of learning trials")
lines(Pb_0.7_meds_f~Trials, type="l", ylim = c(0, 180), lty="dotted", lwd=3, col="purple", ylab="Solo absolute angular error", xlab="Number of learning trials")
lines(Pb_0.9_meds_l~Trials, type="l", ylim = c(0, 180), lty="dashed", lwd=3, col="blue", ylab="Solo absolute angular error", xlab="Number of learning trials")
lines(Pb_0.9_meds_f~Trials, type="l", ylim = c(0, 180), lty="dotted", lwd=3, col="blue", ylab="Solo absolute angular error", xlab="Number of learning trials")


pdf("/Users/joemorford/Desktop/CL_NNs/14122021_figs/Solo_probabilistic.pdf", width=2, height=2,
    pointsize=6)
plot(Pb_0.5_meds_lf~Trials, type="l", ylim = c(0, 180), bty="l",
     lty="dotdash", lwd=1.2, col="red", ylab="Solo absolute angular error", xlab="Number of learning trials")
lines(Solo_meds~Trials, type="l", ylim = c(0, 180), lwd=1.2, col="black")
lines(Pb_0.7_meds_l~Trials, type="l", ylim = c(0, 180), lty="dashed", lwd=1.2, col="purple", ylab="Solo absolute angular error", xlab="Number of learning trials")
lines(Pb_0.7_meds_f~Trials, type="l", ylim = c(0, 180), lty="dotted", lwd=1.2, col="purple", ylab="Solo absolute angular error", xlab="Number of learning trials")
lines(Pb_0.9_meds_l~Trials, type="l", ylim = c(0, 180), lty="dashed", lwd=1.2, col="blue", ylab="Solo absolute angular error", xlab="Number of learning trials")
lines(Pb_0.9_meds_f~Trials, type="l", ylim = c(0, 180), lty="dotted", lwd=1.2, col="blue", ylab="Solo absolute angular error", xlab="Number of learning trials")
legend("topright", legend=c("0.5:0.5", "0.7:0.3 leaders", "0.7:0.3 followers",
                            "0.9:0.1 leaders", "0.9:0.1 followers", 
                            "Solo learners"),
       col=c("red", "purple", "purple", "blue", "blue", "black"), 
       lty=c(4, 2, 3, 2, 3, 1), cex=0.85, 
       box.lty=0, lwd=1.2)
dev.off()


Pbs_Solos_trial_last <- c(
  Pbs_25s$Leader[which(Pbs_25s$LF==0.5)],
  Pbs_25s$Follower[which(Pbs_25s$LF==0.5)],
  Pbs_25s$Leader[which(Pbs_25s$LF==0.7)],
  Pbs_25s$Follower[which(Pbs_25s$LF==0.7)],
  Pbs_25s$Leader[which(Pbs_25s$LF==0.9)],
  Pbs_25s$Follower[which(Pbs_25s$LF==0.9)],
  Solo_25s$Performance
)
Pbs_Solos_trial_last <- abs(Pbs_Solos_trial_last)
Category <- c(rep("0.5", 500), rep("0.7_l", 250), rep("0.7_f", 250), 
              rep("0.9_l", 250), rep("0.9_f", 250), rep("Solo", 250))

pairwise.wilcox.test(Pbs_Solos_trial_last, Category)

Pbs_Solos_trial_5th <- c(
  Pbs_5s$Leader[which(Pbs_5s$LF==0.5)],
  Pbs_5s$Follower[which(Pbs_5s$LF==0.5)],
  Pbs_5s$Leader[which(Pbs_5s$LF==0.7)],
  Pbs_5s$Follower[which(Pbs_5s$LF==0.7)],
  Pbs_5s$Leader[which(Pbs_5s$LF==0.9)],
  Pbs_5s$Follower[which(Pbs_5s$LF==0.9)],
  Solo_5s$Performance
)
Pbs_Solos_trial_5th <- abs(Pbs_Solos_trial_5th)

pairwise.wilcox.test(Pbs_Solos_trial_5th, Category)


#####  Pair difference (in pairs with probabilistic decision-making rule)

#0.5:0.5 leadership weighting

Pb_0.5_meds_pd <- c()
for (i in Trials){
  Trial_i <- subset(Pb_0.5, Pb_0.5$Trial_n==i)
  Pb_0.5_meds_pd[i+1] <- median(abs(Trial_i$Pair_diff)*360)
}
plot(Pb_0.5_meds_pd~Trials, type="l", ylim = c(0, 180), lwd=3, col="red", ylab="Pair difference", xlab="Number of learning trials")
for (i in unique(Pb_0.5$Pair_n)){
  pair <- unique(Pb_0.5$Pair_n)[i]
  all_pair <- subset(Pb_0.5, Pb_0.5$Pair_n==pair)
  lines((Pair_diff*360)~Trial_n, data=all_pair, col=alpha("black", 0.4), lwd=1.5)
}
lines(Pb_0.5_meds_pd~Trials, type="l", ylim = c(0, 180), lwd=3, col="red", ylab="Pair difference", xlab="Number of learning trials")

Pb_0.5_trial0 <- Pb_0.5[which(Pb_0.5$Trial_n==0),]
Pb_0.5_trial_last <- Pb_0.5[which(Pb_0.5$Trial_n==25),]

wilcox.test(Pb_0.5_trial0$Pair_diff, Pb_0.5_trial_last$Pair_diff)


#0.7:0.3 leadership weighting

Pb_0.7_meds_pd <- c()
for (i in Trials){
  Trial_i <- subset(Pb_0.7, Pb_0.7$Trial_n==i)
  Pb_0.7_meds_pd[i+1] <- median(abs(Trial_i$Pair_diff)*360)
}
plot(Pb_0.7_meds_pd~Trials, type="l", ylim = c(0, 180), lwd=3, col="purple", ylab="Pair difference", xlab="Number of learning trials")
for (i in unique(Pb_0.7$Pair_n)){
  pair <- unique(Pb_0.7$Pair_n)[i]
  all_pair <- subset(Pb_0.7, Pb_0.5$Pair_n==pair)
  lines((Pair_diff*360)~Trial_n, data=all_pair, col=alpha("black", 0.4), lwd=1.5)
}
lines(Pb_0.7_meds_pd~Trials, type="l", ylim = c(0, 180), lwd=3, col="purple", ylab="Pair difference", xlab="Number of learning trials")

Pb_0.7_trial0 <- Pb_0.7[which(Pb_0.7$Trial_n==0),]
Pb_0.7_trial_last <- Pb_0.7[which(Pb_0.7$Trial_n==25),]

wilcox.test(Pb_0.7_trial0$Pair_diff, Pb_0.7_trial_last$Pair_diff)


#0.9:0.1 leadership weighting
Pb_0.9_meds_pd <- c()
for (i in Trials){
  Trial_i <- subset(Pb_0.9, Pb_0.9$Trial_n==i)
  Pb_0.9_meds_pd[i+1] <- median(abs(Trial_i$Pair_diff)*360)
}
plot(Pb_0.9_meds_pd~Trials, type="l", ylim = c(0, 180), lwd=3, col="blue", ylab="Pair difference", xlab="Number of learning trials")
for (i in unique(Pb_0.9$Pair_n)){
  pair <- unique(Pb_0.9$Pair_n)[i]
  all_pair <- subset(Pb_0.9, Pb_0.9$Pair_n==pair)
  lines((Pair_diff*360)~Trial_n, data=all_pair, col=alpha("black", 0.4), lwd=1.5)
}
lines(Pb_0.9_meds_pd~Trials, type="l", ylim = c(0, 180), lwd=3, col="blue", ylab="Pair difference", xlab="Number of learning trials")

Pb_0.9_trial0 <- Pb_0.9[which(Pb_0.9$Trial_n==0),]
Pb_0.9_trial_last <- Pb_0.9[which(Pb_0.9$Trial_n==25),]

wilcox.test(Pb_0.9_trial0$Pair_diff, Pb_0.9_trial_last$Pair_diff)


plot(Pb_0.5_meds_pd~Trials, type="l", ylim = c(0, 180), lwd=3, col="red", ylab="Pair difference", xlab="Number of learning trials")
lines(Pb_0.7_meds_pd~Trials, type="l", ylim = c(0, 180), lwd=3, col="purple", ylab="Pair difference", xlab="Number of learning trials")
lines(Pb_0.9_meds_pd~Trials, type="l", ylim = c(0, 180), lwd=3, col="blue", ylab="Pair difference", xlab="Number of learning trials")


pdf("/Users/joemorford/Desktop/CL_NNs/14122021_figs/Probabilistic_pair_diff.pdf", width=2, height=2,
    pointsize=6)
plot(Pb_0.5_meds_pd~Trials, type="l", ylim = c(0, 180), bty="l",
     lwd=1.2, col="red", ylab="Pair difference", xlab="Number of learning trials")
lines(Pb_0.7_meds_pd~Trials, type="l", ylim = c(0, 180), lwd=1.2, col="purple", ylab="Pair difference", xlab="Number of learning trials")
lines(Pb_0.9_meds_pd~Trials, type="l", ylim = c(0, 180), lwd=1.2, col="blue", ylab="Pair difference", xlab="Number of learning trials")
legend("topright", legend=c("0.5:0.5", "0.7:0.3", "0.9:0.1"),
       col=c("red", "purple", "blue"), lty=1, cex=1, box.lty=0, lwd=1.2)
dev.off()





#####  Group membership benefit (with probabilistic decision-making rule)
#The difference between performance of a pair compared to a constituent Solo of a pair

#0.5:0.5 leadership weighting

Pb_0.5_meds_gmb <- c()
for (i in Trials){
  Trial_i <- subset(Pb_0.5, Pb_0.5$Trial_n==i)
  Pb_0.5_meds_gmb[i+1] <- median(c(Trial_i$Leader_collective_membership_benefit, Trial_i$Follower_collective_membership_benefit)*360)
}
plot(Pb_0.5_meds_gmb~Trials, type="l", ylim = c(-180, 180), lty="dotdash", lwd=3, col="red", ylab="Group benefit", xlab="Number of learning trials")
for (i in unique(Pb_0.5$Pair_n)){
  pair <- unique(Pb_0.5$Pair_n)[i]
  all_pair <- subset(Pb_0.5, Pb_0.5$Pair_n==pair)
  lines((Leader_collective_membership_benefit*360)~Trial_n, data=all_pair, lty="dotdash", col=alpha("black", 0.4))
  lines((Follower_collective_membership_benefit*360)~Trial_n, data=all_pair, lty="dotdash", col=alpha("black", 0.4))
}
lines(Pb_0.5_meds_gmb~Trials, type="l", ylim = c(-180, 180), lty="dotdash", lwd=3, col="red", ylab="Group benefit", xlab="Number of learning trials")

gmb_0.5_all_Solos_trial0 <- c(Pb_0.5_trial0$Leader_collective_membership_benefit, Pb_0.5_trial0$Follower_collective_membership_benefit)
gmb_0.5_all_Solos_trial_last <- c(Pb_0.5_trial_last$Leader_collective_membership_benefit, Pb_0.5_trial_last$Follower_collective_membership_benefit)
wilcox.test(gmb_0.5_all_Solos_trial_last, gmb_0.5_all_Solos_trial0)

#0.7:0.3 leadership weighting

Pb_0.7_meds_lgmb <- c()
#Leader group benefit
Pb_0.7_meds_fgmb <- c()
#Follower group benefit
for (i in Trials){
  Trial_i <- subset(Pb_0.7, Pb_0.7$Trial_n==i)
  Pb_0.7_meds_lgmb[i+1] <- median(Trial_i$Leader_collective_membership_benefit*360)
  Pb_0.7_meds_fgmb[i+1] <- median(Trial_i$Follower_collective_membership_benefit*360)  
}

plot(Pb_0.7_meds_lgmb~Trials, type="l", ylim = c(-180, 180), lty="dashed", lwd=3, col="purple", ylab="Group benefit", xlab="Number of learning trials")
for (i in unique(Pb_0.7$Pair_n)){
  pair <- unique(Pb_0.7$Pair_n)[i]
  all_pair <- subset(Pb_0.7, Pb_0.7$Pair_n==pair)
  lines((Leader_collective_membership_benefit*360)~Trial_n, data=all_pair, lty="dashed", col=alpha("black", 0.4))
}
lines(Pb_0.7_meds_lgmb~Trials, type="l", ylim = c(-180, 180), lty="dashed", lwd=3, col="purple", ylab="Group benefit", xlab="Number of learning trials")
plot(Pb_0.7_meds_fgmb~Trials, type="l", ylim = c(-180, 180), lty="dotted", lwd=3, col="purple", ylab="Group benefit", xlab="Number of learning trials")
for (i in unique(Pb_0.7$Pair_n)){
  pair <- unique(Pb_0.7$Pair_n)[i]
  all_pair <- subset(Pb_0.7, Pb_0.7$Pair_n==pair)
  lines((Follower_collective_membership_benefit*360)~Trial_n, data=all_pair, lty="dotted", col=alpha("black", 0.4))
}
lines(Pb_0.7_meds_fgmb~Trials, type="l", ylim = c(-180, 180), lty="dotted", lwd=3, col="purple", ylab="Group benefit", xlab="Number of learning trials")

gmb_0.7_leader_trial0 <- Pb_0.7_trial0$Leader_collective_membership_benefit
gmb_0.7_leader_trial_last <- Pb_0.7_trial_last$Leader_collective_membership_benefit
wilcox.test(gmb_0.7_leader_trial0, gmb_0.7_leader_trial_last)
gmb_0.7_follower_trial0 <- Pb_0.7_trial0$Follower_collective_membership_benefit
gmb_0.7_follower_trial_last <- Pb_0.7_trial_last$Follower_collective_membership_benefit
wilcox.test(gmb_0.7_follower_trial0, gmb_0.7_follower_trial_last)

#0.9:0.1 leadership weighting

Pb_0.9_meds_lgmb <- c()
#Leader group benefit
Pb_0.9_meds_fgmb <- c()
#Folloer group benefit
for (i in Trials){
  Trial_i <- subset(Pb_0.9, Pb_0.9$Trial_n==i)
  Pb_0.9_meds_lgmb[i+1] <- median(Trial_i$Leader_collective_membership_benefit*360)
  Pb_0.9_meds_fgmb[i+1] <- median(Trial_i$Follower_collective_membership_benefit*360)  
}
plot(Pb_0.9_meds_lgmb~Trials, type="l", ylim = c(-180, 180), lty="dashed", lwd=3, col="blue", ylab="Group benefit", xlab="Number of learning trials")
for (i in unique(Pb_0.9$Pair_n)){
  pair <- unique(Pb_0.9$Pair_n)[i]
  all_pair <- subset(Pb_0.9, Pb_0.9$Pair_n==pair)
  lines((Leader_collective_membership_benefit*360)~Trial_n, data=all_pair, lty="dashed", col=alpha("black", 0.4))
}
lines(Pb_0.9_meds_lgmb~Trials, type="l", ylim = c(-180, 180), lty="dashed", lwd=3, col="blue", ylab="Group benefit", xlab="Number of learning trials")
plot(Pb_0.9_meds_fgmb~Trials, type="l", ylim = c(-180, 180), lty="dotted", lwd=3, col="blue", ylab="Group benefit", xlab="Number of learning trials")
for (i in unique(Pb_0.9$Pair_n)){
  pair <- unique(Pb_0.9$Pair_n)[i]
  all_pair <- subset(Pb_0.9, Pb_0.9$Pair_n==pair)
  lines((Follower_collective_membership_benefit*360)~Trial_n, data=all_pair, lty="dotted", col=alpha("black", 0.4))
}
lines(Pb_0.9_meds_fgmb~Trials, type="l", ylim = c(-180, 180), lty="dotted", lwd=3, col="blue", ylab="Group benefit", xlab="Number of learning trials")


gmb_0.9_leader_trial0 <- Pb_0.9_trial0$Leader_collective_membership_benefit
gmb_0.9_leader_trial_last <- Pb_0.9_trial_last$Leader_collective_membership_benefit
wilcox.test(gmb_0.9_leader_trial0, gmb_0.9_leader_trial_last)
gmb_0.9_follower_trial0 <- Pb_0.9_trial0$Follower_collective_membership_benefit
gmb_0.9_follower_trial_last <- Pb_0.9_trial_last$Follower_collective_membership_benefit
wilcox.test(gmb_0.9_follower_trial0, gmb_0.9_follower_trial_last)
mean(gmb_0.9_leader_trial0)

plot(Pb_0.5_meds_gmb~Trials, type="l", ylim = c(-0.55*360,0.55*360), lty="dotdash", lwd=3, col="red", ylab="Collective membership gain", xlab="Number of learning trials", yaxs="i")
lines(Pb_0.7_meds_lgmb~Trials, type="l", ylim = c(-0.55*360,0.55*360), lty="dashed", lwd=3, col="purple", ylab="Collective membership gain", xlab="Number of learning trials")
lines(Pb_0.7_meds_fgmb~Trials, type="l", ylim = c(-0.55*360,0.55*360), lty="dotted", lwd=3, col="purple", ylab="Collective membership gain", xlab="Number of learning trials")
lines(Pb_0.9_meds_lgmb~Trials, type="l", ylim = c(-0.55*360,0.55*360), lty="dashed", lwd=3, col="blue", ylab="Collective membership gain", xlab="Number of learning trials")
lines(Pb_0.9_meds_fgmb~Trials, type="l", ylim = c(-0.55*360,0.55*360), lty="dotted", lwd=3, col="blue", ylab="Collective membership gain", xlab="Number of learning trials")
abline(h=0)

pdf("/Users/joemorford/Desktop/CL_NNs/14122021_figs/Probabilistic_collective_membership_benefit_change.pdf", width=2, height=2,
    pointsize=6)
plot(NA, ylim = c(-120, 120), xlim=c(0,24),
     ylab="Collective membership gain", xlab="Number of learning trials", yaxs="i", bty="l")
abline(h=0, col="grey")
axis(2)
lines(Pb_0.5_meds_gmb~Trials, type="l", lty="dotdash", lwd=1.2, col="red", ylab="Collective membership gain", xlab="Number of learning trials", yaxs="i")
lines(Pb_0.7_meds_lgmb~Trials, type="l", lty="dashed", lwd=1.2, col="purple", ylab="Collective membership gain", xlab="Number of learning trials")
lines(Pb_0.7_meds_fgmb~Trials, type="l", lty="dotted", lwd=1.2, col="purple", ylab="Collective membership gain", xlab="Number of learning trials")
lines(Pb_0.9_meds_lgmb~Trials, type="l", lty="dashed", lwd=1.2, col="blue", ylab="Collective membership gain", xlab="Number of learning trials")
lines(Pb_0.9_meds_fgmb~Trials, type="l", lty="dotted", lwd=1.2, col="blue", ylab="Collective membership gain", xlab="Number of learning trials")
legend("bottomright", legend=c("0.5:0.5", "0.7:0.3 leaders", "0.7:0.3 followers",
                               "0.9:0.1 leaders", "0.9:0.1 followers"),
       col=c("red", "purple", "purple", "blue", "blue"), 
       lty=c(4, 2, 3, 2, 3), cex=0.85, 
       box.lty=0, lwd=1.2)

dev.off()
#######


beeswarm(list(c(Pb_0.5$Leader_collective_membership_benefit[Pb_0.5$Trial_n==25]*360, Pb_0.5$Follower_collective_membership_benefit[Pb_0.5$Trial_n==25]*360),
              Pb_0.7$Leader_collective_membership_benefit[Pb_0.7$Trial_n==25]*360, Pb_0.7$Follower_collective_membership_benefit[Pb_0.7$Trial_n==25]*360,
              Pb_0.9$Leader_collective_membership_benefit[Pb_0.9$Trial_n==25]*360, Pb_0.9$Follower_collective_membership_benefit[Pb_0.9$Trial_n==25]*360),
         corral="wrap", method="swarm", col = "black", bg = alpha(c("red", "purple", "purple", "blue", "blue"), 0.5), 
         pch=21, cex=1,ylim=c(-0.55*360,0.55*360), xlim=c(0.5,5.5),
         axes=F, ylab="Collective membership gain", xaxs="i", yaxs="i")
box()
axis(2, tck=-0.027, lwd=0, lwd.ticks=1)
axis(1, at=c(1,2,3,4,5), labels=c("0.5 : 0.5\nAll","0.7 : 0.3\nLeaders","0.7 : 0.3\nFollowers",
                                  "0.9 : 0.1\nLeaders","0.9 : 0.1\nFollowers"), tck=0)
abline(h=0)
axis(1, at=c(0.5,1.5,2.5,3.5,4.5,5.5), tck=-0.027, labels=FALSE)
axis(3, at=c(1.5,2.5,3.5,4.5), tck=1, labels=FALSE, lwd=0, lwd.ticks=1)


beeswarm(list(c(Pb_0.5$Leader_collective_membership_benefit[Pb_0.5$Trial_n==0]*360, Pb_0.5$Follower_collective_membership_benefit[Pb_0.5$Trial_n==0]*360),
              Pb_0.7$Leader_collective_membership_benefit[Pb_0.7$Trial_n==0]*360, Pb_0.7$Follower_collective_membership_benefit[Pb_0.7$Trial_n==0]*360,
              Pb_0.9$Leader_collective_membership_benefit[Pb_0.9$Trial_n==0]*360, Pb_0.9$Follower_collective_membership_benefit[Pb_0.9$Trial_n==0]*360),
         corral="wrap", method="swarm", col = "black", bg = alpha(c("red", "purple", "purple", "blue", "blue"), 0.5), 
         pch=21, cex=1,ylim=c(-0.55*360,0.55*360), xlim=c(0.5,5.5),
         axes=F, ylab="Collective membership gain", xaxs="i", yaxs="i")
box()
axis(2, tck=-0.027, lwd=0, lwd.ticks=1)
axis(1, at=c(1,2,3,4,5), labels=c("0.5 : 0.5\nAll","0.7 : 0.3\nLeaders","0.7 : 0.3\nFollowers",
                                  "0.9 : 0.1\nLeaders","0.9 : 0.1\nFollowers"), tck=0)
abline(h=0)
axis(1, at=c(0.5,1.5,2.5,3.5,4.5,5.5), tck=-0.027, labels=FALSE)
axis(3, at=c(1.5,2.5,3.5,4.5), tck=1, labels=FALSE, lwd=0, lwd.ticks=1)


collective_membership_benefits_last_trial <- c(gmb_0.5_all_Solos_trial_last, gmb_0.7_leader_trial_last, gmb_0.7_follower_trial_last,
                                               gmb_0.9_leader_trial_last, gmb_0.9_follower_trial_last)
lead_ratios <- c(rep("0.5", 500), rep("0.7_l", 250), rep("0.7_f", 250), 
                 rep("0.9_l", 250), rep("0.9_f", 250))

pairwise.wilcox.test(collective_membership_benefits_last_trial, lead_ratios, p.adjust.method = "none")


#######  Error correlation between Solos with Pberaging decision-making rule
#Correlation between leader and follower - or randomly assigned for 0.5:0.5 weighting pairs

Pb_l_0.5_25 <- (360*Pbs_25s$Leader[Pbs_25s$LF==0.5])
Pb_f_0.5_25 <- (360*Pbs_25s$Follower[Pbs_25s$LF==0.5])
Pb_m1 <- lm(Pb_l_0.5_25 ~ Pb_f_0.5_25)
summary(Pb_m1)


Pb_l_0.7_25 <- (360*Pbs_25s$Leader[Pbs_25s$LF==0.7])
Pb_f_0.7_25 <- (360*Pbs_25s$Follower[Pbs_25s$LF==0.7])
Pb_m2 <- lm(Pb_l_0.7_25 ~ Pb_f_0.7_25)
summary(Pb_m2)

Pb_l_0.9_25 <- (360*Pbs_25s$Leader[Pbs_25s$LF==0.9])
Pb_f_0.9_25 <- (360*Pbs_25s$Follower[Pbs_25s$LF==0.9])
Pb_m3 <- lm(Pb_l_0.9_25 ~ Pb_f_0.9_25)
summary(Pb_m3)

plot_data <- data.frame("Leader"=c(Pb_l_0.5_25, Pb_l_0.7_25, Pb_l_0.9_25), 
                        "Follower"=c(Pb_f_0.5_25, Pb_f_0.7_25, Pb_f_0.9_25),
                        "Ratio"=c(rep(0.5, length(Pb_l_0.5_25)), rep(0.7, length(Pb_l_0.7_25)),
                                      rep(0.9, length(Pb_l_0.9_25)))
                        )
plot_data <- plot_data[sample(c(1:nrow(plot_data)), replace=F), ]                        

plot(Leader ~ Follower, data=plot_data, xlab="Agent 2 (Follower) Error", ylab="Agent 1 (Leader) Error", 
     xlim=c(-180, 180), ylim=c(-180, 180), col=ifelse(as.factor(Ratio)==0.5, "red", ifelse(as.factor(Ratio)==0.7, "purple", "blue")), pch=16, axes=FALSE)
box()
axis(1, at=seq(-180, 180, 90))
axis(2, at=seq(-180, 180, 90))
legend("topright", legend=c("0.5:0.5", "0.7:0.3", "0.9:0.1"),
       col=c("red", "purple", "blue"),
       cex=1, pt.cex=0.7, pch=16)

pdf("/Users/joemorford/Desktop/CL_NNs/14122021_figs/Probabilistic_error_correlation.pdf", width=2.5, height=2.5,
    pointsize=7)
plot(Leader ~ Follower, data=plot_data, xlab="Agent 2 (Follower) Error", ylab="Agent 1 (Leader) Error", 
     xlim=c(-180, 180), ylim=c(-180, 180), col=ifelse(as.factor(Ratio)==0.5, "red", ifelse(as.factor(Ratio)==0.7, "purple", "blue")), pch=16, axes=FALSE)
box()
axis(1, at=seq(-180, 180, 90))
axis(2, at=seq(-180, 180, 90))
legend("topright", legend=c("0.5:0.5", "0.7:0.3", "0.9:0.1"),
       col=c("red", "purple", "blue"),
       cex=1, pt.cex=0.7, pch=16)
dev.off()



#############################

#Figure 2

pdf("/Users/joemorford/Desktop/CL_NNs/14122021_figs/Figure 2.pdf", width=6, height=2,
    pointsize=9)

par(mfrow=c(1,3))
plot(Solo_meds~Trials, type="l", ylim = c(0, 180), lwd=1.2, col="black", bty='l',
     ylab="Absolute angular error", xlab="Number of learning trials",
     main="A: Solo learners")


plot(Av_0.5_meds~Trials, type="l", ylim = c(0, 180), lwd=1.2,  bty='l',
     col="red", ylab="Collective absolute angular error", 
     xlab="Number of learning trials", main="B: Pairs with\ndemocratic consensus")
lines(Av_0.7_meds~Trials, type="l", ylim = c(0, 180), lwd=1.2, col="purple", ylab="Collective absolute angular error")
lines(Av_0.9_meds~Trials, type="l", ylim = c(0, 180), lwd=1.2, col="blue", ylab="Collective absolute angular error")
legend(12, 170, title="Decision-making\n weightings", legend=c("0.5:0.5", "0.7:0.3", "0.9:0.1"),
       col=c("red", "purple", "blue"), lty=1, cex=1, box.lty=0, lwd=1.2)



plot(Pb_0.5_meds~Trials, type="l", ylim = c(0, 180), lwd=1.2, col="red", bty="l",
     ylab="Collective absolute angular error", xlab="Number of learning trials",
     main="C: Pairs with\ndespotic consensus")
lines(Pb_0.7_meds~Trials, type="l", ylim = c(0, 180), lwd=1.2, col="purple", ylab="Collective absolute angular error", xlab="Number of learning trials")
lines(Pb_0.9_meds~Trials, type="l", ylim = c(0, 180), lwd=1.2, col="blue", ylab="Collective absolute angular error", xlab="Number of learning trials")
legend(12, 170, title="Decision-making\n weightings", legend=c("0.5:0.5", "0.7:0.3", "0.9:0.1"),
       col=c("red", "purple", "blue"), lty=1, cex=1, box.lty=0, lwd=1.2)
dev.off()



##############
#Figure 3


pdf("/Users/joemorford/Desktop/CL_NNs/14122021_figs/Figure 3.pdf", width=5, height=5,
    pointsize=9)
par(mfrow=c(2,2), mai = c(0.5,0.5,0.5,0.5))

plot(Av_0.5_meds_lf~Trials, type="l", ylim = c(0, 180), lty="dotdash", lwd=1.2, col="red", bty="l",
     ylab="Individual absolute angular error", xlab="Number of learning trials",
     main="A")
lines(Solo_meds~Trials, type="l", ylim = c(0, 180), lwd=1.2, col="black", ylab="Absolute angular error", xlab="Number of learning trials")
lines(Av_0.7_meds_l~Trials, type="l", ylim = c(0, 180), lty="dashed", lwd=1.2, col="purple", ylab="Solo absolute angular error", xlab="Number of learning trials")
lines(Av_0.7_meds_f~Trials, type="l", ylim = c(0, 180), lty="dotted", lwd=1.2, col="purple", ylab="Solo absolute angular error", xlab="Number of learning trials")
lines(Av_0.9_meds_l~Trials, type="l", ylim = c(0, 180), lty="dashed", lwd=1.2, col="blue", ylab="Solo absolute angular error", xlab="Number of learning trials")
lines(Av_0.9_meds_f~Trials, type="l", ylim = c(0, 180), lty="dotted", lwd=1.2, col="blue", ylab="Solo absolute angular error", xlab="Number of learning trials")
legend("topright", legend=c("0.5:0.5", "0.7:0.3 leaders", "0.7:0.3 followers",
                            "0.9:0.1 leaders", "0.9:0.1 followers", 
                            "Solo learners"),
       col=c("red", "purple", "purple", "blue", "blue", "black"), 
       lty=c(4, 2, 3, 2, 3, 1), cex=0.85, 
       box.lty=0, lwd=1.2)


plot(Av_0.5_meds_pd~Trials, type="l", ylim = c(0, 180), lwd=1.2, col="red", bty="l",
     ylab="Pair difference", xlab="Number of learning trials", main="B")
lines(Av_0.7_meds_pd~Trials, type="l", ylim = c(0, 180), lwd=1.2, col="purple", ylab="Pair difference", xlab="Number of learning trials")
lines(Av_0.9_meds_pd~Trials, type="l", ylim = c(0, 180), lwd=1.2, col="blue", ylab="Pair difference", xlab="Number of learning trials")
legend("topright", legend=c("0.5:0.5", "0.7:0.3", "0.9:0.1"),
       col=c("red", "purple", "blue"), lty=1, cex=1, box.lty=0, lwd=1.2)




plot(NA, ylim = c(-120, 120), xlim=c(0,24), main="C",
     ylab="Collective membership gain", xlab="Number of learning trials", yaxs="i", bty="l")
abline(h=0, col="grey")
axis(2)
lines(Av_0.5_meds_gmb~Trials, type="l", ylim = c(-120, 120), lty="dotdash", lwd=1.2, col="red", 
      ylab="Collective membership gain", xlab="Number of learning trials", yaxs="i", bty="l")
lines(Av_0.7_meds_lgmb~Trials, type="l", lty="dashed", lwd=1.2, col="purple")
lines(Av_0.7_meds_fgmb~Trials, type="l",  lty="dotted", lwd=1.2, col="purple")
lines(Av_0.9_meds_lgmb~Trials, type="l",  lty="dashed", lwd=1.2, col="blue")
lines(Av_0.9_meds_fgmb~Trials, type="l", lty="dotted", lwd=1.2, col="blue")
legend("bottomright", legend=c("0.5:0.5", "0.7:0.3 leaders", "0.7:0.3 followers",
                               "0.9:0.1 leaders", "0.9:0.1 followers"),
       col=c("red", "purple", "purple", "blue", "blue"), 
       lty=c(4, 2, 3, 2, 3), cex=0.85, 
       box.lty=0, lwd=1.2)



plot(Av_l_0.5_25 ~ Av_f_0.5_25, xlab="Agent 2 (Follower) Error", ylab="Agent 1 (Leader) Error", 
     axes=F,xlim=c(-180, 180), ylim=c(-180, 180), col="red", pch=16, cex=0.7, main="D")
points(Av_l_0.7_25 ~ Av_f_0.7_25, col="purple", pch=16, cex=0.7)
points(Av_l_0.9_25 ~ Av_f_0.9_25, col="blue", pch=16, cex=0.7)
#Theoretical expectations
lines((0+-1*seq(-200,200))~seq(-200,200), col="red", lwd=0.5)
lines((0+-(3/7)*seq(-200,200))~seq(-200,200), col="purple", lwd=0.5)
lines((0+-(1/9)*seq(-200,200))~seq(-200,200), col="blue", lwd=0.5)
box()
axis(1, at=seq(-180, 180, 90))
axis(2, at=seq(-180, 180, 90))
legend("topright", legend=c("0.5:0.5", "0.7:0.3", "0.9:0.1"),
       col=c("red", "purple", "blue"), cex=1, pt.cex=0.7, pch=16)
dev.off()


#######################
#Figure 4


pdf("/Users/joemorford/Desktop/CL_NNs/14122021_figs/Figure 4.pdf", width=5, height=5,
    pointsize=9)
par(mfrow=c(2,2), mai = c(0.5,0.5,0.5,0.5))

plot(Pb_0.5_meds_lf~Trials, type="l", ylim = c(0, 180), bty="l", main="A",
     lty="dotdash", lwd=1.2, col="red", ylab="Individual absolute angular error", xlab="Number of learning trials")
lines(Solo_meds~Trials, type="l", ylim = c(0, 180), lwd=1.2, col="black")
lines(Pb_0.7_meds_l~Trials, type="l", ylim = c(0, 180), lty="dashed", lwd=1.2, col="purple", ylab="Solo absolute angular error", xlab="Number of learning trials")
lines(Pb_0.7_meds_f~Trials, type="l", ylim = c(0, 180), lty="dotted", lwd=1.2, col="purple", ylab="Solo absolute angular error", xlab="Number of learning trials")
lines(Pb_0.9_meds_l~Trials, type="l", ylim = c(0, 180), lty="dashed", lwd=1.2, col="blue", ylab="Solo absolute angular error", xlab="Number of learning trials")
lines(Pb_0.9_meds_f~Trials, type="l", ylim = c(0, 180), lty="dotted", lwd=1.2, col="blue", ylab="Solo absolute angular error", xlab="Number of learning trials")
legend("topright", legend=c("0.5:0.5", "0.7:0.3 leaders", "0.7:0.3 followers",
                            "0.9:0.1 leaders", "0.9:0.1 followers", 
                            "Solo learners"),
       col=c("red", "purple", "purple", "blue", "blue", "black"), 
       lty=c(4, 2, 3, 2, 3, 1), cex=0.85, 
       box.lty=0, lwd=1.2)


plot(Pb_0.5_meds_pd~Trials, type="l", ylim = c(0, 180), bty="l", main="B",
     lwd=1.2, col="red", ylab="Pair difference", xlab="Number of learning trials")
lines(Pb_0.7_meds_pd~Trials, type="l", ylim = c(0, 180), lwd=1.2, col="purple", ylab="Pair difference", xlab="Number of learning trials")
lines(Pb_0.9_meds_pd~Trials, type="l", ylim = c(0, 180), lwd=1.2, col="blue", ylab="Pair difference", xlab="Number of learning trials")
legend("topright", legend=c("0.5:0.5", "0.7:0.3", "0.9:0.1"),
       col=c("red", "purple", "blue"), lty=1, cex=1, box.lty=0, lwd=1.2)



plot(NA, ylim = c(-120, 120), xlim=c(0,24), main="C",
     ylab="Collective membership gain", xlab="Number of learning trials", yaxs="i", bty="l")
abline(h=0, col="grey")
axis(2)
lines(Pb_0.5_meds_gmb~Trials, type="l", lty="dotdash", lwd=1.2, col="red", ylab="Collective membership gain", xlab="Number of learning trials", yaxs="i")
lines(Pb_0.7_meds_lgmb~Trials, type="l", lty="dashed", lwd=1.2, col="purple", ylab="Collective membership gain", xlab="Number of learning trials")
lines(Pb_0.7_meds_fgmb~Trials, type="l", lty="dotted", lwd=1.2, col="purple", ylab="Collective membership gain", xlab="Number of learning trials")
lines(Pb_0.9_meds_lgmb~Trials, type="l", lty="dashed", lwd=1.2, col="blue", ylab="Collective membership gain", xlab="Number of learning trials")
lines(Pb_0.9_meds_fgmb~Trials, type="l", lty="dotted", lwd=1.2, col="blue", ylab="Collective membership gain", xlab="Number of learning trials")
legend("bottomright", legend=c("0.5:0.5", "0.7:0.3 leaders", "0.7:0.3 followers",
                               "0.9:0.1 leaders", "0.9:0.1 followers"),
       col=c("red", "purple", "purple", "blue", "blue"), 
       lty=c(4, 2, 3, 2, 3), cex=0.85, 
       box.lty=0, lwd=1.2)



plot(NA, xlab="Agent 2 (Follower) Error", ylab="Agent 1 (Leader) Error", main="D",
     xlim=c(-180, 180), ylim=c(-180, 180),axes=FALSE)
for (i in 1:length(Pb_l_0.5_25)){
  points(Pb_l_0.5_25[i] ~ Pb_f_0.5_25[i], col=alpha("red", 0.6), pch=16, cex=0.7)
  points(Pb_l_0.7_25[i] ~ Pb_f_0.7_25[i], col=alpha("purple", 0.6), pch=16, cex=0.7)
  points(Pb_l_0.9_25[i] ~ Pb_f_0.9_25[i],  col=alpha("blue", 0.6), pch=16, cex=0.7)
}
box()
axis(1, at=seq(-180, 180, 90))
axis(2, at=seq(-180, 180, 90))
legend("topright", legend=c("0.5:0.5", "0.7:0.3", "0.9:0.1"),
       col=c("red", "purple", "blue"),
       cex=1, pt.cex=0.7, pch=16)
dev.off()

