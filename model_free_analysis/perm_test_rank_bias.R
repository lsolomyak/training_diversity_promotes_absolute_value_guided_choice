#we calculate whether the difference between subjects in bias is significant using a permutation test after getting the bias data
bias <- plot_rank_bias(data)
null <- c()
bias %>% filter(three==1) ->  threes
bias %>% filter(three==0) ->  threeo
actual_difference <- summarySE(bias,measurevar = "bias",groupvars = c("three","subject"))
act_difference <- summarySE(actual_difference,measurevar = "bias_mean",groupvars = c("three"),na.rm=TRUE)
temp <-c()
for(i in 1:10000){
      temp$bias = sample(c(threes$bias,threeo$bias),replace=FALSE)
      temp$subject=sample(c(threes$subject,threeo$subject),replace=TRUE)
      temp <- as.data.frame(temp)
      resample_honey = temp[1:NROW(threes),]
      resample_nohoney = temp[NROW(threes)+1:NROW(temp),]
      perm_difference <- summarySE(resample_honey,measurevar = "bias",groupvars = c("subject"))
      per_difference <- summarySE(perm_difference,measurevar = "bias_mean",na.rm=TRUE)
      
      perm_difference_group2 <- summarySE(resample_nohoney,measurevar = "bias",groupvars = c("subject"))
      per_difference_group2 <- summarySE(perm_difference_group2,measurevar = "bias_mean",na.rm=TRUE)
      null[i] = per_difference$bias_mean_mean - per_difference_group2$bias_mean_mean
}
per_test <- hist(null, breaks=30, xlab='High-low concurrent diversity mean bias', main='Permutation Test (p=.06)')
abline(v=diff(act_difference$bias_mean_mean), lwd=2, col='orange')

