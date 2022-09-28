switch_cor <- function(data,x){
      return(cor(data$Switch,x))
}

direct_previous <- function(x, y,Chosen,Rejected,outcome,feedback) {
      res <- sapply(2:length(x), function(t) {
            if (length(which(x[t] == Rejected[1:t-1] & y[t]!=Chosen[1:t-1] & feedback[1:t-1]==1)) == 0) {
                  return(0)
            }
            # in which do they match subsequent to equation 
            # we take the outcome 
            mean(outcome[which(x[t] == Rejected[1:t-1] & feedback[1:t-1]==1 &  y[t]==Chosen[1:t-1])]) }
      )
      return(c(NA, res))
}




all_previous <- function(x, Rejected,outcome,feedback) {
      res <- sapply(2:length(x), function(t) {
            if (length(which(x[t] == Rejected[1:t-1] & feedback[1:t-1]==1)) == 0) {
                  return(0)
            }
            # in which do they match subsequent to equation 
            # we take the outcome 
            mean(outcome[which(x[t] == Rejected[1:t-1] & feedback[1:t-1]==1)]) }
      )
      return(c(NA, res))
}

require(data.table)

dat1<- setDT(data)
dat1[, c("stim1_chosen", "stim2_chosen") :=
           lapply(.SD, function(z) mapply(function(x, S) {
                 sum(Chosen[S] %in% x)
           }, z, lapply(seq_len(.N)-1, seq_len))),
     .SDcols = c("stim1", "stim2"), by = .(subject)]

Counter_Full <- dat1 %>% 
      group_by(subject) %>% 
      mutate(
            Rejected=ifelse(Chosen==stim1,stim2,stim1),
            outcome_neg =ifelse(outcome==0,-1,1),
            mean_counter_left=all_previous(stim1,Rejected,outcome_neg,feedback),
            mean_counter_right=all_previous(stim2,Rejected,outcome_neg,feedback),
            direct_counter_left_direct=direct_previous(stim1,stim2,Chosen,Rejected,outcome_neg,feedback),
            direct_counter_right_direct=direct_previous(stim2,stim1,Chosen,Rejected,outcome_neg,feedback),
            counter_direct_diff=direct_counter_right_direct- direct_counter_left_direct, 
            time_diff=stim2_chosen-stim1_chosen,
            
            
            counter_diff=mean_counter_right-mean_counter_left) %>% 
      filter(!is.na(counter_diff))



full_regression <- function(data){
      data_ready <- data %>% mutate(
            two=ifelse(condition_left==1|condition_left==3,1,0),
            twon=ifelse(two==0,-1,1),
            three=ifelse(condition_left<3,1,0),
            
            threen=if_else(three==0,-1,1),
            Rewarddiff=EV_Stim2-EV_Stim1,
            Rewarddiffz=zscore(Rewarddiff,na.rm = TRUE),
            Realx3 = Rewarddiff * threen,
            Realx3_zscored=zscore(Realx3,na.rm = TRUE),
            Realx2 = Rewarddiff * threen,
            counter_direct_diff_zscored= zscore(counter_direct_diff,na.rm = TRUE),
            counter_diff_zscored= zscore(counter_diff,na.rm = TRUE),
            Counterx3 =  counter_diff *threen,
            Counterx2 =  counter_diff *twon,
            timex3=threen*time_diff,
            timex2=twon*time_diff,
            Counterx3_zscored=zscore(Counterx3,na.rm = TRUE),
            
            Counterx3_direct=counter_direct_diff * threen,
            Counterx2_direct=counter_direct_diff * twon,
            
            Counterx3_direct_zscored=zscore(Counterx3_direct),
            
      )
      
      return(data_ready)
}

data_ready <- full_regression(Counter_Full)

#only concurrent diversity + no direct alternative effect 
counters_effect_L1  <- function(data_ready){
      Bayes_counterfactual <- brms::brm(formula =choice ~Rewarddiff+ counter_diff+threen+Realx3+Counterx3 +(Rewarddiff+ counter_diff+threen+Realx3+Counterx3|subject)
                                        , 
                                        data=data_ready, 
                                        family = bernoulli(link = "logit"),
                                        warmup = 1000, 
                                        iter = 10000, 
                                        chains = 2, 
                                        cores=2,
                                        sample_file='samples1.csv',
                                        seed = 123)
      
      return( Bayes_counterfactual)}


#
counters_effect_L2  <- function(data_ready){
      Bayes_counterfactual <- brms::brm(formula =choice ~Rewarddiff+threen+time_diff+ counter_diff+counter_direct_diff+Realx3+Counterx3 +Counterx3_direct+timex3+(Rewarddiff+threen+time_diff+ counter_diff+counter_direct_diff+Realx3+Counterx3 +Counterx3_direct+timex3|subject)
                                        , 
                                        data=data_ready, 
                                        family = bernoulli(link = "logit"),
                                        warmup = 1000, 
                                        iter = 10000, 
                                        chains = 2, 
                                        cores=2,
                                        sample_file='samples1.csv',
                                        seed = 123)
      
      return( Bayes_counterfactual)}

counter=counters_effect_L2(data_ready)

# both concurrent and cumulative diversity 
counters_effect  <- function(data_ready){
      Bayes_counterfactual <- brms::brm(formula =choice ~Rewarddiff+threen+twon+time_diff+ counter_diff+counter_direct_diff+Realx3+Realx2+Counterx3 +Counterx2+Counterx3_direct+Counterx2_direct+timex3+timex2+(Rewarddiff+threen+twon+time_diff+ counter_diff+counter_direct_diff+Realx3+Realx2+Counterx3+Counterx2 +Counterx3_direct+Counterx2_direct+timex3+timex2|subject)
                                        , 
                                        data=data_ready, 
                                        family = bernoulli(link = "logit"),
                                        warmup = 1000, 
                                        iter = 3000, 
                                        chains = 2, 
                                        cores=2,
                                        sample_file='samples1.csv',
                                        seed = 123)
      
      return( Bayes_counterfactual)}
counter_full=counters_effect(data_ready)


#parameters are z scored 
counters_effect_full_z  <- function(data_ready){
      Bayes_counterfactual <- brms::brm(formula =choice ~Rewarddiffz+ counter_diff_zscored+threen+Realx3_zscored+Counterx3_zscored +(Rewarddiffz+ counter_diff_zscored+threen+Realx3_zscored+Counterx3_zscored|subject)
                                        , 
                                        data=data_ready, 
                                        family = bernoulli(link = "logit"),
                                        warmup = 1000, 
                                        iter = 10000, 
                                        chains = 2, 
                                        cores=2,
                                        sample_file='samples1.csv',
                                        seed = 123)
      
      return( Bayes_counterfactual)}


# attemp2 zscored direct - no indirect choices included 
counters_effect_full_direct  <- function(data_ready){
      Bayes_counterfactual <- brms::brm(formula =choice ~ Rewarddiff+ counter_direct_diff+threen+Realx3+Counterx3_direct +(Rewarddiff+ counter_direct_diff+threen+Realx3+Counterx3_direct|subject)
                                        , 
                                        data=data_ready, 
                                        family = bernoulli(link = "logit"),
                                        warmup = 1000, 
                                        iter = 10000, 
                                        chains = 2, 
                                        cores=2,
                                        sample_file='samples1.csv',
                                        seed = 123)
      
      return( Bayes_counterfactual)}


counters_effect_full_direct_z  <- function(data_ready){
      Bayes_counterfactual <- brms::brm(formula =choice ~ Rewarddiffz+ counter_direct_diff_zscored+threen+Realx3_zscored+Counterx3_direct_zscored +(Rewarddiffz+ counter_direct_diff_zscored+threen+Realx3_zscored+Counterx3_direct_zscored|subject)
                                        , 
                                        data=data_ready, 
                                        family = bernoulli(link = "logit"),
                                        warmup = 1000, 
                                        iter = 10000, 
                                        chains = 2, 
                                        cores=2,
                                        sample_file='samples1.csv',
                                        seed = 123)
      
      return( Bayes_counterfactual)}

# all effects included 
counters_effect_both_all  <- function(data_ready){
      Bayes_counterfactual <- brms::brm(formula =choice ~ Rewarddiff+counter_direct_diff+counter_diff+threen+twon+Realx3+Counterx3+Counterx2+Counterx3_direct +Realx2+Counterx2_direct+(Rewarddiff+counter_direct_diff+counter_diff+threen+twon+Realx3+Counterx3+Counterx2+Counterx3_direct +Realx2+Counterx2_direct|subject)
                                        , 
                                        data=data_ready, 
                                        family = bernoulli(link = "logit"),
                                        warmup = 1000, 
                                        iter = 10000, 
                                        chains = 2, 
                                        cores=2,
                                        sample_file='samples1.csv',
                                        seed = 123)
      
      return( Bayes_counterfactual)}

counter_full=counters_effect_full(data_ready)
save(counter_full,file='G:/My Drive/Common lab folder/model-fitting-master/Context_Experiment/R Scripts/Data_Objects/Counter_Updated/zscored_indirect.RData')


