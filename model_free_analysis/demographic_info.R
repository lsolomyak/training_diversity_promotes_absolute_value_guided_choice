
#in this script we evaluate various schedule effects 
# Age Demographic 

# 
setwd('/Volumes/GoogleDrive/My Drive/Common lab folder/Context_Reward/Analysis/EEG_Quality_Checks/Data_Raw/') #directory where the schedule file is located

db_files <- list.files(  path='/Volumes/GoogleDrive/My Drive/Common lab folder/Context_Reward/Analysis/EEG_Quality_Checks/Data_Raw/',  # Specify path
                         pattern = "\\_schedule.db$",
                         recursive=TRUE,no.. = FALSE)


numextract <- function(string){ 
      str_extract(string, "\\-*\\d+\\.*\\d*")
} 



trialFiles<- list()
stimuliFiles <- list()
dem_files <- list()
subject_names <-c()
subject <-list()
for(i in 1:length(db_files)){
      
      if(grepl("^subject",db_files[[i]])){
            data_1 = dbConnect(SQLite(),paste0('/Volumes/GoogleDrive/My Drive/Common lab folder/Context_Reward/Analysis/EEG_Quality_Checks/Data_Raw/',as.character(db_files[i])))
            trialFiles[[i]] = dbGetQuery(data_1, "SELECT * FROM trials")
            subject_names[i] <- as.numeric(numextract(db_files[i]))
            subject[[i]] = dbGetQuery(data_1, "SELECT * FROM subject")
            
      }
      
}

combined_subjects <- dplyr::bind_rows(subject)
combined_subjects$age <- as.numeric(combined_subjects$age)
summarySE(combined_subjects, measurevar = "age")   

combined_subjects %>% filter(trial==0 | trial==71) -> time_diff
time_diff  %>% group_by(subject) %>% 
      mutate(lag=(choice_time-lag(choice_time))/(1000*3600)
      ) %>% filter(lag>4,lag <24)-> lsh

combined_subjects %>% filter(feedback==1) %>% 
      group_by(subject,block) %>% select(subject,block,stim1,stim2,choice_time) %>% 
      uni


summarySE(lsh,measurevar = "lag")




days_effect <- function(combined_subjects){
Fulldata <- LastSeen(Fulldata)
#mixed trials
Fulldata %>% filter(condition_left !=condition_right,subject!=2)-> mixed
mixed <- mixed %>% mutate(av_time=mean(LLSeen/(3600),RLSeen/(3600)))
mixed <- mutate(mixed, mean_col = rowMeans(select(mixed,c(LLSeen,RLSeen)), na.rm = TRUE))
mixed <- mixed %>% mutate(mean_col=mean_col/(1000*3600*24))


days_effecte <- function(high_cumulative_data){
      Bayes_rl <- brms::brm(formula =relatively_correct ~mean_col +(1|subject)
                            , 
                            data=mixed, 
                            family = bernoulli(link = "logit"),
                            warmup = 2000, 
                            iter = 3000, 
                            chains = 2, 
                            cores=2,
                            sample_file='samples1.csv',
                            seed = 123)
      
      return( Bayes_rl)}

# now we run the model
tryzert <- days_effecte(mixed)
return(tryzert)}
