# result 1
figure2a <- function(full_data){
      model_type=7 # our winning model 
      # we load the sim data generated from matlab 
      if(laptop){
            load("/Volumes/GoogleDrive/My Drive/Common lab folder/model-fitting-master/Context/Context_Experiment/R Scripts/Data_Objects/Stimuli.RData")
            sim_data <- read.csv(sprintf("~/Google Drive/My Drive/Common lab folder/model-fitting-master/Context/Context_Experiment/Matlab Scripts/model_fitting/Simulations_current/SimulatedFromFits/simData_Type%d.csv",model_type),sep=',')
      } else{
            sim_data <- as.data.frame(read.csv(sprintf('G:/My Drive/Common lab folder/model-fitting-master/Context_Experiment/Matlab Scripts/model_fitting/Simulations_current/SimulatedFromFits/simData_Type%d.csv',model_type),sep=','))
            
      }
      RealA <- add_absolute_accuracy(Real_data,stimuli_Reward)
      FullSim <- add_absolute_accuracy(sim_data,stimuli_Reward)
      require(lemon)
      
      by_condition_sim=ddply(FullSim,.(subject,condition_left,feedback,learnedtogether),summarize,mean=mean(Accurate,na.rm = TRUE)) 
      by_condition=ddply(RealA,.(subject,condition_left,feedback,learnedtogether),summarize,mean=mean(Accurate,na.rm = TRUE)) 
      # New facet label names for supp variable
      by_condition <- by_condition %>% mutate(real=1)
      by_condition_sim <- by_condition_sim %>% mutate(real=0)
      real_and_sim <- rbind(by_condition,by_condition_sim)
      real_and_sim <- real_and_sim %>% mutate(learning=-feedback) %>%
            filter(!is.na(learning)) %>%
            mutate(Combined=-1*(learnedtogether+feedback),
                   super_combined=-1 * (learnedtogether+feedback) -(3*real))
      real_and_sim <- real_and_sim %>%
            
            # Rename 4 to 4wd, f to Front, r to Rear
            mutate(condition_left = recode(condition_left, "1" = "Concurrent:Low\nCumulative :Low", "2" = "Concurrent :Low\nCumulative:High", "3" = "Concurrent:High\n Cumulative:Low","4"="Concurrent:High\n Cumulative:High"))
      sim_mean=summarySE(data=subset(real_and_sim,real==0),measurevar = "mean",groupvars = c("condition_left","Combined")) %>% mutate(average=mean) %>% select(-c(mean,N,sd,ci))
      pj <- position_jitter(width=0.1,height=0, seed=9)
      real_and_sim <- left_join(real_and_sim,sim_mean,by=c("condition_left","Combined"))
      
      p <- ggplot(real_and_sim,aes(x=factor(condition_left),y=mean,fill=factor(Combined)))+
            geom_boxplot(data=subset(real_and_sim,real==1),aes(x = factor(condition_left), y = mean,fill=factor(Combined),color=factor(super_combined)),outlier.shape = NA, alpha = 0.85, width = .65, colour = "BLACK") +
            geom_point(data=subset(real_and_sim,real==0),aes(x = factor(condition_left), y = average,fill=factor(Combined)),outlier.shape = NA,size=5,alpha=.4,shape=1, colour = "BLACK", position = position_dodge(width = 0.65)) +
            ylab("Correct choice")+
            scale_color_manual(values=c("#E7B800","darkred","green"),guide = FALSE)+
            scale_fill_manual(values=c("#E7B800","darkred","green"),name="Trial Type",labels=c("Learning","Learned pair test","Novel pair test"))+
            
            scale_x_discrete(name="Condition",labels=c("Concurrent:Low\nCumulative:Low","Concurrent:Low\nCumulative:High","Concurrent:High\nCumulative:Low","Concurrent:High\nCumulative:High"))+
            
            scale_y_continuous(limits = c(0,1), breaks = seq(0, 1, by = 0.25), labels = scales::percent) +
            annotate("text", x=2.5,y = .45, label = "Chance performance",size=8,color='black')+
            
            
            geom_hline(yintercept = .5,linetype='dashed', col = 'black')+
            
            
            theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                  panel.background = element_blank(), axis.line = element_line(colour = "black"), 
                  legend.position = "none",legend.title = element_text(size=26),legend.text =element_text(size=22),axis.title.x = element_blank(), axis.text.x = element_text( size=18), axis.text.y = element_text( size=18),axis.title.y = element_text(size=26),  text=element_text(size=20))
      
      
      
      
      if(.Platform$OS.type == "unix") {
            laptop=1;
      } else {
            laptop=0; 
      }
      
      
      # save the data
      if(laptop){
            # ggsave(paste0('~/Google Drive/My Drive/Common lab folder/model-fitting-master/Context_Experiment/R Scripts/All_Scripts_Relevant/Model_Validation/Overall',Sys.Date(),'.png'), width = 12, height = 7, units = "in")
            png(paste0('~/Google Drive/My Drive/Common lab folder/model-fitting-master/Context/Context_Experiment/R Scripts/All_Scripts_Relevant/Model_Validation/Overall_total_with_model',Sys.Date(),'.png'), width = 12, height = 7, units = "in",res=300)
            print(p)
            dev.off()
            
      }else{
            
            ggsave(paste0('G:/My Drive/Common lab folder/model-fitting-master/Context_Experiment/R Scripts/All_Scripts_Relevant/Model_Validation/Overall',Sys.Date(),'.png'), width = 12, height = 7, units = "in")
            
      }
      
      
      return(p)}

