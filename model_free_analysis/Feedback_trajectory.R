# the evolution of performance on feedback trials 
Feedback_trajectory <- function(data){
      if(mean(data$feedback!=1)){
            
            data <- data %>% filter(feedback==1)
      }
      
      require(zoo)
      add_time_learned <- function(data){
            # just three two stimuli
            per_stim <- c()
            across_subjects <- c()
            for(j in unique(data$subject)){
                  for(i in min(data$stim1):max(data$stim1)){
                        # just get nujmber of times each stimuli was learned 
                        sub_data  <- data %>% filter(subject==j)
                        stim <- sub_data %>% filter(stim1 == i | stim2==i)
                        # add the time learned 
                        stim <- stim %>% mutate(Time_Learned=(stim1=cumsum(stim1==i | stim2==i)))
                        per_stim <- rbind(per_stim,stim) 
                  }
            }
            return(per_stim)}
      
      
      per_stim <- add_time_learned(data)
      per_stims1 <-per_stim %>% filter(feedback==1,Time_Learned <65)
      
      
      across_subjects <- ddply(per_stims1,.(subject,condition_left,Time_Learned),summarize,mean=mean(accuracy,na.rm = TRUE))%>% filter(Time_Learned<65)
      
      
      acros <- across_subjects %>% filter(condition_left==1 | condition_left==3)
      LTSED <- summarySE(acros, measurevar="mean", groupvars=c("Time_Learned","condition_left"))
      
      
      
      p1 <- ggplot() +
            geom_point(data = LTSED, aes(x=Time_Learned, y=mean, color=factor(condition_left)), size = 5) +
            geom_errorbar(data = LTSED, aes(x=Time_Learned, y=mean, ymin=mean-se, ymax=mean+se), width=.1) +
            # ylab("Correct choices") + 
            scale_x_continuous(breaks = scales::pretty_breaks(n = 5))+
            scale_y_continuous(limits = c(.5,1), breaks = seq(.5, 1, by = 0.1), labels = scales::percent) +
            scale_color_manual(name="Concurrent",labels=c("Low","High"),values=c("#E69F00", "#56B4E9"))+
            geom_smooth(data = LTSED, aes(x=Time_Learned, y=mean,group=factor(condition_left),color=factor(condition_left)))+
            # xlab("Trials Learned") +
            annotate("text", x=30,y = .65, label = "Second Session",size=5,color='gray',angle=90)+
            geom_vline(xintercept = 32.5,linetype='dashed', col = 'gray')+
            
            ggtitle("Low Cumulative")+
            
            theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                  panel.background = element_blank(), axis.line = element_line(colour = "black"), 
                  legend.position = c(.85,.25),legend.title = element_text(size=24),legend.text =element_text(size=20),axis.title.x = element_blank(), axis.text.x = element_text( size=18), axis.text.y = element_text(size=18),axis.title.y = element_blank(),  text=element_text(size=16))
      
      spe <- across_subjects %>% filter(condition_left==2 | condition_left==4)
      LTSED24 <- summarySE(spe, measurevar="mean", groupvars=c("Time_Learned","condition_left"))
      
      library("wesanderson")
      p2 <- ggplot() +
            geom_point(data = LTSED24, aes(x=Time_Learned, y=mean, color=factor(condition_left)), size = 5) +
            geom_errorbar(data = LTSED24, aes(x=Time_Learned, y=mean, ymin=mean-se, ymax=mean+se), width=.1) +
            # ylab("Correct choices") + 
            scale_x_continuous(breaks = scales::pretty_breaks(n = 5))+
            scale_y_continuous(limits = c(.5,1), breaks = seq(.5, 1, by = 0.1), labels = scales::percent) +
            scale_color_manual(name="Concurrent",labels=c("Low","High"),values=c("#E69F00", "#56B4E9"))+
            geom_smooth(data = LTSED24, aes(x=Time_Learned, y=mean,group=factor(condition_left),color=factor(condition_left)))+
            # xlab("Trials Learned") +
            ggtitle('High Cumulative')+
            annotate("text", x=30,y = .65, label = "Second Session",size=5,color='gray',angle=90)+
            
            geom_vline(xintercept = 32.5,linetype='dashed', col = 'gray')+
            theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                  panel.background = element_blank(), axis.line = element_line(colour = "black"), 
                  legend.position = c(.85,.25),legend.title = element_text(size=24),legend.text =element_text(size=20),axis.title.x = element_blank(), axis.text.x = element_text( size=18), axis.text.y = element_blank(),axis.title.y = element_blank(),  text=element_text(size=16))
      
      
      
      
      
      acros12 <- across_subjects %>% filter(condition_left==1 | condition_left==2)
      LTSED3 <- summarySE(acros12, measurevar="mean", groupvars=c("Time_Learned","condition_left"))
      
      
      # plotting the mean for both 
      p3 <- ggplot() +
            geom_point(data = LTSED3, aes(x=Time_Learned, y=mean, color=factor(condition_left)), size = 5) +
            geom_errorbar(data = LTSED3, aes(x=Time_Learned, y=mean, ymin=mean-se, ymax=mean+se), width=.1) +
            # ylab("Correct choices") + 
            scale_x_continuous(breaks = scales::pretty_breaks(n = 5))+
            scale_y_continuous(limits = c(.5,1), breaks = seq(.5, 1, by = 0.1), labels = scales::percent) +
            scale_color_manual(name="Cumulative",labels=c("Low","High"),values=wes_palette(n=2, name="Moonrise2"))+
            geom_smooth(data = LTSED3, aes(x=Time_Learned, y=mean,group=factor(condition_left),color=factor(condition_left)))+
            # xlab("Trials Learned") +
            ggtitle("Low Concurrent")+
            annotate("text", x=30,y = .65, label = "Second Session",size=5,color='gray',angle=90)+
            geom_vline(xintercept = 32.5,linetype='dashed', col = 'gray')+
            
            theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                  panel.background = element_blank(), axis.line = element_line(colour = "black"), 
                  legend.position = c(.85,.25),legend.title = element_text(size=24),legend.text =element_text(size=20),axis.title.x = element_blank(), axis.text.x = element_text( size=18), axis.text.y = element_text(size=18),axis.title.y = element_blank(),  text=element_text(size=16))
      
      spe <- across_subjects %>% filter(condition_left==3 | condition_left==4)
      LTSED243 <- summarySE(spe, measurevar="mean", groupvars=c("Time_Learned","condition_left"))
      
      p4 <- ggplot() +
            geom_point(data = LTSED243, aes(x=Time_Learned, y=mean, color=factor(condition_left)), size = 5) +
            geom_errorbar(data = LTSED243, aes(x=Time_Learned, y=mean, ymin=mean-se, ymax=mean+se), width=.1) +
            #  ylab("Correct choices") + 
            scale_x_continuous(breaks = scales::pretty_breaks(n = 5))+
            scale_y_continuous(limits = c(.5,1), breaks = seq(.5, 1, by = 0.1), labels = scales::percent) +
            scale_color_manual(name="Cumulative",labels=c("Low","High"),values=wes_palette(n=2, name="Moonrise2"))+
            geom_smooth(data = LTSED243, aes(x=Time_Learned, y=mean,group=factor(condition_left),color=factor(condition_left)))+
            # xlab("Trials Learned") +
            ggtitle('High Concurrent')+
            annotate("text", x=30,y = .65, label = "Second Session",size=5,color='gray',angle=90)+
            geom_vline(xintercept = 32.5,linetype='dashed', col = 'gray')+
            theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                  panel.background = element_blank(), axis.line = element_line(colour = "black"), 
                  legend.position = c(.85,.25),legend.title = element_text(size=24),legend.text =element_text(size=20),axis.title.x = element_blank(), axis.text.x = element_text( size=18), axis.text.y = element_blank(),axis.title.y = element_blank(),  text=element_text(size=16))
      
      
      figure <- ggarrange(p1,p2,p3,p4)
      
      annotate_figure(figure,
                      top = text_grob("Learning Trajectory", color = "black", face = "bold", size = 24),
                      fig.lab = "Figure 3", fig.lab.face = "bold",fig.lab.size = 24,
                      bottom=text_grob("Trials",, color = "black", size = 20),
                      left=text_grob("Correct Choice",rot=90,color = "black", size = 20)
                      
      )
      
      if(laptop){
            ggsave(paste0('~/Google Drive/My Drive/Common lab folder/model-fitting-master/Context_Experiment/R Scripts/All_Scripts_Relevant/Model_Validation/trajector24',Sys.Date(),'.png'), width = 12, height = 7, units = "in")
            
      }else{
            
            ggsave(paste0('G:/My Drive/Common lab folder/model-fitting-master/Context_Experiment/R Scripts/All_Scripts_Relevant/Model_Free/Trajectories/trajectory24',Sys.Date(),'.png'), width = 12, height = 7, units = "in")
            
      }
      
      return(figure)}




# trajectory_fix <- function(){
#    if(.Platform$OS.type == "unix") {
#       laptop=1;
#    } else {
#       laptop=0; 
#       
#    }
#    if(laptop){
#       load("/Volumes/GoogleDrive/My Drive/Common lab folder/model-fitting-master/Context_Experiment/R Scripts/Data_Objects/per_stim.RData")
#       load("/Volumes/GoogleDrive/My Drive/Common lab folder/model-fitting-master/Context_Experiment/R Scripts/Data_Objects/Stimuli.RData")
#    }
#    else
#    {
#       load("G:My Drive/Common lab folder/model-fitting-master/Context_Experiment/R Scripts/Data_Objects/per_stim.RData")
#       load("G:/My Drive/Common lab folder/model-fitting-master/Context_Experiment/R Scripts/Data_Objects/Stimuli.RData")
#       
#    }
#    if(laptop){
#       sim_data <- read.csv(sprintf("~/Google Drive/My Drive/Common lab folder/model-fitting-master/Context_Experiment/Matlab Scripts/model_fitting/Simulations_current/SimulatedFromFits/simData_Type%d.csv",7),sep=',')
#    } else{
#       sim_data <- as.data.frame(read.csv(sprintf('G:/My Drive/Common lab folder/model-fitting-master/Context_Experiment/Matlab Scripts/model_fitting/Simulations_current/SimulatedFromFits/simData_Type%d.csv',7),sep=','))
#       
#    }
#    
#    
#    
#    
#    
#    
#    load("/Volumes/GoogleDrive/My Drive/Common lab folder/model-fitting-master/Context_Experiment/R Scripts/Data_Objects/per_stim.RData")
#    per_stims1 <-per_stim %>% filter(feedback==1,Time_Learned <65)
#    across_subjects <- ddply(per_stims1,.(subject,condition_left,Time_Learned),summarize,mean=mean(accuracy,na.rm = TRUE))%>% filter(Time_Learned<65)
#    
#    LTSED <- summarySE(across_subjects, measurevar="mean", groupvars=c("Time_Learned","condition_left"))
#    LTSED <- LTSED %>% filter(Time_Learned <32 | Time_Learned>32)
#    col3 <- guide_legend(ncol = 2)
#    create_traj <- function(LTSED,col3){
#       figure <-  ggplot() +
#          geom_point(data=LTSED, aes(x=Time_Learned, y=mean,color=factor(condition_left),shape=factor(condition_left)), size = 3,alpha=.8,stroke = 1.4) +
#          geom_errorbar(data=LTSED, aes(x=Time_Learned, y=mean, ymin=mean-se, ymax=mean+se), width=.1,alpha=.1) +
#          scale_x_continuous(breaks = scales::pretty_breaks(n = 5))+
#          scale_y_continuous(limits = c(.5,1), breaks = seq(.5, 1, by = 0.1), labels = scales::percent) +
#          scale_color_manual(name="Learning condition",guide=col3,labels=c("Concurrent: Low\nCumulative: Low","Concurrent: Low\nCumulative: High","Concurrent: High\nCumulative: Low","Concurrent: High\nCumulative: High"),values=c("gold2", "gold4","gold2","gold4"))+
#          scale_shape_manual(name="Learning condition",guide=col3,labels=c("Concurrent: Low\nCumulative: Low","Concurrent: Low\nCumulative: High","Concurrent: High\nCumulative: Low","Concurrent: High\nCumulative: High"),values=c(1,1,19,19))+
#          
#          geom_smooth(data=subset(LTSED, condition_left==1 & Time_Learned<33), aes(x=Time_Learned, y=mean),size=2.05,span=1,color="gold2",alpha=.3,fill="gold2")+
#          geom_smooth(data=subset(LTSED, condition_left==1 & Time_Learned<33), aes(x=Time_Learned, y=mean),size=.75,span=1,color="white",alpha=.01,fill="white")+
#          
#          geom_smooth(data=subset(LTSED, condition_left==1 & Time_Learned>33), aes(x=Time_Learned, y=mean),size=2.05,span=1,color="gold2",alpha=.3,fill="gold2")+
#          geom_smooth(data=subset(LTSED, condition_left==1 & Time_Learned>33), aes(x=Time_Learned, y=mean),size=.75,span=1,color="white",alpha=.01,fill="white")+
#          
#          
#          geom_smooth(data=subset(LTSED, condition_left==2 & Time_Learned<33), aes(x=Time_Learned, y=mean),size=2.05,span=1,color="gold4",alpha=.3,fill="gold4")+
#          geom_smooth(data=subset(LTSED, condition_left==2 & Time_Learned<33), aes(x=Time_Learned, y=mean),size=.75,span=1,color="white",alpha=.01,fill="white")+
#          
#          geom_smooth(data=subset(LTSED, condition_left==2 & Time_Learned>33), aes(x=Time_Learned, y=mean),size=2.05,span=1,color="gold4",alpha=.3,fill="gold4")+
#          geom_smooth(data=subset(LTSED, condition_left==2 & Time_Learned>33), aes(x=Time_Learned, y=mean),size=.75,span=1,color="white",alpha=.01,fill="white")+
#          
#          geom_smooth(data=subset(LTSED, condition_left==3 & Time_Learned<33), aes(x=Time_Learned, y=mean),size=2.05,span=1,color="black",alpha=.01,fill="white")+
#          geom_smooth(data=subset(LTSED, condition_left==3 & Time_Learned<33), aes(x=Time_Learned, y=mean),size=1.75,span=1,color="gold2",alpha=.3,fill="gold2")+
#          geom_smooth(data=subset(LTSED, condition_left==3  & Time_Learned>33), aes(x=Time_Learned, y=mean),size=2.05,span=1,color="black",,alpha=.01,fill="white")+
#          geom_smooth(data=subset(LTSED, condition_left==3  & Time_Learned>33), aes(x=Time_Learned, y=mean),size=1.75,span=1,color="gold2",,alpha=.3,fill="gold2")+
#          
#          geom_smooth(data=subset(LTSED, condition_left==4 & Time_Learned<33), aes(x=Time_Learned, y=mean),size=1.75,span=1,color="gold4",,alpha=.2,fill="gold4")+
#          geom_smooth(data=subset(LTSED, condition_left==4  & Time_Learned>33), aes(x=Time_Learned, y=mean),size=1.75,span=1,color="gold4",,alpha=.2,fill="gold4")+
#          
#          annotate("text", x=15,y = .99, label = "First session",size=7,color='gray')+
#          annotate("text", x=48,y = .99, label = "Second session",size=7,color='gray')+
#          
#          geom_vline(xintercept = 32.5,linetype='dashed', col = 'gray')+
#          theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
#                panel.background = element_blank(), axis.line = element_line(colour = "black"), 
#                legend.position = c(.76,.23),legend.spacing.x = unit(.15, 'cm'),legend.spacing.y = unit(.15, 'cm'),legend.title = element_text(size=14),legend.text =element_text(size=10,margin = margin(t = 8)),axis.title.x = element_blank(), axis.text.x = element_text( size=16), axis.text.y = element_text(size=16),axis.title.y = element_blank(),  text=element_text(size=16))
#       
#       
#       
#       figure <- annotate_figure(figure,
#                                 # top = text_grob("Learning Curve", color = "black", face = "bold", size = 24),
#                                 fig.lab = "C", fig.lab.face = "bold",fig.lab.size = 24,
#                                 bottom=text_grob("Trials", color = "black", size = 20),
#                                 left=text_grob("Correct choice",rot=90,color = "black", size = 20)
#                                 
#       )
#       png(paste0('~/Google Drive/My Drive/Common lab folder/model-fitting-master/Context_Experiment/R Scripts/All_Scripts_Relevant/Model_Validation/traject',Sys.Date(),'.png'), width = 10, height = 7, units = "in",res=300)
#       print(figure)
#       dev.off()
#       return(figure)}
#    
#    
#    fig <- create_traj(LTSED,col3 )
#    
#    return(fig)}