# Script of all the scripts that I need 
require("RSQLite")
require("plyr")
require("dplyr")
require("ggplot2")
require("zoo")
require("modelr")
require("tidybayes")
require("gridExtra")
require("ggpubr")
require("tidyr")
require("stringr")
library(brms)
library(tidyr)
library(dplyr)
library(ggplot2)
require("mosaic")
require("simpleboot")
require(zoo)


# set some parameters for plotting the data 
y_axis_size=20;
y_text_size=16;
x_title_size=20;
x_text_size=16;

#-------------Useful functions----

#1 add time an image was Chosen and seen  
add_times_chosen <- function(Real_data){
      require(data.table)
      
      dat1<- setDT(FullSim)
      dat1[, c("stim1_seen", "stim2_seen") :=
                 lapply(.SD, function(z) mapply(function(x, S) {
                       sum(stim1[S] %in% x | stim2[S] %in% x)
                 }, z, lapply(seq_len(.N)-1, seq_len))),
           .SDcols = c("stim1", "stim2"), by = .(subject)
      ][, c("stim1_chosen", "stim2_chosen") :=
              lapply(.SD, function(z) mapply(function(x, S) {
                    sum(Chosen[S] %in% x)
              }, z, lapply(seq_len(.N)-1, seq_len))),
        .SDcols = c("stim1", "stim2"), by = .(subject)]
      
      return(dat1)}

#2  summarySE - gives mean, se, median and quantiles 
summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE,
                      conf.interval=.95, .drop=TRUE) {
      library(plyr)
      
      # New version of length which can handle NA's: if na.rm==T, don't count them
      length2 <- function (x, na.rm=FALSE) {
            if (na.rm) sum(!is.na(x))
            else       length(x)
      }
      
      # This does the summary. For each group's data frame, return a vector with
      # N, mean, and sd
      datac <- ddply(data, groupvars, .drop=.drop,
                     .fun = function(xx, col) {
                           c(N    = length2(xx[[col]], na.rm=na.rm),
                             mean = mean   (xx[[col]], na.rm=na.rm),
                             sd   = sd     (xx[[col]], na.rm=na.rm)
                           )
                     },
                     measurevar
      )
      
      # Rename the "mean" column    
      datac <- plyr::rename(datac, c("mean" = measurevar))
      
      datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean
      
      # Confidence interval multiplier for standard error
      # Calculate t-statistic for confidence interval: 
      # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
      ciMult <- qt(conf.interval/2 + .5, datac$N-1)
      datac$ci <- datac$se * ciMult
      
      return(datac)
}

#3  we add the absolute rank
add_absolute_rank <- function(Full_data){
      if(laptop==1){
            load("/Volumes/GoogleDrive/My Drive/Common lab folder/model-fitting-master/Context_Experiment/R Scripts/Data_Objects/Stimuli.RData")
      }else{
            load("G:/My Drive/Common lab folder/model-fitting-master/Context_Experiment/R Scripts/Data_Objects/Stimuli.RData")
      }
      
      # add the rank 
      Full_data <- Full_data %>% mutate(rank_left=stimuli_Reward$rank[Full_data$stim1+1],rank_right=stimuli_Reward$rank[Full_data$stim2+1])
      return(Full_data)}


# add session
add_session <- function(data){
      
      session=1;
      new_session= c(13,15,17,19,21,23,25,27,29,31,33,35,37,39,41,43,45,47,49,51,53,55);
      data$session[1]=session; 
      
      for (i in 2:nrow(data)){
            if(data$subject[i] !=data$subject[i-1]){
                  session=1;
            }
            else if(data$block[i] %in% new_session & !(data$block[i-1] %in% new_session)){
                  session=session+1; 
                  
            }
            data$session[i]=session
            
      }
      
      return(data);
}
# add accuracy -not based on experience probabilities but rather the latent probability 
add_absolute_accuracy <- function(data,stimuli){
      # data is the data frame 
      # stimuli is what I will use for objective accuracy 
      library(dplyr)
      data <- left_join(data, stimuli, by=c("stim1"="number"))
      data <- data %>% mutate(EV_Stim1=reward,rank_left=rank) %>% select(-c(reward,image,punishment,rank,condition,numBlocksBetweenNoFeedbackBlocks,numMaxRepetitionNoFeedbackBlocks))
      data <- left_join(data, stimuli, by=c("stim2"="number"))
      data <- data %>% mutate(EV_Stim2=reward,rank_right=rank) %>% select(-c(reward,image,punishment,rank,condition,numBlocksBetweenNoFeedbackBlocks,numMaxRepetitionNoFeedbackBlocks))
      
      data <- data %>% mutate(Accurate=case_when(EV_Stim1==EV_Stim2 ~ NaN,
                                                 (EV_Stim1>EV_Stim2 & choice==0) | (EV_Stim1<EV_Stim2 & choice==1)~ 1,
                                                 TRUE ~ 0))
      
      return(data)}

# useful for counterfactual effects of other options outcomes 
LastSeen <- function(Fulldata){
      
      last_time_right <- c()
      last_time_left <- c()
      for(i in 1:NROW(Fulldata)){
            left <- Fulldata$stim1[i];
            right <- Fulldata$stim2[i];
            current_subject <- Fulldata$subject[i];
            current_block <- Fulldata$block[i];
            current_trial <- Fulldata$trial[i];
            last_time_chosen_right <- Fulldata %>% filter(Chosen==right, feedback==1,subject==current_subject,trial < current_trial & block<=current_block | block < current_block) %>%   select(trial) %>% tail(1)
            last_time_chosen_left <-  Fulldata %>%  filter(Chosen==left,  feedback==1,subject==current_subject,trial < current_trial & block<=current_block | block < current_block) %>%  select(trial) %>% tail(1)
            last_time_rejected_right <- Fulldata %>% filter(Rejected==right, feedback==1,subject==current_subject,trial < current_trial & block<=current_block | block < current_block) %>% select(trial) %>% tail(1)
            last_time_rejected_left <-  Fulldata %>%  filter(Rejected==left,  feedback==1,subject==current_subject,trial < current_trial & block<=current_block | block < current_block) %>% select(trial) %>% tail(1)

            if(!empty(last_time_rejected_left) & !empty(last_time_chosen_left)){
                  Fulldata$LastTimeLeft[i] <-  last_time_rejected_left<last_time_chosen_left;
            }
            else if(!empty(last_time_rejected_left)){
                  Fulldata$LastTimeLeft[i]=0
            }else if(empty(last_time_rejected_left) & !empty(last_time_chosen_left)  ){
                  Fulldata$LastTimeLeft[i]=1
            }
            else{
                  Fulldata$LastTimeLeft[i] <- NaN
            }
            if(!empty(last_time_rejected_right) & !empty(last_time_chosen_right)){
                  Fulldata$LastTimeRight[i] <-  last_time_rejected_right <  last_time_chosen_right;
            } else if(!empty(last_time_rejected_right)){
                  Fulldata$LastTimeRight[i]=0
            }else if(empty(last_time_rejected_right)){
                  Fulldata$LastTimeRight[i]=1
            }
            else{
                  Fulldata$LastTimeRight[i] <- NaN
            }
      }
      
      
      return(Fulldata);   
}


# infer subjects relative ranks 
RelativeRank <- function(Full_data){
      No_feedback <- Full_data %>% filter(feedback==0)
      Feedback <- Full_data %>% filter(feedback==1)
      
      Feed_3 <- Feedback %>% filter(stim1<31)
      Revalant <- Feed_3 %>% select(block,trial,stim1,stim2,choice,relatively_correct,rank_left,rank_right,subject,Chosen,condition_left)
      chosen <- as.data.frame(table(Revalant$Chosen,Revalant$subject,Revalant$block))
      chosen  <- chosen %>% filter(Freq>0)
      colnames(chosen)=c('Stimuli','Subject','Block','TChosen')
      chosen %>% group_by(Subject,Block) %>% mutate(Percentage=paste0(round(TChosen/sum(TChosen)*100,2),"%")) -> chosen
      chosen %>% group_by(Subject,Block)%>%  mutate(rank_relative=dense_rank(desc(TChosen))) -> chosen
      
      chosen %>%
            mutate(Percentage = readr::parse_number(Percentage)) %>%
            arrange(Subject, Block, Percentage) %>%
            group_by(Subject, Block) %>%
            filter(Percentage - lag(Percentage, default = -Inf) > 10 & 
                         lead(Percentage, default = Inf) - Percentage > 10) %>%
            ungroup -> chosen
      
      ranked <- ddply(chosen,.(Subject,Stimuli),summarize,mean=mean(rank_relative)) 
      
      # those that are consistnelty ranked the lowest 
      ranked_3 <- ranked %>% filter(floor(mean)==mean)
      
      # now  for the high concurrent diversity condition 
      Feed_6 <- Feedback %>% filter(stim1>31)
      # what we want to do is count how many choices there were per column
      Feed_6 %>% group_by(subject,session,stim1,stim2,Chosen) %>% tally() -> check
      library("data.table")
      check <- setDT(check)
      # Switch values of stim2 and stim1 if stim2 < stim1
      check <- check[stim2 < stim1, `:=`(stim1 = stim2, stim2 = stim1)]
      # Now summarise and sum
      check <- check[, .(n = sum(n, na.rm = TRUE)), by = .(subject,session,stim1, stim2, Chosen)]
      ## Now we want to rank the ones that we do have 
      check <- check %>% rename(Tchosen=n)
      library(tidyr)
      check %>% 
            #transform table 
            # subjecct - stim1 - stim2 - votes for stim1 (Tc1) -
            # votes for stim2 (Tc2)
            group_by(subject,session,stim1, stim2) %>%
            mutate(id = ifelse(stim1 == Chosen, 1, 2)) %>%
            ungroup() %>%
            pivot_wider(id_cols = -c(Tchosen, id, Chosen ),
                        names_from = id, 
                        names_prefix = "Tc",
                        values_from = c(Tchosen),
                        values_fill = 0) %>%
            #how many votes got stim1 over stim2 
            #(positive diff - stim1 got more votes, 
            #negative ifff - stim2 go more votes)
            mutate(diff = Tc1- Tc2) %>%
            # transformed stims into long format
            pivot_longer(cols = c(stim1, stim2),
                         values_to = "stim") %>% 
            group_by(subject,session, stim) %>%
            # how often was another stim chosen
            summarise(loss = sum(name == "stim1" & diff < 0 |  
                                       name  == "stim2" & diff > 0 )
            )%>%
            # rank losses (less losses - higher rank)
            mutate(rank = loss + 1)  %>%
            select(-loss) -> checks
      
      
      
      ranked_6 <- ddply(checks,.(subject,stim),summarize,mean=mean(rank)) 
      # those that are consistnelty ranked the lowest 
      ranked_6 <- ranked_6 %>% filter(floor(mean)==mean) %>% rename(Subject=subject,Stimuli=stim)
      full_rank  <- rbind(ranked_6,ranked_3)
      
      
      full_rank$mean[full_rank$mean==3]=0
      full_rank$mean[full_rank$mean==1]=5
      full_rank$mean[full_rank$mean==2]=1
      full_rank$mean[full_rank$mean==5]=2
      
      
      ranked_by_stim <- full_rank
      for(i in 1:NROW(No_feedback)){
            if(length(ranked_by_stim$mean[ranked_by_stim$Subject== No_feedback$subject[i] & ranked_by_stim$Stimuli==No_feedback$stim1[i]])>0){
                  No_feedback$rank_rel_left[i]=ranked_by_stim$mean[ranked_by_stim$Subject== No_feedback$subject[i] & ranked_by_stim$Stimuli==No_feedback$stim1[i]]
            }else{
                  No_feedback$rank_rel_left[i]=NaN;
                  
            }
            if(length(ranked_by_stim$mean[ranked_by_stim$Subject== No_feedback$subject[i] & ranked_by_stim$Stimuli==No_feedback$stim2[i]])>0){
                  No_feedback$rank_rel_right[i]=ranked_by_stim$mean[ranked_by_stim$Subject== No_feedback$subject[i] & ranked_by_stim$Stimuli==No_feedback$stim2[i]]
            }else{
                  No_feedback$rank_rel_right[i]=NaN;
                  
            }
      }
      No_feedback <- No_feedback %>% filter(!is.na(rank_rel_right),!is.na(rank_rel_left))
      
      
      
      
      return(No_feedback);}

# visualizing the rank bias 
plot_rank_bias <- function(Full_data){
      library(raincloudplots)
      library("wesanderson")
      library(brms)
      library(tidyr)
      library(dplyr)
      library(ggplot2)
      
      if (!require(remotes)) {
            install.packages("remotes")
      }
      remotes::install_github('jorvlan/raincloudplots')
      library(rstan)
      library(bayesplot)
      # add session numbers 
      Full_data <- Full_data %>% mutate(difficulty=abs(relative_stim1-relative_stim2))
      No_feedback=RelativeRank(Full_data)
      run_bias <- function(data,typer){
            dats <- data %>% 
                  filter(learnedtogether==0,difficulty<.1) %>% 
                  filter(abs(rank_right -rank_left)>=1) %>% 
                  mutate(bias=ifelse((rank_right>rank_left & choice==1) | (rank_right<rank_left & choice==0),1,0))  
            if(typer==1){
                  dats <- dats %>% group_by(subject,three)}
            else{
                  dats <- dats %>% group_by(subject,two)}
            dats %>%  dplyr::summarize(bias=mean(bias,na.rm=TRUE),number=n()) %>% 
                  ungroup() -> bias_by_group
            return(bias_by_group)}
      
      bias_by_group <- run_bias(No_feedback,1)
      bias_by_group2 <- run_bias(No_feedback,2)
      
      a1= bias_by_group %>% filter(three==0) 
      a2= bias_by_group %>% filter(three==1)
      
      array_1=a1$bias
      array_2=a2$bias
      
      dor <- data_1x1(
            
            array_1= array_1,
            array_2= array_2,
            jit_distance = .09,
            jit_seed = 321)
      
      
      a241= bias_by_group2 %>% filter(two==0) 
      a242= bias_by_group2 %>% filter(two==1)
      array_2=a241$bias
      array_4=a242$bias
      
      dor24 <- data_1x1(
            
            array_1= array_2,
            array_2= array_4,
            jit_distance = .09,
            jit_seed = 321)
      
      
      get_summary_bias <- function(data,type){
            if(type==1){
                  sumrepdat <- summarySE(data, measurevar = "bias", groupvars=c("three"))
            } else{
                  
                  sumrepdat <- summarySE(data, measurevar = "bias", groupvars=c("two")) 
                  
            }
            return(sumrepdat)}
      
      sumrepdat36 <- get_summary_bias(bias_by_group,1)
      sumrepdat24 <- get_summary_bias(bias_by_group2,2)
      
      sumrepdat36$six <- c(0,1)
      
      bias <- rep_replace(
            data = dor,
            colors = (c('dodgerblue', 'orchid4')),
            fills = (c('dodgerblue', 'orchid4')),
            line_color = 'gray',
            line_alpha = .5,
            size = 6,
            alpha = .6,
            align_clouds = FALSE) +
            
            geom_line(data = sumrepdat36, aes(x = as.numeric(six)+1, y = bias), linetype = 1)+
            geom_point(data = sumrepdat36, aes(x = as.numeric(six)+1, y = bias, group=six),shape=20, size=3) +
            geom_errorbar(data = sumrepdat36, aes(x = as.numeric(six)+1, y = bias, group=six, ymin = bias-se, ymax = bias+se), width = .1)+
            geom_smooth(data=dor,aes(x=x_axis,y=y_axis),method = "lm",color="black", formula = y ~ x) +
            # geom_quantile(data=ld24,aes(x=x_axis,y=y_axis),quantiles = 0.5,color="black",se=TRUE)+
            ggtitle(" ")+
            scale_x_continuous(breaks=c(1,2),name="Concurrent diversity", labels=c("Low", "High")) +
            scale_y_continuous(limits = c(0,1), breaks = seq(0, 1, by = 0.25), labels = scales::percent) +
            # ggtitle(sprintf("Testing (p>%.02f)",.05))+
            #  ylab("Preference for higher\n ranked option")+
            
            theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                  panel.background = element_blank(), 
                  axis.line = element_line(colour = "black"), 
                  legend.position = "none",
                  axis.title.y = element_blank(),
                  axis.text.y = element_text(size=16),
                  axis.title.x = element_text(size=20,face="bold"),
                  axis.text.x = element_text(size=16))
      
      
      
      
      
      
      bias24 <- rep_replace(
            data = dor24,
            colors = (values=wes_palette(n=2, name="Moonrise2")),
            fills = (values=wes_palette(n=2, name="Moonrise2")),
            line_color = 'gray',
            line_alpha = .5,
            size = 6,
            alpha = .6,
            align_clouds = FALSE) +
            
            geom_line(data = sumrepdat24, aes(x = as.numeric(two)+1, y = bias), linetype = 1)+
            geom_point(data = sumrepdat24, aes(x = as.numeric(two)+1, y = bias, group=two),shape=20, size=3) +
            geom_errorbar(data = sumrepdat24, aes(x = as.numeric(two)+1, y = bias, group=two, ymin = bias-se, ymax = bias+se), width = .1)+
            geom_smooth(data=dor24,aes(x=x_axis,y=y_axis),method = "lm",color="black", formula = y ~ x) +
            # geom_quantile(data=ld24,aes(x=x_axis,y=y_axis),quantiles = 0.5,color="black",se=TRUE)+
            
            scale_x_continuous(breaks=c(1,2),name="Cumulative diversity", labels=c("Low", "High")) +
            scale_y_continuous(limits = c(0,1), breaks = seq(0, 1, by = 0.25), labels = scales::percent) +
            # ggtitle(sprintf("Testing (p>%.02f)",.05))+
            # ylab("Correct Choice")+
            
            theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                  panel.background = element_blank(), 
                  axis.line = element_line(colour = "black"), 
                  legend.position = "none",axis.ticks.y = element_blank(),
                  axis.title.y = element_blank(),
                  
                  axis.line.y = element_blank(),
                  axis.title.x = element_text(size=20,face="bold"),
                  axis.text.x = element_text(size=16),  ,
                  axis.text.y = element_blank())
      
      # 
      
      figure <- ggarrange(bias,bias24,nrow=1,widths=c(1.25,1))
      
      figure <- annotate_figure(figure,
                                fig.lab = "A",
                                fig.lab.size = 24,fig.lab.face = "bold",
                                left=text_grob("Preference for higher\n ranked option",size=20,rot=90))
      #
      # 
      if(laptop){
            png(paste0('~/Google Drive/My Drive/Common lab folder/model-fitting-master/Context_Experiment/R Scripts/All_Scripts_Relevant/Model_Validation/rank',Sys.Date(),'.png'), width = 8, height = 6, units = "in",res=300)
            print(figure)
            dev.off()
            # ggsave(paste0('~/Google Drive/My Drive/Common lab folder/model-fitting-master/Context_Experiment/R Scripts/All_Scripts_Relevant/Model_Validation/rank_bias_nototmm',Sys.Date(),'.png'), width = 8, height = 6, units = "in")
      }else{
            ggsave(paste0('G:/My Drive/Common lab folder/model-fitting-master/Context_Experiment/R Scripts/All_Scripts_Relevant/Model_Validation/rank_bias_notomm',Sys.Date(),'.png'), width = 8, height = 6, units = "in")
      }
      #
      
      
      return(figure)}




# this i use for my visualization - creates all of the plots using run_replace 
globalVariables(c("ggplot", "aes", "geom_point", "geom_line",
                  "geom_half_violin",
                  "position_nudge", "%>%", "position", "id"))
library(gghalves)
rep_replace <- function (data_1x1, colors = (c("dodgerblue", "darkorange")), 
                         fills = (c("dodgerblue", "darkorange")), line_color = "gray", 
                         line_alpha = 0., size = 1.5, alpha = 0.6, align_clouds = FALSE) 
{
      if (align_clouds == FALSE) {
            figure_1x1 <- ggplot(data = data_1x1) + geom_point(data = data_1x1 %>% 
                                                                     dplyr::filter(x_axis == "1"), aes(x = jit, y = y_axis), 
                                                               color = colors[1], size = size, alpha = alpha, show.legend = FALSE) + 
                  geom_point(data = data_1x1 %>% dplyr::filter(x_axis == 
                                                                     "2"), aes(x = jit, y = y_axis), color = colors[2], 
                             size = size, alpha = alpha, show.legend = FALSE) + 
                  geom_line(aes(x = jit, y = y_axis, group = id), color = line_color, 
                            alpha = line_alpha, show.legend = FALSE)+ #+ geom_half_boxplot(data = data_1x1 %>% 
                  #                                                                    dplyr::filter(x_axis == "1"), aes(x = x_axis, y = y_axis), 
                  #                                                              color = colors[1], fill = fills[1], position = position_nudge(x = -0.3), 
                  #                                                              side = "r", outlier.shape = NA, center = TRUE, errorbar.draw = FALSE, 
                  #                                                              width = 0.2, alpha = alpha, show.legend = FALSE) + 
                  # geom_half_boxplot(data = data_1x1 %>% dplyr::filter(x_axis == 
                  #                                                           "2"), aes(x = x_axis, y = y_axis), color = colors[2], 
                  #                   fill = fills[2], position = position_nudge(x = 0.2), 
                  #                   side = "r", outlier.shape = NA, center = TRUE, 
                  #                   errorbar.draw = FALSE, width = 0.2, alpha = alpha, 
                  #                   show.legend = FALSE) 
                  geom_half_violin(data = data_1x1 %>% 
                                         dplyr::filter(x_axis == "1"), aes(x = x_axis, y = y_axis),
                                   color = colors[1], fill = fills[1], position = position_nudge(x = -0.2),
                                   side = "l", alpha = alpha, show.legend = FALSE) +
                  geom_half_violin(data = data_1x1 %>% dplyr::filter(x_axis == 
                                                                           "2"), aes(x = x_axis, y = y_axis), color = colors[2], 
                                   fill = fills[2], position = position_nudge(x = 0.2), 
                                   side = "r", alpha = alpha, show.legend = FALSE)
      }
      else if (align_clouds == TRUE) {
            figure_1x1 <- ggplot(data = data_1x1) + geom_point(data = data_1x1 %>% 
                                                                     dplyr::filter(x_axis == "1"), aes(x = jit, y = y_axis), 
                                                               color = colors[1], fill = fills[1], size = size, 
                                                               alpha = alpha, show.legend = FALSE) + geom_point(data = data_1x1 %>% 
                                                                                                                      dplyr::filter(x_axis == "2"), aes(x = jit, y = y_axis), 
                                                                                                                color = colors[2], fill = fills[2], size = size, 
                                                                                                                alpha = alpha, show.legend = FALSE) + geom_line(aes(x = jit, 
                                                                                                                                                                    y = y_axis, group = id), color = line_color, alpha = line_alpha, 
                                                                                                                                                                show.legend = FALSE) + geom_half_boxplot(data = data_1x1 %>% 
                                                                                                                                                                                                               dplyr::filter(x_axis == "1"), aes(x = x_axis, y = y_axis), 
                                                                                                                                                                                                         color = colors[1], fill = fills[1], position = position_nudge(x = 1.3), 
                                                                                                                                                                                                         side = "r", outlier.shape = NA, center = TRUE, errorbar.draw = FALSE, 
                                                                                                                                                                                                         width = 0.2, alpha = alpha, show.legend = FALSE) + 
                  # geom_half_boxplot(data = data_1x1 %>% dplyr::filter(x_axis == 
                  #                                                           "2"), aes(x = x_axis, y = y_axis), color = colors[2], 
                  #                   fill = fills[2], position = position_nudge(x = 0.2), 
                  #                   side = "r", outlier.shape = NA, center = TRUE, 
                  #                   errorbar.draw = FALSE, width = 0.2, alpha = alpha, 
                  #                   show.legend = FALSE) + geom_half_violin(data = data_1x1 %>% 
                  #                                                                 dplyr::filter(x_axis == "1"), aes(x = x_axis, y = y_axis), 
                  #                                                           color = colors[1], fill = fills[1], position = position_nudge(x = 1.43), 
                  #                                                           side = "r", alpha = alpha, show.legend = FALSE) + 
                  geom_half_violin(data = data_1x1 %>% dplyr::filter(x_axis == 
                                                                           "2"), aes(x = x_axis, y = y_axis), color = colors[2], 
                                   fill = fills[2], position = position_nudge(x = 0.43), 
                                   side = "r", alpha = alpha, show.legend = FALSE)
      }
      return(figure_1x1)
}

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

# how do subjects perform on learning trials as a functin of condition
learnin_advantage <- function(Full_data){
      library(cowplot)
      library(dplyr)
      library(readr)
      library(simpleboot)
      library("wesanderson")
      
      if (!require(remotes)) {
            install.packages("remotes")
      }
      remotes::install_github('jorvlan/raincloudplots')
      
      library(raincloudplots)
      
      Learning_three <- function(Full_data){ 
            Full_data %>% filter(feedback==1)-> L_T
            
            learning_only <- L_T %>% group_by(subject,three) %>% summarize(mean=mean(relatively_correct,na.rm=TRUE)) 
            
            return(learning_only);}
      learning_only <- Learning_three(Full_data) %>% mutate(six=ifelse(three==1,0,1))
      learning_only %>% select(-three) %>% pivot_wider(names_from="six",values_from='mean')-> sum_data
      colnames(sum_data)<- c("subject","three","six")
      learning_only <- learning_only %>% select(-three)
      sumrepdat <- summarySE(learning_only, measurevar = "mean", groupvars=c("six"))
      sumrepdat$six <- c(1,0)
      ld <- data_1x1(
            
            array_1= sum_data$three,
            array_2= sum_data$six,
            jit_distance = .09,
            jit_seed = 321)
      
      learn1 <- rep_replace(
            data = ld,
            colors = (c('dodgerblue', 'orchid4')),
            fills = (c('dodgerblue', 'orchid4')),
            line_color = 'gray',
            line_alpha = .5,
            size = 5,
            alpha = .6,
            align_clouds = FALSE) +
            
            geom_line(data = sumrepdat, aes(x = as.numeric(six)+1, y = mean), linetype = 1)+
            geom_point(data = sumrepdat, aes(x = as.numeric(six)+1, y = mean, group=six),shape=20, size=1.4) +
            geom_errorbar(data = sumrepdat, aes(x = as.numeric(six)+1, y = mean, group=six, ymin = mean-se, ymax = mean+se), width = .1)+
            geom_smooth(data=ld,aes(x=x_axis,y=y_axis),method='lm',color="black", formula = y ~ x) +
            
            scale_x_continuous(breaks=c(1,2),name="Concurrent", labels=c("Low", "High")) +
            scale_y_continuous(limits = c(.5,1), breaks = seq(.5, 1, by = 0.1), labels = scales::percent) +
            
            theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                  panel.background = element_blank(), axis.line = element_line(colour = "black"), 
                  legend.position = "none",axis.text.x = element_text(size=16),  axis.title.x = element_text(size=20),axis.text.y = element_text( size=16),axis.title.y = element_blank(), text=element_text(size=24))
      
      
      Full_data <- Full_data %>% mutate(two=ifelse(condition_left==1 |condition_left==3,1,0))
      
      
      Learning_two <- function(Full_data){ 
            Full_data %>% filter(feedback==1)-> L_T
            
            learning_only <- L_T %>% group_by(subject,two) %>% summarize(mean=mean(relatively_correct,na.rm=TRUE)) 
            
            return(learning_only);}
      
      
      learning_only_two <- Learning_two(Full_data)
      learning_only_two %>%  pivot_wider(names_from="two",values_from='mean')-> two_four
      colnames(two_four)<- c("subject","four","two")
      learning_only_two %>% mutate(four=ifelse(two==1,0,1)) %>% select(-two)-> learning_only_two
      sumrepdat4 <- summarySE(learning_only_two, measurevar = "mean", groupvars=c("four"))
      
      
      
      dor <- data_1x1(
            
            array_1= two_four$two,
            array_2= two_four$four,
            jit_distance = .09,
            jit_seed = 321)
      
      # make sure we are plotting the means 
      a  <- learning_only_two[learning_only_two$four==0,]
      b   <- learning_only_two[learning_only_two$four==1,]
      
      
      
      learn2 <- rep_replace(
            data = dor,
            colors = (values=wes_palette(n=2, name="Moonrise2")),
            fills = (values=wes_palette(n=2, name="Moonrise2")),
            line_color = 'gray',
            line_alpha = .5,
            size = 5,
            alpha = .6,
            align_clouds = FALSE) +
            
            geom_line(data = sumrepdat4, aes(x = as.numeric(four)+1, y = mean), linetype = 1)+
            geom_point(data = sumrepdat4, aes(x = as.numeric(four)+1, y = mean, group=four),shape=20, size=1.4) +
            geom_errorbar(data = sumrepdat4, aes(x = as.numeric(four)+1, y = mean, group=four, ymin = mean-se, ymax = mean+se), width = .1)+
            geom_smooth(data=dor,aes(x=x_axis,y=y_axis),method = "lm",color="black", formula = y ~ x) +
            # geom_quantile(aes(x=quantiles = 0.5)+
            # geom_quantile(data=ld24,aes(x=x_axis,y=y_axis),quantiles = 0.5,color="black",se=TRUE)+
            
            scale_x_continuous(breaks=c(1,2), name="Cumulative",labels=c("Low", "High")) +
            scale_y_continuous(limits = c(.5,1), breaks = seq(.5, 1, by = 0.1), labels = scales::percent) +
            # ggtitle(sprintf("Learning (p>%.02f)",.05))+
            # ylab("Correct Choice")+
            
            theme(panel.grid.major = element_blank(),axis.line.y =element_blank(), panel.grid.minor = element_blank(),
                  panel.background = element_blank(), axis.line = element_line(colour = "black"), 
                  legend.position = "none",axis.ticks.y = element_blank(),axis.title.y = element_blank(),axis.title.x = element_text(size=20),axis.text.x = element_text(size=16),axis.text.y = element_blank(), text=element_text(size=24))
      
      
      figure <- ggarrange(learn1,learn2,nrow=1)
      figure <- annotate_figure(figure,
                                #top = text_grob("Learning Phase", color = "black", face = "bold", size = 24),
                                fig.lab = "B", fig.lab.face = "bold",fig.lab.size = 24,
                                left = text_grob("Correct choice", color = "black", size = 20,rot=90),
                                
      )
      
      
      
      
      
      if(.Platform$OS.type == "unix") {
            laptop=1;
      } else {
            laptop=0;
      }
      
      
      
      if(laptop){
            png(paste0('~/Google Drive/My Drive/Common lab folder/model-fitting-master/Context_Experiment/R Scripts/All_Scripts_Relevant/Model_Validation/learning',Sys.Date(),'.png'), width = 9, height = 6, units = "in",res=300)
            print(figure)
            dev.off()
      }else{
            
            ggsave(paste0('G:/My Drive/Common lab folder/model-fitting-master/Context_Experiment/R Scripts/All_Scripts_Relevant/Model_Validation/Learning_advantage',Sys.Date(),'.png'), width = 12, height = 7, units = "in")
            
      }
      
      
      return(figure)}

# compare novel pair and learnt pair trial performance 
generalization_dropoff <- function(Real){
      
      library(raincloudplots)
      library("wesanderson")
      if (!require(remotes)) {
            install.packages("remotes")
      }
      remotes::install_github('jorvlan/raincloudplots')
      
      library(raincloudplots)
      
      
      Real <- Real %>% mutate(three=ifelse(condition_left <3,1,0),two=ifelse(condition_left==1|condition_left==3,1,0))
      
      Recall_transfer<- function(Real,type){
            No_feedback <- Real %>% filter(feedback==0,condition_left==condition_right)
            if(type==1){
                  LT_types=ddply(No_feedback,.(subject,three,learnedtogether),summarize,mean=mean(relatively_correct,na.rm = TRUE)) 
                  LT_type <- LT_types %>% pivot_wider(names_from="learnedtogether",values_from="mean") 
                  colnames(LT_type) <- c("subject","three","NLT","LT")
                  
            }else{
                  LT_types=ddply(No_feedback,.(subject,two,learnedtogether),summarize,mean=mean(relatively_correct,na.rm = TRUE)) 
                  LT_type <- LT_types %>% pivot_wider(names_from="learnedtogether",values_from="mean") 
                  colnames(LT_type) <- c("subject","two","NLT","LT") 
                  
            }
            
            LT_type <- LT_type %>% mutate(diff=NLT-LT)
            
            return(LT_type)}
      
      
      three_lt <- Recall_transfer(Real_data,1)
      
      
      three_lt %>% select(-NLT,-LT) %>% pivot_wider(names_from="three",values_from=c('diff'))-> sum_data
      colnames(sum_data) <- c("subject","six",'three')
      for_summary <- sum_data  %>% pivot_longer(col = c("six","three"),names_to = "type",values_to="mean" )  
      for_summary$type=ifelse(for_summary$type=="three",0,1) 
      sumrepdat <- summarySE(for_summary, measurevar = "mean", groupvars=c("type"))
      
      ld <- data_1x1(
            
            array_1= sum_data$six,
            array_2= sum_data$three,
            jit_distance = .09,
            jit_seed = 321)
      sumrepdat$type <- c(1,0)
      
      recall1 <- rep_replace(
            data = ld,
            colors = (c('dodgerblue', 'orchid4')),
            fills = (c('dodgerblue', 'orchid4')),
            line_color = 'gray',
            line_alpha = .5,
            size = 6,
            alpha = .6,
            align_clouds = FALSE) +
            
            geom_line(data = sumrepdat, aes(x = as.numeric(type)+1, y = mean), linetype = 1)+
            geom_point(data = sumrepdat, aes(x = as.numeric(type)+1, y = mean, group=type),shape=20, size=3) +
            geom_errorbar(data = sumrepdat, aes(x = as.numeric(type)+1, y = mean, group=type, ymin = mean-se, ymax = mean+se), width = .1)+
            geom_smooth(data=ld,aes(x=x_axis,y=y_axis),method='lm',color="black", formula = y ~ x) +
            
            scale_x_continuous(breaks=c(1,2),name="Concurrent diversity", labels=c("Low", "High")) +
            scale_y_continuous(limits = c(-.25,.1), breaks = seq(-.2, .1, by = 0.1), labels = scales::percent) +
            #ylab("Novel Pair  -  Learnt Pair\nAccuracy Difference")+
            
            theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                  panel.background = element_blank(), axis.line = element_line(colour = "black"), 
                  legend.position = "none",
                  axis.title.x = element_text(size=20),
                  axis.text.x = element_text(size=16),  
                  axis.text.y = element_text( size=16),
                  axis.title.y = element_blank())
      
      
      Real <- Real %>% mutate(two=ifelse(condition_left==1 |condition_left==3,1,0))
      
      
      two_four_recall<- Recall_transfer(Real,2)
      
      
      
      two_four_recall %>% select(-NLT,-LT) %>% pivot_wider(names_from="two",values_from=c('diff'))-> sum_data
      colnames(sum_data) <- c("subject","four",'two')
      for_summary24 <- sum_data  %>% pivot_longer(col = c("four","two"),names_to = "type",values_to="mean" )  
      for_summary24$type=ifelse(for_summary24$type=="two",0,1) 
      sumrepdat24 <- summarySE(for_summary24, measurevar = "mean", groupvars=c("type"))
      
      
      
      
      ldbe <- data_1x1(
            
            array_1= sum_data$two,
            array_2= sum_data$four,
            jit_distance = .09,
            jit_seed = 321)
      
      recall2 <- rep_replace(
            data = ldbe,
            colors = (values=wes_palette(n=2, name="Moonrise2")),
            fills = (values=wes_palette(n=2, name="Moonrise2")),
            line_color = 'gray',
            line_alpha = .5,
            size = 6,
            alpha = .6,
            align_clouds = FALSE) +
            
            geom_line(data = sumrepdat24, aes(x = as.numeric(type)+1, y = mean), linetype = 1)+
            geom_point(data = sumrepdat24, aes(x = as.numeric(type)+1, y = mean, group=type),shape=20, size=3) +
            geom_errorbar(data = sumrepdat24, aes(x = as.numeric(type)+1, y = mean, group=type, ymin = mean-se, ymax = mean+se), width = .1)+
            geom_smooth(data=ldbe,aes(x=x_axis,y=y_axis),method='lm',color="black", formula = y ~ x) +
            
            scale_x_continuous(breaks=c(1,2),name="Cumulative diversity", labels=c("Low", "High")) +
            scale_y_continuous(limits = c(-.25,.12), breaks = seq(-.2, .1, by = 0.1), labels = scales::percent) +
            #  ylab("Correct Choice")+
            
            theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                  panel.background = element_blank(), 
                  axis.line = element_line(colour = "black"), 
                  axis.line.y=element_blank(),
                  legend.position = "none",
                  axis.ticks.y = element_blank(),axis.title.y = element_blank(),
                  axis.title.x = element_text(size=20),axis.text.x = element_text(size=16),  axis.title =element_text(size=14),axis.text.y = element_blank())
      
      
      figure <- ggarrange(recall1,recall2,nrow=1,widths=c(1.25,1))
      figure <- annotate_figure(figure,
                                left=text_grob("Novel pair  -  Learnt pair\naccuracy difference",size = 18,rot=90)
      )
      
      
      # we save the data 
      if(.Platform$OS.type == "unix") {
            laptop=1;
      } else {
            laptop=0; 
      }
      
      
      
      if(laptop){
            #  ggsave(paste0('~/Google Drive/My Drive/Common lab folder/model-fitting-master/Context_Experiment/R Scripts/All_Scripts_Relevant/Model_Validation/Learnedtogethera',Sys.Date(),'.png'), width = 11, height = 6, units = "in")
            png(paste0('~/Google Drive/My Drive/Common lab folder/model-fitting-master/Context_Experiment/R Scripts/All_Scripts_Relevant/Model_Validation/generalization',Sys.Date(),'.png'), width = 8, height = 5, units = "in",res=300)
            print(figure)
            dev.off()
      }else{
            
            ggsave(paste0('G:/My Drive/Common lab folder/model-fitting-master/Context_Experiment/R Scripts/All_Scripts_Relevant/Model_Validation/Learnedtogethera',Sys.Date(),'.png'), width = 11, height = 6, units = "in")
            
      }
      
      return(figure)}




# now we prepare for the log odds dropoff 

LastOrNa <- function(x) {
      if (length(x) == 0) {
            return(NA)
      }
      return(last(x))
}

allOrNa <- function(x,last_eq,outcome) {
      if (length(x) == 0) {
            return(NA)
      }
      return(sum(outcome[x>last_eq]))
}

LastEq <- function(x, y) {
      res <- sapply(2:length(x), function(t) {
            LastOrNa(which(
                  (x[1:(t - 1)] == x[t] & y[1:(t - 1)] == y[t]) |
                        (x[1:(t - 1)] == y[t] & y[1:(t - 1)] == x[t])
            ))
      }
      )
      return(c(NA, res))
}


Last1 <- function(x, y) {
      res <- sapply(2:length(x), function(t) {
            LastOrNa(which(
                  (x[t] == y[1:t-1])
            ))
      }
      )
      return(c(NA, res))
}

all_previous <- function(x, Rejected,last_eq,outcome) {
      res <- sapply(2:length(x), function(t) {
            # in which do they match subsequent to equation 
            # we take the outcome 
            sum(outcome[(which((x[t] == Rejected[1:t-1])))]*(which((x[t] == Rejected[1:t-1]))>last_eq[t]))   }
      )
      return(c(NA, res))
}



add_accuracy_real <- function(sim_data){
      data <- c()
      real=1
      for(i in 1:length(unique(sim_data$subject))){
            if(real==0){
                  subk <- sim_data %>% filter(subject==unique(sim_data$subject)[i])
            }else{
                  subk <- sim_data %>% filter(subject==unique(sim_data$subject)[i])
                  
                  
            }
            if(NROW(subk)>100){
                  subk$trial=1:NROW(subk)
                  # get relative accuracy 
                  subk$relative_stim1=rep(NaN, nrow(subk))
                  subk$relative_stim2=rep(NaN, nrow(subk))
                  for (i in 2:nrow(subk)){
                        
                        subk$relative_stim1[i]=mean(subk$outcome[which((subk$stim1==subk$stim1[i]&subk$choice==0&subk$feedback==1& subk$trial<subk$trial[i]) | (subk$stim2==subk$stim1[i]&subk$choice==1&subk$feedback==1& subk$trial<subk$trial[i]))])
                        subk$relative_stim2[i]=mean(subk$outcome[which((subk$stim1==subk$stim2[i]&subk$choice==0&subk$feedback==1& subk$trial<subk$trial[i]) | (subk$stim2==subk$stim2[i]&subk$choice==1&subk$feedback==1& subk$trial<subk$trial[i]))])
                  }
                  subk$relatively_correct=NA
                  index=which(!is.nan(subk$relative_stim1)&!is.nan(subk$relative_stim2))
                  
                  # accuracy according to experienced probabilities (differences of less than 10% between probabilties are omitted)
                  for (i in index){
                        subk$relatively_correct[i]=((subk$relative_stim1[i]>subk$relative_stim2[i]+0.1)&&(subk$choice[i]==0)||(subk$relative_stim1[i]+0.1<subk$relative_stim2[i])&&(subk$choice[i]==1))
                        if (((abs(subk$relative_stim1[i]-subk$relative_stim2[i])<0.1)&&(abs(subk$relative_stim2[i]-subk$relative_stim1[i]))<0.1))
                              subk$relatively_correct[i]=NA
                  }
                  data <- bind_rows(data,subk)
            }
      } 
      return(data)}






