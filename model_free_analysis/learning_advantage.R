# comparing cumulative and concurrent diversity differences during learning
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