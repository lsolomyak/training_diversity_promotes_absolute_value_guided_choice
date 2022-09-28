plotting_log_odds <- function(real){
      require(modelr)
      require(tidybayes)
      if(real==1){
            load("/Volumes/GoogleDrive/My Drive/Common lab folder/model-fitting-master/Context/Context_Experiment/R Scripts/Data_Objects/Counter_Updated/updated3.RData")
      }else{
            load("/Volumes/GoogleDrive/My Drive/Common lab folder/model-fitting-master/Context_Experiment/R Scripts/Data_Objects/ahsim_redec23.RData")
            counter_agaaain <- sim_reg
      }
      counter %>% 
            spread_draws(b_Intercept,b_counter_direct_diff,b_counter_diff,b_threen,b_Counterx3_direct,b_Counterx3,b_Realx3,b_Rewarddiff) %>% 
            mutate(real_3=b_Intercept + b_Rewarddiff - b_Realx3-b_threen,
                   real_6=b_Intercept + b_Rewarddiff + b_Realx3 + b_threen,
                   direct_3=b_Intercept + b_counter_direct_diff-b_Counterx3_direct -b_threen,
                   direct_6=b_Intercept + b_counter_direct_diff+b_Counterx3_direct +b_threen,
                   indirect_3=b_Intercept + b_counter_diff-b_Counterx3 -b_threen,
                   indirect_6=b_Intercept + b_counter_diff+b_Counterx3 +b_threen,
                   
            )-> posterior_new
      h2 %>% 
            spread_draws(b_Intercept,b_counter_direct_diff,b_counter_diff,b_twon,b_Counterx2_direct,b_Counterx2,b_Realx2,b_Rewarddiff) %>% 
            mutate(
                  real_2=b_Intercept + b_Rewarddiff - b_Realx2 - b_twon,
                  real_4=b_Intercept + b_Rewarddiff + b_Realx2+ b_twon,
                  direct_4=b_Intercept + b_counter_direct_diff-b_Counterx2_direct -b_twon,
                  direct_2=b_Intercept + b_counter_direct_diff+b_Counterx2_direct +b_twon,
                  indirect_4=b_Intercept + b_counter_diff-b_Counterx2 -b_twon,
                  indirect_2=b_Intercept + b_counter_diff+b_Counterx2 +b_twon,
                  
            )-> posterior_new_2
      
      posterior_new %>% select(-c(1:11)) %>% tidyr::gather("id", "value") -> draw
      posterior_new_2 %>% select(-c(1:11)) %>% tidyr::gather("id", "value") -> draw_t
      draw_both <- rbind(draw,draw_t)
      draw_both %>%   filter(str_detect(id, "real"))-> draw_actual
      draw_both %>%   filter(str_detect(id, "^direct"))-> draw_direct
      draw_both %>%   filter(str_detect(id, "indirect"))-> draw_indirect
      draw_actual <- draw_actual %>% mutate(id=recode(id,
                                                      "real_2"="Cumulative\nLow",
                                                      
                                                      "real_4"="Cumulative\nHigh",
                                                      
                                                      "real_3"= "Concurrent\nLow", "real_6" = "Concurrent\nHigh"
                                                      
      ))
      
      draw_direct <- draw_direct %>% mutate(id=recode(id,
                                                      "direct_2"="Cumulative Low",
                                                      "direct_4"="Cumulative High",
                                                      "direct_3"= "Concurrent Low",
                                                      "direct_6" = "Concurrent High"))
      
      draw_indirect <- draw_indirect %>% mutate(id=recode(id,
                                                          "indirect_2"="Cumulative Low",
                                                          "indirect_4"="Cumulative High",
                                                          "indirect_3"= "Concurrent Low",
                                                          "indirect_6" = "Concurrent High"))
      
      library("wesanderson")
      
      plot1 <- ggplot(draw_actual,aes(x=factor(id),y=value,color=factor(id)))+
            
            stat_pointinterval(size=10) +
            scale_color_manual(values=c("orchid4","steelblue", "#C27D38","#798E87"))+
            scale_y_continuous(limits=c(-1.5,3), breaks = seq(-1, 3, by = 1)) +
            coord_flip()+
            geom_hline(aes(yintercept = 0), linetype="dashed",col = 'black')+
            ggtitle("Own")+
            theme( panel.grid.minor = element_blank(),
                   panel.background = element_blank(), axis.line = element_line(colour = "black"), 
                   axis.text.x = element_text(size=16),
                   axis.title.y = element_blank(),
                   
                   axis.text.y = element_text(size=16),
                   legend.position = "none",
                   plot.title = element_text(size=20,face="bold",hjust = 0.5),
                   
                   axis.title.x = element_blank(),
                   axis.ticks.y  = element_blank() 
            )
      
      plot2 <- ggplot(draw_direct,aes(x=factor(id),y=value,color=factor(id)))+
            
            stat_pointinterval(size=10) +
            scale_color_manual(values=c("orchid4","steelblue","#C27D38","#798E87"))+
            #scale_y_continuous(limits = c(.6,1), breaks = seq(.6, 1, by = 0.1), labels = scales::percent) +
            coord_flip()+
            scale_y_continuous(limits=c(-1.5,3), breaks = seq(-1, 3, by = 1)) +
            geom_hline(aes(yintercept = 0), linetype="dashed",col = 'black')+
            
            ggtitle("Current alternative")+
            theme( panel.grid.minor = element_blank(),
                   panel.background = element_blank(), axis.line = element_line(colour = "black"), 
                   axis.text.x = element_text(size=16),
                   axis.title.y = element_blank(),
                   axis.text.y = element_blank(),
                   plot.title = element_text(size=20,face="bold",hjust = 0.5),
                   
                   legend.position = "none",  
                   axis.title.x = element_blank(),
                   axis.ticks.y  = element_blank() 
            )
      
      plot3 <- figure <-ggplot(draw_indirect,aes(x=factor(id),y=value,color=factor(id)))+
            
            stat_pointinterval(size=10,.width = c(0.66, 0.95)) +
            scale_color_manual(values=c("orchid4","steelblue","#C27D38","#798E87"))+
            #scale_y_continuous(limits = c(.6,1), breaks = seq(.6, 1, by = 0.1), labels = scales::percent) +
            coord_flip()+
            geom_hline(aes(yintercept = 0), linetype="dashed",col = 'black')+
            scale_y_continuous(limits=c(-1.5,3), breaks = seq(-1, 3, by = 1)) +
            #scale_y_continuous(limits=c(-1.1,0), breaks = seq(-1, 0, by = .5)) +
            
            ggtitle("Other")+
            theme( panel.grid.minor = element_blank(),
                   panel.background = element_blank(), axis.line = element_line(colour = "black"), 
                   axis.text.x = element_text(size=16),
                   axis.text.y = element_blank(),
                   plot.title = element_text(size=20,face="bold",hjust = 0.5),
                   axis.title.y = element_blank(),
                   legend.position = "none", 
                   axis.title.x = element_blank(),
                   axis.ticks.y  = element_blank() 
            )
      
      
      figure <- ggarrange(plot1,plot2,plot3,nrow=1,widths=c(1.43,1,1))
      figure <- annotate_figure(figure,
                                fig.lab = "B",fig.lab.size=24,
                                bottom=text_grob("Log-odds change in choice probability",size=20,face="bold")
      )
      
      png(paste0('~/Google Drive/My Drive/Common lab folder/model-fitting-master/Context_Experiment/R Scripts/All_Scripts_Relevant/Model_Validation/log_odds',Sys.Date(),'.png'), width = 13, height =7, units = "in",res=300)
      print(figure)
      dev.off()
      
      return(figure)}