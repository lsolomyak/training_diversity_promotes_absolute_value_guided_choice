plot_model_fits <- function(){
      #P <- read.csv2("~/Google Drive/My Drive/Common lab folder/model-fitting-master/Context.Context_Experiment/Matlab Scripts/model_fitting/Fits_7.csv",header=TRUE,sep=",")
      P <- read.csv2("~/Google Drive/My Drive/Common lab folder/model-fitting-master/Context/Context_Experiment/Matlab Scripts/model_fitting/Lik_current/FiveMain/plos_models/reviewer3/learning_to_learn/P_time_effect.csv",header=TRUE,sep=",")
      
      text=12
      title=19
      plot1 <- ggplot(P,aes(x=as.numeric(three_test_policy),y=as.numeric(six_test_policy)))+
            geom_point(color="steelblue",size=4)+
            geom_abline(intercept = 0, slope = 1,linetype=2)+
            
            scale_x_continuous(name=expression(paste(beta[Preference]("Concurrent Low"))),limits = c(0,9),breaks = seq(0, 8, by = 2)) +
            scale_y_continuous(name=expression(paste(beta[Preference]("Concurrent High"))),limits = c(0,9),breaks = seq(0, 8, by = 2)) +
            ggtitle(" ")+
            
            theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                  panel.background = element_blank(), 
                  axis.line = element_line(colour = "black"), 
                  plot.title = element_text(hjust = 0.5),legend.position = "none",
                  axis.text.x = element_text(size=text),  axis.title.x = element_text(size=title),
                  axis.text.y = element_text( size=text),axis.title.y = element_text(size=title), 
                  text = element_text(size=title)
            )
      
      plot2 <- ggplot(P,aes(x=as.numeric(three_test_value),y=as.numeric(six_test_value)))+
            geom_point(color="steelblue",size=4)+
            geom_abline(intercept = 0, slope = 1,linetype=2)+
            scale_x_continuous(name=expression(paste(beta[Value]("Concurrent Low"))),limits = c(0,9),breaks = seq(0, 8, by = 2)) +
            scale_y_continuous(name=expression(paste(beta[Value]("Concurrent High"))),limits = c(0,9),breaks = seq(0, 8, by = 2)) +
            ggtitle(" ")+
            
            theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                  panel.background = element_blank(), 
                  axis.line = element_line(colour = "black"), 
                  plot.title = element_text(hjust = 0.5),legend.position = "none",
                  axis.text.x = element_text(size=text),  axis.title.x = element_text(size=title),
                  axis.text.y = element_text( size=text),axis.title.y = element_text(size=title),
                  text = element_text(size=title)
            )
      plot3 <- ggplot(P,aes(x=as.numeric(two_test_policy),y=as.numeric(four_test_policy)))+
            geom_point(color="chocolate4",size=4)+
            geom_abline(intercept = 0, slope = 1,linetype=2)+
            
            scale_x_continuous(name=expression(paste(beta[Preference]("Cumulative Low"))),limits = c(0,9),breaks = seq(0, 8, by = 2)) +
            scale_y_continuous(name=expression(paste(beta[Preference]("Cumulative High"))),limits = c(0,9),breaks = seq(0, 8, by = 2)) +
            
            
            theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                  panel.background = element_blank(), 
                  axis.line = element_line(colour = "black"), 
                  plot.title = element_text(hjust = 0.5),legend.position = "none",
                  axis.text.x = element_text(size=text),  axis.title.x = element_text(size=title),
                  axis.text.y = element_text( size=text),axis.title.y = element_text(size=title),
                  text = element_text(size=title)
                  
            )
      
      plot4 <- ggplot(P,aes(x=as.numeric(two_test_value),y=as.numeric(four_test_value)))+
            geom_point(color="chocolate4",size=4)+
            geom_abline(intercept = 0, slope = 1,linetype=2)+
            scale_x_continuous(name=expression(paste(beta[Value]("Cumulative Low"))),limits = c(0,9),breaks = seq(0, 8, by = 2)) +
            scale_y_continuous(name=expression(paste(beta[Value]("Cumulative High"))),limits = c(0,9),breaks = seq(0, 8, by = 2)) +
            
            
            theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                  panel.background = element_blank(), 
                  axis.line = element_line(colour = "black"), 
                  plot.title = element_text(hjust = 0.5),legend.position = "none",
                  axis.text.x = element_text(size=text),  axis.title.x = element_text(size=title),
                  axis.text.y = element_text( size=text),axis.title.y = element_text(size=title),
                  text = element_text(size=title)
            )
      plot1 <-annotate_figure(plot1,
                              fig.lab = "B", fig.lab.face = "bold",
                              fig.lab.size =24
                              
      )
      plot2 <-annotate_figure(plot2,
                              fig.lab = "A", fig.lab.face = "bold",
                              fig.lab.size =24
                              
      )
      plot3 <-annotate_figure(plot3,
                              fig.lab = "D", fig.lab.face = "bold",
                              fig.lab.size =24
                              
      )
      plot4 <-annotate_figure(plot4,
                              fig.lab = "C", fig.lab.face = "bold",
                              fig.lab.size =24
                              
      )
      
      
      
      figure1 <- ggarrange(plot2,plot1,nrow=1)
      
      
      figure2 <- ggarrange(plot4,plot3,nrow=1)
      
      figure <- ggarrange(figure1,figure2,nrow=2)
      # figure <- annotate_figure(figure,
      #                           fig.lab = "B", fig.lab.face = "bold",
      #                                            fig.lab.size =24)
      
      
      png(paste0('~/Google Drive/My Drive/Common lab folder/model-fitting-master/Context/Context_Experiment/R Scripts/All_Scripts_Relevant/Model_Validation/model',Sys.Date(),'.png'), width = 8, height = 9, units = "in",res=800)
      print(figure)
      dev.off()
      return(figure)}
