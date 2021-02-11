mean(subset(data_selag,n_seeds>0&FFD_s_y<=-0.84)$FFD_s_y) # Mean cat 1 = -1.247518
mean(subset(data_selag,n_seeds>0&FFD_s_y>-0.84&FFD_s_y<=-0.28)$FFD_s_y) # Mean cat 2 = -0.5992093
mean(subset(data_selag,n_seeds>0&FFD_s_y>-0.28&FFD_s_y<=0.26)$FFD_s_y) # Mean cat 3 = -0.008394325
mean(subset(data_selag,n_seeds>0&FFD_s_y>0.16)$FFD_s_y) # Mean cat 4 = 0.7891997

pred_seedpred_mean3<-rbind(
  (data.frame(ggpredict(mod_seedpred_bb,terms = c("mean_3 [all]","FFD_s_y [-1.247518]")))%>%
     mutate(FFD_s_y_cat=1)%>%
     rename(temp=x, prop_pred_seeds=predicted,FFD_s_y=group)),
  (data.frame(ggpredict(mod_seedpred_bb,terms = c("mean_3 [all]","FFD_s_y [-0.5992093]")))%>%
     mutate(FFD_s_y_cat=2)%>%
     rename(temp=x, prop_pred_seeds=predicted,FFD_s_y=group)),
  (data.frame(ggpredict(mod_seedpred_bb,terms = c("mean_3 [all]","FFD_s_y [-0.008394325]")))%>%
     mutate(FFD_s_y_cat=3)%>%
     rename(temp=x, prop_pred_seeds=predicted,FFD_s_y=group)),
  (data.frame(ggpredict(mod_seedpred_bb,terms = c("mean_3 [all]","FFD_s_y [0.7891997]")))%>%
     mutate(FFD_s_y_cat=4)%>%
     rename(temp=x, prop_pred_seeds=predicted,FFD_s_y=group)))%>%
  mutate(month="3")
pred_seedpred_mean4<-rbind(
  (data.frame(ggpredict(mod_seedpred_bb,terms = c("mean_4 [all]","FFD_s_y [-1.247518]")))%>%
     mutate(FFD_s_y_cat=1)%>%
     rename(temp=x, prop_pred_seeds=predicted,FFD_s_y=group)),
  (data.frame(ggpredict(mod_seedpred_bb,terms = c("mean_4 [all]","FFD_s_y [-0.5992093]")))%>%
     mutate(FFD_s_y_cat=2)%>%
     rename(temp=x, prop_pred_seeds=predicted,FFD_s_y=group)),
  (data.frame(ggpredict(mod_seedpred_bb,terms = c("mean_4 [all]","FFD_s_y [-0.008394325]")))%>%
     mutate(FFD_s_y_cat=3)%>%
     rename(temp=x, prop_pred_seeds=predicted,FFD_s_y=group)),
  (data.frame(ggpredict(mod_seedpred_bb,terms = c("mean_4 [all]","FFD_s_y [0.7891997]")))%>%
     mutate(FFD_s_y_cat=4)%>%
     rename(temp=x, prop_pred_seeds=predicted,FFD_s_y=group)))%>%
  mutate(month="4")
pred_seedpred<-rbind(pred_seedpred_mean3,pred_seedpred_mean4)

label_names1 <- list(
  '1'="First quarter\nMean FFD = -1.25",
  '2'="Second quarter\nMean FFD = -0.60",
  '3'="Third quarter\nMean FFD = -0.01",
  '4'="Fourth quarter\nMean FFD = 0.79"
)

label_names2 <- list(
  '3'="March",
  '4'="April"
)

labeller_function1 <- function(variable,value){
  return(label_names1[value])
}

labeller_function2 <- function(variable,value){
  return(label_names2[value])
}

ggplot(subset(data_selag,n_seeds>0&!is.na(FFD_s_y))%>%
         ungroup()%>%
         # Define 4 FFD_s_y categories based on quartiles
         mutate(FFD_s_y_cat=as.factor(ifelse(FFD_s_y<=-0.84,1,
                                             ifelse(FFD_s_y>-0.84&FFD_s_y<=-0.28,2,
                                                    ifelse(FFD_s_y>-0.28&FFD_s_y<=0.26,3,4)))))%>%
         select(mean_3,mean_4,prop_pred_seeds,FFD_s_y_cat)%>%
         rename(March=mean_3,April=mean_4)%>%
         pivot_longer(cols=March:April,names_to="month",values_to="temp")%>%
         mutate(month=ifelse(month=="March",3,4)),
       aes(x=temp,y=prop_pred_seeds))+
  facet_grid(month~FFD_s_y_cat,scales="free",
             labeller=labeller(FFD_s_y_cat=labeller_function1,month=labeller_function2))+
  geom_jitter(size=1.5,alpha=0.3,width=0.05)+
  geom_line(data=pred_seedpred,aes(x=temp,y=prop_pred_seeds,color=FFD_s_y_cat),size=1)+
  geom_ribbon(data=pred_seedpred,aes(x=temp,y=prop_pred_seeds,
                                     ymin=conf.low,ymax=conf.high,
                                     fill=FFD_s_y_cat),alpha=0.3)+
  my_theme()+scale_color_viridis(labels=NULL)+scale_fill_viridis(labels=NULL)+
  theme(legend.position="top")+labs(colour="First flowering date      ")+
  xlab("Mean temperature (ºC)")+
  ylab("Predicted seed predation")+
  scale_x_continuous(breaks=c(-4,-2,0,2,4,6,8))+
  theme(strip.text.x=element_text(margin=margin(2,0,2,0)))+
  guides(fill=FALSE)


