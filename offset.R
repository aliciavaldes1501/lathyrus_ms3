
model1<-glmmTMB(n_intact_seeds_rel_y ~ FFD_s_y+n_fl_s_y+
                                   FFD_s_y:n_seeds_per_fl_res_mean+FFD_s_y:grazing_mean+FFD_s_y:prop_pred_seeds_mean+
                                   (1|id),data_selag,family="gaussian")
model2<-glmmTMB(n_intact_seeds_rel_y ~ FFD_s_y+n_fl_s_y+
                  FFD_s_y:n_seeds_per_fl_res_mean+FFD_s_y:grazing_mean+FFD_s_y:prop_pred_seeds_mean+
                  (1|id),subset(data_selag,year<2017),family="gaussian")

ggpredict(model1,terms = c("FFD_s_y [all]","grazing_mean [quart]"))%>%
  ggplot(aes(x,predicted,ymin=conf.low,ymax=conf.high,colour=group,fill=group))+
  geom_line(aes(color=group),size=1)+my_theme()+
  theme(legend.position="right")+labs(colour="Grazing")+
  scale_color_manual(values=c("#d7191c","#fdae61","#ffffbf","#abdda4","#2b83ba"))+
  xlab("FFD (standardized within years)")+ylab("Predicted number of intact seeds")

ggpredict(model1,terms = c("FFD_s_y [all]","n_seeds_per_fl_res_mean [quart]"))%>%
  ggplot(aes(x,predicted,ymin=conf.low,ymax=conf.high,colour=group,fill=group))+
  geom_line(aes(color=group),size=1)+my_theme()+
  theme(legend.position="right")+labs(colour="n_seeds_per_fl_res_mean")+
  scale_color_manual(values=c("#d7191c","#fdae61","#ffffbf","#abdda4","#2b83ba"))+
  xlab("FFD (standardized within years)")+ylab("Predicted number of intact seeds")

ggpredict(model1,terms = c("FFD_s_y [all]","prop_pred_seeds_mean [quart]"))%>%
  ggplot(aes(x,predicted,ymin=conf.low,ymax=conf.high,colour=group,fill=group))+
  geom_line(aes(color=group),size=1)+my_theme()+
  theme(legend.position="right")+labs(colour="prop_pred_seeds_mean")+
  scale_color_manual(values=c("#d7191c","#fdae61","#ffffbf","#abdda4","#2b83ba"))+
  xlab("FFD (standardized within years)")+ylab("Predicted number of intact seeds")

ggpredict(model2,terms = c("FFD_s_y [all]","grazing_mean [quart]"))%>%
  ggplot(aes(x,predicted,ymin=conf.low,ymax=conf.high,colour=group,fill=group))+
  geom_line(aes(color=group),size=1)+my_theme()+
  theme(legend.position="right")+labs(colour="Grazing")+
  scale_color_manual(values=c("#d7191c","#fdae61","#ffffbf","#abdda4","#2b83ba"))+
  xlab("FFD (standardized within years)")+ylab("Predicted number of intact seeds")

ggpredict(model2,terms = c("FFD_s_y [all]","n_seeds_per_fl_res_mean [quart]"))%>%
  ggplot(aes(x,predicted,ymin=conf.low,ymax=conf.high,colour=group,fill=group))+
  geom_line(aes(color=group),size=1)+my_theme()+
  theme(legend.position="right")+labs(colour="n_seeds_per_fl_res_mean")+
  scale_color_manual(values=c("#d7191c","#fdae61","#ffffbf","#abdda4","#2b83ba"))+
  xlab("FFD (standardized within years)")+ylab("Predicted number of intact seeds")

ggpredict(model2,terms = c("FFD_s_y [all]","prop_pred_seeds_mean [quart]"))%>%
  ggplot(aes(x,predicted,ymin=conf.low,ymax=conf.high,colour=group,fill=group))+
  geom_line(aes(color=group),size=1)+my_theme()+
  theme(legend.position="right")+labs(colour="prop_pred_seeds_mean")+
  scale_color_manual(values=c("#d7191c","#fdae61","#ffffbf","#abdda4","#2b83ba"))+
  xlab("FFD (standardized within years)")+ylab("Predicted number of intact seeds")

model3<-glmmTMB(round(n_intact_seeds) ~ FFD_s_y+n_fl_s_y+
                  FFD_s_y:n_seeds_per_fl_res_mean_s+
                  FFD_s_y:grazing_mean_s+
                  FFD_s_y:prop_pred_seeds_mean_s+
                  (1|id),data_selag,family="nbinom2",offset=n_intact_seeds_mean)

model4<-glmmTMB(round(n_intact_seeds) ~ FFD_s_y+n_fl_s_y+
                  FFD_s_y:n_seeds_per_fl_res_mean_s+
                  FFD_s_y:grazing_mean_s+
                  FFD_s_y:prop_pred_seeds_mean_s+
                  (1|id),subset(data_selag,year<2017),family="nbinom2",offset=n_intact_seeds_mean)

model3<-glmmTMB(round(n_intact_seeds) ~ FFD_s_y+n_fl_s_y+
                  FFD_s_y:n_seeds_per_fl_res_mean+FFD_s_y:grazing_mean+FFD_s_y:prop_pred_seeds_mean+
                  offset(n_intact_seeds_mean)+(1|id),data_selag,family="nbinom2")
model4<-glmmTMB(round(n_intact_seeds) ~ FFD_s_y+n_fl_s_y+
                  FFD_s_y:n_seeds_per_fl_res_mean+FFD_s_y:grazing_mean+FFD_s_y:prop_pred_seeds_mean+
                  offset(n_intact_seeds_mean)+(1|id),subset(data_selag,year<2017),family="nbinom2")

ggpredict(model3,terms = c("FFD_s_y [all]","grazing_mean [0:1 by=.01]"))%>%
  ggplot(aes(x,predicted,colour=group,fill=group))+
  geom_line(aes(color=group),size=1)+my_theme()+
  theme(legend.position="right")+labs(colour="Grazing")+
  scale_color_manual(values=c("#d7191c","#fdae61","#ffffbf","#abdda4","#2b83ba"))+
  xlab("FFD (standardized within years)")+ylab("Predicted number of intact seeds")

ggpredict(model3,terms = c("FFD_s_y [all]","n_seeds_per_fl_res_mean [quart]"))%>%
  ggplot(aes(x,predicted,colour=group,fill=group))+
  geom_line(aes(color=group),size=1)+my_theme()+
  theme(legend.position="right")+labs(colour="seedsperfl")+
  scale_color_manual(values=c("#d7191c","#fdae61","#ffffbf","#abdda4","#2b83ba"))+
  xlab("FFD (standardized within years)")+ylab("Predicted number of intact seeds")

ggpredict(model3,terms = c("FFD_s_y [all]","prop_pred_seeds_mean [quart]"))%>%
  ggplot(aes(x,predicted,colour=group,fill=group))+
  geom_line(aes(color=group),size=1)+my_theme()+
  theme(legend.position="right")+labs(colour="predation")+
  scale_color_manual(values=c("#d7191c","#fdae61","#ffffbf","#abdda4","#2b83ba"))+
  xlab("FFD (standardized within years)")+ylab("Predicted number of intact seeds")

ggpredict(model4,terms = c("FFD_s_y [all]","grazing_mean [quart]"))%>%
  ggplot(aes(x,predicted,colour=group,fill=group))+
  geom_line(aes(color=group),size=1)+my_theme()+
  theme(legend.position="right")+labs(colour="Grazing")+
  scale_color_manual(values=c("#d7191c","#fdae61","#ffffbf","#abdda4","#2b83ba"))+
  xlab("FFD (standardized within years)")+ylab("Predicted number of intact seeds")

ggpredict(model4,terms = c("FFD_s_y [all]","n_seeds_per_fl_res_mean [quart]"))%>%
  ggplot(aes(x,predicted,colour=group,fill=group))+
  geom_line(aes(color=group),size=1)+my_theme()+
  theme(legend.position="right")+labs(colour="seedsperfl")+
  scale_color_manual(values=c("#d7191c","#fdae61","#ffffbf","#abdda4","#2b83ba"))+
  xlab("FFD (standardized within years)")+ylab("Predicted number of intact seeds")

ggpredict(model4,terms = c("FFD_s_y [all]","prop_pred_seeds_mean [quart]"))%>%
  ggplot(aes(x,predicted,colour=group,fill=group))+
  geom_line(aes(color=group),size=1)+my_theme()+
  theme(legend.position="right")+labs(colour="predation")+
  scale_color_manual(values=c("#d7191c","#fdae61","#ffffbf","#abdda4","#2b83ba"))+
  xlab("FFD (standardized within years)")+ylab("Predicted number of intact seeds")

simulationOutput_model3 <- 
  simulateResiduals(fittedModel = model3, n = 5000)
plot(simulationOutput_model3)


