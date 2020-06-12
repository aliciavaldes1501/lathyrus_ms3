plot(ggpredict(mod_int_sel_GLMM_nsints,terms=c("FFD_s_y","grazing_mean")))
plot(ggpredict(mod_int_sel_GLMM_nsints,terms=c("FFD_s_y","grazing_mean [quart]")))


ggpredict(mod_int_sel_GLMM_nsints,terms = c("FFD_s_y [all]","grazing_mean [all]"))%>%
  ggplot(aes(x,predicted,ymin=conf.low,ymax=conf.high,colour=group,fill=group))+
  geom_line(aes(color=as.numeric(as.character(group))))+
  scale_colour_gradientn(colours = myPalette(100))+my_theme()+
  theme(legend.position="right")+labs(colour="Grazing")+
  xlab("FFD (standardized within years)")+ylab("Predicted number of intact seeds")

ggpredict(mod_int_sel_GLMM_nsints,terms = c("FFD_s_y [all]","grazing_mean [all]"))%>%
  ggplot(aes(x,predicted,ymin=conf.low,ymax=conf.high,colour=group,fill=group))+
  geom_line(aes(color=group))+my_theme()+
  theme(legend.position="right")+labs(colour="Grazing")+
  xlab("FFD (standardized within years)")+ylab("Predicted number of intact seeds")

ggpredict(mod_int_sel_GLMM_nsints,terms = c("FFD_s_y [all]","grazing_mean [meansd]"))%>%
  ggplot(aes(x,predicted,ymin=conf.low,ymax=conf.high,colour=group,fill=group))+
  geom_line(aes(color=group))+my_theme()+
  theme(legend.position="right")+labs(colour="Grazing")+
  xlab("FFD (standardized within years)")+ylab("Predicted number of intact seeds")

# Values 0-1
ggpredict(mod_int_sel_GLMM_nsints,
          terms = c("FFD_s_y [all]","grazing_mean [0:1,by=.05]"))%>%
  ggplot(aes(x,predicted,ymin=conf.low,ymax=conf.high,colour=group,fill=group))+
  geom_line(aes(color=group))+my_theme()+geom_hline(yintercept=1, linetype="dashed")+
  theme(legend.position="right")+labs(colour="Grazing")+
  xlab("FFD (standardized within years)")+ylab("Predicted number of intact seeds")

# Min, max and quartiles
ggpredict(mod_int_sel_GLMM_nsints,terms = c("FFD_s_y [all]","grazing_mean [quart]"))%>%
  ggplot(aes(x,predicted,ymin=conf.low,ymax=conf.high,colour=group,fill=group))+
  geom_line(aes(color=group),size=1)+my_theme()+
  theme(legend.position="right")+labs(colour="Grazing")+
  xlab("FFD (standardized within years)")+ylab("Predicted number of intact seeds")

library(emmeans)
emtrends(mod_int_sel_GLMM_nsints, ~grazing_mean, var="FFD_s_y",at=list(grazing=c(0,0.2,0.5)))

         