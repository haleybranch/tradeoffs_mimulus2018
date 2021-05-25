y3 <- read.csv("Data/y3.csv")

library(ggplot2)

wc.gs.graph <- ggplot(y3, aes(Stomatal_Conductance,Water_Content,colour = Year))+ facet_grid(.~Site) + geom_point()+ geom_smooth(method=lm, se=FALSE)+ theme_classic()
wc.gs.graph + theme(legend.text = element_text(size = 12, face = "bold"),
                       axis.text.x = element_text(size=14, face="bold", angle=45,hjust=1),
                       axis.text.y = element_text(size=14,face="bold"),
                       axis.title.x = element_text(color="black", size=16, vjust = 0.5, face="bold"),
                       axis.title.y = element_text(color="black", size=16,vjust = 2, face="bold")) +
  scale_x_continuous(name="Stomatal Conductance") + scale_y_continuous(name="Water Content")

wc.gs.graph <- ggplot(y3, aes(Stomatal_Conductance,Water_Content,colour = Year))+ facet_grid(.~Drought) + geom_point()+ geom_smooth(method=lm, se=FALSE)+ theme_classic()
wc.gs.graph + theme(legend.text = element_text(size = 12, face = "bold"),
                    axis.text.x = element_text(size=14, face="bold", angle=45,hjust=1),
                    axis.text.y = element_text(size=14,face="bold"),
                    axis.title.x = element_text(color="black", size=16, vjust = 0.5, face="bold"),
                    axis.title.y = element_text(color="black", size=16,vjust = 2, face="bold")) +
  scale_x_continuous(name="Stomatal Conductance") + scale_y_continuous(name="Water Content")

ft.a.graph <- ggplot(y3, aes(Flowering_Date,Assimilation))+ facet_grid(.~Drought) + geom_point()+ geom_smooth(method=lm, se=FALSE)+ theme_classic()
ft.a.graph + theme(legend.text = element_text(size = 12, face = "bold"),
                    axis.text.x = element_text(size=14, face="bold", angle=45,hjust=1),
                    axis.text.y = element_text(size=14,face="bold"),
                    axis.title.x = element_text(color="black", size=16, vjust = 0.5, face="bold"),
                    axis.title.y = element_text(color="black", size=16,vjust = 2, face="bold")) +
  scale_x_continuous(name="Flowering Time") + scale_y_continuous(name="Assimilation")


gs.a.graph <- ggplot(y3, aes(Stomatal_Conductance,Assimilation))+ facet_grid(.~Drought) + geom_point()+ geom_smooth(method=lm, se=FALSE)+ theme_classic()
gs.a.graph + theme(legend.text = element_text(size = 12, face = "bold"),
                   axis.text.x = element_text(size=14, face="bold", angle=45,hjust=1),
                   axis.text.y = element_text(size=14,face="bold"),
                   axis.title.x = element_text(color="black", size=16, vjust = 0.5, face="bold"),
                   axis.title.y = element_text(color="black", size=16,vjust = 2, face="bold")) +
  scale_x_continuous(name="Stomatal Conductance") + scale_y_continuous(name="Assimilation")




#######

#TRIALS to look at tradeoff differences 
S07 <- y3[  y3$Site=="S07" , ]

S07_plot<-ggplot(S07, aes(Water_Content, y=Assimilation, colour=Drought))+
  geom_jitter(aes(colour=Drought), size=0.2)+
  geom_smooth(method="lm",aes(colour=Drought,fill=Drought))+
  facet_wrap(.~Year)+
  scale_x_continuous(name= "Water Content") +
  scale_y_continuous(name="Carbon ASsimilation")+
  scale_color_manual(values= c("D"="#FF7700", "W"="#006600"))+
  scale_fill_manual(values= c("D"="#FF7700", "W"="#006600")) +
  theme_classic()
S07_plot <- S07_plot + theme(legend.position = "none",
                                                   axis.text.x = element_text(size=12, face="bold", angle=45,hjust=1),
                                                   axis.text.y = element_text(size=12,face="bold"),
                                                   axis.title.x = element_text(color="black", size=16, vjust = 0.5, face="bold"),
                                                   axis.title.y = element_text(color="black", size=16,vjust = 2, face="bold",hjust=0.5))
S07_plot + facet_wrap(.~Year) +
  theme(strip.background = element_blank(), strip.text.x=element_text(size=12,face="bold",hjust=0.05,vjust=-1.7))

#S02
S02 <- y3[  y3$Site=="S02" , ]

S02_plot<-ggplot(S02, aes(Water_Content, y=Assimilation, colour=Drought))+
  geom_jitter(aes(colour=Drought), size=0.2)+
  geom_smooth(method="lm",aes(colour=Drought,fill=Drought))+
  facet_wrap(.~Year)+
  scale_x_continuous(name= "Water Content") +
  scale_y_continuous(name="Carbon ASsimilation")+
  scale_color_manual(values= c("D"="#FF7700", "W"="#006600"))+
  scale_fill_manual(values= c("D"="#FF7700", "W"="#006600")) +
  theme_classic()
S02_plot <- S02_plot + theme(legend.position = "none",
                             axis.text.x = element_text(size=12, face="bold", angle=45,hjust=1),
                             axis.text.y = element_text(size=12,face="bold"),
                             axis.title.x = element_text(color="black", size=16, vjust = 0.5, face="bold"),
                             axis.title.y = element_text(color="black", size=16,vjust = 2, face="bold",hjust=0.5))
S02_plot + facet_wrap(.~Year) +
  theme(strip.background = element_blank(), strip.text.x=element_text(size=12,face="bold",hjust=0.05,vjust=-1.7))


#S11
S11 <- y3[  y3$Site=="S11" , ]

S11_plot<-ggplot(S11, aes(Water_Content, y=Assimilation, colour=Drought))+
  geom_jitter(aes(colour=Drought), size=0.2)+
  geom_smooth(method="lm",aes(colour=Drought,fill=Drought))+
  facet_wrap(.~Year)+
  scale_x_continuous(name= "Water Content") +
  scale_y_continuous(name="Carbon ASsimilation")+
  scale_color_manual(values= c("D"="#FF7700", "W"="#006600"))+
  scale_fill_manual(values= c("D"="#FF7700", "W"="#006600")) +
  theme_classic()
S11_plot <- S11_plot + theme(legend.position = "none",
                             axis.text.x = element_text(size=12, face="bold", angle=45,hjust=1),
                             axis.text.y = element_text(size=12,face="bold"),
                             axis.title.x = element_text(color="black", size=16, vjust = 0.5, face="bold"),
                             axis.title.y = element_text(color="black", size=16,vjust = 2, face="bold",hjust=0.5))
S11_plot + facet_wrap(.~Year) +
  theme(strip.background = element_blank(), strip.text.x=element_text(size=12,face="bold",hjust=0.05,vjust=-1.7))

#S10
S10 <- y3[  y3$Site=="S10" , ]

S10_plot<-ggplot(S10, aes(Water_Content, y=Assimilation, colour=Drought))+
  geom_jitter(aes(colour=Drought), size=0.2)+
  geom_smooth(method="lm",aes(colour=Drought,fill=Drought))+
  facet_wrap(.~Year)+
  scale_x_continuous(name= "Water Content") +
  scale_y_continuous(name="Carbon ASsimilation")+
  scale_color_manual(values= c("D"="#FF7700", "W"="#006600"))+
  scale_fill_manual(values= c("D"="#FF7700", "W"="#006600")) +
  theme_classic()
S10_plot <- S10_plot + theme(legend.position = "none",
                             axis.text.x = element_text(size=12, face="bold", angle=45,hjust=1),
                             axis.text.y = element_text(size=12,face="bold"),
                             axis.title.x = element_text(color="black", size=16, vjust = 0.5, face="bold"),
                             axis.title.y = element_text(color="black", size=16,vjust = 2, face="bold",hjust=0.5))
S10_plot + facet_wrap(.~Year) +
  theme(strip.background = element_blank(), strip.text.x=element_text(size=12,face="bold",hjust=0.05,vjust=-1.7))


#S08
S08 <- y3[  y3$Site=="S08" , ]

S08_plot<-ggplot(S08, aes(Water_Content, y=Assimilation, colour=Drought))+
  geom_jitter(aes(colour=Drought), size=0.2)+
  geom_smooth(method="lm",aes(colour=Drought,fill=Drought))+
  facet_wrap(.~Year)+
  scale_x_continuous(name= "Water Content") +
  scale_y_continuous(name="Carbon ASsimilation")+
  scale_color_manual(values= c("D"="#FF7700", "W"="#006600"))+
  scale_fill_manual(values= c("D"="#FF7700", "W"="#006600")) +
  theme_classic()
S08_plot <- S08_plot + theme(legend.position = "none",
                             axis.text.x = element_text(size=12, face="bold", angle=45,hjust=1),
                             axis.text.y = element_text(size=12,face="bold"),
                             axis.title.x = element_text(color="black", size=16, vjust = 0.5, face="bold"),
                             axis.title.y = element_text(color="black", size=16,vjust = 2, face="bold",hjust=0.5))
S08_plot + facet_wrap(.~Year) +
  theme(strip.background = element_blank(), strip.text.x=element_text(size=12,face="bold",hjust=0.05,vjust=-1.7))


#S32
S32 <- y3[  y3$Site=="S32" , ]

S32_plot<-ggplot(S32, aes(Water_Content, y=Assimilation, colour=Drought))+
  geom_jitter(aes(colour=Drought), size=0.2)+
  geom_smooth(method="lm",aes(colour=Drought,fill=Drought))+
  facet_wrap(.~Year)+
  scale_x_continuous(name= "Water Content") +
  scale_y_continuous(name="Carbon ASsimilation")+
  scale_color_manual(values= c("D"="#FF7700", "W"="#006600"))+
  scale_fill_manual(values= c("D"="#FF7700", "W"="#006600")) +
  theme_classic()
S32_plot <- S32_plot + theme(legend.position = "none",
                             axis.text.x = element_text(size=12, face="bold", angle=45,hjust=1),
                             axis.text.y = element_text(size=12,face="bold"),
                             axis.title.x = element_text(color="black", size=16, vjust = 0.5, face="bold"),
                             axis.title.y = element_text(color="black", size=16,vjust = 2, face="bold",hjust=0.5))
S32_plot + facet_wrap(.~Year) +
  theme(strip.background = element_blank(), strip.text.x=element_text(size=12,face="bold",hjust=0.05,vjust=-1.7))


#S29
S29 <- y3[  y3$Site=="S29" , ]

S29_plot<-ggplot(S29, aes(Water_Content, y=Assimilation, colour=Drought))+
  geom_jitter(aes(colour=Drought), size=0.2)+
  geom_smooth(method="lm",aes(colour=Drought,fill=Drought))+
  facet_wrap(.~Year)+
  scale_x_continuous(name= "Water Content") +
  scale_y_continuous(name="Carbon ASsimilation")+
  scale_color_manual(values= c("D"="#FF7700", "W"="#006600"))+
  scale_fill_manual(values= c("D"="#FF7700", "W"="#006600")) +
  theme_classic()
S29_plot <- S29_plot + theme(legend.position = "none",
                             axis.text.x = element_text(size=12, face="bold", angle=45,hjust=1),
                             axis.text.y = element_text(size=12,face="bold"),
                             axis.title.x = element_text(color="black", size=16, vjust = 0.5, face="bold"),
                             axis.title.y = element_text(color="black", size=16,vjust = 2, face="bold",hjust=0.5))
S29_plot + facet_wrap(.~Year) +
  theme(strip.background = element_blank(), strip.text.x=element_text(size=12,face="bold",hjust=0.05,vjust=-1.7))

#S18
S18 <- y3[  y3$Site=="S18" , ]

S18_plot<-ggplot(S18, aes(Water_Content, y=Assimilation, colour=Drought))+
  geom_jitter(aes(colour=Drought), size=0.2)+
  geom_smooth(method="lm",aes(colour=Drought,fill=Drought))+
  facet_wrap(.~Year)+
  scale_x_continuous(name= "Water Content") +
  scale_y_continuous(name="Carbon ASsimilation")+
  scale_color_manual(values= c("D"="#FF7700", "W"="#006600"))+
  scale_fill_manual(values= c("D"="#FF7700", "W"="#006600")) +
  theme_classic()
S18_plot <- S18_plot + theme(legend.position = "none",
                             axis.text.x = element_text(size=12, face="bold", angle=45,hjust=1),
                             axis.text.y = element_text(size=12,face="bold"),
                             axis.title.x = element_text(color="black", size=16, vjust = 0.5, face="bold"),
                             axis.title.y = element_text(color="black", size=16,vjust = 2, face="bold",hjust=0.5))
S18_plot + facet_wrap(.~Year) +
  theme(strip.background = element_blank(), strip.text.x=element_text(size=12,face="bold",hjust=0.05,vjust=-1.7))


#############



plot(trial$Water_Content , trial$Assimilation, cond=list(Drought="W"))

trialgraph <- ggplot(trial, aes(Assimilation,Water_Content, colour=Year)) + geom_point()+ geom_smooth(method=lm)+ theme_classic()
trialgraph + theme(legend.text = element_text(size = 12, face = "bold"),
                    axis.text.x = element_text(size=14, face="bold", angle=45,hjust=1),
                    axis.text.y = element_text(size=14,face="bold"),
                    axis.title.x = element_text(color="black", size=16, vjust = 0.5, face="bold"),
                    axis.title.y = element_text(color="black", size=16,vjust = 2, face="bold")) +
  scale_x_continuous(name="Assimilation") + scale_y_continuous(name="Water Content")

year.MAT.weath <- ggplot(y3, aes(Year,MAT.weath))+ geom_point()+ geom_smooth(method=lm)+ theme_classic()
year.MAT.weath + theme(legend.text = element_text(size = 12, face = "bold"),
                       axis.text.x = element_text(size=14, face="bold", angle=45,hjust=1),
                       axis.text.y = element_text(size=14,face="bold"),
                       axis.title.x = element_text(color="black", size=16, vjust = 0.5, face="bold"),
                       axis.title.y = element_text(color="black", size=16,vjust = 2, face="bold")) +
  scale_x_continuous(name="Year") + scale_y_continuous(name="MAT Weather")
