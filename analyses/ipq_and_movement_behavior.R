library(ggplot2)
library(cowplot)
library(Rmisc)
library(sjmisc)
library(lmerTest)
library(car)
library(MASS)

# load data
load("/Users/lukasgehrke/Documents/bpn_work/publications/data/behavior_IMT1.Rdata")
head(df)

# explore spatial presence
df_agg_participant <- aggregate(. ~ Participant, data = df, mean)
hist(df_agg_participant$IPQ_Spatial_Presence)
df_agg_participant$vgame <- c(4,3,3,3,4,4,3,3,3,4,4,2,2,3,3,3,2,1,3,4,2,4,4,3,2,4,1,3,4)

# step wise regression
full.model <- lm(IPQ_Presence ~ Duration + Hand_Touches + Mean_Ratings + vgame + Gender + PTSOT + SOD, data = df_agg_participant)
step.model <- stepAIC(full.model, direction = "both", trace = FALSE)
summary(step.model)

full.model <- lm(IPQ_Spatial_Presence ~ Duration + Hand_Touches + Mean_Ratings + vgame + Gender + PTSOT + SOD, data = df_agg_participant)
step.model <- stepAIC(full.model, direction = "both", trace = FALSE)
summary(step.model)

# full.model <- lm(`IPQ Involvement` ~ Duration + Hand_Touches + Mean_Ratings + vgame + Gender + PTSOT + SOD, data = df_agg_participant)
# step.model <- stepAIC(full.model, direction = "both", trace = FALSE)
# summary(step.model)
# 
# full.model <- lm(IPQ_experienced_realism ~ Duration + Hand_Touches + Mean_Ratings + vgame + Gender + PTSOT + SOD, data = df_agg_participant)
# step.model <- stepAIC(full.model, direction = "both", trace = FALSE)
# summary(step.model)

ylab_title <- 'spatial presence'
xlab_title <- 'sketchmap'
s <- ggplot(df_agg_participant, aes(x=Mean_Ratings, y=IPQ_Spatial_Presence)) + #`IPQ Involvement`
  geom_point(size=3)+
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE)+
  labs(title = "", x = xlab_title, y = ylab_title)+
  scale_color_brewer(palette="Paired")+
  theme_classic()
  # ylim(c(1,7))
  # ylim(c(min(df_agg_participant$IPQ_Spatial_Presence)-2,max(df_agg_participant$IPQ_Spatial_Presence)))
xlab_title <- 'time on task (s)'
d <- ggplot(df_agg_participant, aes(x=Duration, y=IPQ_Spatial_Presence)) + #`IPQ Involvement`
  geom_point(size=3)+
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE)+
  labs(title = "", x = xlab_title, y = ylab_title)+
  scale_color_brewer(palette="Paired")+
  theme_classic()
  # ylim(c(1,7))
  # ylim(c(min(df_agg_participant$IPQ_Spatial_Presence)-2,max(df_agg_participant$IPQ_Spatial_Presence)))
all_plot <- plot_grid(s,d,labels = "AUTO", nrow = 1, ncol = 2)
all_plot

ylab_title <- 'general presence'
xlab_title <- 'gaming experience'
t <- ggplot(df_agg_participant, aes(x=vgame, y=IPQ_Presence)) + #`IPQ Involvement`
  geom_point(size=3)+
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE)+
  labs(title = "", x = xlab_title, y = ylab_title)+
  scale_color_brewer(palette="Paired")+
  theme_classic()+
  ylim(c(1,7))
xlab_title <- 'sex'
g <- ggplot(df_agg_participant, aes(x=Gender, y=IPQ_Presence)) + #`IPQ Involvement`
  geom_boxplot(alpha=.2, outlier.shape = NA) +
  labs(title = "", x = xlab_title, y = ylab_title)+
  scale_color_brewer(palette="Paired")+
  theme_classic()+
  ylim(c(1,7))
all_plot <- plot_grid(t,g,labels = "AUTO", nrow = 1, ncol = 2)
all_plot





df_agg_participant$IPQ_Presence <- as.factor(df_agg_participant$IPQ_Presence)
ggplot(df_agg_participant, aes(x=IPQ_Presence, y=Mean_Ratings))+
  geom_boxplot(alpha=.2, outlier.shape = NA) +
  theme_classic()



ggplot(df_agg_participant, aes(x=Mean_Ratings, y=`IPQ Involvement`, color=Run, shape=Run)) + 
  geom_point(size=3)+
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE)+
  labs(title = "", x = xlab_title, y = ylab_title)+
  scale_color_brewer(palette="Paired")+
  theme_minimal()














# dichotomized
# group participants into high and low IPQ spatial presence
df_agg_participant$presence <- dicho(df_agg_participant$IPQ_Presence, append = FALSE, dich.by = "mean")
dicho_presence <- rep(df_agg_participant$presence,1,each=3)

# match integrator variable to full dataset
integrator_id <- df_agg_participant[,c(1,33)]
for (i in 1:dim(df)[1]) {
  # find participant index in integrator_id and add integrator variable
  ix <- which(integrator_id$Participant==df$Participant[i])[1]
  df$presence[i] <- integrator_id$presence[ix]
}

# # save median splitted indices to matlab file
# library(R.matlab)
# fname <- "/Users/lukasgehrke/Documents/bpn_work/publications/2019-IMT-Map-Formation/data/participants_presence_median_split.mat"
# writeMat(fname, integrator_ids = as.integer(df_agg_participant$presence))

# average over mazes as not of interest and not a significant effect
df_agg <- aggregate(. ~ Run + Participant, data = df, mean)
# add dichotomized variable presence
df_agg$presence <- dicho_maps

summarySE(df_agg, 'Mean_Ratings', c(1,33))
summarySE(df_agg, 'Duration', c(1,33))
summarySE(df_agg, 'Hand_Touches', c(1,33))


df_agg$presence <- as.factor(df_agg$presence)
ylab_title <- 'time on task (s)'
xlab_title <- 'maze trial'
duration <- ggplot(df_agg, aes(x = Run,
                                           y = Duration,
                                           fill = presence,
                                           colour = presence,
                                           shape = presence)) +
  # geom_line(aes(group=presence), size=.5, alpha=.2) +
  stat_summary(aes(group=presence), fun.y = "median", geom = "line", size = 1.2, position = position_dodge(width = .75)) +
  geom_boxplot(alpha=.2, outlier.shape = NA) +
  theme_bw() +
  # theme(legend.position = "none") +
  theme(axis.title.y = element_text(colour="grey20",size=14),
        axis.title.x = element_text(colour="grey20",size=14),
        axis.text.x = element_text(colour="grey20",size=12),
        axis.text.y = element_text(colour="grey20",size=12)) +
  ylab(ylab_title) +
  xlab(xlab_title)
duration

# touches
mdl <- lmer(Hand_Touches ~ Run*presence + (1|Participant),data = df_agg)
Anova(mdl)
ylab_title <- '# of touches'
xlab_title <- 'maze trial'
touches <- ggplot(df_agg, aes(x = Run,
                                          y = Hand_Touches,
                                          fill = presence,
                                          colour = presence,
                                          shape = presence)) +
  # geom_line(aes(group=presence), size=.5, alpha=.2) +
  stat_summary(aes(group=presence), fun.y = "median", geom = "line", size = 1.2, position = position_dodge(width = .75)) +
  geom_boxplot(alpha=.2, outlier.shape = NA) +
  theme_bw() +
  # theme(legend.position = "none") +
  theme(axis.title.y = element_text(colour="grey20",size=14),
        axis.title.x = element_text(colour="grey20",size=14),
        axis.text.x = element_text(colour="grey20",size=12),
        axis.text.y = element_text(colour="grey20",size=12)) +
  ylab(ylab_title) +
  xlab(xlab_title)
touches

# sketchmap
mdl <- lmer(Mean_Ratings ~ Run*presence + (1|Participant),data = df_agg)
Anova(mdl)
ylab_title <- 'sketchmap'
xlab_title <- 'maze trial'
sketchmap <- ggplot(df_agg, aes(x = Run,
                                            y = Mean_Ratings,
                                            fill = presence,
                                            colour = presence,
                                            shape = presence)) +
  # geom_line(aes(group=presence), size=.5, alpha=.2) +
  stat_summary(aes(group=presence), fun.y = "median", geom = "line", size = 1.2, position = position_dodge(width = .75)) +
  geom_boxplot(alpha=.2, outlier.shape = NA) +
  theme_bw() +
  # theme(legend.position = "none") +
  theme(axis.title.y = element_text(colour="grey20",size=14),
        axis.title.x = element_text(colour="grey20",size=14),
        axis.text.x = element_text(colour="grey20",size=12),
        axis.text.y = element_text(colour="grey20",size=12)) +
  ylab(ylab_title) +
  xlab(xlab_title)
sketchmap

all_plot <- plot_grid(duration, touches, sketchmap, labels = "AUTO")
all_plot
