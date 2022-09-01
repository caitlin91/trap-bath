## head ---------------------------
source("scripts/TB_cleaning.R")

packages <- c("tidyverse","phonR","patchwork","plyr")

package.check <- lapply(
  packages,
  FUN = function(x) {
    if (!require(x, character.only = TRUE)) {
      install.packages(x, dependencies = TRUE)
      library(x, character.only = TRUE)
    }
  }
)


data_TBP_plots <-  data_TBP %>%
  droplevels() %>%
  filter(style %in% c("interview","minimalpair","wordlist")) %>% 
  mutate(dur_ms = 1000*dur) %>%
  mutate(log_dur_ms = log10(dur_ms)) %>% 
  droplevels() %>%
  ungroup()

## theme ------------------------------------------
theme_Caitlin <- function() {theme_bw(base_size = 12) %+replace%
    theme(plot.background  = element_rect(fill = "transparent", colour = NA),
          panel.background = element_rect(fill="gray90", colour=NA),
          legend.background = element_rect(fill="transparent", colour=NA),
          legend.key = element_rect(fill="transparent", colour=NA),
          panel.grid.major = element_line(colour = "white", size = 0.2), 
          panel.grid.minor = element_line(colour = "grey98", size = 0.5))}

theme_Caitlin_present <- function() {theme_bw(base_size = 22) %+replace%
    theme(plot.background  = element_rect(fill = "transparent", colour = NA),
          panel.background = element_rect(fill="gray90", colour=NA),
          legend.background = element_rect(fill="transparent", colour=NA),
          legend.key = element_rect(fill="transparent", colour=NA),
          panel.grid.major = element_line(colour = "white", size = 0.2), 
          panel.grid.minor = element_line(colour = "grey98", size = 0.5))}

# lexical set colours
bathColours <- c("#EAFB88","#D64358","#3C8C4D")
names(bathColours) = levels(data_TBP_plots$lexSet)
TBFillScale <- scale_fill_manual(name = "lexSet", values = bathColours)
TBColScale <- scale_colour_manual(name= "lexSet", values = bathColours)

# region colours
regionColours <- c("#77b144","#524c95","#95524C")
names(regionColours) = levels(data_TBP_plots$corpus)
regionFillScale <- scale_fill_manual(name = "corpus",values = regionColours)
regionColScale <- scale_colour_manual(name = "corpus",values=regionColours)

yaxisF1 <- scale_y_continuous(limits = c(400,1200), breaks = seq(0,1200,100))
yaxisF2 <- scale_y_continuous(limits = c(600,2200), breaks = seq(0,3000,200))

# eyeball predictors ####
BATH_predcheck_F1.plot <- ggplot(data_TBP_plots %>%
                                   filter(lexSet == "BATH")
                                 # %>% filter(corpus == "CoRP-NE")
                                   ,aes(x = lexSet, y = norm_F1, fill = lexSet)) +
  geom_boxplot() +
  theme_Caitlin() +
  theme(legend.position = "none") +
  xlab("lexical set") +
  ylab("F1 (Hz)") +
  yaxisF1 +
  TBFillScale +
  # facet_wrap(~folPlace) +
  # facet_wrap(~folVc) + #lose
  # facet_wrap(~preSeg_small) + #lose
  NULL
BATH_predcheck_F1.plot

BATH_predcheck_F2.plot <- ggplot(data_TBP_plots %>% filter(lexSet == "BATH"), aes(x = lexSet, y = norm_F2, fill = lexSet)) +
  geom_boxplot() +
  theme_Caitlin() +
  theme(legend.position = "none") +
  xlab("lexical set") +
  ylab("F2 (Hz)") +
  scale_y_continuous(limits = c(600,2200), breaks = seq(0,3000,200))+
  TBFillScale +
  # facet_wrap(~folPlace) +
  # facet_wrap(~folVc) + #lose
  # facet_wrap(~preSeg_small) +
  NULL
BATH_predcheck_F2.plot

## trap
TRAP_predcheck_F1.plot <- ggplot(data_TBP_plots %>%
                                   filter(lexSet == "TRAP")
                                 # %>% filter(corpus == "CoRP-NE")
                                 ,aes(x = lexSet, y = norm_F1, fill = lexSet)) +
  geom_boxplot() +
  theme_Caitlin() +
  theme(legend.position = "none") +
  xlab("lexical set") +
  ylab("F1 (Hz)") +
  yaxisF1 +
  TBFillScale +
  # facet_wrap(~folPlace) + #lose
  # facet_wrap(~folVc) + #lose
  # facet_wrap(~preSeg_small) + #lose
  NULL
TRAP_predcheck_F1.plot

TRAP_predcheck_F2.plot <- ggplot(data_TBP_plots %>% filter(lexSet == "TRAP"), aes(x = lexSet, y = norm_F2, fill = lexSet)) +
  geom_boxplot() +
  theme_Caitlin() +
  theme(legend.position = "none") +
  xlab("lexical set") +
  ylab("F2 (Hz)") +
  scale_y_continuous(limits = c(600,2200), breaks = seq(0,3000,200))+
  TBFillScale +
  # facet_wrap(~folPlace) +
  # facet_wrap(~folVc) + #lose
  # facet_wrap(~preSeg_small) +
  NULL
TRAP_predcheck_F2.plot

## PALM
PALM_predcheck_F1.plot <- ggplot(data_TBP_plots %>%
                                   filter(lexSet == "PALM")
                                 # %>% filter(corpus == "CoRP-NE")
                                 ,aes(x = lexSet, y = norm_F1, fill = lexSet)) +
  geom_boxplot() +
  theme_Caitlin() +
  theme(legend.position = "none") +
  xlab("lexical set") +
  ylab("F1 (Hz)") +
  yaxisF1 +
  TBFillScale +
  # facet_wrap(~folPlace) + #lose
  # facet_wrap(~folVc) +
  # facet_wrap(~preSeg_small) + #lose
  NULL
PALM_predcheck_F1.plot

PALM_predcheck_F2.plot <- ggplot(data_TBP_plots %>% filter(lexSet == "PALM"), aes(x = lexSet, y = norm_F2, fill = lexSet)) +
  geom_boxplot() +
  theme_Caitlin() +
  theme(legend.position = "none") +
  xlab("lexical set") +
  ylab("F2 (Hz)") +
  scale_y_continuous(limits = c(600,2200), breaks = seq(0,3000,200))+
  TBFillScale +
  # facet_wrap(~folPlace) + #lose
  # facet_wrap(~folVc) +
  # facet_wrap(~preSeg_small) +
  NULL
PALM_predcheck_F2.plot



## TBP ####
### All corpuses ####
data_TBP_plots.means = as_tibble(ddply(data_TBP_plots,.(id,lexSet,corpus),summarise,
                                          mean_F2 = mean(norm_F2),
                                          mean_F1 = mean(norm_F1)))
data_TBP_plots.means

TBP_F2.plot <- ggplot(data_TBP_plots, aes(x=relevel(lexSet,"PALM","BATH","TRAP"), y=norm_F2, fill=lexSet))+
  geom_boxplot() +
  # geom_violin() +
  geom_point(data=data_TBP_plots.means, aes(y=mean_F2,lexSet,colour=NULL))+
  geom_line(data=data_TBP_plots.means, aes(y=mean_F2,x=lexSet,group=id,colour=NULL))+
  facet_wrap(~corpus)+
  xlab("lexical set") +
  ylab("F1 (Hz)") +
  theme_Caitlin_present()+
  yaxisF2+
  theme(legend.position = "none") +
  TBColScale +
  TBFillScale +
  scale_shape_manual("cross")+
  # ggtitle("TRAP, BATH, PALM (F1) CoRP-NE")+
  NULL
TBP_F2.plot
ggsave("figures/TBP-F2.svg",TBP_F2.plot,height=8,width=12,units="in")


### South East ####

TBP_SE_F1.plot <- ggplot(data_TBP_plots %>% filter(corpus=="CoRP-SE"), aes(x=lexSet, y=norm_F1, fill=lexSet))+
  geom_boxplot() +
  xlab("lexical set") +
  ylab("F1 (Hz)") +
  theme_Caitlin_present()+
  yaxisF1+
  theme(legend.position = "none") +
  TBColScale +
  TBFillScale +
  # ggtitle("TRAP, BATH, PALM (F1) CoRP-SE")+
  NULL
TBP_SE_F1.plot
ggsave("figures/TBP-SE-F1.svg",TBP_SE_F1.plot,height=4,width=6,units="in")


TBP_SE_F2.plot <- ggplot(data_TBP_plots %>% filter(corpus=="CoRP-SE"), aes(x=lexSet, y=norm_F2, fill=lexSet))+
  geom_boxplot() +
  xlab("lexical set") +
  ylab("F2 (Hz)") +
  theme_Caitlin_present()+
  yaxisF2+
  theme(legend.position = "none") +
  TBColScale +
  TBFillScale +
  # ggtitle("TRAP, BATH, PALM (F2) CoRP-SE")+
  NULL
TBP_SE_F2.plot
ggsave("figures/TBP-SE-F2.svg",TBP_SE_F2.plot,height=4,width=6,units="in")

TBP_SE_dur_hist <- ggplot(data_TBP_plots %>% filter(corpus=="CoRP-SE"),aes(x=dur_ms)) + 
  geom_histogram(bins=20)
TBP_SE_dur_hist

TBP_SE_dur_hist_log <- ggplot(data_TBP_plots %>% filter(corpus=="CoRP-SE"),aes(x=log10(dur_ms))) + 
  geom_histogram(bins=20)
TBP_SE_dur_hist_log

TBP_SE_logdur.plot <- ggplot(data_TBP_plots %>% filter(corpus=="CoRP-SE"), aes(x=lexSet, y=log_dur_ms, fill=lexSet))+
  # geom_jitter() +
  # geom_count() +
  # geom_violin() +
  geom_boxplot() +
  xlab("lexical set") +
  ylab("log_{10}(duration (msec))") +
  theme_Caitlin_present()+
  scale_y_continuous(limits = c(1.5,2.8), breaks = seq(1.5,2.8,0.2))+
  # yaxisF1+
  theme(legend.position = "none") +
  # facet_grid(cols = vars(has_coda),rows=vars(style)) +
  TBColScale +
  TBFillScale +
  #ggtitle("TRAP, BATH, PALM (duration) CoRP-SE")+
  NULL
TBP_SE_logdur.plot
ggsave("figures/TBP-SE-logdur.svg",TBP_SE_logdur.plot,height=4,width=6,units="in")

TBP_SE_dur.plot <- ggplot(data_TBP_plots %>% filter(corpus=="CoRP-SE"), aes(x=lexSet, y=dur_ms, fill=lexSet))+
  # geom_jitter() +
  # geom_count() +
  # geom_violin() +
  geom_boxplot() +
  xlab("lexical set") +
  ylab("duration (msec)") +
  theme_Caitlin_present()+
  scale_y_continuous(limits = c(0,600), breaks = seq(0,600,100))+
  theme(legend.position = "none") +
  # facet_grid(cols = vars(has_coda),rows=vars(style)) +
  TBColScale +
  TBFillScale +
  #ggtitle("TRAP, BATH, PALM (duration) CoRP-SE")+
  NULL
TBP_SE_dur.plot
ggsave("figures/TBP-SE-dur.svg",TBP_SE_dur.plot,height=4,width=6,units="in")

TBP_SE_dur_all.plot <-  TBP_SE_logdur.plot + TBP_SE_dur.plot
ggsave("figures/TBP-SE-dur-all.svg",TBP_SE_dur_all.plot,height=4,width=6,units="in")

### DECTE ####
TBP_DE_dur_hist <- ggplot(data_TBP_plots %>% filter(corpus=="DECTE-NE"),aes(x=dur_ms)) + 
  geom_histogram(bins=15)
TBP_DE_dur_hist

TBP_DE_dur_hist_log <- ggplot(data_TBP_plots %>% filter(corpus=="DECTE-NE"),aes(x=log10(dur_ms))) + 
  geom_histogram(bins=15)
TBP_DE_dur_hist_log

TBP_DE_logdur.plot <- ggplot(data_TBP_plots %>% filter(corpus=="DECTE-NE"), aes(x=lexSet, y=log_dur_ms, fill=lexSet))+
  # geom_jitter() +
  # geom_count() +
  # geom_violin() +
  geom_boxplot() +
  xlab("lexical set") +
  ylab("log_{10}(duration (msec))") +
  theme_Caitlin_present()+
  scale_y_continuous(limits = c(1.5,2.8), breaks = seq(1.5,2.8,0.2))+
  theme(legend.position = "none") +
  # facet_grid(cols = vars(has_coda),rows=vars(style)) +
  TBColScale +
  TBFillScale +
  #ggtitle("TRAP, BATH, PALM (duration) DECTE-NE")+
  NULL
TBP_DE_logdur.plot
ggsave("figures/TBP-DE-logdur.svg",TBP_DE_logdur.plot,height=4,width=6,units="in")

TBP_DE_dur.plot <- ggplot(data_TBP_plots %>% filter(corpus=="DECTE-NE"), aes(x=lexSet, y=dur_ms, fill=lexSet))+
  # geom_jitter() +
  # geom_count() +
  # geom_violin() +
  geom_boxplot() +
  xlab("lexical set") +
  ylab("duration (msec)") +
  theme_Caitlin_present()+
  scale_y_continuous(limits = c(0,600), breaks = seq(0,600,100))+
  theme(legend.position = "none") +
  # facet_grid(cols = vars(has_coda),rows=vars(style)) +
  TBColScale +
  TBFillScale +
  #ggtitle("TRAP, BATH, PALM (duration) DECTE-NE")+
  NULL
TBP_DE_dur.plot
ggsave("figures/TBP-DE-dur.svg",TBP_DE_dur.plot,height=4,width=6,units="in")

TBP_DE_dur_all.plot <-  TBP_DE_logdur.plot + TBP_DE_dur.plot
ggsave("figures/TBP-DE-dur-all.svg",TBP_DE_dur_all.plot,height=4,width=6,units="in")


TBP_DE_F2.plot <- ggplot(data_TBP_plots %>% filter(corpus=="DECTE-NE"), aes(x=lexSet, y=norm_F2, fill=lexSet))+
  geom_boxplot() +
  xlab("lexical set") +
  ylab("F2 (Hz)") +
  theme_Caitlin_present()+
  yaxisF2+
  theme(legend.position = "none") +
  TBColScale +
  TBFillScale +
  # #ggtitle("TRAP, BATH, PALM (F2) CoRP-SE")+
  NULL
TBP_DE_F2.plot
ggsave("figures/TBP-DE-F2.svg",TBP_DE_F2.plot,height=4,width=6,units="in")

TBP_DE_F1.plot <- ggplot(data_TBP_plots %>% filter(corpus=="DECTE-NE"), aes(x=lexSet, y=norm_F1, fill=lexSet))+
  geom_boxplot() +
  xlab("lexical set") +
  ylab("F1 (Hz)") +
  theme_Caitlin_present()+
  yaxisF1+
  theme(legend.position = "none") +
  TBColScale +
  TBFillScale +
  # #ggtitle("TRAP, BATH, PALM (F2) CoRP-SE")+
  NULL
TBP_DE_F1.plot
ggsave("figures/TB-DE-F1.png",TB_DE_F1.plot,height = 8, width = 12,units = "in")

### North East ####
data_TBP_plots_NE.means = as_tibble(ddply(data_TBP_plots%>% filter(corpus == "CoRP-NE"),.(id,lexSet,sex,ageGroup),summarise,
                                         mean_F2 = mean(norm_F2),
                                         mean_F1 = mean(norm_F1)))
data_TBP_plots_NE.means

TBP_NE_F1.plot <- ggplot(data_TBP_plots %>% filter(corpus=="CoRP-NE"), aes(x=relevel(lexSet,"PALM","BATH","TRAP"), y=norm_F1, fill=lexSet))+
  geom_boxplot() +
  # geom_violin() +
  geom_point(data=data_TBP_plots_NE.means, aes(y=mean_F1,lexSet,colour=NULL))+
  geom_line(data=data_TBP_plots_NE.means, aes(y=mean_F1,x=lexSet,group=id,colour=NULL,shape=id))+
  # facet_grid(cols = vars(sex),rows=vars(ageGroup))+
  xlab("lexical set") +
  ylab("F1 (Hz)") +
  theme_Caitlin_present()+
  yaxisF1+
  theme(legend.position = "none") +
  TBColScale +
  TBFillScale +
  # ggtitle("TRAP, BATH, PALM (F1) CoRP-NE")+
  NULL
TBP_NE_F1.plot
ggsave("figures/TBP-NE-F1.svg",TBP_NE_F1.plot,height=4,width=6,units="in")


TBP_NE_F2.plot <- ggplot(data_TBP_plots %>% filter(corpus=="CoRP-NE"), aes(x=relevel(lexSet,"PALM","BATH","TRAP"), y=norm_F2, fill=lexSet))+
  geom_boxplot() +
  # geom_violin() +
  # stat_slab(
  #   # data = xdata_sub1,
  #           aes(
  #             # y = Yumminess,
  #             #   fill = Pizza,
  #               fill_ramp = stat(cut_cdf_qi(cdf, 
  #                                           .width = c(.5, .8, .95),
  #                                           labels = scales::percent_format()))), 
  #           color = NA, 
  #           scale = 0.5, 
  #           side = "both") +
  # scale_fill_ramp_discrete(from = "white", 
  #                          range = c(0.9,0.1)) +
geom_point(data=data_TBP_plots_NE.means, aes(y=mean_F2,lexSet,colour=NULL))+
  geom_line(data=data_TBP_plots_NE.means, aes(y=mean_F2,x=lexSet,group=id,colour=NULL))+
  facet_wrap(~sex)+
  xlab("lexical set") +
  ylab("F2 (Hz)") +
  theme_Caitlin_present()+
  yaxisF2+
  theme(legend.position = "none") +
  # facet_grid(rowsz=vars(sex),cols=vars(ageGroup)) +
  TBColScale +
  TBFillScale +
  # ggtitle("TRAP, BATH, PALM (F2) CoRP-NE")+
  NULL
TBP_NE_F2.plot
ggsave("figures/TBP-NE-F2.svg",TBP_NE_F2.plot,height=4,width=6,units="in")

TBP_NE_dur_hist <- ggplot(data_TBP_plots %>% filter(corpus=="CoRP-NE"),aes(x=dur_ms)) + 
  geom_histogram(bins=15)
TBP_NE_dur_hist

TBP_NE_dur_hist_log <- ggplot(data_TBP_plots %>% filter(corpus=="CoRP-NE"),aes(x=log10(dur_ms))) + 
  geom_histogram(bins=15)
TBP_NE_dur_hist_log

TBP_NE_logdur.plot <- ggplot(data_TBP_plots %>% filter(corpus=="CoRP-NE"), aes(x=lexSet, y=log_dur_ms, fill=lexSet))+
    geom_violin() +
  # geom_boxplot() +
  # geom_jitter() +
  geom_count() +
  xlab("lexical set") +
  ylab("log_{10}(duration (msec))") +
  theme_Caitlin_present()+
  scale_y_continuous(limits = c(1.5,2.8), breaks = seq(1.5,2.8,0.2))+
  theme(legend.position = "none") +
  # facet_grid(cols = vars(has_coda),rows=vars(style)) +
  TBColScale +
  TBFillScale +
  #ggtitle("TRAP, BATH, PALM (duration) CoRP-NE")+
  NULL
TBP_NE_logdur.plot
ggsave("figures/TBP-NE-logdur.svg",TBP_NE_logdur.plot,height=4,width=6,units="in")

TBP_NE_dur.plot <- ggplot(data_TBP_plots %>% filter(corpus=="CoRP-NE"), aes(x=lexSet, y=dur_ms, fill=lexSet))+
  # geom_jitter() +
  # geom_count() +
  # geom_violin() +
  geom_boxplot() +
  xlab("lexical set") +
  ylab("duration (msec)") +
  theme_Caitlin_present()+
  scale_y_continuous(limits = c(0,600), breaks = seq(0,600,100))+
  theme(legend.position = "none") +
  # facet_grid(cols = vars(has_coda),rows=vars(style)) +
  TBColScale +
  TBFillScale +
  #ggtitle("TRAP, BATH, PALM (duration) CoRP-NE")+
  NULL
TBP_NE_dur.plot
ggsave("figures/TBP-NE-dur.svg",TBP_NE_dur.plot,height=4,width=6,units="in")

## BATH alone ####
# speaker means
data_B_plots.means = as_tibble(ddply(data_TBP_plots%>% filter(lexSet == "BATH"),.(corpus,id),summarise,
                                     mean_F2 = mean(norm_F2),
                                     mean_F1 = mean(norm_F1)))

data_B_plots.means
# word means
data_B_plots.wordmeans = as_tibble(ddply(data_TBP_plots%>% filter(lexSet == "BATH"),.(corpus,word),summarise,
                                     mean_F2 = mean(norm_F2),
                                     mean_F1 = mean(norm_F1)))

data_B_plots.wordmeans

B_F2.plot <- ggplot(data_TBP_plots %>% filter(lexSet=="BATH"), aes(x=corpus, y=norm_F2, fill=corpus))+
  # geom_violin() +
  geom_boxplot() +
  # geom_point(data=data_B_plots.means, aes(y=mean_F2,x=corpus,colour=NULL))+
  # geom_line(data=data_B_plots.means, aes(y=mean_F2,x=corpus,group=id,colour=NULL))+
  theme_Caitlin()+
  theme(legend.position = "none")+
  regionFillScale+
  yaxisF2 +
  geom_text(data = data_B_plots.wordmeans, aes(y=mean_F2, x=corpus, label=word), size=1.75, alpha=0.5, position = "jitter") +
  NULL
B_F2.plot

ggsave("figures/B-F2.svg", B_F2.plot, height=4,width=6,units="in")

# word-speaker means
data_B_plots.wordspeakermeans = as_tibble(ddply(data_TBP_plots%>% filter(lexSet == "BATH"),.(id,corpus,word),summarise,
                                         mean_F2 = mean(norm_F2),
                                         mean_F1 = mean(norm_F1)))

data_B_plots.wordspeakermeans

# NE F2
B_NE_F2.plot <- ggplot(data_TBP_plots %>% filter(lexSet=="BATH",corpus=="CoRP-NE"), aes(x=corpus, y=norm_F2, fill=corpus))+
  # geom_violin() +
  geom_boxplot() +
  # geom_point(data=data_B_plots.means, aes(y=mean_F2,x=corpus,colour=NULL))+
  # geom_line(data=data_B_plots.means, aes(y=mean_F2,x=corpus,group=id,colour=NULL))+
  theme_Caitlin()+
  theme(legend.position = "none")+
  regionFillScale+
  yaxisF2 +
  geom_text(data = data_B_plots.wordspeakermeans %>% filter(corpus=="CoRP-NE"), aes(y=mean_F2, x=corpus, label=word), size=1.75, alpha=0.5, position = "jitter") +
  facet_wrap(~id) +
  NULL
B_NE_F2.plot
ggsave("figures/B-NE-F2-wordmeans.png",B_NE_F2.plot,height=4,width=6)

# duration
B_logdur.plot <- ggplot(data_TBP_plots %>% filter(lexSet=="BATH"), aes(x=corpus, y=log_dur_ms, fill=corpus))+
  # geom_jitter() +
  # geom_count() +
  # geom_violin() +
  geom_boxplot() +
  xlab("corpus") +
  ylab("log_{10}(duration (msec))") +
  theme_Caitlin()+
  scale_y_continuous(limits = c(1.5,2.8), breaks = seq(1.5,2.8,0.2))+
  theme(legend.position = "none") +
  regionFillScale+
  # facet_wrap(~has_coda)+
  NULL
B_logdur.plot
ggsave("figures/B-logdur.svg",B_logdur.plot,height=4,width=6,units="in")

B_dur.plot <- ggplot(data_TBP_plots %>% filter(lexSet=="BATH"), aes(x=corpus, y=dur_ms, fill=corpus))+
  # geom_jitter() +
  # geom_count() +
  # geom_violin() +
  geom_boxplot() +
  xlab("corpus") +
  ylab("duration (msec)") +
  theme_Caitlin()+
  scale_y_continuous(limits = c(0,600), breaks = seq(0,600,100))+
  theme(legend.position = "none") +
  regionFillScale+
  NULL
B_dur.plot
ggsave("figures/B-dur.svg",B_dur.plot,height=4,width=6,units="in")

B_dur_all.plot <-  B_logdur.plot + B_dur.plot
ggsave("figures/B-dur-all.svg",B_dur_all.plot,height=4,width=6,units="in")

## duration vs F2
B_durF2.plot <- ggplot(data_TBP_plots, aes(x=norm_F2, y=dur_ms,color=lexSet))+
  geom_point()+
  geom_smooth(method="lm",aes(color=NULL))+
  TBColScale+
  facet_wrap(~corpus)+
  scale_x_reverse()+
  NULL
B_durF2.plot


# vowel space plots ####
### TRAP-BATH ####
#### South East ####

data_TBP_plots_SE.means = as_tibble(ddply(data_TBP_plots%>% filter(corpus == "CoRP-SE"),.(lexSet),summarise,
                                         mean_F2 = mean(norm_F2),
                                         mean_F1 = mean(norm_F1)))

data_TBP_plots_SE.means

TBP_SE.vplot = ggplot(data_TBP_plots %>% filter(corpus=="CoRP-SE"), aes(x=norm_F2, y = norm_F1, color = lexSet, label = lexSet)) +
  geom_text(aes(label=word), size=1.75, alpha=0.5) +
  stat_ellipse(level = 0.67, geom = "polygon", alpha = 0.2, aes(fill = lexSet)) +
  geom_label(data = data_TBP_plots_SE.means, aes(x = mean_F2, y = mean_F1), size = 2,color="black") + 
  theme_Caitlin() +
  theme(legend.position = "none") +
  scale_x_reverse(position = "top", breaks=seq(800, 2200, 200), limits=c(2200,800)) + 
  scale_y_reverse(position = "right",breaks=seq(400, 1200, 100), limits=c(1200, 400)) +
  TBColScale +
  TBFillScale +
  NULL
TBP_SE.vplot
ggsave("figures/TBP-SE-vplot.png", TBP_SE.vplot,height = 4, width = 6,units = "in")


#### DECTE ####
data_TBP_plots_DE.means = as_tibble(ddply(data_TBP_plots%>% filter(corpus == "DECTE-NE"),.(lexSet),summarise,
                                          mean_F2 = mean(norm_F2),
                                          mean_F1 = mean(norm_F1)))

data_TBP_plots_DE.means

TBP_DE.vplot = ggplot(data_TBP_plots %>% filter(corpus=="DECTE-NE"), aes(x=norm_F2, y = norm_F1, color = lexSet, label = lexSet)) +
  geom_text(aes(label=word), size=1.75, alpha=0.5) +
  stat_ellipse(level = 0.67, geom = "polygon", alpha = 0.2, aes(fill = lexSet)) +
  geom_label(data = data_TBP_plots_DE.means, aes(x = mean_F2, y = mean_F1), size = 2,color="black") + 
  theme_Caitlin() +
  theme(legend.position = "none") +
  scale_x_reverse(position = "top", breaks=seq(800, 2200, 200), limits=c(2200,800)) + 
  scale_y_reverse(position = "right",breaks=seq(400, 1200, 100), limits=c(1200, 400)) +
  TBColScale +
  TBFillScale +
  NULL
TBP_DE.vplot
ggsave("figures/TBP-DE-vplot.png", TBP_DE.vplot,height = 4, width = 6,units = "in")

#### North East ####

data_TBP_plots_NE.means = as_tibble(ddply(data_TBP_plots%>% filter(corpus=="CoRP-NE"),.(lexSet),summarise,
                                         mean_F2 = mean(norm_F2),
                                         mean_F1 = mean(norm_F1)))



data_TBP_plots_NE.means

TBP_NE.vplot = ggplot(data_TBP_plots %>% filter(corpus=="CoRP-NE"), aes(x=norm_F2, y = norm_F1, color = lexSet, label = lexSet)) +
  geom_text(aes(label=word), size=1.75, alpha=0.5) +
  stat_ellipse(level = 0.67, geom = "polygon", alpha = 0.2, aes(fill = lexSet)) +
  geom_label(data = data_TBP_plots_NE.means, aes(x = mean_F2, y = mean_F1), size = 2,color="black") + 
  theme_Caitlin() +
  theme(legend.position = "none") +
  scale_x_reverse(position = "top", breaks=seq(800, 2200, 200), limits=c(2200,800)) + 
  scale_y_reverse(position = "right",breaks=seq(400, 1200, 100), limits=c(1200, 400)) +
  TBColScale +
  TBFillScale +
  NULL
TBP_NE.vplot
ggsave("figures/TBP-NE-vplot.png",TBP_NE.vplot,height=4,width=4,units="in")


### TRAP ####
data_T_plots.means = as_tibble(ddply(data_TBP_plots%>% filter(lexSet == "TRAP"),.(corpus),summarise,
                                          mean_F2 = mean(norm_F2),
                                          mean_F1 = mean(norm_F1)))

data_T_plots.means

trap.vplot = ggplot(data_TBP_plots %>% filter(lexSet=="TRAP"), aes(x=norm_F2, y = norm_F1, color = corpus, label = corpus)) +
  # geom_text(aes(label=word), size=1.75, alpha=0.5) +
  stat_ellipse(level = 0.67, geom = "polygon", alpha = 0.2, aes(fill = corpus)) +
  geom_label(data = data_T_plots.means, aes(x = mean_F2, y = mean_F1), size = 2,color="black") + 
  theme_Caitlin() +
  # theme(legend.position = "none") +
  scale_x_reverse(position = "top", breaks=seq(800, 2200, 200), limits=c(2200,800)) + 
    scale_y_reverse(position = "right",breaks=seq(400, 1200, 100), limits=c(1200, 400)) +
  # TBColScale +
  # TBFillScale +
  NULL
trap.vplot

### PALM ####
data_P_plots.means = as_tibble(ddply(data_TBP_plots%>% filter(lexSet == "PALM"),.(corpus),summarise,
                                     mean_F2 = mean(norm_F2),
                                     mean_F1 = mean(norm_F1)))

data_P_plots.means

palm.vplot = ggplot(data_TBP_plots %>% filter(lexSet=="PALM"), aes(x=norm_F2, y = norm_F1, color = corpus, label = corpus)) +
  geom_text(aes(label=word), size=1.75, alpha=0.5) +
  stat_ellipse(level = 0.67, geom = "polygon", alpha = 0.2, aes(fill = corpus)) +
  geom_label(data = data_P_plots.means, aes(x = mean_F2, y = mean_F1), size = 2,color="black") + 
  theme_Caitlin() +
  # theme(legend.position = "none") +
  scale_x_reverse(position = "top", breaks=seq(800, 2200, 200), limits=c(2200,800)) + 
  scale_y_reverse(position = "right",breaks=seq(400, 1200, 100), limits=c(1200, 400)) +
  # TBColScale +
  # TBFillScale +
  NULL
palm.vplot

### BATH ####
data_B_plots.means = as_tibble(ddply(data_TBP_plots%>% filter(lexSet == "BATH"),.(corpus),summarise,
                                     mean_F2 = mean(norm_F2),
                                     mean_F1 = mean(norm_F1)))

data_B_plots.means

bath.vplot = ggplot(data_TBP_plots %>%
                         filter(lexSet=="BATH"), aes(x=norm_F2, y = norm_F1, color = corpus,fill=corpus, label = corpus)) +
  geom_density2d() +
  geom_point(aes(size=dur_ms)) +
  # geom_text(aes(label=word), size=1.2, alpha=0.8) +
  # stat_ellipse(level = 0.67, geom = "polygon", alpha = 0.2, aes(fill = corpus)) +
  # geom_label(data = data_B_plots.means, aes(x = mean_F2, y = mean_F1), size = 2,color="black") + 
  theme_Caitlin() +
  theme(legend.position = "bottom") +
  scale_x_reverse(position = "top", breaks=seq(700, 2100, 200), limits=c(2100,700)) + 
  scale_y_reverse(position = "right",breaks=seq(500, 1200, 100), limits=c(1200, 500)) +
  # TBColScale +
  # TBFillScale +
  regionColScale +
  NULL
bath.vplot
ggsave("figures/B-vplot.png",bath.vplot,height=4,width=4,units="in")


data_B_plots.wordspeakermeans = as_tibble(ddply(data_TBP_plots%>% filter(lexSet == "BATH",corpus=="CoRP-NE"),.(word,id),summarise,
                                                mean_F2 = mean(norm_F2),
                                                mean_F1 = mean(norm_F1)))

data_B_plots.wordspeakermeans

bath_NE.vplot = ggplot(data_TBP_plots %>%
                         filter(lexSet=="BATH") %>%
                         filter(corpus=="CoRP-NE"), aes(x=norm_F2, y = norm_F1)) +
  geom_density2d(aes(color = corpus)) +
  # geom_point() +
  geom_text(data=data_B_plots.wordspeakermeans,aes(x=mean_F2,y=mean_F1, color=NULL,label=word), size=1.75, alpha=0.8) +
  # stat_ellipse(level = 0.67, geom = "polygon", alpha = 0.2, aes(fill = corpus)) +
  # geom_label(data = data_B_plots.means, aes(x = mean_F2, y = mean_F1), size = 2,color="black") + 
  theme_Caitlin() +
  theme(legend.position = "none") +
  scale_x_reverse(position = "top", breaks=seq(700, 2200, 200), limits=c(2200,700)) + 
  scale_y_reverse(position = "right",breaks=seq(400, 1200, 100), limits=c(1200, 400)) +
  # TBColScale +
  # TBFillScale +
  # regionColScale +
  facet_wrap(~id)+
  NULL
bath_NE.vplot

# word means by speaker

bath_NE.hist <- ggplot(data_TBP_plots %>% filter(lexSet == "BATH", corpus=="CoRP-NE"), aes(x=norm_F2)) +
  geom_histogram()+
  facet_wrap(~id)+
  NULL
bath_NE.hist
ggsave("figures/B-NE-F2-hist.png",bath_NE.hist,height=4,width=6)
