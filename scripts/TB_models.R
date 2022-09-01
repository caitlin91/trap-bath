source("scripts/TB_cleaning.R")

options(digits = 6)

packages = c("lme4","stats","cAIC4","stargazer","tidymodels","broom.mixed","xtable","MASS")




package.check <- lapply(
  packages,
  FUN = function(x) {
    if (!require(x, character.only = TRUE)) {
      install.packages(x, dependencies = TRUE)
      library(x, character.only = TRUE)
    }
  }
)


data_TBP_stats <- data_TBP %>% 
  dplyr::select(rowNumber_all, rowid, word, style, time, norm_F1, norm_F2, dur, rowNumber_all_fac, lastName, sex, YOB, ageGroup, corpus, id, juniorEd, SecondEd, sixthFEd, privateTotal, occupation, occClass, edLevel, fatherRegion, motherRegion, fatherEd, motherEd, motherEdLevel, fatherEdLevel, parentEdLevelHigh, motherOccupation, fatherOccupation, parentOccClass, lexicalSet_broad, lexicalSet_narrow, folMan, folSeq, folVc, preSeg_small,lexSet, FreqCount,BNC_freq, LogFreq.Zipf., LogFreqBNC.Zipf.,has_coda) %>% 
  droplevels() %>% 
  mutate(time_z = scale(time)) %>% 
  mutate(privateTotal_z=scale(privateTotal)) %>% 
  mutate(corpus=factor(corpus, levels=rev(levels(corpus)))) %>% 
  mutate(F1_z = scale(norm_F1)) %>% 
  mutate(F2_z = scale(norm_F2)) %>% 
  mutate(freq.zipf_z = scale(LogFreq.Zipf.)) %>% #Turton&Baranowski2021
  mutate(motherRegion_TBP = recode(motherRegion, "Birmingham"="north", "Lancashire"="north", "Lincoln"="north","London"="south","North-East"="north", "Nottingham"="north", "Sheffield"="north","South-East"="south")) %>% 
  mutate(fatherRegion_TBP = recode(fatherRegion, "IsleofMan"="north","North-East"="north","South-East"="south")) %>% 
  mutate(dur_ms = 1000*dur) %>%
  mutate(log_dur_ms = log10(dur_ms)) %>% 
  mutate(lexSetSum = factor(lexSet)) %>% 
  mutate(sexSum = factor(sex)) %>% 
  mutate(ageGroupSum = factor(ageGroup)) %>% 
  mutate(corpusSum = factor(corpus)) %>% 
  mutate(motherRegionSum = factor(motherRegion_TBP)) %>% 
  mutate(fatherRegionSum = factor(fatherRegion_TBP)) %>% 
  mutate(styleSum = factor (style)) %>% 
  mutate(folManSum = factor(folMan)) %>% 
  mutate(folVcSum = factor(folVc)) %>% 
  mutate(preSeg_smallSum = factor(preSeg_small)) %>%
  mutate(has_codaSum = factor(has_coda)) %>% 
  filter(style %in% c("interview","minimalpair","wordlist")) %>%
  droplevels()

# TRAP-BATH ####
## South East ####
### data ####
data_TBP_statsSE <- data_TBP_stats %>%
  filter(corpus == "CoRP-SE") %>% 
  droplevels()
contrasts(data_TBP_statsSE$lexSetSum) <- contr.sum(3)
contrasts(data_TBP_statsSE$sexSum) <- contr.sum(2)
contrasts(data_TBP_statsSE$ageGroupSum) <- contr.sum(2)
contrasts(data_TBP_statsSE$styleSum) <- contr.sum(3)
contrasts(data_TBP_statsSE$preSeg_smallSum) <- contr.sum(6)
contrasts(data_TBP_statsSE$has_codaSum) <- contr.sum(2)
contrasts(data_TBP_statsSE$folVcSum) <- contr.sum(2)

### F1 ---------
# fullest model
TBP_SE_F1.full.mod <- lmer(norm_F1 ~ 
                            lexSet*
                            sex +
                            ageGroupSum +
                            freq.zipf_z +
                            styleSum +
                            has_codaSum +
                            # time_z+
                            # (1 + styleSum | id) +
                            (1+time_z|id) +
                            # (1|id) +
                            (1|word)
                          , data = data_TBP_statsSE)
TBP_SE_F1.full.mod %>% tidy()
cAIC(TBP_SE_F1.full.mod)
TBP_SE_F1.step <- stepcAIC(TBP_SE_F1.full.mod)
TBP_SE_F1.step
TBP_SE_F1.step.og <- lmer(norm_F1 ~ 
                             lexSet*
                             sex +
                             ageGroupSum +
                             freq.zipf_z +
                             styleSum +
                             has_codaSum +
                             # time_z+
                             # (1 + styleSum | id) +
                             (1+time_z|id) +
                             # (1|id) +
                             (1|word)
                           , data = data_TBP_statsSE)
TBP_SE_F1.step.mod <- TBP_SE_F1.step.og %>% 
  tidy()
print(TBP_SE_F1.step.mod, n=nrow(TBP_SE_F1.step.mod))

print(
  xtable(
    (TBP_SE_F1.step.mod %>%
       filter(effect=="fixed") %>%
       dplyr::select(term, estimate, statistic) %>%
       dplyr::rename(tvalue = statistic) %>% 
       dplyr::rename(fixedeffect = term)),
    caption = "Linear Mixed Effects Model of F1 of \\textsc{trap},\\textsc{bath}, and \\textsc{palm} in CoRP-SE speakers \\label{tbl:TBPF1SE}"),
  include.rownames=FALSE,
  file="models/TBP-SE-F1-mod.tex"
)


### F2 -------------
sd((data_TBP_statsSE %>% filter(lexSet=="PALM"))$norm_F2)
TBP_SE_F2.full.mod <- lmer(norm_F2 ~ 
                            lexSet+
                            sexSum +
                            ageGroupSum +
                            freq.zipf_z +
                            styleSum +
                            has_codaSum +
                            time_z+
                            # (1 + styleSum | id) +
                            # (1+time_z|id) +
                            (1|id) +
                            (1|word)
                          , data = data_TBP_statsSE)
TBP_SE_F2.full.mod %>% tidy()
cAIC(TBP_SE_F2.full.mod)
TBP_SE_F2.step <- stepcAIC(TBP_SE_F2.full.mod,numberOfSavedModels = 1)
TBP_SE_F2.step
TBP_SE_F2.step.og <- lmer(norm_F2 ~ 
                            lexSet+
                            sexSum +
                            ageGroupSum +
                            freq.zipf_z +
                            styleSum +
                            has_codaSum +
                            time_z+
                            # (1 + styleSum | id) +
                            # (1+time_z|id) +
                            (1|id) +
                            (1|word)
                          , data = data_TBP_statsSE)
TBP_SE_F2.step.mod <- TBP_SE_F2.step.og %>% 
  tidy()
print(TBP_SE_F2.step.mod, n=nrow(TBP_SE_F2.step.mod))

print(
  xtable(
    (TBP_SE_F2.step.mod %>%
       filter(effect=="fixed") %>%
       dplyr::select(term, estimate, statistic) %>%
       dplyr::rename(tvalue = statistic) %>% 
       dplyr::rename(fixedeffect = term)),
    caption = "Linear Mixed Effects Model of F2 of \\textsc{trap},\\textsc{bath}, and \\textsc{palm} in CoRP-SE speakers \\label{tbl:TBPF2SE}"),
  include.rownames=FALSE,
  file="models/TBP-SE-F2-mod.tex"
)

### duration ####
TBP_SE_dur.full.mod <- lmer(dur_ms ~ 
                              lexSet*style+
                              sexSum +
                              ageGroupSum +
                              freq.zipf_z +
                              # styleSum +
                              has_codaSum +
                              time_z +
                              folVcSum +
                              # (1 + styleSum | id) +
                              # (1+time_z|id) +
                              (1|id) +
                              (1|word)
                           , data = data_TBP_statsSE)
TBP_SE_dur.full.mod %>% tidy()
cAIC(TBP_SE_dur.full.mod)
TBP_SE_dur.step <- stepcAIC(TBP_SE_dur.full.mod)
TBP_SE_dur.step
TBP_SE_dur.step.og <- lmer(dur_ms ~ 
                             lexSet*style+
                             sexSum +
                             ageGroupSum +
                             freq.zipf_z +
                             # styleSum +
                             has_codaSum +
                             time_z +
                             folVcSum +
                             # (1 + styleSum | id) +
                             # (1+time_z|id) +
                             (1|id) +
                             (1|word)
                           , data = data_TBP_statsSE)
TBP_SE_dur.step.mod <- TBP_SE_dur.step.og %>% 
  tidy()
print(TBP_SE_dur.step.mod, n=nrow(TBP_SE_dur.step.mod))

print(
  xtable(
    (TBP_SE_dur.step.mod %>%
       filter(effect=="fixed") %>%
       dplyr::select(term, estimate, statistic) %>%
       dplyr::rename(tvalue = statistic) %>% 
       dplyr::rename(fixedeffect = term)),
    caption = "Linear Mixed Effects Model of duration of \\textsc{trap},\\textsc{bath}, and \\textsc{palm} in CoRP-SE speakers \\label{tbl:TBPdurSE}"),
  include.rownames=FALSE,
  file="models/TBP-SE-dur-mod.tex"
)

### duration-log10 ####
TBP_SE_logdur.full.mod <- lmer(log_dur_ms ~ 
                              lexSet*has_coda+
                              sexSum +
                              ageGroupSum +
                              freq.zipf_z +
                              styleSum*
                              # has_codaSum +
                              time_z +
                              folVcSum +
                              # (1 + styleSum | id) +
                              # (1+time_z|id) +
                              (1|id) +
                              (1|word)
                            , data = data_TBP_statsSE)
TBP_SE_logdur.full.mod %>% tidy()
cAIC(TBP_SE_logdur.full.mod)
TBP_SE_logdur.step <- stepcAIC(TBP_SE_logdur.full.mod)
TBP_SE_logdur.step
TBP_SE_logdur.step.og <- lmer(log_dur_ms ~ 
                                lexSet*has_coda+
                                sexSum +
                                ageGroupSum +
                                freq.zipf_z +
                                styleSum*
                                # has_codaSum +
                                time_z +
                                folVcSum +
                                # (1 + styleSum | id) +
                                # (1+time_z|id) +
                                (1|id) +
                                (1|word)
                              , data = data_TBP_statsSE)
TBP_SE_logdur.step.mod <- TBP_SE_logdur.step.og %>% 
  tidy()
print(TBP_SE_logdur.step.mod, n=nrow(TBP_SE_logdur.step.mod))

print(
  xtable(
    (TBP_SE_logdur.step.mod %>%
       filter(effect=="fixed") %>%
       dplyr::select(term, estimate, statistic) %>%
       dplyr::rename(tvalue = statistic) %>% 
       dplyr::rename(fixedeffect = term)),
    caption = "Linear Mixed Effects Model of log10(duration) of \\textsc{trap},\\textsc{bath}, and \\textsc{palm} in CoRP-SE speakers \\label{tbl:TBPlogdurSE}"),
  include.rownames=FALSE,
  file="models/TBP-SE-logdur-mod.tex"
)





## DECTE ####
### data ####
data_TBP_statsDE <- data_TBP_stats %>%
  filter(corpus == "DECTE-NE") %>% 
  droplevels()
contrasts(data_TBP_statsDE$lexSetSum) <- contr.sum(3)
contrasts(data_TBP_statsDE$sexSum) <- contr.sum(2)
contrasts(data_TBP_statsDE$ageGroupSum) <- contr.sum(2)
# contrasts(data_TBP_statsDE$styleSum) <- contr.sum(3)
contrasts(data_TBP_statsDE$preSeg_smallSum) <- contr.sum(6)
contrasts(data_TBP_statsDE$has_codaSum) <- contr.sum(2)
contrasts(data_TBP_statsDE$folVcSum) <- contr.sum(2)

### F1 ---------
# fullest model
TBP_DE_F1.full.mod <- lmer(norm_F1 ~ 
                             lexSet+
                             sex+
                             ageGroupSum +
                             freq.zipf_z +
                             has_codaSum +
                             time_z+
                             # (1 + styleSum | id) +
                             # (1+time_z|id) +
                             (1|id) +
                             (1|word)
                           , data = data_TBP_statsDE)
TBP_DE_F1.full.mod %>% tidy()
cAIC(TBP_DE_F1.full.mod)
TBP_DE_F1.step <- stepcAIC(TBP_DE_F1.full.mod)
TBP_DE_F1.step #lose id
TBP_DE_F1.step.og <- lmer(norm_F1 ~ 
                            lexSet+
                            sex+
                            ageGroupSum +
                            freq.zipf_z +
                            has_codaSum +
                            time_z+
                            # (1 + styleSum | id) +
                            # (1+time_z|id) +
                            # (1|id) +
                            (1|word)
                          , data = data_TBP_statsDE)
TBP_DE_F1.step.mod <- TBP_DE_F1.step.og %>% 
  tidy()
print(TBP_DE_F1.step.mod, n=nrow(TBP_DE_F1.step.mod))

print(
  xtable(
    (TBP_DE_F1.step.mod %>%
       filter(effect=="fixed") %>%
       dplyr::select(term, estimate, statistic) %>%
       dplyr::rename(tvalue = statistic) %>% 
       dplyr::rename(fixedeffect = term)),
    caption = "Linear Mixed Effects Model of F1 of \\textsc{trap},\\textsc{bath}, and \\textsc{palm} in DECTE-NE speakers \\label{tbl:TBPF1DE}"),
  include.rownames=FALSE,
  file="models/TBP-DE-F1-mod.tex"
)


### F2 -------------
sd((data_TBP_statsDE %>% filter(lexSet=="PALM"))$norm_F2)
TBP_DE_F2.full.mod <- lmer(norm_F2 ~ 
                             lexSet+
                             sexSum +
                             ageGroupSum +
                             freq.zipf_z +
                             has_codaSum +
                             time_z+
                             # (1 + styleSum | id) +
                             # (1+time_z|id) +
                             (1|id) +
                             (1|word)
                           , data = data_TBP_statsDE)
TBP_DE_F2.full.mod %>% tidy()
cAIC(TBP_DE_F2.full.mod)
TBP_DE_F2.step <- stepcAIC(TBP_DE_F2.full.mod,numberOfSavedModels = 1)
TBP_DE_F2.step
TBP_DE_F2.step.og <- lmer(norm_F2 ~ 
                            lexSet+
                            sexSum +
                            ageGroupSum +
                            freq.zipf_z +
                            has_codaSum +
                            time_z+
                            # (1 + styleSum | id) +
                            # (1+time_z|id) +
                            (1|id) +
                            (1|word)
                          , data = data_TBP_statsDE)
TBP_DE_F2.step.mod <- TBP_DE_F2.step.og %>% 
  tidy()
print(TBP_DE_F2.step.mod, n=nrow(TBP_DE_F2.step.mod))

print(
  xtable(
    (TBP_DE_F2.step.mod %>%
       filter(effect=="fixed") %>%
       dplyr::select(term, estimate, statistic) %>%
       dplyr::rename(tvalue = statistic) %>% 
       dplyr::rename(fixedeffect = term)),
    caption = "Linear Mixed Effects Model of F2 of \\textsc{trap},\\textsc{bath}, and \\textsc{palm} in DECTE-NE speakers \\label{tbl:TBPF2DE}"),
  include.rownames=FALSE,
  file="models/TBP-DE-F2-mod.tex"
)

### duration ####
TBP_DE_logdur.full.mod <- lmer(log_dur_ms ~ 
                              lexSet*has_coda+
                              sexSum +
                              ageGroupSum +
                              freq.zipf_z +
                              # has_codaSum +
                              time_z +
                              folVcSum +
                              # (1 + styleSum | id) +
                              # (1+time_z|id) +
                              (1|id) +
                              (1|word)
                            , data = data_TBP_statsDE)
TBP_DE_logdur.full.mod %>% tidy()
cAIC(TBP_DE_logdur.full.mod)
TBP_DE_logdur.step <- stepcAIC(TBP_DE_logdur.full.mod)
TBP_DE_logdur.step
TBP_DE_logdur.step.og <- lmer(log_dur_ms ~ 
                                lexSet*has_coda+
                                sexSum +
                                ageGroupSum +
                                freq.zipf_z +
                                # has_codaSum +
                                time_z +
                                folVcSum +
                                # (1 + styleSum | id) +
                                # (1+time_z|id) +
                                (1|id) +
                                (1|word)
                              , data = data_TBP_statsDE)
TBP_DE_logdur.step.mod <- TBP_DE_logdur.step.og %>% 
  tidy()
print(TBP_DE_logdur.step.mod, n=nrow(TBP_DE_logdur.step.mod))

print(
  xtable(
    (TBP_DE_logdur.step.mod %>%
       filter(effect=="fixed") %>%
       dplyr::select(term, estimate, statistic) %>%
       dplyr::rename(tvalue = statistic) %>% 
       dplyr::rename(fixedeffect = term)),
    caption = "Linear Mixed Effects Model of duration of \\textsc{trap},\\textsc{bath}, and \\textsc{palm} in DECTE-NE speakers \\label{tbl:TBPdurDE}"),
  include.rownames=FALSE,
  file="models/TBP-DE-logdur-mod.tex"
)

## North East ####
### data ####
data_TBP_statsNE <- data_TBP_stats %>%
  filter(corpus == "CoRP-NE") %>% 
  droplevels()
contrasts(data_TBP_statsNE$lexSetSum) <- contr.sum(3)
contrasts(data_TBP_statsNE$sexSum) <- contr.sum(2)
contrasts(data_TBP_statsNE$ageGroupSum) <- contr.sum(2)
contrasts(data_TBP_statsNE$styleSum) <- contr.sum(3)
contrasts(data_TBP_statsNE$preSeg_smallSum) <- contr.sum(6)
contrasts(data_TBP_statsNE$has_codaSum) <- contr.sum(2)
contrasts(data_TBP_statsNE$folVcSum) <- contr.sum(2)

### F1 ---------
# fullest model
TBP_NE_F1.full.mod <- lmer(norm_F1 ~ 
                             lexSet*
                             sex*
                             ageGroup +
                             freq.zipf_z +
                             styleSum +
                             has_codaSum +
                             time_z+
                             # (1 + styleSum | id) +
                             # (1+time_z|id) +
                             (1|id) +
                             (1|word)
                           , data = data_TBP_statsNE)
TBP_NE_F1.full.mod %>% tidy()
cAIC(TBP_NE_F1.full.mod)
TBP_NE_F1.step <- stepcAIC(TBP_NE_F1.full.mod)
TBP_NE_F1.step
TBP_NE_F1.step.og <- lmer(norm_F1 ~ 
                            lexSet*
                            sex*
                            ageGroup +
                            freq.zipf_z +
                            styleSum +
                            has_codaSum +
                            time_z+
                            # (1 + styleSum | id) +
                            # (1+time_z|id) +
                            (1|id) +
                            (1|word)
                          , data = data_TBP_statsNE)
TBP_NE_F1.step.mod <- TBP_NE_F1.step.og %>% 
  tidy()
print(TBP_NE_F1.step.mod, n=nrow(TBP_NE_F1.step.mod))

print(
  xtable(
    (TBP_NE_F1.step.mod %>%
       filter(effect=="fixed") %>%
       dplyr::select(term, estimate, statistic) %>%
       dplyr::rename(tvalue = statistic) %>% 
       dplyr::rename(fixedeffect = term)),
    caption = "Linear Mixed Effects Model of F1 of \\textsc{trap},\\textsc{bath}, and \\textsc{palm} in CoRP-NE speakers \\label{tbl:TBPF1NE}"),
  include.rownames=FALSE,
  file="models/TBP-NE-F1-mod.tex"
)


### F2 -------------
sd((data_TBP_statsNE %>% filter(lexSet=="PALM"))$norm_F2)
TBP_NE_F2.full.mod <- lmer(norm_F2 ~ 
                             lexSet*
                             sexSum*
                             ageGroupSum+
                             has_codaSum +
                             freq.zipf_z +
                             # styleSum +
                             time_z+
                             (1 + styleSum | id) +
                             # (1+time_z|id) +
                             # (1|id) +
                             (1|word)
                           , data = data_TBP_statsNE)
TBP_NE_F2.full.mod %>% tidy()
cAIC(TBP_NE_F2.full.mod)
TBP_NE_F2.step <- stepcAIC(TBP_NE_F2.full.mod,numberOfSavedModels = 1)
TBP_NE_F2.step
TBP_NE_F2.step.og <- lmer(norm_F2 ~ 
                            lexSet*
                            sex*
                            ageGroup+
                            has_codaSum +
                            freq.zipf_z +
                            # styleSum +
                            time_z+
                            (1 + styleSum | id) +
                            # (1+time_z|id) +
                            # (1|id) +
                            (1|word)
                          , data = data_TBP_statsNE)
TBP_NE_F2.step.mod <- TBP_NE_F2.step.og %>% 
  tidy()
print(TBP_NE_F2.step.mod, n=nrow(TBP_NE_F2.step.mod))

print(
  xtable(
    (TBP_NE_F2.step.mod %>%
       filter(effect=="fixed") %>%
       dplyr::select(term, estimate, statistic) %>%
       dplyr::rename(tvalue = statistic) %>% 
       dplyr::rename(fixedeffect = term)),
    caption = "Linear Mixed Effects Model of F2 of \\textsc{trap},\\textsc{bath}, and \\textsc{palm} in CoRP-NE speakers \\label{tbl:TBPF2NE}"),
  include.rownames=FALSE,
  file="models/TBP-NE-F2-mod.tex"
)

### duration ####
TBP_NE_logdur.full.mod <- lmer(log_dur_ms ~ 
                              lexSet*has_coda +
                              sexSum +
                              ageGroupSum +
                              freq.zipf_z +
                              styleSum +
                              # has_codaSum +
                              time_z +
                              folVcSum +
                              # (1 + styleSum | id) +
                              # (1+time_z|id) +
                              (1|id) +
                              (1|word)
                            , data = data_TBP_statsNE)
TBP_NE_logdur.full.mod %>% tidy()
cAIC(TBP_NE_logdur.full.mod)
TBP_NE_logdur.step <- stepcAIC(TBP_NE_logdur.full.mod)
TBP_NE_logdur.step
TBP_NE_logdur.step.og <- lmer(log_dur_ms ~ 
                             lexSet*has_coda +
                             sexSum +
                             ageGroupSum +
                             freq.zipf_z +
                             styleSum +
                             # has_codaSum +
                             time_z +
                             folVcSum +
                             # (1 + styleSum | id) +
                             # (1+time_z|id) +
                             (1|id) +
                             (1|word)
                           , data = data_TBP_statsNE)
TBP_NE_logdur.step.mod <- TBP_NE_logdur.step.og %>% 
  tidy()
print(TBP_NE_logdur.step.mod, n=nrow(TBP_NE_logdur.step.mod))

print(
  xtable(
    (TBP_NE_logdur.step.mod %>%
       filter(effect=="fixed") %>%
       dplyr::select(term, estimate, statistic) %>%
       dplyr::rename(tvalue = statistic) %>% 
       dplyr::rename(fixedeffect = term)),
    caption = "Linear Mixed Effects Model of duration of \\textsc{trap},\\textsc{bath}, and \\textsc{palm} in CoRP-NE speakers \\label{tbl:TBPdurNE}"),
  include.rownames=FALSE,
  file="models/TBP-NE-dur-mod.tex"
)

# BATH only ####
## data ####
data_B_stats <- data_TBP_stats %>%
  filter(lexSet == "BATH") %>% 
  droplevels()
contrasts(data_B_stats$corpusSum) <- contr.sum(3)
contrasts(data_B_stats$sexSum) <- contr.sum(2)
contrasts(data_B_stats$ageGroupSum) <- contr.sum(2)
contrasts(data_B_stats$styleSum) <- contr.sum(3)
contrasts(data_B_stats$preSeg_smallSum) <- contr.sum(6)
contrasts(data_B_stats$has_codaSum) <- contr.sum(2)
contrasts(data_B_stats$folVcSum) <- contr.sum(2)

data_B_statsNE <- data_B_stats %>% 
  filter(corpus == "CoRP-NE") %>% 
  droplevels()
contrasts(data_B_statsNE$sexSum) <- contr.sum(2)
contrasts(data_B_statsNE$ageGroupSum) <- contr.sum(2)
contrasts(data_B_statsNE$styleSum) <- contr.sum(3)
contrasts(data_B_statsNE$preSeg_smallSum) <- contr.sum(6)
contrasts(data_B_statsNE$has_codaSum) <- contr.sum(2)
contrasts(data_B_stats$folVcSum) <- contr.sum(2)

## F1 ####
B_F1.full.mod <- lmer(norm_F1 ~
                        relevel(corpus,"CoRP-NE")*ageGroup*
                        sex +
                        # ageGroupSum +
                        freq.zipf_z +
                        styleSum +
                        has_codaSum +
                        time_z+
                        # (1 + styleSum | id) +
                        # (1+time_z|id) +
                        (1|id) +
                        (1|word)
                      , data = data_B_stats)
B_F1.full.mod %>% tidy()
cAIC(B_F1.full.mod)
B_F1.full.step <- stepcAIC(B_F1.full.mod)
B_F1.full.step
B_F1.full.step.og <- lmer(norm_F1 ~
                            relevel(corpus,"CoRP-NE")*ageGroup*
                            sex +
                            # ageGroupSum +
                            freq.zipf_z +
                            styleSum +
                            has_codaSum +
                            time_z+
                            # (1 + styleSum | id) +
                            # (1+time_z|id) +
                            (1|id) +
                            (1|word)
                          , data = data_B_stats)
B_F1.full.step.mod <- B_F1.full.step.og %>% 
  tidy()
print(B_F1.full.step.mod, n=nrow(B_F1.full.step.mod))

print(
  xtable(
    (B_F1.full.step.mod %>%
       filter(effect=="fixed") %>%
       dplyr::select(term, estimate, statistic) %>%
       dplyr::rename(tvalue = statistic) %>% 
       dplyr::rename(fixedeffect = term)),
    caption = "Linear Mixed Effects Model of F1 of \\textsc{bath} \\label{tbl:BF1}"),
  include.rownames=FALSE,
  file="models/B-F1-mod.tex"
)

## F2 ####
B_F2.full.mod <- lmer(norm_F2 ~
                        relevel(corpus,"CoRP-NE")*
                        sex+
                        ageGroupSum +
                        freq.zipf_z +
                        # styleSum +
                        has_codaSum +
                        time_z+
                        (1 + styleSum | id) +
                        # (1+time_z|id) +
                        # (1|id) +
                        (1|word)
                      , data = data_B_stats)
B_F2.full.mod %>% tidy()
cAIC(B_F2.full.mod)
B_F2.full.step <- stepcAIC(B_F2.full.mod)
B_F2.full.step
B_F2.full.step.og <- lmer(norm_F2 ~
                            relevel(corpus,"CoRP-NE")*
                            sex+
                            ageGroupSum +
                            freq.zipf_z +
                            # styleSum +
                            has_codaSum +
                            time_z+
                            (1 + styleSum | id) +
                            # (1+time_z|id) +
                            # (1|id) +
                            (1|word)
                          , data = data_B_stats)
B_F2.full.step.mod <- B_F2.full.step.og %>% 
  tidy()
print(B_F2.full.step.mod, n=nrow(B_F2.full.step.mod))

print(
  xtable(
    (B_F2.full.step.mod %>%
       filter(effect=="fixed") %>%
       dplyr::select(term, estimate, statistic) %>%
       dplyr::rename(tvalue = statistic) %>% 
       dplyr::rename(fixedeffect = term)),
    caption = "Linear Mixed Effects Model of F2 of \\textsc{bath} \\label{tbl:BF2}"),
  include.rownames=FALSE,
  file="models/B-F2-mod.tex"
)

## Duration ####
B_logdur.full.mod <- lmer(log_dur_ms ~ 
                            relevel(corpus,"CoRP-NE")*has_coda +
                                 sexSum +
                                 ageGroupSum +
                                 freq.zipf_z +
                                 styleSum +
                                 # has_codaSum +
                                 time_z +
                                 folVcSum +
                                 # (1 + styleSum | id) +
                                 # (1+time_z|id) +
                                 (1|id) +
                                 (1|word)
                               , data = data_B_stats)
B_logdur.full.mod %>% tidy()
cAIC(B_logdur.full.mod)
B_logdur.step <- stepcAIC(B_logdur.full.mod)
B_logdur.step
B_logdur.step.og <- lmer(log_dur_ms ~ 
                           relevel(corpus,"CoRP-NE")*has_coda +
                           sexSum +
                           ageGroupSum +
                           freq.zipf_z +
                           styleSum +
                           # has_codaSum +
                           time_z +
                           folVcSum +
                           # (1 + styleSum | id) +
                           # (1+time_z|id) +
                           (1|id) +
                           (1|word)
                         , data = data_B_stats)
B_logdur.step.mod <- B_logdur.step.og %>% 
  tidy()
print(B_logdur.step.mod, n=nrow(B_logdur.step.mod))

print(
  xtable(
    (B_logdur.step.mod %>%
       filter(effect=="fixed") %>%
       dplyr::select(term, estimate, statistic) %>%
       dplyr::rename(tvalue = statistic) %>% 
       dplyr::rename(fixedeffect = term)),
    caption = "Linear Mixed Effects Model of log10(duration) of \\textsc{bath}, in all three speaker groups \\label{tbl:Blogdur}"),
  include.rownames=FALSE,
  file="models/B-logdur-mod.tex")

## BATH-NE ####
B_NE_F2.full.mod <- lmer(norm_F2 ~
                        ageGroupSum +
                        freq.zipf_z +
                        styleSum +
                        has_codaSum +
                        time_z+
                        styleSum +
                        # (1 + styleSum | id) +
                        # (1+time_z|id) +
                        (1|id) +
                        (1|word)
                      , data = data_B_statsNE)
B_NE_F2.full.mod %>% tidy()
cAIC(B_NE_F2.full.mod)
B_NE_F2.full.step <- stepcAIC(B_NE_F2.full.mod)
B_NE_F2.full.step
B_NE_F2.full.step.og <- lmer(norm_F2 ~
                               ageGroupSum +
                               freq.zipf_z +
                               styleSum +
                               has_codaSum +
                               time_z+
                               styleSum +
                               # (1 + styleSum | id) +
                               # (1+time_z|id) +
                               (1|id) +
                               (1|word)
                             , data = data_B_statsNE)
B_NE_F2.full.step.mod <- B_NE_F2.full.step.og %>% 
  tidy()
print(B_NE_F2.full.step.mod, n=nrow(B_NE_F2.full.step.mod))

# print(
#   xtable(
#     (B_F2.full.step.mod %>%
#        filter(effect=="fixed") %>%
#        dplyr::select(term, estimate, statistic) %>%
#        dplyr::rename(tvalue = statistic) %>% 
#        dplyr::rename(fixedeffect = term)),
#     caption = "Linear Mixed Effects Model of F2 of \\textsc{bath} \\label{tbl:BF2}"),
#   include.rownames=FALSE,
#   file="models/B-F2-mod.tex"
# )
