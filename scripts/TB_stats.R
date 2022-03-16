### Head -----------------------------------
## Load and install packages, control options ####
## First specify the packages of interest
options(pillar.sigfig = 7)
packages = c("lme4","broom")

## Now load or install&load all
package.check <- lapply(
  packages,
  FUN = function(x) {
    if (!require(x, character.only = TRUE)) {
      install.packages(x, dependencies = TRUE)
      library(x, character.only = TRUE)
    }
  }
)
## If a package is installed, it will be loaded. If any are not, the missing package(s) will be installed from CRAN and then loaded.
source("scripts/TB_cleaning.R")
zscore.fn <- function(x){(x - mean(x)) / sd(x)}

data_stats = data_TB %>%
  ungroup() %>% 
  droplevels() %>% 
  mutate(norm_F2_z = zscore.fn(norm_F2),
         norm_F1_z = zscore.fn(norm_F1),
         dur_z = zscore.fn(dur))

table(data_stats$folMan)

TB.mod = lmer(norm_F2_z ~ relevel(lexSet,"TRAP") + sex + ageGroup + region + folMan + folPlace + folVc + preSeg + folSeq +(1|id), data_stats %>% filter(lexSet !="PALM"))
summary(TB.mod)
AIC(TB.mod)

#1st Sept 2020:
## Removed, with AIC checks:

data_stats_NE <- data_stats %>% filter(region == "North-East") %>% 
  droplevels()
NE.TB.mod <- lmer(norm_F2_z ~ relevel(lexSet,"TRAP") +
                    # sex + 
                    ageGroup +
                    privateTotal +
                    # rowid +
                    folMan +
                    folPlace +
                    folVc +
                    # preSeg +
                    # folSeq +
                    (1|id),
                  data_stats_NE %>% filter(lexSet !="PALM"))
summary(NE.TB.mod)
AIC(NE.TB.mod)

#SE
data_stats_SE <- data_stats %>% filter(region == "South-East") %>% 
  droplevels()
SE.TB.mod <- lmer(norm_F2_z ~ relevel(lexSet,"TRAP") +
                    # sex +
                    ageGroup +
                    privateTotal +
                    rowid +
                    # folMan +
                    # folPlace +
                    folVc +
                    # preSeg +
                    # folSeq +
                    (1|id),
                  data_stats_SE %>% filter(lexSet !="PALM"))
summary(SE.TB.mod)
