## Load and install packages, control options ####
## First specify the packages of interest
options(pillar.sigfig = 6)
packages = c("plyr","tidyverse","ruler","broom","syllabifyr")

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



source("scripts/plotnik_functions.R")

# read in data -------------
data_path <- "../speakers"
FAVEfiles <- dir(data_path, pattern = "*_traj_norm.txt")

read.fn <- function(x) (read.delim(file.path(data_path, x),stringsAsFactors = TRUE,fileEncoding = "UTF-8-BOM") %>% rowid_to_column())
data_nested <- tibble(fileName = FAVEfiles) %>% # create a data frame holding the file names
  mutate(file_contents = map(fileName, read.fn)  # read files into a new data column 
         )
data_speakers_norm <- unnest(data_nested,cols=c(file_contents)) %>%
  dplyr::select(rowid,vowel,stress,word,t,style,norm_F1,norm_F2,dur,fm,fp,fv,ps,fs,fileName) %>%
  rowid_to_column("rowNumber_all") %>%
  mutate(time = t) %>% 
  mutate(rowid_fac=factor(rowid)) %>%
  mutate(rowNumber_all_fac = factor(rowNumber_all)) %>%
  mutate(fileName=factor(fileName)) %>% 
  droplevels() %>% 
  as_tibble()

# merge in non-normed data ####
data_path_nonnorm <- "../speakers-nonnorm"
FAVEfiles_nonnorm <- dir(data_path_nonnorm, pattern = "*_traj_full.txt")
read.fn2 <- function(x) (read.delim(file.path(data_path_nonnorm, x),stringsAsFactors = TRUE,fileEncoding = "UTF-8-BOM") %>% rowid_to_column())
data_nested_nonnorm <- tibble(fileName = FAVEfiles_nonnorm) %>%
  mutate(file_contents = map(fileName, read.fn2)  # read files into a new data column 
  )
data_speakers_nonnorm <- unnest(data_nested_nonnorm,cols=c(file_contents)) %>% dplyr::select(pre_word,fol_word,vowel_index,pre_word_trans,fol_word_trans,nFormants,word_trans) %>% 
  rowid_to_column("rowNumber_all")


###merge two sets ####
data_speakers <- data_speakers_norm %>% 
  inner_join(data_speakers_nonnorm,by="rowNumber_all")


# add social data ####


data_social = read_csv("../CoRP-master.csv"
                       #,stringsAsFactors=TRUE
                       #,fileEncoding = "UTF-8-BOM"
) %>% 
  mutate_if(is.character, factor) %>%
  mutate(region = recode(region,"North-East"="NE","South-East"="SE")) %>% 
  mutate(corpus = factor(paste(corpus, region, sep = "-")))

data_lexSets = read_delim("../../DataExtraction/LexicalSet_referenceList.txt"
                          # , stringsAsFactors = TRUE
                          , delim="\t"
) %>%
  droplevels() %>% 
  mutate_if(is.character, factor)

data_SUBTLEX <- read.delim("../SUBTLEX-UK.txt") %>%
  dplyr::select(Spelling,FreqCount,BNC_freq, LogFreq.Zipf., LogFreqBNC.Zipf.) %>% 
  mutate(word = Spelling) %>%
  mutate_if(is.character, str_to_upper)


data_all <-  data_speakers %>%
  inner_join(data_social) %>%
  inner_join(data_lexSets) %>% 
  left_join(data_SUBTLEX) %>% 
  mutate(fileName = factor(fileName)) %>%
  mutate(word = factor(word)) %>%
  mutate(vowel = factor(vowel)) %>%
  droplevels() %>%
  filter(lexicalSet_broad != "")

# data_SDoutliers = data_all %>%
#   group_by(lexicalSet_broad) %>%
#   filter(between(norm_F1, mean(norm_F1, na.rm=TRUE) - (2.5 * sd(norm_F1, na.rm=TRUE)), 
#                  mean(norm_F1, na.rm=TRUE) + (2.5 * sd(norm_F1, na.rm=TRUE)))) %>%
#   filter(between(norm_F2, mean(norm_F2, na.rm=TRUE) - (2.5 * sd(norm_F2, na.rm=TRUE)), 
#                  mean(norm_F2, na.rm=TRUE) + (2.5 * sd(norm_F2, na.rm=TRUE))))

Q1.fn <- function(x){nth(fivenum(x,na.rm=TRUE), 2, order_by = NULL)}
Q3.fn <- function(x){nth(fivenum(x,na.rm=TRUE), 4, order_by = NULL)}
between.IQR.fn <- function(x){between(x, Q1.fn(x) - (1.5 * IQR(x, na.rm=TRUE)), 
                                     Q3.fn(x) + (1.5 * IQR(x, na.rm=TRUE)))}


# filter(between(norm_F1,  - (1.5*IQR(norm_F1, na.rm=TRUE)),
#                norm_F1, nth(fivenum(data_all$norm_F1,na.rm=TRUE), 2, order_by = NULL, default = default_missing(x)) + (1.5*IQR(norm_F1, na.rm=TRUE))))

data_IQRoutliers = data_all %>%
  filter(stress == "1") %>%
  filter(!word %in% c("A","AH", "ARE","AREN'T", "AN","ABOUT", "AND","AS", "AT,", "BUT","BY", "BE","BUT", "'CAUSE","COS","CA-","CAN","CA-+CAN'T","DOS", "CAN'T", "DID", "DIDN'T", "DO", "DUNNO","EC", "EE","EW", "FOR","G","GOT","GOTTA","GONNA", "HAHAHA","HE", "HE'S", "HUH",  "ING", "I", "I'LL", "I'M", "IS", "IT", "IT'S", "ITS","JUST","LA","LG}UM", "MY","NAH","NADA","O","OKAY","ON", "OU","OUR","OURS", "OF", "OH", "SHE", "SHE'S", "THAT", "THE", "THEM", "THEN", "THERE", "THEY", "THIS", "UH", "UM", "UP","US", "WAS", "WE", "WERE", "WHAT", "YEAH", "YOU")) %>%
  filter(!str_detect(word,regex("^XX"))) %>%
  filter(!str_detect(word,regex("\\w+\\*"))) %>%
  filter(!str_detect(word,regex("\\*\\w+"))) %>% 
  droplevels() %>% 
  group_by(lexicalSet_broad,corpus) %>%
  # filter(between(norm_F1, Q1.fn(norm_F1) - (1.5 * IQR(norm_F1, na.rm=TRUE)), 
  # Q3.fn(norm_F1) + (1.5 * IQR(norm_F1, na.rm=TRUE)))) %>%
  filter(between.IQR.fn(norm_F1)) %>%
  filter(between.IQR.fn(norm_F2))
  

# data_outliers = inner_join(data_all,data_tbl)
data_clean = data_IQRoutliers %>%
  mutate(folMan = plt_manner.fn(fm)) %>%
  mutate(folPlace = plt_place.fn(fp)) %>%
  mutate(folVc = plt_voice.fn(fv)) %>%
  mutate(preSeg = plt_preseg.fn(ps)) %>%
  mutate(folSeq = plt_folseq.fn(fs)) %>% 
  dplyr::select(rowNumber_all,rowid,word,vowel,stress,style,time,norm_F1,norm_F2,dur, rowid_fac, rowNumber_all_fac, lastName, sex, YOB, ageRecording, ageGroup, region, speakerNumber, id, juniorEd, SecondEd, sixthFEd, privateTotal, occupation, occClass, edLevel, fatherRegion, motherRegion, fatherEd, motherEd, motherEdLevel,fatherEdLevel,parentEdLevelHigh,motherOccupation,fatherOccupation,parentOccClass,corpus,lexicalSet_broad,lexicalSet_narrow,folMan,folPlace,folVc,preSeg,folSeq, FreqCount,BNC_freq, LogFreq.Zipf., LogFreqBNC.Zipf.,pre_word,fol_word,vowel_index,pre_word_trans,fol_word_trans,nFormants,word_trans) %>% 
  mutate(folSeq = recode(folSeq, "none" = "none", "oneSyll" = "oneSyll", "twoSyll" = "twoSyll", "complxcoda" = "complxcoda", "compcoda_onesyll" = "compcoda_sylls", "compcoda_twosyll" = "compcoda_sylls")) %>%
  mutate(folSeq_small = recode(folSeq, "none" = "none", "oneSyll" = "Syll", "twoSyll" = "Syll", "complxcoda" = "complxcoda", "compcoda_sylls" = "Syll")) %>% 
  ungroup()


data_TBP_clean = data_clean %>%
  filter(lexicalSet_broad %in% c("TRAP","BATH","PALM")) %>%
  mutate(lexSet = lexicalSet_broad) %>% 
  droplevels() %>% 
  filter(!word %in% c("AT","AN","HAD","HADN'T","HAS","HASN'T","HAVE","HAVEN'T","THAT","THAT'D","THAT'LL","THAT'S","HA")) %>% 
  droplevels()

# numbers of levels #### remove if 3 or more orders of magnitude smaller than the largest or total is less than 10 - not including PALM due to R phoneme issues

## folMan ####
table(data_TBP_clean$folMan)
table((data_TBP_clean %>% filter(lexSet=="BATH"))$folMan)
table((data_TBP_clean %>% filter(lexSet=="TRAP"))$folMan) #lose none

# table((data_TBP %>% filter(lexSet=="PALM"))$folMan)
table((data_TBP_clean %>% filter(corpus=="CoRP-SE"))$folMan)
table((data_TBP_clean %>% filter(corpus=="CoRP-NE"))$folMan)

## folPlace ####
table(data_TBP_clean$folPlace)
table((data_TBP_clean %>% filter(lexSet=="BATH"))$folPlace)
table((data_TBP_clean %>% filter(lexSet=="TRAP"))$folPlace) # lose none
# table((data_TBP %>% filter(lexSet=="PALM"))$folPlace) #lose "none" "velar"

table((data_TBP_clean %>% filter(corpus=="CoRP-SE"))$folPlace)
table((data_TBP_clean %>% filter(corpus=="CoRP-NE"))$folPlace)

## folVc ####
table(data_TBP_clean$folVc)
table((data_TBP_clean %>% filter(lexSet=="BATH"))$folVc)
table((data_TBP_clean %>% filter(lexSet=="TRAP"))$folVc)
# table((data_TB %>% filter(lexSet=="PALM"))$folVc)

table((data_TBP_clean %>% filter(corpus=="CoRP-NE"))$folVc)
table((data_TBP_clean %>% filter(corpus=="CoRP-SE"))$folVc)

## preSeg ####
table(data_TBP_clean$preSeg)
table((data_TBP_clean %>% filter(lexSet=="BATH"))$preSeg) # lose palatal #lose nasal_apical
table((data_TBP_clean %>% filter(lexSet=="TRAP"))$preSeg) #lose "w/y"
# table((data_TBP_clean %>% filter(lexSet=="PALM"))$preSeg)

table((data_TBP_clean %>% filter(corpus=="CoRP-NE"))$preSeg)
table((data_TBP_clean %>% filter(corpus=="CoRP-SE"))$preSeg)

## folSeq_small ####
table(data_TBP_clean$folSeq_small)
table((data_TBP_clean %>% filter(lexSet == "BATH"))$folSeq_small)
table((data_TBP_clean %>% filter(lexSet == "TRAP"))$folSeq_small)
# table((data_TB %>% filter(lexSet == "PALM"))$folSeq_small)

table((data_TBP_clean %>% filter(corpus=="CoRP-NE"))$folSeq_small)
table((data_TBP_clean %>% filter(corpus=="CoRP-SE"))$folSeq_small)

#### filtering out small levels ####

data_TBP_clean <- data_TBP_clean %>%
  droplevels() %>% 
  filter(folMan != "none") %>% 
  filter(folPlace != "none") %>% 
  filter(!folPlace %in% c("nasal_apical","palatal","w/y")) %>% 
  mutate(id = paste(id, corpus, sep = "_")) %>% 
  filter(!preSeg %in% c("w/y")) %>% 
  mutate(folPlace_small = recode_factor(folPlace, "apical" = "coronal", "interdental" = "coronal", "labiodental" = "labial", "palatal"="coronal", "velar"="dorsal")) %>%
  mutate(preSeg_small = recode_factor(preSeg, "liquid" = "liquid", "nasal_apical" = "stop","nasal_labial" = "labial","obstruent_liquid" = "obstruent-liquid","oral_apical" = "stop","oral_labial" = "labial","palatal"="SH/JH","velar"="stop")) %>% 
  dplyr::rename(stress_og = stress)

data_TBP_syll <- data_TBP_clean %>% 
  mutate(syllab = map(word_trans, syllabify),
         vowel_info = map2(syllab, vowel_index, ~.x[.y, ] )) %>%
  unnest(vowel_info) %>%
  mutate(target_syl = map2(syllab, syll, ~.x %>% filter(syll == .y)),
         has_coda = map(target_syl, ~"coda" %in% .x$part ) %>% simplify())

data_TBP <- data_TBP_syll %>% 
  dplyr::select(-c(target_syl,stress,phone,part,syllab,word_trans,nFormants,fol_word_trans,pre_word_trans,vowel_index,pre_word,fol_word)) %>% 
  filter(rowNumber_all != "116308")
