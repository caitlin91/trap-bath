

data_TBP_syll <- data_TBP_clean %>% 
  mutate(syllab = map(word_trans, syllabify),
         vowel_info = map2(syllab, vowel_index, ~.x[.y, ] ))
