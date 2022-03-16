trans_path <- "C:/Users/Caitlin Halfacre/OneDrive - Newcastle University/Documents/Linguistics/CoRP/Summer2019/fullfiles"
full_files <- dir(trans_path, pattern = "*_traj.txt")
read.fn <- function(x) (read.delim(file.path(data_path, x),stringsAsFactors = TRUE,fileEncoding = "UTF-8-BOM") %>% rowid_to_column())
data_nested <- tibble(fileName = full_files) %>% # create a data frame holding the file names
  mutate(file_contents = map(fileName, read.fn))  # read files into a new data column
data_trans <- unnest(data_nested,cols=c(file_contents)) %>%
  dplyr::select(word,word_trans)
