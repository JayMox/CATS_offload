#script for building a flexible text miner for the tag meta datalogs
#must be able to handle changing formats over the year
#maybe be duplicated effort of TagTools > read_cats() & read_cats_csv()
#see here: https://github.com/stacyderuiter/TagTools/blob/master/matlab/read_write/read_cats.m
#& here: https://github.com/stacyderuiter/TagTools/blob/master/matlab/read_write/read_cats_csv.m
rm(list = ls())
library(tidyverse)

dir <- "/Users/jmoxley/Documents/GitTank/CC_CamTags/logs"
depid <- "TOM_CC0705_20171005"
#find info file; formats capable .txt or .cfg
(files <- list.files(dir, pattern=("*.txt|CFG"),full.names = T))

#2017 metadata scraping, read in as character strings for regex
txt <- sapply(read.delim(files[3]), "as.character")
#get field categories & delimit field names from field values
labels <- c("[global]", txt[which(str_detect(txt, "\\[[:alpha:]*[:space:]?[:alpha:]*?\\]"))])
fields <- data.frame(str_split(txt, "\\=", n = 2, simplify = T)) %>% #split names & fields
  rename(field = X1, value = X2) %>% 
  mutate(class = labels[findInterval(seq(1:nrow(txt)), which(str_detect(txt, "\\[[:alpha:]*[:space:]?[:alpha:]*?\\]")))+1]) %>% 
  filter(!(str_detect(txt, "\\[[:alpha:]*[:space:]?[:alpha:]*?\\]")))
#parse video trigger field in old gen tags that doesn't mine well
idx <- str_detect(fields$field, "rawon")
fields <- fields %>% mutate(field = ifelse(idx, str_replace(field, " rawon", ""), as.character(field)),
                      value = ifelse(idx, str_c("rawon=", value), as.character(value)))

