#script for building a flexible text miner for the tag meta datalogs
#must be able to handle changing formats over the year
#maybe be duplicated effort of TagTools > read_cats() & read_cats_csv()
#see here: https://github.com/stacyderuiter/TagTools/blob/master/matlab/read_write/read_cats.m
#& here: https://github.com/stacyderuiter/TagTools/blob/master/matlab/read_write/read_cats_csv.m

dir <- "/Users/jmoxley/Documents/GitTank/CC_CamTags/logs"
depid <- "TOM_CC0705_20171005"
#find info file; formats capable .txt or .cfg
(files <- list.files(dir, pattern=("*.txt|CFG"),full.names = T))

#2017 info harvesting, read in as character strings for regex
txt <- sapply(read.delim(files[1]), "as.character")
txt2 <- sapply(read.delim(files[2]), "as.character")
txt3 <- sapply(read.delim(files[3]), "as.character")
lst <- list(txt, txt2, txt3); lst %>% map(dim)
#

info <- list(NULL)
str_extract_all(sapply(txt, "as.character"), "sn") %>% filter(. != "Character,0")
matrix(unlist(strsplit(txt, "=")), ncol = 2, byrow = T)  #issues with the header names
txt[str_detect(txt, "\\[[:alpha:]*[:space:]?[:alpha:]*?\\]")]


