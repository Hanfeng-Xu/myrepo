
library(dplyr)
library(purrr)
library(readr)
library(rgbif)
library(taxize)
library(data.table)

# fill in your gbif.org credentials
GBIF_user <- "xuhanfeng"   # your gbif.org username 
GBIF_pwd <- "19921205xhf1216"   # your gbif.org password
GBIF_email <- "xhfBot@126.com"   # your email 

###加载校正过的版纳园物种名
spinf <- read.csv('G:/Papers/Garden.final3/Data/clean_data/TRY_climtesp_diaz_anger.csv')
head(spinf)

#物种表
sp.field <- unique(na.omit(spinf$Species))


### 制作GIBF下载信息
sp_list <- sp.field %>% taxize::get_gbifid_(method="backbone") %>%   # obtain the GBIF number of each species (taxonkeys)
  imap(~ .x %>% mutate(original_sciname = .y)) %>%   # add sp_name to the end of each table with the column name of original_sciname
  bind_rows()   # convert the list to data.frame

#accept_name <- filter(sp_list, matchtype=="EXACT" & status=="ACCEPTED")
accept_name <- filter(sp_list, matchtype=="EXACT" & kingdom=="Plantae") %>% dplyr::select(usagekey, scientificname,
                                                                                          status, canonicalname, kingdom:genus, species, specieskey, original_sciname, matchtype)

gbif_taxon_keys <- pull(accept_name, "usagekey")   # extract the taxonkeys column

# download species occurrence data by taxonKey
occ_download(
  pred_in("taxonKey", gbif_taxon_keys),   # taxonkeys list
  pred("hasCoordinate", TRUE),   # has coordinates or not
  format = "SIMPLE_CSV",   # download file format
  user=GBIF_user, pwd=GBIF_pwd, email=GBIF_email
)

#保存下载信息
write.csv(accept_name, 'G:/Papers/Garden/Data_cleaning/Field/step1.Data/Field_download_all_inf.csv', row.names = F)

