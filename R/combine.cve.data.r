## clean.cyber.data.r
## extract contents of the json objects and 
## compile in data frames

library(jsonlite)
library(tidyverse)

setwd("/Users/kw782/Desktop/Year_2_Sem_1/SDS625/Case Studies/425-625-Fall-2023-main/cyber")

tot = read_json('https://services.nvd.nist.gov/rest/json/cves/2.0/')
head(tot)

n = tot$totalResults
max.per.query = 2000
J = ceiling(n/max.per.query)
n/max.per.query
J
dd = NULL

for (j in 1:J){
  cat(j, '')
  
  filename = paste0('rawdata/cve.', j, '.rds')
  d = readRDS(filename)
  
  df = d$vulnerabilities$cve

  ## join 
  dd = bind_rows(dd, df)
  
}

saveRDS(dd , file='rawdata/cve.with.desc.ref.conf.rds')
