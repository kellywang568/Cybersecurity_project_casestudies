## clean.cve.data.r

library(jsonlite)
library(tidyverse)

#setwd("/Users/kw782/Desktop/Year_2_Sem_1/SDS625/Case Studies/425-625-Fall-2023-main/cyber")
d = readRDS('rawdata/cve.with.desc.ref.conf.rds') ## This will take a minute

## make the data smaller for developing the code
d1 = d[1:10000,] 

## get rid of columns that are mostly text
## use d1 for now, but later change this to d
d2 = d1 %>%      
  select(-descriptions, 
         -references, 
         -configurations, 
         -vendorComments)
head(d2,2)
is(d2)
##  The data shows as being a "data.frame" but some of the columns aren't normal columns. They are columns of data.frames or columns of lists. 

is(d$metrics) ## a data frame!

## Since this format is annoying to work with, the first goal is to make it a data frame with normal columns.  The function `unnest` can help with this. You'll have to use it more than once, because some columns are e.g. lists of lists of lists. 

## you will notice the first unnest will give 3 columns 
## metrics.cvssMetricV2,
## metrics.cvssMetricV30, and 
## metrics.cvssMetricV31
## You can ignore metrics.cvssMetricV2 and V30.  You'll notice that 


d3 = d2 %>% 
  unnest(cols = c(metrics, 
                  weaknesses), 
         names_sep = '.') %>%
  select(-metrics.cvssMetricV2, 
         -metrics.cvssMetricV30)
head(d3,2)
dim(d3)   #10000   16


dup.ids = d3 %>% filter(duplicated(id)) %>% select(id) %>% unlist()
dup.ids

length(dup.ids)
d3 %>% filter(id == dup.ids[1])

## metrics.cvssMetricV31 is a "column" that contains data.frames
is(d3$metrics.cvssMetricV31) ## ugh, a data frame
# I got list, vector

## So this has to be unnested again. 

d4 = d3 %>%
  select(-weaknesses.source) %>%
  pivot_wider(names_from = weaknesses.type, 
              values_from = weaknesses.description) 
head(d4,2)

# Old code
# d4 = d3 %>%
#   unnest(cols = c(metrics.cvssMetricV31), 
#          names_sep = '.', keep_empty = TRUE)
# head(d4,2)

is(d4$metrics.cvssMetricV31) 

## Now this has same number of rows, with no duplicate IDs
dim(d4)
dim(d) 
length(unique(d4$id))

## However, it still has nested stuff
## metrics.cvssMetricV31 is a "column" that contains data.frames
## So does Secondary
is(d3$metrics.cvssMetricV31) ## ugh, a data frame
length(d3$metrics.cvssMetricV31)
is(d4$Primary)
is(d4$Secondary)

## So this has to be unnested again. 
d5 = d4 %>%
  unnest(cols = c(metrics.cvssMetricV31), 
         names_sep = '.', 
         keep_empty = TRUE) %>%
  unnest(cols = Primary, 
         names_sep = '.', 
         keep_empty = TRUE)
head(d5,2)


########### More unnesting/cleaning here #############



########## Make a function to see which areas are 

# Function to check if an object is a dataframe or vector
is_other_type <- function(obj) {
  if (is.data.frame(obj) || is.vector(obj)) {
    return(FALSE)
  } else {
    return(TRUE)
  }
}

# Loop through columns and check the type of elements
non_standard_columns <- character(0)

for (col in names(d5)) {
  col_elements <- d5[[col]]
  
  if (any(sapply(col_elements, is_other_type))) {
    non_standard_columns <- c(non_standard_columns, col)
  }
}

# Print the columns that have non-standard types
##Columns with non-standard types
print(non_standard_columns) #"NA

## get rid of duplicates
dup.ids5 = d5 %>% filter(duplicated(id)) %>% select(id) %>% unlist()
dup.ids5

length(dup.ids5)
# sanity check
## d5 %>% filter(id == dup.ids5[1])


########## get rid of duplicates
d6 <- d5 %>% distinct(id, .keep_all = TRUE)

dup.ids6 = d6 %>% filter(duplicated(id)) %>% select(id) %>% unlist()
dup.ids6
length(dup.ids6)



########### end of More unnesting/cleaning ###########

saveRDS(d6, file='rawdata/cve.rds')






