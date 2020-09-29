
# I use pacman for package management, but you can do the regular install.packages and library/require
require(pacman)
pacman::p_load(tidyverse, readr, groupdata2, anticlust) # in the order that they are used


# simulated data I trained on before getting a better one
data_sim <- tibble(id = seq(1, 144, 1),
                   faculty = c(rep("Arts", 45), rep("BSS", 64), rep("STH", 35)),
                   dominant = sample(1:4, 144, replace = T))

# not gonna be useful, I'll simulate some based on the few entries Merethe sent me


#part data


 data_part <- read.csv2("C:/Users/Jedrek/Documents/R_random_stuff/test_participants.csv") # dataset from merethe (same structure as the final one but fewer participants)
 setwd("C:/Users/Jedrek/Documents/Github/Grouping-participants")
 # probably not GDPR friendly so I will censor it a bit and then make it available for download online
 colnames(data_part)[1] <- "first.name" # first col had bogus name, needed changing
 data_part <- data_part %>% select(-c(Mobile.phone,Last.name,Email,CV,Motivated.application, first.name))
 write.csv(data_part, file = "test_participants_censored.csv") # save it
# set up download from github
csv <- "https://raw.githubusercontent.com/jedrekmiecznikowski/Grouping-participants/main/test_participants_censored.csv"

data_part <- read_csv(csv)

# ok now we're off to the races

View(data_part) # looksie

str(data_part) # looksie into structure

data_part <- data_part[rep(seq_len(nrow(data_part)), each = 5), ] # everything 5 times because going to work on larger dataframe

data_part$Faculty <- as.factor(data_part$Faculty) # gotta change that to factor

rownames(data_part) <- seq(1,135,1) # replicate creates messy rownames, clean it up

## OBS! ALGORITHM WORKS FOR K FOLDS WHERE K_MAX IS MIN NUMBER OF STUDENTS FROM SMALLEST FACULTY!!!! ##

## probably going to have to join ST back together for that reason

# first count
nrow(data_part %>% filter(Faculty=="Natural Sciences" | Faculty == "Technical Sciences"))
# 20 groups might be good enough and that way I'll test if the sizes are equal (crossing fingers)

## something to ask: is health still a thing?? # apparently it is (checked sign up page)

# new col with faculties health, technical and natural sciences together
data_part <- data_part %>% mutate("Faculty_grouped" = ifelse(Faculty=="Natural Sciences" | Faculty == "Technical Sciences" | Faculty == "Health", "AaaaNatur|Technic|Health", as.character(Faculty)),
                                  "Faculty_grouped2" = ifelse(Faculty=="Natural Sciences" | Faculty == "Technical Sciences" | Faculty == "Health" | Faculty == "Arts", "Arts|Natur|Technic|Health", as.character(Faculty)))
# did it work
plot(as.factor(data_part$Faculty_grouped))
plot(as.factor(data_part$Faculty_grouped2))
# arrange and then take a nice round number 
data_part <- data_part %>% arrange(Faculty_grouped)
set.seed(123) # for reproducibility
# folding twice because once I put all the small faculties together (everything other than bss) and once I leave arts stand on its own
# obv its better to randomize with arts on its own but algorithm might break, therefore test both
# i use first 80 observations only because in my fake dataset Natural and Technical give 20
data_folded <-  fold(data_part[1:80,], k = 4, method = "greedy", cat_col = "Faculty_grouped")
data_folded2 <-  fold(data_part[1:80,], k = 4, method = "greedy", cat_col = "Faculty_grouped2")

# different methods, I use greedy, where each fold grabs as many elements as possible (up to the specified size), meaning that there might be less elements available to the last group, but that all other groups than the last are guaranteed to have the size specified.

# from now on I will use fold and group interchangeably

# see if it worked

# arts alone
GROUPED <- data_folded %>%
  group_by(.folds) %>%
  summarise(n = n())
plot(GROUPED)

# arts blended in
GROUPED2 <- data_folded2 %>%
  group_by(.folds) %>%
  summarise(n = n())
plot(GROUPED2)
# yes!
# arrange back by folds for easy reading
data_folded <- data_folded %>% arrange(.folds)

# look at a random fold 
data_folded %>% select(Faculty_grouped, .folds) %>% filter(.folds == 12)


## ok, we have problems here but maybe I can cluster it now to balance the groups xd

# anticlust is a random package for creating balanced clusters
# need to make .folds numeric for clustering to work
data_folded2$.folds <- as.numeric(data_folded2$.folds)
# cluster into a vector
bc <- balanced_clustering(data_folded2 %>% select(.folds), 20)
# add new column to the data set
data_folded2$NEW <- bc
# check even groups visually
plot(as.factor(data_folded2$NEW))
# yay that works and groups are even

# now for clarity I will append the dropped observations (if it doesn't div by 4) to the frame

