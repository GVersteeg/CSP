## Script for quiz 1 (week 1) for capstone project
## Author: Gerrit Versteeg
## Last saved: 29-10-2017
##
##-----------------------------------------------------------------------------
##---------------- PART 1. GETTING THE DATA -----------------------------------
##
##---- step 0. Loading relevant packages
library("tm")
library("dplyr")
library("magrittr")
myLoc <- "./raw_txt"
##
##---- Q2: lines in twitter -----------------------
con <- file("./raw_txt/en_US.twitter.txt", "r")
v <- readLines(con)
length(v)
close(con)
##---- Q3: longest line -----------------------
con <- file("./raw_txt/en_US.twitter.txt", "r")
v <- readLines(con)
close(con)
xt <- 0
for (i in seq_along(v)) {
        if (nchar(v[i])>xt) {
                xt <- nchar(v[i])
        }
}
rm(v)
con <- file("./raw_txt/en_US.news.txt", "r")
v <- readLines(con)
close(con)
xn <- 0
for (i in seq_along(v)) {
        if (nchar(v[i])>xn) {
                xn <- nchar(v[i])
        }
}
rm(v)
con <- file("./raw_txt/en_US.blogs.txt", "r")
v <- readLines(con)
close(con)
xb <- 0
for (i in seq_along(v)) {
        if (nchar(v[i])>xb) {
                xb <- nchar(v[i])
        }
}
rm(v)
##---- Q4 Twitter: #lines "love" / # lines "hate" -----------------------
con <- file("./raw_txt/en_US.twitter.txt", "r")
v <- readLines(con)
close(con)
library(stringr)
xl <- 0
xh <- 0
for (i in seq_along(v)) {
        if (str_detect(v[i],"love")) {
                xl <- xl +1
        }
        if (str_detect(v[i],"hate")) {
                xh <- xh +1
        }
}
answer <- xl/xh
##---- Q5 Twitter: print lines containing biostats -----------------------
library(stringr)
pattern <- "A computer once beat me at chess, but it was no match for me at kickboxing"
xp <- 0
for (i in seq_along(v)) {
        if (str_detect(v[i],pattern)) {
                xp <- xp + 1
        }
}
xp
rm(v)
##
##-----------------------------------------------------------------------------
## End of script
##-----------------------------------------------------------------------------

