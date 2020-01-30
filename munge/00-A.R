# Example preprocessing script.
# How Do I in R?                                                                https://tinyurl.com/y9j67lfk
# MCA - Multiple Correspondence Analysis                                        https://is.gd/HRVWgN
# checkpoint("2015-01-15") ## or any date in YYYY-MM-DD format after 2014-09-17 https://tinyurl.com/yddh54gn
################################################################################
## Step 00.00 Processing Start Time - start the timer                        ###
################################################################################
start.time = Sys.time()
################################################################################
## Step 00.01 create object table                                            ###
## Check existence of directory and create if doesn't exist                  ### https://tinyurl.com/y3adrqwa
dirCheck(mainDir, subDir)
################################################################################
## Step 00.02: clean dataframes with Janitor                                 ###
################################################################################
graduation[, `:=`(c(7, 8, 22, 50, 51, 52, 59, 68, 76, 77), 
            lapply(.SD,   as.factor)),
            .SDcols = c(7, 8, 22, 50, 51, 52, 59, 68, 76, 77)]
graduation <- graduation %>% mutate_if(is.character,as.factor)
graduation[ is.na(graduation) ] <- 0
graduation <- graduation %>% mutate_if(is.numeric,as.integer)
dt_graduation <- graduation[,c(7,8,22, 50,51, 52, 59, 68, 76, 77) ]
dt_graduation <- dt_graduation %>% mutate_if(is.character,as.factor)
################################################################################
## Step 00.99: VERSION HISTORY                                               ###
################################################################################
a00.version = "1.0.0"
a00.ModDate = as.Date("2020-01-27")
## -----------------------------------------------------------------------------
## 2019.01.01 - v.1.0.0
##  1st release
## -----------------------------------------------------------------------------
