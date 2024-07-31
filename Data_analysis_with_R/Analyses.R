
# Setup --------------------------------------------------------------------------------------------
library(dplyr)
library(tidyr)
library(RJSONIO)
library(magrittr)
library(lmerTest)
library(data.table)
library(reshape)
library(gdata)
library(ggplot2)
library(rcicr) 
library(effectsize)
library(BayesFactor)
library(openxlsx)

# Ensure that relative paths start from the same directory as this script
rstudioapi::getActiveDocumentContext()$path %>% dirname %>% setwd

# Some local packages created by Mathias Schmitz
source("R_Schmitz/stats.R")
source("R_Schmitz/RC.R")
source("R_Schmitz/dataWranglingCleaning.R")

# Read RC data -------------------------------------------------------------------------------
# Data extracted from the Firebase repository (json format)
json <- fromJSON("Data_Firebase.json")

RC <- lapply(names(json), function(id){ fread(json[[id]][["data"]], fill=TRUE) }) %>% rbindlist

# Clean RC data ---------------------------------------------------------------------------------------

# Setting response time as a numeric variable
RC$rt <- as.numeric(RC$rt)
RC[, rt := abs(rt)] # on very rare occasions some browsers/version return a negative rt

# We rename stimuli and responses with a clearer signification
RC[, stimuli  := gsub('.*faceInv(.*)\\.png.*','\\1', selectedImg)]
RC[, stimuli  := gsub('.*faceOri(.*)\\.png.*','\\1', stimuli)]

# We create the response and trial number variables
RC[, response := ifelse(selectedImg %like% "Ori", +1, -1)]
RC[, trialNum := rep(1:150, 1), id] # 1 to 150 trial (per 1 time if one block)

# Remove useless columns for RC and numerify the DF
RC <- RC[, .(id, stimuli, response, rt, trialNum, gender, age, language)]
numerify(RC)

# we rename the id column
RC$faceId <- RC$id
RC$id <- NULL

# Participants exclusion ---------------------------------------------------------------------------------------
# We typically subset pp who responded too fast in the RC procedure
RCdescrp <- RC[, .(Med200 = mean(rt < 200) %>% round(2)), faceId]

setorder(RCdescrp, -Med200)
head(RCdescrp, 50)

RC <- RC[faceId %in% RCdescrp[Med200 < .3, faceId], ] # xxx pp

# number of participants
length(unique(RC$faceId)) # N = xxx

#-----------------------------------------------------------
#               to compute condition-level CIs
#-----------------------------------------------------------

# Constant
constant_avg <- .04 # typically, I use .004 (if enough participants)

# Creating the condition-level CI (with Dotsch's package)
avgCImg <- generateCI2IFC(
  stimuli   = RC$stimuli,
  responses = RC$response,
  baseimage = "avg",
  scaling   = "constant",
  constant  = constant_avg,
  rdata     = "rcic_seed_1234_time_Jun_03_2024_09_31.Rdata", # seed automatically created when creating the noisy faces
  antiCI    = FALSE,
  filename  = "condition_CI"
)

#-----------------------------------------------------------
#                 to compute subgroup-level CIs
#-----------------------------------------------------------
# Parameters for the subgroup CIs
set.seed(1990) # To have reproducible subgroup CI 
constant_sbgp <- .012 # typically, I use .012 (when 10 individual per CI)
n <- 10 # number of randomly selected faceIds per CI

# Initialize counter for new IDs
new_id <- 1
N <- 50  # total number of subgroup CIs

# Creating the subgroup CIs for each cell
for (i in 1:N) {
  ids <- sample(unique(RC$faceId), n) # same Id as in RC_neg
  
  RC_sub <- RC[faceId %in% ids]
  
  avgCImg <- generateCI2IFC(
    stimuli   = RC_sub$stimuli,
    responses = RC_sub$response,
    baseimage = "avg",
    scaling   = "constant",
    constant  = constant_sbgp,
    rdata     = "rcic_seed_1234_time_Jun_03_2024_09_31.Rdata",
    antiCI    = FALSE,
    filename  = paste0(new_id, 
                       "_sbgp_CI")
  )
  
  # increment counter after each loop
  new_id <- new_id + 1
}

#___________________________
#
# Demographics   -----------------------------------------------------
#___________________________
DF_demo <- RC[, .(age =  unique(age),
                  gender = unique(gender), 
                  language = unique(language)), faceId]

# age
mean(DF_demo$age)
sd(DF_demo$age)

# gender
table(DF_demo$gender)

# language skills
table(DF_demo$language)



