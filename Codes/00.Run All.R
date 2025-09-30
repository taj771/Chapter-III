
# Chapter III - Public survey pilot data analysis

library(pacman)
library(tidyverse)
library(apollo)
library(car)
library(haven)

## create supoortig data files to set the choice dataset for Apollo
source("./codes/01.Creating Supporting Dataframe.R")

## This code is aim to make consistent name patterns across choices.
source("./codes/02.Create Consistent ColNames for Choices.R")

## Script to create respondent characteristics (responses to non-valuation questions)
source("./codes/03.Respondent Characteristics.R")

## This code aims to clean the survey data and set the data ready for Appol0
source("./codes/04.Create Choice Data Set (Apollo).R")

## Model 1
source("./codes/05.MNL Model 1.R")

## Model 2
source("./codes/06.MNL Model 2.R")

## Model 3
source("./codes/07.MNL Model 3.R")

## Model 4
source("./codes/08.MNL Model 4.R")

## Model 5
source("./codes/09.MNL Model 5.R")

## Exploratory summaries/visualizations from pilot survey data
rmarkdown::render("./codes/10.Pilot Survey Summaries.qmd")

## Model summaries
rmarkdown::render("./codes/11. Preliminary Works - Pilot Data.qmd")

