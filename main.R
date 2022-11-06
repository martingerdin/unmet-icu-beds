## Welcome!

## This is your project's main script file and together with
## manuscript.Rmd it provides and entry point for you and other people
## coming to the project. The code in this file should give an outline
## of the different steps conducted in your study, from importing data
## to producing results.

## This file should be relatively short, and most of the heavy
## lifting should be done by specialised functions. These functions
## live in the folder functions/ and you create a new function using
## create_function().

## Feel free to remove this introductory text as you get started.

## Source all functions (if you tick the box "Source on save" in
## RStudio functions will be automatically sourced when you save
## them). They all need to be sourced however when you compile your
## manuscript file or run this file as a job, as that happens in a
## clean R session.
#noacsr::source_all_functions()

setwd("C:/Users/maria/Downloads/Packt Learning RStudio for R Statistical Computing 2012 RETAIL eBook-repackb00k/unmet-ICU-beds")

#setwd("C:/Users/maria/Downloads/unmet-ICU-beds")

library(tidyverse)
library(dplyr)
library(tableone)

## Import data
headerproperties <- read.csv("headerpropertieswtimers.csv")
titcodata <- read.csv("titcodatawtimers.csv")
#jag har manuellt ändrat och städat lite i dessa så jag sparar dem så jag kan committa dem
save(headerproperties, file = "headerpropertieswtimers.RData")
save(titcodata, file = "titcodatawtimers.RData")
headerrows <- head(headerproperties)


## Whatever you do next, maybe clean data?

