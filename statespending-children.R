### This is a script used to analyze US state spending on children
### Source: https://datacatalog.urban.org/dataset/state-state-spending-kids-dataset

# Script set up
require(tidyverse)
require(dplyr)
require(GGally)

setwd("~/Documents/School/MICA/Design Lab/Final Project/Data")

# Load data file & set states as factor
stateChildSpending <- read.csv("ChildcareSpending_Metrics.csv")
stateChildSpending$State <- as.factor(stateChildSpending$State)

#identify any na values
sapply(stateChildSpending, function(x) sum(is.na(x)))

#explore spending levels
ggpairs(stateChildSpending,
        columns = 2:22)

ggplot(stateChildSpending, aes(y=reorder(State, PK12ed_realperch), x=PK12ed_realperch))+
  theme_classic()+
  geom_col(fill='cadetblue4')+
  xlab("Public spending on elementary and secondary education per child ages 0-18 
       by state and year, adjusted for inflation using a GDP deflator. See PK12ed 
       description for source details.")+
  ylab("State")

ggplot(stateChildSpending, aes(y=reorder(State, pell_realperch), x=pell_realperch))+
  theme_classic()+
  geom_col(fill='cadetblue4')+
  xlab("Public spending on Pell Grants per child ages 0-18 by state and year, 
       adjusted for inflation using a GDP deflator. See pell description for source details. ")+
  ylab("State")

ggplot(stateChildSpending, aes(y=reorder(State, headstartpriv_realperch), x=headstartpriv_realperch))+
  theme_classic()+
  geom_col(fill='cadetblue4')+
  xlab("Federal spending on Head Start awarded to private grantees per child ages 
       0-18 by state and year, adjusted for inflation using a GDP deflator. See 
       HeadStart description for source details.")+
  ylab("State")

ggplot(stateChildSpending, aes(y=reorder(State, SNAP_realperch), x=SNAP_realperch))+
  theme_classic()+
  geom_col(fill='cadetblue4')+
  xlab("Public spending on SNAP benefit payments that go to children by state and
       year per child ages 0-18, adjusted for inflation using a GDP deflator. See 
       SNAP description for source details.")+
  ylab("State")

ggplot(stateChildSpending, aes(y=reorder(State, Medicaid_CHIP_realperch), x=Medicaid_CHIP_realperch))+
  theme_classic()+
  geom_col(fill='cadetblue4')+
  xlab("Public spending on Medicaid for children and CHIP per child ages 0-18 by
        state and year, adjusted for inflation using a GDP deflator. 
       See Medicaid_CHIP description for source details. Data are missing for 1997.")+
  ylab("State")

ggplot(stateChildSpending, aes(y=reorder(State, parkrec_realperch), x=parkrec_realperch))+
  theme_classic()+
  geom_col(fill='cadetblue4')+
  xlab("Public spending on parks and recreation per child ages 0-18 by state and year, 
       adjusted for inflation using a GDP deflator. See parkrec description for source details.")+
  ylab("State")

ggplot(stateChildSpending, aes(y=reorder(State, stateEITC_realperch), x=stateEITC_realperch))+
  theme_classic()+
  geom_col(fill='cadetblue4')+
  xlab("Total state spending on EITC per child ages 0-18 by state and year, adjusted 
       for inflation using a GDP deflator. See stateEITC description for source details.")+
  ylab("State")
