### This is a script used to analyze US state spending on children
### Source: https://datacatalog.urban.org/dataset/state-state-spending-kids-dataset

# Script set up
require(tidyverse)
require(dplyr)
require(reshape)

# make sure to set your working directory to the folder that contains the csv referenced below.


# Load data file & set states as factor
stateChildSpending <- read.csv("ChildcareSpending_Metrics.csv")
stateChildSpending$State <- as.factor(stateChildSpending$State)

summary(stateChildSpending)

#identify any na values
sapply(stateChildSpending, function(x) sum(is.na(x)))

#explore select spending levels using bar charts. Captions from data dictionary
library(GGally)
ggpairs(stateChildSpending,
        columns = 2:22)

ggplot(stateChildSpending, aes(y=reorder(State, PK12ed_realperch), x=PK12ed_realperch))+
  theme_classic()+
  geom_col(fill='cadetblue4')+
  xlab("Public spending on elementary and secondary education per child ages 0-18 
       by state and year, adjusted for inflation using a GDP deflator. See PK12ed 
       description for source details.")+
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


#melt all raw inputs in to 1 long table
stateChildSpendingLong <- melt(stateChildSpending, id.vars = c("State"))

#explore data as histogram small multiples
ggplot(data = stateChildSpendingLong, aes(x = value)) + geom_histogram()+
  facet_wrap(~variable)

### calculate total spending by categories

#education spending per capita
stateChildSpending$education <- with(stateChildSpending,
                                     PK12ed_realperch+
                                      highered_realperch+
                                      edservs_realperch+
                                      pell_realperch+
                                      headstartpriv_realperch)

#financial assistance spending per capita  
stateChildSpending$financial <- with(stateChildSpending,
                                            othercashserv_realperch+
                                            TANFbasic_realperch)

#nutrition spending per capita  
stateChildSpending$nutrition <- stateChildSpending$SNAP_realperch

#social security spending per capita
stateChildSpending$soc_security <- with(stateChildSpending,
                                        socsec_realperch+
                                        fedSSI_realperch)

#tax credits spending per capita
stateChildSpending$tax <- with(stateChildSpending,
                              fedEITC_realperch+
                              CTC_realperch+
                              addCC_realperch+
                              stateEITC_realperch)

#healthcare spending per capita
stateChildSpending$health <- with(stateChildSpending,
                                  Medicaid_CHIP_realperch+
                                  pubheath_realperch+
                                  other_health_realperch)

#housing spending per capita
stateChildSpending$housing <- stateChildSpending$HCD_realperch

#public resources spending per capita
stateChildSpending$resources <- with(stateChildSpending,
                                    lib_realperch+
                                    parkrec_realperch)

#employment benefits spening per capita
stateChildSpending$employment <- with(stateChildSpending,
                                      wcomp_realperch+
                                      unemp_realperch)

#create new dataframe with totals
stateChildSpendingTotals <- data.frame(stateChildSpending$State,
                                  stateChildSpending$education,
                                  stateChildSpending$financial,
                                  stateChildSpending$nutrition,
                                  stateChildSpending$soc_security,
                                  stateChildSpending$tax,
                                  stateChildSpending$health, 
                                  stateChildSpending$housing,
                                  stateChildSpending$resources,
                                  stateChildSpending$employment)

#rename columns
names(stateChildSpendingTotals)[1] <- "state"
names(stateChildSpendingTotals)[2] <- "education"
names(stateChildSpendingTotals)[3] <- "financial"
names(stateChildSpendingTotals)[4] <- "nutrition"
names(stateChildSpendingTotals)[5] <- "soc_security"
names(stateChildSpendingTotals)[6] <- "tax"
names(stateChildSpendingTotals)[7] <- "health"
names(stateChildSpendingTotals)[8] <- "housing"
names(stateChildSpendingTotals)[9] <- "resources"
names(stateChildSpendingTotals)[10] <- "employment"

stateChildSpendingTotalsLong <- melt(stateChildSpendingTotals, id.vars = c("state"))

#total spending
ggplot(stateChildSpendingTotalsLong, aes(y=reorder(state, value), x=value))+
  theme_classic()+
  geom_col(fill='cadetblue4')+
  xlab("total spending per child ($)")+
  ylab("state")

#total spending per category
ggplot(stateChildSpendingTotalsLong, aes(y=reorder(state, value), x=value, fill = variable))+
  theme_classic()+
  geom_col()+
  xlab("total spending per child ($)")+
  ylab("state")+
  guides(fill=guide_legend("spending category"))

#percent of total spending
ggplot(stateChildSpendingTotalsLong, aes(y=reorder(state, value), x=value, fill = variable))+
  theme_classic()+
  geom_col(position = "fill")+
  xlab("% of total spending")+
  ylab("state")+
  guides(fill=guide_legend("spending category"))

#export results a csv to use in GIS
write.csv(stateChildSpendingTotals,"stateChildSpendingTotals.csv", row.names = FALSE)


