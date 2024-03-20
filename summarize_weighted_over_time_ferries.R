library(data.table)
library(stringr)
library(travelSurveyTools)
library(psrcelmer)
library(dplyr)
library(psrcplot)
library(tidyverse)

### Load in Data --------

#hh<- get_table(schema= 'HHSurvey', tbl_name='v_households_labels')
#person<- get_table(schema= 'HHSurvey', tbl_name='v_persons_labels')
#day<- get_table(schema= 'HHSurvey', tbl_name='v_days_labels')
# only get the columns needed for the analysis to reduce table size
trip<- get_query(sql= "select trip_id, household_id as hh_id, day_id,
person_id, mode_1, survey_year,  trip_weight 
                from HHSurvey.v_trips_labels")


#setDT(hh)
#setDT(person)
#setDT(day)
setDT(trip)


# load in codebook
cb_path = str_glue("J:/Projects/Surveys/HHTravel/Survey2023/Data/codebook/PSRC_Combined_Codebook_2023_packagable.xlsx")

variable_list = readxl::read_xlsx(cb_path, sheet = 'variable_list')
value_labels = readxl::read_xlsx(cb_path, sheet = 'value_labels')

setDT(variable_list)
setDT(value_labels)



# make all ids characters
#hh[, hhid := as.character(household_id)]


#person[, person_id := as.character(person_id)]
#person[, hhid := as.character(household_id)]


#day[, day_id := as.character(day_id)]
#day[, hhid := as.character(household_id)]
#day[, person_id := as.character(person_id)]


trip[, trip_id := as.character(trip_id)]
trip[, hh_id := as.character(hh_id)]
trip[, person_id := as.character(person_id)]
trip[, day_id := as.character(day_id)]



#hh = hh[!is.na(hh_weight)]
#person = person[hhid %in% hh$hhid]
#day = day[hhid %in% hh$hhid]
#trip = trip[hhid %in% hh$hhid]
trip <- trip%>%mutate(survey_year=as.character(survey_year))


ferry_trip<-trip%>%filter(mode_1 %in% c('Ferry or water taxi', 'Vehicle ferry (took vehicle on board)'))

### Data Updates -------

# make hts_data a list
hts_data = list(#hh = hh,
                #person = person,
                #day = day,
                trip = ferry_trip)


variable_list<-setDT(variable_list)
value_labels<-setDT(value_labels)
value_labels[, val_order := seq_len(nrow(value_labels))]


### Example data summaries-----
### Use package for summary -----
prepped_dt = hts_prep_variable(summarize_var = 'mode_1',
                               summarize_by = 'survey_year',
                               data = hts_data,
                               id_cols = 'trip_id',
                               wt_cols = 'trip_weight',
                               weighted = TRUE,
                               missing_values = '')

mode_summary<-hts_summary(prepped_dt = prepped_dt$cat,
                          summarize_var = 'mode_1',
                          summarize_by = 'survey_year',
                          id_cols = 'trip_id',
                          wtname='trip_weight',
                          weighted = TRUE)


ferry<-mode_summary$summary$wtd
static_bar_chart(ferry, y='mode_1', x='count', fill='survey_year')


static_bar_chart(ferry, y='mode_1', x='est', fill='survey_year')
