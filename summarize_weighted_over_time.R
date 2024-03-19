library(data.table)
library(stringr)
library(travelSurveyTools)
library(psrcelmer)
library(dplyr)
library(psrcplot)
library(tidyverse)

# testing a local version of the package so I can debug
devtools::load_all('C:/GitHub/travelSurveyTools/R')
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
# 
# # mode_simple is an aggregation of mode_1; we need to add it to the value_labels
# # as it's own items
# 
# # it already exists on the variables table somehow but not the values table;
# 
# 
  variable_list<-rbind(
    variable_list,
    data.table(
      variable = "mode_simple",
      is_checkbox = 0,
      hh = 0,
      person = 0,
      day = 0,
      trip = 1,
      vehicle = 0,
      location = 0,
      description = "mode aggregation",
      logic = 'mode aggregation',
      data_type = "integer/categorical",
      shared_name = "mode_simple"
    )
  )
# 
# 
mode_simple_labels = value_labels[group_1_title == 'mode_simple',
                                   c('label', 'group_1_value')]
# # 
# # 
# # # add mode_simple to the trip table with the correct label
trip <-merge(trip, mode_simple_labels, by.x = 'mode_1', by.y = 'label')
setnames(trip, 'group_1_value', 'mode_simple')
# # 
# # 
# # # only select the necessary columns
  value_labels<-value_labels%>%select(variable, value, label, val_order)
# # 
  mode_simple_value_labels<-mode_simple_labels%>%
                            mutate(variable='mode_simple')%>%
                            distinct(group_1_value, .keep_all=TRUE)%>%
                            rowid_to_column(var='value')%>%
                           rowid_to_column(var='val_order')%>%
                             select(variable, value, group_1_value, val_order)%>%
                           rename(label=group_1_value)


 
value_labels<-rbind(value_labels,
                     data.table(mode_simple_value_labels))


### Data Updates -------

# make hts_data a list
hts_data = list(#hh = hh,
                #person = person,
                #day = day,
                trip = trip)


variable_list<-setDT(variable_list)
value_labels<-setDT(value_labels)
value_labels[, val_order := seq_len(nrow(value_labels))]


devtools::load_all('C:/GitHub/travelSurveyTools/R')
### Example data summaries-----
### Use package for summary -----
prepped_dt = hts_prep_variable(summarize_var = 'mode_simple',
                               summarize_by = 'survey_year',
                               data = hts_data,
                               id_cols = 'trip_id',
                               wt_cols = 'trip_weight',
                               weighted = TRUE,
                               missing_values = '')

mode_summary<-hts_summary(prepped_dt = prepped_dt$cat,
                          summarize_var = 'mode_simple',
                          summarize_by = 'survey_year',
                          id_cols = 'trip_id',
                          wtname='trip_weight',
                          weighted = TRUE)


common_modes<-mode_summary$summary$wtd%>%filter(prop>.005)
static_bar_chart(common_modes, y='mode_simple', x='prop', fill='survey_year')



