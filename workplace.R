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
person<- get_query(sql= "select household_id as hh_id,
person_id, workplace, survey_year,  person_weight 
                from HHSurvey.v_persons_labels")%>%drop_na()%>%
  filter(workplace !='Missing: Skip Logic')%>%
  filter(survey_year != '2017') # question options were different


#setDT(hh)
#setDT(person)
#setDT(day)
setDT(person)


# load in codebook
cb_path = str_glue("J:/Projects/Surveys/HHTravel/Survey2023/Data/codebook/PSRC_Combined_Codebook_2023_packagable.xlsx")

variable_list = readxl::read_xlsx(cb_path, sheet = 'variable_list')
value_labels = readxl::read_xlsx(cb_path, sheet = 'value_labels')

setDT(variable_list)
setDT(value_labels)





### Data Updates -------

# make hts_data a list
hts_data = list(#hh = hh,
                #person = person,
                #day = day,
                person = person)


variable_list<-setDT(variable_list)
value_labels<-setDT(value_labels)
value_labels[, val_order := seq_len(nrow(value_labels))]



### Example data summaries-----
### Use package for summary -----
prepped_dt = hts_prep_variable(summarize_var = 'workplace',
                               summarize_by = 'survey_year',
                               data = hts_data,
                               id_cols = 'person_id',
                               wt_cols = 'person_weight',
                               weighted = TRUE,
                               missing_values = '')

work_summary<-hts_summary(prepped_dt = prepped_dt$cat,
                          summarize_var = 'workplace',
                          summarize_by = 'survey_year',
                          id_cols = 'person_id',
                          wtname='person_weight',
                          weighted = TRUE)


workplace_summary<-work_summary$summary$wtd%>%mutate(survey_factor=as.factor(survey_year))%>%filter(workplace != 'Missing Skip Logic')
static_bar_chart(workplace_summary, y='workplace', x='prop', fill='survey_factor')



