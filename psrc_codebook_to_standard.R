library(data.table)
library(stringr)
library(travelSurveyTools)
library(dplyr)


# read in variables and values
variable_list = readxl::read_xlsx(str_glue('J:/Projects/Surveys/HHTravel/Survey2023/Data/codebook/PSRC_Combined_Codebook_2023_groupings.xlsx'),
                                  sheet = 'variable_list_2023')
value_labels = readxl::read_xlsx(str_glue('J:/Projects/Surveys/HHTravel/Survey2023/Data/codebook/PSRC_Combined_Codebook_2023_groupings.xlsx'),
                                 sheet = 'value_labels_2023')
setDT(variable_list)
setDT(value_labels)


