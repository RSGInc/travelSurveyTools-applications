library(data.table)
library(stringr)
library(travelSurveyTools)

### Load in Data --------
user = Sys.info()[['user']]


data_dir_2023 = str_glue("J:/Projects/Surveys/HHTravel/Survey2023/Data/data_unweighted_cleaned_to_rsg_020124")


files_2023 = Sys.glob(file.path(data_dir_2023, "*.csv"))

# load in data received from PSRC on 1/24

for (fl in files_2023) {
  tbl_name_2023 = stringr::str_match(
    basename(fl), 
    "(.*?).csv"
  )[, 2]
  message("Reading ", basename(fl))
  assign(tolower(paste0(tbl_name_2023)), fread(fl))
}

# rename hh table
hh = household

# load in new codebook from PSRC (PSRC change to their path)
cb_path = str_glue("J:/Projects/Surveys/HHTravel/Survey2023/Data/codebook/PSRC_Combined_Codebook_2023_groupings.xlsx")

variable_list = readxl::read_xlsx(cb_path, sheet = 'variable_list_2023')
value_labels = readxl::read_xlsx(cb_path, sheet = 'value_labels_2023')

setDT(variable_list)
setDT(value_labels)

# load in weights

weights_path_2023 = str_glue("J:/Projects/Surveys/HHTravel/Survey2023/Data/weighting/9_Weighting/9_Weighting/2_RSG_Outputs")



weight_files_2023 = Sys.glob(file.path(weights_path_2023, "*.csv"))

for (w in weight_files_2023) {
  weight_files_2023 = stringr::str_match(
    basename(w), 
    "(.*?).csv"
  )[, 2]
  message("Reading ", basename(w))
  assign(paste0(weight_files_2023), fread(w))
}

# drop old weights
hh[, hh_weight_2023 := NULL]
person[, person_weight := NULL]
day[, day_weight_2023 := NULL]
trip[, trip_weight := NULL]

# make all ids characters
hh[, hhid := as.character(hhid)]
hh_weights[, hh_id := as.character(hh_id)]

person[, person_id := as.character(person_id)]
person[, hhid := as.character(hhid)]
person_weights[, person_id := as.character(person_id)]

day[, day_id := as.character(day_id)]
day[, hhid := as.character(hhid)]
day[, person_id := as.character(person_id)]
day_weights[, day_id := as.character(day_id)]

trip[, tripid := as.character(tripid)]
trip[, hhid := as.character(hhid)]
trip[, person_id := as.character(person_id)]
trip[, day_id := as.character(day_id)]
trip_weights[, trip_id := as.character(trip_id)]

# attach new weights
hh[
  hh_weights,
  hh_weight := i.hh_weight,
  on = c('hhid' = 'hh_id')
]

person[
  person_weights,
  person_weight := i.person_weight,
  on = 'person_id'
]

day[
  day_weights,
  day_weight := i.day_weight,
  on = 'day_id'
]

trip[
  trip_weights,
  trip_weight := i.trip_weight,
  on = c('tripid' = 'trip_id')
]

#check the weights
stopifnot(sum(hh$hh_weight, na.rm = T) == sum(hh_weights$hh_weight))
stopifnot(sum(person$person_weight, na.rm = T) == sum(person_weights$person_weight))
stopifnot(sum(day$day_weight, na.rm = T) == sum(day_weights$day_weight))
stopifnot(sum(trip$trip_weight, na.rm = T) == sum(trip_weights$trip_weight))


hh = hh[!is.na(hh_weight)]
person = person[hhid %in% hh$hhid]
day = day[hhid %in% hh$hhid]
trip = trip[hhid %in% hh$hhid]
vehicle = vehicle[hhid %in% hh$hhid]

### Data Updates -------

# make hts_data a list
hts_data = list(hh = hh,
                person = person,
                day = day,
                trip = trip,
                vehicle = vehicle)

# codebook updates

setnames(variable_list, c('psrc_variable', 'hh_23', 'per_23', 'veh_23', 'day_23', 'trip_23', 'location_final', 'description_2023', 'data_type_2023'),
         c('variable', 'hh', 'person', 'vehicle', 'day', 'trip', 'location', 'description', 'data_type'),
         skip_absent=TRUE)

setnames(value_labels, c('final_label', 'psrc_variable', 'psrc_value'), c('label', 'variable', 'value'),
         skip_absent=TRUE)

value_labels[, val_order := seq_len(nrow(value_labels))]

variable_list[, shared_name := ifelse(
  grepl('--', description),
  sub('_[^_]*$', '', variable), variable)
]

variable_list[, is_checkbox := ifelse(grepl('--', description), 1, 0)]

variable_list = variable_list[!is.na(hh) | !is.na(person) | !is.na(day) | !is.na(trip) | !is.na(vehicle) | location != 0]

### Example data summaries-----

### Summarize age by gender-----
prepped_dt = hts_prep_variable(summarize_var = 'age',
                               summarize_by = 'gender',
                               data = hts_data,
                               id_cols = c('hhid', 'person_id', 'day_id', 'tripid', 'vehicle_id'),
                               wt_cols = c('hh_weight', 'person_weight', 'day_weight', 'trip_weight', 'hh_weight'),
                               weighted = TRUE)

hts_summary(prepped_dt = prepped_dt$cat,
            summarize_var = 'age',
            summarize_by = 'gender',
            id_cols = c('hhid', 'person_id', 'day_id', 'tripid', 'vehicle_id'),
            wtname = 'person_weight',
            weighted = TRUE)

### Summarize delivery----
prepped_dt = hts_prep_variable(summarize_var = 'deliver',
                               data = hts_data,
                               id_cols = c('hhid', 'person_id', 'day_id', 'tripid', 'vehicle_id'),
                               wt_cols = c('hh_weight', 'person_weight', 'day_weight', 'trip_weight', 'hh_weight'),
                               weighted = TRUE)

hts_summary(prepped_dt = prepped_dt$cat,
            summarize_var = 'deliver',
            id_cols = c('hhid', 'person_id', 'day_id', 'tripid', 'vehicle_id'),
            wtname = 'day_weight',
            weighted = TRUE,
            checkbox_valname = 'value',
            checkbox_yesval = 1,
            summarize_vartype = 'checkbox')

### Summarize trip rate by income----
prepped_dt = hts_prep_triprate(summarize_by = 'hhincome_broad',
                               hts_data = hts_data,
                               ids = c('hhid', 'person_id', 'day_id', 'tripid', 'vehicle_id'),
                               wts = c('hh_weight', 'person_weight', 'day_weight', 'trip_weight', 'hh_weight'),
                               weighted = TRUE)

hts_summary(prepped_dt = prepped_dt$num,
            summarize_var = 'num_trips_wtd',
            summarize_by = 'hhincome_broad',
            id_cols = c('hhid', 'person_id', 'day_id', 'tripid', 'vehicle_id'),
            wtname = 'day_weight',
            weighted = TRUE,
            summarize_vartype = 'numeric')
