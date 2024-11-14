library(tmap)
library(sf)
library(tidyverse)
df2 <- tibble(planning_area = '',
              total_population = 0,
              below_1000 = 0,
              below_10999 = 0,
              below_11999 = 0,
              above_12000 = 0,
              below_1499 = 0,
              below_1999_above1500 = 0,
              below_2499 = 0,
              below_2999_above_2500 = 0,
              below_3999 = 0,
              below_4999 = 0,
              below_5999 = 0,
              above_6000 = 0,
              below_6999 = 0,
              below_7999 = 0,
              above_8000 = 0,
              below_8999 = 0,
              below_9999 = 0,
              below_1999_above_1000 = 0,
              below_2999_above_2000 = 0,
              below_14999 = 0,
              above_15000 = 0,
              year = 0
)[0,]

df2
for (region in area_names) {
  base_url_onemap = paste0("https://www.onemap.gov.sg/api/public/popapi/getIncomeFromWork?planningArea=",region,"&year=2020")
  
  req_obj <- request(base_url_onemap)
  req_header <- req_obj |>
    req_headers(Authorization = access_token_onemap)
  
  response <- req_header |>
    req_perform()
  
  results_onemap <- response |>
    resp_body_json()
  df2 <- df2 |>
    add_row(
      tibble_row(
        planning_area = results_onemap[[1]]$planning_area,
        total_population = results_onemap[[1]]$total,
        below_1000 = results_onemap[[1]]$below_sgd_1000,
        below_10999 = results_onemap[[1]]$sgd_10000_to_10999,
        below_11999 = results_onemap[[1]]$sgd_11000_to_11999,
        above_12000 = results_onemap[[1]]$sgd_12000_over,
        below_1499 = results_onemap[[1]]$sgd_1000_to_1499,
        below_1999_above1500 =  results_onemap[[1]]$sgd_1500_to_1999,
        below_2499 = results_onemap[[1]]$sgd_2000_to_2499,
        below_2999_above_2500 = results_onemap[[1]]$sgd_2500_to_2999,
        below_3999 = results_onemap[[1]]$sgd_3000_to_3999,
        below_4999 = results_onemap[[1]]$sgd_4000_to_4999,
        below_5999 = results_onemap[[1]]$sgd_5000_to_5999,
        above_6000 = results_onemap[[1]]$sgd_6000_over,
        below_6999 = results_onemap[[1]]$sgd_6000_to_6999,
        below_7999 = results_onemap[[1]]$sgd_7000_to_7999,
        above_8000 = results_onemap[[1]]$sgd_8000_over,
        below_8999 = results_onemap[[1]]$sgd_8000_to_8999,
        below_9999 = results_onemap[[1]]$sgd_9000_to_9999,
        below_1999_above_1000 = results_onemap[[1]]$sgd_1000_to_1999,
        below_2999_above_2000 = results_onemap[[1]]$sgd_2000_to_2999,
        below_14999 = results_onemap[[1]]$gd_12000_14999,
        above_15000 = results_onemap[[1]]$sgd_15000_over,  
        year = results_onemap[[1]]$year
      ))
}

df2$planning_area <- str_to_upper(df2$planning_area)

sf_data 
clean_income_geometry <- sf_data |>
  left_join(df2, by = c('pln_area_n'='planning_area'))  

tmap_options(check.and.fix=TRUE)
tm_shape(clean_income_geometry) +
  tm_polygons(col='below_2999_above_2000',
              palette = 'Blues')
colnames(df2)
names(results_onemap[[1]])

dim(df2)
as_tibble(results_onemap[[1]])
print(response.text)

area_names <- str_replace(area_names, '%20', ' ') 

tibble(area_names = area_names) |>
  arrange(area_names) |>
  pull(area_names)
