# Look at the economy

library(tidyverse)
library(readxl)
library(lubridate)

read_account_transactions <- function(filename) {
  account <- read_excel(filename) %>% 
    dplyr::select(1, 3:6) %>% 
    dplyr::rename(dato = 1,
                  kode = 2,
                  beskrivelse = 3,
                  ut = 4,
                  inn = 5) %>% 
    dplyr::mutate(dato = dmy(dato))
}

account_files <- list.files(pattern = "\\.xls$")

all_account_transactions <- purrr::map_dfr(account_files, 
                                           read_account_transactions) %>% 
  dplyr::mutate(maaned = month(dato),
                aar = year(dato))

economy_balance <- all_account_transactions %>% 
  dplyr::group_by(aar, maaned) %>% 
  dplyr::summarise(inn = round(sum(inn, na.rm = T), digits = 0),
                   ut = round(sum(ut, na.rm = T), digits = 0),
                   diff = inn + ut)

