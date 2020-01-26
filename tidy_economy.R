# Look at the economy

library(tidyverse)
library(readxl)
library(lubridate)

read_account_transactions <- function(filename, account_name) {
  account <- read_excel(filename) %>% 
    dplyr::select(1, 3:6) %>% 
    dplyr::rename(dato = 1,
                  kode = 2,
                  beskrivelse = 3,
                  ut = 4,
                  inn = 5) %>% 
    dplyr::mutate(konto = account_name,
                  dato = dmy(dato))
}

bruks <- read_account_transactions("bruks_2019.xls", "bruks")
spare <-  read_account_transactions("spare_2019.xls", "spare")
hoyrente <- read_account_transactions("hoyrente_2019.xls", "hoyrente")

all_account_transactions <- dplyr::bind_rows(bruks, spare, hoyrente) %>% 
  dplyr::mutate(maaned = month(dato))

monthly_economy <- all_account_transactions %>% 
  dplyr::group_by(maaned) %>% 
  dplyr::summarise(inn = round(sum(inn, na.rm = T), digits = 0),
                   ut = round(sum(ut, na.rm = T), digits = 0),
                   diff = inn + ut)

