require(dplyr)
require(xtable)
############################################################
############################################################
##############   1: entry and exit   #######################
############################################################
############################################################

#i will consider firms exited after their first bankrupcy event
#firms who leave due to reasons other than bankruptcy will be excluded
  #because we cannot confirm whether their 
  #exit was due to finacial hardships or not

#set exit based on delete date
#consider exit only if reported bankruptcy or liquidation
#either if listed as reason for exit or if appears on
#requires at_ft (footnotes of total assets)

exit_vars <- function(data) {

  #first we will code all exits from data
  #will correct for exits not due to bankruptcy later
  data <- data %>% #nolint
    group_by(GVKEY) %>% #nolint
    mutate(
      exit = max(fyear)#nolint
    )

  # Sort data by GVKEY and fyear (allows use of lead in next step)
  data <- data %>%
    arrange(GVKEY, fyear) #nolint

  #now code indicaor for exit
  data <- data %>% #nolint
    group_by(GVKEY) %>% #nolint
    mutate(
      exit_flag = ifelse(
                         dlrsn %in% c(2, 3) & (exit == fyear) |  #nolint
                         at_fn == "TL" |  #nolint
                         dplyr::lead(at_fn) == "AG", 1, 0) #nolint
    )

  #if left data without exit flag set exit to NA
  #we cannot confirm whether or not this is a true exit
  #(may be a non-bankruptcy merger, may be a merger due to impeding bankruptcy)
  data <- data %>%
    mutate(
      exit_flag = ifelse(exit == fyear & exit_flag == 0, NA, exit_flag) #nolint
    )

  #now correct exit year based on flag
  #if multiple bankruptcies recorded set exit to first year of exit
  data <- data %>%
    group_by(GVKEY) %>% #nolint
    mutate(
      exit = ifelse(any(is.na(exit_flag)), NA, exit) #nolint
    ) %>%
    mutate(
      exit = ifelse(!is.na(exit_flag) & is.na(exit) & any(exit_flag == 1),
          min(fyear[which(exit_flag == 1)]), #nolint
          exit) #nolint #nolint
    ) %>%
    mutate(
      life = ifelse(exit - fyear >= 0, exit - fyear, NA)  #nolint
    )

  data
}


# entry
entry_vars <- function(data) { #nolint

  ############################################################
  ###1:
  #make sure not first year in data (then cant confirm entry)
  min_year <- min(data$fyear, na.rm = TRUE)

  tempdata <- data %>%
    group_by(GVKEY) %>% #nolint
    mutate(
      entry = min(fyear), #nolint
      age = ifelse(entry == min_year, NA, fyear - entry), #nolint
    )

  tempdata
}