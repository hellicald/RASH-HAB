#### Description ####
# Identify events in time series by creating a new column with events distinguished by numbers
# An event is defined as a time period where some values exceed a given value (threshold)

#### Arguments ####
# df: a data frame 
# date: character, the name of the date column which has to be in date format
# value: character, the name of the values column which has to be in numeric format without missing values (NA, NaN, NULL)
# group: character, the name of the grouping column
# thresh: threshold value(s) to identify events, either a numeric value or a column name (name in character, column in numeric) if different threshold are used between groups
# n: numeric, minimum number of data points for an event to be considered as such
# daysjoin: numeric, maximum days number required to merge two events 
# njoin: numeric, maximum measures number required to merge two events
# daysbefore: numeric, maximum number of days prior to the event to be considered part of the event
# nbefore: numeric, maximum number of measures prior to the event to be considered part of the event
# daysafter: numeric, maximum number of days after the event to be considered part of the event
# nafter: numeric, maximum number of measures after the event to be considered part of the event
# threshfreq: numeric, maximum number of days between two measurements for them to belong to the same event
# duplic: boolean, Should measures belonging to multiple events be duplicated (TRUE), or should only the last event to which they belong be considered (FALSE)
# keep: boolean, should rows where no event were identified be kept (TRUE) or not (FALSE)

#### Details ####
# If you do not have groups, just create a column with a single value and use it as the group column
# NULL value indicates that the argument will not be considered to define events
# Interaction between days[...], n[...], n and threshfreq arguments are possible and encouraged
# Events merging is processed before adding marginal dates
# In case of overlapping events and if duplic is FALSE, measures will be associated to the most recent event they ever belonged 
#   However, if some measures preceding a new event are already belonging to an older event, they will not be added to the new event 

#### Value ####
# The initial data frame with a supplementary column flaging events with numbers

#### Examples ####  
# Lets create a fake dataset:
# df <- data.frame(value= c(0, 1, 1, 2, 3, 4, 1, 0, 0, 0, 0, 0),
#                  date= seq.Date(as.Date("2001-10-29"), as.Date("2001-11-09")),
#                  group= rep(1, 12))
# Note that we created a group column with only one value because we have only one group
# We can plot it with:
# ggplot(df, aes(date, value)) +
#   geom_point() +
#   geom_path()
# And identify events with:
# idevents(df, date= "date", value= "value", group= "group", thresh= 2, n=3, nbefore= 1, nafter= 1) 
# Note that if we use number of days arguments rather of number of measures arguments it do not change anything because the data points are regularly spaced.
# idevents(df, date= "date", value= "value", group= "group", thresh= 2, n=3, daysbefore= 1, daysafter= 1) 

####################################################################
###################### Function & arguments ########################
####################################################################

idevents <- function (df, date, value, group, thresh, 
                      n= NULL, 
                      daysjoin= NULL, njoin= NULL, 
                      daysbefore= NULL, nbefore= NULL, daysafter= NULL, nafter= NULL,
                      threshfreq= NULL, 
                      duplic= TRUE,
                      keep= TRUE) {
  
  ####################################################################
  ############################# Packages #############################
  ####################################################################
  
  require(gtools)
  require(dplyr)
  
  ####################################################################
  ########################### Verification ###########################
  ####################################################################
  
  #### Date: format ####
  if (!(is.Date(df[[date]]))) {
    stop("Date column needs to be in Date format")
  } else { }
  
  #### Group: format ####
  df[[group]] <- factor(df[[group]])
  
  #### Threshold: format ####
  if (is.character(thresh)) {
    if (!(is.numeric(df[[thresh]]))) {
      df[[thresh]] <- as.numeric(df[[thresh]]) 
      warning("Threshold column converted to numeric format, please check for errors")
    } else { }
    #### Threshold: checking number per group ####
    dftreshgrp <- df %>%
      select(all_of(c(group, thresh))) %>%
      distinct() %>%
      summarise(n= n(),
                .by= all_of(group))
    if (sum(dftreshgrp$n > 1)) {
      stop("There is more than one threshold per group")
    } else { }
  }
  
  #### Value: format ####
  if (!(is.numeric(df[[value]]))) {
    df[[value]] <- as.numeric(df[[value]]) 
    warning("Value column converted to numeric format, please check for errors")
  } else { }
  
  #### Value: checking missing ####
  if (sum(sapply(as.matrix(df), invalid)) > 1) {
    stop("Missing values (NA, NaN, NULL) not supported")
  } else { }
  
  ####################################################################
  ####################### Event identification #######################
  ####################################################################
  
  dfbloom <- data.frame()
  for (h in levels(df[[group]])) {
    
    ##################################
    ########### Selections ###########
    ##################################
    
    ##### Group #####
    dftempsta <- df %>%
      filter(.data[[group]] == h)
    
    ##### Threshold #####
    if (is.character(thresh)) {
      treshselect <- unique(dftempsta[[thresh]])
    } else { 
      treshselect <- thresh
    }
    
    ##### Events #####
    dftempstafilt <- dftempsta %>%
      filter(.data[[value]] >= treshselect)
    
    ##################################
    ########## Preparation ###########
    ##################################
    
    #### Creating event column ####
    dftempsta$event <- NA
    #### Dates vector ####
    ##### All ######
    alldate <- dftempsta[[date]]
    ##### Above the threshold #####
    bloomdate <- dftempstafilt[[date]]
    #### Initializing counters #####
    ##### Events #####
    event_counter <- 0
    ##### Number of rows between events #####
    nrowsup <- 0
    
    if (nrow(dftempstafilt) > 0) {
      for (k in 1:nrow(dftempstafilt)) {
        
        ##################################
        ####### Event distinction ########
        ##################################
        
        nrowsupstock <- nrowsup
        #### New event distinguished ####
        if (!(bloomdate[k] %in% alldate[k + nrowsup])) {
          ##### Past event with insufficient length #####
          if (!(is.null(n)) && 
              nrow(subset(dftempsta, event == event_counter)) < n && 
              event_counter != 0) {
            # Erase past event #
            dftempsta <- dftempsta %>%
              mutate(event= recode_values(event,
                                           event_counter ~ NA,
                                           default= event))
          ##### Past event with sufficient length #####
          } else { 
            # Count a new event #
            event_counter <- event_counter + 1
          }
          ##### Number of rows before next event #####
          repeat {
            ifelse(bloomdate[k] %in% alldate[k + nrowsup], 
                   break, 
                   nrowsup <- nrowsup + 1)
          }
        #### Old event distinguished ####
        } else {
          ##### False old: event at the first row #####
          if (k+nrowsup == 1) {
            # Count a new event #
            event_counter <- event_counter + 1
            # Number of rows before next event #
            repeat {
              ifelse(bloomdate[k] %in% alldate[k + nrowsup], 
                     break, 
                     nrowsup <- nrowsup + 1)
            }
          ##### Real old event #####
          } else { }
        }
        
        ##################################
        ######### Event merging ##########
        ##################################
        
        #### Creating measures counter ####
        jointbloomx <- 0
        repeat { 
          #### Increasing measures counter ####
          jointbloomx <- jointbloomx + 1
          if (nrowsupstock != 0 && # Verify if an event as already been identified
              dftempsta[[date]][k+nrowsup-jointbloomx] != min(dftempsta[[date]]) && # Verify if the searched date exists
              (is.null(njoin) || jointbloomx <= njoin) && # Verify the number of measures condition
              (is.null(daysjoin) || dftempsta[[date]][k+nrowsup-jointbloomx] >= (dftempsta[[date]][k+nrowsup]-daysjoin)) # Verify the time window condition
              ) {
            #### Past event detected ####
            if (nrowsupstock == nrowsup-jointbloomx && !(is.na(dftempsta$event[k+nrowsup-jointbloomx]))) {
              stopjoint <- c()
              ##### Verifying frequency of sample condition #####
              for (z in 1:jointbloomx) {
                if (is.null(threshfreq) ||
                    as.numeric(difftime(as.Date(dftempsta[[date]][k+nrowsup]), 
                                        as.Date(dftempsta[[date]][k+nrowsup-z]), 
                                        units= "days")) <= threshfreq) {
                  stopjoint <- c(stopjoint, FALSE)
                } else { 
                  stopjoint <- c(stopjoint, TRUE)
                }
              }
              ##### Merging events #####
              if (sum(stopjoint) == 0) {
                event_counter <- event_counter - 1
                dftempsta$event[(k+nrowsup-jointbloomx):(k+nrowsup)] <- event_counter
              } else { }
              break
            } else { }
          } else { 
            break
          }
        }
        
        ##################################
        ######### Event writing ##########
        ##################################
        
        #### Duplication of date of overlapping events ####
        if (duplic && 
            !(is.na(dftempsta$event[k+nrowsup])) && # Verify if an event is already identified 
            dftempsta$event[k+nrowsup] != event_counter # Verify if the previously identified event is not the current one
            ) {
          # Duplicating old event #
          dftempsta <- rbind(dftempsta, dftempsta[k+nrowsup,])
          # Writing new event on the duplicated old #
          dftempsta$event[k+nrowsup] <- event_counter
        #### No duplication of date of overlapping events ####
        } else {
          # Writing event #
          dftempsta$event[k+nrowsup] <- event_counter
        }
        
        ##################################
        ######### Marginal dates #########
        ##################################
        
        #### Previous dates ####
        ##### Creating measures counter #####
        lagx <- 0
        repeat {
          ##### Increasing measures counter #####
          lagx <- lagx + 1
          if (k+nrowsup-lagx > 0 && # Verify if the searched row exist
              (is.null(nbefore) || lagx <= nbefore) && # Verify the number of measures condition
              (is.null(daysbefore) || as.Date(dftempsta[[date]][k+nrowsup-lagx]) >= (as.Date(dftempsta[[date]][k+nrowsup])-daysbefore)) && # Verify the time window condition
              (is.null(threshfreq) || as.numeric(difftime(as.Date(dftempsta[[date]][k+nrowsup-lagx+1]), 
                                                         as.Date(dftempsta[[date]][k+nrowsup-lagx]), 
                                                         units= "days")) <= threshfreq) # Verify the frequency of sampling condition
              ) {
            ##### No previously identified event #####
            if (is.na(dftempsta$event[k+nrowsup-lagx])) {
              # Writing event #
              dftempsta$event[k+nrowsup-lagx] <- event_counter
            ##### Previously identified event #####
            } else {
              # Duplication of date of overlapping events #
              if (duplic && 
                  dftempsta$event[k+nrowsup-lagx] != event_counter) {
                dftempsta <- rbind(dftempsta, dftempsta[k+nrowsup-lagx,]) # Duplicating old event
                dftempsta$event[k+nrowsup-lagx] <- event_counter # Writing new event on the old
              # No duplication of date of overlapping events #
              } else {
                break
              }
            }
          } else { 
            break
          }
        }
        #### Following dates ####
        ##### Creating measures counter #####
        lagx <- 0
        repeat {
          ##### Increasing measures counter #####
          lagx <- lagx + 1
          if (k+nrowsup+lagx <= nrow(dftempsta) && # Verify if the searched row exist
              (is.null(nafter) || lagx <= nafter) && # Verify the number of measures condition
              (is.null(daysafter) || as.Date(dftempsta[[date]][k+nrowsup+lagx]) <= (as.Date(dftempsta[[date]][k+nrowsup])+daysafter)) && # Verify the time window condition
              (is.null(threshfreq) || as.numeric(difftime(as.Date(dftempsta[[date]][k+nrowsup+lagx]), 
                                                         as.Date(dftempsta[[date]][k+nrowsup+lagx-1]), 
                                                         units= "days")) <= threshfreq) # Verify the frequency of sampling condition
              ) {
            # Writing event #
            dftempsta$event[k+nrowsup+lagx] <- event_counter
          } else { 
            break
          }
        }
      }
      if (!(is.null(n)) && 
          nrow(subset(dftempsta, event == event_counter)) < n && 
          event_counter != 0) {
        # Erase past event #
        dftempsta <- dftempsta %>%
          mutate(event= case_match(event,
                                   event_counter ~ NA,
                                   .default= event))
      } else { }
      dfbloom <- rbind(dfbloom, dftempsta)
    } else { }
  }
  
  ####################################################################
  ################## Organizing & returning results ##################
  ####################################################################
  
  #### Sorting by date ####
  dfbloom <- dfbloom %>%
    arrange(.data[[group]], .data[[date]])
  
  #### Not keeping non-event rows ####
  if (!(keep)) {
    dfbloom <- subset(dfbloom, is.na(event) == FALSE)
  } else { }
  
  #### Returning results ####
  return(dfbloom)
}