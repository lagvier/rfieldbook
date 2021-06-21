#' metadata
#' @title Fieldbook data linkage and completeness function
#' Function to to connect data in different fieldbook files: inventory, nurseries and trials
#' @param inventories Directory path to the inventory file
#' @param nurseries Directory path to nursery files
#' @param trials Directory path to trial files
#' @keywords fieldbook, inventories, nurseries, trials, trial, nursery, inventory, breeder, breeding, cimmyt, cgiar, maize, stma
#' @export

metadata <- function(inventories = NULL,nurseries = NULL, trials = NULL){

  dir = paste0(Sys.getenv('R_USER'), "/", format(Sys.time(), "%Y%m%d_%H%M%S"))
  if(!dir.exists(dir)) dir.create(dir)
  if(!dir.exists(paste0(dir, "/log"))) dir.create(paste0(dir, "/log"))
  setwd(paste0(dir, "/log"))
  logfile <- "output.txt"

  cat(rep('-', 80), "\n")
  cat(rep('-', 28), "CHECKING LINKS OF DIFFERENT STAGES", rep('-', 27), "\n")
  cat(rep('-', 80), "\n")

  if(is.null(inventories)){
    inventories = readline(prompt = "Do you have inventories file for your data? (Yes/No): ")
    if(grepl("^y", inventories, ignore.case = TRUE))
      inventories =  choose.dir("Choose a the location of the files") %>% gsub('[\\]', '/', .)
  }
  if(is.null(nurseries)){
    nurseries = readline(prompt = "Do you have nurseries folder? (Yes/No): ")
    if(grepl("^y", nurseries, ignore.case = TRUE))
      nurseries =  choose.dir("Choose a the location of the nursery folder") %>% gsub('[\\]', '/', .)
  }
  if(is.null(trials)){
    trials = readline(prompt = "Do you have trials folder? (Yes/No): ")
    if(grepl("^y", trials, ignore.case = TRUE))
      trials =  choose.dir("Choose a the location of the nursery folder") %>% gsub('[\\]', '/', .)
  }

  inventory_sheets = c("Inventory", "StockLabels")
  nursery_sheets = c("Entrylist", "Stocklist", "StockLabels")
  trial_sheets = c("Fieldbook", "Results", "Master")

  cat('\nChecking inventory files!...\n')
  if(is.null(inventories) || length(list.files(inventories, pattern = '.xls$')) ==0){
    cat('\nSorry the inventory folder does not exists or empty. Check again and retry it!...\n')
  }else{
  if(length(list.files(inventories, pattern = '.xls$'))>0){
    inventory_files = list.files(path = inventories, pattern = '.xls$', full.names = TRUE)
    invents = lapply(inventory_files, function(x){
      file_info = readxl::excel_sheets(x)
      if(all(inventory_sheets %in% file_info)){
        xdata = readxl::read_excel(x, sheet = 'Inventory', col_types = 'text', col_names = TRUE, trim_ws = FALSE) %>%
          .[['StockID']] %>% grep('-1$', .) %>% min(.)
        if(as.numeric(as.character(xdata))<Inf & as.numeric(as.character(xdata)) > 0){
          xdata = readxl::read_excel(x, sheet = 'Inventory', col_types = 'text', col_names = TRUE, trim_ws = FALSE) %>%
            dplyr::mutate(., RNUMBER_ = row_number()) %>%
            subset(., !RNUMBER_ %in% c(1:xdata), select = c("StockID", "Name", "BreedersPedigree1", "Origin"))
          return(xdata)
        }
      }
    }) %>% do.call('rbind', .) %>%
      subset(., !duplicated(paste0(StockID, Name, BreedersPedigree1, Origin)))
      }
    }

  cat('\nChecking nursery directories and/or files!...\n')
    if(is.null(nurseries) || length(list.files(nurseries, pattern = '.xls$')) ==0){
        cat('\nSorry the nursery folder does not exists or empty!...\n')
    }else{
    if(length(list.files(nurseries, pattern = '.xls$'))>0){
    nurseries_files = list.files(path = nurseries, pattern = '.xls$', full.names = TRUE, recursive = TRUE)
    nurs = lapply(nurseries_files, function(x){
      if(grep(x, nurseries_files) %% 10 == 0)
        cat(grep(x, nurseries_files), 'of', length(nurseries_files), '...\n')
      file_info = readxl::excel_sheets(x)
      if(all(nursery_sheets %in% file_info)){
        x1 = readxl::read_excel(x, sheet = "Entrylist", col_types = 'text', col_names = TRUE, trim_ws = FALSE) %>%
          .[['StockID']] %>% grep('-1$', .) %>% min(.)
        if(as.numeric(as.character(x1))<Inf & as.numeric(as.character(x1)) > 0){
          xdata = readxl::read_excel(x, sheet = 'Entrylist', col_types = 'text', col_names = TRUE, trim_ws = FALSE) %>%
            dplyr::mutate(., RNUMBER_ = row_number()) %>%
            subset(., !RNUMBER_ %in% c(1:x1), select = c("StockID", "Name", "BreedersPedigree1", "Origin"))
          x1 = readxl::read_excel(x, sheet = "Stocklist", col_types = 'text', col_names = TRUE, trim_ws = FALSE) %>%
            .[['StockID']] %>% grep('-1$', .) %>% min(.)
          if(as.numeric(as.character(x1))<Inf & as.numeric(as.character(x1)) > 0){
            xdata = rbind(xdata,
                          readxl::read_excel(x, sheet = 'Stocklist', col_types = 'text', col_names = TRUE, trim_ws = FALSE) %>%
                            dplyr::mutate(., RNUMBER_ = row_number()) %>%
                            subset(., !RNUMBER_ %in% c(1:x1), select = c("StockID", "Name", "BreedersPedigree1", "Origin"))
            )
            xdata$Source = x
          }
        }
      }
      return(xdata)
    }) %>% do.call('rbind.fill', .) %>%
      subset(., !duplicated(paste0(StockID, Name, BreedersPedigree1, Origin)))
      }
    }

    cat('\n\tComparing inventory and nursery entries!...\n')
    if(!is.null(nurseries) &&  length(list.files(nurseries, pattern = '.xls$'))>0 && nrow(nurs) > 0){
        if(is.null(inventories) ||  length(list.files(inventories, pattern = '.xls$'))==0 || nrow(invents) == 0){
          missnurs = dplyr::mutate(nurs, details = 'incomplete information')
        }else{
          missnurs = lapply(nurs$BreedersPedigree1, function(x){
            xdata = subset(nurs, BreedersPedigree1 == x)
            xdata =  dplyr::mutate(xdata, details = 'incomplete information') %>%
              subset(., any(is.na(StockID), is.na(BreedersPedigree1), is.na(Origin)))

            if(nrow(xdata)>0) return(xdata)
            else{
              xdata = dplyr::mutate(invents, details = 'missing pedigree') %>%
                merge(
                  subset(nurs, BreedersPedigree1 == x), .,
                  by = c("StockID", "Name", "BreedersPedigree1", "Origin"), all.x = TRUE) %>%
                subset(., is.na(details))

              if(nrow(xdata)>0) return(xdata)
            }
          }) %>% do.call('rbind.fill', .) %>%
            subset(., !duplicated(paste0(StockID, Name, BreedersPedigree1, Origin)))
        }
    cat("\n", nrow(missnurs), ": seed stock entries missing in the inventory...\n")
    write.csv(missnurs, 'nurseries miss in inventories.csv', row.names = FALSE)
    }

  cat('\n\tComparing inventory and nursery verses trials entries!...\n')


  if((!is.null(inventories) && length(list.files(inventories, pattern = '.xls$'))> 0 && nrow(invents)>0) &&
       (!is.null(nurseries) && length(list.files(nurseries, pattern = '.xls$'))> 0 && nrow(nurs)>0)){
    invent_nurs = rbind(mutate(invents, Source = 'Inventory list'), nurs) %>%
      subset(., !duplicated(paste0(StockID, Name, BreedersPedigree1, Origin)))
    }else{
        if(!is.null(inventories) && length(list.files(inventories, pattern = '.xls$'))> 0 && nrow(invents)>0){
          invent_nurs = mutate(invents, Source = 'Inventory list') %>%
            subset(., !duplicated(paste0(StockID, Name, BreedersPedigree1, Origin)))
        }else{
          if(!is.null(nurseries) && length(list.files(nurseries, pattern = '.xls$'))> 0 && nrow(nurs)>0){
            invent_nurs = nurs %>%
              subset(., !duplicated(paste0(StockID, Name, BreedersPedigree1, Origin)))
          }else{
            invent_nurs = NULL
          }
        }
    }

    if(!is.null(invent_nurs) && nrow(invent_nurs) >0 &&
       !is.null(trials) && length(list.files(trials, pattern = '.xls$'))>0){
      trials_files = list.files(path = trials, pattern = '.xls$', full.names = TRUE, recursive = TRUE) %>%
              subset(., grepl('/backup/', .))

      misstrials = lapply(trials_files, function(x){
      file_info = readxl::excel_sheets(x)
      if(all(trial_sheets %in% file_info)){
        Start = readxl::read_excel(x, sheet = 'Fieldbook', col_names = TRUE) %>%
          dplyr::mutate(., IDX = c(1:nrow(.)), Plot = as.character(Plot)) %>%
          subset(., Plot == 'Plot', select = c('IDX')) %>% min(.)-1
        xdata = readxl::read_excel(x, sheet = 'Fieldbook', col_names = TRUE, col_types = 'text', skip = Start) %>%
          dplyr::mutate(., Plot = as.integer(as.character(Plot))) %>%
          subset(., !is.na(Plot))
        ped = grep('pedigree', names(xdata), v = T, ignore.case = T)[1]
        if(!grepl('StockID', names(xdata), ignore.case = TRUE)) xdata$StockID = ''
        xdata = subset(xdata, select = c("Name", "StockID", ped, "Origin")) %>%
          mutate(., BreedersPedigree1 = .[[ped]])
        xdata[[ped]] <- NULL
        xdata = subset(xdata, !(StockID %in% invent_nurs$StockID) &
                       !(paste0(Name, Origin) %in% paste0(invent_nurs$Name, invent_nurs$Origin)) &
                         !(paste0(BreedersPedigree1, Origin) %in% paste0(invent_nurs$Name, invent_nurs$Origin)) &
                         !(paste0(Name, Origin) %in% paste0(invent_nurs$BreedersPedigree1, invent_nurs$Origin)) &
                         !(paste0(BreedersPedigree1, Origin) %in% paste0(invent_nurs$BreedersPedigree1, invent_nurs$Origin))) %>%
          subset(., !duplicated(paste0(BreedersPedigree1, Origin)))
      }
    }) %>% do.call('rbind.fill', .)

      cat("\n", nrow(misstrials), ": trial entries missing in the inventory/nurseries...\n")
      write.csv(misstrials, 'trials miss in inventories_nurseries.csv', row.names = FALSE)
    }

   cat("\n", rep('-', 80), "\n")

}
