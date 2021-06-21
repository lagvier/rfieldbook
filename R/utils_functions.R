#' stem
#' @title Common names
#' A function to identify the longest name that is common in a list of paths, files or names
#' @param nams The list of paths, files or names
#' @keywords Stem
#' @export

stem = function(nams){
  x = nams[1]
  y = nams[nams != x]
  nc = sapply(1:nchar(x), function(i){
    i = ifelse(all(grepl(paste0("^", substring(x, 1, i)), y, ignore.case = T)) == TRUE, i, 0)
    return(i)
  })
  nc = max(nc)
  nc = ifelse(nc == 0, '_no_common_stem_',
              substring(x, 1, min(nc)))
  return(nc)
}

#' commonDir
#' @title The common location
#' A function to determine the common location of a list of paths (files, directories or both)
#' @param nams List of paths
#' @keywords location
#' @export

commonDir = function(nams){
  row = lapply(strsplit(nams, "/"), function(row){
    row = cbind.data.frame(
      id = paste(row, collapse = "/"),
      No = c(1:length(row)), value = row
    )
    return(row)
  })
  row = do.call('rbind', row)
  row = reshape(row, idvar = 'id', timevar = 'No', direction = 'wide')
  if(nrow(row) == 1){
    rowT = c(FALSE, rep(TRUE, length(row)-2), FALSE)
  }else{
    rowT = unlist(lapply(names(row), function(same){
      all(row[[same]] %in% row[[same]][1])
    }))
  }
  rowT = as.character(row[1,])[rowT]
  rowT = paste(rowT, collapse = "/")
  return(rowT)
}

splitString <- function(string, pattern, include = FALSE){
  x = str_split(string, pattern)[[1]]
  x = as.data.frame(cbind(
    x = x,
    n = as.numeric(c(1:length(x)))
  ))
  if(include == TRUE){
    x = mutate(x, x = ifelse(as.character(x)=='', as.character(ki[i]), x))
  }else{
    x = subset(x, as.character(x)!='')
  }

  x = x$x
  return (x)
}


#' replaceNested
#' @title Replace nested brackets in a pedigree name
#' A function to replace pairs of nested brackets (squire and open) to establish breeding generation in a pedigree
#' @param x the pedigree or name string
#' @param left The type of left bracket
#' @param right The type of right bracket
#' @param replacement symbol for the string between the left and right bracket
#' @keywords nomenclature
#' @export

replaceNested <- function(x, left=NA, right=NA, replace = '??'){
  x = as.character(x)

  if(any(is.na(left), is.na(right)) || stringr::str_count(x, left) != stringr::str_count(x, right))
    stop("both left and right delimiters must be provided and must be equal pairs in the string!")

  while(grepl(left, x)){
    ki = stringr::str_extract_all(x, paste0(left, "[^",  left,  right, "]+", right))[[1]]
    for(i in ki){
      x = gsub(i, replace, x, fixed = TRUE)
    }
  }

  return(x)
}

#' typeBreeding
#' @title Check the level of
#' A function to extract the level of crosses in a nomenclature
#' @param stringx A string for name or pedigree
#' @keywords pedigree, breeding, nomenclature
#' @export

typeBreeding = function(stringx){
  stringx = as.character(stringx)
  x = unlist(strsplit(stringx, ''))
  if(!all(sum(grepl('\\(', x)) == sum(grepl('\\)', x)),
      sum(grepl('\\[', x)) == sum(grepl('\\]', x))))
    stringx = 'Invalid nomenclature'
  else{
    while(grepl('\\(', stringx)){
      ki = stringr::str_extract_all(stringx, "\\([^()]+\\)")[[1]]
      for(i in ki){
        stringx = gsub(i, 'mimi_mdogo', stringx, fixed = TRUE)
      }
    }

    while(grepl('\\[', stringx)){
      ki = stringr::str_extract_all(stringx, "\\[[^\\[\\]]*\\]")[[1]]
      for(i in ki){
        stringx = gsub(i, 'mimi_mdogo', stringx, fixed = TRUE)
      }
    }
  }

  return(stringx)
}

#' checkAnalysed
#' @title Check the analysis status
#' Function to determine if trial(s) has been analyzed - results sheet has any traits results
#' @param dir Path to trial(s) files
#' @keywords trial, analysis
#' @export

checkAnalysed <- function(dir){

  trf= list.files(dir,pattern ='.xls$', recursive = TRUE)

  data = lapply(trf, function(x){
    start = getFirstPlot(file.path(dir, x), sheet = 'Results')
    dat = readxl::read_excel(file.path(dir, x), sheet = 'Results', skip = start, col_names = TRUE)
    dnames = readxl::read_excel(file.path(dir, x), sheet = 'Results', col_names = TRUE) %>%
      names()
    names(dat) <- dnames
    dat = subset(dat, !is.na(as.numeric(as.character(Entry))) & !duplicated(Entry))
    st = grep('Index', names(dat))[1] + 1;
    sp = ifelse(length(grep("SelectionsMarked", names(dat)))==0, ncol(dat), grep("SelectionsMarked", names(dat)) - 1)
    dat = subset(dat, select = c('Entry', names(dat)[st:sp])) %>%
      reshape2::melt(., id = 'Entry') %>%
      mutate(., value = as.numeric(as.character(value))) %>%
      subset(., !is.na(value))

    x = as.data.frame(cbind(
      File = gsub('.xls', '', x),
      Entries = length(unique(dat$Entry)),
      Variables = length(unique(dat$variable))
    ))

    return(x)
  })

  data = do.call('rbind', data) %>%
    arrange(Variables)

  write.csv(data, file.path(dir,
                            paste(unlist(strsplit(dir, '/')) %>% .[length(.)], '.csv')
  ))
}


#' checkDataAvailable
#' @title Check data availability status of trials
#' A function to check if trial file(s) has any traits data in them
#' @param dir Path to the location of the trial file(s)
#' @keywords trial, data
#' @export

checkDataAvailable <- function(dir){

  trf= list.files(dir,pattern ='.xls$', recursive = TRUE) %>%
    subset(., !grepl('stack',., ignore.case = T) &
             !grepl('import',., ignore.case = T) &
             !grepl('summary',., ignore.case = T)
    )

  data = lapply(trf, function(x){
    sheetName = grep('cdata', readxl::excel_sheets(file.path(dir, x)),
                     ignore.case = T, value =TRUE)
    dat = readxl::read_excel(file.path(dir, x), sheet = sheetName, col_names = FALSE)
    x = cbind.data.frame(
      File = gsub('.xls', '', x),
      nRows = nrow(dat)
    )

    return(x)
  })

  data = do.call('rbind', data) %>%
    arrange(nRows)

  write.csv(data, file.path(dir,
                            paste(unlist(strsplit(dir, '/')) %>% .[length(.)], '.csv')
  ))
}

#' importsPrepared
#' @title Compare trial files and respective import files
#' A function to check which files have respective import files
#' @param dir Path to directory containing the trial files
#' @keywords import, trial
#' @export

importsPrepared = function(dir){
  all_files = list.files(dir)
  xls_files = subset(all_files, grepl('.xls$', all_files, ignore.case = T))
  other_files = subset(all_files, !grepl('.xls$', all_files, ignore.case = T))
  import_files = subset(xls_files, grepl('Import.xls$', all_files, ignore.case = T))
  trial_files = subset(xls_files, !grepl('Import.xls$', all_files, ignore.case = T))

  unpaired_import = import_files[!tolower(gsub('Import.xls', '.xls', import_files)) %in%
                                   tolower(trial_files)]

  unpaired_trials = trial_files[!tolower(trial_files) %in% tolower(gsub('Import.xls', '.xls', import_files))]

  if(length(unpaired_import) == 0) unpaired_import = ""
  if(length(unpaired_trials) == 0) unpaired_trials = ""

  dir = cbind.data.frame(unpaired_import, unpaired_trials)

  return(dir)
}


#' hasStocks
#' @title Status of stocklist sheet in a nursery file
#' A function to check if stockList has been generated
#' @param nursery_file  Full path to the nursery file
#' @keywords nursery, stocklist, entrylist, list, stock, entry
#' @export

hasStocks <- function(nursery_file){
  File = unlist(strsplit(nursery_file, "/")) %>% .[length(.)] %>%
    gsub(".xlsx", "", ., ignore.case = T) %>% gsub(".xls", "", ., ignore.case = T)

  d = readxl::read_excel(nursery_file, sheet="Entrylist", col_names = T) %>%
    subset(., select = c(grep('entry', names(.), ignore.case = T, v = T)[1],
                         grep('Stock', names(.), ignore.case = T, v = T)[1],
                         grep('pedigr', names(.), ignore.case = T, v = T)[1],
                         grep('origin', names(.), ignore.case = T, v = T)[1]))
  names(d) <- c("Entry", "StockID", "Pedigree", "Origin")
  d <- subset(d, !is.na(as.numeric(Entry)))

  Entries = nrow(d)

  d = readxl::read_excel(nursery_file, sheet="Stocklist", col_names = T) %>%
    subset(., select = c(grep('entry', names(.), ignore.case = T, v = T)[1],
                         grep('Stock', names(.), ignore.case = T, v = T)[1],
                         grep('pedigr', names(.), ignore.case = T, v = T)[1],
                         grep('origin', names(.), ignore.case = T, v = T)[1]))
  names(d) <- c("Entry", "StockID", "Pedigree", "Origin")
  d <- subset(d, !is.na(as.numeric(Entry)))

  Stocks = nrow(d)

  nursery_file <- cbind.data.frame(File, Entries, Stocks)

  return(nursery_file)
}


#' getFirstPlot
#'  @title Fieldbook first plot identification
#' Function to identify the location of the first plot in a sheet of a trial or nursery
#' @keywords fieldbook, inventories, nurseries, trials, trial, nursery, inventory, entry, stock, results
#' @param file Full path to the trial or nursery file
#' @param sheet The sheet of the file
#' @export

getFirstPlot <- function(file, sheet){
  if(all(c("entrylist","stocklist") %in% tolower(readxl::excel_sheets(file))))
    Start = suppressMessages(readxl::read_excel(file, sheet = sheet, col_names = TRUE)) %>%
      mutate(., IDX = c(1:nrow(.))) %>% subset(., Entry=='Entry', select=IDX) %>% min()
  else{
    if(all(c("fieldbook","results","master") %in% tolower(readxl::excel_sheets(file)))){
      Start = suppressMessages(readxl::read_excel(file, sheet = sheet, col_names = TRUE))
      names(Start) <- tolower(names(Start))
      Start = mutate(Start, IDX = c(1:nrow(Start)))
      if(tolower(sheet) == 'fieldbook')
        Start = subset(Start, plot=='Plot', select=IDX) %>% min()
      else
        Start = subset(Start, entry=='Entry', select=IDX) %>% min()
    }
    else
      Start = NA
  }
  return(Start)
}

#' fieldbookFile
#' @title Identify the type of fieldbook file selected
#' A function to determine the fieldbook file type: stack, import, trial, etc
#' @param file Full path to an excel file
#' @keywords fieldbook, trial, nursery, inventory, stack, import, summary, file
#' @export

fieldbookFile = function(file){
  file = readxl::excel_sheets(file)

  file = ifelse(all(c('FieldbookStack', 'MasterStack', 'ResultsStack',
                      'SummaryStatsStack', 'PedigreeImportStack') %in% file), "Stack",
         ifelse(all(c('Fieldbook', 'Master', 'Results', 'SummaryStats',
                             'PedigreeImport') %in% file), "Import",
         ifelse(all(c('Fieldbook', 'Master', 'Results') %in% file), "Trial",
         ifelse(all(c('SeedPrepSummary','NurserySummary', 'TrialSummary') %in% file), "Summary",
         ifelse(all(c('Entrylist','Stocklist') %in% file), "Nursery", "Unknown")))))

  return(file)
}


#' Mode
#' @title identify most common
#' A simple function to identify the most occurring category in data points
#' @param x A vector of datapoints
#' @keywords Mode
#' @export

Mode <- function(x) {
  ux <- unique(x)
  ux <- ux[which.max(tabulate(match(x, ux)))]
  return(ux)
}



