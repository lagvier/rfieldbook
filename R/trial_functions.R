#' separate_trials
#'  @title Fieldbook files separation function
#' Function to check and separate different breeding files into the separate components:
#' inventories, nurseries, trials, seed preparations and stack files within the current directory.
#' @keywords fieldbook, inventories, nurseries, trials, trial, nursery, inventory, breeder, breeding
#' @export

separate_trials = function(dir = NA){
  import_sheets = c("PedigreeImport")
  summary_sheets = c("SeedPrepSummary", "NurserySummary", "TrialSummary")
  inventory_sheets = c("Inventory", "StockLabels")
  nursery_sheets = c("Entrylist", "Stocklist", "StockLabels")
  trial_sheets = c("Fieldbook", "Results", "Master")
  seedprep_sheets = c("SeedPrep", "Design", "SeedLabels")


  if(is.na(dir)){
  main.dir = choose.dir("Choose the location of the trial files") %>%
    gsub('[\\]', '/', .)

  is_trial_folder = function(main.dir = main.dir){
    x = readline(prompt='Was this folder created by this package (Y/N)?')
    if(grepl('^n', x, ignore.case = TRUE)){
      dir= seperate_trials(calldir = main.dir)
    }else {
      if(grepl('^y', x, ignore.case = TRUE)){
        dir = main.dir
      }else x= is_trial_folder(main.dir = main.dir)
    }
  }
  dir = is_trial_folder(main.dir = main.dir)
  }

  cat(paste("Trials files:\n"))
  cat(paste("\tChecking and separating different trials files\n\n"))

  if(length(list.files(dir))>0){
    type = "trials"
    y = list.files(path = dir, all.files = FALSE, full.names = TRUE, pattern = '.xls')
    progbar <- txtProgressBar(min = 0, max = length(y), style = 3)
    y_data = lapply(y, function(x){
      stepi = grep(x, y, ignore.case = TRUE, value = FALSE)
      setTxtProgressBar(progbar,stepi)

      Site = stringr::str_split(x, '/') %>% unlist() %>% .[length(.)] %>%
        stringr::str_split(., '[.]') %>% unlist() %>% .[1]
      File = stringr::str_split(Site, '-') %>% unlist() %>% .[-length(.)] %>%
        paste(., collapse = '-')

      Start = getFirstPlot(x)
      if(is.na(Start)) stop(paste('The file', x, 'is not a nursery of trial!\n'))
      ydata = suppressMessages(readxl::read_excel(x, sheet = 'Fieldbook', skip = Start, col_names = TRUE))
      pedigreeName = names(ydata)[min(grep('pedigree', names(ydata), ignore.case = TRUE))]
      if(all(!grepl('stockid', names(ydata), ignore.case = TRUE)))
        ydata = dplyr::mutate(ydata, StockID = '')
      ydata = dplyr::mutate(ydata, Plot = as.numeric(Plot), Site = Site) %>%
        subset(., !is.na(Plot),
               select = c("Site", "Rep", "Entry", "StockID", pedigreeName)) %>%
        as.data.frame(.) %>% dplyr::mutate(., Rep = as.numeric(Rep), Entry = as.numeric(Entry))
      names(ydata) <- c("Site", "Rep", "Entry", "StockID", "Pedigree")
      return(ydata)
    }) %>% do.call(rbind, .)

    cat('\tcheck the distribution of entries and consistency across sites\n')
    ydata = dplyr::mutate(y_data, Rep = ifelse(!is.na(Rep), 1, NA)) %>%
      aggregate(Rep ~ Entry + StockID + Pedigree + Site, data = ., function(x) sum(x, na.rm = TRUE)) %>%
      data.table::data.table(.) %>%
      dcast(., Entry+StockID+Pedigree ~ Site, value.var = "Rep")

    cat(paste("\n\tChecking trial entries with missing information\n"))
    missingStocks = subset(ydata, (is.na(StockID) | StockID == '' | StockID == " ") &
                             (is.na(Pedigree) | Pedigree == '' | Pedigree == " "))

    if(nrow(missingStocks) == 0)
      cat("\t\tNo entries with missing information\n")

    cat("\n\tChecking duplicates\n")
    duplicateStocks = subset(ydata, duplicated(paste0(Entry, StockID, Pedigree)))
    if(nrow(duplicateStocks) == 0) cat("\t\tNo duplicated entries in the compiled data\n")

    cat(rep('-', 50))

    cat('\nGrouping entries\n')
    y_data = dplyr::mutate(y_data, id = paste0(Entry, StockID, Pedigree))
    groups = as.data.frame(cbind(id ='', Entry='', StockID='', Pedigree='', site='', siteno = ''))

    progbar = txtProgressBar(min = 0, max = length(unique(y_data$id)), style=3)
    for(x in unique(y_data$id)[4242:7972]){

      stepi = (1:length(unique(y_data$id)))[unique(y_data$id) %in% x]
      if(stepi %% 10 == 0)
        cat ( "At ", stepi,round(100*stepi/length(unique(y_data$id)), 2) ,"% of", length(unique(y_data$id)), "\n")

      if(!x %in% groups$id){
        sites = reshape2::melt(y_data, id = c('id', 'Entry', 'StockID', 'Pedigree')) %>%
          subset(., id == x & variable == 'Site' & !is.na(value)) %>%
          .[['value']] %>% unique(.)
        subgroup = subset(reshape2::melt(y_data, id = c('id', 'Entry', 'StockID', 'Pedigree')) %>%
                            subset(.,  !is.na(value)) %>%
                            plyr::rename(., c(value = 'site')),
                          as.character(site) %in% sites & variable == 'Site' &
                            !duplicated(paste0('Entry', 'StockID', 'Pedigree','site')),
                          select = c('id', 'Entry', 'StockID', 'Pedigree','site')) %>%
          dplyr::mutate(., siteno = x) %>% unique(.) %>% arrange(id)
      }
      groups = rbind(groups, subgroup)
      groups = subset(groups, id !='' & id != ' ')
    }
    groups = dplyr::mutate(groups, sitegroup = as.integer(as.factor(siteno)))

    subdir = as.character(unique(groups$sitegroup))
    sapply(subdir, function(x)
      if(!dir.exists(paste0(dir, "/Site-", x)))
        dir.create(paste0(dir, "/Site-", x))
    )

    if(!dir.exists(paste0(dir, "/backup"))) dir.create(paste0(dir, "/backup"))
    sapply(y, function(x) file.copy(y, paste0(dir, "/backup")))
    cat(paste("\n\tCopied trial files to the trial backup folder\n"))

    cat(paste("\n\tCopying trial files to the trial respective trial folder\n"))
    progbar = txtProgressBar(min = 0, max = length(subdir), style=3)
    sapply(subdir, function(x){
      stepi = (1:length(subdir))[subdir %in% x]
      setTxtProgressBar(progbar,stepi)
      x1 = subset(groups, as.character(sitegroup) == as.character(x)) %>%
        dplyr::mutate(., site = as.character(site)) %>%
        .[['site']] %>% unique(.) %>%
        sapply(., function(z) grepl(z, y)) %>%
        as.data.frame(.) %>% apply(., 1, sum) %>%
        sapply(., function(z) ifelse(z >= 1, TRUE, FALSE)) %>%
        y[.]

      sapply(x1, function(z){
        file.copy(z, paste0(dir, "/Site-", x))
        unlink(z)
      })
    })
  }
  cat(paste("\tDeleting trials files from the subfolder", dir, "\n"))
  cat(rep('-', 50), "\n")
}


#' correctTrialNames
#'  @title Use trial files to create trial name
#' Function that infers the names of the trial name based on the names of the trial files within the folder.
#' @keywords fieldbook, inventories, nurseries, trials, trial, nursery, inventory, breeder, breeding
#' @export


correctTrialNames <- function(trialsRootFolder = NA){
  library(dplyr)
  foldersToRename = list.dirs(trialsRootFolder, recursive = FALSE, full.names = FALSE) %>%
    subset(., grepl('^site', ., ignore.case = T))

  sapply(foldersToRename[-c(1:20)], function(x){
    cat(x, '...!\n')
    getFiles = list.files(file.path(trialsRootFolder, x), pattern = 'xls$',
                          full.names = FALSE, recursive = FALSE)
    minLength = lapply(getFiles, function(x) nchar(x)) %>% unlist() %>% min()
    frame = as.data.frame(cbind(index = c(1:minLength)))
    for(i in getFiles){
      frame[[i]] = strsplit(i, "")[[1]][1:minLength]
    }
    frame$Exclude =  apply(subset(frame, select = -index), 1, function(x)length(unique(x))>1)

    i = subset(frame, Exclude) %>% arrange(index) %>% .['index'] %>% .[1,1]
    i = paste(frame[,2][1:i], collapse = '')

    dir.create(file.path(trialsRootFolder, i))
    file.copy(file.path(trialsRootFolder, x, getFiles), file.path(trialsRootFolder, i))
    unlink(file.path(trialsRootFolder, x), recursive=TRUE)
  })
}


#' checkAnalyzed
#' @title Analyzed trials
#' Function to check if trial files are analyzed
#' @param String for the directory containing the trial files
#' @export

checkAnalyzed = function(cdir){

  # list files
  files = list.files(dir, pattern = '.xls$', recursive = TRUE) %>%
    sort()

  data = lapply(files, function(file){
    cat(file, "\n")
    if(all(c('Fieldbook', 'Results', 'Master') %in% readxl::excel_sheets(file.path(dir, file)))){
      sk = getFirstPlot(file.path(dir, file), sheet = "Results")
      # get relevant columns
      Cols = readxl::read_excel(file.path(dir, file), sheet = "Results") %>% names()
      # Cols = c(grep('entr', Cols, ignore.case = T)[1],
      #          grep('grainyield', Cols, ignore.case = T))
      sp_cols = c(grep('^SelectionIndex$', Cols, ignore.case = T)+1,
                  grep('^SelectionsMarked$', Cols, ignore.case = T)-1)

      dat = read.xlsx2(file.path(dir, file), sheetName="Results", startRow=sk+1,
                       as.data.frame=TRUE,colIndex = 1:sp_cols[2], header=FALSE)
      names(dat) <- Cols[1:sp_cols[2]] # c("Entry", paste0("Yield_", c(1:(ncol(Cols)-1))))
      dat = subset(dat, !is.na(as.numeric(Entry)))
      sp_cols = c(grep('^entry$', names(dat), ignore.case = T), sp_cols)
      dat = dat[, c(c(sp_cols[1], c(sp_cols[2]:sp_cols[3])))]

      dat = melt(dat, id = 'Entry') %>%
        subset(., !is.na(as.numeric(value)))

      # consider trials with 20% results data analyzed
      Status = ifelse(sum(duplicated(paste(dat$Entry, dat$variable)))>0,"Duplicated entries",
                      ifelse(nrow(dat) > length(unique(dat$Entry)) * length(unique(dat$variable))*0.5, 'Analyzed',
                             'Not analyzed'))

    }else{
      Status = 'Not trial'
    }
    da = as.data.frame(cbind(File = file, Status = Status))
    return(da)
  }) %>% do.call('rbind', .)

  data = lapply(c('duplicates', 'unAnalyzed', 'analyzed'), function(group){

    if(group == 'duplicates') df = subset(data, Status == 'Duplicated entries')
    if(group == 'unAnalyzed') df = subset(data, Status != 'Analyzed')
    if(group == 'analyzed') df = subset(data, Status == 'Analyzed')

    return(df)
  })

  data = data[unlist(lapply(data, function(df) nrow(df) > 0))]

  names(data) = unlist(lapply(data, function(df) unique(df$Status)))

  return(data)
}

