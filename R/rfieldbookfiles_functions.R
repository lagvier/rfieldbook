#' separate_files
#'  @title Fieldbook files separation function
#' Function to check and separate different breeding files into the separate components:
#' inventories, nurseries, trials, seed preparations and stack files within the current directory.
#' @keywords fieldbook, inventories, nurseries, trials, trial, nursery, inventory, breeder, breeding
#' @export

separate_files = function(){
  import_sheets = c("PedigreeImport")
  summary_sheets = c("SeedPrepSummary", "NurserySummary", "TrialSummary")
  inventory_sheets = c("Inventory", "StockLabels")
  nursery_sheets = c("Entrylist", "Stocklist", "StockLabels")
  trial_sheets = c("Fieldbook", "Results", "Master")
  seedprep_sheets = c("SeedPrep", "Design", "SeedLabels")

  main.dir = choose.dir("Choose a the location of the files") %>%
    gsub('[\\]', '/', .)
  dir = paste0(main.dir, "/", format(Sys.time(), "%Y%m%d_%H%M%S"))
  if(!dir.exists(dir)) dir.create(dir)
  xcel_files = list.files(path = main.dir, pattern = '.xls$', recursive = TRUE,
                          full.names = TRUE) %>% .[!duplicated(.)]
  sapply(xcel_files, function(x) file.copy(x, dir))

  sapply(c('imports', 'summaries', 'inventories', 'nurseries', 'trials',
           'seedpreps', "stacks", "unknown", "Decision directory", "backup"),
         function(x){
           if(!dir.exists(paste0(dir, "/", x))) dir.create(paste0(dir, "/", x))
           else  unlink(paste0(dir, "/", x))
         }
  )

  logfile <- paste0(dir, "/output.txt")
  sink(logfile)
  cat(rep('-', 80), "\n")
  cat(rep('-', 28), "SEPARATING FILES AND FOLDERS IN BREEDING PGOGRAMS", rep('-', 27), "\n")
  cat(rep('-', 80), "\n")
  cat(paste("Transfered files to the new subdirectory:", dir, "\n"))
  cat(paste("Create subdirectories for different files\n"))

  xcel_files = list.files(path = dir, pattern = '.xls$')

  for(xcel_file in xcel_files){
    cat(paste("\tIn file", xcel_file, "\n"))
    xcel_file = paste0(dir, "/", xcel_file)
    file_info = excel_sheets(xcel_file)
    file.copy(xcel_file, paste0(dir, "/", 'backup'))

    if(all(import_sheets %in% file_info))
      file.copy(xcel_file, paste0(dir, "/", 'imports'))
    else{
      if(all(summary_sheets %in% file_info))
        file.copy(xcel_file, paste0(dir, "/", 'summaries'))
      else{
        if(all(inventory_sheets %in% file_info))
          file.copy(xcel_file, paste0(dir, "/", 'inventories'))
        else{
          if(all(nursery_sheets %in% file_info))
            file.copy(xcel_file, paste0(dir, "/", 'nurseries'))
          else{
            if(all(trial_sheets %in% file_info))
              file.copy(xcel_file, paste0(dir, "/", 'trials'))
            else{
              if(all(seedprep_sheets %in% file_info))
                file.copy(xcel_file, paste0(dir, "/", 'seedpreps'))
              else{
                if(all(grepl('stack', file_info, ignore.case = TRUE)))
                  file.copy(xcel_file, paste0(dir, "/", 'stacks'))
                else
                  file.copy(xcel_file, paste0(dir, "/", 'unknown'))
              }
            }
          }
        }
      }
    }
    unlink(xcel_file)
  }
  cat(paste("\nMoved files to the different subfolders and all files to the backup folder\n"))
  cat(rep('-', 80), "\n")

  cat(paste("Trials files:\n"))
  cat(paste("\tChecking and separating different trials files\n\n"))


  if(dir.exists(paste0(dir, "/", 'trials')) &&
     length(list.files(paste0(dir, "/", 'trials')))>0){
    type = "trials"
    y = list.files(path = paste0(dir, "/", type), all.files = FALSE, full.names = TRUE, pattern = '.xls')
    y_data = lapply(y, function(x){
      cat("\t", grep(x, y), 'of', length(y),':',x, '\n')
      Site = stringr::str_split(x, '/') %>% unlist() %>% .[length(.)] %>%
        stringr::str_split(., '[.]') %>% unlist() %>% .[1]
      Start = readxl::read_excel(x, sheet = 'Fieldbook', col_names = TRUE) %>%
        mutate(., IDX = c(1:nrow(.)))
      Start = subset(Start, Plot == '1', select = c('IDX')) %>% as.numeric(.)-2
      ydata = readxl::read_excel(x, sheet = 'Fieldbook', skip = Start, col_names = TRUE)
      pedigreeName = names(ydata)[min(grep('pedigree', names(ydata), ignore.case = TRUE))]
      if(all(!grepl('stockid', names(ydata), ignore.case = TRUE)))
        ydata = dplyr::mutate(ydata, StockID = Stock)
      ydata = dplyr::mutate(ydata, Plot = as.numeric(Plot), Site = Site) %>%
        subset(., !is.na(Plot))

      davail = subset(ydata, select = c('Plot', grep('Wght', names(ydata),v=T, ignore.case = T),
                                        grep('weight', names(ydata),v=T, ignore.case = T),
                                        grep('field', names(ydata),v=T, ignore.case = T),
                                        grep('grain', names(ydata),v=T, ignore.case = T))) %>%
        reshape2::melt(., id = 'Plot') %>%
        subset(., !is.na(value)) %>% nrow()

      ydata = subset(ydata, select = c("Site", "Rep", "Entry", "StockID", pedigreeName)) %>%
        as.data.frame(.) %>% dplyr::mutate(., Rep = as.numeric(Rep), Entry = as.numeric(Entry))
      names(ydata) <- c("Site", "Rep", "Entry", "StockID", "Pedigree")
      ydata = mutate(ydata, davail=davail, file= x)
      return(ydata)
    }) %>% do.call(rbind, .)

    xx = subset(y_data, davail == 0 & !duplicated(file), select = file) %>%
      as.vector(.) %>% .[['file']]
    y_data = subset(y_data, select = - c(davail, file))
    ydata = dplyr::mutate(y_data, Rep = ifelse(!is.na(Rep), 1, NA)) %>%
      aggregate(Rep ~ Entry + StockID + Pedigree + Site,
                data = ., function(x) sum(x, na.rm = TRUE)) %>%
      reshape(., direction = 'wide', idvar = c("Entry", "StockID", "Pedigree"), timevar = 'Site')
    names(ydata) = gsub('Rep.', '', names(ydata))

    cat(paste("\n\tChecking trial entries with missing information\n"))
    missingStocks = subset(ydata, (is.na(StockID) | StockID == '' | StockID == " ") &
                             (is.na(Pedigree) | Pedigree == '' | Pedigree == " "))

    if(nrow(missingStocks) == 0)
      cat("\t\tNo entries with missing information\n")

    cat("\n\tChecking duplicates\n")
    duplicateStocks = subset(ydata, duplicated(paste0(Entry, StockID, Pedigree)))
    if(nrow(duplicateStocks) == 0) cat("\t\tNo duplicated entries in the compiled data\n")

    cat(rep('-', 80))

    cat('\nGrouping entries\n')
    y_data = dplyr::mutate(y_data, id = paste0(Entry, StockID, Pedigree))
    groups = as.data.frame(cbind(id ='', Entry='', StockID='', Pedigree='', site='', siteno = ''))

    for(x in unique(as.character(y_data$id))){
      if(c(1:length(unique(as.character(y_data$id))))[unique(as.character(y_data$id)) %in% as.character(x)] %% 50 == 0)
        cat('\t', c(1:length(unique(as.character(y_data$id))))[unique(as.character(y_data$id)) %in% as.character(x)],'of',
            length(unique(as.character(y_data$id))),x,'...\n')

      if(!x %in% as.character(groups$id)){
        sites = reshape2::melt(y_data, id = c('id', 'Entry', 'StockID', 'Pedigree')) %>%
          subset(., as.character(id) == x & variable == 'Site' & !is.na(value)) %>%
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
      groups = subset(groups, id !='' & id != ' ' & !is.na(id))
    }
    groups = dplyr::mutate(groups, sitegroup = as.integer(as.factor(siteno)))

    subdir = as.character(unique(groups$sitegroup))
    sapply(subdir, function(x)
      if(!dir.exists(paste0(dir, "/trials/Site-", x)))
        dir.create(paste0(dir, "/trials/Site-", x))
    )

    if(!dir.exists(paste0(dir, "/trials/backup"))) dir.create(paste0(dir, "/trials/backup"))
    if(!dir.exists(paste0(dir, "/trials/excluded"))) dir.create(paste0(dir, "/trials/excluded"))

    sapply(y, function(x) file.copy(x, paste0(dir, "/trials/backup")))
    sapply(xx, function(x) file.copy(x, paste0(dir, "/trials/excluded")))
    cat(paste("\n\tCopied trial files to the trial backup folder\n"))

    cat(paste("\n\tCopying trial files to the trial respective trial folder\n"))
    sapply(subdir, function(x){
      x1 = subset(groups, as.character(sitegroup) == as.character(x)) %>%
        dplyr::mutate(., site = as.character(site)) %>%
        .[['site']] %>% unique(.) %>%
        sapply(., function(z) grepl(z, y)) %>%
        as.data.frame(.) %>% apply(., 1, sum) %>%
        sapply(., function(z) ifelse(z >= 1, TRUE, FALSE)) %>%
        y[.] %>% .[!(. %in% xx)]

      if(length(x1)>0){
        sapply(x1, function(z){
          file.copy(z, paste0(dir, "/trials/Site-", x))
          unlink(z)
        })
      }
    })
    correctTrialNames(trialsRootFolder = paste0(dir, "/trials"))

  }
  cat(paste("\tDeleting trials files from the subfolder", dir, "\n"))
  cat(rep('-', 80), "\n")

  cat("Summary information:")
  dirs = list.dirs(path = dir, full.names = FALSE, recursive = TRUE) %>%
    subset(., . !="" & . !="." & . !=".." & !grepl('backup', ., ignore.case = TRUE))
  sapply(dirs,  function(x){
    if(length(list.files(path = paste0(dir, "/", x))) == 0){
      unlink(paste0(dir, "/", x), recursive = TRUE)
      cat("\n\t", x, "No relevant folders found")
    }
    else cat("\n\t", x, ":\t", length(list.files(path = paste0(dir, "/", x))))
  })

  if(dir.exists(paste0(dir, "/", inventories))) inventories = inventories
  else inventories=NULL

  if(dir.exists(paste0(dir, "/", nurseries))) nurseries = nurseries
  else nurseries=NULL

  if(dir.exists(paste0(dir, "/", trials))) trials = trials
  else trials=NULL

  metadata(separate_files_function = TRUE, inventories = inventories, nurseries = nurseries)
}
