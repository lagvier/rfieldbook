#' ksuCSV
#' @title Prepare CSV files for trials for KSU use
#' Funtion to convert trial files to csv for KSU fieldbook field files
#' @keywords fieldbook, trial, ksu, breeding, data collection, handheld
#' @export

ksuCSV <- function(){
  csv_time = format(Sys.time(), "CSV_%Y%m%d_%H%M")
  trialSheets = c('fieldbook', 'master', 'results')
  nurserySheets = c('stocklist', 'entrylist')

  typeSelected = dlg_list(choices = c("File(s)", "Folder"), preselect = FALSE,
    multiple = FALSE, title = "Select appropriately"
  )
  typeSelected = as.character(typeSelected$res)

  if(tolower(typeSelected) == "folder"){
    DirFile <- choose.dir(caption = 'Select directory')
  }
  if(tolower(typeSelected) == "file(s)"){
    DirFile <- choose.files(caption = "Select files", multi = TRUE,
                            filters = Filters[c(".xls$")])
  }
  if(!tolower(typeSelected) %in% c( "file(s)", "folder")){
    stop("Nothing selected!")
  }

  DirFile = gsub('[\\]', '/', as.character(DirFile))

  if(dir.exists(DirFile)){
    filesInDir = list.files(DirFile, pattern = '.xls$',
                            ignore.case = T, recursive = T, full.names = T)
    filesInDir = filesInDir[sapply(filesInDir, function(x){
      return(all(trialSheets %in% tolower(readxl::excel_sheets(x))) |
               all(nurserySheets %in% tolower(readxl::excel_sheets(x))))
    })]

    Path = DirFile

    if(length(filesInDir) == 0 ) stop("No trial file in the selected directory!")
  }

  if(!dir.exists(DirFile) & length(DirFile)>0){

    Path = commonDir(DirFile)


    filesInDir = DirFile[sapply(DirFile, function(x){
      return(all(trialSheets %in% tolower(readxl::excel_sheets(x))) |
               all(nurserySheets %in% tolower(readxl::excel_sheets(x))))
    })]
    if(length(filesInDir) == 0 ) stop("None of the selected file is a trial file!")
  }

  sapply(filesInDir, function(currentFile){
    File = strsplit(currentFile, "[/]") %>% unlist(.) %>% .[length(.)] %>%
      strsplit(., "[.]") %>% unlist(.) %>% .[1:(length(.)-1)] %>%
      paste(., collapse = ".") %>% paste0(., '.csv')

    if(all(trialSheets %in% tolower(readxl::excel_sheets(currentFile)))){
          currentFileData <- readxl::read_excel(currentFile, col_names = T,
                                                sheet = "Fieldbook")
          currentFileData <- subset(currentFileData, select = c(
            grep('rep', names(currentFileData), v = T, ignore.case = T)[1],
            grep('bloc', names(currentFileData), v = T, ignore.case = T)[1],
            grep('plot', names(currentFileData), v = T, ignore.case = T)[1],
            grep('entry', names(currentFileData), v = T, ignore.case = T)[1],
            grep('pedigree', names(currentFileData), v = T, ignore.case = T)[1],
            grep('origin', names(currentFileData), v = T, ignore.case = T)[1]
          ))
          dataVars = names(currentFileData)
          names(currentFileData) = c('rep','bloc','plot','entry','ped','origin')
          currentFileData <- mutate(currentFileData,
                                    rep = as.integer(rep),
                                    bloc = as.integer(bloc),
                                    plot = as.integer(plot),
                                    entry = as.integer(entry)) %>%
            subset(., !is.na(plot) & !is.na(entry) & !is.na(rep)) %>%
            mutate(., SN = as.integer(c(nrow(.):1))) %>%
            arrange(SN) %>%
            subset(., !duplicated(plot), select = -SN) %>%
            arrange(plot)
          names(currentFileData) = dataVars
    }

    if(all(nurserySheets %in% tolower(readxl::excel_sheets(currentFile)))){
      currentFileData <- readxl::read_excel(currentFile, col_names = T,
                                            sheet = "Entrylist")
      currentFileData <- subset(currentFileData, select = c(
        grep('entry', names(currentFileData), v = T, ignore.case = T)[1],
        grep('stock', names(currentFileData), v = T, ignore.case = T)[1],
        grep('name', names(currentFileData), v = T, ignore.case = T)[1],
        grep('pedigree', names(currentFileData), v = T, ignore.case = T)[1],
        grep('origin', names(currentFileData), v = T, ignore.case = T)[1]
      ))
      dataVars = names(currentFileData)
      names(currentFileData) = c('entry','stockid','name','pedigree','origin')
      currentFileData <- mutate(currentFileData,
                                entry = as.integer(entry)) %>%
        subset(., !is.na(entry)) %>%
        mutate(., Plot = c(1:nrow(.)))

      names(currentFileData) = c(dataVars,'Plot')
      currentFileData = subset(currentFileData, select= c('Plot',dataVars))
      dataVars = names(currentFileData)
    }

    currentFileData = mutate(currentFileData,
                             RowID = replicate(nrow(currentFileData),
                                               UUIDgenerate())) %>%
      subset(., select = c('RowID', dataVars))

    if(!dir.exists(file.path(Path, csv_time)))
      dir.create(file.path(Path, csv_time))

    write.csv(currentFileData, file = file.path(Path, csv_time, File),
              row.names = F, na='')
  })

  cat(paste0("process completed ... \n\tCheck results in `", file.path(Path, csv_time), "`\n"))
}











