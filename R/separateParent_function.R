#' separateParent
#' @title utility function for separating the names for male and female from a pedigree name based on defined nomenclature
#' @keywords fieldbook, trial, nursery, inventory
#' @param pedigreeName string for the pedigree name
#' @export


separateParent = function(pedigreeName){

  breedResults = typeBreeding(pedigreeName)

  if(!is.null(breedResults[[3]])){

    outNest = length(breedResults[[3]])
    inNest =length(breedResults[[3]])
    dMatrix = matrix("", nrow = outNest, ncol = inNest)


    for(i in 1:outNest){
      for(j in 1:inNest){
        if(i == 1){
          dMatrix[i,j] = breedResults[[3]][[i]][j]
        }else{
          ca= grep(gsub('itsReplaced', dMatrix[i-1,j], breedResults[[3]][[i]]),
                   breedResults[[1]], fixed = TRUE)
          if(length(ca) == 0){
            dMatrix[i,j] = dMatrix[i-1,j]
          }else{
            dMatrix[i,j] = gsub('itsReplaced', dMatrix[i-1,j], breedResults[[3]][[i]])[min(ca)]
          }
        }
      }
    }

    sn = ifelse(grepl('//', breedResults[[2]]), '//',
                ifelse(grepl('&', breedResults[[2]]), '&',
                       ifelse(grepl('/', breedResults[[2]]), '/'," ")))
    y = unlist(strsplit(breedResults[[2]], sn))
    for(r in 1:inNest){
      if(grepl('itsReplaced', y[1])) {
        y[1] = gsub('itsReplaced', dMatrix[outNest, r], y[1])
      }else{
        y[2] = gsub('itsReplaced', dMatrix[outNest, r], y[2])
      }
    }
  } else{
    y = ""
  }
  return(list(y))
}
