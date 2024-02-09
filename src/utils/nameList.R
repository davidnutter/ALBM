## Borrowed from: https://raw.githubusercontent.com/jsta/nml/master/R/read.R
## as I needed to adjust this for our namelist

allowed_nml_extensions = c("nml", "bLake")

is_nml_file <- function(x){
 tools::file_ext(x) %in% allowed_nml_extensions
}

ascii_only <- function(file){
  response <- what_ascii(file)

  if (length(response) > 0){
    return(FALSE)
  } else {
    return(TRUE)
  }
}

#' @importFrom utils capture.output
what_ascii <- function(file){
  response <- capture.output(tools::showNonASCIIfile(file))
  return(response)
}

buildVal	<-	function(textLine, lineNum, blckName){
  #-----function appends nml list with new values-----
  # remove all text after comment string
  textLine	<-	strsplit(textLine,'!')[[1]][1]

  if (!any(grep("=", textLine))){
    stop(c("no hanging lines allowed in .nml, used ",textLine,'.\nSee line number:',lineNum,' in "&',blckName,'" section.'))
  }
  params	<-	strsplit(textLine,"=") # break text at "="
  parNm	  <-	params[[1]][1]
  parVl	  <-	params[[1]][2]
  # figure out what parval is...if string, remove quotes and keep as string
  # ***for boolean text, use "indentical" so that 0!= FALSE
  # can be: string, number, comma-sep-numbers, or boolean

  # special case for date:
  if (is.na(parVl)){
    stop('Empty values after "', textLine, '" on line ', lineNum,
         '. \nPerhaps the values are on the next line?', call. = FALSE)
  }
  if (nchar(parVl>17) & substr(parVl,14,14)==':' & substr(parVl,17,17)==':'){
    parVl<-paste(c(substr(parVl,1,11),' ',substr(parVl,12,nchar(parVl))),collapse='')
  }
  if (any(grep("'",parVl))){

    parVl	<-	gsub("'","",parVl)
  }else if (any(grep("\"",parVl))){
    parVl  <-	gsub("\"","",parVl)
  }else if (isTRUE(grepl(".true.",parVl) || grepl(".false.",parVl))){
    logicals <- unlist(strsplit(parVl,","))
    parVl <- from.glm_boolean(logicals)
  }else if (any(grep(",",parVl))){	# comma-sep-nums
    parVl	<-	c(as.numeric(unlist(strsplit(parVl,","))))
  }else {	# test for number
    ret <- base::tryCatch(as.numeric(parVl), error = function(e) NULL)
    if (is.na(ret)) { # no success
        # fallback value: keep as string; nothing to do
    } else {
        parVl	<-	as.numeric(parVl)
    }
  }
  lineVal	<-	list(parVl)
  names(lineVal)	<-	parNm
  return(lineVal)
}

#' go from glm2.nml logical vectors to R logicals
#'
#' @param values a vector of strings containing either .false. or .true.
#' @return a logical vector
#' @keywords internal
from.glm_boolean <- function(values){

  logicals <- sapply(values, FUN = function(x){
    if (!isTRUE(grepl(".true.", x) || grepl(".false.", x))){
      stop(x, ' is not a .true. or .false.; conversion to TRUE or FALSE failed.',
           call. = FALSE)
    }
    return(ifelse(isTRUE(grepl(".true.", x)), TRUE, FALSE))
  })
  return(as.logical(logicals))
}

to.glm_boolean <- function(values){
  val.logical <- values
  values[val.logical] <- '.true.'
  values[!val.logical] <- '.false.'
  return(values)
}

.nml <- function(list_obj){
  nml <- list_obj
  class(nml) <- "nml"
  invisible(nml)
}
#'@title read in an *.nml file
#'@description
#'read in na nml file and create a list.  \cr
#'
#'
#'@param nml_file a string with the path to an nml file
#'@return an nml list
#'@author
#'Jordan S. Read
#'@examples \dontrun{
#'fpath <- system.file("extdata/glm2.nml", package = "nml")
#'sim_nml <- read_nml(fpath)
#'print(sim_nml)
#'}
#'@export
read_nml  <-	function(nml_file){
  nml_file <- nml_path_norm(nml_file)

  if (!ascii_only(nml_file)){
    stop('non-ASCII characters found in nml file on line ', what_ascii(nml_file))
  }
  # skip all commented lines, return all variables and associated values
  # requires NO return line variables (all variables must be completely defined on a single line)
  c <- file(nml_file,"r")
  fileLines <- readLines(c)
  close(c)
  lineStart	<-	substr(trimws(fileLines, which = 'left'),1,1)
  # ignore comment lines or lines of white space only
  ignoreLn	<-	lineStart=='!' | fileLines=="" | fileLines=="\t"
  lineStart	<-	lineStart[!ignoreLn]
  fileLines	<-	fileLines[!ignoreLn]
  # find all lines which start with "&" * requires FIRST char to be value

  lineIdx		<- seq(1,length(lineStart))
  blckOpen	<-	lineIdx[lineStart=="&"]
  blckClse	<-	lineIdx[lineStart=="/"]


  nml <- list()
  for (i in 1:length(blckOpen)){
    blckName	<-	substr(fileLines[blckOpen[i]],2,nchar(fileLines[blckOpen[i]]))
    blckName <- gsub("\\s", "", blckName)
    if (grepl("!", blckName)) { # remove everything from first "!" if present in block name
        blckName <- base::substr(blckName, 1, regexpr("!", blckName)-1)
    }
    oldNms	<-	names(nml)
    nml[[i]]	<-	list()
    names(nml)	<-	c(oldNms,blckName)

    carryover = ''

    for (j in (blckOpen[i]+1):(blckClse[i]-1)){

      textLine	<-	paste(carryover, gsub("\t","",gsub(" ","",fileLines[j])), sep='')
      if(substr(textLine,1,1)!='!'){
        # Add a check here, sometimes, if there is a hanging comma,
        #and only sometimes that means add next row
        if(substr(textLine,nchar(textLine), nchar(textLine)) == ',' &&
           j+1 <= length(fileLines) && !any(grep("=",fileLines[j+1])) && !any(grep("/",fileLines[j+1]))){

          carryover = textLine
          next
        }else{
          carryover = ''
        }
        # else, line is commented out
        lineVal	<-	buildVal(textLine, lineNum=j, blckName)
        nml[[i]]	<-	c(nml[[i]],lineVal)
      }
    }
  }
  nml <- .nml(nml)
  return(nml)
}

nml_path_norm <- function(nml_file){
  if (!is_nml_file(nml_file)){
    warning(nml_file, ' is not of file type .nml')
  }

  return(nml_file)
}
