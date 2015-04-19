#' An implementation of a data-step
#' 
#' @docType class
#' @export
#' @importFrom pipeR %>>%
#' @format An \code{\link{R6Class}} generator object
#' 
#' @section Fields:
#'   \describe{
#'     \item{\code{i}}{An index variable}}
#'   
#'   \describe{
#'     \item{\code{envir}}{An environment for evaluation}}
#'   
#'   \describe{
#'     \item{\code{results}}{A dataframe of results}}
#'     
#' @section Methods:
#'   \describe{\item{\code{set(df)}}{Import all values in row 
#'     \code{self$i} of \code{df} into \code{self$env}}}
#'
#'   \describe{\item{\code{lag_(.dots)}}{\code{\link{select_}(.dots)} from the last row of 
#'     \code{self$results} and import into \code{self$env}}}
#'     
#'   \describe{\item{\code{lag(...)}}{\code{\link{select}(...)} from the last row of 
#'     \code{self$results} and import into \code{self$env}}}
#'     
#'   \describe{\item{\code{output_(.dots)}}{Convert the environment to a dataframe,  
#'     using only vectors, \code{\link{select_}(.dots)} and \code{\link{bind_cols}} 
#'     onto \code{self$results}}}
datastep = R6::R6Class(
  "datastep",
  public = list(
    
    envir = new.env(),
    
    results = data.frame(),
    
    i = 1,
    
    set = function(df)
      df[self$i,] %>>%
      list2env(self$envir),
    
    lag_ = function(.dots)
      if (self$i > 1) {
        
        self$results[nrow(self$results),] %>>%
        dplyr::select_(.dots = .dots) %>>%
        list2env(self$envir)} else {
          
          nulls = rep(NA, length(.dots))
          names(nulls) = names(.dots)
          list2env(as.list(nulls), self$envir)},
    
    lag = function(...) {
      .dots = lazyeval::lazy_dots(...)
      self$lag_(.dots)},
    
    output_ = function(.dots = NULL) {
      
      row = self$envir %>>%
        as.list %>>%
        magrittr::extract(sapply(., function(x) is.vector(x))) %>>%
        as.data.frame
      
      if(length(.dots) > 0)
         row = row %>>% dplyr::select_(.dots = .dots)

      self$results = dplyr::bind_rows(self$results, row)
      return(NULL)},

    output = function(...) {
      .dots = lazyeval::lazy_dots(...)
      
      self$output_(.dots)}))

teststep = datastep$new()