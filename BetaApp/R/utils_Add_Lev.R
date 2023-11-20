#' Add_Lev
#'
#' @description replaces blank or NA cells with "Unknown" as a factor Level.
#'
#' @return The return value, if any, from executing the utility.
#'
#' @noRd
Add_Lev<-function(x)
{

  levels(x)<-c(levels(x), "Unknown")
  droplevels(replace(x,x=="","Unknown"))
  droplevels(replace(x,is.na(x),"Unknown"))

}
