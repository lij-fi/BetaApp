#' Prop_Order
#'
#' @description Reorders the Factors from the order given in the COLUMN_NAMES File
#'
#' @return The return value, if any, from executing the utility.
#'
#' @noRd
Prop_Order<-function(fac_one, COLUMN_NAMES)
{
  tst66<-levels(fac_one)
  for(i in 1:nrow(COLUMN_NAMES))
  {
    tst65<-c(as.character(COLUMN_NAMES[i,5:(4+length(tst66))]))
    if(setequal(tst65,tst66))
    {
      fac_one<-factor(fac_one, levels = tst65)
      return(fac_one)
    }
  }
  print("This one broke")
  return(fac_one)
}
