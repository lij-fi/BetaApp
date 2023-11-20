#' State_Formatting
#'
#' @description Reads in the Data from Florida. Not sure if this will be needed but going to put it in here. Used to be called in Global R
#'
#' @return The return value, if any, from executing the utility.
#'
#' @noRd
State_Formatting<-function(COLUMN_NAMES)
{
  CT<-paste(COLUMN_NAMES$TYPE, collapse ="")

  STATE<-readr::read_csv("C:\\Users\\eli31\\Desktop\\APP WD\\FL_ALL.csv", col_types=CT, n_max=10e5)

  STATE<-STATE %>% dplyr::mutate_if(is.factor, Add_Lev) %>%
   dplyr:: mutate_if(is.factor, Prop_Order, COLUMN_NAMES)


  colnames(STATE)<-COLUMN_NAMES$OUR_NAME
  haystack_issues<-colnames(STATE[39:102])
  #print(colnames(STATE))


  #STATE_SAMPLE<-slice_sample(STATE,n=10e3)

  #STATE<-slice_sample(STATE, n=1000000)

  Holer<- STATE %>% dplyr::group_by(.data[[haystack_issues[1]]]) %>%
    dplyr::count() %>%
    dplyr::rename(Flag=CHURCH_ATTENDANCE)

  Holer[,3]<-haystack_issues[2]
  Holer<-Holer %>% dplyr::rename(Issue='...3')


  return(STATE)

}
