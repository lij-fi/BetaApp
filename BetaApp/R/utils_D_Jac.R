#' D_Jac
#'
#' @description This Function Calculates the Jaccards distance between
#' issues/Flags. It returns Jaccards distance along with
#' the Raw counts for each flag combination. It calls How_Partisan
#' which retruns the counts broken down by independent, republican,
#' and democrat.
#'
#' @return The return value, if any, from executing the utility.
#'
#' @noRd
D_Jac<-function(N_COUNT, issues_Internal, ALL_Probs)
{
  issues1<-issues_Internal[1,1]
  issues2<-issues_Internal[2,1]


  mini_1<-ALL_Probs %>% filter(Issue==issues1) %>% mutate_at(c("Flag"), as.character)
  mini_2<-ALL_Probs %>% filter(Issue==issues2) %>% mutate_at(c("Flag"), as.character)
  N_COUNT<-N_COUNT %>%  mutate_if(is.factor, as.character)
  #print(N_COUNT)
  #print(mini_1)

  Mer_2<-N_COUNT %>% rowwise() %>%
    mutate(Similarity=Total/(with(mini_1, sum(n[Flag==.data[[issues1]]]))+with(mini_2, sum(n[Flag==.data[[issues2]]]))-Total))


  return(Mer_2)

}
