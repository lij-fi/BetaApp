Make_Venn<-function(issueAV, issueBV, Lev1, Lev2, population_internal){

  # Gets the Counts for each flag and puts them into tst11 and then gets the counts
  # for people that have both flags.
  tst11 <- population_internal %>% filter(.data[[issueAV]]=={{Lev1}}) %>% select(LALVOTERID)
  tst12 <- population_internal %>% filter(.data[[issueBV]]=={{Lev2}}) %>% select(LALVOTERID)
  tst13 <-  population_internal %>% filter(.data[[issueAV]]=={{Lev1}} & .data[[issueBV]]=={{Lev2}}) %>% select(LALVOTERID)


  # generates the Venn diagram grid object and returns it.
  return(draw.pairwise.venn(area1 = nrow(tst11), area2 = nrow(tst12), cross.area = nrow(tst13), category =(c(Lev1, Lev2)), fill = c("blue", "red"), alpha = .5, ind=FALSE))

}
