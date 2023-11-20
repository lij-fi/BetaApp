Voter_Type<-function(VOTE_VEC)
{
  if (identical(as.vector(VOTE_VEC[1:2]),c("0","1")))
  {
    if(VOTE_VEC[5]=="Democratic")
    {
      return("Lost Dem")
    }
    else if(VOTE_VEC[5]=='Republican')
    {
      return('Lost Republican')
    }
    else if(VOTE_VEC[5]=="Non-Partisan")
    {
      return('Lost Non-Partisan')
    }
    else
    {
      return("Other")
    }
  }
  else if (identical(as.vector(VOTE_VEC[1:4]),c("0","0","0","0")))
  {
    if(VOTE_VEC[5]=="Democratic")
    {
      return("Non-Voting Dem")
    }
    else if(VOTE_VEC[5]=='Republican')
    {
      return('Non-Voting Rep')
    }
    else if(VOTE_VEC[5]=="Non-Partisan")
    {
      return('Non-Voting NP')
    }
    else
    {
      return('Other')
    }

  }
  else if (identical(as.vector(VOTE_VEC[1:3]),c("1","1","1")))
  {
    if(VOTE_VEC[5]=="Democratic")
    {
      return("Reliable Dem")
    }
    else if(VOTE_VEC[5]=='Republican')
    {
      return('Reliable Rep')
    }
    else if(VOTE_VEC[5]=="Non-Partisan")
    {
      return('Reliable NP')
    }
    else
    {
      return('Other')
    }

  }
  else if (identical(as.vector(VOTE_VEC[1:2]),c("1","1")) || identical(as.vector(VOTE_VEC[1:3]),c("1","0","1")))
  {
    if(VOTE_VEC[5]=="Democratic")
    {
      return("Reliable Dem")
    }
    else if(VOTE_VEC[5]=='Republican')
    {
      return('Reliable Rep')
    }
    else if(VOTE_VEC[5]=="Non-Partisan")
    {
      return('Reliable NP')
    }
    else
    {
      return('Other')
    }

  }
  else
  {
    if(VOTE_VEC[5]=="Democratic")
    {
      return("Other")
    }
    else if(VOTE_VEC[5]=='Republican')
    {
      return('Other')
    }
    else if(VOTE_VEC[5]=="Non-Partisan")
    {
      return('Other')
    }
    else
    {
      return('Other')
    }
  }


}
