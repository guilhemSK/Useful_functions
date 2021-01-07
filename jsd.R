jsd <- function(data,names=rownames(data))
{
  # jsd computes the Jensen-Shannon divergence between the rows of data
  # data contains sample composition by row
  # data should not contain NAs
  # names contains sample names, if different from rownames(data)
  output.matrix = matrix(nrow = nrow(data), ncol = nrow(data),
                                        dimnames = list(names, names),
                                        data = 0)
  for (i in 2:nrow(data))
  {
    for (j in 1:(i-1))
    {
      support_distrib = data[i,] !=0 | data[j,] !=0
      # Checking that the support distribution is not empty (should always be the case unless both samples are empty),
      # and that both samples are non-empty, otherwise setting the JSD to NA.
      if (length(which(support_distrib)) > 0 && 
          sum(data[i,support_distrib]) > 0 &&
          sum(data[j,support_distrib]) > 0)
      {
        data_i = data[i,support_distrib]/sum(data[i,support_distrib])
        data_j = data[j,support_distrib]/sum(data[j,support_distrib])
        data_ij = (data_i + data_j)/2
        output.matrix[i,j] = output.matrix[j,i] = 
          sqrt(1/2*(KL.plugin(data_i,data_ij,unit="log2") + KL.plugin(data_j,data_ij,unit="log2")))
      } else
      {
        output.matrix[i,j] = output.matrix[j,i] = NA
      }
    }
  }
  return(output.matrix)
}
