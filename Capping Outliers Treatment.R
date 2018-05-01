# Outliers Treatement using Capping

capping.outliers <- function(colname)
{
  if(!is.numeric(colname))
    stop("Required Numeric Column as Input!")
  low.ind <- which(colname < quantile(colname,probs = 0.25) - 1.5 * IQR(colname))
  colname[low.ind] <- quantile(colname,probs = 0.01)
  high.ind <- which(colname > quantile(colname,probs = 0.75) + 1.5 * IQR(colname))
  colname[high.ind] <- quantile(colname,probs = 0.99)
  return(colname)
}