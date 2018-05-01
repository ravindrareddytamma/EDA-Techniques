impute.na <- function(colname)
{
  if(!is.numeric(colname))
    stop("Required Numeric Column as Input!")
  suppressWarnings(require(zoo))
  return(na.approx(colname))
}
