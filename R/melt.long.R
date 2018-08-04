## Transform data.frame to long form
melt.long <- function(data, select, group){
  if(missing(select)) select <- seq_len(ncol(data))
  stopifnot(!is.null(colnames(data)))
  vars <- colnames(data)[select]
  if(!missing(group)){
    stopifnot(length(group) == nrow(data))
    res <- data.frame(group = rep(group, ncol(data[,select])),
                      value = do.call(c, data[,select]),
                      variable = factor(rep(vars, each = nrow(data)),
                                        levels = vars))
  }else{
    res <- data.frame(value = do.call(c, data[,select]),
                      variable = factor(rep(vars, each = nrow(data)),
                                        levels = vars))
  }
  res
}
