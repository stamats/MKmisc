thyroid <- function(TSH, fT3, fT4, TSHref, fT3ref, fT4ref){
  stopifnot(is.numeric(TSH) | length(TSH) != 1)
  stopifnot(is.numeric(fT3) | length(fT3) != 1)
  stopifnot(is.numeric(fT4) | length(fT4) != 1)
  stopifnot(is.numeric(TSHref) | length(TSHref) != 2)
  stopifnot(is.numeric(fT3ref) | length(fT3ref) != 2)
  stopifnot(is.numeric(fT4ref) | length(fT4ref) != 2)

  if(TSHref[1] >= TSHref[2])
    stop('TSH: Lower references value larger or equal than upper reference value!')
  if(fT3ref[1] >= fT3ref[2])
    stop('fT3: Lower references value larger or equal than upper reference value!')
  if(fT4ref[1] >= fT4ref[2])
    stop('fT4: Lower references value larger or equal than upper reference value!')

  df <- data.frame(Value = 100*c((TSH-TSHref[1])/(TSHref[2]-TSHref[1]),
                                 (fT3-fT3ref[1])/(fT3ref[2]-fT3ref[1]),
                                 (fT4-fT4ref[1])/(fT4ref[2]-fT4ref[1])),
                   Parameter = c("TSH", "fT3", "fT4"))
  fig <- ggplot(data = df, aes_string(x = "Parameter", y = "Value", fill = "Parameter")) +
    geom_col(width = 0.5) + ylim(0, 100) + xlab("") +
    geom_hline(yintercept = c(40, 60)) + coord_flip() +
    ylab("Relative value [%]") +
    annotate(geom = "text", x = 3.5, y = 20, label = "too low", color = "red") +
    annotate(geom = "text", x = 3.5, y = 50, label = "O.K.", color = "darkgreen") +
    annotate(geom = "text", x = 3.5, y = 80, label = "too high", color = "red") +
    ggtitle("Thyroid Function")
  print(fig)
  invisible(df)
}
