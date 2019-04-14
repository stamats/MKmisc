pairwise.mod.t.test <- function(x, group, adjust.method = "BH", sort.by = "none"){
  if(missing(x)) stop("'x' is missing")
  if(!is.matrix(x)) stop("'x' must be a matrix")
  if(missing(group)) stop("'group' is missing")
  if(!is.factor(group)) group <- factor(group)
  if(ncol(x) != length(group))
    stop("length of group must be equal to ncol(x)")

  nlev <- nlevels(group)
  if(nlev < 3)
    stop("'group' has less than three levels, use 'mod.t.test' instead")

  group.tmp <- factor(group, labels = LETTERS[1:nlev])
  design <- model.matrix(~ 0 + group.tmp)
  colnames(design) <- levels(group.tmp)
  fit1 <- lmFit(x, design)
  combs <- apply(combn(levels(group.tmp), 2), 2, paste0, collapse = "-")
  cont.matrix <- makeContrasts(contrasts = combs, levels=design)
  fit2 <- contrasts.fit(fit1, cont.matrix)
  fit3 <- eBayes(fit2)
  combs1 <- apply(combn(levels(group), 2), 2, paste0, collapse = "-")
  for(i in 1:length(combs)){
    tmp <- topTable(fit3, coef = combs[i], adjust.method = adjust.method,
                    number = Inf, sort.by = sort.by)[,-2]
    names(tmp) <- c(combs1[i], paste(combs1[i], c("t", "p", "adj.p", "B"), sep = ": "))
    if(i == 1) res <- tmp else res <- cbind(res, tmp)
  }
  res
}
