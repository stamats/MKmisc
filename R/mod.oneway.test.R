mod.oneway.test <- function(x, group, adjust.method = "BH", sort.by = "none"){
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
  res <- topTableF(fit3, adjust.method = adjust.method, number = Inf,
                   sort.by = sort.by)
  levs <- levels(group)
  names(res) <- c(combs, "grand mean", "F", "p.value", "adj.p.value")
  res
}
