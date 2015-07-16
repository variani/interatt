
#' @export
infogain2 <- function(C, A) entropy(A) - condentropy(A, C)

#' @export
infogain3 <- function(C, A, B) condinformation(A, B, C) - mutinformation(A, B)

#' @export
int2 <- function(X, S)
{
  ### par
  stopifnot(all(laply(X, class) == "factor"))
  stopifnot(class(S) == "factor")

  H_S <- entropy(S)
  nlevels_S <- nlevels(S)
  
  tab <- ldply(1:ncol(X), function(i) {
    IG_i <- infogain2(S, X[, i])
    IGrel_i <- IG_i / H_S
    
    n <- nrow(X)
    chi <- 2 * n * IG_i
    df <- (nlevels(X[, i]) - 1) * (nlevels_S - 1)
    pval <- pchisq(chi, df,  lower.tail = FALSE)
    
    data.frame(Var = names(X)[i], IG = IG_i, IGrel = IGrel_i,
      n = n, chi = chi, df = df, pval = pval, stringsAsFactors = FALSE)
  })
  
  # pval code
  tab <- mutate(tab,
    pval.code = ifelse(pval < 0.001, "***", ifelse(pval < 0.01, "**", ifelse(pval < 0.05, "*", ifelse(pval < 0.1, ".", "")))))
  
  # order  
  ord <- with(tab, order(pval))
  tab <- tab[ord, ]
  
  return(tab)  
}

#' @export
int3 <- function(X, S)
{
  ### par
  stopifnot(all(laply(X, class) == "factor"))
  stopifnot(class(S) == "factor")

  H_S <- entropy(S)
  nlevels_S <- nlevels(S)

  mat <- t(combn(1:ncol(X), 2))
  
  tab <- ldply(1:nrow(mat), function(k) {
    i <- mat[k, 1]
    j <- mat[k, 2]
    
    IG_k <- infogain3(S, X[, i], X[, j])
    IGrel_k <- IG_k / H_S
    
    n <- nrow(X)
    chi <- 2 * n * abs(IG_k)
    df <- (nlevels(X[, i]) - 1) * (nlevels(X[, j]) - 1) * (nlevels_S - 1)
    pval <- pchisq(chi, df,  lower.tail = FALSE)
    
    data.frame(Var1 = names(X)[i], Var2 = names(X)[j], IG = IG_k, IGrel = IGrel_k,
      n = n, chi = chi, df = df, pval = pval, stringsAsFactors = FALSE)
  })
  
  # pval code
  tab <- mutate(tab,
    pval.code = ifelse(pval < 0.001, "***", ifelse(pval < 0.01, "**", ifelse(pval < 0.05, "*", ifelse(pval < 0.1, ".", "")))))
  
  # order  
  ord <- with(tab, order(pval))
  tab <- tab[ord, ]

  return(tab)
}
