# MSc Thesis
# 23/02/2021
# Subpixel confusion matrix (SCM) function

# Implementation following https://doi.org/10.1016/j.rse.2007.07.017


# Function for calculating a confusion matrix
SCM = function(predicted, observed, agreement=min, disagreement="SCM", 
               scale=FALSE, accuracy=FALSE, totals=FALSE, plot=FALSE){
  
  # Aliases; TODO: remove/rename
  s_nk = predicted
  r_nl = observed
  
  # Sanity checks: the input must be 0-1 range and add up to 100%
  stopifnot(all(r_nl >= 0-options()$ts.eps))
  stopifnot(all(r_nl <= 1+options()$ts.eps))
  stopifnot(all(s_nk >= 0-options()$ts.eps))
  stopifnot(all(s_nk <= 1+options()$ts.eps))
  if (is.vector(s_nk))
    stopifnot(all(round(sum(s_nk), 6) == 1))
  else
    stopifnot(all(round(rowSums(s_nk), 6) == 1))
  if (is.vector(r_nl))
    stopifnot(all(round(sum(r_nl), 6) == 1))
  else
    stopifnot(all(round(rowSums(r_nl), 6) == 1))
  
  if (is.character(disagreement) && disagreement == "SCM") # Mean of MIN_D and LEAST_D + confusion
  {
    p_min = Comparator(s_nk, r_nl, A=agreement, D=MIN_D, scale=scale)
    p_least = Comparator(s_nk, r_nl, A=agreement, D=LEAST_D, scale=scale)
    P_kl = (p_min + p_least)/2
    U_kl = (p_min - p_least)/2
  } else {
    P_kl = Comparator(s_nk, r_nl, A=agreement, D=disagreement, scale=scale)
    U_kl = matrix(0, nrow=nrow(P_kl), ncol=ncol(P_kl), dimnames=dimnames(P_kl)) # No uncertainties for other types; U is a zero matrix
  }
  
  scm = structure(list(P=P_kl, U=U_kl, agreement=as.character(substitute(agreement)), disagreement=as.character(substitute(disagreement))), class="scm")
  
  if (plot)
    plot(scm)
  
  if (accuracy || totals)
    scm = accuracy.scm(scm)
  
  if (totals)
    scm = totals.scm(scm)
  
  return(scm)
}



# Comparator function
Comparator = function(s_k, r_l, A=MIN, D=PROD_D, scale=FALSE)
{
  # If we have a data.frame, convert into a matrix
  if (is.data.frame(s_k))
    s_k = as.matrix(s_k)
  if (is.data.frame(r_l))
    r_l = as.matrix(r_l)
  
  stopifnot(is.numeric(s_k))
  stopifnot(is.numeric(r_l))
  stopifnot(!is.null(A) || !is.null(D))
  
  if (!is.null(A))
    A = match.fun(A)
  if (!is.null(D))
    D = match.fun(D)
  
  # If s_k and r_l are matrices/data.frames, we sum them all
  # If not, make it a matrix anyway to make the same code handle it
  if (is.vector(s_k))
    s_k = t(as.matrix(s_k))
  if (is.vector(r_l))
    r_l = t(as.matrix(r_l))
  
  stopifnot(all(dim(s_k) == dim(r_l)))
  
  CumulativeResult = matrix(0, ncol(s_k), ncol(r_l))
  for (n in 1:nrow(s_k))
  {
    s_nk = s_k[n,]
    r_nl = r_l[n,]
    
    K = length(s_nk)
    
    # Overestimation and underestimation
    sp_nk = s_nk - r_nl; sp_nk[sp_nk<0] = 0
    rp_nl = r_nl - s_nk; rp_nl[rp_nl<0] = 0
    
    Result = matrix(NA, K, K) #p_nkl
    for (k in 1:K)
    {
      for (l in 1:K)
      {
        if (k == l) { # Diagonal
          if (!is.null(A))
            Result[k,l] = A(s_nk[k], r_nl[l])
          else
            Result[k,l] = D(sp_nk, rp_nl, k, l)
        } else {
          if (!is.null(D))
            Result[k,l] = D(sp_nk, rp_nl, k, l)
          else
            Result[k,l] = A(s_nk[k], r_nl[l])
        }
      }
    }
    CumulativeResult = CumulativeResult + Result
  }
  rownames(CumulativeResult) = colnames(s_k)
  colnames(CumulativeResult) = colnames(r_l)
  if (scale)
    CumulativeResult = CumulativeResult / nrow(s_k)
  
  return(CumulativeResult)
}


plot.scm = function(scm, ...)
{
  if (requireNamespace("lattice", quietly = TRUE) && requireNamespace("gridExtra", quietly = TRUE))
  {
    P_plot = lattice::levelplot(t(scm$P), xlab="Observed", ylab="Predicted", sub="Centre values", ...)
    U_plot = lattice::levelplot(t(scm$U), xlab="Observed", ylab="Predicted", sub="Uncertainty", ...)
    gridExtra::grid.arrange(P_plot, U_plot, ncol=2)
  } else {
    stop("Unable to plot due to missing lattice or gridExtra packages")
  }
}

# Function for adding "total" columns/rows to the SCM
totals.scm = function(scm)
{
  stopifnot(class(scm) == "scm")
  
  if (is.null(scm[["P_row_totals"]]))
  {
    warning("Passed an SCM without calculating accuracy metrics. This will be done for you.")
    scm = accuracy.scm(scm)
  }
  
  P_kp = rowSums(scm$P)
  P_pl = colSums(scm$P)
  P_pp = sum(scm$P)
  
  U_kp = rowSums(scm$U)
  U_pl = colSums(scm$U)
  U_pp = sum(scm$U)
  
  # Add total rows/columns: with uncertainty
  P_FullMatrix = cbind(scm$P, total=scm$P_row_totals)
  P_FullMatrix = rbind(P_FullMatrix, total=c(scm$P_col_totals, scm$P_total))
  
  U_FullMatrix = cbind(scm$U, total=scm$U_row_totals)
  U_FullMatrix = rbind(U_FullMatrix, total=c(scm$U_col_totals, scm$U_total))
  
  # Add user/producer accuracy
  P_FullMatrix = cbind(P_FullMatrix, user.acc=c(scm$P_user_accuracy, NA))
  P_FullMatrix = rbind(P_FullMatrix, prod.acc=c(scm$P_producer_accuracy, NA, scm$P_overall_accuracy))
  
  U_FullMatrix = cbind(U_FullMatrix, user.acc=c(scm$U_user_accuracy, NA))
  U_FullMatrix = rbind(U_FullMatrix, prod.acc=c(scm$U_producer_accuracy, NA, scm$U_overall_accuracy))
  
  scm$P = P_FullMatrix
  scm$U = U_FullMatrix
  return(scm)
}

# Add accuracy metrics to an scm object
accuracy.scm = function(scm)
{
  stopifnot(class(scm) == "scm")
  
  P_kp = rowSums(scm$P)
  P_pl = colSums(scm$P)
  P_pp = sum(scm$P)
  
  U_kp = rowSums(scm$U)
  U_pl = colSums(scm$U)
  U_pp = sum(scm$U)
  
  P_OA_s = (P_pp*sum(diag(scm$P))) / (P_pp^2 - U_pp^2)
  U_OA_s = (U_pp*sum(diag(scm$P))) / (P_pp^2 - U_pp^2)
  
  P_UA_s = (diag(scm$P)*P_kp) / (P_kp^2 - U_kp^2) # Problems when we have uncertainty == prediction, division by 0
  U_UA_s = (diag(scm$P)*U_kp) / (P_kp^2 - U_kp^2) # In which case the diagonals are 0, so it's 0 by definition
  P_UA_s[is.nan(P_UA_s)] = 0
  U_UA_s[is.nan(U_UA_s)] = 0
  
  P_PA_s = (diag(scm$P)*P_pl) / (P_pl^2 - U_pl^2)
  U_PA_s = (diag(scm$P)*U_pl) / (P_pl^2 - U_pl^2)
  P_PA_s[is.nan(P_PA_s)] = 0
  U_PA_s[is.nan(U_PA_s)] = 0
  
  # Kappa coefficient
  # Expected proportion of agreement (i.e. when using the monkey method)
  P_e = sum(((P_pp^2 + U_pp^2)*(P_pl*P_kp + U_pl*U_kp) - 2 * P_pp*U_pp*(U_pl*P_kp + P_pl*U_kp)) / (P_pp^2-U_pp^2)^2)
  U_e = sum((2 * P_pp*U_pp*(P_pl*P_kp + U_pl*U_kp) - (P_pp^2 + U_pp^2)*(U_pl*P_kp + P_pl*U_kp)) / (P_pp^2-U_pp^2)^2)
  
  Sign = (1-P_OA_s-U_OA_s)*(1-P_e-U_e)
  P_Kappa_s = ((P_OA_s-P_e) * (1-P_e) - (sign(Sign)*U_OA_s+U_e) * U_e)/((1-P_e)^2-U_e^2)
  U_Kappa_s = ((sign(Sign)*(1-P_OA_s)*U_e + (1-P_e)*U_OA_s)/((1-P_e)^2-U_e^2))
  
  scm[["P_row_totals"]] = P_kp
  scm[["P_col_totals"]] = P_pl
  scm[["P_total"]] = P_pp
  scm[["U_row_totals"]] = U_kp
  scm[["U_col_totals"]] = U_pl
  scm[["U_total"]] = U_pp
  scm[["P_overall_accuracy"]] = P_OA_s
  scm[["U_overall_accuracy"]] = U_OA_s
  scm[["P_user_accuracy"]] = P_UA_s
  scm[["U_user_accuracy"]] = U_UA_s
  scm[["P_producer_accuracy"]] = P_PA_s
  scm[["U_producer_accuracy"]] = U_PA_s
  scm[["P_user_accuracy"]] = P_UA_s
  scm[["P_kappa"]] = P_Kappa_s
  scm[["U_kappa"]] = U_Kappa_s
  
  return(scm)
}



# Agreement operators:

LEAST_D = function(sp_nk, rp_nl, k, l)
{
  max(sp_nk[k]+rp_nl[l]-sum(rp_nl), 0)
}

MIN_D = function(sp_nk, rp_nl, k, l)
{
  min(sp_nk[k], rp_nl[l])
}