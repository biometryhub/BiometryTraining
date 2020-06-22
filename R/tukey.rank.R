#' Rank means using Tukey's HSD
#'
#' Compares means pairwise using Tukey's Honest Significant Difference test
#'
#' @param Mean The predicted mean from an `ASreml-R` `predict` function call.
#' @param SED The Standard Error of Difference from an `ASreml-R` `predict` function call.
#' @param Names The treatment names.
#' @param crit.val The confidence level required.
#' @author Remy van de Ven
#'
#' @keywords internal
#'
#' @return A data frame with the predicted means, standard errors and Tukey's rankings.
#'
tukey.rank <- function(Mean, SED, Names = NULL, crit.val) {
    # .Deprecated(msg = "tuk.out has been replaced with mct.out and will be removed in a future version of this package.")

    # Label the means
    if (length(Names)==0)
        Names <- names(Mean)
    if (length(Names)==0)
    {
        t0 <- 10^(floor(log10(length(Mean)))+1)
        Names <- paste("M", substring(t0 + (1:length(Mean)),2), sep="")
    }

    # Rearrange arguments to correspond to ascending order.
    t1 <- order(Mean)
    Mean <- Mean[t1]
    SED <- SED[t1,t1]
    Names <- Names[t1]
    N <- length(Mean)

    # Determine pairs that are not significantly different
    Same <- abs(outer(Mean, Mean,"-")/SED) <= crit.val
    diag(Same) <- TRUE

    # Tukey Ranking
    # matrix R will be such that R[i,j] = 1 iff Mean[i] gets Symbol[j]
    R <- matrix(c(1, rep(0, N-1)), N)
    L <- 1

    # Step through the means, at each time TOTALLY IGNORING larger means
    for ( i in 2:N )
    {
        # Means not significantly different to Mean[i]
        t0 <- (1:(i-1))[ Same[1:(i-1), i] ]
        # Elements of t0 will be progressively removed
        if ( length(t0) == 0 )
        {
            L <- L + 1
            R <- cbind(R, 0)
            R[i,L] <- 1
        } else
        {
            j <- L
            while ( (length(t0) > 0) & (j >= 1) )
            {
                # Means allocated Symbol[j] so far
                t1 <- (1:(i-1))[ R[1:(i-1), j]==1 ]
                if ( (length(t1) > 0) & (all(Same[t1, i])) )
                {
                    R[i, j] <- 1
                    t0 <- setdiff(t0, t1)
                }
                j <- j - 1
            }
            # Code below needed if Symbols[1:L] not sufficient
            while ( length(t0) > 0)
            {
                L <- L + 1
                R <- cbind(R, 0)
                R[i,L] <- 1
                for (j in 1:(i-1) )
                {
                    if ( is.element(j, t0) )
                    {
                        # Means so far given Symbol[L]
                        t1 <- (1:i)[ R[1:i, L]==1 ]
                        #					if ( all(Same[i, t1]) )
                        if ( all(Same[c(j,t1), c(j,t1)]) )
                        {
                            R[j, L] <- 1
                            t0 <- setdiff(t0, j)
                        }
                    }
                }
            }
        }
    }

    Symbols <- c(letters[1:26], LETTERS[1:26])
    Ranking <- c()
    for (i in 1:min(length(Symbols),ncol(R)))
        Ranking <- paste(Ranking, c(" ", Symbols[i])[R[,i]+1], sep="")
    if ( ncol(R) > length(Symbols) )
        Ranking <- paste(Ranking, "(Run out of Symbols to complete)")

    # Check Tukey Ranking
    if ( !all(((R %*% t(R)) != 0) == Same) )
    {
        print("ERROR IN RANKING")
        print("----------------")
        Ranking <- paste(Ranking, " - RANKING IN ERROR")
    }

    return( data.frame(Names=Names, Mean= Mean, groups=Ranking) )
}
