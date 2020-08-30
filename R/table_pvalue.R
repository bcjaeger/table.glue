

#' P-value rounding
#'
#' When presenting p-values, journals tend to request a lot of finessing.
#'   `table_pvalue()` is meant to do almost all of the finessing for you.
#'   the part it does not do is _interpret_ the p-value. For that,
#'   please see the guideline on interpretation of p-values by the American
#'   Statistical Association (Wasserstein, 2016). The six main statements
#'   on p-value usage are included in the "Interpreting p-values" section
#'   below.
#'
#' @param x a vector of numeric values. All values should be > 0 and < 1.
#'
#' @param round_half_to a character value indicating how to break ties when
#'   the rounded unit is exactly halfway between two rounding points.
#'   See [round_half_even] and [round_half_up] for details. Valid inputs
#'   are 'even' and 'up'.
#'
#' @param decimals_outer number of decimals to print when
#'   p > thresh_hi or p < thresh_lo.
#'
#' @param decimals_inner number of decimals to print when
#'   `thresh_lo` < p < `thresh_hi`.
#'
#' @param alpha a numeric value indicating the significance level,
#'   i.e. the probability that you will make the mistake of rejecting
#'   the null hypothesis when it is true.
#'
#' @param boundary_lo the lower bound of the inner range.
#'
#' @param boundary_hi the upper bound of the inner range.
#'
#' @param thresh_lo the lowest value printed. Values lower than the
#'   threshold will be printed as <threshold.
#'
#' @param thresh_hi the highest value printed. Values higher than the
#'   threshold will be printed as >threshold.
#'
#' @param miss_replace a character value that replaces missing values.
#'
#' @param drop_leading_zero a logical value. If `TRUE`, the leading 0
#'   is dropped for all p-values. So, '0.04' will become '.04'. If `FALSE`,
#'   no leading zeroes are dropped.
#'
#' @return a character vector
#'
#' @references Wasserstein, Ronald L., and Nicole A. Lazar.
#' "The ASA statement on p-values: context, process, and purpose." (2016):
#'  _The American Statistician_: 129-133.
#'  DOI: https://doi.org/10.1080/00031305.2016.1154108
#'
#' @section Interpreting p-values:
#'
#' The American Statistical Association (ASA) defines the p-value as follows:
#'
#' A p-value is the probability under a specified statistical model that a statistical summary of the data (e.g., the sample mean difference between two compared groups) would be equal to or more extreme than its observed value.
#'
#' It then provides six principles to guide p-value usage:
#'
#'  1. P-values can indicate how incompatible the data are with a specified statistical model.A p-value provides one approach to summarizing the incompatibility between a particular set of data and a proposed model for the data. The most common context is a model, constructed under a set of assumptions, together with a so-called "null hypothesis". Often the null hypothesis postulates the absence of an effect, such as no difference between two groups, or the absence of a relationship between a factor and an outcome. The smaller the p-value, the greater the statistical incompatibility of the data with the null hypothesis, if the underlying assumptions used to calculate the p-value hold. This incompatibility can be interpreted as casting doubt on or providing evidence against the null hypothesis or the underlying assumptions.
#'
#'  2. P-values do not measure the probability that the studied hypothesis is true, or the probability that the data were produced by random chance alone. Researchers often wish to turn a p-value into a statement about the truth of a null hypothesis, or about the probability that random chance produced the observed data. The p-value is neither. It is a statement about data in relation to a specified hypothetical explanation, and is not a statement about the explanation itself.
#'
#'  3. Scientific conclusions and business or policy decisions should not be based only on whether a p-value passes a specific threshold. Practices that reduce data analysis or scientific inference to mechanical "bright-line" rules (such as "p < 0.05") for justifying scientific claims or conclusions can lead to erroneous beliefs and poor decision making. A conclusion does not immediately become "true" on one side of the divide and "false" on the other. Researchers should bring many contextual factors into play to derive scientific inferences, including the design of a study, the quality of the measurements, the external evidence for the phenomenon under study, and the validity of assumptions that underlie the data analysis. Pragmatic considerations often require binary, "yes-no" decisions, but this does not mean that p-values alone can ensure that a decision is correct or incorrect. The widespread use of "statistical significance" (generally interpreted as "p<0.05") as a license for making a claim of a scientific finding (or implied truth) leads to considerable distortion of the scientific process.
#'
#'   4. Proper inference requires full reporting and transparency P-values and related analyses should not be reported selectively. Conducting multiple analyses of the data and reporting only those with certain p-values (typically those passing a significance threshold) renders the reported p-values essentially uninterpretable. Cherry picking promising findings, also known by such terms as data dredging, significance chasing, significance questing, selective inference, and "p-hacking," leads to a spurious excess of statistically significant results in the published literature and should be vigorously avoided. One need not formally carry out multiple statistical tests for this problem to arise: Whenever a researcher chooses what to present based on statistical results, valid interpretation of those results is severely compromised if the reader is not informed of the choice and its basis. Researchers should disclose the number of hypotheses explored during the study, all data collection decisions, all statistical analyses conducted, and all p-values computed. Valid scientific conclusions based on p-values and related statistics cannot be drawn without at least knowing how many and which analyses were conducted, and how those analyses (including p-values) were selected for reporting.
#'
#'   5. A p-value, or statistical significance, does not measure the size of an effect or the importance of a result. Statistical significance is not equivalent to scientific, human, or economic significance. Smaller p-values do not necessarily imply the presence of larger or more important effects, and larger p-values do not imply a lack of importance or even lack of effect. Any effect, no matter how tiny, can produce a small p-value if the sample size or measurement precision is high enough, and large effects may produce unimpressive p-values if the sample size is small or measurements are imprecise. Similarly, identical estimated effects will have different p-values if the precision of the estimates differs.
#'
#'  6. By itself, a p-value does not provide a good measure of evidence regarding a model or hypothesis. Researchers should recognize that a p-value without context or other evidence provides limited information. For example, a p-value near 0.05 taken by itself offers only weak evidence against the null hypothesis. Likewise, a relatively large p-value does not imply evidence in favor of the null hypothesis; many other hypotheses may be equally or more consistent with the observed data. For these reasons, data analysis should not end with the calculation of a p-value when other approaches are appropriate and feasible.
#'
#' @export
#'
#' @family table helpers
#'
#' @examples
#'
#' # Guideline by the American Medical Association Manual of Style:
#' # Round p-values to 2 or 3 digits after the decimal point depending
#' # on the number of zeros. For example,
#' ## - Change .157 to .16.
#' ## - Change .037 to .04.
#' ## - Don't change .047 to .05, because it will no longer be significant.
#' ## - Keep .003 as is because 2 zeros after the decimal are fine.
#' ## - Change .0003 or .00003 or .000003 to <.001
#' #
#' # In addition, the guideline states that "expressing P to more than 3
#' # significant digits does not add useful information." You may or may not
#' # agree with this guideline (I do not agree with parts of it),
#' # but you will (hopefully) appreciate `table_pvalue()` automating these
#' # recommendations if you submit papers to journals associated with
#' # the American Medical Association.
#'
#' pvals_ama <- c(0.157, 0.037, 0.047, 0.003, 0.0003, 0.00003, 0.000003)
#'
#' table_pvalue(pvals_ama)
#' # > [1] ".16"   ".04"   ".047"  ".003"  "<.001" "<.001" "<.001"
#'
#' # `table_pvalue()` will fight valiantly to keep your p-value < alpha if
#' # it is < alpha. If it's >= alpha, `table_pvalue()` treats it normally.
#'
#' pvals_close <- c(0.04998, 0.05, 0.050002)
#'
#' table_pvalue(c(pvals_close, pvals_ama))
#' # > [1] ".04998" ".05"    ".05"    ".16"    ".04"    ".047"   ".003"
#' # > [8] "<.001"  "<.001"  "<.001"
#'
table_pvalue <- function(x,
                         round_half_to = 'even', # or 'up'
                         decimals_outer = 3L,
                         decimals_inner = 2L,
                         alpha = 0.05,
                         boundary_lo = 0.01,
                         boundary_hi = 0.99,
                         thresh_lo = 0.001,
                         thresh_hi = 0.999,
                         miss_replace = '--',
                         drop_leading_zero = TRUE){

  check_call(
    match.call(),
    expected = list(
      'x' = list(
        type = 'double',
        length = NULL,
        lwr = 0,
        upr = 1
      ),
      'round_half_to' = list(
        type = 'character',
        length = 1,
        options = c('even', 'up')
      ),
      'decimals_outer' = list(
        type = 'numeric',
        length = 1,
        lwr = 1,
        upr = 20
      ),
      'decimals_inner' = list(
        type = 'numeric',
        length = 1,
        lwr = 1,
        upr = 20
      ),
      'boundary_lo' = list(
        type = 'double',
        length = 1,
        lwr = 0,
        upr = 1
      ),
      'boundary_hi' = list(
        type = 'double',
        length = 1,
        lwr = 0,
        upr = 1
      ),
      'thresh_lo' = list(
        type = 'double',
        length = 1,
        lwr = 0,
        upr = 1
      ),
      'thresh_hi' = list(
        type = 'double',
        length = 1,
        lwr = 0,
        upr = 1
      ),
      'miss_replace' = list(
        type = 'character',
        length = 1,
        lwr = NULL,
        upr = NULL
      ),
      'drop_leading_zero' = list(
        type = 'logical',
        length = 1,
        lwr = NULL,
        upr = NULL
      )
    )
  )

  # take care of the boundary cases (lowest low and highest high)
  out <- rep(miss_replace, length(x))
  out[x < thresh_lo] <- paste0("<", thresh_lo)
  out[x > thresh_hi] <- paste0(">", thresh_hi)

  # set up separate rounding specifications for each
  # of the three inner boundaries (lower, middle, upper)
  rspec <- round_spec()

  if(round_half_to == 'even')
    rspec <- round_half_even(rspec)

  if(round_half_to == 'up')
    rspec <- round_half_up(rspec)

  # rounding specifications to match user specified thresholds/decimals

  ## lower interval
  rspec_lwr <- round_using_magnitude(
    rspec = rspec,
    digits = c(decimals_outer, decimals_inner),
    breaks = c(boundary_lo, boundary_hi)
  )

  ## middle interval
  rspec_mid <- round_using_magnitude(
    rspec = rspec,
    digits = c(decimals_outer, decimals_inner),
    breaks = c(boundary_lo, boundary_hi)
  )

  ## upper interval
  rspec_upr <- round_using_magnitude(
    rspec = rspec,
    digits = c(decimals_outer, decimals_inner, decimals_outer),
    breaks = c(boundary_lo, boundary_hi, 1)
  )

  x_in_lower_bound <- x <  boundary_lo & x >= thresh_lo
  x_in_middle      <- x >= boundary_lo & x <= boundary_hi
  x_in_upper_bound <- x >  boundary_hi & x <= thresh_hi

  if ( any(x_in_lower_bound) )
    out[x_in_lower_bound] <- table_value(x, rspec_lwr)[x_in_lower_bound]

  if ( any(x_in_middle) )
    out[x_in_middle] <- table_value(x, rspec_mid)[x_in_middle]

  if( any(x_in_upper_bound) )
    out[x_in_upper_bound] <- table_value(x, rspec_upr)[x_in_upper_bound]

  # captures rounded outputs up to 20 figures past 0.049
  # (surely this is enough [???])
  zeros <- vector(mode = 'character', length = 20)

  for(z in seq_along(zeros))
    zeros[z] <- paste(rep('0', times = z), collapse = '')

  yellow_vals <- paste0(alpha, c('', zeros))
  yellow_decimals <- decimals_outer

  while( any(out %in% yellow_vals & x < alpha) ) {

    rspec_yellow <- round_using_decimal(rspec, digits = yellow_decimals)

    x_in_yellow_zone <- which(out %in% yellow_vals & x < alpha)

    out[x_in_yellow_zone] <- table_value(x[x_in_yellow_zone], rspec_yellow)

    yellow_decimals <- yellow_decimals + 1

    # I hope this is never used
    if(yellow_decimals == 10){

      warning(
        "Some p-values were too close to ", alpha, " for accurate rounding.\n",
        "Those p-values have not been rounded",
        call. = FALSE
      )

      out[x_in_yellow_zone] <- format(x[x_in_yellow_zone], nsmall = 11)

      break

    }

  }

  if (drop_leading_zero ) {
    out <- gsub(pattern = '0.', replacement = '.', x = out, fixed = TRUE)
  }

  out

}
