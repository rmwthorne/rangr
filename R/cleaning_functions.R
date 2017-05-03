
#' Remove out-of-bounds ranges, using a given genome build
#'
#' @param gr A \code{GenomicRanges} object.
#' @param genome A genome build, e.g. hg19
#' @return a filtered \code{GenomicRanges} object.
#' @export

trim_GRanges <- function(gr, genome = "") {

  genome(gr) <- genome
  gr <- GenomicRanges::trim(gr)
}
