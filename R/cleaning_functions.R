
#' Removes out-of-bounds intervals, given a specific genome build.
#'
#' @param gr a genomic ranges object.
#' @param genome a genome build. Defaults to "hg19"
#' @return A genomic ranges object.
#' @export

trim_GRanges <- function(gr, genome = "hg19") {
  seqinfo(gr) <- Seqinfo(genome = genome)
  gr <- trim(gr)
}
