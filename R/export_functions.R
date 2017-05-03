

#' Save a GRanges object to a bed file.
#'
#' @param gr A \code{\link[GenomicRanges]{GenomicRanges}} object.
#' @param path Path to write to.
#' @param ucsc_name A header track name. Required for visualisation by UCSC
#' genome browser.
#' @return writes an object to a specific path.
#' @examples
#' gr <- GRanges(Rle(c("chr2", "chr2", "chr1", "chr3"), c(1, 3, 2, 4)),
#' IRanges(1:10, width=10:1, names=head(letters, 10)),
#' Rle(strand(c("-", "+", "*", "+", "-")), c(1, 2, 2, 3, 2)),
#' score=1:10, GC=seq(1, 0, length=10))
#'
#' write_bed(gr, path = "output/file.ucsc.bed", ucsc = "Example track")
#' @export

write_bed <- function(gr, path = "", ucsc_name = F) {
  # Makes a bed file from a granges object
  # name.col and score.col not working yet.
  df <- gr %>%
    as.data.frame() %>%
    select(seqnames, start, end)
  if (ucsc_name != "") {
    cat(paste0("track name=\"",ucsc_name,"\"\n"), file = path)
  }
  write.table(df, file = path, append = T,
              quote = F, sep = "\t", row.names = F, col.names = F)
}


#' Save a GRanges or data frame object to tsv and UCSC-ready bed file for MIG.
#'
#' @param x A \code{\link[GenomicRanges]{GenomicRanges}} or \code{data.frame} object.
#' @param path Path to write to.
#' @param ucsc_suffix The suffix for the ucsc-ready bed file.
#' @param genome A genome build, e.g. hg19
#' @return writes an object to a specific path.
#' @examples
#' gr <- GRanges(Rle(c("chr2", "chr2", "chr1", "chr3"), c(1, 3, 2, 4)),
#' IRanges(1:10, width=10:1, names=head(letters, 10)),
#' Rle(strand(c("-", "+", "*", "+", "-")), c(1, 2, 2, 3, 2)),
#' score=1:10, GC=seq(1, 0, length=10))
#'
#' write_bed(gr, path = "output/file.ucsc.bed", ucsc = TRUE)
#' @export

write_mig <- function(x, path = "", genome = "hg19", ucsc_suffix = ".ucsc.bed") {
  # convert to gr,
  # remove out of bounds (trim_GRanges() is in cleaning_functions.R)
  # convert to df
  x %>%
    GRanges() %>%
    rangr::trim_GRanges(gr = ., genome = genome) %>%
    as.data.frame() ->
    x

  # Write a tab delim file for MIG upload
  write.table(x, file = path, quote = F, sep = "\t",
              row.names = F, na = "NA", col.names = T)
  message(paste0("Wrote ", path))

  # Write a ucsc-compatible bed for visualisation
  write_bed(x, path = paste0(path, ucsc_suffix),
            ucsc_name = paste0(path, ucsc_suffix))
  message(paste0("Wrote ", path, ucsc_suffix))

}

