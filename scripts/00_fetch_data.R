# Download the public nflverse files this project uses into data/raw/ and pin them by SHA-256.
#
# nflverse release files are updated in place (stat corrections, re-scrapes), so a URL alone
# does not identify the data. The first run writes data/manifest.csv with each file's size and
# SHA-256. Later runs verify every file against the manifest and stop if anything differs.
#
# Usage: Rscript scripts/00_fetch_data.R           # download missing files, verify all
#        Rscript scripts/00_fetch_data.R --repin   # accept new upstream versions (log in DEVIATIONS.md)

suppressPackageStartupMessages({
  library(arrow)
  library(digest)
})

SEASONS <- 2016:2025 # completed seasons only; 2026 is in progress
BASE <- "https://github.com/nflverse/nflverse-data/releases/download"
RAW <- file.path("data", "raw")
MANIFEST <- file.path("data", "manifest.csv")

files <- rbind(
  data.frame(release = "pbp", name = sprintf("play_by_play_%d.parquet", SEASONS)),
  data.frame(release = "pbp_participation", name = sprintf("pbp_participation_%d.parquet", SEASONS)),
  data.frame(release = "ftn_charting", name = sprintf("ftn_charting_%d.parquet", 2022:2025)),
  data.frame(release = "schedules", name = "games.parquet")
)
files$url <- file.path(BASE, files$release, files$name)

dir.create(RAW, recursive = TRUE, showWarnings = FALSE)
options(timeout = 600)
for (i in seq_len(nrow(files))) {
  dest <- file.path(RAW, files$name[i])
  if (!file.exists(dest)) {
    message("downloading ", files$name[i])
    tmp <- paste0(dest, ".part")
    status <- utils::download.file(files$url[i], tmp, mode = "wb", quiet = TRUE)
    if (status != 0) stop("download failed: ", files$url[i])
    # A parquet file starts and ends with the magic bytes PAR1; an HTML error page does not.
    con <- file(tmp, "rb")
    head4 <- readBin(con, "raw", 4)
    close(con)
    if (!identical(rawToChar(head4), "PAR1")) stop("not a parquet file: ", files$url[i])
    file.rename(tmp, dest)
  }
}

# The schedule file is rewritten daily during the current season, so it is pinned by the content
# this project uses (completed 2016-2025 games) rather than by the whole file.
schedule_fingerprint <- function(path) {
  g <- as.data.frame(read_parquet(path, col_select = c("game_id", "season", "game_type", "week",
                                                         "home_team", "away_team", "home_score",
                                                         "away_score")))
  g <- g[g$season %in% SEASONS & !is.na(g$home_score), ]
  g <- g[order(g$game_id), ]
  digest(paste(utils::capture.output(utils::write.csv(g, row.names = FALSE)), collapse = "\n"),
         algo = "sha256", serialize = FALSE)
}

files$bytes <- file.size(file.path(RAW, files$name))
files$sha256 <- vapply(file.path(RAW, files$name), digest, "", algo = "sha256", file = TRUE)
is_sched <- files$name == "games.parquet"
files$bytes[is_sched] <- NA
files$sha256[is_sched] <- schedule_fingerprint(file.path(RAW, "games.parquet"))

if (!file.exists(MANIFEST) || "--repin" %in% commandArgs(TRUE)) {
  files$pinned_on <- format(Sys.Date())
  utils::write.csv(files, MANIFEST, row.names = FALSE)
  message("wrote ", MANIFEST, " (", nrow(files), " files)")
} else {
  pinned <- utils::read.csv(MANIFEST)
  m <- merge(pinned[, c("name", "sha256")], files[, c("name", "sha256")], by = "name",
             suffixes = c("_pinned", "_local"), all = TRUE)
  bad <- m[is.na(m$sha256_pinned) | is.na(m$sha256_local) | m$sha256_pinned != m$sha256_local, ]
  if (nrow(bad) > 0) {
    print(bad)
    stop("files differ from data/manifest.csv. Upstream nflverse data changed; results may differ. ",
         "Delete the listed files to re-download, or rerun with --repin and log it in DEVIATIONS.md.")
  }
  message("all ", nrow(files), " files match data/manifest.csv")
}
