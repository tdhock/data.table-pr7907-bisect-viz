library(data.table)
Test <- "fread N=cols regression"
src.dir <- file.path("~/R/data.table/.ci/atime/bisect-restart", atime:::test_file_name(Test))
tests.R.vec <- Sys.glob(file.path(src.dir, "*", "tests.R"))
eval(parse(tests.R.vec[1]))
tdata <- test.list[[Test]]
ref.list <- tdata[sapply(tdata, is.character)]
(data.dir <- file.path("data", system("hostname", intern=TRUE), basename(dirname(src.dir))))
unlink(data.dir, recursive = TRUE)
dir.create(data.dir, showWarnings = FALSE, recursive = TRUE)
file.copy(src.dir, data.dir, recursive = TRUE)
(ref.dt <- data.table(
  Rname=names(ref.list),
  ref=as.character(ref.list)
)[
, gert::git_log(ref, 1, repo="~/R/data.table")
, by=.(Rname, ref)][, .(Rname, commit, time)])
fwrite(ref.dt, "refs.csv")
