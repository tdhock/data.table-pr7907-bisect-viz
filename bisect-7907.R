prefix <- "bisect/fread_N=cols_regression_2026-10-09-"
(Test.dir.vec <- Sys.glob(paste0(prefix, "*")))
library(data.table)

version.colors <- c(
  Fast="violet",
  Slow="red",
  HEAD="blue")
version.colors <- c(#RColorBrewer::brewer.pal(7, "Dark2")
  HEAD="#1B9E77",
  Slow="#E6AB02",
  Fast="#A6761D")
details.dt.list <- list()
dot.dt.list <- list()
common.dt.list <- list()
results.dt.list <- list()
for(Test.dir in Test.dir.vec){
  Bisect <- function(dt)dt[
  , Bisect := gsub("-", " ", sub(prefix, "", Test.dir))
  ][
  , Commit := paste(Bisect, commit)
  ][]
  for(csv in Sys.glob(file.path(Test.dir, "csv", "*"))){
    results.dt.list[[csv]] <- Bisect(fread(csv))
  }
  for(result.RData in Sys.glob(file.path(Test.dir, "RData", "*"))){
    (objs <- load(result.RData))
    commit <- sub(".RData", "", basename(result.RData))
    details <- Bisect(tres.or.status$measurements[
      names(version.colors),
      on="version",
      data.table(commit, expr.name, version, N, median, min, max)
    ])
    common.dt.list[[paste(Test.dir, commit)]] <- Bisect(details[, .(
      versions=.N
    ), by=N][
      versions==max(versions)
    ][.N, data.table(commit, N)])
    evers <- unique(details[, .(expr.name, version)])
    details.dt.list[[paste(Test.dir, commit)]] <- details
    dot.dt.list[[paste(Test.dir, commit)]] <- Bisect(data.table(
      commit,
      tpred$prediction[evers, on="expr.name"]))
  }
}
(details.dt <- rbindlist(details.dt.list))
(dot.dt <- rbindlist(dot.dt.list))
(common.dt <- rbindlist(common.dt.list))
(results.wide <- rbindlist(results.dt.list)[, let(
  rank = rank(time),
  Status = factor(status)
)][])
(results.long <- melt(
  results.wide,
  measure.vars=measure(variable, version, sep=".")))
(label.dt <- results.long[, {
  if(Bisect=="run2b intermediate v1.14.8"){
    fun <- min
    hjust <- 1
  }else{
    fun <- max
    hjust <- 0
  }
  data.table(hjust, .SD[rank==fun(rank)])
}, by=Bisect])
limit.dt <- data.table(seconds=tpred$seconds.limit)
library(animint2)
animint(
  title="
  overview=ggplot()+
    ggtitle("Overview of three git bisect runs, select commit")+
    theme_bw()+
    theme(axis.text.x=element_text(angle=30, hjust=1))+
    theme_animint(width=1000)+
    geom_vline(aes(
      xintercept=rank,
      key=1),
      color="black",
      showSelected="Commit",
      data=results.wide[, .(rank, Commit)])+
    geom_tallrect(aes(
      xmin=rank-0.5, xmax=rank+0.5,
      key=rank,
      fill=Status),
      color="grey",
      alpha=0.5,
      data=results.wide)+
    geom_line(aes(
      rank, value, color=version, group=version),
      data=results.long)+
    geom_point(aes(
      rank, value, color=version, group=version),
      fill=NA,
      data=results.long)+
    facet_grid(
      variable ~ Bisect,
      scales="free_y",
      labeller=label_both)+
    geom_label_aligned(aes(
      rank, value,
      hjust=hjust,
      label=version, color=version),
      data=label.dt,
      alignment="vertical")+
    scale_fill_manual(values=c(
      "0"="grey",
      "1"="white"))+
    scale_y_continuous("")+
    scale_x_continuous("Commit rank over time")+
    scale_color_manual(values=version.colors)+
    geom_tallrect(aes(
      xmin=rank-0.5, xmax=rank+0.5),
      clickSelects="Commit",
      alpha=1,
      alpha_off=0,
      color="black",
      color_off=NA,
      fill=NA,
      data=results.wide),
  details=ggplot()+
    ggtitle("Asymptotic timing results for selected commit")+
    theme_bw()+
    theme(legend.position="none")+
    theme_animint(width=1000)+
    geom_hline(aes(
      yintercept=seconds),
      color="grey",
      data=limit.dt)+
    geom_text(aes(
      0, seconds*1.1,
      label=sprintf(
        "time limit = %s seconds",
        paste(seconds)
      )),
      hjust=0,
      color="grey50",
      data=limit.dt)+
    geom_vline(aes(
      xintercept=N,
      key=1),
      data=common.dt,
      color="grey",
      showSelected="Commit")+
    geom_text(aes(
      N, 0, label=sprintf("largest common N=%s", paste(N)),
      key=1),
      data=common.dt,
      hjust=1,
      color="grey50",
      showSelected="Commit")+
    geom_text(aes(
      0, 0,
      label=commit,
      href=paste0("https://github.com/Rdatatable/data.table/commit/", commit),
      key=1),
      hjust=0,
      vjust=0,
      data=common.dt,
      showSelected="Commit")+
    geom_ribbon(aes(
      N, ymin=min, ymax=max,
      key=version, fill=version, group=version),
      showSelected=c("version","Commit"),
      alpha=0.2,
      color=NA,
      data=details.dt)+
    geom_line(aes(
      N, median,
      key=version, color=version, group=version),
      showSelected=c("version","Commit"),
      data=details.dt)+
    geom_point(aes(
      N, unit.value,
      key=version, color=version),
      showSelected=c("version","Commit"),
      fill="white",
      data=dot.dt)+
    geom_label_aligned(aes(
      N, unit.value, label=version,
      key=version, color=version),
      showSelected=c("version","Commit"),
      alignment="horizontal",
      vjust=-0.1,
      alpha=0.5,
      data=dot.dt)+
    scale_x_log10("N = number of columns in CSV file")+
    scale_color_manual(values=version.colors)+
    scale_fill_manual(values=version.colors)+
    scale_y_log10("Seconds to fread(file.csv)"),
  duration=list(
    Commit=1000)
)

