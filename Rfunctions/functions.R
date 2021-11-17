# Need to document see_prop #
# Used in plots 1, 3, and 5
see_prop <- function(data, xvar, yvar, ovar, main=NA, xlab=NA, ylab=NA, dr=NA, legend=legend, lx, ly, ci="Yes", ylim){
  d1 <- dr[1]
  d2 <- dr[2]
  d1 <- as.POSIXct(d1, format="%y-%m-%d")
  d2 <- as.POSIXct(d2, format="%y-%m-%d")
  data <- data[data$date >= d1 & data$date <= d2,]
  dates <- unique(as.character(sort(data$date), format="%m/%Y"))
  if (ci =="95% Confidence Interval"){
    ci <- T
  }else{
    ci <- F
  }
  if (ovar == "Health Center"){
    ovar <- "center"
  }
  if (ovar == "Age"){
    ovar <- "age"
    legend <- "Y"
  }
  if (ovar == "Sex"){
    ovar <- "sex"
    legend <- "Y"
  }
  if (ovar == "Race"){
    ovar <- "race"
    legend <- "Y"
  }
  if (ovar == "Ethnicity"){
    ovar <- "ethnicity"
    legend <- "Y"
  }
  if (ovar == "Language"){
    ovar <- "language"
    legend <- "Y"
  }
  if (ovar == "Insurance Class"){
    ovar <- "insurance"
    legend <- "Y"
  }
  if (ovar == "Alcohol Use Disorder"){
    ovar <- "alcohol"
    legend <- "Y"
  }
  if (ovar == "Depression Diagnosis"){
    ovar <- "depression"
    legend <- "Y"
  }
  if (ovar == "Tobacco Use"){
    ovar <- "tobacco"
    legend <- "Y"
  }
  df <- data[,c(yvar, xvar, ovar)]
  tab <- table(df)
  n <- array(tab[dimnames(tab)[[1]]=="N"], dim=dim(tab)[2:length(dim(tab))])+array(tab[dimnames(tab)[[1]]=="Y"], dim=dim(tab)[2:length(dim(tab))])
  p <- array(tab[dimnames(tab)[[1]]=="Y"], dim=dim(tab)[2:length(dim(tab))])/n
  p[is.nan(p)] <- NA
  p_lo <- p-1.96*sqrt((p*(1-p))/n)
  p_hi <- p+1.96*sqrt((p*(1-p))/n)
  p_mat <- matrix(as.vector(p), nrow = length(na.omit(unique(data[,xvar]))))
  p_lo_mat <- matrix(as.vector(p_lo), nrow = length(na.omit(unique(data[,xvar]))))
  p_hi_mat <- matrix(as.vector(p_hi), nrow = length(na.omit(unique(data[,xvar]))))
  l <- list()
  for (i in 1:length(ovar)){
    l[[i]] <- dimnames(tab)[[i+2]]
  }
  name_mat <- expand.grid(l)
  for (j in 1:length(ovar)){
    name_mat[,j] <- as.character(name_mat[,j])
  }
  name_v <- c()
  for (i in 1:nrow(name_mat)){
    name_v[i] <- paste(name_mat[i,], collapse = " + ")
  }
  colnames(p_mat) <- name_v
  set.seed(1)
  cols <- c("red", "green", "pink", "yellow", "#FF8000", "#80FF00", "cyan", "#0080FF", "#8000FF", "magenta", "#FF0080","blue")
  lwd=4
  for (j in 1:ncol(p_mat)){
    if (j==1){
      plot(1:nrow(p_mat), p_mat[,j]*100, ylim=ylim, col=cols[j], xlab=xlab, ylab=ylab, main=main, xaxt="n")
      rect(par("usr")[1], par("usr")[3],
           par("usr")[2], par("usr")[4],
           col = "grey10") # Color
      lines(c(-1, 50), c(0,0))
      # lines(c(-1, 50), c(1, 1))
      # lines(c(-1, 50), c(2, 2))
      # lines(c(-1, 50), c(3, 0.30))
      # lines(c(-1, 50), c(0.40, 0.40))
      # lines(c(-1, 50), c(0.50, 0.50))
      # lines(c(-1, 50), c(0.60, 0.60))
      # lines(c(-1, 50), c(0.70, 0.70))
      # lines(c(-1, 50), c(0.80, 0.80))
      # lines(c(-1, 50), c(0.90, 0.90))
      lines(c(-1, 50), c(100, 100))
      axis(1, at=1:length(dates), dates)
      if (!isTRUE(ci)){
        lines(1:nrow(p_mat), p_mat[,j]*100, col=cols[j], lwd=2)
      }else{
        x <- c(1:nrow(p_mat), nrow(p_mat):1)
        y <- c(p_lo_mat[,j], rev(p_hi_mat[,j]))
        y[y < 0] <- 0
        polygon(
          x = x,
          y = y*100,
          col = adjustcolor(cols[j], alpha.f=0.5),
          border = NA
        )
      }
    }else{
      if(!isTRUE(ci)){
        lines(1:nrow(p_mat), p_mat[,j]*100, col=cols[j], lwd=2)
      }else {
        x = c(1:nrow(p_mat), nrow(p_mat):1)
        y = c(p_lo_mat[,j], rev(p_hi_mat[,j]))
        y[y < 0] <- 0
        polygon(
          x = x,
          y = y*100,
          col = adjustcolor(cols[j], alpha.f=0.5),
          border = NA
        )
      }
    }
  }
  if (legend == "Y"){
    legend(
      x = "topright",
      bg="grey10",
      name_v,
      cex = 1.25,
      col=cols[1:length(name_v)],
      lty=1,
      text.col=cols[1:length(name_v)]
    )
  }
}


get_month <- function(date){
  as.numeric(as.character(date, format="%m"))
}

get_year <- function(date){
  as.numeric(as.character(date, format="%Y"))
}

# goof around with some survival analysis #
#' @param times vector of encounter times
#' @param np number of patients seen
surv_fun <- function(times, np){
  df <- data.frame(
    time = 1:max(times),
    cov = rep(NA, length(1:max(times))),
    sur = rep(NA, length(1:max(times))),
    n = rep(NA, length(1:max(times)))
  )
  for (t in 1:max(times)){
    df$cov[t] <- sum(na.omit(times==t))
  }
  for (t in max(times):1){
    df$sur[t] <- (np-sum(df$cov[1:t]))/np
    df$n[t] <- np - sum(df$cov[1:t])
  }
  return(df)
}

# Existing code #
#' @param times 
#' list of vectors; 
#'    each vector stores encounter times; 
#'    names(list) = variable names over which data is stratified
#' @param nps
#' (vector) number of patients associated with stratification variables  
plot_surv <- function(times, nps, dates, var, ylim, lx, leg, ci, main){
  for (i in 1:length(times)){
    sf <- surv_fun(times[[i]], nps[i])
    cols <- c("red", "green", "pink", "yellow", "#FF8000", "#80FF00", "cyan", "#0080FF", "#8000FF", "magenta", "#FF0080","blue")
    lwd=4
    if (i==1){
      plot(sf[,1], sf[,3], ty="l", col=cols, ylim=ylim, xlab=NA, ylab="Percentage", xaxt="n", main=main)
      rect(par("usr")[1], par("usr")[3],
           par("usr")[2], par("usr")[4],
           col = "grey10") # Color
      lines(c(-1, 50), c(0,0))
      # lines(c(-1, 50), c(0.10, 0.10))
      # lines(c(-1, 50), c(0.20, 0.20))
      # lines(c(-1, 50), c(0.30, 0.30))
      # lines(c(-1, 50), c(0.40, 0.40))
      # lines(c(-1, 50), c(0.50, 0.50))
      # lines(c(-1, 50), c(0.60, 0.60))
      # lines(c(-1, 50), c(0.70, 0.70))
      # lines(c(-1, 50), c(0.80, 0.80))
      # lines(c(-1, 50), c(0.90, 0.90))
      lines(c(-1, 50), c(100, 100))
      axis(1, at=1:length(dates), dates)
      if (ci == "Point Estimate"){
        lines(sf[,1], sf[,3]*100, col=cols[i], lwd=lwd)
      }else{
        sf$n[sf$n > 60000] <- 60000
        se <- sf$sur*sqrt(sum(sf$cov/(sf$n*(sf$n-sf$cov))))
        lo <- sf$sur-1.96*se
        hi <- sf$sur+1.96*se
        x = c(1:length(sf[,1]), length(sf[,1]):1)
        y = c(lo, rev(hi))
        y[y > 1] <- 1
        polygon(
          x = x,
          y = y*100,
          col = adjustcolor(cols[i], alpha.f=0.5),
          border = NA
        )
      }
      name_v <- names(times)
      if (isTRUE(leg)){
        legend(
          lx, 
          ly,
          bg="grey10",
          name_v,
          cex = 1.25,
          col=cols[1:length(name_v)],
          lty=1,
          lwd=4,
          text.col=cols[1:length(name_v)]
        )
      }
    }else{
      if (ci=="Point Estimate"){
        lines(sf[,1], sf[,3]*100, ty="l", col=cols[i], lwd=lwd)
      }else{
        sf$n[sf$n > 60000] <- 60000
        se <- sf$sur*sqrt(sum(sf$cov/(sf$n*(sf$n-sf$cov))))
        lo <- sf$sur-1.96*se
        hi <- sf$sur+1.96*se
        x = c(1:length(sf[,1]), length(sf[,1]):1)
        y = c(lo, rev(hi))
        y[y > 1] <- 1
        polygon(
          x = x,
          y = y*100,
          col = adjustcolor(cols[i], alpha.f=0.5),
          border = NA
        )
      }
    }
  }
}

#' @param data main data table
#' @param xvar x-axis variable -- will generally be time
#' @param yvar y-axis variable -- e.g., covid or vaccinated
#' @param ovar user input stratification variable
#' @param dr user input daterange
#' @param ylim user input y range
survival_plot <- function(data, xvar, yvar, ovar, dr, ylim, ci, main){
  if (ovar == "Health Center"){
    ovar <- "center"
  }
  if (ovar == "Age"){
    ovar <- "age"
    legend <- "Y"
  }
  if (ovar == "Sex"){
    ovar <- "sex"
    legend <- "Y"
  }
  if (ovar == "Race"){
    ovar <- "race"
    legend <- "Y"
  }
  if (ovar == "Ethnicity"){
    ovar <- "ethnicity"
    legend <- "Y"
  }
  if (ovar == "Language"){
    ovar <- "language"
    legend <- "Y"
  }
  if (ovar == "Insurance Class"){
    ovar <- "insurance"
    legend <- "Y"
  }
  if (ovar == "Alcohol Use Disorder"){
    ovar <- "alcohol"
    legend <- "Y"
  }
  if (ovar == "Depression Diagnosis"){
    ovar <- "depression"
    legend <- "Y"
  }
  if (ovar == "Tobacco Use"){
    ovar <- "tobacco"
    legend <- "Y"
  }
  d <- data[!is.na(data[,ovar]),]
  lev <- levels(d[,ovar])
  nps <- rep(NA, length(lev))
  for (i in 1:length(lev)){
    nps[i] <- length(unique(d$mrn[d[,ovar]==lev[i]]))
  }
  times <- list()
  d <- d[d[,which(names(d)==yvar)] == "Y",]
  for (i in 1:length(lev)){
    times[[i]] <- d$time[d[,ovar]==lev[i]]
  }
  names(times) <- lev
  # extract dates #
  d1 <- dr[1]
  d2 <- dr[2]
  d1 <- as.POSIXct(d1, format="%y-%m-%d")
  d2 <- as.POSIXct(d2, format="%y-%m-%d")
  data <- data[data$date >= d1 & data$date <= d2,]
  dates <- unique(as.character(sort(data$date), format="%m/%Y"))
  if (ovar=="center"){
    plot_surv(times, nps, dates, ovar, ylim=ylim, lx="bottomleft", leg=F, ci=ci, main=main)
  }else{
    plot_surv(times, nps, dates, ovar, ylim=ylim, lx="bottomleft", leg=T, ci=ci, main=main)
  }
}
