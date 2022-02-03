#' ---
#' author: Richard Sigman
#' ---

# rss_regress_funcs_v2.R

# User-defined functions that create graphs to support linear regression analysis:
#   responsePredictor()   Plot response versus predictor  for a simple linear
#                            regression
#   residFit()       Plots residuals vs fitted values
#   residOrder()     Plots residuals vs data order or date
#   normalQQ()       QQ plot
#   scaleLocation()  Plots sqrt(|residual|) vs fitted values
#   cooksDistance()  Plots Cooks distance vs data order
#   residLeverage()  Plots standardized residuals vs leverage
#   diagPlots123     Grid of (1) residuals vs fitted, (2) QQ plot, and 
#                      (3) sqrt(|residual|) vs fitted value
#   diagPlots45     Grid of (4) standardized residuals vs leverage and
#                      (5) Cooks distance vs data order

# Requires tidyverse, ggrepel, qqplotr, and gridExtra packages.

#--------------------------------------------------------------------------
# Plot response versus predictor  for a simple linear regression
#   model       Model object created by lm()
#   title2      Subtitle
#   line        TRUE to plot regression line
#   outlierVal  Labels points with |standardized residual| > outlierVal
responsePredictor <- function(model, title2='',line=FALSE,outlierVal=2 ){
    coefs <- coef(model)
    p     <- length(coefs)
    intercept <- names(coefs)[1]=="(Intercept)"
    if ((p==1 & intercept) | (p==2 & !intercept) | p>2){
      cat("Not a simple linear model\n")
      return()
    }
    m <- if_else(intercept, coef(model)[2], coef(model)[1])
    b <- if_else(intercept, coef(model)[1], 0)
    fmodel <- fortify(model)
    fmodel <- rownames_to_column(fmodel,var="label")
    fmodelo <- filter(fmodel, abs(.stdresid) > outlierVal)
    gg <-  ggplot(aes(x=fmodel[,3], y=fmodel[,2]), data=fmodel) +
        geom_point() +
        ggrepel::geom_text_repel(data=fmodelo,
                                 aes(x=fmodelo[,3], y=fmodelo[,2],label=label)) +
        labs(x=names(fmodel)[3],
             y=names(fmodel)[2],
             title=paste("Response vs Predictor", title2, sep='\n'))
    if (line==TRUE) {
        gg <- gg + geom_abline(aes(intercept=b,slope=m))
    }
    gg
}

#----------------------------------------------------------------------
#  Plots residuals vs fitted values
#   model       Model object created by lm()
#   title2      Subtitle
#   outlierVal  Labels points with |standardized residual| > outlierVal
residFit <- function(model, title2='',outlierVal=2){
    fmodel <- fortify(model)
    fmodel <- rownames_to_column(fmodel,var="label")
    ggplot(aes(x=.fitted, y=.resid), data=fmodel) +
        geom_point() +
        geom_hline(yintercept=0) +
        geom_smooth(se = TRUE) +
        ggrepel::geom_text_repel(data=filter(fmodel,abs(.stdresid)>outlierVal),
                                 aes(label=label)) +
        labs(x="Fitted Values",
             y="Residuals",
             title=paste("Residuals vs Fitted: ", title2, sep="\n")) 
}

# --------------------------------------------------------------------
# Plots residuals vs data order or date
#   model       Model object created by lm()
#   data        If useDate is TRUE, data is the data frame containing date
#   useDate     TRUE for x to be the date in data frame data
#   title2      Subtitle
residOrder <- function(model, data=NULL, useDate=FALSE, title2='') {
    fmodel <- fortify(model)
    if (useDate == TRUE) {
         fmodel <- rownames_to_column(fmodel, var=".key.") %>%
            select(.key.,.resid)
         data2  <- rownames_to_column(data, var=".key.")
         data2  <- inner_join(fmodel,data2, by=".key.")
         gg <- ggplot(data2, aes(x=date, y=.resid))+
               labs(x="date",
                    title=paste("Residuals vs Date",
                                title2, sep="\n"))
    } else {
         gg <- ggplot(fmodel, aes(x=seq_along(.resid), y=.resid)) +
               labs(x="order",
                    title=paste("Residuals vs Order",
                                title2, sep="\n"))
    }
    gg + geom_point() +
         geom_hline(yintercept=0) +
         geom_smooth(se = TRUE) +
         labs(y="Residuals")
}

#--------------------------------------------------------------------
# QQ plot
#   model       Model object created by lm()
#   title2      Subtitle
#   outlierVal  Labels points with |standardized residual| > outlierVal
normalQQ <- function(model, title2='', outlierVal=2) {
    fmodel <- fortify(model)
    fmodel <- rownames_to_column(fmodel,var="label") %>%
        mutate(.stdresid = if_else(near(.resid, 0.), 0., .stdresid),
              .r. =rank(.stdresid),
              .x. = qnorm((.r.-0.5)/(length(.stdresid)))
              )
    ggplot(fmodel, aes(sample=.stdresid)) +
        qqplotr::stat_qq_band()  +
        qqplotr::stat_qq_line()  +
        qqplotr::stat_qq_point() +
        ggrepel::geom_text_repel(data=filter(fmodel, abs(.stdresid) > outlierVal),
                                 aes(x=.x., y=.stdresid, label=label)) +
        labs(x="Theoretical",
             y="Standaridized residuals",
             title=paste("Normal Q-Q:", title2
                         , sep="\n")) 
}

#---------------------------------------------------------------
# Plots sqrt(|residual|) vs fitted values
#   model       Model object created by lm()
#   title2      Subtitle
#   outlierVal  Labels points with |standardized residual| > outlierVal
scaleLocation <- function(model, title2='', outlierVal=2 ){
    fmodel <- fortify(model)
    fmodel <- rownames_to_column(fmodel,var="label") 
    ggplot(aes(x=.fitted, y=sqrt(abs(.stdresid))), data=fmodel) +
        geom_point() +
        geom_smooth(se = FALSE, span=0.95) +
        ggrepel::geom_text_repel(data=filter(fmodel, abs(.stdresid) > outlierVal),
                                 aes(label=label)) +
        labs(x="Fitted Values",
             y="Sqrt(|Standardized residuals|)",
             title=paste("Scale-Location: ", title2, sep="\n"))
}

#--------------------------------------------------------------
#  Plots Cooks distance vs data order
#   model       Model object created by lm()
#   title2      Subtitle
# Labels points with Cooks distance > 0.4
cooksDistance <- function(model,title2=''){
    fmodel <- fortify(model)
    fmodel <- rownames_to_column(fmodel,var="label") %>%
              mutate(order=seq_along(.cooksd))
    ggplot(aes(x=order, y= .cooksd), data=fmodel) +
        geom_col() +
        ggrepel::geom_text_repel(data=filter(fmodel,.cooksd > 0.4),
                  aes(label=label),position="stack") + 
        labs(x="Obs number",
             y="Cook's distance",
             title=paste("Cook's Distance: ", title2,  sep="\n")) 
}

#--------------------------------------------------------------------------
# Plots standardized residuals vs leverage.  Size of plotting symbol
# indicates Cooks distance.
#   model       Model object created by lm()
#   title2      Subtitle
#   Labels points with leverage > 3p/n. If none and one point with
#   leverage > 2p/n, labels it. Also, labels points withf Cook's distance > 0.4
residLeverage <- function(model,title2='',outlierVal=3){
  p <- length(coef(model))
  fmodel <- fortify(model) %>%
    mutate(.stdresid = if_else(near(.resid, 0.), 0., .stdresid)) %>%
    mutate(.cooksd = if_else(near(.resid, 0.), 0., .cooksd))
  n <- length(fmodel$.stdresid)
  n2 <- length(fmodel$.hat[fmodel$.hat > 2*p/n])
  n3 <- length(fmodel$.hat[fmodel$.hat > 3*p/n])
  fmodel <- rownames_to_column(fmodel,var="label")
  hmax <- min(1.0, max(3*p/n, max(fmodel$.hat)))
  rmax <- 1.25*max(fmodel$.stdresid)
  rmin <- 1.25*min(fmodel$.stdresid)
  skip <- near(rmax,0.) | near(rmin,0.) | max(fmodel$.cooksd) < 0.4
  if (!skip) {
    cook <- data.frame(hp1=seq(p*1.0/(rmax^2 + p*1.0), hmax, length.out=100),
                       hph=seq(p*0.5/(rmax^2 + p*0.5), hmax, length.out=100),
                       hn1=seq(p*1.0/(rmin^2 + p*1.0), hmax, length.out=100),
                       hnh=seq(p*0.5/(rmin^2 + p*0.5), hmax, length.out=100)) %>%
      mutate(rp1= if_else(hp1<hmax,  sqrt(p*(1-hp1)*1.0/hp1), as.numeric(NA)),
             rph= if_else(hph<hmax,  sqrt(p*(1-hph)*0.5/hph), as.numeric(NA)),
             rn1= if_else(hn1<hmax, -sqrt(p*(1-hn1)*1.0/hn1), as.numeric(NA)),
             rnh= if_else(hn1<hmax, -sqrt(p*(1-hnh)*0.5/hnh), as.numeric(NA)))
  }                
  g <- ggplot(aes(x=.hat, y=.stdresid), data=fmodel) +
    geom_point(aes(size=.cooksd), na.rm=TRUE) +
    geom_hline(yintercept=0) +
    geom_vline(xintercept=2*p/n, linetype="dashed") +
    geom_vline(xintercept=3*p/n, linetype = "dashed") +
    labs(x="Leverage",
         y="Standardized residuals",
         title=paste("Residuals vs Leverage: ", title2,  sep="\n")) +
    scale_size_continuous("Cook's D:", range=c(1,5)) +
    theme(legend.position="bottom")
  if (n3>0) {
    g <- g +
      ggrepel::geom_text_repel(data=filter(fmodel, .hat>3*p/n & .cooksd<=0.4),
                               aes(label=label))
  }
  if (n3==0 & n2==1) {
    g <- g +
      ggrepel::geom_text_repel(data=filter(fmodel, .hat>2*p/n & .cooksd<=0.4),
                               aes(label=label))
  }
  if (max(fmodel$.cooksd) > 0.4) {
    g <- g +
      ggrepel::geom_text_repel(data=filter(fmodel, .cooksd>0.4),
                               aes(label=label))
  }
  
  if(!skip) {
    g <- g + geom_line(data=cook[3:99,], aes(x=hp1, y=rp1), color="red", linetype="dashed") +
             geom_text(data=cook[1,], aes(x=hp1, y=rp1), color="red",
                       label=if_else(cook[1,"hp1"]<hmax, "1.0", as.character(NA))) +
      
             geom_line(data=cook[3:99,], aes(x=hph, y=rph), color="red", linetype="dashed") +
             geom_text(data=cook[1,], aes(x=hph, y=rph), color="red", 
                       label=if_else(cook[1,"hph"]<hmax, "0.5", as.character(NA))) +
      
             geom_line(data=cook[3:99,], aes(x=hn1, y=rn1), color="red", linetype="dashed") +
             geom_text(data=cook[1,], aes(x=hn1, y=rn1), color="red", 
                label=if_else(cook[1,"hn1"]<hmax, "1.0", as.character(NA))) +
      
             geom_line(data=cook[3:99,], aes(x=hnh, y=rnh), color="red", linetype="dashed") +
             geom_text(data=cook[1,], aes(x=hnh, y=rnh), color="red", 
                label=if_else(cook[1,"hnh"]<hmax, "0.5", as.character(NA)))
  }
  g
}

#--------------------------------------------------------------------------
# Regression diagnostic plots
#   model       Model object created by lm()
#   title2      Subtitle
#   outlierVal  Labels points with |standardized residual| > outlierVal
diagPlots123 <- function(model,title2, outlierVal=2) {
    gridExtra::grid.arrange(residFit(model,title2,outlierVal),
                 normalQQ(model,title2,outlierVal),
                 scaleLocation(model,title2,outlierVal),
                 ncol=2)
}

diagPlots45 <- function(model,title2) {
    gridExtra::grid.arrange(cooksDistance(model,title2),
                 residLeverage(model,title2),
                 ncol=2)
}