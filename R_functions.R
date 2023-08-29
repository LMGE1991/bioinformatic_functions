
plist <- c("tidyverse", "janitor", "readxl", "lme4","lmerTest","emmeans","performance","broom.mixed","spdep","spatialreg","raster","nlme")  
for(p in plist){
  if(! p %in% installed.packages()){
    install.packages(p)
  }
}
for(p in plist){
  require(p,character.only = T)
}

#create an empty plot with ranges x=c(low,high) and y=ditto
null_plot <- function(x,y,xlab=NA,ylab=NA,revx=F,revy=F,...){
  xl<-range(x,na.rm=T)
  yl<-range(y,na.rm=T)
  if(revx==T){ xl <- rev(xl) }
  if(revy==T){ yl <- rev(yl) }
  plot(NULL,xlim=xl,ylim=yl,xlab=xlab,ylab=ylab,...)
}

#correlation coefficient R^2__________________________________________
#This function takes two arguments, x and y, which represent the predictor and response variables, respectively. 
#The function calculates a linear regression model using lm() and then constructs a formatted equation string that displays the linear regression equation, coefficients, and R-squared value.
eq <- function(x, y) {
  m <- lm(y ~ x)  # Fit a linear regression model of y ~ x
  # Create a formatted equation string
  formatted_eq <- as.character(
    as.expression(
      substitute(
        italic(y) == a + b %.% italic(x) * "," ~~ italic(r)^2 ~ "=" ~ r2,
        list(
          a = format(coef(m)[1], digits = 4),                 # Format coefficient a
          b = format(coef(m)[2], digits = 4),                 # Format coefficient b
          r2 = format(summary(m)$r.squared, digits = 3)      # Format R-squared value
        )
      )
    )
  )

  return(formatted_eq)  # Return the formatted equation string
}




