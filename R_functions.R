plist <- c("dplyr",
           "lmerTest",
           "performance",
           "parameters",
           "tidyverse", 
           "janitor", 
           "readxl", 
           "lme4",
           "lmerTest",
           "emmeans",
           "performance",
           "broom.mixed",
           "spdep",
           "spatialreg",
           "raster",
           "nlme",
           "openxlsx",
           "data.table",
           "tsne",
           "scatterplot3d",
           "plotly",
           "pheatmap",
           "dendextend",
           "Hmisc",
           "corrplot",
           "plyr",
           "kableExtra",
           "stringr"
          )  
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
#add this in ggplot: "+geom_text(x=-2.5, y=0.2, label = eq2(df_3$Tc_Ta,df_3$gsw), parse = TRUE, colour="gray33")+"
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


eq2 <- function(x, y) {
  m <- lm(y ~ x)  # Fit a linear regression model of y ~ x
  # Create a formatted string showing only R-squared
  formatted_r2 <- as.character(
    as.expression(
      substitute(
        italic(r)^2 ~ "=" ~ r2,
        list(
          r2 = format(summary(m)$r.squared, digits = 3)  # Format R-squared value
        )
      )
    )
  )
  return(formatted_r2)
}

## This one provides the equation only:
eq3 <- function(x, y) {
  m <- lm(y ~ x)  # Fit a linear regression model of y ~ x
  
  # Extract coefficients and R-squared
  a <- format(coef(m)[1], digits = 4)  # Intercept
  b <- format(coef(m)[2], digits = 4)  # Slope
  r2 <- format(summary(m)$r.squared, digits = 3)  # R-squared
  
  # Create the equation and R-squared strings
  equation <- paste0("italic(y) == ", a, " + ", b, " %.% italic(x)")
  r_squared <- paste0("italic(r)^2 == ", r2)
  
  # Combine into a two-line string with newline character
  formatted <- paste0(equation, "\n", r_squared)
  
  # Return the formatted equation as an expression for plotting
  return(as.expression(formatted))
}



############ QQ plot manually created. Residuals must be extracted and added as a new column in the original data frame from which the linear model was built on.
############ Usage example:
############ manual_qq_plot(df_summary2, "residuals")
manual_qq_plot <- function(df, column_name) {
  # Calculate standard deviation of residuals, ignoring NAs
  std_dev_residuals <- sd(df[[column_name]], na.rm = TRUE)
    # Normalize the data by standard deviation
  my_data <- df[[column_name]] / std_dev_residuals
    # Remove NAs
  my_data <- my_data[!is.na(my_data)]
    # Calculate observed quantiles (sort your data)
  observed_quantiles <- sort(my_data)
    # Calculate theoretical quantiles
  n <- length(my_data)  # Number of data points
  theoretical_quantiles <- qnorm((1:n - 0.5) / n)
    # Create the plot with equal x and y limits
  plot(theoretical_quantiles, observed_quantiles, 
       xlab = "Theoretical Quantiles", 
       ylab = "Observed Quantiles", 
       main = "QQ Plot",
       xlim = range(c(theoretical_quantiles, observed_quantiles)),
       ylim = range(c(theoretical_quantiles, observed_quantiles))
  )
    # Add a 45-degree line
  abline(0, 1, col = "red")
}

################### Error propagation for MP index: 
# First get sample data
sample_data <- data.frame(
  Genotype = c("001", "001", "002", "002"),
  Treatment = c("Drought", "Irrigated", "Drought", "Irrigated"),
  estimated_mean = c(2, 3, 4, 2),
  standard_error = c(0.5, 0.2, 0.1, 0.3)
)

# Split data by genotype
split_data_sample <- split(sample_data, sample_data$Genotype)

# Propagation of errors
# Function to calculate MP tolerance index and its error for each genotype
calculate_MP <- function(genotype_data){
  Ypi <- genotype_data$estimated_mean[genotype_data$Treatment == "Irrigated"]
  Ysi <- genotype_data$estimated_mean[genotype_data$Treatment == "Drought"]
  sigma_pi <- genotype_data$standard_error[genotype_data$Treatment == "Irrigated"]
  sigma_si <- genotype_data$standard_error[genotype_data$Treatment == "Drought"]
  MP <- (Ypi + Ysi) / 2
  MP_error <- sqrt((sigma_pi^2 + sigma_si^2) / 4)
  return(data.frame(Genotype = unique(genotype_data$Genotype), MP = MP, MP_error = MP_error))
}

# Apply the function to each genotype
MP_index <- do.call(rbind, lapply(split_data_sample, calculate_MP))


## The function 'panel.cor' and 'panel.hist' described here 
## are designed to be used as a custom panel functions in a pairs plot 
## (e.g., created with the pairs() function in R). 
## Its primary purpose is to display the correlation coefficient between two variables in the cells of the upper or lower triangle of a pairs plot matrix.
## See for more details: https://r-graphics.org/recipe-scatter-splom


panel.cor <- function(x, y, digits = 2, prefix = "", cex.cor, ...) { # x and y are the pairs of variables for which the correlation will be calculated and displayed. 'cex.cor' is a character expansion factor for the correlation coefficient text, determining its size. If it's not provided, it will be calculated based on the text width.
  usr <- par("usr")
  on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- abs(cor(x, y, use = "complete.obs"))
  txt <- format(c(r, 0.123456789), digits = digits)[1]
  txt <- paste(prefix, txt, sep = "")
  if (missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
  text(0.5, 0.5, txt, cex =  cex.cor * (1 + r) / 2)
}

panel.hist <- function(x, ...) {
  usr <- par("usr")
  on.exit(par(usr))
  par(usr = c(usr[1:2], 0, 1.5) )
  h <- hist(x, plot = FALSE)
  breaks <- h$breaks
  nB <- length(breaks)
  y <- h$counts
  y <- y/max(y)
  rect(breaks[-nB], 0, breaks[-1], y, col = "white", ...)
}

####### Format p.value columns 
# use it like this: CTD_stats$p.value_format <- sapply(CTD_stats$p.value, format_p_value)

format_p_value <- function(p) {
  if (is.na(p)) {
    return(NA)
  }
  formatted_p <- formatC(p, format = "f", digits = 3)
  if (p < 0.001) {
    return(paste0(formatted_p, "***"))
  } else if (p < 0.01) {
    return(paste0(formatted_p, "**"))
  } else if (p < 0.05) {
    return(paste0(formatted_p, "*"))
  } else {
    return(formatted_p)
  }
}

# Here "p" is a numeric vector. Usually is the p.value column of the z1 or z_con data frame generated with emmeans()
format_p_value_2 <- function(p) { 
  if (is.na(p)) {
    return(NA)
  }
  formatted_p <- formatC(p, format = "f", digits = 3)
  if (p < 0.001) {
    return(paste0("***"))
  } else if (p < 0.01) {
    return(paste0("**"))
  } else if (p < 0.05) {
    return(paste0("*"))
  } else {
    return(NA)
  }
}

format_p_value_3 <- function(p) {
  if (is.na(p)) {
    return(NA)
  }
  formatted_p <- formatC(p, format = "f", digits = 3)
  if (p < 0.001) {
    return(paste("<0.001", "***"))
  } else if (p < 0.01) {
    return(paste("<.01", "**"))
  } else if (p < 0.05) {
    return(paste(formatted_p, "*"))
  } else {
    return(formatted_p)
  }
}


