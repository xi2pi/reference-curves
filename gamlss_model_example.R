
# author: Christian Winkler, University Hospital Bonn
# date:   19-01-2018

# Load packages
library(gamlss)

# Load some example data from the GAMLSS package
training_data <- abdom

##################
#### Configuration
##################

# Smoothing parameters
median_df = 1
sigma_df = 0
nu_df = 0

# Plot with points yes/no
points_on = TRUE

# resolution of plot
res_plot = 400

# Choose a title and labels for the axes
graph_title = "My example Data"

x_label = "x axis"
y_label = "y axis"

# Choose the limits and step size for the axes
xlim1 = 10
xlim2 = 47
xsteps = 5

ylim1 = 50
ylim2 = 430
ysteps = 50


##################
#### Fitting Model
##################

model <- gamlss(y ~ pb(x, df = median_df),
             sigma.formula = ~ pb(x, df = sigma_df),
             nu.formula = ~pb(x, df = nu_df),
             family = "BCCG",
             method = RS(),
             data = training_data)

summary(model)

# define the position of the label for the percentiles
label_perc <- centiles.pred(model, xvalues=max(training_data$x), xname= "x", cent = c(3, 10, 25, 50, 75, 90, 97))

png("./reference_curves_example_1.png", 
    width     = 4.5,
    height    = 3.5,
    units     = "in",
    res       = 1200,
    pointsize = 5)

# Plotting curves
centiles(model,
         xvar = training_data$x,
         ylim = c(ylim1,ylim2),
         xlim = c(xlim1,xlim2),
         legend = FALSE,col=1,col.cent=1,
         cent = c(3, 10, 25, 50, 75, 90, 97),
         lwd.cent=c(0.8,0.8,0.8,1.5,0.8,0.8,0.8),
         main=graph_title,
         xlab = "",
         ylab = "",
         labels = FALSE,
         axes=TRUE,
         points = points_on,
         tck = FALSE,
         xaxs="i",
         yaxs="i")


# Plotting Labels
title(ylab = y_label, line=2.2)
title(xlab = x_label, line=1.6)
axis(1, at = seq(xlim1, xlim2, by=xsteps), labels = seq(xlim1, xlim2, by=xsteps),tck = -0.01, cex.axis=0.8,mgp=c(0,0.2,0))
abline(h= seq(ylim1, ylim2, by=ysteps), v=seq(xlim1, xlim2, by=xsteps), col="gray", lty=3)

axis(2, at = seq(ylim1, ylim2, by=ysteps), labels = seq(ylim1, ylim2, by=ysteps),tck = -0.01, cex.axis=1,las=2,mgp=c(0,0.4,0))
text(max(training_data$x)+ 2, label_perc$C3, "P3",cex=1)
text(max(training_data$x)+ 2, label_perc$C10, "P10",cex=1)
text(max(training_data$x)+ 2, label_perc$C25, "P25",cex=1)
text(max(training_data$x)+ 2, label_perc$C50, "P50",cex=1)
text(max(training_data$x)+ 2, label_perc$C75, "P75",cex=1)
text(max(training_data$x)+ 2, label_perc$C90, "P90",cex=1)
text(max(training_data$x)+ 2, label_perc$C97, "P97",cex=1)
dev.off()
