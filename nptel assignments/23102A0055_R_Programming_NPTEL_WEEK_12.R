# =====================================================================
# Foundations of R Software - NPTEL Week 12 (Lectures 50-53)
# Graphics + Examples of R Programming
# Run in Google Colab with the R runtime (Runtime > Change runtime type > R)
# =====================================================================

# Remove all data
rm(list = ls())

# =====================================================================
# LECTURE 50 : SUBDIVIDED BAR PLOTS AND PIE DIAGRAM
# =====================================================================

# ---------- Subdivided / component bar diagram ----------
# No. of customers visiting 3 shops during 10-11 AM on 4 consecutive days
cust = matrix(nrow = 4, ncol = 3,
              data = c(2, 20, 30,
                       26, 53, 40,
                       42, 15, 25,
                       30, 75, 100),
              byrow = T)
cust

# Basic subdivided bar diagram (columns of matrix become bars)
barplot(cust)

# Adding labels and colours
barplot(cust,
        names.arg = c("Shop 1", "Shop 2", "Shop 3"),
        xlab = "Shops", ylab = "Days",
        col = c("red", "green", "orange", "brown"))

# ---------- Pie diagram ----------
# Gender coding: 1 = male, 2 = female
gender = c(1, 2, 1, 2, 1, 1, 1, 2, 1, 1)
gender

pie(gender)               # "Do you want this?" - plots each value, not what we want
pie(table(gender))        # correct: pie of frequencies

# Pizza home delivery data (1 = East, 2 = West, 3 = Central)
direction = c(1,1,2,1,2,3,2,2,3,3,3,1,2,3,2,2,3,1,1,3,3,1,2,
              1,3,3,3,2,2,2,2,1,2,2,1,1,1,3,2,2,1,2,3,2,2,1,
              2,3,3,2,1,2,2,3,1,1,2,1,2,3,2,3,2,2,3,1,2,3,3,3,
              2,1,1,1,2,1,1,2,1,2,3,3,1,2,3,3,2,1,2,3,2,1,3,
              2,2,2,2,3,2,2)

pie(table(direction))

# Adding colours and main title
pie(table(direction), col = c("red", "green", "blue"),
    main = "Directions of food delivery")

# ---------- Combining graphics with par() ----------
par(mfrow = c(1, 2))      # 1 x 2 array
barplot(table(direction))
pie(table(direction))

par(mfrow = c(2, 1))      # 2 x 1 array
barplot(table(direction))
pie(table(direction))

par(mfrow = c(1, 1))      # reset to a single plot


# =====================================================================
# LECTURE 51 : HISTOGRAM
# =====================================================================
# hist(x)            -> absolute frequencies
# hist(x, freq = F)  -> relative frequencies (density)

# Height of 50 persons in centimetres
height = c(166,125,130,142,147,159,159,147,
           165,156,149,164,137,166,135,142,133,136,127,143,
           165,121,142,148,158,146,154,157,124,125,158,159,
           164,143,154,152,141,164,131,152,152,161,143,143,
           139,131,125,145,140,163)

hist(height)

# Relative frequencies
hist(height, freq = F)

# Adding colour to bars and titles on axes
hist(height, main = "Heights of persons", col = "green",
     xlab = "Heights", ylab = "Number of Persons")

# Using xlim and ylim
hist(height, main = "Heights of persons", col = "green",
     xlab = "Heights", ylab = "Number of Persons",
     xlim = c(100, 200), ylim = c(0, 20))

# Adding shading density (lines per inch)
hist(height, main = "Heights of persons", col = "red",
     xlab = "Heights", ylab = "Number of Persons", density = 2)

# Increasing density
hist(height, main = "Heights of persons", col = "red",
     xlab = "Heights", ylab = "Number of Persons", density = 8)

# Adding angle of shading lines
hist(height, main = "Heights of persons", col = "red",
     xlab = "Heights", ylab = "Number of Persons",
     density = 8, angle = 100)

# help("hist")   # more details


# =====================================================================
# LECTURE 52 : BIVARIATE AND THREE DIMENSIONAL SCATTER PLOTS
# =====================================================================

# Marks (out of 500) of 20 students and hours studied per week
marks = c(337,316,327,340,374,330,352,353,370,380,
          384,398,413,428,430,438,439,479,460,450)
hours = c(23,25,26,27,30,26,29,32,33,34,
          35,38,39,42,43,44,45,46,44,41)

# ---------- plot() with different types ----------
plot(hours, marks)            # points (default)
plot(hours, marks, "l")       # "l" lines
plot(hours, marks, "b")       # "b" both points and lines
plot(hours, marks, "o")       # "o" both, overplotted
plot(hours, marks, "h")       # "h" histogram-like (high-density) vertical lines
plot(hours, marks, "s")       # "s" stair steps
# other types: "p" points, "c" lines part alone of "b"

# With axis labels and title
plot(hours, marks,
     xlab = "Number of weekly hours",
     ylab = "Marks obtained",
     main = "Marks obtained versus Number of hours per week")

# ---------- Matrix scatter plot ----------
pairs(cbind(hours, marks))

# Adding labels and colour
pairs(cbind(hours, marks),
      labels = c("Study hours", "Marks obtained"),
      col = "red")

# ---------- Scatter plot with smooth curve (LOESS) ----------
scatter.smooth(hours, marks)

# More options
scatter.smooth(hours, marks, lpars = list(col = "red", lwd = 3, lty = 3))

# help("scatter.smooth")   # span, degree, family, xlab, ylab, ylim ...

# ---------- Three dimensional scatter plot ----------
if (!require("scatterplot3d")) {
  install.packages("scatterplot3d")
}
library(scatterplot3d)

# Height (cm), weight (kg) and age (years) of 5 persons
height = c(100, 125, 145, 160, 170)
weight = c(30, 35, 50, 65, 70)
age    = c(10, 15, 20, 30, 35)

scatterplot3d(height, weight, age)

# Change direction of the figure
scatterplot3d(height, weight, age, angle = 120)

# Change colour of points
scatterplot3d(height, weight, age, color = "red")

# ---------- More functions (mentioned in notes) ----------
# contour()    contour lines
# dotchart()   dot charts (replacement for bar charts)
# image()      pictures with colours as third dimension
# mosaicplot() mosaic plot for categorical variables (contingency tables)
# persp()      perspective surfaces over the x-y plane

# Example of perspective plot
x = seq(-10, 10, length = 30)
y = x
f = function(x, y) { r = sqrt(x^2 + y^2); 10 * sin(r) / r }
z = outer(x, y, f)
z[is.na(z)] = 1
op = par(bg = "white")

persp(x, y, z, theta = 30, phi = 30, expand = 0.5, col = "lightblue")

persp(x, y, z, theta = 30, phi = 30, expand = 0.5, col = "lightblue",
      ltheta = 120, shade = 0.75, ticktype = "detailed",
      xlab = "X", ylab = "Y", zlab = "Sinc( r )")

par(op)


# =====================================================================
# LECTURE 53 : SOME EXAMPLES OF R PROGRAMMING
# =====================================================================
# Tips: loops are slow -> prefer vectors/matrices; use # for comments;
#       use meaningful variable names; initialize variables.

rm(list = ls())

# ---------------------------------------------------------------------
# Example 1
#   g = sum(x_i^2) / sum(y_i^2)      h = sum( (x_i / y_i)^2 )
# Input : x, y     Output : g, h
# ---------------------------------------------------------------------
example1 = function(x, y)
{
  # Number of observations
  n = length(x)

  # Initialize vectors to store squared values
  x1 = 0
  y1 = 0
  z1 = 0

  # Start of loop
  for (i in 1:n)
  {
    x1[i] = x[i]^2
    y1[i] = y[i]^2
    z1[i] = (x[i] / y[i])^2
  }   # End of loop

  # Sum of squared quantities
  sum_square_x = sum(x1)
  sum_square_y = sum(y1)
  sum_square_z = sum(z1)

  # Computation of g and h
  g = sum_square_x / sum_square_y
  h = sum_square_z

  # Format the output
  cat("The value of g and h are", g, "and", h, "respectively", "\n")
}

# Test 1
x = c(10, 20, 30)
y = c(1, 2, 3)
example1(x, y)       # g = 100, h = 300

# Test 2
x = c(67, 87, 26, 85, 6, 45)
y = c(54, 64, 22, 94, 20, 88)
example1(x, y)       # g = 0.8996568, h = 5.953203

# ---- Example 1 (alternative approach, without loop) ----
# Vectorised version; same outputs as the loop version above
example1_alt = function(x, y)
{
  g = sum(x^2) / sum(y^2)
  h = sum((x / y)^2)     # sum of squared ratios (matches the loop output)
  cat("The value of g and h are", g, "and", h, "respectively", "\n")
}

x = c(10, 20, 30); y = c(1, 2, 3)
example1_alt(x, y)

x = c(67, 87, 26, 85, 6, 45); y = c(54, 64, 22, 94, 20, 88)
example1_alt(x, y)

# Note: the slide writes the alternative as  h = sum(x/y)^2 , which is
# (sum of ratios) squared and gives a different value (900 for the first
# data set). Use whichever matches the formula you are asked to compute:
cat("h using sum(x/y)^2 :", sum(c(10,20,30) / c(1,2,3))^2, "\n")


# ---------------------------------------------------------------------
# Example 2
#   g(x,y) = (x + ln y) / y
#   f(x,y) = [ g^2 / (5 + g^3) ] * ( exp(g) )^(2/3)
# Input : x, y     Output : f
# Idea  : compute g as a function, then call it inside f
# ---------------------------------------------------------------------
rm(list = ls())

# define g(x,y)
g = function(x, y)
{
  (x + log(y)) / y
}

# define f(x,y)  (g(x,y) must have been defined earlier)
f = function(x, y)
{
  (((g(x, y))^2) / (5 + (g(x, y))^3)) * (exp(g(x, y)))^(2/3)
}

x = 10
y = 20
f(x, y)              # 0.1234539

x = 1896
y = 23454
f(x, y)              # 0.001394291


# ---------------------------------------------------------------------
# Example 3
#            exp( (x + ln(1 + x^3)) / x^2 )   if x > 0
#   f(x) =   10                                if x = 0
#            (2 + x^3) / x                     if x < 0
# and plot f(x) as a line for x = -1 to 5 in steps of 0.2
# ---------------------------------------------------------------------
rm(list = ls())

f = function(x)
{
  if (x > 0) {
    exp((x + log(1 + x^3)) / x^2)
  } else if (x == 0) {
    10
  } else {
    (2 + x^3) / x
  }
}

h = function()
{
  # Generation of data on x
  x = seq(-1, 5, by = 0.2)

  # Initialization of y to store values of f(x)
  y = 0

  # Generation of f(x) values corresponding to x
  for (i in 1:length(x))
  {
    y[i] = f(x[i])
  }

  # length(x) and length(y) must be same to plot y = f(x) against x
  plot(x, y, type = "l")
}

# Checking the function at some values
f(123)     # 1.009126
f(-123)    # 15128.98
f(0)       # 10
f(8)       # 1.249201
f(-4)      # 15.5

# Plot
h()

# =====================================================================
# END OF SCRIPT
# =====================================================================
