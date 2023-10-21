w_and_h <- par('din')

xx <- seq(from = 1, to = w_and_h[1] - 1, length.out = 40)
# the simplest form of the line
# y <- -x + w_and_h[2]
# yy <- xx + 0.5
# yy <- xx^ (1/3) + 0.5
# yy <- sin(xx) + 3
yy <- rep(2,length(xx))

# aa <- produce_model_coordinate_points(from = 0,by = 5,to = 360,radius = 2)
# xx <- aa[1,] + 3
# yy <- aa[2,] + 3

grid.newpage()

xyPrime <- do_bilateral_extension_alongCurve(xx,yy,d = 0.2)
xyPrime <- do_bilateral_extension_alongCurve(rep(2,5),1:5,d = 0.2)
xyPrime <- do_bilateral_extension_alongCurve(1:5,1:5,d = 0.2)

x = xyPrime$xPrime_1;y = xyPrime$yPrime_1
len_ofX <- checkCurvePoints(x, y)

x_a <- x[1:(len_ofX - 1)]
y_a <- y[1:(len_ofX - 1)]
x_b <- x[2:len_ofX]
y_b <- y[2:len_ofX]

half_distance_all <- distance(x_a,y_a,x_b,y_b) *0.5

temp_length <- length(x)
grid.segments(
  x0 = xx[1:temp_length] - 0.05,
  y0 = yy[1:temp_length],
  x1 = x[1:temp_length],
  y1 = y[1:temp_length],
  default.units = 'in'
)
grid.segments(
  x0 = xx[1:temp_length] + 0.05,
  y0 = yy[1:temp_length],
  x1 = x[1:temp_length],
  y1 = y[1:temp_length],
  default.units = 'in'
)
## Draw the first lipid

lipid_head_par <- gpar(fill = 'lightblue', col = 'grey30')
grid.circle(x[1],y[1], default.units = 'in', r = half_distance_all[1], gp = lipid_head_par)
grid.circle(x_b,y_b, default.units = 'in', r = half_distance_all,gp = lipid_head_par)

# grid.lines(x = x,y = y, default.units = 'in')
# grid.text(
#   label = 1:10,
#   x = xyPrime$xPrime_1[1:10],
#   y = xyPrime$yPrime_1[1:10] + 0.2,
#   default.units = 'in'
# )


###############################
x = xyPrime$xPrime_2;y = xyPrime$yPrime_2
len_ofX <- checkCurvePoints(x, y)

x_a <- x[1:(len_ofX - 1)]
y_a <- y[1:(len_ofX - 1)]
x_b <- x[2:len_ofX]
y_b <- y[2:len_ofX]

half_distance_all <- distance(x_a,y_a,x_b,y_b) *0.5

temp_length <- length(x)
grid.segments(
  x0 = xx[1:temp_length] + 0.05,
  y0 = yy[1:temp_length],
  x1 = x[1:temp_length],
  y1 = y[1:temp_length],
  default.units = 'in'
)
grid.segments(
  x0 = xx[1:temp_length] - 0.05,
  y0 = yy[1:temp_length],
  x1 = x[1:temp_length],
  y1 = y[1:temp_length],
  default.units = 'in'
)
## Draw the first lipid
grid.circle(x[1],y[1], default.units = 'in', r = half_distance_all[1], gp = lipid_head_par)
grid.circle(x_b,y_b, default.units = 'in', r = half_distance_all,gp = lipid_head_par)

# grid.lines(x = x,y = y, default.units = 'in')
# grid.text(
#   label = 1:10,
#   x = xyPrime$xPrime_1[1:10],
#   y = xyPrime$yPrime_1[1:10] + 0.2,
#   default.units = 'in'
# )
#

grid.lines(xx,yy, default.units = 'in',gp = gpar(col = 'white', lwd = 2))

