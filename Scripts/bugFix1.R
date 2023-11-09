
coords <- create_round_rectangle_xyCoords(x = 1, y = 0.2, w = 3, h =2, r = 1, space = 0.1,angle_for_roundCorner = 5)

grid.newpage()
nCol <- ncol(coords)
lineX = coords[1, 50:nCol];
lineY = coords[2, 50:nCol]

xyPrime <-
  do_bilateral_extension_alongCurve(lineX, lineY, d = 0.15, closedPolygon = F)


grid.lines(lineX,lineY,default.units = 'in', gp = gpar(col = 'orange'))
grid.circle(lineX,lineY,default.units = 'in', gp = gpar(col = 'orange'),r= unit(0.5,'mm'))
# grid.text(1:nCol,lineX,lineY + 0.1,default.units = 'in', gp = gpar(col = 'orange',fontsize = 7))
grid.lines(xyPrime$xPrime_1, xyPrime$yPrime_1, default.units = 'in', gp = gpar(col = 'green'))
grid.circle(xyPrime$xPrime_1, xyPrime$yPrime_1, default.units = 'in',r= unit(0.5,'mm'), gp = gpar(col = 'green'))
# grid.text(1:(nCol-1),xyPrime$xPrime_1,xyPrime$yPrime_1 + 0.1,default.units = 'in', gp = gpar(col = 'green',fontsize = 7))
grid.lines(xyPrime$xPrime_2, xyPrime$yPrime_2, default.units = 'in', gp = gpar(col = 'blue'))
grid.circle(xyPrime$xPrime_2, xyPrime$yPrime_2, default.units = 'in', r= unit(0.5,'mm'),gp = gpar(col = 'blue'))
# grid.text(1:(nCol-1),xyPrime$xPrime_2,xyPrime$yPrime_2 + 0.1,default.units = 'in', gp = gpar(col = 'blue',fontsize = 7))




################################################################################

grid.newpage()
aa <- produce_model_coordinate_points(from = 0, to = 360)

lineX = aa[1, ] + 3;
lineY = aa[2, ] + 3

xyPrime <-
  do_bilateral_extension_alongCurve(lineX, lineY, d = 0.15, closedPolygon = F)

grid.lines(lineX,lineY,default.units = 'in', gp = gpar(col = 'orange'))
grid.lines(xyPrime$xPrime_1, xyPrime$yPrime_1 +, default.units = 'in', gp = gpar(col = 'green'))
# grid.circle(xyPrime$xPrime_1, xyPrime$yPrime_1, default.units = 'in',r= unit(1,'mm'), gp = gpar(col = 'green'))
grid.lines(xyPrime$xPrime_2, xyPrime$yPrime_2, default.units = 'in', gp = gpar(col = 'blue'))
