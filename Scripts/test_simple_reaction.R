
## Create the nodes
substrate <- GraphicNode$new(1)
substrate$label <- 'substrate'
product <- GraphicNode$new(2)
product$label <- 'product'

enzyme <- GraphicNode$new()
enzyme$label <- 'enzyme'



# Obtain the layout
enzyme$addChild(substrate)
enzyme$addChild(product)

global_pars$blank_area_ratio$r <- 0.2
global_pars$blank_area_ratio$t <- 0.2
global_pars$blank_area_ratio$b <- 0.2

tree <- GraphicTree$new(enzyme)
tree$assignAttributes()

rectLayoutDesigner<- RectangularLayoutDesigner$new()
rectLayoutDesigner$calculate(tree)
rectLayoutDesigner$plotTree(tree)



grid.newpage()
grid.circle(
  x = substrate$xAxis_or_radius,
  y = substrate$yAxis_or_angle,
  r = 0.7 ,
  default.units = 'in',
  gp = gpar(fill = NA)
)


grid.circle(
  x = product$xAxis_or_radius,
  y = product$yAxis_or_angle,
  r = 0.7 ,
  default.units = 'in',
  gp = gpar(fill = NA)
)


grid.circle(
  x = enzyme$xAxis_or_radius,
  y = enzyme$yAxis_or_angle,
  r = 0.7 ,
  default.units = 'in',
  gp = gpar(fill = NA)
)

