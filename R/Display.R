show_phospholipid_models <- function() {
  # w_and_h <- par('din')


  grid.newpage()
  for (i in 1:6) {
    painter <- create_lipidBilayer_drawer(lipidModelIndex = i)
    scaler <- 1 / 100
    painter$draw_one_unit_lipid(
      scaleWidth = scaler,
      scaleHeight = scaler,
      theta = 0,
      moveX = 0.5 * i,
      moveY = 2
    )

    grid.text(label = i , x = 0.5 *i , y = 1, default.units = 'in')
  }
}
