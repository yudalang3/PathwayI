# example1
display_TF_targets_coexpress()
display_TF_targets_coexpress(buttomStyleBezier = T)
# example2
set_global_pars('fontsize', 7)
data = list(
  TF = "TCF/LEF",
  targets = c('Axin2','TCF7','ST7','BZRAP1', 'PDK2', 'PIK3C2A', 'CLK1' , 'SERPINB1','SPEN','NUCB2','JMJD6','DGKD','MLLT10'),
  coexpression = c('SPEN','NUCB2','JMJD6','DGKD','MLLT10','AXIN2','PIK3C2A','SERPINB1','TP53','CXCL1','INS')
)
display_TF_targets_coexpress(data = data,buttomStyleBezier = T)
# example 3
set_global_pars('fontsize', 12)
data = list(
  TF = "TF1",
  targets = paste0("g", 1:10),
  coexpression = c(paste0("g", 1:4), paste0("k", 1:4))
)
display_TF_targets_coexpress(data = data)


