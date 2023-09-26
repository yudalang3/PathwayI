# example1
display_TF_targets_coexpress()
display_TF_targets_coexpress(buttomStyleBezier = T)

# example2

set_global_pars('fontsize', 8)
data = list(
  TF = "TCF/LEF",
  targets = c('Axin2','TCF7','ST7','BZRAP1', 'PDK2', 'PIK3C2A', 'CLK1' , 'SERPINB1'),
  coexpression = c('Axin2','PIK3C2A','SERPINB1','TP53','CXCL1','INS')
)
display_TF_targets_coexpress(data = data,buttomStyleBezier = F)


data = list(
  TF = "TF1",
  targets = paste0("g", 1:10),
  coexpression = c(paste0("g", 1:4), paste0("k", 1:4))
)
display_TF_targets_coexpress(data = data)


