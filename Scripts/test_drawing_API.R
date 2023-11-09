pathIllustrator <- membrane(shape = 'ellipse', x = 1, y = 1, w = 3, h = 2) +
  nucleus(envelop = list(x = 2, y = 2, w = 1, h = 1), DNA = list(x = 2.5,y = 2.5, h = 0.3, rep = 2)) +
  nodes(data = df1) +
  edges(fixIndi = df2, freeIndi = df3)
