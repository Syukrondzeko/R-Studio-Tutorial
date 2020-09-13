library(ggplot2)
p1=ggplot(dataku, aes(x = jml_unit, y = waktu)) +
  geom_point() +
  stat_smooth()
p2=ggplot(dataku, aes(x = jarak, y = waktu)) +
  geom_point() +
  stat_smooth()
gridExtra::grid.arrange(p1,p2,ncol=2)
