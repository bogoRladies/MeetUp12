#kunci jawaban
#set working directory
setwd("D:\\[2022] R Ladies 2022\\R Ladies Bogor\\Maret")
#baca data
ds<-read.csv("GTL.csv")
#cek beberapa baris teratas
head(ds)
#karena ada dua faktor: tipe kaca dan suhu
#maka kita coba pakai two way anova

ds.aov<-aov(Light~Temp+Glass,
            data=ds)
summary(ds.aov)

#cek distribusi residual
shapiro.test(resid(ds.aov))

#cek histogram
hist(resid(ds.aov))

#mungkin harus pakai model lanjutan (regresi linear dst)

#visualisasi

ggplot(ds, aes(x=as.factor(Temp), y=Light, color=Glass))+
  geom_boxplot()+
  ylab("Output cahaya")+
  xlab("Suhu")+
  theme_bw()+
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=14),
        plot.title=element_text(size=14),
        axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = 20, r = 20, b = 0, l = 0)))+
  guides(color=guide_legend(title="Tipe kaca"))+
  scale_color_brewer(palette="Set1")

