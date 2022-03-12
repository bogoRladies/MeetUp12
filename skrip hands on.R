#kalau belum, install dulu package berikut ini:
#install.packages("ggplot2")
#install.packages("RColorBrewer")

library(ggplot2)
library(RColorBrewer)

## Classical test



#Misalkan, kita ingin tahu apakah banyak mobil (rata-rata) yang masuk 
#pom bensin A pada jam 11 siang, berbeda dengan jumlah yang masuk pada pukul 7 pagi. 
#Kita tahu, pada pukul 7 pagi, rata-ratanya adalah 15 mobil.

#jumlah mobil yg masuk diukur selama 10 hari
jam11<-c(3, 7, 11, 0, 7, 0, 4, 5, 6, 2)
#cek tipe data
class(jam11)
#cek distribusi data
qqnorm(jam11)
qqline(jam11)
shapiro.test(jam11)

## t-test

t.test(jam11, mu=15)
#mu adalah nilai rata-rata di jam 7 pagi

t.test(jam11, mu=5)
#mu adalah nilai rata-rata di jam 7 pagi

#Bagaimana kalau kita ingin membandingkan dua nilai rata-rata?
#Misalkan, kita ingin tahu apakah berat rata-rata ikan mas di supermarket A 
#berbeda dengan yg ditemukan di supermarket B?
  
# Data bernilai numerik

supA <- c(389, 612, 733, 218, 634, 646, 484, 488, 485)
supB <- c(678, 600, 634, 760, 894, 733, 673, 613, 624) 

# Siapkan dataframe
my_data <- data.frame( 
  group = rep(c("supermarket A", "supermarket B"), each = 9),
  weight = c(supA,  supB)
)

#print 
print (my_data)

# t-test

t.res<-t.test(supA, supB, var.equal = T)
t.res

#atau

t.res <-t.test(weight~group, data= my_data, var.equal = T)
t.res

#Visualisasi?

#base plot
boxplot(weight~group, data=my_data, 
        ylab="Berat ikan (gr)",
        xlab="Supermarket")

#ggplot
ggplot(my_data, aes(x=group, y=weight, color=group))+
  geom_boxplot()+
  ylab("Berat (gr)")+
  xlab("Supermarket")+
  theme_bw()+
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=14),
        plot.title=element_text(size=14),
        axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = 20, r = 20, b = 0, l = 0)))+
  guides(color=guide_legend(title="Nama supermarket"))+
  scale_color_brewer(palette="Set1")

## anova

#Kita akan menggunakan contoh dataset yang ada di dalam R, 
#'PlantGrowth', yakni data berat tanaman dalam eksperimen dengan jenis perlakuan "control", 
#"treatment1", dan "treatment2". Apakah ada perbedaan dari berat tanaman dengan perlakukan 
#berbeda ini?
  
my_data<-PlantGrowth
print(my_data)

res.aov<-aov(weight~group, data=my_data)
summary(res.aov)

#Kita bisa melihat perbedaan antar kelompok/group dengan Tukey Honest Significant Differences 
#(multiple pairwise-comparison test).

TukeyHSD(res.aov)

# visualisasi

ggplot(my_data, aes(x=group, y=weight, color=group))+
  geom_boxplot()+
  ylab("Berat tanaman (gr)")+
  xlab("Perlakuan")+
  theme_bw()+
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=14),
        plot.title=element_text(size=14),
        axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = 20, r = 20, b = 0, l = 0)))+
  guides(color=guide_legend(title="Tipe perlakuan"))+
  scale_color_brewer(palette="Set1")

#Untuk anova, distribusi data dicek setelah model dijalankan. 
#Dan ini diterapkan pada 'residual', atau sisa 'pola' yang masih ada setelah model dibuat. 
#Apabila masih ada pola, artinya distribusi data residual tidak acak, yang artinya kita masih 
#harus menjelaskan lebih lanjut hasil permodelan yang dilakukan (mencari variabel penjelas lain 
#misalnya).

#ekstra residual
aov_resid<-resid(res.aov)
#run shapiro-wilk test
shapiro.test(x=aov_resid)

#cek residual dengan histogram
hist(aov_resid)

plot(res.aov, 2)

#Bagaimana kalau kita punya dua faktor yang bisa mempengaruhi data yang kita ukur? 
#Misalkan, kita memakai contoh data R bernama ToothGrowth. Data ini dipakai untuk 
#melihat efek vitamin C pada pertumbuhan gigi hamster. Vitamin C yang dipakai terdiri 
#atas tiga dosis berbeda (0.5, 1, dan 2 mg/hari) dan memakai dua metode berbeda, yakni 
#dengan diberikan jus jeruk (C) atau langsung dalam bentuk asam ascorbic (VC).

my_data<- ToothGrowth
head(my_data, 6)

#Two way anova dengan interaksi
res.aov3<-aov(len ~ supp + dose + supp:dose, data=my_data)
summary(res.aov3)

#Two way anova tanpa interaksi
res.aov2<-aov(len ~ supp + dose, data=my_data)
summary(res.aov2)

#Pairwise t-test: untuk melihat perbandingan 'berpasangan' antar level group, 
#tapi dengan koreksi adanya multiple testing.

pairwise.t.test(my_data$len, my_data$dose,
                p.adjust.method = "BH")
#Visualisasi

ggplot(my_data, aes(x=as.factor(dose), y=len, color=supp))+
  geom_boxplot()+
  ylab("Panjang gigi (mm)")+
  xlab("Dosis vitamin")+
  theme_bw()+
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=14),
        plot.title=element_text(size=14),
        axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = 20, r = 20, b = 0, l = 0)))+
  guides(color=guide_legend(title="Tipe vitamin"))+
  scale_color_brewer(palette="Set1")

## Non-parametrik?

#t-test -> wilcoxon signed rank test

supA <- c(389, 612, 733, 218, 634, 646, 484, 488, 485)
supB <- c(678, 600, 634, 760, 894, 733, 673, 613, 624) 

# Siapkan dataframe
my_data <- data.frame( 
  group = rep(c("supermarket A", "supermarket B"), each = 9),
  weight = c(supA,  supB)
)
## Non-parametrik: t-test vs wilcox test
#parametrik
t.test(weight~group, data=my_data)
## Non-parametrik: t-test vs wilcox test
#non-parametrik
wilcox.test(weight~group, data=my_data)


## Non-parametrik: anova vs kruskal wallis test
#parametrik
my_data<-PlantGrowth
res.aov<-aov(weight~group, data=my_data)
summary(res.aov)

## Non-parametrik: anova vs kruskal wallis test
#non-parametrik: Kruskal Wallis test
kruskal.test(weight~group, data=my_data)
