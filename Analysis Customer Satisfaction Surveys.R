library(haven)
dataset1 <- read_sav("dataset_ .sav")
View(dataset1)
dataset1
str(dataset1)

# 1.Data wrangling and manipulating demography data
library(dplyr)
demography1 <- select(dataset1, kota, usiar, sex, didik, status)
demography1 <- as.data.frame(demography1)

# a. mengubah data menjadi character
demography1
unique(demography1$kota)
demography1$kota <- factor(demography1$kota,levels =c(1,2,3,4,5,6,7,8), labels = c("Jakarta", "Bandung", "Semarang", "Surabaya", "Palembang", "Medan", "Makassar","Banjarmasin"))
unique(demography1$usiar)
demography1$usiar<-factor(demography1$usiar, level = c(1,2,3,4,5,6,7), labels = c("19-25 thn", "26-31 thn", "32-37 thn", "38-43 thn", "44-49 thn", "50-55 thn", "56-61 thn"))
unique(demography1$sex)
demography1$sex<-factor(demography1$sex, level = c(1,2), labels = c("Pria", "Wanita"))
unique(demography1$didik)
demography1$didik<-factor(demography1$didik, level = c(1,2,3,4,5,6), labels = c("SD", "SLTP", "SLTA", "Akademi/Diploma", "Sarjana (S1)", "Pasca Sarjana (S2/S3)"))
unique(demography1$status)
demography1$status<-factor(demography1$status, level = c(1,2,3), labels = c("Belum menikah", "Menikah", "Duda/Janda"))
dim(demography1)
sum(is.na(demography1))

# b. demography data visualization
library(ggplot2)
table(demography1$kota)
kota <- ggplot(demography1, aes(kota))
kota_asal <- kota + geom_bar() + labs(x = "Kota", y = "Jumlah Konsumen" , title = "Konsumen Berdasarkan Lokasi")
kota_asal

table(demography1$sex)
sex <- ggplot(demography1, aes(sex))
sex <- sex + geom_bar() + theme_bw() + 
  labs(x = "Jenis Kelamin", y = "Jumlah Konsumen" , title = "Konsumen Berdasarkan Jenis Kelamin")
sex  

table(demography1$didik)
didik <- ggplot(demography1, aes(didik))
didik <- didik + geom_bar() +coord_flip()+
  labs(x = "Tinglat Pendidikan" , y = "Jumlah Konsumen", title = "Konsumen Berdasarkan Tingkat Pendidikan")
didik

table(demography1$status)
status <- ggplot(demography1, aes(status))
status <- status + geom_bar() + theme_light() + coord_flip() +
  labs(x = "Status" , y = "Jumlah Konsumen", title = "Konsumen Berdasarkan Status")
status

table(demography1$usiar)
usia <-ggplot(demography1, aes(usiar))
usia <- usia + geom_bar() + labs(x = "Rentang Usia" , y = "Jumlah Konsumen", title = "Konsumen Berdasarkan Rentang Usia")
usia


# 2.Uji Validasi dan Reliabilitas pada Kepuasan Konsumen
# a. dataset kepuasan konsumen
puas1 <- select(dataset1, puas_all, puas_lay, puas_kua, puas_pri)
puas1 <- as.data.frame(puas1)
puas1
str(puas1)

# b. mengubah dataset menjadi numeric
puas1$puas_all <- as.numeric(as_factor(puas1$puas_all))
puas1$puas_kua <- as.numeric(as_factor(puas1$puas_kua))
puas1$puas_lay <- as.numeric(as_factor(puas1$puas_lay))
puas1$puas_pri <- as.numeric(as_factor(puas1$puas_pri))
str(puas1)

# c.uji validasi dan reliabilitas
#   uji validasi menggunakan data correlation
cor(puas1)

#   uji reliabilitas
library(psych)
psych::alpha(puas1)


# 3. Tingkat Kepuasan Konsumen Berdasarkan Lokasi
# a. membuat dataset tingkat kepuasan konsumen berdasarkan lokasi
dataset2 <- cbind(demography1, puas1)
dataset2
dataset2$avg_puas <- apply(dataset2[6:9],1, mean)
dataset3 <- select(dataset2, kota, avg_puas)
head(dataset3)

# b. boxplot
puas_kota <- ggplot(dataset3, aes(kota, avg_puas))
puas_kota <- puas_kota + geom_boxplot() + theme_bw() +
  labs(x = "Lokasi" , y = "Rata-Rata Tingkat Kepuasan", title = "Rata-Rata Tingkat Kepuasan Konsumen Berdasakan Lokasi")
puas_kota
  
# c. distribusi rata-rata
dataset3%>%
  group_by(kota)%>%
  summarise(mean_puas = mean(avg_puas))%>%
  arrange(mean_puas)

# d. ANOVA model
dataset3%>%
  aov(avg_puas~kota, data=.)%>%
  summary()

aov_model <- dataset3%>%
  aov(avg_puas~kota, data= . )

# e. TukeyHSD testing
dataset3%>%
  aov(avg_puas~kota, data=.)%>%
  TukeyHSD()
