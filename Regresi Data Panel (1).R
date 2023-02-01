panel=read.csv("E:\\Kriminalitas\\Panel1.3.0.csv",header=TRUE,sep=";")
panel

View(panel)
summary(panel$Pencurian.Pemberatan.)
str(panel)
library(plm)
library(lmtest)
library(foreign)

#uji chow
#ho common
#h1 fixed
gc=plm(Jumlah.Kejahatan~UMP+Jumlah.Pengangguran+Jumlah.Penduduk.Miskin+IPM+PDRB+KDRT+Narkotika+Penggelapan+Penipuan, data=panel,model="pooling")
gf=pvcm(Jumlah.Kejahatan~UMP+Jumlah.Pengangguran+Jumlah.Penduduk.Miskin+IPM+PDRB+KDRT+Narkotika+Penggelapan+Penipuan, data=panel,model="within")
pooltest(gc,gf)
#Tolak H0-->maka model fix

#uji Hausman
#Ho random
#h1 fixed
gf=plm(Jumlah.Kejahatan~UMP+Jumlah.Pengangguran+Jumlah.Penduduk.Miskin+IPM+PDRB+KDRT+Narkotika+Penggelapan+Penipuan,data=panel, model="within")
gr=plm(Jumlah.Kejahatan~UMP+Jumlah.Pengangguran+Jumlah.Penduduk.Miskin+IPM+PDRB+KDRT+Narkotika+Penggelapan+Penipuan,data=panel,model="random")
phtest(gf,gr)
# karena hasil uji hausmann pada pengujian model random error maka diambil model fix


###Tidak perlu uji LM karena telah didapat model fix
#Uji BP
plmtest(g1, type="bp")

#Estimasi Model


g0=plm(Jumlah.Kejahatan~UMP+Jumlah.Pengangguran+Jumlah.Penduduk.Miskin+IPM+PDRB+KDRT+Narkotika+Penggelapan+Penipuan,data=panel, model="random")
summary(g0)
g1=plm(Jumlah.Kejahatan~UMP+Jumlah.Pengangguran+Jumlah.Penduduk.Miskin+IPM+KDRT+Narkotika+Penggelapan+Penipuan,data=panel, model="random")
summary(g1)
g2=plm(Jumlah.Kejahatan~UMP+Jumlah.Pengangguran+Jumlah.Penduduk.Miskin+KDRT+Narkotika+Penggelapan+Penipuan,data=panel, model="random")
summary(g2)
g3=plm(Jumlah.Kejahatan~UMP+Jumlah.Pengangguran+KDRT+Narkotika+Penggelapan+Penipuan,data=panel, model="random")
summary(g3)
g4=plm(Jumlah.Kejahatan~Jumlah.Pengangguran+KDRT+Narkotika+Penggelapan+Penipuan,data=panel, model="random")
summary(g4)

#mencari intersep umum
within_intercept(g1)

#mencari efek individu , waktu dan 2 arah
fixef(g1, effect = "individual")
fixef(g1, effect = "time")
fixef(g1, effect = "twoways")

#mencari pengaruh efek dua arah, individu dan waktu
plmtest(g1, effect="twoways",type="bp") # tidak ada efek 2 arah
plmtest(g1, effect="individual", type="bp")# tidak ada efek individu
plmtest(g1, effect="time", type="bp")#tidak ada efek waktu

#uji homoskedastisitas
#fixed
bptest(g1, data=panel, studentize=FALSE)
#random
coeftest(g1, vcovHC)

#Uji Autokorelasi
dwtest(g1)

#Uji Autokorelasi serial
pbgtest(g1)
#ada autokorelasi
pbgtest(g1, order=1)

##diagnostik
#cross sectional dependend test
pcdtest(g1, test = c("lm"))

#unit root
library(tseries)
adf.test(g1$residuals, k=1)
adf.test(g1$residuals, k=2)
adf.test(g1$residuals, k=3)
adf.test(g1$residuals, k=4)

#normalitas
jarque.bera.test(g1$residuals)
shapiro.test(g1$residuals)
