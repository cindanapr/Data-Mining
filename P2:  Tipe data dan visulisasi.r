ins <- read.csv("~/insurance.csv")


#buat tau tipe datanya
str(ins)

#mean,media,kuartil,dll
summary(ins$age)

#simpangan baku
sd(ins$age)

#boxplot , ylab=membuat tabal
boxplot(ins$bmi, main="Boxplot of Insurance BMI",ylab="BMI")

#histogram
hist(ins$charges, main="Histogram of Insurance Charges",xlab="Charges")

#scatterplot: untuk melihat pencilan
plot(x=ins$bmi, y=ins$charges, main="Scatterplot of BMIvs Charges", xlab="BMI", ylab="Charges")

#density: melihat persebaran, terseval normal, ke kiri apa ke kanan
plot(density(ins$bmi), main="Density Plot For InsuranceBMI")

#----------------------------------------------------------------
#mencari covarian : ukuran penyebaran, untuk mencari korelasi
cov(ins$age, ins$bmi)
cov(ins[, c("age", "bmi", "children", "charges")])
#koefisien korelasi nilai -1 s.d 1, kalo 0 baru gaada korelasi
cor(ins$age, ins$bmi)
cor(ins[, c("age", "bmi", "children", "charges")])
aggregate(bmi ~ sex, summary, data = ins) #buat ambil datanya

#-----Visualisasi data untuk multiple variables-----------------
#1. Boxplot
boxplot(bmi ~ age, data = ins)

#2. Scatter Plot
# Scatter plot
with(ins, plot(age, bmi, col = sex, pch =as.numeric(sex)))
# Scatter plot with jitter, jitter buat nambah noise
with(ins, plot(jitter(children), jitter(age)))
# Scatter plot with ggplot2
library(ggplot2)
qplot(age, charges, data = ins, facets = sex ~ .)

#3. # Static 3D scatter plotl
library(scatterplot3d)
with(ins, scatterplot3d(age, bmi, charges))
# Interactive 3D scatter plot 
library(rgl)
with(ins, plot3d(age, bmi, charges))

#Pairsplot, untuk melihat keseluruhan scatter plot
pairs(ins[, c("age", "bmi", "children", "charges")])

#Heat Map, map yang bewarna
distMatrix <- as.matrix(dist(ins[,c("age", "bmi","children", "charges")]))
heatmap(distMatrix)

#Level Plot
library(lattice)
levelplot(children ~ bmi * age, ins, cuts = 5,col.regions = grey.colors(6)[6:1])

#Countor, volcano data dari r nya
filled.contour(volcano, color = terrain.colors, asp = 1, plot.axes = contour(volcano, add = T))

#3D surface
persp(volcano, theta = 25, phi = 30, expand = 0.5, col ="lightblue")

#Parallel Coordinate
# Parallel coordinates
library(MASS)
parcoord(ins[, c("age", "bmi", "children", "charges")],col = ins$sex)
# Parallel coordinates with lattice
library(lattice)
parallelplot(~ins[, c("age", "bmi", "children","charges")] | sex, data = ins)

#======================SAVE DALAM GAMBAR ==============

# Save as a PNG file
png(“myPlot.png”)
plot(1:10, log(1:10))
graphics.off()

# Save as a JPEG file
jpeg(“myPlot.jpeg”)
plot(1:10, log(1:10))
graphics.off()
# Save as a PDF fil
epdf(“myPlot.pdf”)
plot(1:10, log(1:10))
graphics.off()
# Save as a Postscript file
postscript(“myPlot.ps”)
plot(1:10, log(1:10))
graphics.off()