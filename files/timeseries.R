options(download.file.method="libcurl")
library(lubridate)
library(forecast)
library(tseries)
library(gtrendsR)
setwd("C:/Users/Klejtys/Desktop/r_workspace/time_series")
data = read.csv("macro.csv", header=FALSE)
name_1 = "Макроэкономика"
name_2 = "Макроэкономика прогноз"
macro <- ts(data[,2], freq=365.25/7, start=decimal_date(ymd("2012-11-04")))
fit <- tbats(macro)
plot(fit, main = name_1)
dev.copy(png, paste0(name_1,'.png'))
dev.off()
summary(fit)
fc <- forecast(fit, h=53)
plot(fc, main = name_2)
dev.copy(png,paste0(name_2,'.png'))
dev.off()
plot(fit$errors)
adf.test(fit$errors)
Box.test(fit$errors, lag = 1, type = c( "Ljung-Box"), fitdf = 0)

data = read.csv("micro.csv", header=FALSE)
name_1 = "Микроэкономика"
name_2 = "Микроэкономика прогноз"
micro <- ts(data[,2], freq=365.25/7, start=decimal_date(ymd("2012-11-04")))
fit <- tbats(micro) 
plot(fit, main = name_1)
dev.copy(png, paste0(name_1,'.png'))
dev.off()
summary(fit)
fc <- forecast(fit, h=53)
plot(fc, main = name_2)
dev.copy(png,paste0(name_2,'.png'))
dev.off()
plot(fit$errors)
adf.test(fit$errors)
Box.test(fit$errors, lag = 1, type = c("Ljung-Box"), fitdf = 0)

data = read.csv("econometrics.csv", header=FALSE)
name_1 = "Эконометрика"
name_2 = "Эконометрика прогноз"
econometrics <- ts(data[,2], freq=365.25/7, start=decimal_date(ymd("2012-11-04")))
mean(econometrics)
fit <- tbats(econometrics) 
plot(fit, main = name_1)
dev.copy(png, paste0(name_1,'.png'))
dev.off()
summary(fit)
fc <- forecast(fit, h=53)
plot(fc, main = name_2)
dev.copy(png,paste0(name_2,'.png'))
dev.off()
plot(fit$errors)
adf.test(fit$errors)
Box.test(fit$errors, lag = 1, type = c("Ljung-Box"), fitdf = 0)

data = read.csv("instek.csv", header=FALSE)
name_1 = "Инстэк"
name_2 = "Инстэк прогноз"
instek <- ts(data[,2], freq=365.25/7, start=decimal_date(ymd("2012-11-04")))
mean(instek)
fit <- tbats(instek) 
plot(fit, main = name_1)
dev.copy(png, paste0(name_1,'.png'))
dev.off()
summary(fit)
fc <- forecast(fit, h=53)
plot(fc, main = name_2)
dev.copy(png,paste0(name_2,'.png'))
dev.off()
plot(fit$errors)
adf.test(fit$errors)
Box.test(fit$errors, lag = 1, type = c("Ljung-Box"), fitdf = 0)

data = read.csv("altai.csv", header=FALSE)
name_1 = "Алтайский государственный университет"
name_2 = "Алтайский государственный университет прогноз"
altai <- ts(data[,2], freq=365.25/7, start=decimal_date(ymd("2012-11-04")))
mean(altai)
fit <- tbats(altai)
plot(fit, main = name_1)
dev.copy(png, paste0(name_1,'.png'))
dev.off()
summary(fit)
fc <- forecast(fit, h=53)
plot(fc, main = name_2)
dev.copy(png,paste0(name_2,'.png'))
dev.off()
plot(fit$errors)
adf.test(fit$errors)
Box.test(fit$errors, lag = 1, type = c("Ljung-Box"), fitdf = 0)

data = read.csv("belgorod.csv", header=FALSE)
name_1 = "Белгородский государственный университет"
name_2 = "Белгородский государственный университет прогноз"
belgorod <- ts(data[,2], freq=365.25/7, start=decimal_date(ymd("2012-11-04")))
mean(belgorod)
fit <- tbats(belgorod) 
plot(fit, main = name_1)
dev.copy(png, paste0(name_1,'.png'))
dev.off()
summary(fit)
fc <- forecast(fit, h=53)
plot(fc, main = name_2)
dev.copy(png,paste0(name_2,'.png'))
dev.off()
plot(fit$errors)
adf.test(fit$errors)
Box.test(fit$errors, lag = 1, type = c("Ljung-Box"), fitdf = 0)

data = read.csv("ranepa.csv", header=FALSE)
name_1 = "РАНХиГС"
name_2 = "РАНХиГС прогноз"
ranepa <- ts(data[,2], freq=365.25/7, start=decimal_date(ymd("2012-11-04")))
mean(ranepa)
fit <- tbats(ranepa) 
plot(fit, main = name_1)
dev.copy(png, paste0(name_1,'.png'))
dev.off()
summary(fit)
fc <- forecast(fit, h=53)
plot(fc, main = name_2)
dev.copy(png,paste0(name_2,'.png'))
dev.off()
plot(fit$errors)
adf.test(fit$errors)
Box.test(fit$errors, lag = 1, type = c("Ljung-Box"), fitdf = 0)

data = read.csv("voronezh.csv", header=FALSE)
name_1 = "Воронежский государственный университет"
name_2 = "Воронежский государственный университет прогноз"
voronezh <- ts(data[,2], freq=365.25/7, start=decimal_date(ymd("2012-11-04")))
fit <- tbats(voronezh) 
plot(fit, main = name_1)
dev.copy(png, paste0(name_1,'.png'))
dev.off()
summary(fit)
fc <- forecast(fit, h=53)
plot(fc, main = name_2)
dev.copy(png,paste0(name_2,'.png'))
dev.off()
plot(fit$errors)
adf.test(fit$errors)
Box.test(fit$errors, lag = 1, type = c("Ljung-Box"), fitdf = 0)

data = read.csv("kemerovo.csv", header=FALSE)
name_1 = "Кемеровский государственный университет"
name_2 = "Кемеровский государственный университет прогноз"
kemer <- ts(data[,2], freq=365.25/7, start=decimal_date(ymd("2012-11-04")))
fit <- tbats(kemer) 
plot(fit, main = name_1)
dev.copy(png, paste0(name_1,'.png'))
dev.off()
summary(fit)
fc <- forecast(fit, h=53)
plot(fc, main = name_2)
dev.copy(png,paste0(name_2,'.png'))
dev.off()
plot(fit$errors)
adf.test(fit$errors)
Box.test(fit$errors, lag = 1, type = c( "Ljung-Box"), fitdf = 0)


data = read.csv("yale.csv", header=FALSE)
name_1 = "Yale university"
name_2 = "Yale university прогноз"
yale <- ts(data[,2], freq=365.25/7, start=decimal_date(ymd("2012-11-04")))
fit <- tbats(yale) 
plot(fit, main = name_1)
dev.copy(png, paste0(name_1,'.png'))
dev.off()
summary(fit)
fc <- forecast(fit, h=53)
plot(fc, main = name_2)
dev.copy(png,paste0(name_2,'.png'))
dev.off()
plot(fit$errors) 
adf.test(fit$errors) 
Box.test(fit$errors, lag = 1, type = c("Ljung-Box"), fitdf = 0)

data = read.csv("caltech.csv", header=FALSE)
name_1 = "Caltech"
name_2 = "Caltech прогноз"
caltech <- ts(data[,2], freq=365.25/7, start=decimal_date(ymd("2012-11-04")))
fit <- tbats(caltech) 
plot(fit, main = name_1)
dev.copy(png, paste0(name_1,'.png'))
dev.off()
summary(fit)
fit$alpha
fc <- forecast(fit, h=53)
plot(fc, main = name_2)
dev.copy(png,paste0(name_2,'.png'))
dev.off()
plot(fit$errors)
adf.test(fit$errors)
Box.test(fit$errors, lag = 1, type = c("Ljung-Box"), fitdf = 0)

data = read.csv("princeton.csv", header=FALSE)
name_1 = "University of Princeton"
name_2 = "University of Princeton прогноз"
prince <- ts(data[,2], freq=365.25/7, start=decimal_date(ymd("2012-11-04")))
fit <- tbats(prince) 
plot(fit, main = name_1)
dev.copy(png, paste0(name_1,'.png'))
dev.off()
summary(fit)
fit$alpha
fc <- forecast(fit, h=53)
plot(fc, main = name_2)
dev.copy(png,paste0(name_2,'.png'))
dev.off()
plot(fit$errors)
adf.test(fit$errors)
Box.test(fit$errors, lag = 1, type = c("Ljung-Box"), fitdf = 0)

data = read.csv("jhu.csv", header=FALSE)
name_1 = "John Hopkins university"
name_2 = "John Hopkins university прогноз"
jhu <- ts(data[,2], freq=365.25/7, start=decimal_date(ymd("2012-11-04")))
mean(jhu)
fit <- tbats(jhu) 
plot(fit, main = name_1)
dev.copy(png, paste0(name_1,'.png'))
dev.off()
summary(fit)
tbats.components(fit)
fc <- forecast(fit, h=53)
plot(fc, main = name_2)
dev.copy(png,paste0(name_2,'.png'))
dev.off()
plot(fit$errors)
adf.test(fit$errors)
Box.test(fit$errors, lag = 1, type = c("Ljung-Box"), fitdf = 0)

data = read.csv("nusingapore.csv", header=FALSE)
name_1 = "National University of Singapore"
name_2 = "National University of Singapore прогноз"
nsu <- ts(data[,2], freq=365.25/7, start=decimal_date(ymd("2012-11-04")))
fit <- tbats(nsu) 
plot(fit, main = name_1)
dev.copy(png, paste0(name_1,'.png'))
dev.off()
summary(fit)
fc <- forecast(fit, h=53)
plot(fc, main = name_2)
dev.copy(png,paste0(name_2,'.png'))
dev.off()
plot(fit$errors)
adf.test(fit$errors)
Box.test(fit$errors, lag = 1, type = c("Ljung-Box"), fitdf = 0)


data = read.csv("mit.csv", header=FALSE)
name_1 = "MIT"
name_2 = "MIT прогноз"
mit <- ts(data[,2], freq=365.25/7, start=decimal_date(ymd("2012-11-04")))
fit <- tbats(mit) 
plot(fit, main = name_1)
dev.copy(png, paste0(name_1,'.png'))
dev.off()
summary(fit)
fc <- forecast(fit, h=53)
plot(fc, main = name_2)
dev.copy(png,paste0(name_2,'.png'))
dev.off()
plot(fit$errors)
adf.test(fit$errors)
Box.test(fit$errors, lag = 1, type = c("Ljung-Box"), fitdf = 0)


data = read.csv("harvard.csv", header=FALSE)
name_1 = "University of Harvard"
name_2 = "University of Harvard прогноз"
harvard <- ts(data[,2], freq=365.25/7, start=decimal_date(ymd("2012-11-04")))
fit <- tbats(harvard) 
plot(fit, main = name_1)
dev.copy(png, paste0(name_1,'.png'))
dev.off()
summary(fit)
fc <- forecast(fit, h=53)
plot(fc, main = name_2)
dev.copy(png,paste0(name_2,'.png'))
dev.off()
plot(fit$errors)
adf.test(fit$errors)
Box.test(fit$errors, lag = 1, type = c("Ljung-Box"), fitdf = 0)

data = read.csv("oxford.csv", header=FALSE)
name_1 = "University of Oxford"
name_2 = "University of Oxford прогноз"
oxford <- ts(data[,2], freq=365.25/7, start=decimal_date(ymd("2012-11-04")))
fit <- tbats(oxford) 
plot(fit, main = name_1)
dev.copy(png, paste0(name_1,'.png'))
dev.off()
summary(fit)
fc <- forecast(fit, h=53)
plot(fc, main = name_2)
dev.copy(png,paste0(name_2,'.png'))
dev.off()
plot(fit$errors)
adf.test(fit$errors)
Box.test(fit$errors, lag = 1, type = c("Ljung-Box"), fitdf = 0)

data = read.csv("ucl.csv", header=FALSE)
name_1 = "University College London"
name_2 = "University College London прогноз"
ucl <- ts(data[,2], freq=365.25/7, start=decimal_date(ymd("2012-11-04")))
fit <- tbats(ucl) 
plot(fit, main = name_1)
dev.copy(png, paste0(name_1,'.png'))
dev.off()
summary(fit)
fc <- forecast(fit, h=53)
plot(fc, main = name_2)
dev.copy(png,paste0(name_2,'.png'))
dev.off()
plot(fit$errors)
adf.test(fit$errors)
Box.test(fit$errors, lag = 1, type = c("Ljung-Box"), fitdf = 0)


data = read.csv("chicago.csv", header=FALSE)
name_1 = "University of Chicago"
name_2 = "University of Chicago прогноз"
chicago <- ts(data[,2], freq=365.25/7, start=decimal_date(ymd("2012-11-04")))
fit <- tbats(chicago) 
plot(fit, main = name_1)
dev.copy(png, paste0(name_1,'.png'))
dev.off()
summary(fit)
fc <- forecast(fit, h=53)
plot(fc, main = name_2)
dev.copy(png,paste0(name_2,'.png'))
dev.off()
plot(fit$errors)
adf.test(fit$errors)
Box.test(fit$errors, lag = 1, type = c("Ljung-Box"), fitdf = 0)


data = read.csv("ntu.csv", header=FALSE)
name_1 = "Nanyang Technological University"
name_2 = "Nanyang Technological University прогноз"
ntu <- ts(data[,2], freq=365.25/7, start=decimal_date(ymd("2012-11-04")))
fit <- tbats(ntu) 
plot(fit, main = name_1)
dev.copy(png, paste0(name_1,'.png'))
dev.off()
summary(fit)
fit$parameters
fc <- forecast(fit, h=53)
plot(fc, main = name_2)
dev.copy(png,paste0(name_2,'.png'))
dev.off()
plot(fit$errors)
auto.arima(fit$errors)
adf.test(fit$errors)
Box.test(fit$errors, lag = 2, type = c("Ljung-Box"), fitdf = 0)

data = read.csv("cornell.csv", header=FALSE)
name_1 = "Cornell University"
name_2 = "Cornell University прогноз"
cornell <- ts(data[,2], freq=365.25/7, start=decimal_date(ymd("2012-11-04")))
fit <- tbats(cornell) 
plot(fit, main = name_1)
dev.copy(png, paste0(name_1,'.png'))
dev.off()
summary(fit)
fc <- forecast(fit, h=53)
plot(fc, main = name_2)
dev.copy(png,paste0(name_2,'.png'))
dev.off()
plot(fit$errors)
adf.test(fit$errors)
Box.test(fit$errors, lag = 1, type = c("Ljung-Box"), fitdf = 0)

data = read.csv("pennsylvania.csv", header=FALSE)
name_1 = "University of Pennsylvania"
name_2 = "University of Pennsylvania прогноз"
pennsylvania <- ts(data[,2], freq=365.25/7, start=decimal_date(ymd("2012-11-04")))
fit <- tbats(pennsylvania) 
plot(fit, main = name_1)
dev.copy(png, paste0(name_1,'.png'))
dev.off()
summary(fit)
fc <- forecast(fit, h=53)
plot(fc, main = name_2)
dev.copy(png,paste0(name_2,'.png'))
dev.off()
plot(fit$errors)
adf.test(fit$errors)
Box.test(fit$errors, lag = 1, type = c("Ljung-Box"), fitdf = 0)

data = read.csv("columbia.csv", header=FALSE)
name_1 = "Columbia University"
name_2 = "Columbia University прогноз"
columbia <- ts(data[,2], freq=365.25/7, start=decimal_date(ymd("2012-11-04")))
fit <- tbats(columbia) 
plot(fit, main = name_1)
dev.copy(png, paste0(name_1,'.png'))
dev.off()
summary(fit)
fc <- forecast(fit, h=53)
plot(fc, main = name_2)
dev.copy(png,paste0(name_2,'.png'))
dev.off()
plot(fit$errors)
adf.test(fit$errors)
Box.test(fit$errors, lag = 2, type = c("Ljung-Box"), fitdf = 0)

data = read.csv("edinburgh.csv", header=FALSE)
name_1 = "University of Edinburgh"
name_2 = "University of Edinburgh прогноз"
edinb <- ts(data[,2], freq=365.25/7, start=decimal_date(ymd("2012-11-04")))
fit <- tbats(edinb, use.trend = FALSE) 
plot(fit, main = name_1)
dev.copy(png, paste0(name_1,'.png'))
dev.off()
summary(fit)
fc <- forecast(fit, h=53)
plot(fc, main = name_2)
dev.copy(png,paste0(name_2,'.png'))
dev.off()
plot(fit$errors)
adf.test(fit$errors)
Box.test(fit$errors, lag = 1, type = c("Ljung-Box"), fitdf = 0)

data = read.csv("auNU.csv", header=FALSE)
name_1 = "The Australian National University"
name_2 = "The Australian National University прогноз"
auNU <- ts(data[,2], freq=365.25/7, start=decimal_date(ymd("2012-11-04")))
fit <- tbats(auNU) 
plot(fit, main = name_1)
dev.copy(png, paste0(name_1,'.png'))
dev.off()
summary(fit)
fc <- forecast(fit, h=53)
plot(fc, main = name_2)
dev.copy(png,paste0(name_2,'.png'))
dev.off()
plot(fit$errors)
adf.test(fit$errors)
Box.test(fit$errors, lag = 1, type = c("Ljung-Box"), fitdf = 0)

data = read.csv("michigan.csv", header=FALSE)
name_1 = "University of Michigan"
name_2 = "University of Michigan прогноз"
michigan <- ts(data[,2], freq=365.25/7, start=decimal_date(ymd("2012-11-04")))
fit <- tbats(michigan) 
plot(fit, main = name_1)
dev.copy(png, paste0(name_1,'.png'))
dev.off()
summary(fit)
fit$parameters
fc <- forecast(fit, h=53)
plot(fc, main = name_2)
dev.copy(png,paste0(name_2,'.png'))
dev.off()
plot(fit$errors)
adf.test(fit$errors)
Box.test(fit$errors, lag = 1, type = c("Ljung-Box"), fitdf = 0)

data = read.csv("duke.csv", header=FALSE)
name_1 = "Duke University"
name_2 = "Duke University прогноз"
duke <- ts(data[,2], freq=365.25/7, start=decimal_date(ymd("2012-11-04")))
fit <- tbats(duke) 
plot(fit, main = name_1)
dev.copy(png, paste0(name_1,'.png'))
dev.off()
summary(fit)
fc <- forecast(fit, h=53)
plot(fc, main = name_2)
dev.copy(png,paste0(name_2,'.png'))
dev.off()
plot(fit$errors)
adf.test(fit$errors)
Box.test(fit$errors, lag = 1, type = c("Ljung-Box"), fitdf = 0)

data = read.csv("northwestern.csv", header=FALSE)
name_1 = "Northwestern University"
name_2 = "Northwestern University прогноз"
northwestern <- ts(data[,2], freq=365.25/7, start=decimal_date(ymd("2012-11-04")))
fit <- tbats(northwestern) 
plot(fit, main = name_1)
dev.copy(png, paste0(name_1,'.png'))
dev.off()
summary(fit)
fc <- forecast(fit, h=53)
plot(fc, main = name_2)
dev.copy(png,paste0(name_2,'.png'))
dev.off()
plot(fit$errors)
adf.test(fit$errors)
Box.test(fit$errors, lag = 1, type = c("Ljung-Box"), fitdf = 0)

data = read.csv("HK.csv", header=FALSE)
name_1 = "The University of Hong Kong"
name_2 = "The University of Hong Kong прогноз"
HK <- ts(data[,2], freq=365.25/7, start=decimal_date(ymd("2012-11-04")))
fit <- tbats(HK) 
plot(fit, main = name_1)
dev.copy(png, paste0(name_1,'.png'))
dev.off()
summary(fit)
fc <- forecast(fit, h=53)
plot(fc, main = name_2)
dev.copy(png,paste0(name_2,'.png'))
dev.off()
plot(fit$errors)
adf.test(fit$errors)
Box.test(fit$errors, lag = 1, type = c("Ljung-Box"), fitdf = 0)

data = read.csv("berkeley.csv", header=FALSE)
name_1 = "University of California, Berkeley"
name_2 = "University of California, Berkeley прогноз"
berceley <- ts(data[,2], freq=365.25/7, start=decimal_date(ymd("2012-11-04")))
fit <- tbats(berceley) 
plot(fit, main = name_1)
dev.copy(png, paste0(name_1,'.png'))
dev.off()
summary(fit)
fc <- forecast(fit, h=53)
plot(fc, main = name_2)
dev.copy(png,paste0(name_2,'.png'))
dev.off()
plot(fit$errors)
adf.test(fit$errors)
Box.test(fit$errors, lag = 1, type = c("Ljung-Box"), fitdf = 0)

data = read.csv("mcgill.csv", header=FALSE)
name_1 = "McGill University"
name_2 = "McGill University прогноз"
mcgill <- ts(data[,2], freq=365.25/7, start=decimal_date(ymd("2012-11-04")))
fit <- tbats(mcgill) 
plot(fit, main = name_1)
dev.copy(png, paste0(name_1,'.png'))
dev.off()
summary(fit)
fc <- forecast(fit, h=53)
plot(fc, main = name_2)
dev.copy(png,paste0(name_2,'.png'))
dev.off()
plot(fit$errors)
adf.test(fit$errors)
Box.test(fit$errors, lag = 1, type = c("Ljung-Box"), fitdf = 0)


univer <- c("Caltech",
            "University College London",
            "University of Chicago",
            "University of Princeton",
            "National University of Singapore",
            "Nanyang Technological University",
            "Yale University",
            "Cornell University",
            "Johns Hopkins University",
            "University of Pennsylvania",
            "Columbia University", 
            "University of Edinburgh",
            "Australian National University",
            "University of Michigan",
            "Duke University",
            "Northwestern University",
            "University of Hong Kong",
            "University of California, Berkeley",
            "McGill University")

univer <- c("Иркутский государственный университет",
            "Казанский федеральный университет",
            "МГИМО",
            "МФТИ",
            "Московский гуманитарный университет",
            "Мордовский государственный университет",
            "Новосибирский государственный университет",
            "Пензенский государственный университет",
            "Плешка",
            "РУДН",
            "Саратовский государственный университет",
            "Томский государственный университет", 
           "Томский политехнический университет",
            "Южный федеральный университет", 
            "ВАВТ",
            "ВШЭ",
            "МГУ")


m <- matrix(0, ncol = length(univer), nrow = 4)
m <- data.frame(t(m))
names(m)<- c("base_univer","univer","mean_1","mean_2")

univer <- c("Воронежский государственный университет",
            "Белгородский государственный университет",
            "Кемеровский государственный университет",
            "Алтайский государственный университет")

 for (j in 1:1){
   for (i in 3:3){
 x <- gtrends(c("РАНХиГС",univer[i]), time = "today+5-y")
 part1 <- x$interest_over_time$hits[1:261]
 part2 <- x$interest_over_time$hits[262:522]
 m$base_univer[i] <- "РАНХиГС"
 m$univer[i] <- univer[i]
 m$mean_1[i] <- mean(part1)
 m$mean_2[i] <- mean(part2)
 print(mean(part1))
 print(mean(part2))
 print(i)
 print(j)
 Sys.sleep(10)
   }
 }


  for (i in 1:17){
    x <- gtrends(univer[i], time = "today+5-y")
    part1 <- x$interest_over_time$hits
    lel <- ts(part1, freq=365.25/7, start=decimal_date(ymd("2012-11-11")))
    fit <- tbats(lel) 
    plot(fit, main = univer[i])
    dev.copy(png, paste0(univer[i],'.png'))
    dev.off()
    fc <- forecast(fit, h=53)
    plot(fc, main = paste0(univer[i]," прогноз"))
    dev.copy(png, paste0(paste0(univer[i]," прогноз"),'.png'))
    dev.off()
    print(univer[i])
    print(Box.test(fit$errors, lag = 1, type = c("Ljung-Box"), fitdf = 0))
    y = data.frame(x$interest_over_time$date, x$interest_over_time$hits)
    write.csv(y, file = paste0(univer[i],'.csv') , row.names=FALSE)
  }


m$ratio <- m$mean_2/m$mean_1
write.csv(m,file="отношения_предпочтений_РАНХиГС.csv")

x <- gtrends(c("caltech","MIT"), time = "today+5-y")
part1 <- x$interest_over_time$hits[1:260]
part2 <- x$interest_over_time$hits[261:520]
mean(part1)
mean(part2)
mean(part2)/mean(part1)

x <- gtrends(c("caltech","Harvard"), time = "today+5-y")
part1 <- x$interest_over_time$hits[1:260]
part2 <- x$interest_over_time$hits[261:520]
mean(part1)
mean(part2)
mean(part2)/mean(part1)

x <- gtrends(c("caltech","Oxford"), time = "today+5-y")
part1 <- x$interest_over_time$hits[1:260]
part2 <- x$interest_over_time$hits[261:520]
mean(part1)
mean(part2)
mean(part2)/mean(part1)

x <- gtrends(c("РАНХиГС","Кемеровский государственный университет"), time = "today+5-y")
 part1 <- x$interest_over_time$hits[1:260]
part2 <- x$interest_over_time$hits[261:520]
mean(part1)
mean(part2)
mean(part2)/mean(part1)



data = read.csv("micro_en.csv", header=FALSE)
name_1 = "Microeconomics"
name_2 = "Microeconomics прогноз"
micro_en <- ts(data[,2], freq=365.25/7, start=decimal_date(ymd("2012-11-11")))
fit <- tbats(micro_en) 
plot(fit, main = name_1)
dev.copy(png, paste0(name_1,'.png'))
dev.off()
summary(fit)
fc <- forecast(fit, h=53)
plot(fc, main = name_2)
dev.copy(png,paste0(name_2,'.png'))
dev.off()
plot(fit$errors)
adf.test(fit$errors)
Box.test(fit$errors, lag = 1, type = c("Ljung-Box"), fitdf = 0)


data = read.csv("macro_en.csv", header=FALSE)
name_1 = "Macroeconomics"
name_2 = "Macroeconomics прогноз"
macro_en <- ts(data[,2], freq=365.25/7, start=decimal_date(ymd("2012-11-11")))
fit <- tbats(macro_en) 
plot(fit, main = name_1)
dev.copy(png, paste0(name_1,'.png'))
dev.off()
summary(fit)
fc <- forecast(fit, h=53)
plot(fc, main = name_2)
dev.copy(png,paste0(name_2,'.png'))
dev.off()
plot(fit$errors)
adf.test(fit$errors)
Box.test(fit$errors, lag = 1, type = c("Ljung-Box"), fitdf = 0)


data = read.csv("econometrics_en.csv", header=FALSE)
name_1 = "Econometrics"
name_2 = "Econometrics прогноз"
econometrics_en <- ts(data[,2], freq=365.25/7, start=decimal_date(ymd("2012-11-11")))
fit <- tbats(econometrics_en) 
plot(fit, main = name_1)
dev.copy(png, paste0(name_1,'.png'))
dev.off()
summary(fit)
fit$parameters
fc <- forecast(fit, h=53)
plot(fc, main = name_2)
dev.copy(png,paste0(name_2,'.png'))
dev.off()
plot(fit$errors)
adf.test(fit$errors)
Box.test(fit$errors, lag = 1, type = c("Ljung-Box"), fitdf = 0)




data = read.csv("instek_en.csv", header=FALSE)
name_1 = "Institutional economics"
name_2 = "Institutional economics прогноз"
instek_en <- ts(data[,2], freq=365.25/7, start=decimal_date(ymd("2012-11-11")))
fit <- tbats(instek_en) 
plot(fit, main = name_1)
dev.copy(png, paste0(name_1,'.png'))
dev.off()
summary(fit)
fc <- forecast(fit, h=53)
plot(fc, main = name_2)
dev.copy(png,paste0(name_2,'.png'))
dev.off()
plot(fit$errors)
adf.test(fit$errors)
Box.test(fit$errors, lag = 1, type = c("Ljung-Box"), fitdf = 0)



  x <- gtrends("МФТИ", time = "today+5-y")
  part1 <- x$interest_over_time$hits
  lel <- ts(part1, freq=365.25/7, start=decimal_date(ymd("2012-11-11")))
  fit <- tbats(lel) 
  plot(fit, main = "МФТИ")
  dev.copy(png, paste0("МФТИ",'.png'))
  dev.off()
  fc <- forecast(fit, h=53)
  plot(fc, main = paste0("МФТИ"," прогноз"))
  dev.copy(png, paste0(paste0("МФТИ"," прогноз"),'.png'))
  dev.off()
  print(univer[i])
  print(Box.test(fit$errors, lag = 1, type = c("Ljung-Box"), fitdf = 0))
  y = data.frame(x$interest_over_time$date, x$interest_over_time$hits)
  write.csv(y, file = paste0("МФТИ",'.csv') , row.names=FALSE)

  x <- gtrends("Плеханова", time = "today+5-y")
  part1 <- x$interest_over_time$hits
  lel <- ts(part1, freq=365.25/7, start=decimal_date(ymd("2012-11-11")))
  fit <- tbats(lel,use.trend = FALSE) 
  plot(fit, main = "РЭУ ИМ. Г.В.Плеханова")
  dev.copy(png, paste0("РЭУ ИМ. Г.В.Плеханова",'.png'))
  dev.off()
  fc <- forecast(fit, h=53)
  plot(fc, main = paste0("РЭУ ИМ. Г.В.Плеханова"," прогноз"))
  dev.copy(png, paste0(paste0("РЭУ ИМ. Г.В.Плеханова"," прогноз"),'.png'))
  dev.off()
  print(univer[i])
  print(Box.test(fit$errors, lag = 1, type = c("Ljung-Box"), fitdf = 0))
  y = data.frame(x$interest_over_time$date, x$interest_over_time$hits)
  write.csv(y, file = paste0("РЭУ ИМ. Г.В.Плеханова",'.csv') , row.names=FALSE)
