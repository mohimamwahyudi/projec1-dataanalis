library(tidyverse)
library(lubridate)  
library(ggplot2)  
getwd()
library(dplyr)

#read csv
library(readr)
q2_2019 <- read_csv("dataset2/Divvy_Trips_2019_Q2.csv")
q3_2019 <- read_csv("dataset2/Divvy_Trips_2019_Q3.csv")
q4_2019 <- read_csv("dataset2/Divvy_Trips_2019_Q4.csv")
q1_2020 <- read_csv("dataset2/Divvy_Trips_2020_Q1.csv")

#mencocokan colom 
colnames(q3_2019)
colnames(q4_2019)
colnames(q2_2019)
colnames(q1_2020)

#menyamanakan kolom

(q4_2019 <- rename(q4_2019
                   ,ride_id = trip_id
                   ,rideable_type = bikeid 
                   ,started_at = start_time  
                   ,ended_at = end_time  
                   ,start_station_name = from_station_name 
                   ,start_station_id = from_station_id 
                   ,end_station_name = to_station_name 
                   ,end_station_id = to_station_id 
                   ,member_casual = usertype))
(q3_2019 <- rename(q3_2019
                   ,ride_id = trip_id
                   ,rideable_type = bikeid 
                   ,started_at = start_time  
                   ,ended_at = end_time  
                   ,start_station_name = from_station_name 
                   ,start_station_id = from_station_id 
                   ,end_station_name = to_station_name 
                   ,end_station_id = to_station_id 
                   ,member_casual = usertype))

(q2_2019 <- rename(q2_2019
                   ,ride_id = "01 - Rental Details Rental ID"
                   ,rideable_type = "01 - Rental Details Bike ID" 
                   ,started_at = "01 - Rental Details Local Start Time"  
                   ,ended_at = "01 - Rental Details Local End Time"  
                   ,start_station_name = "03 - Rental Start Station Name" 
                   ,start_station_id = "03 - Rental Start Station ID"
                   ,end_station_name = "02 - Rental End Station Name" 
                   ,end_station_id = "02 - Rental End Station ID"
                   ,member_casual = "User Type"))

#view colom
str(q1_2020)
str(q2_2019)
str(q3_2019)
str(q4_2019)
# Ubah ride_id dan rideable_type menjadi karakter sehingga dapat ditumpuk dengan benar
q4_2019 <-  mutate(q4_2019, ride_id = as.character(ride_id)
                   ,rideable_type = as.character(rideable_type)) 
q3_2019 <-  mutate(q3_2019, ride_id = as.character(ride_id)
                   ,rideable_type = as.character(rideable_type)) 
q2_2019 <-  mutate(q2_2019, ride_id = as.character(ride_id)
                   ,rideable_type = as.character(rideable_type)) 
#gabungkan semua dataset
all_trips <- bind_rows(q2_2019, q3_2019, q4_2019, q1_2020)
#Hapus bidang lintang, panjang, tahun lahir, dan jenis kelamin karena data ini dihapus mulai tahun 2020
all_trips <- all_trips %>%  
  select(-c(start_lat, start_lng, end_lat, end_lng, birthyear, gender, "01 - Rental Details Duration In Seconds Uncapped", "05 - Member Details Member Birthday Year", "Member Gender", "tripduration"))
table(all_trips$member_casual)
all_trips <-  all_trips %>% 
  mutate(member_casual = recode(member_casual
                                ,"Subscriber" = "member"
                                ,"Customer" = "casual"))
table(all_trips$member_casual)
#format tanggal
all_trips$date <- as.Date(all_trips$started_at) #The default format is yyyy-mm-dd
all_trips$month <- format(as.Date(all_trips$date), "%m")
all_trips$day <- format(as.Date(all_trips$date), "%d")
all_trips$year <- format(as.Date(all_trips$date), "%Y")
all_trips$day_of_week <- format(as.Date(all_trips$date), "%A")
#menambah kolom panjang perjalanan
all_trips$ride_length <- difftime(all_trips$ended_at,all_trips$started_at)
#menampilkan struktur all_trip
str(all_trips)
#mengubah tipe data ride_lenght agar dapat di hitung
is.factor(all_trips$ride_length)
all_trips$ride_length <- as.numeric(as.character(all_trips$ride_length))
is.numeric(all_trips$ride_length)
# Hapus data "buruk"
# Kerangka data mencakup beberapa ratus entri saat sepeda dikeluarkan dari dok dan diperiksa kualitasnya oleh Divvy atau ride_length negatif
# Kami akan membuat versi baru dari dataframe (v2) karena data sedang dihapus
all_trips_v2 <- all_trips[!(all_trips$start_station_name == "HQ QR" | all_trips$ride_length<0),]
#menganalisis panjang perjalanana kolom ride_lenght
mean(all_trips_v2$ride_length) 
median(all_trips_v2$ride_length)
max(all_trips_v2$ride_length) 
min(all_trips_v2$ride_length) 
summary(all_trips_v2$ride_length)
#perbandingan members dan casual
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = mean)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = median)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = max)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = min)
#waktu perjalanan rata-rata setiap hari untuk members vs casual
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual + all_trips_v2$day_of_week, FUN = mean)

all_trips_v2$day_of_week <- ordered(all_trips_v2$day_of_week, levels=c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual + all_trips_v2$day_of_week, FUN = mean)

# menganalisis data penumpang berdasarkan jenis dan hari kerja
mutate(weekday = wday(started_at , label = TRUE)) %>% group_by(member_casual, weekday) %>% summarise(number_of_rides = n()						
,average_duration = mean(ride_length)) %>% 	arrange(member_casual, weekday)		

#cek
all_trips_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>% 
  arrange(member_casual, weekday)  %>% 
  ggplot(aes(x = weekday, y = number_of_rides, fill = member_casual)) +
  geom_col(position = "dodge")

all_trips_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>% 
  arrange(member_casual, weekday)  %>% 
  ggplot(aes(x = weekday, y = average_duration, fill = member_casual)) +
  geom_col(position = "dodge")
counts <- aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual + all_trips_v2$day_of_week, FUN = mean)
write.csv(counts, file = '~avg_ride_length.csv')

