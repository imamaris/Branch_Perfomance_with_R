#load Data loan disbursement as df_loan
library(dplyr)
library(scales)
#load data from csv
df_loan <- read.csv('https://dqlab-dataset.s3-ap-southeast-1.amazonaws.com/loan_disbursement.csv', stringsAsFactors = F)
dplyr::glimpse(df_loan)

# Memfilter data bulan Mei 2020, dan jumlahkan data per cabang
df_loan_mei <- df_loan %>% 
  filter(tanggal_cair >= '2020-05-01', tanggal_cair <= '2020-05-31') %>% 
  group_by(cabang) %>% 
  summarise(total_amount = sum(amount))
df_loan_mei

#data 5 cabang dengan total amount paling besar


df_loan_mei %>% 
  arrange(desc(total_amount)) %>% 
  mutate(total_amount = comma(total_amount)) %>% 
  head(5)

#data 5 cabang dengan total amount paling kecil
df_loan_mei %>% 
  arrange(total_amount) %>% 
  mutate(total_amount = comma(total_amount)) %>% 
  head(5)

#Menghitung umur cabang (dalam bulan)

df_cabang_umur <- df_loan %>%
  group_by(cabang) %>%
  summarise(pertama_cair = min(tanggal_cair)) %>%
  mutate(umur = as.numeric(as.Date('2020-05-15') - as.Date(pertama_cair)) %/% 30)
df_cabang_umur

df_loan_mei_umur <- df_cabang_umur %>%
  inner_join(df_loan_mei, by = 'cabang')
df_loan_mei_umur

#Plot relasi umur dan performa mei
ggplot(df_loan_mei_umur, aes(x = umur, y = total_amount)) +
  geom_point() +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "Semakin berumur, perfoma cabang akan semakin baik",
       x = "Umur(bulan)",
       y = "Total Amount")

#Mencari cabang yang perfoma rendah untuk setiap umur

df_loan_mei_flag <- df_loan_mei_umur %>% 
  group_by(umur) %>% 
  mutate(Q1 = quantile(total_amount, 0.25),
         Q3 = quantile(total_amount, 0.75),
         IQR = (Q3 - Q1)) %>%
  mutate(flag = ifelse(total_amount < (Q1 - IQR), 'rendah','baik'))

df_loan_mei_flag %>% 
  filter(flag == 'rendah') %>% 
  mutate_if(is.numeric, funs(comma))

#change color scatterplot
library(ggplot2)

ggplot(df_loan_mei_flag, aes(x = umur, y = total_amount)) +
  geom_point(aes(color = flag)) +
  scale_color_manual(breaks = c("baik", "rendah"),
                     values=c("blue", "red")) +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "cabang berpeforma rendah padahal tidak termasuk bottom 5 nasional",
       color = "",
       x = "Umur(Bulan)",
       y = "Total Amount")

#perbandingan performa cabang di umur yang sama
df_loan_mei_flag %>% 
  filter(umur == 3) %>% 
  inner_join(df_loan, by = 'cabang') %>% 
  filter(tanggal_cair >= '2020-05-01', tanggal_cair <= '2020-05-31') %>% 
  group_by(cabang, flag) %>% 
  summarise(jumlah_hari = n_distinct(tanggal_cair),
            agen_aktif = n_distinct(agen),
            total_loan_cair = n_distinct(loan_id),
            avg_amount = mean(amount), 
            total_amount = sum(amount)) %>% 
  arrange(total_amount) %>% 
  mutate_if(is.numeric, funs(comma))

df_loan_mei_flag %>% 
  filter(umur == 3, flag == 'rendah') %>% 
  inner_join(df_loan, by = 'cabang') %>% 
  filter(tanggal_cair >= '2020-05-01', tanggal_cair <= '2020-05-31') %>% 
  group_by(cabang, agen) %>% 
  summarise(jumlah_hari = n_distinct(tanggal_cair),
            total_loan_cair = n_distinct(loan_id),
            avg_amount = mean(amount), 
            total_amount = sum(amount)) %>% 
  arrange(total_amount) %>% 
  mutate_if(is.numeric, funs(comma))


df_loan %>% 
  filter(cabang == 'AH') %>% 
  filter(tanggal_cair >= '2020-05-01', tanggal_cair <='2020-05-31') %>% 
  group_by(cabang, agen) %>% 
  summarise(jumlah_hari = n_distinct(tanggal_cair),
            total_loan_cair = n_distinct(loan_id),
            avg_amount = mean(amount), 
            total_amount = sum(amount)) %>% 
  arrange(total_amount) %>% 
  mutate_if(is.numeric, funs(comma))