### PART A: Create an Artificial Dataset (Total 30 Points)

# install.packages('dplyr')
# install.packages('tidyr')
# install.packages('zoo')
# install.packages('lubridate')
# install.packages('ggplot2')

library(dplyr)
library(tidyr)
library(zoo)
library(lubridate)
library(ggplot2)

## Q1 (5 PTS)

# age listesini olustur
age = sample(18:25, size = 10000, replace = TRUE)

# team listesini olustur
team = sample(
  c('Fenerbahce', 'Galatasaray', 'Besiktas', 'Trabzonspor', 'Other'),
  size = 10000,
  replace = TRUE,
  prob = c(.25, .25, .25, .1, .15)
)

# rating listesini olustur
rating = sample(1:5, size = 10000, replace = TRUE)

# income listesini olustur
income = sample(seq(3000, 10000, by = 100), size = 10000, replace = TRUE)

# dataframe olustur
Q1 = data.frame(age, team, rating, income)

## Q2 (10 PTS)

# olasiliklari tanimlayacak fonksiyonu olusturdum
gen_probs = function(probs) {
  return (c(rep(probs[1], 30), rep(probs[2], 10), rep(probs[3], 20), rep(probs[4], 30), rep(probs[5], 10)))
}

# apply fonksiyonuna verecegim sorgu fonksiyonu
grade = function(row) {
  if(row['team'] == 'Fenerbahce' & (row['rating'] == 4 | row['rating'] == 5)) {
    return (sample(1:100, size = 1, prob = gen_probs(c(0, .01, .30, .60, .09))))
  }
  
  else if (row['team'] == 'Fenerbahce' & (row['rating'] == 1 | row['rating'] == 2 | row['rating'] == 3)) {
    return (sample(1:100, size = 1, prob = gen_probs((c(.30, .20, .20, .30, 0)))))
  }
  
  else if (row['team'] != 'Fenerbahce' & (row['rating'] == 4 | row['rating'] == 5)) {
    return (sample(1:100, size = 1, prob = gen_probs(c(.30, .10, .20, .30, .10))))
  }
  
  else if (row['team'] != 'Fenerbahce' & (row['rating'] == 1 | row['rating'] == 2 | row['rating'] == 3)) {
    return (sample(1:100, size = 1, prob = gen_probs(c(.60, .10, .30, 0, 0))))
  }
  
  else {
    return (-1)
  }
}

Q2 = Q1

Q2$grade = apply(Q2, 1, grade)

## Q3 (5 PTS)

# team, rating, grade sutunlarini al
# team ve rating ile grupla
# team ve rating sutunlarini index olarak alma
# gruplanmis verilerin ortalamasini al ve 0 dan sonra 2 basamakli olacak sekilde yuvarla

Q3 = Q2

Q3 = Q3 %>%
  group_by(team, rating) %>%
  summarise(mean_grade = mean(grade)) %>%
  arrange(desc(mean_grade))

## Q4 (10 PTS)

Q4 = Q3
Q4$mean_grade = round(Q4$mean_grade, 2)

# veriyi grafige aktar
Q4 = ggplot(data = Q4, aes(x = team, y = mean_grade, fill = as.factor(rating))) +
  geom_bar(stat = 'identity', position = 'dodge') + # barlari ayarla (gruplara ayir)
  theme(axis.text.x = element_text(angle = 0, hjust = 1)) + 
  ggtitle('Average grade based on Team Name and Rating') + # grafigin basligi
  xlab('Team Name') + # x ekseni basligi
  ylab('Average Grade') + # y ekseni basligi
  scale_y_continuous(breaks = seq(0, 85, by = 5)) + # y ekseni artis orani
  scale_fill_manual(values = c("#2E8B57", "#2E688B", "#8B2E43", "#D8D92F", "#2FD9C6"), name = "Rating Level") + # renkler
  theme(legend.position = "top") + # legend pozisyonu
  geom_text(aes(label = mean_grade), position = position_dodge(width = .8), vjust = -.35, size = 2.75) # bar kalinligi ve barlarin uzerindeki yazilarin buyuklugu

### PART B: Water Quality Data (Total 70 Points)

# veriyi oku ve data degiskenine ata
data = read.csv(file('data.csv', open = 'r', encoding = 'latin1'),
                sep = ';', header = TRUE, na.strings = 'NA', stringsAsFactors = FALSE)

## Q5 (10 PTS)

Q5 = data

# bosluklara NA degerini ver
Q5[Q5 == ''] = NA

Q5$X = na.locf(Q5$X)                               # X sutununundaki NA degerleri karsilasilan son dolu degerle doldur
Q5 = Q5 %>% filter(!is.na(Parameter))              # Parameter sutunundaki NA degerlerinin hepsini sil
colnames(Q5)[colnames(Q5) == 'X'] = 'Station_Name' # X sutununu Station_Name olarak degistir
Q5 = select(Q5, -X.1, -Unit)                       # X.1 ve Unit sutunlarini sil

## Q6 (5 PTS)

Q6 = Q5

Q6$Year = as.integer(Q6$Year)              # Year sutununu tamsayi olarak degistir
Q6$Year <- ymd(paste0(Q6$Year, '-01-01'))  # Year sutununu yil ay gun olarak degistir

## Q7 (5 PTS)

Q7 = Q6

date_range = seq.Date(from = min(Q7$Year), to = max(Q7$Year), by = 'year')

Q7 = complete(Q7, Station_Name, Parameter, Year = date_range)
Q7 = Q7 %>% arrange(Station_Name, Parameter)


## Q8 (5 PTS)

Q8 = Q7

# Station_Code sutununu olustur
Q8$Station_Code = apply(Q8, 1, function(x) {
  return (strsplit(strsplit(x['Station_Name'], ';')[[1]][1], 'Station No: ')[[1]][2])
})

# Station_Name sutununu olustur
Q8$Station_Name = apply(Q8, 1, function(x) {
  return (strsplit(strsplit(x['Station_Name'], ';')[[1]][2], 'Station Name: ')[[1]][2])
})

# sutunlarin siralarini tekrar duzenle (Station_Code sondaydi)
Q8 = Q8 %>% select(Station_Code, Station_Name, Parameter, Year, X1, X2, X3, X4, X5, X6, X7, X8, X9, X10, X11, X12)

## Q9 (5 PTS)

Q9 = Q8

Q9 = pivot_longer(data = Q9,
                  cols = c('X1', 'X2', 'X3', 'X4', 'X5', 'X6', 'X7', 'X8', 'X9', 'X10', 'X11', 'X12'), 
                  names_to = 'Month', values_to = 'Value')

## Q10 (10 PTS)

Q10 = Q9

# Month sutunu verilerinden X karakterini sil ve karakterleri integer formatina cevir
Q10$Month = as.numeric(gsub('X', '', Q9$Month))

# Year sutunundan sadece yili cek ve sutunun elemanlarini bu sekilde degistir
Q10$Year = as.numeric(format(Q10$Year, '%Y'))

# Year ve Month sutunlarinin birlesiminden Year_Month sutununu olustur
Q10$Year_Month = as.Date(paste(Q10$Year, Q10$Month, '01'), format = '%Y %m %d')

## Q11 (5 PTS)

Q11 = Q10

# pivot_wider
Q11 = Q11 %>% pivot_wider(names_from = 'Parameter', values_from = 'Value')

# Degerleri Station_Code sutununa gore sirala
Q11 = Q11 %>% arrange(Station_Code)

# Sutunlari istenilen sekilde sirala
Q11 = select(Q11, 'Station_Code', 'Station_Name', 'Year', 'Month', 'Year_Month',
            'Al','As','B','Ba','BOD5','Ca++','Cd','Cl-','CN-','Co','CO2','COD','Col','Cr','Cu','DO','DO%',
            'E-Coli','EC','F-','F-Coli','F-Strp','Fe','Fenoller','Hg','Hidrokarbonlar','K+','M-Al','M.Oil',
            'Mg++','Mn','Na+','NH4-N','Ni','NO2-N','NO3-N','o-PO4','P-Al','PAH','Pb','pH','pV','Qanlik','Se',
            'SO4=','SS','Surfaktanlar','T','T-Coli','TDS','TH','TKN','TOC','Top.N','Top.P','Tot.Pest.','Turb','Zn')

## Q12 (5 PTS)

Q12 = Q11

Q12 = Q12 %>% 
  group_by(Station_Code, Station_Name) %>% 
  summarize(min_BOD5 = min(BOD5, na.rm = TRUE),
            max_BOD5 = max(BOD5, na.rm = TRUE),
            mean_BOD5 = mean(BOD5, na.rm = TRUE),
            sd_BOD5 = sd(BOD5, na.rm = TRUE))

## Q13 (10 PTS)

Q13 = Q11

# istenilen parametreler
parameters = c("BOD5", "Ca++", "Cl-", "Cu", "DO", "EC", "Fe", "Mg++", "Na+")

# sutun siralarini olusturmak icin herbir parametreye _min, _max, _mean, _sd ekle ve listeye kaydet
# dataframe sutunlari bu liste elemanlarina gore isimlendirilecek ve bu sirada olusturulacak
select_cols = c()
for (param in parameters) {
  select_cols = c(select_cols, paste0(param, "_min"), paste0(param, "_max"), paste0(param, "_mean"), paste0(param, "_sd"))
}

# min, max, mean, sd fonksiyonlarini parametrelere across fonksiyonunu kullanarak uygula
# ve dataframe sutun sirasini yukarida hesaplandigi gibi degistir
Q13 = Q13 %>% 
  group_by(Station_Code, Station_Name) %>% 
  summarise(across(parameters, list(min = min, max = max, mean = mean, sd = sd), na.rm = TRUE)) %>%
  select(select_cols)


## Q14 (10 PTS)


