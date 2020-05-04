# Абрамкина Татьяна - создайте модель множественной линейной регрессии дневных потоков 
# углекислого газа за осенний период 2013 года по данным измерений методом турбулентной пульсации
library("tidyverse")# целая вселенная
library("readr")# функция read_csv()
library("stringr")# функция str_replace_all
library("dplyr")# функции: filter(),arrange(),select(),mutate(),summarize(),group_by(),sample_n()
library("ggplot2")# графики функций qplot()
# Скачиваем файл онлайн
# Или считываем файл офлайн
# При этом пропускаем первую строку, заменяем все на числовые значения на NA, и игнорируем с символом "["
# eddypro=read_csv("https://www.dropbox.com/s/erhs9hoj4vhr0b/eddypro.csv?dl=1", skip = 1, na = c("","NA","-9999","-9999.0"), comment = c("["))
getwd()
eddypro = read.csv("eddypro.csv", skip = 1, na = c ("","NA","-9999","-9999.0"), comment = c("["))
#
# Блок подготовки данных
#
# Удаляем ненужную пустую первую строку
eddypro = eddypro [-1,]
# Удаляем ненужный пустой столбец "roll"
eddypro = select(eddypro,-(roll))
# Преобразуем в факторы (factor) столбы типа char(символ)
eddypro = eddypro %>% mutate_if(is.character,factor)
#Заменим специальные символы в названии стобцов на допустимые для переменных имена
names(eddypro) = names(eddypro) %>%
  str_replace_all("[!]","_exclam_") %>%
  str_replace_all("[?]", "_quest_") %>% 
  str_replace_all("[*]", "_star_") %>% 
  str_replace_all("[+]", "_plus_") %>%
  str_replace_all("[-]", "_minus_") %>%
  str_replace_all("[@]", "_at_") %>%
  str_replace_all("[$]", "_dollar_") %>%
  str_replace_all("[#]", "_hash_") %>%
  str_replace_all("[/]", "_slash_") %>%
  str_replace_all("[%]", "__pecent_") %>%
  str_replace_all("[&]", "_amp_") %>%
  str_replace_all("[\\^]", "_power_") %>%
  str_replace_all("[()]","_")
#Возвратим столбцы таблицы в виде векторов для проверки
glimpse(eddypro)
#Удалим строки в которых содержится NA, так как они содержат неполные данные и только мешают
eddypro = drop_na(eddypro)
# Отфильтруем по заданию данные только за осенний период. С начала сентября (245 день) по конец ноября(335 день)
eddypro = filter(eddypro,DOY >= 245 & DOY < 335)
# Отфильтруем данные по заданию только за дневное время
eddypro = filter(eddypro, daytime ==TRUE)
# Получим таблицу, состоящую только из чисел. Будем работать с ней.
eddypro_numeric = eddypro[,sapply(eddypro, is.numeric)]
# Получим таблицу, содержащую остальные колонки
eddypro_non_numeric = eddypro[,!sapply(eddypro, is.numeric)]
# Создадим обучающую и тестирующую непересекающиеся выборки 
row_numbers = 1:length(eddypro_numeric$co2_flux)
teach = sample(row_numbers, floor(length(eddypro_numeric$co2_flux)*.7))
test = row_numbers[-teach]
#Обучающая выборка
teaching_tbl = eddypro_numeric[teach,]
#Тестирующая выборка
testing_tbl = eddypro_numeric[test,]
# Создадим модель по обучающей выборке
mod1 = lm(co2_flux~ (.) , data = teaching_tbl)
# Получим информацию о моделе 
summary(mod1)
# Получим коэффициенты модели
coef(mod1)
# Проанализируем переменные по значимости
anova(mod1)
#Выведем графики
plot(mod1)
# Создадим модель 2 
mod2 = lm( co2_flux~ DOY + file_records + Tau + qc_Tau + rand_err_Tau + H + qc_H + rand_err_H + LE + qc_LE + qc_co2_flux 
           + rand_err_co2_flux + h2o_flux +rand_err_h2o_flux + H_strg +co2_v.adv + co2_molar_density + co2_mole_fraction + co2_mixing_ratio
           + h2o_molar_density + h2o_mole_fraction + h2o_mixing_ratio + h2o_time_lag + sonic_temperature + air_temperature + air_pressure 
           + air_density + air_heat_capacity + air_molar_volume + water_vapor_density + e + es + specific_humidity + RH + VPD + Tdew
           + u_unrot + w_rot + max_speed + wind_dir + yaw + pitch + u. + TKE + L + X.z.d..L + bowen_ratio + T. + x_peak + x_offset 
           + x_10. + x_30. + x_50. +x_70. + x_90. + un_Tau + Tau_scf + un_H + H_scf + un_LE + LE_scf +un_co2_flux + un_h2o_flux + v_spikes 
           + w_spikes + mean_value + ts_var + co2_var + w.co2_cov + w.h2o_cov + co2 + h2o.1 + h2o_signal_strength_7200 + flowrate, data = teaching_tbl)
# Получим информацио о моделе и коэффициенты
summary(mod2)
# Получим коэффициенты модели
coef(mod2)
# Проанализируем переменные по значимости
anova(mod2)
# Сравним с предыдущей моделью, не ухудшилась ли она
anova(mod2, mod1)

# Создадим модель 3
mod3 = lm(co2_flux~ DOY + Tau + qc_Tau + rand_err_Tau + H + qc_H + rand_err_H + LE + qc_LE + qc_co2_flux 
          + rand_err_co2_flux + h2o_flux +rand_err_h2o_flux + H_strg +co2_v.adv + co2_molar_density + co2_mole_fraction + co2_mixing_ratio
          + h2o_molar_density + h2o_mole_fraction + h2o_mixing_ratio + h2o_time_lag + sonic_temperature + air_temperature + air_pressure 
          + air_density + air_heat_capacity + air_molar_volume + water_vapor_density + e + es + specific_humidity + RH + VPD + Tdew
          + u_unrot + w_rot + wind_dir + yaw + pitch + u. + TKE + X.z.d..L + bowen_ratio + T. + x_peak + x_offset 
          + x_10. + x_30. + x_50. +x_70. + x_90. + un_Tau + Tau_scf + un_H + H_scf + un_LE + LE_scf +un_co2_flux + un_h2o_flux
          + w_spikes + mean_value + co2_var + w.co2_cov + w.h2o_cov + co2 , data = teaching_tbl )
# Получим информацио о модели и коэффициенты
summary(mod3)
# Получим коэффициенты модели
coef(mod3)
# Проанализируем переменные по значимости
anova(mod3)
# Сравним с предыдущей моделью, не ухудшилась ли она
anova(mod3, mod2)
# Выведем графики
plot(mod3)
# Проведем корреляционный анализ переменных
# Выберем из таблицы только участвующие у линейной модели переменные 
cor_teaching_tbl = select(teaching_tbl, Tau, H, DOY, LE, qc_LE, h2o_flux, 
                          H_strg, h2o_mole_fraction, h2o_mixing_ratio, specific_humidity,
                          Tdew, u_unrot, w_rot, u., X.z.d..L, T., x_peak, x_offset,
                          x_10., x_70., x_90., un_Tau, Tau_scf, un_H, H_scf, un_LE,
                          LE_scf, un_co2_flux, un_h2o_flux, mean_value, co2_var,
                          w.co2_cov, w.h2o_cov, co2)

#Получаем таблицу коэффициентов корреляций
cor_td = cor(cor_teaching_tbl) %>% as.data.frame

#
# Графики по полученной моделе
#
#Проверка модели
#Построение точек по значениями обучающей выборки и наложение предсказанных значений по 3 модели
#В идеале линия должна  пройти через все точки. А так как у нас график сo2_flux от самой себя, то он должен идти под 45градусов
qplot( co2_flux, co2_flux, data = teaching_tbl) + geom_line(aes(y = predict(mod3, teaching_tbl)))
#Теперь сделаем тоже самое на ТЕСТИРУЮЩЕЙ выборке
qplot(co2_flux , co2_flux, data = testing_tbl) + geom_line(aes(y = predict(mod3, testing_tbl)))
#Так как у нас модель зависит от множества переменных, мы можем вывести много графиков зависимостей co2_flux от учитываемых в моделе параметров
#В идеале предсказанная линия должна пройти через все точки, или как можно ближе к ним на ТЕСТИРУЮЩЕЙ выборке
#Примеры
qplot(H, co2_flux, data = testing_tbl) + geom_line(aes(y = predict(mod3, testing_tbl)))
qplot(Tdew, co2_flux, data = testing_tbl) + geom_line(aes(y = predict(mod3, testing_tbl)))
qplot(co2, co2_flux, data = testing_tbl) + geom_line(aes(y = predict(mod3, testing_tbl)))


