#+ echo=F ----------------------------------------------------------------------
library(knitr)
# align the root with the project working directory
opts_knit$set(root.dir='../')  #Don't combine this call with any
#+ echo=F ----------------------------------------------------------------------
rm(list = ls(all.names = TRUE)) # Clear the memory of variables from previous run.
#This is not called by knitr, because it's above the first chunk.
#+ results="hide",echo=F -------------------------------------------------------
cat("\014") # Clear the console
#+ echo=FALSE, results="asis" --------------------------------------------------
cat("Report's native working directory: `", getwd(),"`") # Must be set to Project Directory
#+ echo=F, results="asis" ------------------------------------------------------
cat("\n# 1.Environment")
#+ set_options, echo=F ---------------------------------------------------------
echo_chunks <- TRUE
eval_chunks <- TRUE
cache_chunks <- TRUE
report_render_start_time <- Sys.time()
options(width=100) # number of characters to display in the output (dflt = 80)
Sys.setlocale("LC_CTYPE", "russian")
#+ load-sources ----------------------------------------------------------------
base::source("./scripts/common-functions.R") # project-level
#+ load-packages ---------------------------------------------------------------
library(tidyverse)

#+ declare-globals -------------------------------------------------------------
path_osbb<- "./data-public/raw/minregion-osbb.xlsx"
path_admin <- "./data-public/derived/ua-admin-map-2020.csv"


#+ results="asis", echo=F ------------------------------------------------------
cat("\n# 2.Data ")

#+ load-data, eval=eval_chunks -------------------------------------------------
ds0 <- readxl::read_excel(path_osbb)
ds_admin <- readr::read_csv(path_admin)

#+ create columns for merge name+oblast-----------------------------------------
ds_admin$settlement_name_full <- paste(ds_admin$settlement_type,
                              ds_admin$settlement_name)
ds_admin$oblast_name_full <- case_when(ds_admin$oblast_name=="Автономна Республіка Крим"~ds_admin$oblast_name,
TRUE~paste(ds_admin$oblast_name,"область"))
ds_admin$raion_name_full <- paste(ds_admin$raion_name,
                                  "район")

#+ Correcting mistakes and problems with cities---------------------------------
ds0 <- ds0 %>% mutate(locality = case_when(
  locality == "місто Артемівськ" ~ "місто Бахмут",
  locality == "місто Біла" & region == "Київська область"  ~ "місто Біла Церква",
  locality == "місто Берегово" & region == "Закарпатська область"~ "місто Берегове",
  locality == "місто Володимир-Волинський" & region == "Волинська область"~ "місто Володимир",
  locality == "місто Горішні" ~ "місто Горішні Плавні",
  locality == "місто Гола" & region == "Херсонська область"~ "місто Гола Пристань",
  locality == "місто Великі" & region == "Львівська область"~ "місто Великі Мости",
  locality == "місто Кам'янське" ~ "місто Кам’янське",
  locality == "місто Дніпродзержинськ" ~ "місто Кам’янське",
  locality == "місто Дзержинськ" ~ "місто Торецьк",
  locality == "місто Дніпропетровськ" ~ "місто Дніпро",
  locality == "місто Жовті" & region == "Дніпропетровська область"~ "місто Жовті Води",
  locality == "місто Знам'янка" ~ "місто Знам’янка",
  locality == "місто Іллічівськ" & region == "Одеська область"~ "місто Чорноморськ",
  locality == "місто Кам'янець-Подільський" ~ "місто Кам’янець-Подільський",
  locality == "місто Кіровоград" ~ "місто Кропивницький",
  locality == "місто Красний" & region == "Донецька область"~ "місто Лиман",
  locality == "місто Кривий" & region == "Дніпропетровська область"~ "місто Кривий Ріг",
  locality == "місто Мала" & region == "Кіровоградська область"~ "місто Мала Виска",
  locality == "місто Мукачеве" ~ "місто Мукачево",
  locality == "місто Нова" & region == "Миколаївська область"~ "місто Нова Одеса",
  locality == "місто Нова" & region == "Херсонська область"~ "місто Нова Каховка",
  locality == "місто Новий" & region == "Миколаївська область"~ "місто Новий Буг",
  locality == "місто Новий" & region == "Львівська область"~ "місто Новий Розділ",
  locality == "місто Переяслав-Хмельницький" & region == "Київська область"~ "місто Переяслав",
  locality == "місто Судова" & region == "Львівська область"~ "місто Судова Вишня",
  locality == "місто Сумська" & region == "Сумська область"~ "місто Лебедин",
  locality == "місто Орджонікідзе" ~ "місто Покров",
  locality == "місто Свердловськ" ~ "місто Довжанськ",
  locality == "місто Старий" & region == "Львівська область"~ "місто Старий Самбір",
  locality == "місто Часів" & region == "Донецька область"~ "місто Часів Яр",
  locality == "місто Червонозаводське" & region == "Полтавська область"~ "місто Заводське",
TRUE~locality)
)


ds0$locality <- gsub("'", "’",ds0$locality)

ds0 <- ds0 %>% mutate(region = case_when(
  locality == "місто Харків" & region == "Сумська область"~ "Харківська область",
  TRUE ~region))

#+ Correcting mistakes and problems with villages-------------------------------
ds0 <- ds0 %>% mutate(locality = case_when(
  locality == "село Петропавлівська" & district == "Києво-Святошинський район" ~ "село Петропавлівська Борщагівка",
  locality == "селище міського типу Більмак" & district == "Більмацький район" ~ "село Кам’янка",
  locality == "селище міського типу Велика" & district == "Великомихайлівський район" ~ "селище міського типу Велика Михайлівка",
  locality == "селище міського типу Велика" & district == "Великоолександрівський район" ~ "селище міського типу Велика Олександрівка",
  locality == "селище міського типу Великий" & district == "Великоберезнянський район" ~ "селище міського типу Великий Березний",
  locality == "селище міського типу Стара" & district == "Старовижівський район" ~ "селище міського типу Стара Вижівка",
  locality == "село Софіївська" & district == "Києво-Святошинський район" ~ "село Софіївська Борщагівка",
  locality == "село Мостиська" & district == "Мостиський район" ~ "місто Мостиська",
  locality == "селище міського типу Велика" & district == "Тернопільський район" ~ "селище міського типу Велика Березовиця",
  locality == "селище міського типу Велика" & district == "Великолепетиський район" ~ "селище міського типу Велика Лепетиха",
  locality == "селище міського типу Велика" & district == "Великобагачанський район" ~ "селище міського типу Велика Багачка",
  locality == "селище міського типу Велика" & district == "Великоновосілківський район" ~ "селище міського типу Велика Новосілка",
  locality == "селище міського типу Великий" & district == "Городоцький район" ~ "селище міського типу Великий Любінь",
  locality == "селище міського типу Великий" & district == "Великобурлуцький район" ~ "селище міського типу Великий Бурлук",
  locality == "селище міського типу Великі" & district == "Тернопільський район" ~ "селище міського типу Великі Бірки",
  locality == "село Зимна" & district == "Пустомитівський район" ~ "село Зимна Вода",
  locality == "село Ілліча" & district == "Костянтинівський район" ~ "село Іллінівка",
  locality == "село Верхня" & district == "Карлівський район" ~ "село Верхня Ланна",
  locality == "селище Зелений" & district == "Великоновосілківський район" ~ "селище Зелений Гай",
  locality == "селище Комсомольське" & district == "Первомайський район" ~ "селище Трійчате",
  locality == "селище Мінеральні" & district == "Павлоградський район" ~ "село Карабинівка",
  locality == "селище Голендри" & district == "Калинівський район"~ "село Голендри",
  locality == "селище міського типу Артемівка" & district == "Чутівський район" ~ "селище міського типу Скороходове",
  locality == "селище міського типу Білий" & district == "Вовчанський район" ~ "селище міського типу Білий Колодязь",
  locality == "селище міського типу Володарське" & district == "Нікольський район" ~ "селище міського типу Нікольське",
  locality == "селище міського типу Козача" & district == "Дергачівський район" ~ "селище міського типу Козача Лопань",
  locality == "селище міського типу Комсомольське" & district == "Зміївський район" ~ "селище міського типу Слобожанське",
  locality == "селище міського типу Костянтинівка" & district == "Краснокутський район" ~ "село Костянтинівка",
  locality == "селище міського типу Липова" & district == "Липоводолинський район" ~ "селище міського типу Липова Долина",
  locality == "селище міського типу Нижні" & district == "Нижньосірогозький район" ~ "селище міського типу Нижні Сірогози",
  locality == "селище міського типу Нижній" & district == "Антрацитівський район" ~ "селище міського типу Нижній Нагольчик",
  locality == "селище міського типу Нова" & district == "Нововодолазький район" ~ "селище міського типу Нова Водолага",
  locality == "селище міського типу Нова" & district == "Новоушицький район" ~ "селище міського типу Нова Ушиця",
  locality == "селище міського типу Нова" & district == "Олександрійський район" ~ "селище міського типу Нова Прага",
  locality == "селище міського типу Нова" & district == "Козельщинський район" ~ "селище міського типу Нова Галещина",
  locality == "селище міського типу Нова" & district == "Хорошівський район" ~ "селище міського типу Нова Борова",
  locality == "селище міського типу Новий" & district == "Кам'янка-Бузький район" ~ "селище міського типу Новий Яричів",
  locality == "селище міського типу Нові" & district == "Олевський район" ~ "селище міського типу Нові Білокоровичі",
  locality == "селище міського типу Нові" & district == "Новосанжарський район" ~ "селище міського типу Нові Санжари",
  locality == "селище міського типу Решетилівка" & district == "Решетилівський район" ~ "місто Решетилівка",
  locality == "селище міського типу Станиця" & district == "Станично-Луганський район" ~ "селище міського типу Станиця Луганська",
  locality == "селище міського типу Стара" & district == "Старосинявський район" ~ "селище міського типу Стара Синява",
  locality == "селище міського типу Старий" & district == "Валківський район" ~ "селище міського типу Старий Мерчик",
  locality == "селище міського типу Щорськ" & district == "Криничанський район" ~ "селище міського типу Божедарівка",
  locality == "селище Нижня" & district == "Сорокинський район" ~ "селище Нижня Шевирівка",
  locality == "селище Новий" & district == "Харківський район" ~ "селище Новий Коротич",
  locality == "село ," & district == "Березанський район" ~ "село Коблеве",
  locality == "селище Перше" & district == "Покровський район" ~ "село Звірове",
  locality == "село Баня" & district == "Стрийський район" ~ "село Баня Лисовицька",
  locality == "село Біла" & district == "Рівненський район" ~ "село Біла Криниця",
  locality == "село Білий" & district == "Золочівський район" ~ "село Білий Камінь",
  locality == "село Велика" & district == "Пирятинський район" ~ "село Велика Круча",
  locality == "село Велика" & district == "Рівненський район" ~ "село Велика Омеляна",
  locality == "село Великий" & district == "Косівський район" ~ "село Великий Рожин",
  locality == "село Великий" & district == "Біляївський район" ~ "село Великий Дальник",
  locality == "село Великий" & district == "Луцький район" ~ "село Великий Омеляник",
  locality == "село Великий" & district == "Косівський район" ~ "село Великий Вистороп",
  locality == "село Великий" & district == "Косівський район" ~ "село Великий Вистороп",
  postalCode == "47707" & district == "Тернопільський район" ~ "село Біла",
  postalCode == "47703" & district == "Тернопільський район" ~ "село Великий Глибочок",
  locality == "село Велика" & district == "Васильківський район" ~ "село Велика Солтанівка",
  postalCode == "35302" ~ "село Великий Олексин",
  locality == "село Великий" & district == "Рівненський район" ~ "село Великий Олексин",
  postalCode == "42242" ~ "село Великий Вистороп",
  locality == "село Великий" & district == "Лебединський район" ~ "село Великий Вистороп",
  
  locality == "село Великі" & district == "Тернопільський район" ~ "село Великі Гаї",
  locality == "село Великі" & district == "Кременецький район" ~ "село Великі Млинівці",
  locality == "село Великі" & district == "Олешківський район" ~ "село Великі Копані",
  locality == "село Вища" & district == "Вишгородський район" ~ "село Вища Дубечня",
  locality == "село Володькова" & district == "Носівський район" ~ "село Володькова Дівиця",
  locality == "село Гірка" & district == "Луцький район" ~ "село Гірка Полонка",
  locality == "село Давидів" & district == "Великоолександрівський район" ~ "село Давидів Брід",
  locality == "село Драбове-Барятинське" & district == "Драбівський район" ~ "селище Драбове-Барятинське",
  locality == "село Дубові" & district == "Сахновщинський район" ~ "село Дубові Гряди",
  locality == "село Зазим’є" & district == "Броварський район" ~ "село Зазим’я",
  locality == "село Залізний" & district == "Голопристанський район" ~ "село Залізний Порт",
  locality == "село Інтернаціональне" & district == "Близнюківський район" ~ "село Батюшки",
  locality == "село Козачі" & district == "Олешківський район" ~ "село Козачі Лагері",
  locality == "село М." & district == "Новоайдарський район" ~ "селище міського типу Новоайдар",
  locality == "село Майстрова" & district == "Новоград-Волинський район" ~ "село Майстрова Воля",
  locality == "село Мала" & district == "Білоцерківський район" ~ "село Мала Вільшанка",
  locality == "село Малий" & district == "Рівненський район" ~ "село Малий Шпаків",
  locality == "село Малий" & district == "Лебединський район" ~ "село Малий Вистороп",
  locality == "село Малий" & district == "Смілянський район" ~ "село Малий Бузуків",
  locality == "село Малі" & district == "Дубенський район" ~ "село Малі Сади",
  locality == "село Мирогоща" & postalCode == "35624" ~ "село Мирогоща Перша",
  locality == "село Нижча" & district == "Вишгородський район" ~ "село Нижча Дубечня",
  locality == "село Нова" & district == "Рівненський район" ~ "село Нова Любомирка",
  locality == "село Нова" & district == "Голопристанський район" ~ "село Нова Збур’ївка",
  locality == "село Новий" & district == "Печенізький район" ~ "село Новий Бурлук",
  locality == "село Новий" & district == "Козелецький район" ~ "село Новий Шлях",
  locality == "село Нові" & district == "Вишгородський район" ~ "село Нові Петрівці",
  locality == "село Переізне" & district == "Бахмутський район" ~ "село Званівка",
  locality == "село Петрівське" & district == "Києво-Святошинський район" ~ "село Святопетрівське",
  locality == "село Руська" & district == "Черкаський район" ~ "село Руська Поляна",
  locality == "село Стара" & district == "Голопристанський район" ~ "село Стара Збур’ївка",
  locality == "село Стара" & district == "Липовецький район" ~ "село Стара Прилука",
  locality == "село Старий" & district == "Коломийський район" ~ "село Старий Гвіздець",
  locality == "село Старий" & district == "Косівський район" ~ "село Старий Косів",
  locality == "село Старі" & district == "Вишгородський район" ~ "село Старі Петрівці",
  locality == "село Старі" & district == "Богородчанський район" ~ "село Старі Богородчани",
  locality == "село Центральне" & district == "Снігурівський район" ~ "селище Центральне",
  locality == "село Циблівська" & district == "Переяслав-Хмельницький район" ~ "село Циблі",
  locality == "село Червона" & district == "Черкаський район" ~ "село Червона Слобода",
  locality == "село Червона" & district == "Макарівський район" ~ "село Червона Слобода",
  locality == "село Червона" & district == "Снігурівський район" ~ "село Червона Долина",
  locality == "село Черкаська" & district == "Дергачівський район" ~ "село Черкаська Лозова",
  locality == "село Ясногородська" & district == "Макарівський район" ~ "село Ясногородка",
  locality == "село П’ятигірське" & district == "Балаклійський район" ~ "селище П’ятигірське",
  
  TRUE~locality)
)

ds0 <- ds0 %>% mutate(district = case_when(
  locality == "село Голендри" & district == "Калинівський район"~ "Хмільницький район",
  locality == "селище Трійчате" & district == "Первомайський район"~ "Лозівський район",
  locality == "село Карабинівка" & district == "Павлоградський район" ~ "Павлоградський район",
  locality == "селище міського типу Скороходове" & district == "Чутівський район" ~ "Полтавський район",
  locality == "селище міського типу Нікольське" & district == "Нікольський район" ~ "Маріупольський район",
  
  TRUE ~district))

ds_admin <- ds_admin %>% mutate(settlement_name_full = case_when(
  settlement_name_full == "село Чернівці" ~ "місто Чернівці",
  TRUE~settlement_name_full)
)

ds_admin$settlement_name_full <- gsub("'", "’",ds_admin$settlement_name_full)

#+ merging OSBB with admin names for ATCs---------------------------------------
d1 <- ds0 %>% 
  left_join(
    ds_admin
    ,by = c("locality" = "settlement_name_full",
            "region" = "oblast_name_full")
  )

# Unmerged cases
d1 %>% filter(is.na(raion_name) ) %>% filter(locality!="місто Київ") %>%
  filter(locality!="місто Севастополь") %>%
  filter(region!="Автономна Республіка Крим") %>% View()
d1 %>% filter(is.na(raion_name) ) %>% filter(locality!="місто Київ") %>%
  filter(locality!="місто Севастополь") %>%
  filter(region!="Автономна Республіка Крим") %>% nrow()


#View(d1[duplicated(d1[c("edrpou")]),])
# Duplicated cases 
d1 %>% filter(,duplicated(edrpou)) %>% 
  View()

d1 %>% filter(,duplicated(edrpou)) %>% 
  distinct(locality,.keep_all= TRUE) %>% View()

d1 %>% filter(,duplicated(edrpou)) %>% 
  filter(,district == raion_name_full) %>%
  View()
  

d1 %>% 
  filter(,district == raion_name_full) %>%
  View()




#### Managing duplicated cases ones again using raions additionally

dups <- d1 %>%
  group_by(edrpou) %>%
  filter(dplyr::n_distinct(settlement_code) > 1) %>%
  select(,1:21) %>%
  distinct(edrpou,.keep_all = TRUE)

dups <- dups %>% mutate(district = case_when(
  locality == "селище Мирне" & district == "Слов'янський район"~ "Краматорський район",
  locality == "село Кам’янка" & district == "Більмацький район"~ "Запорізький район",
  locality == "селище міського типу Калинівка" & district == "Васильківський район"~ "Фастівський район",
  locality == "селище міського типу Слобожанське" & district == "Зміївський район"~ "Чугуївський район",
  locality == "село Костянтинівка" & district == "Краснокутський район"~ "Богодухівський район",
  locality == "селище міського типу Олександрівка" & district == "Олександрівський район"~ "Краматорський район",
  locality == "селище міського типу Оленівка" & district == "Волноваський район"~ "Кальміуський район",
  locality == "селище міського типу Оленівка" & district == "Бахмутський район"~ "Горлівський район",
  locality == "селище міського типу Слобожанське" & district == "Зміївський район"~ "Чугуївський район",
  locality == "селище Опитне" & district == "Ясинуватський район"~ "Покровський район",
  locality == "селище Слобідське" & district == "Первомайський район"~ "Лозівський район",
  locality == "село Березівка" & district == "Маловисківський район"~ "Новоукраїнський район",
  locality == "село Берестове" & district == "Дворічанський район"~ "Куп’янський район",
  locality == "село Бобриця" & district == "Києво-Святошинський район"~ "Бучанський район",
  locality == "село Варварівка" & district == "Олевський район"~ "Новоград-Волинський район",
  locality == "село Вербка" & district == "Кам'янець-Подільський район"~ "Кам’янець-Подільський район",
  locality == "село Володимирівка" & district == "Знам'янський район"~ "Кропивницький район",
  locality == "село Городище" & district == "Березнівський район"~ "Рівненський район",
  locality == "село Уютне" & district == "Сакський район"~ "Євпаторійський район",
  locality == "село Тарасівка" & district == "Києво-Святошинський район"~ "Фастівський район",
  locality == "село Старовірівка" & district == "Шевченківський район"~ "Куп’янський район",
  locality == "село Протопопівка" & district == "Дергачівський район"~ "Харківський район",
  locality == "село Крижанівка" & district == "Лиманський район"~ "Одеський район",
  locality == "село Зоря" & district == "Нікольський район"~ "Маріупольський район",
  locality == "село Гусарівка" & district == "Балаклійський район"~ "Ізюмський район",
  locality == "село Гряда" & district == "Жовківський район"~ "Львівський район",
  locality == "село Довжик" & district == "Золочівський район"~ "Богодухівський район",
  locality == "село Іванів" & district == "Калинівський район"~ "Хмільницький район",
  locality == "село Іванівка" & district == "Барвінківський район"~ "Ізюмський район",
  locality == "село Калинівка" & district == "Макарівський район"~ "Бучанський район",
  locality == "село Кам’янка" & district == "Новопсковський район"~ "Старобільський район",
  locality == "село Козачі Лагері" & district == "Олешківський район"~ "Херсонський район",
  locality == "село Крижанівка" & district == "Комінтернівський район"~ "Одеський район",
  locality == "село Кульчин" & district == "Ківерцівський район"~ "Луцький район",
  locality == "село Олександрівка" & district == "Комінтернівський район"~ "Одеський район",
  locality == "село Олександрівка" & district == "Лиманський район"~ "Одеський район",
  locality == "село Лиман" & district == "Зміївський район"~ "Чугуївський район",
  locality == "село Микільське" & district == "Білозерський район"~ "Херсонський район",
  locality == "село Микільське" & district == "Світловодський район"~ "Олександрійський район",
  locality == "село Миколаївка" & district == "Великолепетиський район"~ "Каховський район",
  locality == "село Мирне" & district == "Біляївський район"~ "Одеський район",
  locality == "село Мирне" & district == "Оріхівський район"~ "Пологівський район",
  locality == "село Млинівці" & district == "Зборівський район"~ "Тернопільський район",
  locality == "село Моквин" & district == "Березнівський район"~ "Рівненський район",
  locality == "село Муроване" & district == "Пустомитівський район"~ "Львівський район",
  locality == "село Нетреба" & district == "Рокитнівський район"~ "Сарненський район",
  locality == "село Нововасилівка" & district == "Снігурівський район"~ "Баштанський район",
  locality == "село Новодмитрівка" & district == "Великоолександрівський район"~ "Бериславський район",
  locality == "село Новоєгорівка" & district == "Дворічанський район"~ "Куп’янський район",
  locality == "село Новосілки" & district == "Києво-Святошинський район"~ "Фастівський район",
  locality == "село Пеньківка" & district == "Шаргородський район"~ "Жмеринський район",
  locality == "село Петрівка" & district == "Шевченківський район"~ "Куп’янський район",
  locality == "село Петрівське" & district == "Балаклійський район"~ "Ізюмський район",
  locality == "село Підлісся" & district == "Тисменицький район"~ "Івано-Франківський район",
  locality == "село Поляна" & district == "Свалявський район"~ "Мукачівський район",
  locality == "село Привілля" & district == "Слов'янський район"~ "Краматорський район",
  locality == "село Першотравневе" & district == "Зміївський район"~ "Чугуївський район",
  locality == "село Приморське" & district == "Голопристанський район"~ "Скадовський район",
  locality == "село Протопопівка" & district == "Балаклійський район"~ "Ізюмський район",
  locality == "село Рівне" & district == "Красногвардійський район"~ "Курманський район",
  locality == "село Скаржинці" & district == "Ярмолинецький район"~ "Хмельницький район",
  locality == "село Спас" & district == "Рожнятівський район"~ "Калуський район",
  locality == "село Старява" & district == "Мостиський район"~ "Яворівський район",
  locality == "село Суховоля" & district == "Городоцький район"~ "Львівський район",
  locality == "село Травневе" & district == "Міловський район"~ "Старобільський район",
  locality == "село Трудове" & district == "Більмацький район"~ "Пологівський район",
  locality == "село Устимівка" & district == "Глобинський район"~ "Кременчуцький район",
  locality == "село Чайки" & district == "Києво-Святошинський район"~ "Бучанський район",
  locality == "село Червоне" & district == "Сакський район"~ "Євпаторійський район",
  locality == "село Червоне" & district == "Первомайський район"~ "Лозівський район",
  locality == "село Шевченкове" & district == "Васильківський район"~ "Синельниківський район",
  locality == "село Шевченкове" & district == "Києво-Святошинський район"~ "Бучанський район",
  locality == "село Юрівка" & district == "Березнівський район"~ "Фастівський район",
  locality == "село Яринівка" & district == "Березнівський район"~ "Рівненський район",
  locality == "село Ясногородка" & district == "Макарівський район"~ "Фастівський район",
  locality == "село Юрівка" & district == "Києво-Святошинський район"~ "Фастівський район",
  
  
  
  TRUE ~district))


dups_merge <- dups %>% 
  left_join(
    ds_admin
    ,by = c("locality" = "settlement_name_full",
            "region" = "oblast_name_full",
            "district" = "raion_name_full")
  )

dups_merge %>% filter(is.na(settlement_code) )  %>% View()
dups_merge %>% filter(duplicated(edrpou))  %>% View()

dups_merge$raion_name_full <- dups_merge$district

no_dups <- d1 %>%
  group_by(edrpou) %>%
  filter(dplyr::n_distinct(settlement_code) == 1)

full_merge <- rbind(no_dups, dups_merge)

#+ Year when OSBB was created and terminated------------------------------------
full_merge <- full_merge %>% 
  mutate(registration_year = substr(registration, 1, 4),
         termination_year = substr(termination, 1, 4))

#+ Counting number of OSBB since 2015 within ATC--------------------------------
d2 <- full_merge %>% 
  group_by(hromada_code) %>% 
  mutate(sum_osbb_2020 = n() - sum(!is.na(termination_year)))

d2 <- d2 %>% 
  group_by(hromada_code) %>% 
  mutate(sum_osbb_2019 = (sum(registration_year!="2020", na.rm = TRUE)+sum(is.na(registration_year))-sum(!is.na(termination_year)&termination_year!="2020", na.rm = TRUE)),
         sum_osbb_2018 = (sum(registration_year!="2020"&registration_year!="2019", na.rm = TRUE)+sum(is.na(registration_year))-sum(termination_year!="2020"&registration_year!="2019", na.rm = TRUE)),
         sum_osbb_2017 = (sum(registration_year!="2020"&registration_year!="2019"&registration_year!="2018", na.rm = TRUE)+sum(is.na(registration_year))-sum(termination_year!="2020"&registration_year!="2019"&registration_year!="2018", na.rm = TRUE)),
         sum_osbb_2016 = (sum(registration_year!="2020"&registration_year!="2019"&registration_year!="2018"&registration_year!="2017", na.rm = TRUE)+sum(is.na(registration_year))-sum(termination_year!="2020"&registration_year!="2019"&registration_year!="2018"&registration_year!="2017", na.rm = TRUE)),
         sum_osbb_2015 = (sum(registration_year!="2020"&registration_year!="2019"&registration_year!="2018"&registration_year!="2017"&registration_year!="2016", na.rm = TRUE)+sum(is.na(registration_year))-sum(termination_year!="2020"&registration_year!="2019"&registration_year!="2018"&registration_year!="2017"&registration_year!="2016", na.rm = TRUE))
  )

ds2 <- d2 %>%
  distinct(hromada_code,.keep_all= TRUE) %>%
  select(hromada_code,
         hromada_name,
         sum_osbb_2020,
         sum_osbb_2019,
         sum_osbb_2018,
         sum_osbb_2017,
         sum_osbb_2016,
         sum_osbb_2015)

oblast_distr <- d2 %>%
  distinct(hromada_code,.keep_all= TRUE) %>%
  group_by(region_ua, region) %>%
  summarise(osbb_2020 = sum(sum_osbb_2020),
            osbb_2019 = sum(sum_osbb_2019),
            osbb_2018 = sum(sum_osbb_2018),
            osbb_2017 = sum(sum_osbb_2017),
            osbb_2016 = sum(sum_osbb_2016),
            osbb_2015 = sum(sum_osbb_2015)) 

region_distr <- d2 %>%
  distinct(hromada_code,.keep_all= TRUE) %>%
  group_by(region_ua) %>%
  summarise(osbb_2020 = sum(sum_osbb_2020),
            osbb_2019 = sum(sum_osbb_2019),
            osbb_2018 = sum(sum_osbb_2018),
            osbb_2017 = sum(sum_osbb_2017),
            osbb_2016 = sum(sum_osbb_2016),
            osbb_2015 = sum(sum_osbb_2015)) 

#+ save-data, eval=eval_chunks -------------------------------------------------
readr::write_csv(full_merge, "./data-public/derived/osbb-all.csv") #long format
readr::write_csv(ds2, "./data-public/derived/osbb-hromada.csv") #aggregated on hromada level
