library(dplyr)
library(readxl)
library(here)
library(readxl)

# Загрузка датафреймов
AGVHD <- read_excel("/Users/juliat/Downloads/TKM6/ezyZip/Final export tables/AGVHD_20230119_120304.xlsx")
CGVHD <- read_excel("/Users/juliat/Downloads/TKM6/ezyZip/Final export tables/CGVHD_20230119_120304.xlsx")
CM <- read_excel("/Users/juliat/Downloads/TKM6/ezyZip/Final export tables/CM_20230119_120304.xlsx")
DM <- read_excel("/Users/juliat/Downloads/TKM6/ezyZip/Final export tables/DM_20230119_120304.xlsx")
GVHD <- read_excel("/Users/juliat/Downloads/TKM6/ezyZip/Final export tables/GVHD_20230119_120304.xlsx")
PREV <- read_excel("/Users/juliat/Downloads/TKM6/ezyZip/Final export tables/PREV_20230119_120304.xlsx")
RS <- read_excel("/Users/juliat/Downloads/TKM6/ezyZip/Final export tables/RS_20230119_120304.xlsx")
STAT <- read_excel("/Users/juliat/Downloads/TKM6/ezyZip/Final export tables/STAT_20230119_120304.xlsx")
TR <- read_excel("/Users/juliat/Downloads/TKM6/ezyZip/Final export tables/TR_20230119_120304.xlsx")
TU <- read_excel("/Users/juliat/Downloads/TKM6/ezyZip/Final export tables/TU_20230119_120304.xlsx")

# Проверка имен столбцов для каждого датафрейма
colnames_AGVHD <- colnames(AGVHD)
colnames_CGVHD <- colnames(CGVHD)
colnames_CM <- colnames(CM)
colnames_DM <- colnames(DM)
colnames_GVHD <- colnames(GVHD)
colnames_PREV <- colnames(PREV)
colnames_RS <- colnames(RS)
colnames_STAT <- colnames(STAT)
colnames_TR <- colnames(TR)
colnames_TU <- colnames(TU)

# Сохранение всех имен столбцов в список для удобства
list_colnames <- list(
  AGVHD = colnames_AGVHD,
  CGVHD = colnames_CGVHD,
  CM = colnames_CM,
  DM = colnames_DM,
  GVHD = colnames_GVHD,
  PREV = colnames_PREV,
  RS = colnames_RS,
  STAT = colnames_STAT,
  TR = colnames_TR,
  TU = colnames_TU
)

# Вывод имен столбцов для проверки
list_colnames

# Вычисление максимальной степени DAMDEG (почему не GVHDAGE?)
CGVHD_aggregated <- CGVHD %>%
  group_by(SUBJID) %>%
  summarize(Max_DAMDEG = max(DAMDEG, na.rm = TRUE))

# каждый SUBJID может иметь несколько записей о разных лекарствах (REPDRUG), разных периодах лечения (TRSTDTC и TRENDTC), разных исходах терапии (TRRESP). 
CM_aggregated <- CM %>%
  group_by(SUBJID) %>%
  summarize(
    Total_Treatments = n(),
    Unique_Meds_Count = n_distinct(REPDRUG),
    Avg_Treatment_Duration = mean(as.Date(TRENDTC, format="%d/%m/%Y") - as.Date(TRSTDTC, format="%d/%m/%Y"), na.rm = TRUE)
  )

# столбцы, такие как GVHDCAT (тип GVHD), GVHDYN (был ли установлен диагноз GVHD), GVHDDTC (дата диагноза GVHD), и GVHDAGE (возраст при диагнозе GVHD), 
# можно, например,подсчитать количество диагнозов GVHD для каждого SUBJID или вычислить средний возраст при диагнозе.
head(GVHD$GVHDAGE)
unique(GVHD$GVHDAGE)

GVHD <- GVHD %>%
  filter(GVHDAGE != "Age at the moment of GVHD diagnosis established") %>%
  mutate(GVHDAGE = as.numeric(GVHDAGE))

GVHD_aggregated <- GVHD %>%
  group_by(SUBJID) %>%
  summarize(
    Total_GVHD_Diagnoses = n(),
    Avg_Age_At_Diagnosis = mean(GVHDAGE, na.rm = TRUE)
  )

# агрегировать данные по REFSTYN (была ли определена рефрактерность к стероидам) и REFSTDTC (дата определения рефрактерности). 
# можно подсчитать количество случаев рефрактерности к стероидам для каждого SUBJID.
RS_aggregated <- RS %>%
  group_by(SUBJID) %>%
  summarize(
    Total_Steroid_Refractory_Cases = sum(REFSTYN == "yes", na.rm = TRUE)
  )

# Объединение датафреймов
combined_df <- DM %>%
  left_join(AGVHD, by = "SUBJID") %>%
  left_join(CGVHD_aggregated, by = "SUBJID") %>%
  left_join(CM_aggregated, by = "SUBJID") %>%
  left_join(GVHD_aggregated, by = "SUBJID") %>%
  left_join(PREV, by = "SUBJID") %>%
  left_join(RS_aggregated, by = "SUBJID") %>%
  left_join(STAT, by = "SUBJID") %>%
  left_join(TR, by = "SUBJID") %>%
  left_join(TU, by = "SUBJID")

View(combined_df)


