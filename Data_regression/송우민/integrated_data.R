library(lubridate)
library(dplyr)
#경로 설정
getwd()
path = "/Users/song-woomin/study/23_1/dataMining/final_project/economicdata"
setwd(path)
#econmic_data_handling
temp = list.files(pattern = "*.csv" )
#file 한번에 불러오기.
myfiles = lapply(temp, read_delim)

#data.frame으로 변환.
myfiles = lapply(myfiles, as.data.frame)


#전처리 함수
month_data_handling <- function(data)
{
  before = dim(data)[2]
  for(i in 0:13)
  {
    add = 2023 - i
    add = paste0("(X",add,")")
    data[before+i+1] = apply(dplyr::select(data,matches(add)),1,mean)  
  }
  after = dim(data)[2]
  data = data[,(before+1):after]
  names(data) = as.character(2023:2010)
  
  return(data)
}


#apartment_sale_index_전처리
apartemnt_sale_index = as.data.frame(myfiles[1])
apartemnt_sale_index = apartemnt_sale_index %>% 
  slice(1)
apartemnt_sale_index = month_data_handling(apartemnt_sale_index)
apartemnt_sale_index = t(apartemnt_sale_index)




# 보류.
# central_bank_policy_rate = myfiles[2]
# central_bank_policy_rate = as.data.frame(central_bank_policy_rate)



#경제 심리지수 데이터
economic_sentiment_index = as.data.frame(myfiles[3])[1,]
economic_sentiment_index = month_data_handling(economic_sentiment_index)
economic_sentiment_index = t(economic_sentiment_index)


#경제동행지수
Economic_Coincidence_Index = as.data.frame(myfiles[4])
Economic_Coincidence_Index = month_data_handling(Economic_Coincidence_Index)
Economic_Coincidence_Index = t(Economic_Coincidence_Index)

#경제성장률
economic_growth_international = as.data.frame(myfiles[5])
economic_growth_international = economic_growth_international[,c(2,5:dim(economic_growth_international)[2])]
colnames(economic_growth_international) = c("국가이름",2022:2010)
economic_growth = t(economic_growth_international[1,-1])
rownames(economic_growth) = 2022:2010
economic_growth



#경제활동인구(clear)
#단위 1000명
economically_active_population = as.data.frame(myfiles[6])
economically_active_population = economically_active_population[c(1,103),][,6:18]
row.names(economically_active_population) = c("경제활동인구","실업자")
colnames(economically_active_population) = c(2022:2010)
a = data.frame("2023" = c(mean(c(29236.0,29063.0,28604.0,28387.0)),mean(c(804.0,840.0,890.0,1024.0))))
row.names(a) = c("경제활동인구","실업자")
colnames(a) = "2023"
economically_active_population = cbind(a,economically_active_population)
economically_active_population = t(economically_active_population)



#기대 인플레이션 지수(보류)
expected_inflation_korea = as.data.frame(myfiles[7])
expected_inflation_korea = expected_inflation_korea



#주요국 국내총생산 단위 백만달러
GDP_international_total = as.data.frame(myfiles[8])
GDP_international_total = GDP_international_total[,c(-1,-3,-4)]
colnames(GDP_international_total) = c("국가이름",2022:2010)
GDP_Korea_total = t(GDP_international_total[1,-1])


#국제 주요국 1인당 GDP 단위 : 달러
GDP_international = as.data.frame(myfiles[9])
GDP_international = GDP_international[,-c(1,3,4)]
colnames(GDP_international) = c("국가이름",2022:2010)
GDP_Korea_OnePerson = t(GDP_international[1,-1])

#국제 주요국  산업생산지수
industrial_production_index = as.data.frame(myfiles[10])
industrial_production_index = industrial_production_index[,-c(1,3,4)]
colnames(industrial_production_index) = c("국가이름",2022:2010)
industrial_production_index = t(industrial_production_index[1,-1])


#총 수주액 단위 백만원
instruction_rate = as.data.frame(myfiles[11])
instruction_rate = instruction_rate[1,][,5:dim(instruction_rate)[2]]
colnames(instruction_rate) = c(2022:2010)
instruction_rate = t(instruction_rate)


#이자율
interest_rate = as.data.frame(myfiles[12])
interest_rate = interest_rate[1,][,5:dim(interest_rate)[2]]
colnames(interest_rate) = c(2022:2010)
interest_rate = t(interest_rate)



#main_indicator(보류)
# main_indicator = as.data.frame(myfiles[13])
# View(main_indicator)



#major_international_rate
#주요 국제 금리.....
major_internation_rate = as.data.frame(myfiles[14])
major_internation_rate = major_internation_rate[,c(2,5:dim(major_internation_rate)[2])]
colnames(major_internation_rate) = c("주요국제금리",2022:2010)
a_treasury_bond_3year = t(major_internation_rate[1,-1])


#population
population = as.data.frame(myfiles[15])
population = population[c(1,5),][,c(2,6:dim(population)[2])]
population$계정항목[2] = "합계출산율"
colnames(population) = c("출산율관련지표",2022:2010)
population = t(population[,-1])
colnames(population) = c("추계인구","합계출산율")
population


#raw_material_price_international
raw_material_price_international = as.data.frame(myfiles[16])
unit_raw_material_price_international = raw_material_price_international$단위
raw_material_price_international = raw_material_price_international[,c(2,5:dim(raw_material_price_international)[2])]
colnames(raw_material_price_international) = c("원재료명",2022:2010)
raw_material_price_international = na.omit(raw_material_price_international)
material_temp = raw_material_price_international[,1]
raw_material = t(raw_material_price_international[,-1])
colnames(raw_material) = material_temp
raw_material


#stock 국제주가지수
stock_index = as.data.frame(myfiles[17])
stock_index = stock_index[,c(2,5:dim(stock_index)[2])]
colnames(stock_index) = c("국제주가지수",2022:2010)
name = stock_index[,1]
stock_index = t(stock_index[,-1])
colnames(stock_index) = name
stock_index

#국제 주요국 실업율 -> 실업율 한국 데이터만 뽑아서 겹치니까 제외.
unemployment_rate_index = as.data.frame(myfiles[18])
unemployment_rate_index = unemployment_rate_index[,c(2,5:dim(unemployment_rate_index)[2])]
colnames(unemployment_rate_index) = c("국가이름",2022:2010)
unemployment_rate_index




#data 내보내기.
path1 = "/Users/song-woomin/study/23_1/dataMining/final_project/final_eco_data/"

data = c(Economic_Coincidence_Index,
  economic_growth,
  economically_active_population,
  GDP_Korea_total,
  GDP_Korea_OnePerson,
  industrial_production_index,
  instruction_rate,
  interest_rate,
  a_treasury_bond_3year,
  population,
  raw_material,
  stock_index
)
data_name = c("Economic_Coincidence_Index",
              "economic_growth",
              "economically_active_population",
              "GDP_Korea_total",
              "GDP_Korea_OnePerson",
              "industrial_production_index",
              "instruction_rate",
              "interest_rate",
              "a_treasury_bond_3year",
              "population",
              "raw_material",
              "stock_index"
)

for(i in 1:length(data_name))
{
  data_name[i] = paste0(data_name[i],".csv")
  print(data_name[i])
}

for(i in 1:length(data_name))
{
  temp = paste0(path1,data_name[i])
  write.csv(data[i],temp)
}








