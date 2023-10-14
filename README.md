# Seoul_apt_prediction


## I. 연구 내용

### 아파트 특성 분석

아파트의 크기, 방의 개수, 시설물 유무, 건축 연도 등 아파트의 특성에 대한 데이터를 수집하고 분석한다. 이를 통해 아파트 특성과 가격 간의 상관관계를 확인하고자 한다.

### 주변 지리적 환경 분석

아파트의 위치와 주변 환경에 대한 데이터를 수집한다. 주변 학군, 교통 편의성, 상업 시설의 접근성 등의 변수를 고려하여 아파트의 가치에 영향을 미치는 지리적 요소를 분석한다.

### 거래연도의 경제적 상황 분석

아파트 거래가 발생한 연도를 고려하여 해당 연도의 경제적 상황 데이터를 수집한다. 주택 시장의 변동성, 금리 변동 등과 같은 경제적 요소가 집값에 미치는 영향을 분석한다.

### 머신러닝 및 통계적 기법 적용

수집한 데이터를 활용하여 머신러닝 알고리즘과 통계적 기법을 적용한다. 회귀 분석, 의사결정 트리, 신경망 등의 방법을 활용하여 아파트 가격을 예측하는 모델을 개발한다.

# II. 본론

---

## II - 1. 데이터 수집

본 연구에서는 주거용 아파트의 실거래가를 예측하기 위해 다양한 데이터를 수집하였고, 해당 데이터들은 큰 범주에 따라 구분할 수 있다.

- 거래된 아파트가 가지고 있는 특성
- 해당 아파트 주변의 지리적 특성
- 거래연도의 경제적 상황을 나타내는 지표

해당 특성들을 바탕으로 연구를 진행하기 위해 본 연구에서는 공공 데이터 포털, 서울시 열린 데이터 광장에서 데이터를 수집하였다.

이후 좌표를 기준으로 각 아파트에 지리적 특성을 부여하기 위해서 각 아파트와 여러 시설들의 좌표를 알기 위해 구글 맵 API를 통해 지오코딩을 진행하였으며, 각 아파트의 특성을 알기 위해 네이버 부 동산 API 크롤링을 진행하였다.

또한 각 연도의 경제적 상황을 나타내는 지표를 구하기 위해 한국은행 API를 활용하였다.

해당 데이터들을 수집 및 병합하여 만들어진 전체 특성들의 목록 다음과 같다.

![Untitled](https://s3-us-west-2.amazonaws.com/secure.notion-static.com/aee0c0ae-0569-4def-987c-83925236d9c9/Untitled.png)

- min_dist_ 가 붙은 Feature 들은 해당 시설까지의 최단 거리를 의미한다.
- 거리_시설_count는 해당 시설이 해당 반경에 몇 개 위치하고 있는지 나타낸다.

## II - 2. EDA & 데이터 전처리

해당 과정에서는 각 Feature와 부동산 거래 가격의 상관관계를 확인하여 예측에 필요한 변수들을 판별했으며, 다중공선성 문제가 발생하지 않도록 하였다.

### 아파트가 가지고 있는 특성

1. **난방 방식**

![Untitled](https://s3-us-west-2.amazonaws.com/secure.notion-static.com/b593f199-ce81-4ced-88cd-825a68d4f4fa/Untitled.png)

‘HT001’ 값이 절대 다수를 차지하고 있기 때문에 난방 방식에 대해서는 HT001 방식인지, HT001 방식이 아닌지로 구분하였으며, HT001과 부동산 가격과의 상관관계는 약 -0.273 정도로 난방 방식이 HT001일 경우 부동산 가격이 떨어지는 경향을 보였다.

1. **현관 구조**

![Untitled](https://s3-us-west-2.amazonaws.com/secure.notion-static.com/4bc27e13-cf2a-4a83-b9c8-47312d89a7b6/Untitled.png)

현관 구조에는 계단형, 복도형, 복합형이 존재하였으며, 이 역시도 계단형이 압도적으로 많은 모습을 보여 현관 구조가 계단형인지 아닌지에 대해 범주형 변수를 설정하였다. 해당 변수와 부동산 가격의 상관관계는 0.171로 현관 구조가 계단형일 때 다른 구조보다 높은 부동산 가격을 보이고 있다.

1. **세대 수, 주차대수, 세대당 주차대수**

![Untitled](https://s3-us-west-2.amazonaws.com/secure.notion-static.com/fcfa0283-a221-48c5-89c8-b85d56894c49/Untitled.png)

해당 세가지 변수를 모두 고려할 경우 VIF가 10을 초과하여 다중공선성 문제를 초래하였다. 따라서 ‘주차대수’ 변수를 제거하여 해당 문제를 해결하였다

1. **건설사**

![Untitled](https://s3-us-west-2.amazonaws.com/secure.notion-static.com/b6bb74d0-4c97-4f74-812b-1e3f74e355a8/Untitled.png)

해당 데이터에서 2891가지의 건설사가 존재하였으며, 해당 건설사를 모두 고려하여 원-핫 인코딩을 수행한다면 데이터의 차원이 너무 커지는 문제가 발생한다.

해당 건설사 목록에서 상위 7가지 건설사를 선정하였고, 해당 건설사가 건설한 아파트인가에 대한 범주형 변수를 생성하였다.

![Untitled](https://s3-us-west-2.amazonaws.com/secure.notion-static.com/62f2c1a2-336b-4074-a46c-cb83a398e344/Untitled.png)

본 연구에서는 VIF Factor가 10 이상일 때 유의미한 영향을 미친다고 가정하였으며, 해당 과정을 거쳐 선택된 변수들에서는 다중공선성 문제가 발생하지 않았음으로 본 연구에 활용하는 것에 문제가 없다고 판단하였다.

![Untitled](https://s3-us-west-2.amazonaws.com/secure.notion-static.com/c6e5e050-3d8a-4fbb-a566-9f91d20c3645/Untitled.png)

부동산 거래 가격에 대해서는 건물면적의 상관계수가 0.575로 가장 높은 상관관계를 보였으며, 세대 당 주차대수가 0.395, 최고층이 0.291, 난방_HT001이 -0.273으로 높은 상관관계를 보였다.

### 아파트 주변 지리적 특성

1. 자치구, 행정동에 대한 정보는 위도, 경도가 포함하고 있기 때문에 제거했다.
2. 각 시설에 대한 최단 거리, 각 반경 내에 위치한 시설의 개수 등은 서로 상관관계가 굉장히 강하기 때문에 각 시설에 대해 유의한 변수를 선택하여야 했다.

변수의 특성과 상관계수를 고려하여 우리의 상식 안에서 가장 유의하다고 판단되는 변수들을 선택하였고, 그 목록은 다음과 같다.

![Untitled](https://s3-us-west-2.amazonaws.com/secure.notion-static.com/8977d7d9-965c-40ea-a309-5dcb9f6bc770/Untitled.png)

1. 기존에는 해당 아파트에 대해 가장 가까운 지하철 역까지 거리에 대한 데이터가 있었으나, 해당 변수와 부동산 가격 간 음의 상관관계가 발견되었다. 해당 부분은 상식 밖이므로, 해당 아파트가 비역세권인지, 역세권인지, 초역세권인지에 대한 범주형 변수를 생성하였다.

![Untitled](https://s3-us-west-2.amazonaws.com/secure.notion-static.com/2ad2cdce-eb0c-41dd-91c5-feb354ae0899/Untitled.png)

해당 지리적 특성을 가진 변수들의 VIF Factor를 확인해 본 결과, 모두 1.2 아래로 측정되었으며, 이를 통해 해당 변수들에서도 다중공선성 문제가 발생하지 않았다고 판단하였다.

![Untitled](https://s3-us-west-2.amazonaws.com/secure.notion-static.com/e564e91b-3e16-450e-87da-8de742aad74e/Untitled.png)

해당 특성들에 대해 부동산 거래 가격과의 상관계수를 살펴보면 Interchange와 가까울수록 가격이 떨어지는 경향을 보였으며, 마트와 거리가 가까울수록 가격이 올라가는 경향이 크게 보였다.

### 해당 연도의 경제적 상황을 나타내는 지표

1. **다중공선성 문제 해결**

경제 지표의 특성 상 각 연도의 경제 지표 사이에는 강한 상관관계가 발생할 수 밖에 없다. 각 지표들의 VIF Factor를 확인해 본 결과 Inf, 즉 무한대로 확인되어 다중공선성 문제가 강하게 발생하였다는 것을 확인할 수 있었다.

따라서 우리는 유의하다고 판단되는 변수들을 선택하는 과정을 거칠 수 밖에 없었으며, 본 연구에서 최대로 허용하기로 한 10을 넘기지 않는 선에서 우리의 상식 상에서 가장 유의하다고 판단되는 변수들을 하나씩 추가하며 변수를 선택하는 과정을 거쳤다.

![Untitled](https://s3-us-west-2.amazonaws.com/secure.notion-static.com/ad5129d1-7530-4894-8da4-c485b3c99332/Untitled.png)

총 5개의 경제지표가 선택되었으며, 해당 개수 이상으로 변수를 늘릴 경우 바로 VIF Factor가 굉장히 커지는 현상이 발생하였다. 본 5개의 변수에서는 다중공선성 문제가 발생하지 않아 연구에 활용하기 적합하다고 판단하였으며, 해당 5개의 변수 목록은 다음과 같다.

- GDP_Person : 1인당 GDP
- Economic Growth : 해당 연도 경제성장률
- Instruction Rate : 건설 총 수주율
- Interest Rate : 이자율
- 한국(KOSPI) : KOSPI 지수

![Untitled](https://s3-us-west-2.amazonaws.com/secure.notion-static.com/19b67e27-bee8-4798-bba8-b19b29a5c3ac/Untitled.png)

경제성장률과 이자율(금리)가 높은 경우 부동산 거래 가격이 떨어지는 경향을 보였으며, 다른 변수들과 부동산 거래 가격은 양의 상관관계를 보였다.

### 종합

최종적으로 선택된 변수들의 목록은 다음과 같다.

![Untitled](https://s3-us-west-2.amazonaws.com/secure.notion-static.com/395b2d27-21db-4770-b0dd-84a9d6ee413d/Untitled.png)

또한 선택된 모든 변수들을 대상으로 VIF Factor를 확인하여 다중공선성 문제가 발생하였는지 확인한 결과,

![Untitled](https://s3-us-west-2.amazonaws.com/secure.notion-static.com/93b3849e-f44b-4cb3-ab5a-aac0da8d46ce/Untitled.png)

‘총유동인구(서울거주)’, ‘총유동인구(서울비거주)’ 변수에서 약 6 정도의 VIF 수준이 나타났으며, 다른 변수들에서는 매우 작았다. 따라서 본 연구에서는 해당 변수들 사이에서 다중공선성 문제가 발생하지 않았다고 판단하고 모델링 및 추정을 진행하였다.


# III. 결론

---

### III - 1. 결과 정리

본 연구에서는 아파트의 특성, 주변 지리적 특성, 경제적 지표 등 여러 가지 요인을 고려하여 부동산의 거래 가격을 예측하였다.

다중 선형 회귀, Random Forest, Gradient Boosting 등 본 데이터마이닝 강의에서 학습한 모델로도 예측하였으며, 별도로 DNN을 활용하여 예측도 진행해 보았고 그 결과를 간단히 정리하면 다음과 같다.

![Untitled](https://s3-us-west-2.amazonaws.com/secure.notion-static.com/b43f636e-9f2b-4475-b9e5-0e8d47394deb/Untitled.png)

RMSE와 MSE를 기준으로 보았을 때 DNN, Random Forest가 상대적으로 좋은 성능을 보였다.

### III - 2. 한계점 및 문제점

하지만 실제 현장에서는 해당 변수들 이외에도 부동산 관련 정책 등 정량화하기 어려운 변수들이 부동산 거래 가격에 영향을 미칠 수 있기 때문에 이러한 부분에서 보다 정확한 예측을 진행하기 어려운 한계점이 존재한다.

또한 본 연구에서는 모든 데이터를 직접 수집하여 정제, 처리한 후 모델링을 진행하였기 때문에 데이터 정제, 전처리 과정에서 많은 시간을 할애했고, 학기 중에 진행한 프로젝트인 만큼 시간적인 한계가 존재하였다.

따라서 모델링 과정에서 기존 계획보다 더 적은 시간을 할애하여 프로젝트를 진행해야 했고, 이에 따라 완벽하지 않은 부분이 존재하였다.

### III - 3. 개선점 및 의의

본 연구에서는 부동산 거래 가격 예측을 위해 다양한 변수와 모델을 활용하여 결과를 도출하였다. 그러나 몇 가지 개선점과 이를 통해 얻은 의의가 있다.

첫째로, 모델의 성능 향상을 위해 더 다양한 변수를 고려할 수 있다. 현재 연구에서는 아파트 특성, 지리적 특성, 경제적 지표 등을 활용하였지만, 부동산 거래에 영향을 미치는 다른 요소들도 고려할 수 있다. 예를 들어, 인구 통계학적 데이터, 교통량 빅데이터, 상가 시설 등을 추가적인 변수로 포함시킬 수 있다. 이를 통해 모델의 예측력을 향상시킬 수 있다.

둘째로, 데이터 수집 및 전처리 과정을 보다 체계적으로 수행할 필요가 있다. 현재 연구에서는 데이터 수집과정에서 시간적인 제약으로 인해 완벽하지 않은 부분이 존재했다. 따라서 더 많은 데이터를 수집하고, 데이터 정제 및 전처리를 신중하게 수행함으로써 모델의 신뢰도를 높일 수 있다.

셋째로, 보다 정확한 예측을 위해 부동산 관련 정책 등 비정량화된 요소들을 고려할 수 있는 방안을 모색해야 한다. 현재 연구에서는 정량화하기 어려운 변수들을 고려하지 못했다. 이러한 비정량화된 요소들은 부동산 거래 가격에 큰 영향을 미칠 수 있으므로, 해당 변수들을 적절히 반영하는 방법을 탐구해야 한다.

이 연구의 의의는 부동산 거래 가격 예측에 대한 초기 탐색과 모델링 방법의 적용을 보여준 것입니다. 다양한 모델과 변수를 활용하여 예측을 수행하고, 그 결과를 비교하였다. 이를 통해 부동산 시장에 대한 이해를 높이고, 향후 연구나 실제 현장에서의 예측 모델 개발에 활용할 수 있다. 또한, 한계점을 파악하고 개선점을 제시함으로써 더 정확하고 신뢰성 있는 부동산 거래 가격 예측에 대한 연구의 방향성을 제시하는 역할을 수행할 수 있다.

# ***Data Sources***

1. 서울시 부동산 실거래가 정보****,**** 서울 열린 데이터 광장
https://data.seoul.go.kr/dataList/OA-21275/S/1/datasetView.do
2. 한국도로공사_고속도로 출입시설 위치정보(LOD) , 공공 데이터 포털
https://www.data.go.kr/data/15045551/fileData.do?recommendDataYn=Y
3. 서울시 버스정류소 위치정보, 서울 열린 데이터 광장
https://data.seoul.go.kr/dataList/OA-15067/S/1/datasetView.do
4. 서울시 병의원 위치 정보, 서울 열린 데이터 광장
https://data.seoul.go.kr/dataList/OA-20337/S/1/datasetView.do
5. 전국도시철도역사정보표준데이터, 공공 데이터 포털
https://www.data.go.kr/tcs/dss/selectApiDataDetailView.do?publicDataPk=15013205
6. 서울시 대학 및 전문대학 DB 정보 (한국어), 서울 열린 데이터 광장
https://data.seoul.go.kr/dataList/OA-12974/S/1/datasetView.do
7. 서울시 학교 기본정보, 서울 열린 데이터 광장
https://data.seoul.go.kr/dataList/OA-20502/S/1/datasetView.do
8. 서울시 대규모점포 인허가 정보, 서울 열린 데이터 광장
http://data.seoul.go.kr/dataList/OA-16096/A/1/datasetView.do;jsessionid=D0EAB9A2DD155F1DFA5F5A8099B044CC.new_portal-svr-11
9. 서울시 휴게음식점 인허가 정보, 서울 열린 데이터 광장
http://data.seoul.go.kr/dataList/OA-16095/S/1/datasetView.do;jsessionid=767A8C81B0EAD1071775C6640E104020.new_portal-svr-11
10. 한국은행 Open API 서비스, 한국은행
https://ecos.bok.or.kr/api/#/

# *Reference*

1. 배성완•유정석, ⌈기계 학습을 이용한 공동주택 가격 추정: 서울 강남구를 사례로⌋, 부동산학연구 제24집 제1호, 2018. 3, pp. 69~85
2. 주정민 외 3명, ⌈기계학습을 이용한 아파트 매매가격 예측 연구 : 한국아파트의 내·외적 데이터 수집과 가격 예측 중심으로⌋, 2020 온라인 추계학술발표대회 논문집 제27권 제2호 (2020. 11)
3. 이지영•유재필, 인공신경망을 이용한 주택가격지수 예측, Journal of the Korea Academia-Industrial cooperation Society Vol. 22, No. 4 pp. 228-234, 2021
