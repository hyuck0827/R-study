library(dplyr)
library(ggplot2)

midwest = ggplot2::midwest


# 미성년자의 인구 비율이 낮은 하위 5개의 지역을 출력
# popadults 컬럼의 이름을 adult 변경(rename)
# poptotal 컬럼의 이름을 total 변경(rename)
# child 컬럼을 생성 -> total - adult(mutate)
# child_ratio 컬럼을 생성 -> (child / total) * 100 (mutate)
# child_ratio 컬럼을 기준으로 오름차순 정렬(arrange)
# 상위 5개를 출력(head)
midwest %>% 
  rename(adult = popadults) %>% 
  rename(total = poptotal) %>% 
  mutate(child = total - adult) %>% 
  mutate(child_ratio = (child / total) * 100) %>% 
  arrange(child_ratio) %>% 
  select(county ,adult, total, child, child_ratio) %>% 
  tail(5) -> df
df

# ifelse()함수를 간단하게 구현 
# 조건식이 참인경우 출력은 2번째 인자 값
# 조건식이 거짓인 경우 출력은 3번째 인자 값
# 1번째 인자는 조건식
iftest = function(flag, t_value, f_value){
  # 결과 값을 데이터의 타입은 벡터
  result = c()
  # 위치 변수를 하나 생성 
  result_index = 1
  # flag -> c(TRUE, FALSE, FALSE)
  # flag데이터를 반복문을 생성하여 하나씩 비교
  for (i in flag){
    if (i){
      # result라는 변수 result_index 위치에 t_value 대입
      result[result_index] = t_value
    }else{
      # result라는 변수 result_index 위치에 f_value 대입
      result[result_index] = f_value
    }
    # index의 값을 1 증가시킨다. 
    result_index = result_index + 1
  }
  # result를 되돌려준다. 
  return (result)
}

flag_test = c(TRUE, FALSE, FALSE)
iftest(flag_test, 'T', 'F')

iftest( df$child_ratio > 49 , 'large', 'small' ) -> df$test
df


# 시각화
mpg = ggplot2::mpg
View(mpg)

# 산점도 그래프 
ggplot(
  data=mpg, 
  aes(x= displ, y=hwy)
       ) + geom_point()
# 막대 그래프 
ggplot(
  data=mpg, 
  aes(x = drv)
) + geom_bar()
table(mpg$drv)

mpg %>% 
  group_by(drv) %>% 
  summarise(mean_hwy = mean(hwy)) -> group_data

ggplot(
  data = group_data, 
  aes(x = drv, y = mean_hwy)
) + geom_col()

# 박스플롯
ggplot(
  data = mpg, 
  aes(x = drv, y = hwy)
) + geom_boxplot()

# 라인 그래프 
group_data2 = mpg %>% 
  group_by(year) %>% 
  summarise(count = n())
group_data2

ggplot(
  data = group_data2, 
  aes(x = year, y = count)
) + geom_line()

# 외부 데이터 로드 
# .sav 확장자 파일 로드 
install.packages('foreign')
library(foreign)

read.spss("./data/Koweps.sav", to.data.frame = T) -> welfare

# 데이터프레임중 특정 컬럼만 추출하고 컬럼의 이름을 변경 
df = welfare %>% 
  rename(
    gender = h10_g3, 
    birth = h10_g4, 
    code_job = h10_eco9, 
    income = p1002_8aq1, 
    loc = h10_reg7
  ) %>% 
  select(gender, birth, code_job, income, loc)
head(df)

# 결측치 
NA + 10
10 + NA
NA == NA
NA != NA

# 데이터프레임에서 정보 출력
str(df)
summary(df)

table(is.na(df))

# 컬럼별로 결측치의 개수 출력
table(is.na(df$gender))
table(is.na(df$birth))
table(is.na(df$code_job))
table(is.na(df$income))
table(is.na(df$loc))

# 데이터 수정
# 성별 데이터를 1은 male, 2는 female로 변경
table(df$gender)

df$gender
ifelse(
  # 조건식
  df$gender == 1
  'male',
  ifelse(
    df$gender == 2
    'female'
    NA
  )
) -> df$gender
table(df$gender)
table(is.na(df$gender))

# 과연 성별 평균 임금의 차이가 얼마나 나는가?
# 특정 컬럼을 선택 -> gender , income (select)
# income 컬럼에서 0과 결측치를 제외시킨다. (filter)
# gender를 기준으로 그룹화 (group_by)
# 그룹화 연산 income 의 평균 값 (summaries)
# 변수에 저장 
# 막대그래프 시각화해서 표시

df %>%
  select(gender, income) %>%
  # 인텍스의 조건식 : 0이 아닌 경우, 9999가 아닌경우, 결측치가 아닌경우
  # filter(is.na(income)) %>%
  # filter(income != 0) %>%
  # filter(income != 9999)
  filter(!is.na(income) & income != 0 & income != 9999) %>%
  group_by(gender) %>%
  summarise(gender_income = mean(income)) -> gender_income

ggplot(
  data = gender_income,
  aes(x = gender, y = gender_mean)
) + geom_col()


  
