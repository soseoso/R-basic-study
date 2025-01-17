# R 데이터 구조 예습하기

# * * *

# <벡터> 형식은 R의 핵심이다.  -> 2장

# R에서 스칼라 혹은 단일 수치 값은 실제로 존재하지는 않는다. 
# 단일 숫자처럼 보이는 것은 사실 한 개의 원소를 갖는 벡터이다.
x <- 5
x  # Console의 [1]의 의미: 이 줄에서 <벡터>의 첫 번째 원소부터 보여주기 시작한다.

# * * *

# <문자열>은 실제로는 숫자 형식이 아닌 문자 형식의 단일 원소를 갖는 벡터다.  -> 11장

y <- 'abc'
length(y)
mode(y)
# 1개의 원소를 갖는 한 개의 문자열인 벡터

z <- c('abc', '29 88')
length(z)
mode(z)
# 두 개의 문자열

# 문자열 처리 함수
u <- paste('abc', 'de', 'f')  # 문자열을 결합했다.
print(u)

v <- strsplit(u, ' ')  # 문자열을 공백 기준으로 나눈다.
print(v)

# * * *

# 기술적으로 <행렬>은 벡터지만 행의 개수와 열의 개수라는 두 가지 속성을 추가로 갖는다.
m <- rbind(c(1, 4), c(2, 2))  # 행 결합 rbind(), 열 결합 cbind()
print(m)

# 행렬 m과 벡터 (1, 1)의 곱 계산.
# 선형 대수학 - 행렬 곱 연산자 %*%
m %*% c(1, 1)

# 이중 인덱스 - 0이 아닌 1부터 시작한다!!
print(m[1, 2])
print(m[2, 2])

# R에서 유용한 기능: 벡터에서 부분 벡터를 뽑을 수 있다는 것. 
# 행렬에서 부분 행렬을 뽑을 수 있다는 것.

# 1행
m[1, ]
m[, 2]


# * * *

# <벡터>와 달리, <리스트>의 값들은 여러 데이터 형이다.

x <- list(u = 2, v = 'abc')
print(x)
# 리스트의 원소들은 두 부분으로 나뉜 '이름'을 통해 접근 가능하다.
# 이들은 R에서 $ 기호로 구분되어 있다.
print(x$u)
print(x$v)
# 정교한 결과 값을 갖는 통계 함수에서 유용하게 쓰인다. 가령, hist() 같은 함수들.
hn <- hist(Nile)  # 이렇게 하면 특정 값이 반환된다.
print(hn)  # hist() 함수에서 그래프를 만들면서 여러 값을 가진 리스트를 반환한다.
# 히스토그램의 특성들. 이렇게 출력해보자.
str(hn)
# str은 구조 structure을 뜻한다. 이 기능은 리스트뿐 아니라 모든 R 객체의 구조를 보여준다.

# * * *

# R에서 <데이터 프레임>은 일종의 리스트로서  <- 5장
# 각 리스트의 구성 요소는 데이터의 '행렬' 열에 해당하는 벡터가 된다. (무슨 말인지?)
d <- data.frame(list(kids = c('Jack', 'Jill'), ages = c(12, 10)))
print(d)
print(d$ages)

# 보통 데이터 프레임은 '파일'이나 '데이터베이스'에서 'Data set'을 읽어 들일 때 생성된다.

# * * *

# R은 객체지향 언어이다.
print(hn)  # 여기에서, attr -> 리스트의 클래스를 histogram이라고 명명하는 '속성'