# 2.5.2
# 이산적 시계열 값 예측하기
# 매 시간 단위마다 0 혹은 1 값을 갖는 데이터가 있다고 가정해본다.
# 비가 오면 1 값을 갖고, 비가 오지 않으면 0 값을 갖는 기상 데이터라고 하자.
# 어떤 수 k에 대해, 지난 k일의 기상 기록을 기반으로 해 내일의 날씨를 예측할 것이다.

# training set, k 값이 어떤 경우 가장 성능이 좋은지!

preda <- function(x, k){
  n <- length(x)   # n은 전체 기간의 길이.
  k2 <- k/2   # 만약 k 기간 동안 최소 k/2 만큼의 값이 1이었다면, 1로 하고 아닌 경우 0으로 한다.
  
  pred <- vector(length = n-k)  # n-k?  마지막 째 날인 n일은 예측하기만 할 뿐 데이터로 사용하지 않을 것이기 때문에.
  for (i in 1:(n-k)){  # R의 인덱싱은 1부터 시작한다.
    if(sum(x[i:(i+(k-1))]) >= k2)   # 1부터 i+k-1일의 k 개의 데이터로 k+i째 날의 데이터를 예측한다.
      # 마지막 루프 : x[n-k:n-k+k-1]
      pred[i] <- 1   # 마지막 루프 후, n-k, n-k+1, , , , n-1의 데이터로 n번째 날 예측 성공 시.
    else
      pred[i] <- 0
  }
  return (mean(abs(pred - x[(k+1):n])))   # 값 k를 사용했을 경우 오차율 구하기.
  # x[(k+1):n]에는 예측한 날짜들에 대한 실제 값이 들어있다.
  # 뺄샘 -> 절댓값 -> 1인 경우는 오차가 발생한 경우고 0은 아닌 경우이다.
  # 0과 1로 된 데이터의 평균은 1의 비율과 같다는 수학적 사실.
}

test <- rep( c(1, 0, 1, 1), 125)
length(test)  # 500.
preda(test, 4)   # 오차율 ---> 0.25
preda(test, 3)   # 오차율 ---> 0.249497
preda(test, 5)   # 오차율 ---> 0.2505051
preda(test, 2)   # 오차율 ---> 0.248996
# 오차율이 작을 수록, 지정한 k값의 성능이 좋았다는 것.

# sum() 연산이 매번 호출되는 것을 고쳐본다.
predb <- function(x, k){
  n <- length(x)
  k2 <- k/2
  pred <- vector(length = n-k)
  # 처음 트레이닝.
  sm <- sum(x[1:k])
  if (sm >= k2) 
    pred[1] <- 1
  else
    pred[1] <- 0
  
  # n-k는, n개의 데이터를 두고 값 k로 트레이닝 해볼 수 있는 횟수.
  if (n-k >= 2){
    for (i in 2:(n-k)){
      sm <- sm + x[i+k-1] - x[i-1]   # 가장 오래된 원소의 값인 x[i-1]을 빼고
      # 새 데이터인 x[i+k-1]을 더한다.
      if (sm >= k2)
        pred[i] <- 1
      else
        pred[i] <- 0
    }
  }
  return (mean(abs(pred - x[(k+1):n])))
}  


# cumsum()
predc <- function(x, k){
  n <- length(x)
  k2 <- k/2
  # the vector red will contain our predicted values.
  pred <- vector(length = n-k)
  csx <- c(0, cumsum(x))   # 누적합의 벡터는 0부터 시작하도록 고정시켜 준다.
  for (i in 1:(n-k)){
    if(csx[i+k] - csx[i] >= k2)   # 누적합의 차이를 통해 sum(x[i:(i+k-1)])과 같은 값을 구한다.
      pred[i] <- 1
    else
      pred[i] <- 0
  }
  return (mean(abs(pred - x[(k+1):n])))
}
# predb()에서 두 개의 연산을 사용한 것에 비해, 
# predc()의 방식에서는 반복문에서 1개의 뺄셈 연산만을 사용한다.

predc(test, 3)
