findruns <- function(x, k){   # 벡터 x에서 1이 (k 번) 연달아 나오는 부분을 찾는 함수
  n <- length(x)   # 벡터 x의 길이
  runs <- NULL
  for(i in 1:(n-k+1)){   # 1부터 n-K+1 위치까지 테스트. 
    # n-k+1 위치를 테스트할 때, 그 위치로부터 k번째는 벡터의 마지막 요소이기 때문.
    if (all(x[i:(i+k-1)] == 1))   # all() 함수는 편리한 축약식이다.
      # if문에서, 이제 위치 i로부터, k번째 위치까지 검사한다. 모두 1인지를.
      runs <- c(runs, i)   # runs 벡터에 원소를 추가하기 위해, 벡터를 계속해서 다시 할당한다.
      # 위치 i 값을 runs 벡터에 추가한다.
  }
  return (runs)   # 벡터 runs를 반환한다.
}

findruns(c(0, 0, 1, 1, 0, 0, 0, 1, 0, 1, 1, 1), 2)


# runs 벡터 수정 --> 길이가 n인 벡터 공간을 미리 할당한다.
findruns1 <- function(x, k){
  n <- length(x)
  runs <- vector(length = n)   # 메모리 공간을 미리 할당한다. 벡터의 길이는 n.
  count <- 0
  for(i in 1:(n-k+1)){
    if(all(x[i:(i+k-1)] == 1)){
      count <- count+1   # k번 연달아 1인 부분이 존재할 때, 1회를 count에 더한다.
      runs[count] <- i   # count 횟수 만큼, 위치 i들의 정보를 runs가 갖는다.
    }
  }
  if (count > 0)
    runs <- runs[1:count]  # 벡터 runs를 재정의.
  else
    runs <- NULL
  
  return (runs)
}

findruns(c(0, 0, 1, 1, 0, 0, 0, 1, 0, 1, 1, 1), 2)
