#Using QuickSort
Qsort <- function(input_vector,m,n) {
    # m : 起點，n :終點
    if (m < n ){
        i <- m
        j <- n+1
        PK <- input_vector[m]
        # Divide input_vector
        # 利用PivotKey(PK)，尋找其排序後應該存在的位置，並以此位置進行分割
        while (i < j){
            repeat{
                i=i+1
                if (i > n){
                    break #跑出範圍就中斷
                }
                if (input_vector[i] >= PK){
                    break #從右往左找到第一個比PK大的數就中斷
                }
            }
            repeat{
                j=j-1
                if(j == m){
                    break #跑出範圍就中斷
                }
                if (input_vector[j]<=PK){
                    break #從左往右找到第一個比PK小的就中斷
                }
            }
            if (i < j ){
                # 只要 i j 尚未交會就SWAP
                swap <-  input_vector[i]
                input_vector[i] <- input_vector[j]
                input_vector[j] <- swap
            }
        }
        #Tombstone
        #已經被決定位置的數值，會立在位置 j
        #PK與item j SWAP
        tombstone <- input_vector[j]
        input_vector[j] <- input_vector[m]
        input_vector[m] <- tombstone
        #Conquer Part
        #在位置 j 的左右兩側，分別進行Qucik Sort，並儲存結果
        input_vector <-  Qsort(input_vector,m,j-1)
        input_vector <- Qsort(input_vector,j+1,n)
    }
    return(input_vector)
}

# call QuickSort
QuickSort <- function(input_vector,decreasing = FALSE){
    list_length <- length(input_vector) #計算輸入數列的長度
    increading_order <- Qsort(input_vector,1,list_length)
    result <- increading_order
    if (decreasing==TRUE){
        decreasing_order <- rev(increading_order)
        #若decreasing = TRUE，則將遞增的結果反序
        result <- decreasing_order
    }
    return(result)
}

# 建立任意排列之數列，大小介於0 ~ 100，長度為 10
unsorted_vector <- round(runif(10) * 100)
unsorted_vector

# 遞增排序
QuickSort(unsorted_vector,decreasing = FALSE)

# 遞減排序
QuickSort(unsorted_vector,decreasing = TRUE)

#------------------------------------------------

##自訂樣本標準差函式 Standard Deviation 
SSD <- function(input_vector){
    meanX <- mean(input_vector)
    summation <- 0
    for(x in input_vector){
        summation <- summation+ (x-meanX)^2
    }
    y <- summation/(length(input_vector)-1)
    SSD_result <- sqrt(y)
    return(SSD_result)
}

#建立input
input_ssd_vector <- round(runif(10) * 100) 
SSD(input_ssd_vector)
SSD(1:100)

#------------------------------------------------

##自訂BMI計算函式
BMI_Calculater <- function(weights,heights){
    heights <- heights/100
    return(weights / heights^2)
}

#原始資料
heights <- c(173, 168, 171, 189, 179)
weights <- c(65.4, 59.2, 63.6, 88.4, 68.7)
heights_and_weights <- data.frame(heights, weights)

# call BMI_Calculater
bmi <- mapply(FUN=BMI_Calculater,w=weights,h=heights)
BMI <- cbind(heights_and_weights,bmi)
str(BMI)
View(BMI)



