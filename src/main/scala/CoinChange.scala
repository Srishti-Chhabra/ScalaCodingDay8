class CoinChange {
  def coinChange(coins: Array[Int], amount: Int): Int = {
    def minCoins(ind: Int, amount: Int, coins: Array[Int], dp: Array[Array[Long]]): Long = {
      if(amount==0)
        return 0
      if(ind==0){
        if(amount%coins(0)==0){
          return amount/coins(0)
        }
        else
          return Int.MaxValue
      }
      if(dp(ind)(amount) != -1)
        return dp(ind)(amount)
      val notTake = minCoins(ind-1,amount,coins,dp)
      val take = if(coins(ind)<=amount) 1 + minCoins(ind,amount-coins(ind),coins,dp) else Int.MaxValue
      dp(ind)(amount) = Math.min(notTake,take)
      return dp(ind)(amount)
    }
    val n = coins.size
    val dp = Array.fill[Long](n,amount+1)(-1)
    val res = minCoins(n-1,amount,coins,dp)
    if(res>=Int.MaxValue)
      return -1
    else
      return res.toInt
  }
}
