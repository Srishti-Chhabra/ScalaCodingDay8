class FindPosNegWays {
  def findTargetSumWays(nums: Array[Int], target: Int): Int = {
    def findWays(ind: Int, target: Int, nums: Array[Int]): Int = {
      if(ind==0){
        if(nums(0)==0 && target==0)
          return 2
        else if(target-nums(0)==0 || target+nums(0)==0)
          return 1
        else
          return 0
      }
      val add = findWays(ind-1,target-nums(ind),nums)
      val sub = findWays(ind-1,target+nums(ind),nums)
      return add+sub
    }
    val n = nums.size
    findWays(n-1,target,nums)
  }
}
