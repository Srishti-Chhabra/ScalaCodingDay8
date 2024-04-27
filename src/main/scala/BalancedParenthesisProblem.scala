import scala.collection.mutable.Stack

class BalancedParenthesisProblem {

  def isValid(p: String): Boolean = {
    val stack = new Stack[Char]
    def loop(i: Int): Boolean = {
      if(i>=p.size)
        return true
      if(p(i) == '('){
        stack.push('(')
        loop(i+1)
      }
      else{
        if(stack.isEmpty)
          false
        else {
          stack.pop()
          loop(i+1)
        }
      }
    }
    loop(0)
  }

  def generateParentheses(n: Int): List[String] = {
    def generate(p: String, left: Int, right: Int, parens: List[String]): List[String] = {
      if(left==0 && right==0){
        if(isValid(p))
          return p :: parens
        else
          return parens
      }
      val close = if(right==0) parens else generate(p + ")", left, right-1, parens)
      val notClose = if(left==0) parens else generate(p + "(", left-1, right, parens)
      close ++ notClose
    }

    generate("", n, n, List())
  }
}

object BalancedParenthesisProblem extends App{
  val balancedParenthesisProblem = new BalancedParenthesisProblem
  println(balancedParenthesisProblem.generateParentheses(3))
}