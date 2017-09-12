package recfun

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
  }

  /**
   * Exercise 1
   */
  def pascal(column: Int, row: Int): Int = {
    if ((column == row) || (column == 0)) 1 else pascal(column-1, row-1) + pascal(column, row-1)
  }
  
  /**
   * Exercise 2
   */
    def balance(chars: List[Char]): Boolean = {
      val containsOpenBracket = chars.contains('(')
      val containsCloseBracket = chars.contains(')')
      val openBracketLastIndex = chars.lastIndexOf('(')
      val closeBracketAfterOpenIndex = chars.indexOf(')', openBracketLastIndex)
      val closeBracketIsAfterOpenBracket = closeBracketAfterOpenIndex > openBracketLastIndex

      def balanceCharsInnerParentheses : Boolean = {
        val charsWithoutLastBalanceBrackets = chars
          .zipWithIndex
          .filter { case (_, index) => index < openBracketLastIndex && index > closeBracketAfterOpenIndex }
          .map(_._1)
        openBracketLastIndex + 1 == closeBracketAfterOpenIndex || balance(charsWithoutLastBalanceBrackets)
      }

      if(containsOpenBracket && containsCloseBracket && closeBracketIsAfterOpenBracket) balanceCharsInnerParentheses
      else !containsOpenBracket && !containsCloseBracket
    }
  
  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int = {
      val sortedCoins = coins.sorted
      if(sortedCoins.isEmpty || sortedCoins.head > money) 0
      else if (sortedCoins.head < money) countChange(money - sortedCoins.head, sortedCoins) + countChange(money, sortedCoins.tail)
      else 1
    }
  }
