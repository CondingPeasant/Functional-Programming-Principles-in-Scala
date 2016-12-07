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
    def pascal(c: Int, r: Int): Int = 
      if (r <= 0 || c <= 0 || c >= r) {
        1
      } else {
        pascal(c, r - 1) + pascal(c - 1, r - 1)
      }
  
  /**
   * Exercise 2
   */
    def balance(chars: List[Char]): Boolean = doBalance(0, chars)

    def doBalance(leftParentNum: Int, chars: List[Char]): Boolean = {
        if (chars.isEmpty && leftParentNum == 0) {
            true
        } else if (chars.isEmpty && leftParentNum != 0) {
            false
        } else {
            if (chars.head == '(') {
                doBalance(leftParentNum + 1, chars.tail)
            } else if (chars.head == ')') {
                if (leftParentNum - 1 < 0) {
                    false
                } else {
                    doBalance(leftParentNum - 1, chars.tail)
                }
            } else {
                doBalance(leftParentNum, chars.tail)
            }
        }
    }
  
  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int =
        doCountChange(money, coins.sorted)
    def doCountChange(money: Int, coins: List[Int]): Int = 
        if (money > 0 && coins.isEmpty) {
            0
        } else if (money < coins.head) {
            doCountChange(money, coins.tail)
        } else if (money == coins.head) {
            1
        } else {
            doCountChange(money - coins.head, coins) +
                doCountChange(money, coins.tail)
        }
  }
