package recfun

import scala.annotation.tailrec

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 4) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
  }

  /**
    * Exercise 1
    */
  def pascal(c: Int, r: Int): Int = {
    if (c == 0 || r == c) 1 else pascal(c - 1, r - 1) + pascal(c, r - 1)
  }

  /**
    * Exercise 2
    */
  def balance(chars: List[Char]): Boolean = {
    @tailrec
    def balanceChar(isBalance: Boolean,
                    open: Int,
                    close: Int,
                    charList: List[Char]): Boolean =
      charList match {
        case head :: tail => {

          if (check(head, -10) == -1)
            balanceChar(false, open, close, charList.tail)
          else {
            if (close > open) false
            else {
              val openBracket = if (head != 41) check(head, open) else open
              val closeBracket = if (head != 40) check(head, close) else close
              balanceChar(
                if (openBracket == closeBracket) true
                else if (closeBracket > openBracket || closeBracket != openBracket)
                  false
                else false,
                openBracket,
                closeBracket,
                charList.tail)
            }
          }
        }
        case Nil => isBalance
      }

    def check(char: Char, bracket: Int): Int = char match {
      case '(' => bracket + 1
      case ')' => bracket + 1
      case _   => -1
    }

    balanceChar(false, 0, 0, chars)
  }

  /**
    * Exercise 3
    */
  def countChange(money: Int, coins: List[Int]): Int = ???
}
