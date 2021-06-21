package de.htwg.konstanz
package gameoflife
import scala.util.Random

extension (s: String) def red: String = s"${Console.RED}$s${Console.RESET}"

@main def play(mode: String, rest: String*) =

  def waitForEvolution = synchronized {
    wait(500)
  }
  var board = Board(10, 10)

  mode match
    case "blink"       => board = Board.blinking(45, 45)
    case "random" | "" => board = Board(45, 45)

  while (board.cells.exists(_.isAlive)) do
    waitForEvolution
    print("\u001b[2J")
    print("\u001b[100A")
    board.prettyPrint
    board = board.evolve
