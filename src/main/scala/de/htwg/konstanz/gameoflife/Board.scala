package de.htwg.konstanz.gameoflife
import scala.util.Random

case class Board(
    cells: Seq[Cell],
    length: Int,
    height: Int,
  ):

  def getNeighbours(cell: Cell): Seq[Cell] =
    cells
      .collect {
        case c: Cell if cell.x == 0 && c.x == length - 1                       => c
        case c: Cell if cell.x == length - 1 && c.x == 0                       => c
        case c: Cell if ((c.x - 1) to (c.x + 1)).contains(cell.x) && c != cell => c
      }
      .collect {
        case c: Cell if cell.y == 0 && c.y == height - 1          => c
        case c: Cell if cell.y == height - 1 && c.y == 0          => c
        case c: Cell if ((c.y - 1) to (c.y + 1)).contains(cell.y) => c
      }

  def evolve: Board =
    val evolvedCells = cells.zip(cells.map(getNeighbours).map(_.count(_.isAlive))).map {
      case (cell, aliveNeighbours)
           if cell.isAlive && (aliveNeighbours == 2 || aliveNeighbours == 3) =>
        cell.copy(state = CellState.Alive)
      case (cell, aliveNeighbours) if !cell.isAlive && aliveNeighbours == 3 =>
        cell.copy(state = CellState.Alive)
      case (cell, _) => cell.copy(state = CellState.Dead)
    }
    copy(cells = evolvedCells)
  end evolve

  def prettyPrint: Unit =
    print("─" * (length + 2) + '\n')
    cells.grouped(length).foreach(line => print(line.mkString("|", "", "|\n")))
    print("─" * (length + 2) + '\n')

object Board:

  def apply(length: Int, height: Int): Board =
    new Board(
      for
        x <- 0 until length
        y <- 0 until height
      yield Cell(x, y, if Random.nextInt(100) < 10 then CellState.Alive else CellState.Dead),
      length,
      height,
    )

  def blinking(length: Int, height: Int): Board =
    var init = for
      x <- 0 until length
      y <- 0 until height
    yield Cell(x, y, CellState.Dead)
    init = init
      .updated(
        (height / 2 * length) + length / 2 - 1,
        Cell(length / 2, height / 2 - 1, CellState.Alive),
      )
      .updated(
        (height / 2 * length) + length / 2,
        Cell(length / 2, height / 2, CellState.Alive),
      )
      .updated(
        (height / 2 * length) + length / 2 + 1,
        Cell(length / 2, height / 2 + 1, CellState.Alive),
      )
    apply(init, length, height)

  def empty(length: Int, height: Int): Board =
    new Board(
      for
        x <- 0 until length
        y <- 0 until height
      yield Cell(x, y, CellState.Dead),
      length,
      height,
    )
