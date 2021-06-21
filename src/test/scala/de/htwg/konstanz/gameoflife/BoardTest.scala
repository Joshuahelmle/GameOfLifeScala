package de.htwg.konstanz.gameoflife
import de.htwg.konstanz.gameoflife.Board
import org.scalatest.OptionValues

final class BoardTest extends TestSuite with OptionValues:
  test("check neighbours") {
    val b = Board(100, 30)
    //b.getNeighbours(Cell(0, 0, CellState.Alive)).foreach(c => println(c.x -> c.y))
    b.getNeighbours(Cell(0, 0, CellState.Alive)).toSet `shouldBe`
      Set(
        Cell(99, 1, CellState.Alive),
        Cell(0, 1, CellState.Alive),
        Cell(1, 1, CellState.Alive),
        Cell(99, 0, CellState.Alive),
        Cell(0, 0, CellState.Alive),
        Cell(1, 0, CellState.Alive),
        Cell(99, 29, CellState.Alive),
        Cell(0, 29, CellState.Alive),
        Cell(1, 29, CellState.Alive),
      )
  }

  test("evolve once") {
    val b = Board.blinking(10, 10)
    b.evolve.cells.filter(c => c.x == 5 && c.y == 5).headOption.map(_.isAlive) `shouldBe` Some(true)
  }
