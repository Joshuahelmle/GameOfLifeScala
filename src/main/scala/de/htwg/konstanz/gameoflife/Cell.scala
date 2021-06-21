package de.htwg.konstanz.gameoflife

import scala.concurrent.duration.Deadline

enum CellState:
  case Alive
  case Dead
end CellState

case class Cell(
    val x: Int,
    val y: Int,
    state: CellState,
  ):
  def isAlive: Boolean =
    state match
      case CellState.Alive => true
      case CellState.Dead  => false
  end isAlive

  override def toString: String =
    if isAlive then "*".red else " "
