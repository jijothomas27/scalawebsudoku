package controllers

import play.api._
import play.api.mvc._
import play.api.mvc.Results.Ok
import generator.SudokuGenerator


object SudokuController {


  def index(level:Int=1) = Action {
    val (solution,puzzle) = SudokuGenerator.createPuzzle(level%3)
    Ok(views.html.sudoku.index(puzzle))
  }
}