package controllers

import play.api._
import play.api.mvc._
import play.api.mvc.Results.Ok
import generator.SudokuGenerator
import play.api.libs.json.Json
import play.api.libs.json.Writes
import play.api.libs.json.JsArray
import play.api.libs.json.JsArray
import play.api.libs.json.JsString


object SudokuController {


  implicit object RowWrites extends Writes[List[Char]] {
    def writes(row:List[Char]) = JsArray(row.map(c => JsString(c.toString)))
  }
  
  implicit object GridWrites extends Writes[List[List[Char]]] {
    def writes(grid:List[List[Char]]) = JsArray(grid.map(row => Json.toJson(row)(RowWrites)))
  }
  
  def index(level:Int=1) = Action {
    val (solution,puzzle) = SudokuGenerator.createPuzzle(level%3)
    
   val jsonSudoku = Json.toJson(puzzle)(GridWrites)
    Ok(jsonSudoku)
  }
}