package generator

import util.Random

object SudokuDifficulty extends Enumeration {
	  val Easy = Value
	  val Medium = Value
	  val Hard = Value
 }

class SudokuGenerator {

  /* Represents a row of numbers 
   * */
  type Row = List[Char]
  
  /**
   *  Block, could be either a 3x3 or 9x9 matrix
   *  
   **/
  type Block = List[Row]
  
  /**
   * A function type that takes in a block and converts to a Block
   */
  type BlockTransformer = Block => Block
  
  /* A random arrangement of numbers from 1 to 9. We use this randomly generated
   * order a basis to create the grid */
  val numbers = Random.shuffle(List('1','2','3','4','5','6','7','8','9'))
  
  /** 
   * An initial 3x3 Cell or Block. We'll build our sudoku grid based on this grid
   */
  val block = numbers.grouped(3).toList
  
  /**
   * Shifts all elements of a block to left.
   * ex:
   * 1 2 3
   * 4 5 6
   * 7 8 9 
   * 
   * after shiftLeft will become
   * 2 3 1
   * 5 6 4
   * 8 9 7 
   */
  def shiftLeft (m:Block):Block = 
 		for (row <- m) yield row.tail :+ row.head
 
  /**
   * Shifts all elements of a block to right.
   * ex:
   * 1 2 3
   * 4 5 6
   * 7 8 9 
   * 
   * after shiftLeft will become
   * 3 1 2
   * 6 4 5
   * 9 7 8
   */
  def shiftRight (m:Block): Block = 
 		m.map {r:Row => r.reverse.head +: r.take(2)}

  /**
   * Shifts the Cell upwards. ie. second row will become first, third row will
   * become second and first row will become third
   * ex:
   * 1 2 3
   * 4 5 6
   * 7 8 9
   * will become
   * 
   * 4 5 6
   * 7 8 9
   * 1 2 3
   */
  def moveUp(m:Block): Block =
	m.tail :+ m.head
	
  /**
   * Shifts the Cell downwards. ie. second row will become third, third row will
   * become first and first row will become second
   * ex:
   * 1 2 3
   * 4 5 6
   * 7 8 9
   * will become
   * 7 8 9
   * 1 2 3 
   * 4 5 6
   */
  def moveDown(m:Block): Block =
	 	m.reverse.head :: m.take(2)
	
  /* combination of move left and right */
  def moveDownShiftLeft(block:Block): Block = {
    	shiftLeft(moveDown(block))
  }
    	
  def moveUpShiftRight(block:Block): Block = {
	  shiftRight(moveUp(block))
  }
  
  /*
   * We get 3 rows of 3 3x3 blocks. We combine each row into one big row
   */
  def makeSudokuGrid (grid:List[List[Block]]):Block = {
    def combineBlockRows (b1:Block,b2:Block):Block =
    	(b1 zip b2) map (t=> t._1 ++ t._2)
  
    def reduceToBlock(blocks:List[Block]):Block =
    	blocks reduceLeft combineBlockRows
    
 		val su = for (
 			r <- grid
 		) yield reduceToBlock (r)
		su.flatten
  }  
  
  /**
   * Its a recursive function that builds a List of 3 3x3 blocks by applying
   * the transformer function on the previous 3x3 block
   */
  def buildRow (blocks:List[Block],block:Block,rowNum:Int)(transformer:BlockTransformer):List[Block] = {
    
    if (blocks.size >0 && blocks.size % (rowNum*3) == 0)
      blocks
    else {
      val bl = transformer(block)
      buildRow(blocks :+ bl, bl,rowNum)(transformer)
    }
  }
  
  /**
   * The last block of each row are same. So, we need to fix them by
   * shifting each of them to left
   */
  def fixLastBlocks (blocks:List[Block]): List[Block] = { 
    val grid = blocks.grouped(3).toList
    
    (for {
      row <- grid 
      val index = grid indexOf row
      val transformed = 
        if (index == 1)
          shiftLeft(row.last)
        else if (index == 2)
          shiftLeft(shiftLeft(row.last))
        else
          row.last
    } yield row.init :+ transformed).flatten
  }
  
  /**
   * Prepares the 3 rows of blocks by building each row using the transformer
   * function
   */
  def prepare (t1:BlockTransformer)(t2:BlockTransformer)(t3:BlockTransformer): List[Block] = {
    val rowOne = buildRow(Nil, block,1)(t1)
    val rowTwo = buildRow(rowOne, rowOne.last,2)(t2)
    val rowThree = buildRow(rowTwo, rowTwo.last,3)(t3)
     rowThree
  }
  
  
  def generate : List[Block] = {
    
    val blocks = prepare(moveDownShiftLeft)(moveUpShiftRight)(moveDown)
    fixLastBlocks(blocks)
    
  }
  
  def generateGrid = {
    val sudokuBlocks = generate
    makeSudokuGrid(sudokuBlocks.grouped(3).toList)
  }

}



object SudokuGenerator {
  def create = new SudokuGenerator ().generateGrid

  
  
  val easyMask: List[List[Char]] = List(
	List(' ',' ',' ','x','x',' ','x',' ','x'),
	List(' ',' ',' ',' ','x','x',' ',' ',' '),
	List('x','x',' ',' ',' ','x',' ','x',' '),
	List(' ',' ',' ','x',' ','x',' ','x',' '),
	List(' ',' ','x','x',' ','x',' ','x',' '),
	List(' ','x',' ','x',' ','x',' ',' ',' '),
	List(' ','x',' ','x',' ',' ',' ','x','x'),
	List(' ',' ',' ','x','x',' ',' ',' ',' '),
	List('x',' ','x',' ','x','x',' ',' ',' '))
	
  val mediumMask = List(
	List(' ','x','x',' ','x',' ',' ',' ','x'),
	List(' ',' ','x',' ',' ',' ',' ',' ','x'),
	List(' ',' ','x','x',' ',' ',' ','x','x'),
	List(' ','x','x','x','x',' ',' ',' ',' '),
	List('x',' ',' ',' ','x',' ',' ',' ','x'),
	List(' ',' ',' ',' ','x','x','x','x',' '),
	List('x','x',' ',' ',' ','x','x',' ',' '),
	List('x',' ',' ',' ',' ',' ','x',' ',' '),
	List('x',' ',' ',' ','x',' ','x','x',' '))
  val hardMask = List(
	List(' ',' ','x','x',' ',' ',' ','x',' '),
	List('x',' ',' ','x',' ',' ',' ','x','x'),
	List(' ','x',' ','x','x',' ','x',' ',' '),
	List('x',' ',' ',' ',' ','x',' ','x',' '),
	List(' ',' ',' ',' ',' ',' ',' ',' ',' '),
	List(' ','x',' ','x',' ',' ',' ',' ','x'),
	List(' ',' ','x',' ','x','x',' ','x',' '),
	List('x','x',' ',' ',' ','x',' ',' ','x'),
	List(' ','x',' ',' ',' ','x','x',' ',' '))
	
	private def maskElement(actual:Char)(mask:Char): Char = 
	  if(mask == 'x') actual
	  else mask
	  
	private def maskOneRow (actual:List[Char],mask:List[Char]): List[Char] = 
	    (actual zip mask) map (t => maskElement(t._1)( t._2))
	
	private def convertToPuzzle(actual:List[List[Char]])(mask:List[List[Char]]): List[List[Char]] = 
		(actual zip mask) map (t => maskOneRow(t._1, t._2))
	
  /* now we need to convert this into a puzzle */
	def createPuzzle(level:Int): (List[List[Char]],List[List[Char]]) = {
	    val sudoku : List[List[Char]] = create
	    val mask = if (level ==0) easyMask
	    else if (level == 1) mediumMask
	    else hardMask
	    
	    val puzzle = convertToPuzzle(sudoku)(mask)
	    
	    return (sudoku,puzzle)
	}
}