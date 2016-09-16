package solutions

import scala.io.Source

/**
 * Created by ralphrecto on 9/15/16.
 */
object problem011 {
  /**
    * Given a row/col position, return a list of lines (each line being a list of positions)
    * clamped w.r.t. the given width/height. Floors to 0.
    */
  def clampedLines(matrix : Vector[Vector[Int]], lineLength : Int)(x : Int, y: Int) : Seq[Seq[Int]] = {
    val (right, bottom) = (matrix.size, matrix(0).size)
    val xcoords = x.until(Math.min(right, x+lineLength))
    val downCoords = y.until(Math.min(bottom, y+lineLength))
    val upCoords = Math.max(0, y+1-lineLength).to(y).reverse

    val horizontal= xcoords.map(matrix(_)(y))
    val vertical = downCoords.map(matrix(x)(_))
    val (diagSouth, diagNorth) = if (xcoords.size != downCoords.size) { (List(), List()) } else {
      (
        xcoords.zip(downCoords).map(t => matrix(t._1)(t._2)),
        xcoords.zip(upCoords).map(t => matrix(t._1)(t._2))
      )
    }
    List(horizontal, vertical, diagSouth, diagNorth).filter(_.size == lineLength)
  }

  def main(args : Array[String]) : Unit = {
    val matrix = Source.fromFile("./txt/euler011.txt")
        .getLines()
        .toVector
        .map(_.split(" ").toVector.map(_.toInt))
    val lineLength = 4

    val clampedWrtParams : (Int, Int) => Seq[Seq[Int]] = clampedLines(matrix, lineLength)

    System.out.println(clampedWrtParams(16, 16))

    val res = matrix.indices.map(i =>
      matrix(i).indices.map(j => {
        clampedWrtParams(i, j).map(_.product).fold(0)(Math.max)
      }).fold(0)(Math.max)
    ).fold(0)(Math.max)

    System.out.println(res)
  }
}
