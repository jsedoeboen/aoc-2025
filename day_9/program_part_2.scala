#!/usr/bin/env -S scala-cli -S 3

import scala.collection.mutable.ListBuffer

val source = scala.io.Source.fromFile("input.txt")
val input = source.getLines

val redTiles = ListBuffer[Tile]()

class Tile(val x: Int, val y: Int) {
  def surfaceArea(tile: Tile): Long = {
    val dx = Math.abs(x - tile.x) + 1L
    val dy = Math.abs(y - tile.y) + 1L
    dx * dy
  }

  def crossProduct(twoTilesBack: Tile, previousTile: Tile): Double = {
    (previousTile.x - twoTilesBack.x) * (y - twoTilesBack.y) - (previousTile.y - twoTilesBack.y) * (x - twoTilesBack.x)
  }

  override def toString: String = s"Tile($x, $y)"
}

class Line(val start: Tile, val end: Tile) {
  def intersects(other: Line): Boolean = {
    val eps = 1e-9

    val x1 = start.x.toDouble
    val y1 = start.y.toDouble
    val x2 = end.x.toDouble
    val y2 = end.y.toDouble

    val x3 = other.start.x.toDouble
    val y3 = other.start.y.toDouble
    val x4 = other.end.x.toDouble
    val y4 = other.end.y.toDouble

    val dx1 = x2 - x1
    val dy1 = y2 - y1
    val dx2 = x4 - x3
    val dy2 = y4 - y3

    val denom = dx1 * dy2 - dy1 * dx2

    // ---------------------------------------------
    // CASE 1: PARALLEL OR COLLINEAR
    // ---------------------------------------------
    if (math.abs(denom) < eps) {
      // Check if they are actually collinear (not just parallel)
      val cross = (x3 - x1) * dy1 - (y3 - y1) * dx1
      if (math.abs(cross) > eps) return false // parallel but NOT on same line

      // Collinear: project onto the dominant axis (x or y) and check overlap
      val useX = math.abs(dx1) >= math.abs(dy1)

      val (a0, a1, b0, b1) =
        if (useX) {
          // project on X
          (
            math.min(x1, x2), math.max(x1, x2),
            math.min(x3, x4), math.max(x3, x4)
          )
        } else {
          // project on Y
          (
            math.min(y1, y2), math.max(y1, y2),
            math.min(y3, y4), math.max(y3, y4)
          )
        }

      val overlapStart = math.max(a0, b0)
      val overlapEnd   = math.min(a1, b1)

      // positive-length overlap (ignore single-point touching)
      return overlapEnd - overlapStart > eps
    }

    // ---------------------------------------------
    // CASE 2: PROPER (NON-COLLINEAR) INTERSECTION
    // ---------------------------------------------
    val t = ((x3 - x1) * dy2 - (y3 - y1) * dx2) / denom
    val u = ((x3 - x1) * dy1 - (y3 - y1) * dx1) / denom

    // Only count intersections strictly INSIDE both segments (ignore endpoints)
    t > eps && t < 1.0 - eps && u > eps && u < 1.0 - eps
  }

  override def toString: String = s"Line($start, $end)"
}

def concavePolygon(tiles: ListBuffer[Tile]): ListBuffer[Tile] = {
  var polygon = ListBuffer[Tile]()
  
  for((tile, i) <- tiles.zipWithIndex) {
    if(polygon.size < 3) {
      polygon += tile
    } else {
      val centroid = {
        val xSum = polygon.map(_.x).sum
        val ySum = polygon.map(_.y).sum
        val n = polygon.size
        Tile(xSum / n, ySum / n)
      }
      val lineToMiddle = Line(tile, centroid)
      
      // Check if lineToMiddle intersects with any edge of the polygon
      var intersects = false
       for (j <- 0 until polygon.size if !intersects) {
         val edge = Line(polygon(j), polygon((j + 1) % polygon.size))
         if (lineToMiddle.intersects(edge)) {
           polygon += tile
           intersects = true
         }
       }

      if(!intersects) {
        // check if inside polygon
        val crossings = (0 until polygon.size).count { j =>
          val edge = Line(polygon(j), polygon((j + 1) % polygon.size))
          val rayEnd = Tile(Int.MaxValue, tile.y)
          val ray = Line(tile, rayEnd)
          val i = edge.intersects(ray)
//          println(s"Edge: $edge, ray: $ray, intersects: $i")
          i
        }

//        println(s"Never intersected with polygon, testing tile $tile $crossings. Polygon: ${polygon.mkString(", ")}")
        if (crossings % 2 == 1) { // Odd number of crossings means point is inside
          polygon += tile
        }
      }
    }
  }

  polygon
}

@main def main(): Unit = {
  for(line <- input) {
    val coords = line.split(",").map(_.toInt)
    redTiles += Tile(coords(0), coords(1))
  }

  val polygon = concavePolygon(redTiles)
//  println(polygon)

  var bigestSquare = 0L
  for((redTile, i) <- redTiles.zipWithIndex) {
    for(j <- i + 1 until redTiles.size) {
      val otherTile = redTiles.toList(j)

      println(s"-- Checking $i and $j")

      val squareEdges = List(
        Line(redTile, Tile(redTile.x, otherTile.y)),
        Line(Tile(redTile.x, otherTile.y), otherTile),
        Line(otherTile, Tile(otherTile.x, redTile.y)),
        Line(Tile(otherTile.x, redTile.y), redTile)
      )
      val overlaps = squareEdges.exists(edge =>
        (0 until polygon.size).exists { k =>
          val polyEdge = Line(polygon(k), polygon((k + 1) % polygon.size))
//          println(s"Checking edge $edge against polygon edge $polyEdge = ${edge.intersects(polyEdge)}")

          edge.intersects(polyEdge)
        }
      )
      if (!overlaps) {
//        println(s"No overlap between $redTile and $otherTile")
        val surfaceArea = redTile.surfaceArea(otherTile)
//        if(surfaceArea > bigestSquare) println(s"==========================New bigest square: $redTile and $otherTile = $surfaceArea")
        bigestSquare = Math.max(bigestSquare, surfaceArea)
      } else {
//         println(s"Overlap between $redTile and $otherTile")
      }
    }
  }

  println(s"Answer: $bigestSquare")
}





