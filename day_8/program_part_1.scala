#!/usr/bin/env -S scala-cli -S 3

import scala.collection.mutable.ListBuffer

val source = scala.io.Source.fromFile("input.txt")
val input = source.getLines

val LOOP_SIZE = 1000

class JunctionBox(val x: Int, val y: Int, val z: Int) {
  override def equals(obj: Any): Boolean = {
    obj match {
      case that: JunctionBox => this.x == that.x && this.y == that.y && this.z == that.z
      case _ => false
    }
  }

  def distanceTo(other: JunctionBox): Int = {
    math.sqrt(
      math.pow(other.x - x, 2) +
        math.pow(other.y - y, 2) +
        math.pow(other.z - z, 2)
    ).toInt
  }

  override def toString: String = s"JunctionBox($x, $y, $z)"
}

val circuits = ListBuffer[ListBuffer[JunctionBox]]()
val junctionBoxes = scala.collection.mutable.Set[JunctionBox]()
val distanceMap = scala.collection.mutable.TreeMap[Int, ListBuffer[(JunctionBox, JunctionBox)]]()

def getCircuitIfJunctionBoxInCircuit(junctionBox: JunctionBox): ListBuffer[JunctionBox] = {
  var retValue = ListBuffer[JunctionBox]()
  var found = false
  for(circuit <- circuits) {
    if(circuit.contains(junctionBox)) {
      if(!found) {
        retValue = circuit
        found = true
      }
    }
  }

  if(!found) {
    circuits += retValue
  }
  retValue
}

@main def main(): Unit = {
  for(line <- input) {
    val junctionBox = line.split(",").map(_.toInt) match {
      case Array(x, y, z) => new JunctionBox(x, y, z)
    }
    circuits :+ Array(junctionBox)
    junctionBoxes += junctionBox
  }

  for((junctionBox, i) <- junctionBoxes.zipWithIndex) {
    for(j <- i + 1 until junctionBoxes.size) {
      val otherJunctionBox = junctionBoxes.toList(j)

      distanceMap.updateWith(junctionBox.distanceTo(otherJunctionBox)) {
        case Some(list) => {
//          println(s"${junctionBox.distanceTo(otherJunctionBox)} !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! ${Some(list += ((junctionBox, otherJunctionBox)))}")
          Some(list += ((junctionBox, otherJunctionBox)))
        }
        case None => Some(ListBuffer((junctionBox, otherJunctionBox)))
      }
    }
  }

  for ((distance, pairs) <- distanceMap.take(LOOP_SIZE)) {
    for ((junctionBox1, junctionBox2) <- pairs) {
      println(s"Distance: $distance between $junctionBox1 and $junctionBox2")
    }
  }

  var realCount = 0
  for((shortestDistance, i) <- distanceMap.zipWithIndex if realCount < LOOP_SIZE) {
      //Add a circuit between the two junction boxes if junctionboxes are not already in circuit
    for((junctionBox1, junctionBox2) <- shortestDistance._2 if realCount < LOOP_SIZE) {
      realCount += 1
      val circuit1 = getCircuitIfJunctionBoxInCircuit(junctionBox1)
      val circuit2 = getCircuitIfJunctionBoxInCircuit(junctionBox2)

      //    println(s"Circuit1: ${circuit1}")
      //    println(s"Circuit2: ${circuit2}")

      if (circuit1.size != 0 && circuit2.size != 0 && circuit1 != circuit2) {
        // merge circuits if they are not already merged
        println("Merging circuits")
        circuit1 ++= circuit2
        circuit2.clear()
      } else if (circuit1.size != 0 || circuit2.size == 0) {
        if (!circuit1.contains(junctionBox1)) {
          circuit1 += junctionBox1
        }

        if (!circuit1.contains(junctionBox2)) {
          circuit1 += junctionBox2
        }
      } else {
        if (!circuit2.contains(junctionBox2)) {
          circuit2 += junctionBox2
        }
        if (!circuit2.contains(junctionBox1)) {
          circuit2 += junctionBox1
        }
      }
    }

    println(s"Step $realCount")
    for(circuit <- circuits.sortWith((s, t) => s.size > t.size) if circuit.size > 0) {
      println("Circuit:")
      for(junctionBox <- circuit) {
        println(s"  $junctionBox")
      }
    }
    println("----------------------------------------")

  }

  //remove empty circuits

  println(circuits.count(s => s.size > 0) + " Circuits")
//  for(circuit <- circuits.sortWith((s, t) => s.size > t.size) if circuit.size > 0) {
//    println("Circuit:")
//    for(junctionBox <- circuit) {
//      println(s"  $junctionBox")
//    }
//  }

  println(circuits.sortWith((s, t) => s.size > t.size).take(3).map(_.size))
  val answer = circuits.sortWith((s, t) => s.size > t.size).take(3).map(_.size).product
  println(s"Answer: $answer")

}





