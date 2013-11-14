package simulations

import math.random

class EpidemySimulator extends Simulator {

  def randomBelow(i: Int) = (random * i).toInt

  protected[simulations] object SimConfig {
    val population: Int = 300
    val roomRows: Int = 8
    val roomColumns: Int = 8

    val prevalenceRate: Double = 0.01
    val transmissibilityRate: Double = 0.4
    // to complete: additional parameters of simulation
  }

  import SimConfig._

  val numInitiallySick = (prevalenceRate * population).toInt
  val numHealthy = (population - numInitiallySick).toInt

  val persons: List[Person] =
    List.fill(numInitiallySick)(new Person(0) { infected = true }) ++
    List.fill(numHealthy)(new Person(0) { infected = false })

  class Person (val id: Int) {
    var infected = false
    var sick = false
    var immune = false
    var dead = false

    // demonstrates random number generation
    var row: Int = randomBelow(roomRows)
    var col: Int = randomBelow(roomColumns)

    def moveNextRoom() {
      val (dx, dy) = (randomBelow(2) * 2 - 1, randomBelow(2) * 2 - 1)
      row = (row + roomRows + dx) % roomRows
      col = (col + roomColumns + dy) % roomColumns
    }

    def setupNextMovement() {
      afterDelay(1 + randomBelow(4)) {
        moveNextRoom()
        setupNextMovement()
      }
    }

    setupNextMovement()
  }
}
