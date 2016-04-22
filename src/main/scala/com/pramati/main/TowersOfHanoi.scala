package com.pramati.main

/**
 * @author sharanyak
 *
 */

object TowersOfHanoi {

  def getInput(): Int = {
    var nDisks: Int = 0
    val sc = new java.util.Scanner(System.in)
    print("Enter the number of disks to swap:\t")
    try {
      sc.hasNextInt() match {
        case true =>
          nDisks = sc.nextInt();
        case false =>
          println("Please enter only numbers: \t");
      }
      return nDisks

    } catch {
      case ne: NumberFormatException => {
        println("Error occured due to " + ne)
        System.exit(0)
        return 0
      }
      case e1: Exception => {
        println("Error occured due to" + e1)
        System.exit(0)
        return 0
      }

    }

  }

  def getSrcTowerWithDisks(nDisks: Int): BuildTowers = {
    val tower = new BuildTowers(nDisks)
    pushDisks(nDisks)
    @annotation.tailrec
    def pushDisks(disk: Int): Unit = {
      disk match {
        case 0 => return
        case _ => tower.pushDiskToTower(disk)
      }
      pushDisks(disk - 1)

    }
    return tower

  }
  /**
   * @param args
   */
  def main(args: Array[String]) {

    val nDisks = getInput() // get the input from user

    if (nDisks > 0) { // check for user input greater than zero
      // create towers of type stack
      val towers: Array[BuildTowers] = Array[BuildTowers](new BuildTowers(nDisks), new BuildTowers(nDisks), new BuildTowers(nDisks))
      for (i <- 0 to towers.length - 1) {
        towers(i).towerId = i
      }
      towers(0) = getSrcTowerWithDisks(nDisks)

      println("Before swapping")
      println("Tower 1 [ " + towers(0).stcArray.mkString("\t") + "]\nTower 3 [ " + towers(2).stcArray.mkString("\t") + "]")
      val stepsToPerform = moveDisks(nDisks, towers(0), towers(1), towers(2));
      stepsToPerform.foreach(println)
      println("After swapping")
      println("Tower 1 [ " + towers(0).stcArray.mkString("\t") + "]\nTower 3 [ " + towers(2).stcArray.mkString("\t") + "]")
    } else {
      println("Sorry cannot move disks with value " + nDisks);
    }
  }

  def moveDisks(nDisk: Int, src: BuildTowers, temp: BuildTowers, des: BuildTowers): List[Steps] = {
    nDisk match {

      case 1 => {
        val diskMoved = src.popDiskFromTower()
        des.pushDiskToTower(diskMoved)
        List(Steps(src.towerId, des.towerId, diskMoved))

      }
      case _ => {
        val srcToTemp = moveDisks(nDisk - 1, src, des, temp); //pull n-1 disk from source tower to temporary tower
        val diskMoved = src.popDiskFromTower()
        des.pushDiskToTower(diskMoved)
        val middle = List(Steps(src.towerId, des.towerId, diskMoved))
        val tempToDestin = moveDisks(nDisk - 1, temp, src, des); //pull n-1 disk from temporary tower to destination tower
        srcToTemp ::: middle ::: tempToDestin
      }
    }

  }

  
}
