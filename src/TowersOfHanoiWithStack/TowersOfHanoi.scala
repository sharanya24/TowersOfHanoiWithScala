package TowersOfHanoiWithStack

/**
 * @author sharanyak
 *
 */
object TowersOfHanoi {

  var nDisks: Int = _
  val towers: Array[BuildTowers] = new Array[BuildTowers](4)
  var flag: Boolean = false
  
  def getInput(): Unit = {

    print("Enter the number of disks to swap:\t");
    val sc = new java.util.Scanner(System.in)
    try {
      do{
      if (!sc.hasNextInt()) {
        println("Please enter only numbers: \t");
        sc.next();
      }
      }while(!sc.hasNextInt())
      nDisks = sc.nextInt();

      if (nDisks < 0 | nDisks > 2147483647) { // check whether input value is not less than zero
        println("Please enter number of disks greater than 0 and less than 2147483647");
        System.exit(0)
      }

    } catch {
      case ne: NumberFormatException => {
        println("Error occured due to " + ne)
        System.exit(0)
      }
      case e1: Exception => {
        println("Error occured due to")
        System.exit(0)
      }

    }

  }

  /**
   * @param args
   */
  def main(args: Array[String]) {
    getInput() // get the input from user
    for (i <- 0 until towers.length - 1) { //  generate source, destination and temporary towers
      towers(i) = new BuildTowers(nDisks)
    }
    for (j <- nDisks to 1 by -1) {
      towers(0).pushDiskToTower(j)
    }
    message("Before swapping")
    message("Tower 1 [ " + towers(0).stcArray.mkString("\t") + "]\nTower 3 [ " + towers(2).stcArray.mkString("\t") + "]")
    // Recursively pull disks from source tower to destination tower
    moveDisks(nDisks, 0, 1, 2);
    message("After swapping"); message("Tower 3 :[ " + towers(2).stcArray.mkString("\t") + "]")
  }

  def moveDisks(nDisk: Int, src: Int, temp: Int, des: Int): Unit = {
    // If nDisk if one then move the disc from source to destination
    var diskMoved: Int = 0
    if (nDisk == 1) {
      diskMoved = towers(src).popDiskFromTower()
      towers(des).pushDiskToTower(diskMoved)
      message("Pick the disc " + diskMoved + " from tower " + src + "and  push into " + des + " tower")
    } else {
      moveDisks(nDisk - 1, src, des, temp); //pull n-1 disk from source tower to temporary tower
      diskMoved = towers(src).popDiskFromTower()
      towers(des).pushDiskToTower(diskMoved)
      message("Pick the disc " + diskMoved + " from tower " + src + "and  push into " + des + " tower")
      moveDisks(nDisk - 1, temp, src, des); //pull n-1 disk from temporary tower to destination tower
    }

  }

  def message(inString: String) = {

    println(inString);

  }

}