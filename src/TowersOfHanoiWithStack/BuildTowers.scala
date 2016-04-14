package TowersOfHanoiWithStack
/**
 * @author sharanyak
 *
 *
 */

class BuildTowers(towerSize: Int) {
  var top: Int = -1
  var stcArray = new Array[Int](towerSize)

  /**
   * @param disk
   * @return Boolean This returns true if push is success else false
   */

  def pushDiskToTower(disk: Int): Boolean = {
    if (compareDiskWeight(disk)) {
      top += 1 // move top position
      this.stcArray(top) = disk
      return true
    } else
      return false
  }

  /**
   * @param disk
   * @return Boolean This returns true when disk size is less else false
   */
  def compareDiskWeight(disk: Int): Boolean = {
    if (top == -1||this.stcArray.last < disk)
      return true
    else
      return false

  }

  /**
   * @return Int This returns the value of the disk removed
   */
  def popDiskFromTower(): Int = {
    val diskRemoved: Int = this.stcArray(top)
    top -= 1
    return diskRemoved
  }
  
}