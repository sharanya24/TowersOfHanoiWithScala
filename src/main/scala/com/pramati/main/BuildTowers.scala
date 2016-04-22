package com.pramati.main
/**
 * @author sharanyak
 *
 */
case class Steps(src: Int, des:Int, diskMoved: Int) { // Move is to store the steps of process
  override def toString():String = "Pick the disc " + diskMoved + " from tower " + src + " and push into " + des+ " tower"
}

class BuildTowers(towerSize: Int) {
  var top: Int = -1
  val stcArray = new Array[Int](towerSize)
   var towerId:Int=_
  /**
   * @param disk
   * @return Boolean This returns true if push is success else false
   */

  def pushDiskToTower(disk: Int): Boolean = {
    
    compareDiskWeight(disk) match{
      case true =>{
        top += 1 // move top position
        this.stcArray(top) = disk
        true
     }
      case false =>
        false
    }  
  }

  /**
   * @param disk
   * @return Boolean This returns true when disk size is less else false
   */
  def compareDiskWeight(disk: Int): Boolean = {
    disk match{
      case x if(this.top == -1||this.stcArray.last < disk) => true
      case x=>  false
    }
  }

  /**
   * @return Int This returns the value of the disk removed
   */
  def popDiskFromTower(): Int = {
    val diskRemoved: Int = this.stcArray(top)
    this.stcArray(top) = 0 // replace top disk weight to zero
    this.top -= 1 // move top position
    return diskRemoved
  }

}