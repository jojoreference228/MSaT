package ua.nure.shumak.wumpus

class Room(val x: Int = 1, val y: Int = 1) {
  /**
    *
    * @return the room's x location.
    */
  def getX: Int = x

  /**
    *
    * @return the room's y location.
    */
  def getY: Int = y

  override def toString: String = "[" + x + "," + y + "]"

  override def equals(o: Any): Boolean = {
    if (o != null && o.isInstanceOf[Room]) {
      val r = o.asInstanceOf[Room]
      return x == r.x && y == r.y
    }
    false
  }

  override def hashCode: Int = {
    var result = 17
    result = 37 * result + getX
    result = 43 * result + getY
    result
  }
}
