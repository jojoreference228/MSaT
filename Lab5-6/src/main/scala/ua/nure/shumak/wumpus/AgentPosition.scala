package ua.nure.shumak.wumpus

import javafx.geometry.Orientation

class AgentPosition {
  var room: Room = new Room()
  var orientation: String = Orientation.FACING_NORTH

  def this(x: Int, y: Int, orientation: String) {
    this()
    room = new Room(x, y)
    this.orientation = orientation
  }

  def getRoom: Room = room

  def getX: Int = room.getX

  def getY: Int = room.getY

  def getOrientation: String = orientation

  override def toString: String = room.toString + "->" + orientation

  override def equals(obj: Any): Boolean = {
    if (obj != null && (getClass eq obj.getClass)) {
      val other = obj.asInstanceOf[AgentPosition]
      return (getX == other.getX) && (getY == other.getY)
    }
    false
  }

  override def hashCode: Int = {
    var result = 17
    result = 37 * result + room.hashCode
    result = 43 * result + orientation.hashCode
    result
  }
}

object Orientation {
  val FACING_NORTH = "FacingNorth"
  val FACING_SOUTH = "FacingSouth"
  val FACING_EAST = "FacingEast"
  val FACING_WEST = "FacingWest"

}
