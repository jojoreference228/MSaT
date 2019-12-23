package ua.nure.shumak.wumpus

import scala.collection.mutable

class WumpusCave(var caveXDimension: Int = 0, var caveYDimension: Int = 0) {


  private var start = new AgentPosition(1, 1, Orientation.FACING_NORTH)
  private var wumpus = new Room()
  private var gold = new Room()
  private val pits = new mutable.LinkedHashSet[Room]

  private var allowedRooms = new mutable.HashSet[Room]

  /**
    * Create a grid of rooms of dimensions x and y, representing the wumpus's cave.
    *
    * @param caveXDimension
    * the cave's x dimension.
    * @param caveYDimension
    * the cave's y dimension.
    * @param config
    * cave specification - two character per square (unfortunately a Wumpus can reside on top of a pit),
    * first line first, then second line etc. Mapping: S=start, W=Wumpus, G=gold, P=pit.
    */
  def this(caveXDimension: Int, caveYDimension: Int, config: String) {
    this()
    this.caveXDimension = caveXDimension
    this.caveYDimension = caveYDimension
    if (config.length != 2 * caveXDimension * caveYDimension) throw new IllegalStateException("Wrong configuration length.")
    var i = 0
    while ( {
      i < config.length
    }) {
      val c = config.charAt(i)
      val r = new Room(i / 2 % caveXDimension + 1, caveYDimension - i / 2 / caveXDimension)
      c match {
        case 'S' =>
          start = new AgentPosition(r.getX, r.getY, Orientation.FACING_NORTH)
        case 'W' =>
          wumpus = r
        case 'G' =>
          gold = r
        case 'P' =>
          pits.add(r)
        case _ =>
      }

      {
        i += 1; i - 1
      }
    }
    allowedRooms = getAllRooms
  }

  /**
    * Limits possible movement within the cave (for search).
    *
    * @param allowedRooms
    * the set of legal rooms that can be reached within the cave.
    */
  def setAllowed(allowedRooms: Set[Room]): WumpusCave = {
    this.allowedRooms.clear()
    this.allowedRooms.addAll(allowedRooms)
    this
  }

  def setWumpus(room: Room): Unit = {
    wumpus = room
  }

  def setGold(room: Room): Unit = {
    gold = room
  }

  def setPit(room: Room, b: Boolean): Unit = {
    if (!b) pits.remove(room)
    else if (!(room == start.getRoom) && !(room == gold)) pits.add(room)
  }

  def getCaveXDimension: Int = caveXDimension

  def getCaveYDimension: Int = caveYDimension

  def getStart: AgentPosition = start

  def getWumpus: Room = wumpus

  def getGold: Room = gold

  def isPit(room: Room): Boolean = pits.contains(room)

  def moveForward(position: AgentPosition): AgentPosition = {
    var x = position.getX
    var y = position.getY
    position.getOrientation match {
      case Orientation.FACING_NORTH =>
        y += 1
      case Orientation.FACING_SOUTH =>
        y -= 1
      case Orientation.FACING_EAST =>
        x += 1
      case Orientation.FACING_WEST =>
        x -= 1
    }
    val room = new Room(x, y)
    start = if (allowedRooms.contains(room)) new AgentPosition(x, y, position.getOrientation)
    else position
    start
  }

  def turnLeft(position: AgentPosition): AgentPosition = {
    var orientation = ""
    position.getOrientation match {
      case Orientation.FACING_NORTH =>
        orientation = Orientation.FACING_WEST
      case Orientation.FACING_SOUTH =>
        orientation = Orientation.FACING_EAST
      case Orientation.FACING_EAST =>
        orientation = Orientation.FACING_NORTH
      case Orientation.FACING_WEST =>
        orientation = Orientation.FACING_SOUTH
    }
    start = new AgentPosition(position.getX, position.getY, orientation)
    start
  }

  def turnRight(position: AgentPosition): AgentPosition = {
    var orientation = ""
    position.getOrientation match {
      case Orientation.FACING_NORTH =>
        orientation = Orientation.FACING_EAST
      case Orientation.FACING_SOUTH =>
        orientation = Orientation.FACING_WEST
      case Orientation.FACING_EAST =>
        orientation = Orientation.FACING_SOUTH
      case Orientation.FACING_WEST =>
        orientation = Orientation.FACING_NORTH
    }
    start = new AgentPosition(position.getX, position.getY, orientation)
    start
  }

  def getAllRooms= {
    val allowedRooms = new mutable.HashSet[Room]
    for( x <- 1 to caveXDimension; y <- 1 to caveYDimension){
      allowedRooms += new Room(x, y)
    }
    allowedRooms
  }

  override def toString: String = {
    val builder = new StringBuilder
    var y = caveYDimension
    while ( {
      y >= 1
    }) {
      var x = 1
      while ( {
        x <= caveXDimension
      }) {
        val r = new Room(x, y)
        var txt = ""
        if (r == start.getRoom) txt += "S"
        if (r == gold) txt += "G"
        if (r == wumpus) txt += "W"
        if (isPit(r)) txt += "P"
        if (txt.isEmpty) txt = ". "
        else if (txt.length == 1) txt += " "
        else if (txt.length > 2) { // cannot represent...
          txt = txt.substring(0, 2)
        }
        builder.append(txt)

        {
          x += 1; x - 1
        }
      }
      builder.append("\n")

      {
        y -= 1; y + 1
      }
    }
    builder.toString
  }
}
