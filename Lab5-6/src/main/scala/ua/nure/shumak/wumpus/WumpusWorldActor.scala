package ua.nure.shumak.wumpus

import java.util

import akka.actor.{Actor, ActorLogging, ActorRef, PoisonPill}
import akka.event.Logging

case class WumpusWorldActor() extends Actor with ActorLogging{

  private var cave = new WumpusCave(caveXDimension = 4, caveYDimension = 4, config = Constants.INITIAL_WUMPUS_CAVE)
  private var isWumpusAlive = true
  private var isGoldGrabbed = false
  private var agentPosition = new AgentPosition(x = 1, y = 1, orientation = Orientation.FACING_NORTH)
  private var hasArrow = true

  var sendTerminateMessage = false
  var sendWinMessage = false

  def receive = {
    case "start" =>
      log.info(s"Wumpus world: Hello! The Wumpus world agent ${self.path} is ready.")
      log.info("Wumpus world: Current world state:");
      log.info("\n" + cave.toString);

    case "takeDown" =>
      log.info("Wumpus world: The Wumpus world agent ${self.path} terminating.")
      self ! PoisonPill

    case ActorTaken(act) => act ! "begin"

    case Message(act, typeMes, mes) => typeMes match {
      case Constants.GAME_INFORMATION =>
        act ! Message(self, Constants.GAME_INFORMATION, getPerceptSeenBy().toString)

      case Constants.PROPOSE_TYPE => mes match {
        case Constants.SPELEOLOGIST_TURN_LEFT =>
          turnLeft()
          act ! Message(self, Constants.RESULT_TYPE, Constants.OK_MESSAGE)
          show

        case Constants.SPELEOLOGIST_TURN_RIGHT =>
          turnRight()
          act ! Message(self, Constants.RESULT_TYPE, Constants.OK_MESSAGE)
          show

        case Constants.SPELEOLOGIST_MOVE_FORWARD =>
          sendTerminateMessage = moveForward()
          if (sendTerminateMessage) act ! Message(self, Constants.RESULT_TYPE, Constants.FAIL_MESSAGE) else act ! Message(self, Constants.RESULT_TYPE, Constants.OK_MESSAGE)
          show

        case Constants.SPELEOLOGIST_GRAB =>
          grab()
          act ! Message(self, Constants.RESULT_TYPE, Constants.OK_MESSAGE)

        case Constants.SPELEOLOGIST_SHOOT =>
          shoot()
          act ! Message(self, Constants.RESULT_TYPE, Constants.OK_MESSAGE)

        case Constants.SPELEOLOGIST_CLIMB =>
          if (climb()) act ! Message(self, Constants.RESULT_TYPE, Constants.WIN_MESSAGE) else Message(self, Constants.RESULT_TYPE, Constants.FAIL_MESSAGE)

        case _ => log.info("Wrong Message")
      }
      case _ => log.info("Wrong Message")
    }

    case _ => log.info("Wrong Message")
  }

  private def show = log.info("Wumpus world: Current world state after the action:" + "\n" + cave)

  private def turnLeft() = {
    agentPosition = cave.turnLeft(agentPosition)
  }

  private def turnRight() = {
    agentPosition = cave.turnRight(agentPosition)
  }

  private def moveForward() = {
    agentPosition = cave.moveForward(agentPosition)
    (isWumpusAlive && cave.getWumpus == agentPosition.getRoom) || cave.isPit(agentPosition.getRoom)
  }

  private def grab(): Unit = {
    if (cave.getGold == agentPosition.getRoom) isGoldGrabbed = true
  }

  private def shoot(): Unit = {
    if (hasArrow && isAgentFacingWumpus(agentPosition)) isWumpusAlive = false
  }

  private def climb() = agentPosition.getRoom == new Room(1, 1) && isGoldGrabbed

  def getPerceptSeenBy(): WumpusPercept = {
    val result = new WumpusPercept
    val pos = agentPosition
    val adjacentRooms = Array(new Room(pos.getX - 1, pos.getY), new Room(pos.getX + 1, pos.getY), new Room(pos.getX, pos.getY - 1), new Room(pos.getX, pos.getY + 1))
    var adjacentRoomsFull = Array.empty[Room]
    for (r <- adjacentRooms) {
      adjacentRoomsFull = adjacentRoomsFull ++ Array(new Room(r.getX - 1, r.getY), new Room(r.getX + 1, r.getY), new Room(r.getX, r.getY - 1), new Room(r.getX, r.getY + 1))
    }
    adjacentRoomsFull = adjacentRoomsFull ++ adjacentRooms
    for (r <- adjacentRoomsFull) {
      if (r == cave.getWumpus) result.setStench
      if (cave.isPit(r)) result.setBreeze
    }
    if (pos.getRoom.equals(cave.getGold)) result.setGlitter
    if (!isWumpusAlive) result.setScream
    result
  }

  private def isAgentFacingWumpus(pos: AgentPosition): Boolean = {
    val wumpus = cave.getWumpus
    pos.getOrientation match {
      case Orientation.FACING_NORTH =>
        return pos.getX == wumpus.getX && pos.getY < wumpus.getY
      case Orientation.FACING_SOUTH =>
        return pos.getX == wumpus.getX && pos.getY > wumpus.getY
      case Orientation.FACING_EAST =>
        return pos.getY == wumpus.getY && pos.getX < wumpus.getX
      case Orientation.FACING_WEST =>
        return pos.getY == wumpus.getY && pos.getX > wumpus.getX
    }
    false
  }

}

object Constants {
  val WUMPUS_SERVICE_DESCRIPTION = "wumpus_world"
  val NAVIGATOR_SERVICE_DESCRIPTION = "navigator"
  val GO_INSIDE = "go_inside"
  val WUMPUS_WORLD_DIGGER_CONVERSATION_ID = "digger_world"
  val NAVIGATOR_DIGGER_CONVERSATION_ID = "digger_navigator"
  val NAVIGATOR_AGENT_TYPE = "navigator_agent"
  val INITIAL_WUMPUS_CAVE = ". . . P W G . . . . . . S . P . "
  val OK_MESSAGE = "OK"
  val FAIL_MESSAGE = "FAIL"
  val WIN_MESSAGE = "WIN"
  val SPELEOLOGIST_TURN_LEFT = "SPELEOLOGIST_LOOK_LEFT"
  val SPELEOLOGIST_TURN_RIGHT = "SPELEOLOGIST_TURN_RIGHT"
  val SPELEOLOGIST_MOVE_FORWARD = "SPELEOLOGIST_MOVE_FORWARD"
  val SPELEOLOGIST_GRAB = "SPELEOLOGIST_GRAB"
  val SPELEOLOGIST_SHOOT = "SPELEOLOGIST_SHOOT"
  val SPELEOLOGIST_CLIMB = "SPELEOLOGIST_CLIMB"
  val GAME_INFORMATION = "INFORMATION"
  val GAME_COMMAND = "GAME_COMMAND"
  val ADVICE_TYPE = "ADVICE_TYPE"
  val PROPOSE_TYPE = "PROPOSE_TYPE"
  val RESULT_TYPE = "RESULT_TYPE"
  val ADVICE_PROPOSAL = "Advice me, navigator."
  val INFORMATION_PROPOSAL_NAVIGATOR = "Give me current game information."
  val INFORMATION_PROPOSAL_SPELEOLOGIST = "Giving you information: "
  val ACTION_PROPOSAL1 = "You should "
  val ACTION_PROPOSAL2 = "I think it is a good option to "
  val ACTION_PROPOSAL3 = "Maybe you can "
  val MESSAGE_LEFT = "turn left."
  val MESSAGE_RIGHT = "turn right."
  val MESSAGE_FORWARD = "move forward."
  val MESSAGE_GRAB = "grab the gold."
  val MESSAGE_SHOOT = "shoot."
  val MESSAGE_CLIMB = "climb the ladder."
}

case class Message(from: ActorRef, typeMes: String, mes: String)
case class ActorTaken(from: ActorRef)