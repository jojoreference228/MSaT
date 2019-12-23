package ua.nure.shumak.wumpus

import akka.actor.{Actor, ActorLogging, ActorRef, PoisonPill}
import scala.util.matching.Regex

class SpeleologistActor(world: ActorRef, navigator: ActorRef) extends Actor with ActorLogging{

  def receive = {
    case "start" =>
      log.info(s"Speleologist: Hello! The speleologist agent ${self.path} is ready.")

    case "begin" => self ! Message(self, Constants.ADVICE_PROPOSAL, "")

    case "takeDown" =>
      log.info("Speleologist: The speleologist agent ${self.path} terminating.")
      self ! PoisonPill

    case Message(act, typeMes, mes) => typeMes match {
      case Constants.ADVICE_PROPOSAL =>
        log.info(Constants.ADVICE_PROPOSAL)
        navigator ! Message(self, Constants.ADVICE_PROPOSAL, "")

      case Constants.INFORMATION_PROPOSAL_NAVIGATOR =>
        if (parseNavigatorMessageRequest(mes))
          log.info(world.toString())
          world ! Message(self, Constants.GAME_INFORMATION, "")

      case Constants.GAME_INFORMATION =>
        log.info(Constants.INFORMATION_PROPOSAL_SPELEOLOGIST +  mes)
        navigator ! Message(self, Constants.INFORMATION_PROPOSAL_SPELEOLOGIST, mes)

      case Constants.ADVICE_TYPE =>
        var action = parseNavigatorMessageProposal(mes)
        var content = ""
        action match {
          case Commands.TURN_LEFT => content = Constants.SPELEOLOGIST_TURN_LEFT
          case Commands.TURN_RIGHT => content = Constants.SPELEOLOGIST_TURN_RIGHT
          case Commands.MOVE_FORWARD => content = Constants.SPELEOLOGIST_MOVE_FORWARD
          case Commands.GRAB => content = Constants.SPELEOLOGIST_GRAB
          case Commands.SHOOT => content = Constants.SPELEOLOGIST_SHOOT
          case Commands.CLIMB => content = Constants.SPELEOLOGIST_CLIMB
          case _ =>
        }
        world ! Message(self, Constants.PROPOSE_TYPE, content)

      case Constants.RESULT_TYPE => mes match {
        case Constants.OK_MESSAGE =>
          self ! Message(self, Constants.ADVICE_PROPOSAL, "")

        case Constants.FAIL_MESSAGE =>
          log.info("Speleologist: You failed!")
          self ! PoisonPill

        case Constants.WIN_MESSAGE =>
          log.info("Speleologist: The speleologist survived and won!")
          self ! PoisonPill

        case _ => log.info("Wrong command!")
      }

      case _ => log.info("Wrong command!")
    }
    case _ => log.info("Wrong command!")
  }

  private def parseNavigatorMessageRequest(instruction: String): Boolean = {
    val pattern = "\\binformation\\b".r
    val res = (pattern findAllIn instruction).mkString(",")
    return res.length > 0
  }

  private def parseNavigatorMessageProposal(instruction: String): String = {
    var result: String = ""
    for (v <- Commands.WORDS) {
      val pattern = ("\\b" + v + "\\b").r
      val res = pattern findFirstIn instruction
      result = res.getOrElse("")
      if (result.length > 0)
        return result
    }
    result
  }

  object Commands {
    val TURN_LEFT = "left"
    val TURN_RIGHT = "right"
    val MOVE_FORWARD = "forward"
    val GRAB = "grab"
    val SHOOT = "shoot"
    val CLIMB = "climb"
    val WORDS = Array("left", "right", "forward", "grab", "shoot", "climb")
  }

}
