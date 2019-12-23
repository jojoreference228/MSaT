package ua.nure.shumak.wumpus

import akka.actor.{Actor, ActorLogging, PoisonPill}

class NavigatorActor extends Actor with ActorLogging{

  def receive = {
    case "start" =>
      log.info(s"Navigator: Hello! The navigator agent ${self.path} is ready.")

    case "takeDown" =>
      log.info("Navigator: The navigator agent ${self.path} terminating.")
      self ! PoisonPill

    case Message(act, typeMes, mes) => typeMes match {
      case Constants.ADVICE_PROPOSAL =>
        if (parseSpeleologistMessageRequest(typeMes)) {
          log.info(Constants.INFORMATION_PROPOSAL_NAVIGATOR)
          act ! Message(self, Constants.INFORMATION_PROPOSAL_NAVIGATOR, "")
        }

      case Constants.INFORMATION_PROPOSAL_SPELEOLOGIST =>
        if (parseSpeleologistMessageProposal(typeMes)) {
          var advice = getAdvice(mes)
        act ! Message(self, Constants.ADVICE_TYPE, advice)
          log.info(advice)
      }

      case _ => log.info("Wrong command!")
      }

    case _ => log.info("Wrong command!")
  }

  private def parseSpeleologistMessageRequest(instruction: String): Boolean = {
    val pattern = "\\bAdvice\\b".r
    val res = (pattern findAllIn instruction).mkString(",")
    return res.length > 0
  }

  private def parseSpeleologistMessageProposal(instruction: String): Boolean = {
    val pattern = "\\bGiving\\b".r
    val res = (pattern findAllIn instruction).mkString(",")
    return res.length > 0
  }

  private def getAdvice(content: String) = {
    var stench = false
    var breeze = false
    var glitter = false
    var scream = false
    var advicedAction = ""

    STATES foreach (x => {
      val pattern = ("\\b" + x + "\\b").r
      val res = (pattern findFirstIn  content).getOrElse("")
      res match {
        case "Stench" =>
          stench = true
        case "Breeze" =>
          breeze = true
        case "Glitter" =>
          glitter = true
        case "Scream" =>
          scream = true
        case _ =>
      }
    })


     time match {
      case 0 =>
        advicedAction = Constants.MESSAGE_FORWARD
        time += 1
      case 1 =>
        advicedAction = Constants.MESSAGE_RIGHT
        time += 1
      case 2 =>
        advicedAction = Constants.MESSAGE_FORWARD
        time += 1
      case 3 =>
        advicedAction = Constants.MESSAGE_LEFT
        time += 1
      case 4 =>
        advicedAction = Constants.MESSAGE_FORWARD
        time += 1
      case 5 =>
        advicedAction = Constants.MESSAGE_LEFT
        time += 1
      case 6 =>
        advicedAction = Constants.MESSAGE_FORWARD
        time += 1
    }

    val rand = 1 + (Math.random * 3).toInt
    rand match {
      case 1 =>
        Constants.ACTION_PROPOSAL1 + advicedAction
      case 2 =>
        Constants.ACTION_PROPOSAL2 + advicedAction
      case 3 =>
        Constants.ACTION_PROPOSAL3 + advicedAction
      case _ =>
        ""
    }
  }

  var time = 0
  var STATES = Array("Stench", "Breeze", "Glitter", "Scream")

}
