package ua.nure.shumak.wumpus

import akka.actor.{ActorSystem, Props}

object Main extends App {
  val system = ActorSystem("WumpusWorld")
  val wumpusWorldActor = system.actorOf(Props[WumpusWorldActor], name = "WumpusWorldActor")
  val navigatorActor = system.actorOf(Props[NavigatorActor], name = "NavigatorActor")
  wumpusWorldActor ! "start"
  navigatorActor ! "start"
  val speleologistActor = system.actorOf(Props(classOf[SpeleologistActor], wumpusWorldActor, navigatorActor), name = "SpeleologistActor")
  speleologistActor ! "start"
  wumpusWorldActor ! ActorTaken(speleologistActor)
}
