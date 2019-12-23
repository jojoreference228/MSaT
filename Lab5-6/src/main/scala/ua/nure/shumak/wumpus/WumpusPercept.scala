package ua.nure.shumak.wumpus

class WumpusPercept {
  private var stench = false
  private var breeze = false
  private var glitter = false
  private var scream = false

  def setStench: WumpusPercept = {
    stench = true
    this
  }

  def setBreeze: WumpusPercept = {
    breeze = true
    this
  }

  def setGlitter: WumpusPercept = {
    glitter = true
    this
  }


  def setScream: WumpusPercept = {
    scream = true
    this
  }

  def isStench: Boolean = stench

  def isBreeze: Boolean = breeze

  def isGlitter: Boolean = glitter

  def isScream: Boolean = scream

  override def toString: String = {
    val result = new StringBuilder
    if (stench) result.append("There is Stench. ")
    if (breeze) result.append("There is Breeze. ")
    if (glitter) result.append("There is Glitter. ")
    if (scream) result.append("There is Scream. ")
    result.toString
  }
}
