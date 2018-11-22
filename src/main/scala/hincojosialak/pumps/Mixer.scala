package hincojosialak.pumps

case class Mixer() {
  private var running = false
  private var right = false

  def goLeft = {
    running = true
    right = false
  }

  def goRight = {
    running = true
    right = true
  }

  def off = running = false

  def isRunning = running

  def isRight = running && right

  def isLeft = running && !right
}
