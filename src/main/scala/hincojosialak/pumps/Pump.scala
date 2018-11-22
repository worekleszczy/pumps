package hincojosialak.pumps

import java.util.logging.Logger

import scala.concurrent.duration.FiniteDuration

trait Flowable {
  def calculateAmount(throuput: Double, duration: FiniteDuration): Double = {
    throuput / 1000.0 * duration.toMillis
  }
}

case class Valve(val throuput: Double, upstreamSensor: Sensor) extends Simulateable with Flowable {
  private var valveOpen = false

  def open = valveOpen = true

  def close = valveOpen = false

  def isOpen = valveOpen

  override def tick(duration: FiniteDuration): Unit = {
    if (isOpen) {
      val amount = calculateAmount(throuput, duration)
      if (upstreamSensor.getLiquidVolume > 0.0)
        upstreamSensor.sub(amount)
    }
  }
}

case class Pump(val throuput: Double, upstreamSensor: Sensor, downstreamSensor: Sensor)
  extends Simulateable with Flowable {
  private var running = false;

  def on = running = true

  def off = running = false

  def isRunning = running

  def tick(duration: FiniteDuration): Unit = {
    if (isRunning) {
      val amount = calculateAmount(throuput, duration)
      if (upstreamSensor.getLiquidVolume > 0) {
        downstreamSensor.add(amount)
        upstreamSensor.sub(amount)
      }
    }
  }
}
