package hincojosialak.pumps

trait Sensor {
  def getLiquidVolume: Double

  def add(value: Double): Unit

  def sub(value: Double): Unit
}

case class TankSensor(levels: Int, containerCapacity: Double) extends Sensor {
  private val amountPerLevel = containerCapacity / levels.toDouble
  private var liquidVolume: Double = 0.0

  def currentLevel: Int = math.ceil(liquidVolume / amountPerLevel).toInt

  override def getLiquidVolume: Double = liquidVolume

  override def add(value: Double): Unit = liquidVolume += value

  override def sub(value: Double): Unit = {
    liquidVolume -= value
    if(liquidVolume < 0.0)
      liquidVolume = 0.0
  }
}
