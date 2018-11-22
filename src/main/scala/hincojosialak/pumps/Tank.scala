package hincojosialak.pumps

trait Container {
  //can accept fluid for next simulation tick
  def canReceive: Boolean

  //can produce fluid for next simulation tick
  def canProduce: Boolean
}

case object InfiniteStream extends Container {
  override def canReceive: Boolean = false

  override def canProduce: Boolean = true
}

case class Tank(maxVolume: Double, sensor: TankSensor)


