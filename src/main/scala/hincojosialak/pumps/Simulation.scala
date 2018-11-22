package hincojosialak.pumps

import scala.concurrent.duration._

trait Simulateable {

  def tick(duration: FiniteDuration)
}

object States extends Enumeration {
  type State = Value
  val EMPTYING, FILLING_DRINK, FILLING_SODA, FILLING_VODKA, MIXING = Value
}

class Algorithm(vodkaPercent: Double, vodkaSensor: TankSensor,
                sodaSensor: TankSensor, drinkSensor: TankSensor,
                sodaPump: Pump, vodkaPump: Pump,
                drinkValve: Valve, infSodaPump: Pump, infVodkaPump: Pump, mixer: Mixer) {

  import States._

  private var state: State = EMPTYING

  private var mixerTimer: FiniteDuration = 0 seconds

  val (targetVodkaFill, targetSodaFill) = minLevels(vodkaPercent,
    vodkaSensor.levels, sodaSensor.levels, drinkSensor.levels)

  def preStart(): Unit = {

    if (vodkaPercent > 1.0 || vodkaPercent < 0.0) {
      throw new IllegalArgumentException(s"ratio ${vodkaPercent} not allowed")
    }

    sodaPump.off
    vodkaPump.off
    drinkValve.open
    infSodaPump.on
    infVodkaPump.on
    mixer.off
    logSensors()
    logExecutors()
  }

  def step(offset: FiniteDuration): Unit = {
    logSensors()
    logExecutors()

    state match {
      case EMPTYING => {
        if (drinkEmpty()) {
          drinkValve.close
          if (sodaSensor.currentLevel < targetSodaFill) {
            infSodaPump.on
            this.state = FILLING_SODA
          } else if (vodkaSensor.currentLevel < targetVodkaFill) {
            infVodkaPump.on
            this.state = FILLING_VODKA
          } else {
            sodaPump.on
            vodkaPump.on
            this.state = FILLING_DRINK
          }
        }
      }
      case FILLING_SODA => {
        if(sodaSensor.currentLevel >= targetSodaFill) {
          infSodaPump.off
          if(vodkaSensor.currentLevel < targetVodkaFill) {
            infVodkaPump.on
            this.state = FILLING_VODKA
          } else {
            this.state = FILLING_DRINK
          }
        }
      }
      case FILLING_VODKA => {
        if(vodkaSensor.currentLevel >= targetVodkaFill) {
          infVodkaPump.off
          this.state = FILLING_DRINK
        }
      }
      case FILLING_DRINK => {
        stopPumpIfEmpty(vodkaSensor, vodkaPump)
        stopPumpIfEmpty(sodaSensor, sodaPump)
        if(sodaSensor.currentLevel <= 0 && vodkaSensor.currentLevel <= 0) {
          mixerTimer = 5.seconds
          this.state = MIXING
          mixer.goLeft
        }
      }
      case MIXING => {
        mixerTimer -= offset
        if(mixerTimer <= Duration.Zero) {
          mixerTimer = 5.seconds
          if(mixer.isLeft) {
            mixer.off
          } else if(!mixer.isRunning) {
            mixer.goRight
          } else {
            mixer.off
            this.state = EMPTYING
          }
        }
      }
    }
  }

  //method called roughly every 5 seconds to toggle mixer
  //  private def toggleMixer(): Unit = {
  //    if(toggleMixerTimer < Duration.Zero) {
  //      toggleMixerTimer = 5 seconds
  //      if(mixer.isRunning){
  //        mixer.off
  //      } else {
  //        mixer.on
  //      }
  //    }
  //  }

  //algorithm assumes that level in every tank has same volume
  def minLevels(vodkaRatio: Double, maxVodkaLevel: Int, maxSodaLevel: Int, maxDrinkLevel: Int): (Int, Int) = {
    val tankSum = maxVodkaLevel + maxSodaLevel
    val sumMax = if (tankSum < maxDrinkLevel) {
      tankSum
    }
    else {
      maxDrinkLevel
    }

    val resVodka = math.min(sumMax * vodkaRatio, math.min(maxSodaLevel * vodkaRatio, maxVodkaLevel)).toInt
    val sodaParts = (1 / vodkaRatio) - 1.0
    val resSoda = (resVodka * sodaParts).toInt
    (resVodka, resSoda)
  }

  private def stopPumpIfEmpty(sensor: TankSensor, pump: Pump): Unit = {
    if (sensor.currentLevel <= 0.0) {
      pump.off
    }
  }

  private def drinkEmpty(): Boolean = drinkSensor.currentLevel == 0

  def logSensors(): Unit = {
    println("Sensors")
    println(s"soda: ${sodaSensor.getLiquidVolume}, vodka: ${vodkaSensor.getLiquidVolume}, drink: ${drinkSensor.getLiquidVolume}")
  }

  def logExecutors(): Unit = {
    println("Executors")
    println(s"Soda: ${sodaPump.isRunning}, vodka: ${vodkaPump.isRunning}, drink: ${drinkValve.isOpen}")
  }

}

class Simulation(val timeTick: FiniteDuration) {

  val sodaTankCapacity = 1000.0
  val sodaTankSensor = TankSensor(10, sodaTankCapacity)
  val soda: Tank = Tank(sodaTankCapacity, sodaTankSensor)

  val vodkaTankCapacity = 1000.0
  val vodkaTankSensor = TankSensor(10, vodkaTankCapacity)
  val vodka: Tank = Tank(vodkaTankCapacity, vodkaTankSensor)

  val drinkTankCapacity = 1300.0
  val drinkTankSensor = TankSensor(15, drinkTankCapacity)
  val drink: Tank = Tank(drinkTankCapacity, drinkTankSensor)


  val infTankSensor = new Sensor {
    override def getLiquidVolume: Double = Double.MaxValue

    override def add(value: Double): Unit = ???

    override def sub(value: Double): Unit = {}
  }

  val infSodaToSodaPump = Pump(10.0, infTankSensor, sodaTankSensor)
  val infVodkaToVodkaPump = Pump(10.0, infTankSensor, vodkaTankSensor)
  val vodkaToDrink = Pump(10.0, vodkaTankSensor, drinkTankSensor)
  val sodaToDrink = Pump(10.0, sodaTankSensor, drinkTankSensor)

  def run(): Unit = ???
}

object Simulation extends App {

}
