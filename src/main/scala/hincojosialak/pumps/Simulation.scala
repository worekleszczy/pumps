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

    println(s"targetVF: ${targetVodkaFill}, targetSF: ${targetSodaFill}")

    sodaPump.off
    vodkaPump.off
    drinkValve.open
    infSodaPump.off
    infVodkaPump.off
    mixer.off
    logSensors()
    logExecutors()
  }

  def step(offset: FiniteDuration): Unit = {
    logSensors()
    logExecutors()
    println(s"STATE: ${this.state}")
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
          vodkaPump.on
          sodaPump.on
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
            drinkValve.open
          }
        }
      }
    }
  }

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
    println(s"sodaL: ${sodaSensor.currentLevel}, vodkaL: ${vodkaSensor.currentLevel}, drinkL: ${drinkSensor.currentLevel}")
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

  val drinkTankCapacity = 2000.0
  val drinkTankSensor = TankSensor(20, drinkTankCapacity)
  val drink: Tank = Tank(drinkTankCapacity, drinkTankSensor)

  val mixer = Mixer()


  val infTankSensor = new Sensor {
    override def getLiquidVolume: Double = Double.MaxValue

    override def add(value: Double): Unit = ???

    override def sub(value: Double): Unit = {}
  }

  val infSodaToSodaPump = Pump(100.0, infTankSensor, sodaTankSensor)
  val infVodkaToVodkaPump = Pump(100.0, infTankSensor, vodkaTankSensor)
  val vodkaToDrink = Pump(100.0, vodkaTankSensor, drinkTankSensor)
  val sodaToDrink = Pump(100.0, sodaTankSensor, drinkTankSensor)
  val drinkValve = Valve(100.0, drinkTankSensor )

  def run(): Unit = {
    val elems: List[Simulateable] = List(infSodaToSodaPump, infVodkaToVodkaPump, vodkaToDrink, sodaToDrink, drinkValve)
    val algorithm = new Algorithm(0.2, vodkaTankSensor, sodaTankSensor, drinkTankSensor, sodaToDrink,
      vodkaToDrink,drinkValve, infSodaToSodaPump, infVodkaToVodkaPump, mixer)
    algorithm.preStart()
    Stream
      .continually(timeTick)
      .take(1000)
      .foreach(v => {
        Thread.sleep(1000)
        elems.foreach(_.tick(v))
        algorithm.step(v)
      })
  }
}

object Simulation extends App {

  new Simulation(2000 millis).run()
}
