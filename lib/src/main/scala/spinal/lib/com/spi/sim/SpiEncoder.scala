package spinal.lib.com.spi.sim

import spinal.core.sim._
import spinal.core._
import spinal.sim._

object SpiMasterEncoder {
  def waitFalling(pin : Bool){
    waitUntil(pin.toBoolean == true)
    waitUntil(pin.toBoolean == false)
  }
  def waitRising(pin : Bool){
    waitUntil(pin.toBoolean == false)
    waitUntil(pin.toBoolean == true)
  }
  def apply(MOSI : Bool, CSb:Bool, CLK: Bool, bitPeriod: Long) = fork{
    CSb #= true
    MOSI #= true
    CLK #= true
    val tHigh = bitPeriod/2
    val tLow = bitPeriod - tHigh
    while(true) {
      if(System.in.available() != 0){
        val buffer = System.in.read()
        CSb #= false
        sleep(bitPeriod)
        (7 to 0 by -1).foreach{ bitId =>
          MOSI #= ((buffer >> bitId) & 1) != 0
          CLK #= false
          sleep(tLow)
          CLK #= true
          sleep(tHigh)
        }
        CSb #= true
        MOSI #= true
        sleep(bitPeriod)
      } else {
        sleep(1000*bitPeriod)
      }
    }
  }
}
