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
    val tLow = bitPeriod - tHigh - 1
    while(true) {
      if(System.in.available() != 0){
        val buffer = System.in.read()
        CSb #= false
        sleep(bitPeriod)
        (7 to 0 by -1).foreach{ bitId =>
          CLK #= false
          sleep(1)
          MOSI #= ((buffer >> bitId) & 1) != 0
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

object SpiMasterCodec {
  def waitFalling(pin : Bool){
    waitUntil(pin.toBoolean == true)
    waitUntil(pin.toBoolean == false)
  }
  def waitRising(pin : Bool){
    waitUntil(pin.toBoolean == false)
    waitUntil(pin.toBoolean == true)
  }
  def apply(MOSI : Bool, MISO : Bool, CSb:Bool, CLK: Bool, bitPeriod: Long, color: String = "") = fork{
    CSb #= true
    MOSI #= true
    CLK #= true
    val tHigh = bitPeriod/2
    val tLow = bitPeriod - tHigh - 1
    while(true) {
      if(System.in.available() != 0){
        val buffer = System.in.read()
        var rx=0
        CSb #= false
        sleep(bitPeriod)
        (7 to 0 by -1).foreach{ bitId =>
          CLK #= false
          sleep(1)
          MOSI #= ((buffer >> bitId) & 1) != 0
          sleep(tLow)
          CLK #= true
          if (MISO.toBoolean)
            rx |= 1 << bitId
          sleep(tHigh)
        }
        if ((rx.toChar != '\r') && (rx.toChar != 0)) {
          //println("Spi char %02x".format(rx));println(rx.toChar)
          print(color)
          print(rx.toChar)
        }
        CSb #= true
        MOSI #= true
        sleep(bitPeriod)
      } else {
        sleep(10*bitPeriod)
        CSb #= false
        MOSI #= false
        sleep(bitPeriod)
        var buffer = 0
        (7 to 0 by -1).foreach { bitId =>
          CLK #= false
          sleep(tLow)
          CLK #= true
          if (MISO.toBoolean)
            buffer |= 1 << bitId
          sleep(tHigh)
        }

        sleep(bitPeriod)
        if ((buffer.toChar != '\r') && (buffer.toChar != 0)) {
          //println("Spi char %02x".format(buffer));println(buffer.toChar)
          print("%s%c\u001B[0m".format(color,buffer.toChar))
        }
        CSb #= true
        MOSI #= true
        sleep(10*bitPeriod)
      }
    }
  }
}

object XspiSlaveEncoder {
  def waitRising(sig : Bool): Unit ={
    waitUntil(sig.toBoolean == false)
    waitUntil(sig.toBoolean == true)
  }
  def waitFalling(sig : Bool): Unit ={
    waitUntil(sig.toBoolean == true)
    waitUntil(sig.toBoolean == false)
  }
  def waitRising(sig : Bits, mask: Long): Unit ={
    waitUntil((sig.toLong & mask) == 0)
    waitUntil((sig.toLong & mask) == mask)
  }
  def waitFalling(sig : Bits, mask: Long): Unit ={
    waitUntil((sig.toLong & mask) == mask)
    waitUntil((sig.toLong & mask) == 0)
  }
  def toLong(sigs: Bits*): Long ={
    var state = 0
    var offset = 0
    sigs.zipWithIndex.foreach { case(x,i) => state += x.toInt<<offset; offset+=x.getWidth }
    state
  }
  def toLong(a: Bool, b: Bits): Long ={
    var state = a.toBigInt.toLong
    state |= b.toLong<<1
    state
  }
  def waitAny(a: Bool, b: Bits, mask: Long): Unit ={
    val states = toLong(a,b) & ((mask<<1) | 1)
    waitUntil((toLong(a,b) & ((mask<<1) | 1)) != states)
  }
  def apply(data : Bits, CLK : Bool, ss : Bits, ssMask: Long, offset: Int, dataWidth: Int) = fork{
    var actualDataWidth = dataWidth
    if(0==actualDataWidth) {
      actualDataWidth = data.getBitsWidth
    }
    sleep(1) //Wait boot signals propagation
    waitUntil((ss.toLong & ssMask) == ssMask)
    println("XspiSlaveEncoder ready, actualDataWidth = %d".format(actualDataWidth))
    while(true) {
      //println("Spi: Wait CSb falling")
      waitFalling(ss, ssMask)
      //println("XspiSlaveEncoder: CSb falling")
      while((ss.toLong & ssMask) == 0) {
        var buffer = 0
        if(System.in.available() != 0) {
          buffer = System.in.read()
          //println("XspiSlaveEncoder: buffer = 0x%02x".format(buffer))
        }
        (7 to 0 by -actualDataWidth).foreach { bitId =>
          data #= (buffer >> (8-actualDataWidth)) << offset
          buffer = (buffer << actualDataWidth) & 0xFF
          if (bitId>=actualDataWidth) {
            waitFalling(CLK)
          } else {
            waitRising(CLK)
          }
        }

        waitAny(CLK, ss, ssMask)
      }
    }
  }
}
