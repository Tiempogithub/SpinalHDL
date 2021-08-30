package spinal.lib.com.spi.sim

import spinal.core.{Bits, Bool}
import spinal.core.sim._

object SpiDecoder {
  def waitRising(sig : Bool): Unit ={
    waitUntil(sig.toBoolean == false)
    waitUntil(sig.toBoolean == true)
  }
  def waitFalling(sig : Bool): Unit ={
    waitUntil(sig.toBoolean == true)
    waitUntil(sig.toBoolean == false)
  }
  def waitAnyBits(sigs : Bits): Unit ={
    val state = sigs.toBigInt
    waitUntil(sigs.toBigInt != state)
  }
  def toLong(sigs: Bool*): Long ={
    var state = 0
    sigs.zipWithIndex.foreach { case(x,i) => if(x.toBoolean) state += 1<<i }
    state
  }
  def waitAny(sigs : Bool*): Unit ={
    val states = toLong(sigs:_*)
    waitUntil(toLong(sigs:_*) != states)
  }
  def apply(MOSI : Bool, CLK : Bool, CSb : Bool, color: String = "") = fork{
    sleep(1) //Wait boot signals propagation
    waitUntil(CSb.toBoolean == true)
    println("SpiDecoder ready")
    while(true) {
      //println("Spi: Wait CSb falling")
      waitFalling(CSb)
      //println("Spi: CSb falling")
      while(! CSb.toBoolean) {

        var buffer = 0
        (7 to 0 by -1).foreach { bitId =>
          waitRising(CLK)
          if (MOSI.toBoolean)
            buffer |= 1 << bitId
        }

      //  if (buffer.toChar != '\r') {
          //println("Spi char %02x".format(buffer));println(buffer.toChar)
      //    print(buffer.toChar)
      //  }
        waitAny(CLK, CSb)
        if (buffer.toChar != '\r') print("%s%c\u001B[0m".format(color,buffer.toChar))
        waitFalling(CLK)

      }
    }
  }
}

object SpiMasterDecoder {
  def waitRising(sig : Bool): Unit ={
    waitUntil(sig.toBoolean == false)
    waitUntil(sig.toBoolean == true)
  }
  def waitFalling(sig : Bool): Unit ={
    waitUntil(sig.toBoolean == true)
    waitUntil(sig.toBoolean == false)
  }
  def waitAnyBits(sigs : Bits): Unit ={
    val state = sigs.toBigInt
    waitUntil(sigs.toBigInt != state)
  }
  def toLong(sigs: Bool*): Long ={
    var state = 0
    sigs.zipWithIndex.foreach { case(x,i) => if(x.toBoolean) state += 1<<i }
    state
  }
  def waitAny(sigs : Bool*): Unit ={
    val states = toLong(sigs:_*)
    waitUntil(toLong(sigs:_*) != states)
  }
  def apply(MISO : Bool, CLK : Bool, CSb : Bool, bitPeriod: Long, color: String = "") = fork{
    sleep(1) //Wait boot signals propagation
    val tHigh = bitPeriod/2
    val tLow = bitPeriod - tHigh - 1
    CSb #= true
    CLK #= true
    sleep(500*bitPeriod)
    CSb #= false
    sleep(5*bitPeriod)

    println("SpiMasterDecoder ready")
    while(true) {
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
      sleep(3*bitPeriod)
    }
  }
}


object XspiDecoder {
  def waitRising(sig : Bool): Unit ={
    waitUntil(sig.toBoolean == false)
    waitUntil(sig.toBoolean == true)
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
  def apply(data : Bits, CLK : Bool, ss : Bits, ssMask: Long, offset: Int, dataWidth: Int, color: String = "") = fork{
    sleep(1) //Wait boot signals propagation
    waitUntil((ss.toLong & ssMask) == ssMask)
    println("SpiDecoder ready")
    val dataMask = (1<<dataWidth)-1
    while(true) {
      //println("Spi: Wait CSb falling")
      waitFalling(ss, ssMask)
      //println("Spi: CSb falling")
      while((ss.toLong & ssMask) == 0) {

        var buffer = 0
        (0 to 7 by dataWidth).foreach { bitId =>
          waitRising(CLK)
          buffer |= ((data.toInt >> offset) & dataMask) << (8-dataWidth-bitId)
        }

        if (buffer.toChar != '\r') {
          //println("Spi char %02x".format(buffer));println(buffer.toChar)
          print("%s%c\u001B[0m".format(color,buffer.toChar))
        }
        waitAny(CLK, ss, ssMask)
      }
    }
  }
}
