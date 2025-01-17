package spinal.lib.misc.plic

import spinal.core._
import spinal.lib._
import spinal.lib.bus.misc.{BusSlaveFactory, AllMapping, SingleMapping}
import scala.collection.Seq

/*
The PLIC is the Platform Level Interrupt controller as defined by RISCV:
https://github.com/riscv/riscv-plic-spec/blob/master/riscv-plic.adoc

the spinal doc for this can be found at https://spinalhdl.github.io/SpinalDoc-RTD/SpinalHDL/Libraries/Misc/PLIC/plic_mapper.html

 */
case class PlicMapping(
  gatewayPriorityOffset : Int,
  gatewayPendingOffset  : Int,
  targetEnableOffset    : Int,
  targetThresholdOffset : Int,
  targetClaimOffset     : Int,
  gatewayPriorityShift  : Int,
  targetThresholdShift  : Int,
  targetClaimShift      : Int,
  targetEnableShift     : Int,
  gatewayPriorityWriteGen : Boolean = true,
  gatewayPriorityReadGen : Boolean,
  gatewayPendingReadGen : Boolean,
  gatewayPendingClearGen : Boolean = false,
  gatewayPendingSetGen : Boolean = false,
  targetThresholdWriteGen : Boolean = true,
  targetThresholdReadGen : Boolean,
  targetEnableWriteGen : Boolean = true,
  targetEnableReadGen : Boolean
)

object PlicMapping{
  // Follows the SiFive PLIC mapping (eg. https://sifive.cdn.prismic.io/sifive/9169d157-0d50-4005-a289-36c684de671b_e31_core_complex_manual_21G1.pdf)
  // basically a full fledged PLIC
  def sifive = PlicMapping(
    gatewayPriorityOffset =   0x0000,
    gatewayPendingOffset  =   0x1000,
    targetEnableOffset    =   0x2000,
    targetThresholdOffset = 0x200000,
    targetClaimOffset     = 0x200004,
    gatewayPriorityShift  =       2,
    targetThresholdShift  =      12,
    targetClaimShift      =      12,
    targetEnableShift     =       7,
    gatewayPriorityReadGen = true,
    gatewayPendingReadGen = true,
    targetThresholdReadGen = true,
    targetEnableReadGen = true
  )

  def tiempo = PlicMapping(
    gatewayPriorityOffset =   0x0000,
    gatewayPendingOffset  =   0x1000,
    targetEnableOffset    =   0x2000,
    targetThresholdOffset = 0x200000,
    targetClaimOffset     = 0x200004,
    gatewayPriorityShift  =       2,
    targetThresholdShift  =      12,
    targetClaimShift      =      12,
    targetEnableShift     =       7,
    gatewayPriorityReadGen = true,
    gatewayPendingReadGen = true,
    gatewayPendingClearGen = true,
    gatewayPendingSetGen = true,
    targetThresholdReadGen = true,
    targetEnableReadGen = true
  )

  // this mapping generates a lighter PLIC, at the cost of some missing optional features:
  // - no reading the intrerrupt's priority
  // - no reading the interrupts's pending bit (must use the claim/complete mechanism)
  // - no reading the target's threshold
  // the rest of the functionality is generated
  def light = PlicMapping(
    gatewayPriorityOffset =  0x0000,
    gatewayPendingOffset  =  0x1000,
    targetEnableOffset    =  0x2000,
    targetThresholdOffset =  0xF000,
    targetClaimOffset     =  0xF004,
    gatewayPriorityShift  =       2,
    targetThresholdShift  =      12,
    targetClaimShift      =      12,
    targetEnableShift     =       7,
    gatewayPriorityReadGen = false,
    gatewayPendingReadGen = false,
    targetThresholdReadGen = false,
    targetEnableReadGen = true
  )
}


object PlicMapper{
  // args for PlicMapper:
  // bus: bus to which this ctrl is attached
  // mapping: a mapping configuration (see above)
  // gateways: a sequence of PlicGateway (interrupt sources) to generate the bus access control
  // targets: the sequence of PlicTargets (eg. multiple cores) to generate the bus access control
  def apply(bus: BusSlaveFactory, mapping: PlicMapping)(gateways : Seq[PlicGateway], targets : Seq[PlicTarget]) = new Area{
    import mapping._
    // for each gateway, generate priority register & pending bit as needed
    val gatewayMapping = for(gateway <- gateways) yield new Area{
      if(gatewayPriorityWriteGen && !gateway.priority.hasAssignement) bus.drive(gateway.priority, address = gatewayPriorityOffset + (gateway.id << gatewayPriorityShift), documentation = s"Driving priority for gateway ${gateway.getName()}. Inits to 0 (interrupt is disabled)" ) init(0)
      if(gatewayPriorityReadGen) bus.read(gateway.priority, address = gatewayPriorityOffset + (gateway.id << gatewayPriorityShift), documentation = s"Read priority for gateway ${gateway.getName()}")

      val bitOffset = gateway.id % bus.busDataWidth
      val writeData = Bool()
      bus.nonStopWrite(writeData,bitOffset)

      val pendingReadAddress = gatewayPendingOffset + (gateway.id/bus.busDataWidth)*bus.busDataWidth/8
      val pendingSetAddress = pendingReadAddress+0x800
      if(gatewayPendingReadGen) bus.read(gateway.ip, address = pendingReadAddress, bitOffset = bitOffset, documentation = s"Read Pending bit for gateway " + gateway.getName())

      if (gatewayPendingSetGen) {
        /*
        //SCOPE VIOLATION : (toplevel/when_PlicMapper_l120 :  Bool) is assigned outside its declaration scope at
        when(writeData) {
          when(bus.isWriting(pendingSetAddress)) {
            gateway.doSwWrite(True)
          }
        }*/
        //OK
        when(bus.isWriting(pendingSetAddress)) {
          when(writeData) {
            gateway.doSwWrite(True)
          }
        }
      }
      if (gatewayPendingClearGen) {
        when(bus.isWriting(pendingReadAddress)) {
          when(writeData) {
            gateway.doSwWrite(False)
          }
        }
      }

    }

    // claim/complete logic
    val idWidth = log2Up((gateways.map(_.id) ++ Seq(0)).max + 1)
    val claim = Flow(UInt(idWidth bits))
    claim.valid := False
    claim.payload.assignDontCare()
    when(claim.valid) {
      switch(claim.payload) {
        for (gateway <- gateways) {
          is(gateway.id) {
            gateway.doClaim()
          }
        }
      }
    }

    val completion = Flow(UInt(idWidth bits))
    completion.valid := False
    completion.payload.assignDontCare()
    when(completion.valid) {
      switch(completion.payload) {
        for (gateway <- gateways) {
          is(gateway.id) {
            gateway.doCompletion()
          }
        }
      }
    }


    val coherencyStall = Counter(2)
    when(coherencyStall =/= 0){
      bus.readHalt()
      coherencyStall.increment()
    }
    bus.onReadPrimitive(AllMapping, haltSensitive = false, documentation = ""){
      coherencyStall.increment()
    }
    bus.onWritePrimitive(AllMapping, haltSensitive = false, documentation = ""){
      coherencyStall.increment()
    }

    // for each target/context, generate threshold and claim/complete registers
    val targetMapping = for((target, targetId) <- targets.zipWithIndex) yield new Area {
      val thresholdOffset = targetThresholdOffset + (targetId << targetThresholdShift)
      val claimOffset = targetClaimOffset + (targetId << targetClaimShift)
      if(targetThresholdWriteGen && !target.threshold.hasAssignement) bus.drive(target.threshold, address = thresholdOffset, documentation = s"Drive target threshold for target ${targetId}. inits to 0") init (0)
      if(targetThresholdReadGen) bus.read(target.threshold, address = thresholdOffset, documentation = s"Read target threshold for target ${targetId}")
      bus.read(target.claim, address = claimOffset, documentation = s"Read target claim for target ${targetId} ")
      bus.onRead(claimOffset) {
        claim.valid := True
        claim.payload := target.claim
      }




      val targetCompletion = bus.createAndDriveFlow(UInt(target.idWidth bits), claimOffset)
      when(targetCompletion.valid){
        completion.valid := True
        completion.payload := targetCompletion.payload
      }
      // for each gateway/interrupt source, generate the enable bits for each target/context
      for ((gateway, gatewayIndex) <- gateways.zipWithIndex) {
        val address = targetEnableOffset + (targetId << targetEnableShift) + bus.busDataWidth/8 * (gateway.id / bus.busDataWidth)
        val bitOffset = gateway.id % bus.busDataWidth
        if(targetEnableWriteGen && !target.ie(gatewayIndex).hasAssignement) bus.drive(target.ie(gatewayIndex), address, bitOffset, documentation = s"Drive target enable for gateway ${gatewayIndex} for target ${targetId}. inits to 0b0") init(False)
        if(targetEnableReadGen)  bus.read(target.ie(gatewayIndex),  address, bitOffset, documentation = s"Read target enable for gateway ${gatewayIndex} for target ${targetId}.")
      }
    }
  }
}
