package simulations

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class CircuitSuite extends CircuitSimulator with FunSuite {
  val InverterDelay = 1
  val AndGateDelay = 3
  val OrGateDelay = 5
  
  test("andGate example") {
    val in1, in2, out = new Wire
    andGate(in1, in2, out)
    in1.setSignal(false)
    in2.setSignal(false)
    run
    
    assert(out.getSignal === false, "and 1")

    in1.setSignal(true)
    run
    
    assert(out.getSignal === false, "and 2")

    in2.setSignal(true)
    run
    
    assert(out.getSignal === true, "and 3")
  }

  test("orGate example") {
    val in1, in2, out = new Wire
    orGate(in1, in2, out)
    in1.setSignal(false)
    in2.setSignal(false)
    run
    
    assert(out.getSignal === false, "or 1")

    in1.setSignal(true)
    run
    
    assert(out.getSignal === true, "or2 2")

    in2.setSignal(true)
    in1.setSignal(false)
    run
    
    assert(out.getSignal === true, "or3 3")
  }

  test("orGate2 example") {
    val in1, in2, out = new Wire
    orGate2(in1, in2, out)
    in1.setSignal(false)
    in2.setSignal(false)
    run
    
    assert(out.getSignal === false, "or 1")

    in1.setSignal(true)
    run
    
    assert(out.getSignal === true, "or2 2")

    in2.setSignal(true)
    in1.setSignal(false)
    run
    
    assert(out.getSignal === true, "or3 3")
  }

  test("demux-0") {
    val in, out = new Wire
    demux(in, Nil, List(out))
    in.setSignal(true)
    run
    assert(out.getSignal === true)
    in.setSignal(false)
    run
    assert(out.getSignal === false)
  }


  test("demux-1") {
    val in, s, out0, out1 = new Wire
    demux(in, List(s), List(out0, out1))
    in.setSignal(true)
    s.setSignal(true)
    run
    assert(out0.getSignal === true)
    in.setSignal(false)
    run
    assert(out0.getSignal === false)
    s.setSignal(false)
    run
    assert(out1.getSignal === false)
    in.setSignal(true)
    run
    assert(out1.getSignal === true)
  }



  //
  // to complete with tests for orGate, demux, ...
  //

}
