//******************************************************************************
// Copyright (c) 2015, The Regents of the University of California (Regents).
// All Rights Reserved. See LICENSE for license details.
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
// TAGE U-Bits
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//
// Christopher Celio
// 2016 Nov
//
// Goal:
//    - U-bits provide a "usefulness" metric for each entry in a TAGE predictor.
//    - Only allocate for entries that are "not useful".
//    - Occasionally, degrade entries to prevent unused entries from never leaving.

package boom

import chisel3._
import chisel3.util._


abstract class TageUbitMemory(
   num_entries: Int,
   ubit_sz: Int
   ) extends Module
{
   val index_sz = log2Up(num_entries)
   val io = IO(new Bundle
   {
      // send read addr on cycle 0, get data out on cycle 2.
      val s0_read_idx = Input(UInt(index_sz.W))
      val s2_is_useful = Output(Bool())


      val allocate_valid  = Input(Bool())
      val allocate_idx = Input(UInt(index_sz.W))
      def allocate(idx: UInt) =
      {
         this.allocate_valid := true.B
         this.allocate_idx := idx
      }

      val update_valid  = Input(Bool())
      val update_idx = Input(UInt(index_sz.W))
      val update_inc = Input(Bool())
      def update(idx: UInt, inc: Bool) =
      {
         this.update_valid  := true.B
         this.update_idx := idx
         this.update_inc := inc
      }

      val degrade_valid = Input(Bool())
      def degrade(dummy: Int=0) =
      {
         this.degrade_valid := true.B
      }

      // Degrading may take many cycles. Tell the tage-table if we are degrading.
      val is_degrading = Output(Bool())
      def areDegrading(dummy: Int=0) =
      {
         this.is_degrading
      }

      def InitializeIo(dummy: Int=0) =
      {
         this.allocate_valid := false.B
         this.allocate_idx := UInt(0)
         this.update_valid := false.B
         this.update_idx := UInt(0)
         this.update_inc := false.B
         this.degrade_valid := false.B
         this.is_degrading := false.B
      }
   })

   val UBIT_MAX = (1 << ubit_sz) - 1
   val UBIT_INIT = 1
   require(ubit_sz < 4) // What are you doing? You're wasting bits!
   assert(!(io.allocate_valid && io.update_valid), "[ubits] trying to update and allocate simultaneously.")

   when (io.update_valid)
   {
      assert (io.update_idx === RegNext(RegNext(io.s0_read_idx)),
         "[ubits] update index not matching what was used for reading.")
   }

}

//--------------------------------------------------------------------------
//--------------------------------------------------------------------------
// This version implements the u-bits in sequential memory -- can be placed into SRAM.
// However, degrading the u-bits takes many cycles.
class TageUbitMemorySeqMem(
   num_entries: Int,
   ubit_sz: Int
   ) extends TageUbitMemory(num_entries, ubit_sz)
{
   //------------------------------------------------------------

   val ubit_table       = SeqMem(num_entries, UInt(width = ubit_sz))

   // maintain an async copy purely for assertions
   val debug_ubit_table = Mem(num_entries, UInt(width = ubit_sz))
   val debug_valids     = Reg(init=Vec.fill(num_entries){false.B})

   //------------------------------------------------------------
   // Manage clearing u-bits over time
   // (to prevent entries from never leaving the predictor).

   // TODO

   //------------------------------------------------------------

   val s1_read_idx = RegNext(io.s0_read_idx)
   val s2_bypass_useful =
      RegNext(
         (io.allocate_valid && io.allocate_idx === s1_read_idx) ||
         (io.update_valid && io.update_inc && io.update_idx === s1_read_idx))


   // TODO add a read_enable (only reads on commit.valid within TAGE).
   // But must add assert that allocate/update is always following a Reg(read-enable).
   val s2_out = RegNext(ubit_table.read(io.s0_read_idx, true.B))
   io.s2_is_useful := s2_out =/= UInt(0) || s2_bypass_useful

   // TODO: missing the bypassing in cycle1.
//   when (RegNext(RegNext(debug_valids(io.s0_read_idx))))
//   {
//      assert (s2_out === RegNext(RegNext(debug_ubit_table(io.s0_read_idx))),
//         "[ubits] value doesn't match debug copy.")
//   }

   //------------------------------------------------------------
   // Compute update values.
   // We're performing a read-modify-write here. However, b/c SRAM,
   // the read was performed on Cycle s1, so we need to bypass any
   // updates that only became visible on Cycle s2.

   // bypass updates from the previous cycle.
   val bypass_alloc = RegNext(io.allocate_valid && io.allocate_idx === s1_read_idx)
   val bypass_upinc = RegNext(io.update_valid && io.update_inc && io.update_idx === s1_read_idx)
   val bypass_updec = RegNext(io.update_valid && !io.update_inc && io.update_idx === s1_read_idx)

   val u = Wire(UInt(width=ubit_sz+1))
   u :=
      Mux(bypass_alloc,
         UInt(UBIT_INIT),
      Mux(bypass_upinc && s2_out =/= UInt(UBIT_MAX),
         s2_out + UInt(1),
      Mux(bypass_updec && s2_out =/= UInt(0),
         s2_out - UInt(1),
         s2_out)))

   assert (!(bypass_alloc && s2_out =/= UInt(0)), "[ubit] allocation occurred but s2_out wasn't zero")
   assert (!(u >> ubit_sz), "[ubit] next value logic wrapped around.")

   val inc = io.update_inc
   val next_u =
      Mux(inc && u < UInt(UBIT_MAX),
         u + UInt(1),
      Mux(!inc && u > UInt(0),
         u - UInt(1),
         u))


   val wen = io.allocate_valid || io.update_valid
   val wdata = Mux(io.allocate_valid, UInt(UBIT_INIT), next_u)
   val waddr = Mux(io.allocate_valid, io.allocate_idx, RegNext(RegNext(io.s0_read_idx)))

   when (wen)
   {
      ubit_table(waddr) := wdata
      debug_ubit_table(waddr) := wdata
   }

   //------------------------------------------------------------

   when (io.allocate_valid)
   {
      debug_valids(io.allocate_idx) := true.B
   }

   when (io.update_valid)
   {
      assert (!io.allocate_valid, "[ubits] trying to allocate.")
      assert (io.update_idx === RegNext(RegNext(io.s0_read_idx)), "[ubits] update index not matching what was used for reading.")
   }

   val r_debug_allocate_value = RegNext(RegNext(debug_ubit_table(io.allocate_idx)))
   when (RegNext(RegNext(io.allocate_valid && debug_valids(io.allocate_idx))))
   {
      assert(r_debug_allocate_value === UInt(0), "[ubits] Tried to allocate a useful entry")
   }
}

//--------------------------------------------------------------------------
//--------------------------------------------------------------------------
// This version implements the u-bits in a sinlge register --
// allows for single-cycle reset.
class TageUbitMemoryFlipFlop(
   num_entries: Int,
   ubit_sz: Int=1
   ) extends TageUbitMemory(num_entries, ubit_sz)
{

   //------------------------------------------------------------

   require (ubit_sz == 1)
   // TODO implement each bit as its own Reg.
   val ubit_table       = Reg(UInt(width=num_entries))

   val debug_ubit_table = Mem(num_entries, UInt(width = ubit_sz))
   val debug_valids     = Reg(init=Vec.fill(num_entries){false.B})

   //------------------------------------------------------------

   val s1_read_idx = RegNext(io.s0_read_idx)
   val s2_read_idx = RegNext(s1_read_idx)
   val s2_out = ubit_table(s2_read_idx)
   io.s2_is_useful := s2_out =/= UInt(0)

   when (debug_valids(s2_read_idx))
   {
      assert(s2_out === debug_ubit_table(s2_read_idx), "[ubits] mismatch on output.")
   }

   //------------------------------------------------------------
   // Compute update values.

   val u = s2_out

   val inc = io.update_inc
   val next_u =
      Mux(inc && u < UInt(UBIT_MAX),
         u + UInt(1),
      Mux(!inc && u > UInt(0),
         u - UInt(1),
         u))


   val wen = io.allocate_valid || io.update_valid
   val wdata = Mux(io.allocate_valid, UInt(UBIT_INIT), next_u)
   val waddr = Mux(io.allocate_valid, io.allocate_idx, RegNext(RegNext(io.s0_read_idx)))

   when (io.degrade_valid)
   {
      ubit_table := UInt(0)
   }
   .elsewhen (wen)
   {
      ubit_table := ubit_table.bitSet(waddr, wdata.toBool)
      debug_ubit_table(waddr) := wdata
      debug_valids(waddr) := true.B
   }
   require (ubit_sz == 1)
   require (wdata.getWidth == 1)

   //------------------------------------------------------------

   when (io.allocate_valid)
   {
      assert(ubit_table(io.allocate_idx) === UInt(0), "[ubits] Tried to allocate a useful entry")
   }
}

