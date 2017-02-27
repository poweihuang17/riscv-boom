//******************************************************************************
// Copyright (c) 2015, The Regents of the University of California (Regents).
// All Rights Reserved. See LICENSE for license details.
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
// RISCV BOOM Utility Functions
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

package boom
{

import chisel3._
import chisel3.util._

import rocket.Instructions._
import rocket._

object IsKilledByBranch
{
   def apply(brinfo: BrResolutionInfo, uop: MicroOp): Bool =
   {
      return (brinfo.valid &&
              brinfo.mispredict &&
              maskMatch(brinfo.mask, uop.br_mask))
   }

   def apply(brinfo: BrResolutionInfo, uop_mask: UInt): Bool =
   {
      return (brinfo.valid &&
              brinfo.mispredict &&
              maskMatch(brinfo.mask, uop_mask))
   }
}

object GetNewBrMask
{
   def apply(brinfo: BrResolutionInfo, uop: MicroOp): UInt =
   {
      return Mux(brinfo.valid, (uop.br_mask & ~brinfo.mask),
                               uop.br_mask)
   }
   def apply(brinfo: BrResolutionInfo, br_mask: UInt): UInt =
   {
      return Mux(brinfo.valid, (br_mask & ~brinfo.mask),
                               br_mask)
   }
}

//do two masks have at least 1 bit match?
object maskMatch
{
   def apply(msk1: UInt, msk2: UInt): Bool = (msk1 & msk2) =/= 0.U
}

//clear one-bit in the Mask as specified by the idx
object clearMaskBit
{
   def apply(msk: UInt, idx: UInt): UInt = (msk & ~(1.U << idx))(msk.getWidth-1, 0)
}

//shift a register over by one bit
object PerformShiftRegister
{
   def apply(reg_val: UInt, new_bit: Bool): UInt =
   {
      reg_val := Cat(reg_val(reg_val.getWidth-1, 0).asUInt(), new_bit.asUInt()).asUInt()
      reg_val
   }
}

// Shift a register over by one bit, wrapping the top bit around to the bottom
// (XOR'ed with a new-bit), and evicting a bit at index HLEN.
// This is used to simulate a longer HLEN-width shift register that is folded
// down to a compressed CLEN.
object PerformCircularShiftRegister
{
   def apply(csr: UInt, new_bit: Bool, evict_bit: Bool, hlen: Int, clen: Int): UInt =
   {
      val carry = csr(clen-1)
      val newval = Cat(csr, new_bit ^ carry) ^ (evict_bit << (hlen % clen).U)
      newval
   }
}

// Increment the input "value", wrapping it if necessary.
object WrapAdd
{
   // "n" is the number of increments, so we wrap at n-1.
   def apply(value: UInt, amt: UInt, n: Int): UInt =
   {
      if (isPow2(n))
      {
         (value + amt)(log2Up(n)-1,0)
      }
      else
      {
         val sum = Cat(0.U(1.W), value) + Cat(0.U(1.W), amt)
         Mux(sum >= n.U,
            sum - n.U,
            sum)
      }
   }
}

// Decrement the input "value", wrapping it if necessary.
object WrapSub
{
   // "n" is the number of increments, so we wrap to n-1.
   def apply(value: UInt, amt: Int, n: Int): UInt =
   {
      if (isPow2(n))
      {
         (value - amt.U)(log2Up(n)-1,0)
      }
      else
      {
         val v = Cat(0.U(1.W), value)
         val b = Cat(0.U(1.W), amt.U)
         Mux(value >= amt.U,
            value - amt.U,
            n.U - (amt.U - value))
      }
   }
}

// Increment the input "value", wrapping it if necessary.
object WrapInc
{
   // "n" is the number of increments, so we wrap at n-1.
   def apply(value: UInt, n: Int): UInt =
   {
      if (isPow2(n))
      {
         (value + 1.U)(log2Up(n)-1,0)
      }
      else
      {
         val wrap = (value === (n-1).U)
         Mux(wrap, 0.U, value + 1.U)
      }
   }
}
// Decrement the input "value", wrapping it if necessary.
object WrapDec
{
   // "n" is the number of increments, so we wrap at n-1.
   def apply(value: UInt, n: Int): UInt =
   {
      if (isPow2(n))
      {
         (value - 1.U)(log2Up(n)-1,0)
      }
      else
      {
         val wrap = (value === 0.U)
         Mux(wrap, (n-1).U, value - 1.U)
      }
   }
}


object RotateL1
{
   def apply(signal: UInt): UInt =
   {
      val w = signal.getWidth
      val out = Cat(signal(w-2,0), signal(w-1))

      return out
   }
}


object Sext
{
   def apply(x: UInt, length: Int): UInt =
   {
      return Cat(Fill(length-x.getWidth, x(x.getWidth-1)), x)
   }
}


// translates from BOOM's special "packed immediate" to a 32b signed immediate
// Asking for U-type gives it shifted up 12 bits.
object ImmGen
{
   def apply(ip: UInt, isel: UInt): SInt =
   {
      val sign = ip(LONGEST_IMM_SZ-1).toSInt
      val i30_20 = Mux(isel === IS_U, ip(18,8).toSInt, sign)
      val i19_12 = Mux(isel === IS_U || isel === IS_J, ip(7,0).toSInt, sign)
      val i11    = Mux(isel === IS_U, 0.S,
                   Mux(isel === IS_J || isel === IS_B, ip(8).toSInt, sign))
      val i10_5  = Mux(isel === IS_U, 0.S, ip(18,14).toSInt)
      val i4_1   = Mux(isel === IS_U, 0.S, ip(13,9).toSInt)
      val i0     = Mux(isel === IS_S || isel === IS_I, ip(8).toSInt, 0.S)

      return Cat(sign, i30_20, i19_12, i11, i10_5, i4_1, i0).toSInt
   }
}

// store the rounding-mode and func type for FP in the packed immediate as well
object ImmGenRm { def apply(ip: UInt): UInt = { return ip(2,0) }}
object ImmGenTyp { def apply(ip: UInt): UInt = { return ip(9,8) }} // only works if !(IS_B or IS_S)

object DebugIsJALR
{
   def apply(inst: UInt): Bool =
   {
      // TODO Chisel not sure why this won't compile
//      val is_jalr = rocket.DecodeLogic(inst, List(false.B),
//                                       Array(
//                                       JALR -> true.B))
      inst(6,0) === ("b1100111").U
   }
}

// take an instruction and output its branch or jal target. Only used for a
// debug assert (no where else would we jump straight from instruction bits to
// a target).
object DebugGetBJImm
{
   def apply(inst: UInt): UInt =
   {
      // TODO Chisel not sure why this won't compile
      //val csignals =
      //rocket.DecodeLogic(inst,
      //                    List(false.B, false.B),
      //      Array(
      //         BEQ     -> List(true.B, false.B),
      //         BNE     -> List(true.B, false.B),
      //         BGE     -> List(true.B, false.B),
      //         BGEU    -> List(true.B, false.B),
      //         BLT     -> List(true.B, false.B),
      //         BLTU    -> List(true.B, false.B)
      //      ))
      //val is_br :: nothing :: Nil = csignals

   val is_br = (inst(6,0) === ("b1100011").U)

   val br_targ = Cat(Fill(12, inst(31)), Fill(8,inst(31)), inst(7), inst(30,25), inst(11,8), 0.U(1.W))
   val jal_targ= Cat(Fill(12, inst(31)), inst(19,12), inst(20), inst(30,25), inst(24,21), 0.U(1.W))

   Mux(is_br, br_targ, jal_targ)
  }
}

object AgePriorityEncoder
{
   def apply(in: Seq[Bool], head: UInt): UInt =
   {
      val n = in.size
      require (isPow2(n))
      val temp_vec = (0 until n).map(i => in(i) && i.U >= head) ++ in
      val idx = PriorityEncoder(temp_vec)
      idx(log2Up(n)-1, 0) //discard msb
   }
}

}
