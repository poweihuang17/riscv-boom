//******************************************************************************
// Copyright (c) 2015, The Regents of the University of California (Regents).
// All Rights Reserved. See LICENSE for license details.
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
// RISCV Processor Issue Logic
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

package boom
{
import chisel3._
import chisel3.util._
import cde.Parameters

import FUConstants._
import _root_.util.Str


//-------------------------------------------------------------
//-------------------------------------------------------------

class IssueUnitIO(issue_width: Int, num_wakeup_ports: Int)(implicit p: Parameters) extends BoomBundle()(p)
{
   val dis_valids     = Input(Vec(DISPATCH_WIDTH, Bool()))
   val dis_uops       = Input(Vec(DISPATCH_WIDTH, new MicroOp()))
   val dis_readys     = Output(Vec(DISPATCH_WIDTH, Bool()))

   val iss_valids     = Output(Vec(issue_width, Bool()))
   val iss_uops       = Output(Vec(issue_width, new MicroOp()))
   val wakeup_pdsts   = Input(Vec(num_wakeup_ports, Valid(UInt(PREG_SZ.W))))

   // tell the issue unit what each execution pipeline has in terms of functional units
   val fu_types       = Input(Vec(issue_width, Bits(FUC_SZ.W)))

   val brinfo         = Input(new BrResolutionInfo())
   val flush_pipeline = Input(Bool())

   val tsc_reg        = Input(UInt(xLen.W))
}

abstract class IssueUnit(num_issue_slots: Int, issue_width: Int, num_wakeup_ports: Int)(implicit p: Parameters)
   extends BoomModule()(p)
{
   val io = IO(new IssueUnitIO(issue_width, num_wakeup_ports))

   //-------------------------------------------------------------
   // Set up the dispatch uops
   // special case "storing" 2 uops within one issue slot.

   val dis_uops = Array.fill(DISPATCH_WIDTH) {Wire(new MicroOp())}
   for (w <- 0 until DISPATCH_WIDTH)
   {
      dis_uops(w) := io.dis_uops(w)
      dis_uops(w).iw_state := s_valid_1
      when (dis_uops(w).uopc === uopSTA || dis_uops(w).uopc === uopAMO_AG)
      {
         dis_uops(w).iw_state := s_valid_2
      }
   }

   //-------------------------------------------------------------
   // Issue Table

   val issue_slots = Seq.fill(num_issue_slots)(Module(new IssueSlot(num_wakeup_ports)))

   //-------------------------------------------------------------

   assert (PopCount(issue_slots.map(s => s.io.grant)) <= issue_width.U, "Issue window giving out too many grants.")

   //-------------------------------------------------------------

   if (O3PIPEVIEW_PRINTF)
   {
      for (i <- 0 until ISSUE_WIDTH)
      {
         // only print stores once!
         when (io.iss_valids(i) && io.iss_uops(i).uopc =/= uopSTD)
         {
            printf("%d; O3PipeView:issue: %d\n",
               io.iss_uops(i).debug_events.fetch_seq,
               io.tsc_reg)
         }
      }
   }

   if (DEBUG_PRINTF)
   {
      for (i <- 0 until num_issue_slots)
      {
         printf("  integer_issue_slot[%d](%c)(Req:%c):wen=%c P:(%c,%c,%c) OP:(%d,%d,%d) PDST:%d %c [[DASM(%x)]" +
               " 0x%x: %d] ri:%d bm=%d imm=0x%x\n"
            , i.U(log2Up(num_issue_slots).W)
            , Mux(issue_slots(i).io.valid, Str("V"), Str("-"))
//            , Mux(issue_slots(i).io.request, Str(u_red + "R" + end), Str(grn + "-" + end))
//            , Mux(issue_slots(i).io.in_uop.valid, Str(u_wht + "W" + end),  Str(grn + " " + end))
            , Mux(issue_slots(i).io.request, Str("R"), Str("-"))
            , Mux(issue_slots(i).io.in_uop.valid, Str("W"),  Str(" "))
            , Mux(issue_slots(i).io.debug.p1, Str("!"), Str(" "))
            , Mux(issue_slots(i).io.debug.p2, Str("!"), Str(" "))
            , Mux(issue_slots(i).io.debug.p3, Str("!"), Str(" "))
            , issue_slots(i).io.uop.pop1
            , issue_slots(i).io.uop.pop2
            , issue_slots(i).io.uop.pop3
            , issue_slots(i).io.uop.pdst
            , Mux(issue_slots(i).io.uop.dst_rtype === RT_FIX, Str("X"),
              Mux(issue_slots(i).io.uop.dst_rtype === RT_X, Str("-"),
              Mux(issue_slots(i).io.uop.dst_rtype === RT_FLT, Str("f"),
              Mux(issue_slots(i).io.uop.dst_rtype === RT_PAS, Str("C"), Str("?")))))
            , issue_slots(i).io.uop.inst
            , issue_slots(i).io.uop.pc(31,0)
            , issue_slots(i).io.uop.uopc
            , issue_slots(i).io.uop.rob_idx
            , issue_slots(i).io.uop.br_mask
            , issue_slots(i).io.uop.imm_packed
            )
      }
   }
}


}
