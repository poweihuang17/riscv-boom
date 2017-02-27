//******************************************************************************
// Copyright (c) 2015, The Regents of the University of California (Regents).
// All Rights Reserved. See LICENSE for license details.
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
// Functional Units
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//
// Christopher Celio
// 2013 Mar 10
//
// If regfile bypassing is disabled, then the functional unit must do its own
// bypassing in here on the WB stage (i.e., bypassing the io.resp.data)

// TODO: explore possibility of conditional IO fields? if a branch unit... how to add extra to IO in subclass?

package boom
{

import chisel3._
import chisel3.util._
import cde.Parameters

import rocket.ALU._
import _root_.util._
import uncore.constants.MemoryOpConstants._


object FUConstants
{
   // bit mask, since a given execution pipeline may support multiple functional units
   val FUC_SZ = 8
   val FU_X   = BitPat.DC(FUC_SZ)
   val FU_ALU =   1.U(FUC_SZ.W)
   val FU_BRU =   2.U(FUC_SZ.W)
   val FU_MEM =   4.U(FUC_SZ.W)
   val FU_MUL =   8.U(FUC_SZ.W)
   val FU_DIV =  16.U(FUC_SZ.W)
   val FU_FPU =  32.U(FUC_SZ.W)
   val FU_CSR =  64.U(FUC_SZ.W)
   val FU_FDV = 128.U(FUC_SZ.W)
}
import FUConstants._

// tell the FUDecoders what units it needs to support
class SupportedFuncUnits(
   val alu: Boolean  = false,
   val bru: Boolean  = false,
   val mem: Boolean  = false,
   val muld: Boolean = false,
   val fpu: Boolean  = false,
   val csr: Boolean  = false,
   val fdiv: Boolean = false)
{
}


class FunctionalUnitIo(num_stages: Int
                      , num_bypass_stages: Int
                      , data_width: Int
                      )(implicit p: Parameters) extends BoomBundle()(p)
{
   val req     = Flipped(new DecoupledIO(new FuncUnitReq(data_width)))
   val resp    = (new DecoupledIO(new FuncUnitResp(data_width)))

   val brinfo  = new BrResolutionInfo().asInput

   val bypass  = new BypassData(num_bypass_stages, data_width).asOutput

   val br_unit = new BranchUnitResp().asOutput

   // only used by the fpu unit
   val fcsr_rm = Input(UInt(rocket.FPConstants.RM_SZ.W))

   // only used by branch unit
   // TODO name this, so ROB can also instantiate it
   val get_rob_pc = Flipped(new RobPCRequest())
   val get_pred = new GetPredictionInfo
   val status = new rocket.MStatus().asInput
}

class GetPredictionInfo(implicit p: Parameters) extends BoomBundle()(p)
{
   val br_tag = Output(UInt(BR_TAG_SZ.W))
   val info = new BranchPredictionResp().asInput
}

class FuncUnitReq(data_width: Int)(implicit p: Parameters) extends BoomBundle()(p)
{
   val uop = new MicroOp()

   val num_operands = 3

   val rs1_data = UInt(data_width.W)
   val rs2_data = UInt(data_width.W)
   val rs3_data = UInt(data_width.W) // only used for FMA units
//   val rs_data = Vec.fill(num_operands) {UInt(data_width.W)}
//   def rs1_data = rs_data(0)
//   def rs2_data = rs_data(1)
//   def rs3_data = rs_data(2)

   val kill = Bool() // kill everything

   override def cloneType = new FuncUnitReq(data_width)(p).asInstanceOf[this.type]
}

class FuncUnitResp(data_width: Int)(implicit p: Parameters) extends BoomBundle()(p)
{
   val uop = new MicroOp()
   val data = UInt(data_width.W)
   val fflags = new ValidIO(new FFlagsResp)
   val addr = UInt((vaddrBits+1).W) // only for maddr -> LSU
   val mxcpt = new ValidIO(UInt(rocket.Causes.all.max.W)) //only for maddr->LSU

   override def cloneType = new FuncUnitResp(data_width)(p).asInstanceOf[this.type]
}

class BypassData(num_bypass_ports: Int, data_width: Int)(implicit p: Parameters) extends BoomBundle()(p)
{
   val valid = Vec(num_bypass_ports, Bool())
   val uop   = Vec(num_bypass_ports, new MicroOp())
   val data  = Vec(num_bypass_ports, UInt(data_width.W))

   def getNumPorts: Int = num_bypass_ports
   override def cloneType: this.type = new BypassData(num_bypass_ports, data_width).asInstanceOf[this.type]
}

class BrResolutionInfo(implicit p: Parameters) extends BoomBundle()(p)
{
   val valid      = Bool()
   val mispredict = Bool()
   val mask       = UInt(MAX_BR_COUNT.W) // the resolve mask
   val tag        = UInt(BR_TAG_SZ.W)    // the branch tag that was resolved
   val exe_mask   = UInt(MAX_BR_COUNT.W) // the br_mask of the actual branch uop
                                               // used to reset the dec_br_mask
   val rob_idx    = UInt(ROB_ADDR_SZ.W)
   val ldq_idx    = UInt(MEM_ADDR_SZ.W)  // track the "tail" of loads and stores, so we can
   val stq_idx    = UInt(MEM_ADDR_SZ.W)  // quickly reset the LSU on a mispredict
   val taken      = Bool()                     // which direction did the branch go?
   val is_jr      = Bool()

   // for stats
   val btb_made_pred  = Bool()
   val btb_mispredict = Bool()
   val bpd_made_pred  = Bool()
   val bpd_mispredict = Bool()
}

// for critical path reasons, some of the elements in this bundle may be delayed.
class BranchUnitResp(implicit p: Parameters) extends BoomBundle()(p)
{
   val take_pc         = Bool()
   val target          = UInt((vaddrBits+1).W)

   val pc              = UInt((vaddrBits+1).W) // TODO this isn't really a branch_unit thing

   val brinfo          = new BrResolutionInfo()
   val btb_update_valid= Bool() // TODO turn this into a directed bundle so we can fold this into btb_update?
   val btb_update      = new rocket.BTBUpdate
   val bht_update      = Valid(new rocket.BHTUpdate)
   val bpd_update      = Valid(new BpdUpdate)

   val xcpt            = Valid(new Exception)

   val debug_btb_pred  = Bool() // just for debug, did the BTB and BHT predict taken?
}

abstract class FunctionalUnit(is_pipelined: Boolean
                              , num_stages: Int
                              , num_bypass_stages: Int
                              , data_width: Int
                              , has_branch_unit: Boolean = false)
                              (implicit p: Parameters) extends BoomModule()(p)
{
   val io = IO(new FunctionalUnitIo(num_stages, num_bypass_stages, data_width))
}


// Note: this helps track which uops get killed while in intermediate stages,
// but it is the job of the consumer to check for kills on the same cycle as consumption!!!
abstract class PipelinedFunctionalUnit(val num_stages: Int,
                                       val num_bypass_stages: Int,
                                       val earliest_bypass_stage: Int,
                                       val data_width: Int,
                                       is_branch_unit: Boolean = false
                                      )(implicit p: Parameters) extends FunctionalUnit(is_pipelined = true
                                                              , num_stages = num_stages
                                                              , num_bypass_stages = num_bypass_stages
                                                              , data_width = data_width
                                                              , has_branch_unit = is_branch_unit)(p)
{
   // pipelined functional unit is always ready
   io.req.ready := true.B


   if (num_stages > 0)
   {
      val r_valids = Reg(init = Vec.fill(num_stages) { false.B })
      val r_uops   = Reg(Vec(num_stages, new MicroOp()))

      // handle incoming request
      r_valids(0) := io.req.valid && !IsKilledByBranch(io.brinfo, io.req.bits.uop) && !io.req.bits.kill
      r_uops(0)   := io.req.bits.uop
      r_uops(0).br_mask := GetNewBrMask(io.brinfo, io.req.bits.uop)

      // handle middle of the pipeline
      for (i <- 1 until num_stages)
      {
         r_valids(i) := r_valids(i-1) && !IsKilledByBranch(io.brinfo, r_uops(i-1)) && !io.req.bits.kill
         r_uops(i)   := r_uops(i-1)
         r_uops(i).br_mask := GetNewBrMask(io.brinfo, r_uops(i-1))

         if (num_bypass_stages != 0)// && i > earliest_bypass_stage)
         {
            io.bypass.uop(i-1) := r_uops(i-1)
         }
      }

      // handle outgoing (branch could still kill it)
      // consumer must also check for pipeline flushes (kills)
      io.resp.valid    := r_valids(num_stages-1) && !IsKilledByBranch(io.brinfo, r_uops(num_stages-1))
      io.resp.bits.uop := r_uops(num_stages-1)
      io.resp.bits.uop.br_mask := GetNewBrMask(io.brinfo, r_uops(num_stages-1))

      // bypassing (TODO allow bypass vector to have a different size from num_stages)
      if (num_bypass_stages > 0 && earliest_bypass_stage == 0)
      {
         io.bypass.uop(0) := io.req.bits.uop

         for (i <- 1 until num_bypass_stages)
         {
            io.bypass.uop(i) := r_uops(i-1)
         }
      }
   }
   else
   {
      require (num_stages == 0)
      // pass req straight through to response

      // valid doesn't check kill signals, let consumer deal with it.
      // The LSU already handles it and this hurts critical path.
      io.resp.valid    := io.req.valid && !IsKilledByBranch(io.brinfo, io.req.bits.uop)
      io.resp.bits.uop := io.req.bits.uop
      io.resp.bits.uop.br_mask := GetNewBrMask(io.brinfo, io.req.bits.uop)
   }

}

class ALUUnit(is_branch_unit: Boolean = false, num_stages: Int = 1)(implicit p: Parameters)
             extends PipelinedFunctionalUnit(num_stages = num_stages
                                            , num_bypass_stages = num_stages
                                            , earliest_bypass_stage = 0
                                            , data_width = 64  //xLen
                                            , is_branch_unit = is_branch_unit)(p)
{
   val uop = io.req.bits.uop

   // immediate generation
   val imm_xprlen = ImmGen(uop.imm_packed, uop.ctrl.imm_sel)

   // operand 1 select
   var op1_data: UInt = null
   if (is_branch_unit)
   {
      op1_data = Mux(io.req.bits.uop.ctrl.op1_sel.asUInt() === OP1_RS1 , io.req.bits.rs1_data,
                 Mux(io.req.bits.uop.ctrl.op1_sel.asUInt() === OP1_PC  , Sext(io.get_rob_pc.curr_pc, xLen),
                                                                       0.U))
   }
   else
   {
      op1_data = Mux(io.req.bits.uop.ctrl.op1_sel.asUInt() === OP1_RS1 , io.req.bits.rs1_data,
                                                                       0.U)
   }

   // operand 2 select
   val op2_data = Mux(io.req.bits.uop.ctrl.op2_sel === OP2_IMM,  Sext(imm_xprlen.asUInt(), xLen),
                  Mux(io.req.bits.uop.ctrl.op2_sel === OP2_IMMC, io.req.bits.uop.pop1(4,0),
                  Mux(io.req.bits.uop.ctrl.op2_sel === OP2_RS2 , io.req.bits.rs2_data,
                  Mux(io.req.bits.uop.ctrl.op2_sel === OP2_FOUR, 4.U,
                                                                 0.U))))

   val alu = Module(new rocket.ALU())

   alu.io.in1 := op1_data.asUInt()
   alu.io.in2 := op2_data.asUInt()
   alu.io.fn  := io.req.bits.uop.ctrl.op_fcn
   alu.io.dw  := io.req.bits.uop.ctrl.fcn_dw


   if (is_branch_unit)
   {
      val uop_pc_ = io.get_rob_pc.curr_pc

      // The Branch Unit redirects the PC immediately, but delays the mispredict
      // signal a cycle (for critical path reasons)

      // Did I just get killed by the previous cycle's branch,
      // or by a flush pipeline?
      val killed = Wire(init=false.B)
      when (io.req.bits.kill ||
            (io.brinfo.valid &&
               io.brinfo.mispredict &&
               maskMatch(io.brinfo.mask, io.req.bits.uop.br_mask)
            ))
      {
         killed := true.B
      }

      val rs1 = io.req.bits.rs1_data
      val rs2 = io.req.bits.rs2_data
      val br_eq  = (rs1 === rs2)
      val br_ltu = (rs1.asUInt() < rs2.asUInt())
      val br_lt  = (~(rs1(xLen-1) ^ rs2(xLen-1)) & br_ltu |
                      rs1(xLen-1) & ~rs2(xLen-1)).toBool

      val pc_plus4 = (uop_pc_ + 4.U)(vaddrBits,0)

      val pc_sel = MuxLookup(io.req.bits.uop.ctrl.br_type, PC_PLUS4,
               Seq  (   BR_N  -> PC_PLUS4,
                        BR_NE -> Mux(!br_eq,  PC_BRJMP, PC_PLUS4),
                        BR_EQ -> Mux( br_eq,  PC_BRJMP, PC_PLUS4),
                        BR_GE -> Mux(!br_lt,  PC_BRJMP, PC_PLUS4),
                        BR_GEU-> Mux(!br_ltu, PC_BRJMP, PC_PLUS4),
                        BR_LT -> Mux( br_lt,  PC_BRJMP, PC_PLUS4),
                        BR_LTU-> Mux( br_ltu, PC_BRJMP, PC_PLUS4),
                        BR_J  -> PC_BRJMP,
                        BR_JR -> PC_JALR
                        ))

      val bj_addr = Wire(UInt())

      val is_taken = io.req.valid &&
                     !killed &&
                     uop.is_br_or_jmp &&
                     (pc_sel =/= PC_PLUS4)

      // "mispredict" means that a branch has been resolved and it must be killed
      val mispredict = Wire(Bool()); mispredict := false.B

      val is_br          = io.req.valid && !killed && uop.is_br_or_jmp && !uop.is_jump
      val is_br_or_jalr  = io.req.valid && !killed && uop.is_br_or_jmp && !uop.is_jal

      // did the BTB predict a br or jmp incorrectly?
      // (do we need to reset its history and teach it a new target?)
      val btb_mispredict = Wire(Bool()); btb_mispredict := false.B

      // did the bpd predict incorrectly (aka, should we correct its prediction?)
      val bpd_mispredict = Wire(Bool()); bpd_mispredict := false.B

      // if b/j is taken, does it go to the wrong target?
      val wrong_taken_target = !io.get_rob_pc.next_val || (io.get_rob_pc.next_pc =/= bj_addr)

      assert (!(io.req.valid && uop.is_jal && io.get_rob_pc.next_val && io.get_rob_pc.next_pc =/= bj_addr),
         "[func] JAL went to the wrong target.")

      when (is_br_or_jalr)
      {
         when (pc_sel === PC_JALR)
         {
            // only the BTB can predict JALRs (must also check it predicted taken)
            btb_mispredict := wrong_taken_target ||
                              !io.get_pred.info.btb_resp.taken ||
                              !uop.br_prediction.btb_hit ||
                              io.status.debug // fun hack to perform fence.i on JALRs in debug mode
            bpd_mispredict := false.B
         }
         when (pc_sel === PC_PLUS4)
         {
            btb_mispredict := uop.br_prediction.btb_hit && io.get_pred.info.btb_resp.taken
            bpd_mispredict := uop.br_prediction.bpd_predict_taken
         }
         when (pc_sel === PC_BRJMP)
         {
            btb_mispredict := wrong_taken_target ||
                              !uop.br_prediction.btb_hit ||
                              (uop.br_prediction.btb_hit && !io.get_pred.info.btb_resp.taken)
            bpd_mispredict := !uop.br_prediction.bpd_predict_taken
         }
      }

      when (is_br_or_jalr && pc_sel === PC_BRJMP && !mispredict && io.get_rob_pc.next_val)
      {
         when (io.get_rob_pc.next_pc =/= bj_addr)
         {
            printf ("[FuncUnit] Branch jumped to 0x%x, should have jumped to 0x%x.\n",
               io.get_rob_pc.next_pc, bj_addr)
         }
         assert (io.get_rob_pc.next_pc === bj_addr, "[FuncUnit] branch is taken to the wrong target.")
      }

      when (is_br_or_jalr)
      {
         when (pc_sel === PC_JALR)
         {
            mispredict := btb_mispredict
         }
         when (pc_sel === PC_PLUS4)
         {
            mispredict := Mux(uop.br_prediction.wasBTB, btb_mispredict, bpd_mispredict)
         }
         when (pc_sel === PC_BRJMP)
         {
            mispredict := Mux(uop.br_prediction.wasBTB, btb_mispredict, bpd_mispredict)
         }
      }


      val br_unit =
         if (p(EnableBrResolutionRegister)) Reg(new BranchUnitResp)
         else Wire(new BranchUnitResp)



      br_unit.take_pc := mispredict
      br_unit.target := Mux(pc_sel === PC_PLUS4, pc_plus4, bj_addr)

      // Delay branch resolution a cycle for critical path reasons.
      // If the rest of "br_unit" is being registered too, then we don't need to
      // register "brinfo" here, since in that case we would be double counting.
      val brinfo =
         if (p(EnableBrResolutionRegister)) Wire(new BrResolutionInfo)
         else Reg(new BrResolutionInfo)

      // note: jal doesn't allocate a branch-mask, so don't clear a br-mask bit
      brinfo.valid          := io.req.valid && uop.is_br_or_jmp && !uop.is_jal && !killed
      brinfo.mispredict     := mispredict
      brinfo.mask           := 1.U << uop.br_tag
      brinfo.exe_mask       := GetNewBrMask(io.brinfo, uop.br_mask)
      brinfo.tag            := uop.br_tag
      brinfo.rob_idx        := uop.rob_idx
      brinfo.ldq_idx        := uop.ldq_idx
      brinfo.stq_idx        := uop.stq_idx
      brinfo.is_jr          := pc_sel === PC_JALR
      brinfo.taken          := is_taken
      brinfo.btb_mispredict := btb_mispredict
      brinfo.bpd_mispredict := bpd_mispredict
      brinfo.btb_made_pred  := uop.br_prediction.wasBTB
      brinfo.bpd_made_pred  := uop.br_prediction.bpd_predict_val

      br_unit.brinfo := brinfo

      // updates the BTB same cycle as PC redirect
      val lsb = log2Ceil(FETCH_WIDTH*coreInstBytes)

      // did a branch or jalr occur AND did we mispredict? AND was it taken? (i.e., should we update the BTB)
      val fetch_pc = ((uop_pc_ >> lsb) << lsb) + uop.fetch_pc_lob

      if (p(EnableBTBContainsBranches))
      {
         br_unit.btb_update_valid := is_br_or_jalr && mispredict && is_taken
         // update on all branches (but not jal/jalr)
         br_unit.bht_update.valid := is_br && mispredict
      }
      else
      {
         br_unit.btb_update_valid := is_br_or_jalr && mispredict && uop.is_jump
         br_unit.bht_update.valid := false.B
      }

      br_unit.btb_update.pc               := fetch_pc // tell the BTB which pc to tag check against
      br_unit.btb_update.br_pc            := uop_pc_
      br_unit.btb_update.target           := (br_unit.target.toSInt & (-coreInstBytes).S).asUInt()
      br_unit.btb_update.prediction.valid := io.get_pred.info.btb_resp_valid // did this branch's fetch packet have
                                                                             // a BTB hit in fetch?
      br_unit.btb_update.prediction.bits  := io.get_pred.info.btb_resp       // give the BTB back its BTBResp
      br_unit.btb_update.taken            := is_taken   // was this branch/jal/jalr "taken"
      br_unit.btb_update.isJump           := uop.is_jump
      br_unit.btb_update.isReturn         := uop.is_ret

      br_unit.bht_update.bits.taken            := is_taken   // was this branch "taken"
      br_unit.bht_update.bits.mispredict       := btb_mispredict     // need to reset the history in the BHT
                                                                     // that is updated only on BTB hits
      br_unit.bht_update.bits.prediction.valid := io.get_pred.info.btb_resp_valid // only update if hit in the BTB
      br_unit.bht_update.bits.prediction.bits  := io.get_pred.info.btb_resp
      br_unit.bht_update.bits.pc               := fetch_pc // what pc should the tag check be on?

      br_unit.bpd_update.valid                 := io.req.valid && uop.is_br_or_jmp &&
                                                  !uop.is_jal && !killed
      br_unit.bpd_update.bits.is_br            := is_br
      br_unit.bpd_update.bits.brob_idx         := io.get_rob_pc.curr_brob_idx
      br_unit.bpd_update.bits.taken            := is_taken
      br_unit.bpd_update.bits.mispredict       := mispredict
      br_unit.bpd_update.bits.bpd_predict_val  := uop.br_prediction.bpd_predict_val
      br_unit.bpd_update.bits.bpd_mispredict   := bpd_mispredict
      br_unit.bpd_update.bits.pc               := fetch_pc
      br_unit.bpd_update.bits.br_pc            := uop_pc_
      br_unit.bpd_update.bits.history_ptr      := io.get_pred.info.bpd_resp.history_ptr
      br_unit.bpd_update.bits.info             := io.get_pred.info.bpd_resp.info
      if (!ENABLE_VLHR)
      {
         br_unit.bpd_update.bits.history.get := io.get_pred.info.bpd_resp.history.get
         br_unit.bpd_update.bits.history_u.get := io.get_pred.info.bpd_resp.history_u.get
      }

      // is the br_pc the last instruction in the fetch bundle?
      val is_last_inst = if (FETCH_WIDTH == 1) { true.B }
                         else { ((uop_pc_ >> log2Up(coreInstBytes).U) &
                                 Fill(log2Up(FETCH_WIDTH), 1.U)) === (FETCH_WIDTH-1).U }
      br_unit.bpd_update.bits.new_pc_same_packet := !(is_taken) && !is_last_inst

      require (coreInstBytes == 4)


      // Branch/Jump Target Calculation
      // we can't push this through the ALU though, b/c jalr needs both PC+4 and rs1+offset

      def vaSign(a0: UInt, ea: UInt):Bool = {
         // efficient means to compress 64-bit VA into rc.as.vaddrBits+1 bits
         // (VA is bad if VA(rc.as.vaddrBits) =/= VA(rc.as.vaddrBits-1))
         val a = a0 >> vaddrBits-1
         val e = ea(vaddrBits,vaddrBits-1)
         Mux(a === 0.U || a === 1.U, e =/= 0.U,
         Mux(a.toSInt === (-1).S || a.toSInt === (-2).S, e.toSInt === (-1).S,
            e(0)))
      }

      val bj_base = Mux(uop.uopc === uopJALR, io.req.bits.rs1_data, uop_pc_)
      val bj_offset = imm_xprlen(20,0).toSInt
      val bj64 = (bj_base.toSInt + bj_offset).asUInt()
      val bj_msb = Mux(uop.uopc === uopJALR, vaSign(io.req.bits.rs1_data, bj64.asUInt()), vaSign(uop_pc_, bj64.asUInt()))
      bj_addr := (Cat(bj_msb, bj64(vaddrBits-1,0)).toSInt & (-2).S).asUInt()

      br_unit.pc             := uop_pc_
      br_unit.debug_btb_pred := io.get_pred.info.btb_resp_valid && io.get_pred.info.btb_resp.taken

      // handle misaligned branch/jmp targets
      // TODO BUG only trip xcpt if taken to bj_addr
      br_unit.xcpt.valid     := bj_addr(1) && io.req.valid && mispredict && !killed
      br_unit.xcpt.bits.uop  := uop
      br_unit.xcpt.bits.cause:= rocket.Causes.misaligned_fetch.U
      // TODO is there a better way to get this information to the CSR file? maybe use brinfo.target?
      br_unit.xcpt.bits.badvaddr:= bj_addr

      io.br_unit := br_unit
   }

   // Response
   // TODO add clock gate on resp bits from functional units
//   io.resp.bits.data := RegEnable(alu.io.out, io.req.valid)
//   val reg_data = Reg(outType = Bits(xLen.W))
//   reg_data := alu.io.out
//   io.resp.bits.data := reg_data

   val r_val  = Reg(init = Vec.fill(num_stages) { false.B })
   val r_data = Reg(Vec(num_stages, UInt(xLen.W)))
   r_val (0) := io.req.valid
   r_data(0) := alu.io.out
   for (i <- 1 until num_stages)
   {
      r_val(i)  := r_val(i-1)
      r_data(i) := r_data(i-1)
   }
   io.resp.bits.data := r_data(num_stages-1)

   // Bypass
   // for the ALU, we can bypass same cycle as compute
   require (num_stages >= 1)
   require (num_bypass_stages >= 1)
   io.bypass.valid(0) := io.req.valid
   io.bypass.data (0) := alu.io.out
   for (i <- 1 until num_stages)
   {
      io.bypass.valid(i) := r_val(i-1)
      io.bypass.data (i) := r_data(i-1)
   }

   // Exceptions
   io.resp.bits.fflags.valid := false.B
}


// passes in base+imm to calculate addresses, and passes store data, to the LSU
// for floating point, 65bit FP store-data needs to be decoded into 64bit FP form
class MemAddrCalcUnit(implicit p: Parameters) extends PipelinedFunctionalUnit(num_stages = 0
                                                     , num_bypass_stages = 0
                                                     , earliest_bypass_stage = 0
                                                     , data_width = 65 // TODO enable this only if FP is enabled?
                                                     , is_branch_unit = false)(p)
{
   // perform address calculation
   val sum = (io.req.bits.rs1_data.toSInt + io.req.bits.uop.imm_packed(19,8).toSInt).asUInt()
   val ea_sign = Mux(sum(vaddrBits-1), ~sum(63,vaddrBits) === 0.U,
                                        sum(63,vaddrBits) =/= 0.U)
   val effective_address = Cat(ea_sign, sum(vaddrBits-1,0)).asUInt()

   // compute store data
   // requires decoding 65-bit FP data
   val unrec_s = hardfloat.fNFromRecFN(8, 24, io.req.bits.rs2_data)
   val unrec_d = hardfloat.fNFromRecFN(11, 53, io.req.bits.rs2_data)
   val unrec_out = Mux(io.req.bits.uop.fp_single, Cat(Fill(32, unrec_s(31)), unrec_s), unrec_d)

   var store_data:UInt = null
   if (!usingFPU) store_data = io.req.bits.rs2_data
   else store_data = Mux(io.req.bits.uop.fp_val, unrec_out, io.req.bits.rs2_data)

   io.resp.bits.addr := effective_address
   io.resp.bits.data := store_data

   if (data_width > 63)
   {
      assert (!(io.req.valid && io.req.bits.uop.ctrl.is_std &&
         io.resp.bits.data(64).toBool === true.B), "65th bit set in MemAddrCalcUnit.")
   }

   // Handle misaligned exceptions
   val typ = io.req.bits.uop.mem_typ
   val misaligned =
      (((typ === rocket.MT_H) || (typ === rocket.MT_HU)) && (effective_address(0) =/= 0.U)) ||
      (((typ === rocket.MT_W) || (typ === rocket.MT_WU)) && (effective_address(1,0) =/= 0.U)) ||
      ((typ ===  rocket.MT_D) && (effective_address(2,0) =/= 0.U))

   val ma_ld = io.req.valid && io.req.bits.uop.uopc === uopLD && misaligned
   val ma_st = io.req.valid && (io.req.bits.uop.uopc === uopSTA || io.req.bits.uop.uopc === uopAMO_AG) && misaligned

   io.resp.bits.mxcpt.valid := ma_ld || ma_st
   io.resp.bits.mxcpt.bits  := Mux(ma_ld, rocket.Causes.misaligned_load.U,
                                          rocket.Causes.misaligned_store.U)
   assert (!(ma_ld && ma_st), "Mutually-exclusive exceptions are firing.")
}



// currently, bypassing is unsupported!
// All FP instructions are padded out to the max latency unit for easy
// write-port scheduling.
class FPUUnit(implicit p: Parameters) extends PipelinedFunctionalUnit(num_stages = 3
                                            , num_bypass_stages = 0
                                            , earliest_bypass_stage = 0
                                            , data_width = 65)(p)
{
   val fpu = Module(new FPU())
   fpu.io.req <> io.req
   fpu.io.req.bits.fcsr_rm := io.fcsr_rm

   io.resp.bits.data              := fpu.io.resp.bits.data
   io.resp.bits.fflags.valid      := fpu.io.resp.bits.fflags.valid
   io.resp.bits.fflags.bits.uop   := io.resp.bits.uop
   io.resp.bits.fflags.bits.flags := fpu.io.resp.bits.fflags.bits.flags // kill me now
}



// unpipelined, can only hold a single MicroOp at a time
// assumes at least one register between request and response
abstract class UnPipelinedFunctionalUnit(implicit p: Parameters)
                                       extends FunctionalUnit(is_pipelined = false
                                                            , num_stages = 1
                                                            , num_bypass_stages = 0
                                                            , data_width = 64
                                                            , has_branch_unit = false)(p)
{
   val r_uop = Reg(outType = new MicroOp())

   val do_kill = Wire(Bool())
   do_kill := io.req.bits.kill // irrelevant default

   when (io.req.fire())
   {
      // update incoming uop
      do_kill := IsKilledByBranch(io.brinfo, io.req.bits.uop) || io.req.bits.kill
      r_uop := io.req.bits.uop
      r_uop.br_mask := GetNewBrMask(io.brinfo, io.req.bits.uop)
   }
   .otherwise
   {
      do_kill := IsKilledByBranch(io.brinfo, r_uop) || io.req.bits.kill
      r_uop.br_mask := GetNewBrMask(io.brinfo, r_uop)
   }


   // assumes at least one pipeline register between request and response
   io.resp.bits.uop := r_uop
}


class MulDivUnit(implicit p: Parameters) extends UnPipelinedFunctionalUnit()(p)
{
   val muldiv = Module(new rocket.MulDiv(
      p(rocket.MulDivKey).getOrElse(rocket.MulDivConfig()),
      width = xLen))

   // request
   muldiv.io.req.valid    := io.req.valid && !this.do_kill
   muldiv.io.req.bits.dw  := io.req.bits.uop.ctrl.fcn_dw
   muldiv.io.req.bits.fn  := io.req.bits.uop.ctrl.op_fcn
   muldiv.io.req.bits.in1 := io.req.bits.rs1_data
   muldiv.io.req.bits.in2 := io.req.bits.rs2_data
   io.req.ready           := muldiv.io.req.ready

   // handle pipeline kills and branch misspeculations
   muldiv.io.kill         := this.do_kill

   // response
   io.resp.valid          := muldiv.io.resp.valid
   muldiv.io.resp.ready   := io.resp.ready
   io.resp.bits.data      := muldiv.io.resp.bits.data
}

class PipelinedMulUnit(num_stages: Int)(implicit p: Parameters)
      extends PipelinedFunctionalUnit (num_stages = num_stages
                                      , num_bypass_stages = 0
                                      , earliest_bypass_stage = 0
                                      , data_width = 64)(p)
{
   val imul = Module(new IMul(num_stages))
   // request
   imul.io.valid := io.req.valid
   imul.io.in0   := io.req.bits.rs1_data
   imul.io.in1   := io.req.bits.rs2_data
   imul.io.dw    := io.req.bits.uop.ctrl.fcn_dw
   imul.io.fn    := io.req.bits.uop.ctrl.op_fcn

   // response
   io.resp.bits.data      := imul.io.out
}

}

