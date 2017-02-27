//******************************************************************************
// Copyright (c) 2015, The Regents of the University of California (Regents).
// All Rights Reserved. See LICENSE for license details.
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
// Data Cache Shim/Wrapper to the Hella-Cache
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//
// Christopher Celio
// 2012 Oct 15

// We need to track inflight loads that may have been misspeculated, and filter
// them out before they can be returned to the pipeline (we do not want to hold
// up pipeline resources like LD/ST entries on them).
//
// Also, the hellacache was designed for a 5-stage pipeline, and has some
// pecularities regarding nacks, kills, store-data forwarding, etc.
//
// Contract:
//    everything put in here will be executed by memory
//    branch/kill signals will filter resp_val signals, but otherwise continue on

package boom
{

import chisel3._
import chisel3.util._
import cde.Parameters

import uncore.constants.MemoryOpConstants._


// Track Inflight Memory Requests
class LoadReqSlotIo(implicit p: Parameters) extends BoomBundle()(p)
{
   val valid      = Output(Bool()) // slot has an entry

   val wen        = Input(Bool())
   val in_uop     = new MicroOp().asInput // need ldq_idx, brmask

   val clear      = Input(Bool()) // kill slot immediately (either nacked or succeeded)
   val brinfo     = new BrResolutionInfo().asInput
   val flush_pipe = Input(Bool()) // exceptions, etc. but keep slot valid

   val out_uop    = new MicroOp().asOutput // need ldq_idx

   val was_killed = Output(Bool()) // should we filter out returning mem op?
}

// Note: Anything incoming that gets killed by br or exception is still marked
// as "valid", since it also got sent to the datacache.
class LoadReqSlot(implicit p: Parameters) extends BoomModule()(p)
{
   val io = IO(new LoadReqSlotIo())

   val valid      = Reg(init=false.B)
   val was_killed = Reg(init=false.B)
   val uop        = Reg(outType=(new MicroOp()))

   // did the existing uop get killed by a branch?
   val br_killed = Wire(Bool())
   br_killed := false.B
   // Note: we need to check/clr br_mask for incoming uop, despite previous MAddr
   // unit will have already performed that for us, the LSU waking up loads may not have.
   val br_killed_incoming = Wire(Bool())
   br_killed_incoming := false.B

   // only allow the clearing of a valid entry
   // otherwise we might overwrite an incoming entry
   when (io.clear && valid)
   {
      valid      := false.B
   }
   .elsewhen (io.wen)
   {
      valid      := true.B
      was_killed := io.flush_pipe || br_killed_incoming
      uop        := io.in_uop
   }
   .elsewhen (io.flush_pipe || br_killed)
   {
      was_killed := true.B
   }

   val bmask_match = io.brinfo.valid && maskMatch(io.brinfo.mask, uop.br_mask)
   val bmask_match_incoming = io.brinfo.valid && maskMatch(io.brinfo.mask, io.in_uop.br_mask)

   // Handle the Branch Mask (incoming not necessarily handled, if coming uop from LSU)
   when (io.wen)
   {
      uop.br_mask := Mux(io.brinfo.valid, (io.in_uop.br_mask & ~io.brinfo.mask),
                                          (io.in_uop.br_mask))
   }
   .elsewhen (bmask_match)
   {
      uop.br_mask := uop.br_mask & ~io.brinfo.mask
   }

   // handle branch killed
   when (io.brinfo.mispredict)
   {
      when (bmask_match)
      {
         br_killed := true.B
      }
      when (bmask_match_incoming)
      {
         br_killed_incoming := true.B
      }
   }

   // outputs
   io.valid      := valid
   // "was killed" handles branch killing us same cycle as resp is valid.
   io.was_killed := (was_killed || br_killed)
   io.out_uop    := uop
}

class DCacheReq(implicit p: Parameters) extends BoomBundle()(p)
{
   val addr    = UInt(coreMaxAddrBits.W)
   val uop     = new MicroOp()
   val data    = Bits(coreDataBits.W)
   val kill    = Bool()    // e.g., LSU detects load misspeculation
}

class NackInfo(implicit p: Parameters) extends BoomBundle()(p)
{
   val valid      = Bool()
   val lsu_idx    = UInt(MEM_ADDR_SZ.W)
   val isload     = Bool()
   val cache_nack = Bool() // was the cache nacking us, or the LSU
                           // cache nacks for stuctural hazards (MUST kill st->ld forwardings)
                           // LSU nacks for address conflicts/forwarding
}

class DCacheResp(implicit p: Parameters) extends BoomBundle()(p)
{
   val data         = Bits(coreDataBits.W)
   val data_subword = Bits(coreDataBits.W)
   val uop          = new MicroOp
   val typ          = Bits(rocket.MT_SZ.W)
}


// from pov of datapath
class DCMemPortIO(implicit p: Parameters) extends BoomBundle()(p)
{
   val req     = (new DecoupledIO(new DCacheReq))
   val resp    = Flipped(new ValidIO(new DCacheResp))

   val brinfo  = new BrResolutionInfo().asOutput
   val nack    = new NackInfo().asInput
   val flush_pipe  = Output(Bool())   // exception or other misspec which flushes entire pipeline
   val invalidate_lr = Output(Bool()) // should the dcache clear ld/sc reservations?
   val ordered = Input(Bool())        // is the dcache ordered? (fence is done)

// TODO chisel3 broke this
//   val debug = new BoomBundle()(p)
//   {
//      val memreq_val = Bool()
//      val memreq_lidx = UInt(MEM_ADDR_SZ.W)
//      val memresp_val = Bool()
//      val req_kill = Bool()
//      val nack = Bool()
//      val cache_nack = Bool()
//      val cache_resp_tag = UInt(width=log2Up(MAX_LD_COUNT))
//      val cache_not_ready = Bool()
//
////      val ld_req_slot = Vec.fill(MAX_LD_COUNT) { new Bundle {
////      val ld_req_slot = Vec(MAX_LD_COUNT, new Bundle {
////         val valid = Bool()
////         val killed = Bool()
////         val uop = new MicroOp()
////      })
//   }.asInput

//   override def cloneType: this.type = new DCMemPortIO()(p).asInstanceOf[this.type]
}

class DCacheShim(implicit p: Parameters) extends BoomModule()(p)
{
   val max_num_inflight = MAX_LD_COUNT
   isPow2(max_num_inflight)

   val io = IO(new Bundle
   {
      val core = Flipped(new DCMemPortIO())
      val dmem = new rocket.HellaCacheIO
   })

   // we are going to ignore store acks (for now at least), so filter them out and only listen to load acks
   // we know the store succeeded if it was not nacked
   val cache_load_ack = io.dmem.resp.valid && io.dmem.resp.bits.has_data

   val inflight_load_buffer = Seq.fill(max_num_inflight)(Module(new LoadReqSlot()))

   val m1_inflight_tag  = Wire(Bits()) // one cycle ago, aka now in the Mem1 Stage
   val m2_inflight_tag  = Wire(Bits()) // two cycles ago, aka now in the Mem2 Stage
   val m2_req_uop       = Reg(next=Reg(next=io.core.req.bits.uop)) // nack signals come two cycles later

   val enq_val = io.core.req.valid && (io.core.req.bits.uop.is_load || io.core.req.bits.uop.is_amo)
   val enq_rdy = Wire(Bool())

   for (i <- 0 until max_num_inflight)
   {
      // don't clr random entry, make sure m1_tag is correct
      inflight_load_buffer(i).io.clear    := (cache_load_ack && io.dmem.resp.bits.tag === i.U) ||
                                             (io.dmem.s2_nack &&
                                                (m2_req_uop.is_load || m2_req_uop.is_amo) &&
                                                m2_inflight_tag === i.U &&
                                                Reg(next=Reg(next=(enq_val && enq_rdy)))) ||
                                             (io.core.req.bits.kill &&
                                                m1_inflight_tag === i.U &&
                                                Reg(next=(enq_val && enq_rdy)))
      inflight_load_buffer(i).io.brinfo      := io.core.brinfo
      inflight_load_buffer(i).io.flush_pipe  := io.core.flush_pipe
      inflight_load_buffer(i).io.in_uop      := io.core.req.bits.uop
   }


   // dispatch/entry logic
   val enq_idx = Wire(UInt(log2Up(max_num_inflight).W))
   enq_idx := 0.U

   for (i <- max_num_inflight-1 to 0 by -1)
   {
      when (!inflight_load_buffer(i).io.valid)
      {
         enq_idx := i.U
      }
   }

   // ready logic (is there a inflight buffer slot that's not valid yet?)
   enq_rdy := false.B
   for (i <- 0 until max_num_inflight)
   {
      when (!inflight_load_buffer(i).io.valid && io.dmem.req.ready)
      {
         enq_rdy := true.B
      }
   }

   val new_inflight_tag = enq_idx
   m2_inflight_tag := Reg(next=Reg(next=enq_idx))
   m1_inflight_tag := Reg(next=enq_idx)

   val enq_can_occur = enq_val && enq_rdy

   val enq_idx_1h = (1.U << enq_idx) &
                    Fill(max_num_inflight, enq_can_occur)


   for (i <- 0 until max_num_inflight)
   {
      inflight_load_buffer(i).io.wen := enq_idx_1h(i)
   }

   // NOTE: if !enq_rdy, then we have to kill the memory request, and nack the LSU
   // inflight load buffer resource hazard
   val iflb_kill = Reg(next=(enq_val && !enq_rdy))


   // try to catch if there's a resource leak
   val full_counter = Reg(init=0.U(32.W))
   when (enq_rdy) { full_counter := 0.U }
   .otherwise     { full_counter := full_counter + 1.U }

   assert(full_counter <= 10000.U, "Inflight buffers have been busy for 10k cycles. Probably a resource leak.")


   //------------------------------------------------------------
   //-- Data Prefetcher
   //------------------------------------------------------------
   // listen in on the core<->cache requests/responses, and insert our own
   // prefetch requests to the data cache

//   val prefetcher = Module(new Prefetcher())
//
//      prefetcher.io.core_requests.valid := Reg(next=Reg(next=io.core.req.valid))
//      prefetcher.io.core_requests.bits.addr := Reg(next=Reg(next=io.core.req.bits.addr))
//      prefetcher.io.core_requests.bits.miss := Reg(next=Reg(next=io.core.req.valid)) &&
//                                               nbdcache.io.cpu.resp.bits.miss
//      prefetcher.io.core_requests.bits.secondary_miss := false.B
////                                                         Reg(next=Reg(next=io.core.req.valid)) &&
////                                                         nbdcache.io.cpu.resp.bits.miss &&
////                                                         nbdcache.io.cpu.resp.bits.secondary_miss
//
//      prefetcher.io.cache.req.ready := !io.core.req.valid && nbdcache.io.cpu.req.ready

   //------------------------------------------------------------
   // hook up requests

//   val prefetch_req_val = prefetcher.io.cache.req.valid && ENABLE_PREFETCHING.B
   val prefetch_req_val = false.B

   io.core.req.ready      := enq_rdy && io.dmem.req.ready
   io.dmem.req.valid      := (io.core.req.valid || prefetch_req_val)
   io.dmem.req.bits.typ   := io.core.req.bits.uop.mem_typ
   io.dmem.req.bits.addr  := io.core.req.bits.addr
   io.dmem.req.bits.tag   := new_inflight_tag
   io.dmem.req.bits.cmd   := Mux(io.core.req.valid, io.core.req.bits.uop.mem_cmd, M_PFW)
   io.dmem.s1_data        := Reg(next=io.core.req.bits.data) //notice this is delayed a cycle!
   io.dmem.s1_kill        := io.core.req.bits.kill || iflb_kill // kills request sent out last cycle
   io.dmem.req.bits.phys  := true.B // we always use physical addresses here,
                                        // as we've already done our own translations.

   //------------------------------------------------------------
   // handle responses and nacks

   // note: nacks come two cycles after a response, so I'm delaying everything
   // properly to line up stores, loads, nacks, and subword loads.
   // was two cycles ago a store request?
   val was_store_and_not_amo = m2_req_uop.is_store &&
                               !m2_req_uop.is_amo &&
                               Reg(next=Reg(next=(io.core.req.valid && io.dmem.req.ready)))

   // TODO add entry valid bit?
   val resp_tag = io.dmem.resp.bits.tag
   val inflight_load_buffer_was_killed = Vec(inflight_load_buffer map (_.io.was_killed))
   val inflight_load_buffer_out_uop = Vec(inflight_load_buffer map (_.io.out_uop))

   io.core.resp.valid := Mux(cache_load_ack,
                           !(inflight_load_buffer_was_killed(resp_tag)), // hide loads that were killed
                         Mux(was_store_and_not_amo && !io.dmem.s2_nack && !Reg(next=io.core.req.bits.kill),
                           true.B,    // stores succeed quietly, so valid if no nack
                           false.B))  // filter out nacked responses

   io.core.resp.bits.uop := Mux(cache_load_ack, inflight_load_buffer_out_uop(resp_tag), m2_req_uop)

   // comes out the same cycle as the resp.valid signal
   // but is a few gates slower than resp.bits.data
   // TODO change resp bundle to match the new hellacache resp bundle
   io.core.resp.bits.data_subword := io.dmem.resp.bits.data
   io.core.resp.bits.data         := io.dmem.resp.bits.data_word_bypass
   io.core.resp.bits.typ          := io.dmem.resp.bits.typ

   //------------------------------------------------------------
   // handle nacks from the cache (or from the IFLB or the LSU)

   io.core.nack.valid     := (io.dmem.s2_nack) || Reg(next=io.core.req.bits.kill) || Reg(next=iflb_kill) ||
                              Reg(next=Reg(next=(io.core.req.valid && !(io.dmem.req.ready))))
   io.core.nack.lsu_idx   := Mux(m2_req_uop.is_load, m2_req_uop.ldq_idx, m2_req_uop.stq_idx)
   io.core.nack.isload    := m2_req_uop.is_load
   io.core.nack.cache_nack:= io.dmem.s2_nack ||
                              Reg(next=iflb_kill) ||
                              Reg(next=Reg(next= (!(io.dmem.req.ready))))

   //------------------------------------------------------------
   // Handle exceptions and fences
   io.core.ordered := io.dmem.ordered

   // we handle all of the memory exceptions (unaligned and faulting) in the LSU
   assert (!(io.core.resp.valid && RegNext(io.dmem.xcpt.ma.ld) &&
      io.dmem.resp.bits.tag === RegNext(RegNext(io.dmem.req.bits.tag))),
      "Data cache returned an misaligned load exception, which BOOM handles elsewhere.")
   assert (!(io.core.resp.valid && RegNext(io.dmem.xcpt.ma.st) &&
      io.dmem.resp.bits.tag === RegNext(RegNext(io.dmem.req.bits.tag))),
      "Data cache returned an misaligned store exception, which BOOM handles elsewhere.")
   assert (!(io.core.resp.valid && RegNext(io.dmem.xcpt.pf.ld) &&
      io.dmem.resp.bits.tag === RegNext(RegNext(io.dmem.req.bits.tag))),
      "Data cache returned an faulting load exception, which BOOM handles elsewhere.")
   assert (!(io.core.resp.valid && RegNext(io.dmem.xcpt.pf.st) &&
      io.dmem.resp.bits.tag === RegNext(RegNext(io.dmem.req.bits.tag))),
      "Data cache returned an faulting store exception, which BOOM handles elsewhere.")

   //------------------------------------------------------------
   // debug

//   io.core.debug.memreq_val := io.core.req.valid
//   io.core.debug.memreq_lidx := Mux(io.core.req.bits.uop.is_load, io.core.req.bits.uop.ldq_idx,
//                                                                  io.core.req.bits.uop.stq_idx)
//   io.core.debug.memresp_val := io.core.resp.valid
//   io.core.debug.memresp_val := io.core.resp.valid
//   io.core.debug.req_kill := io.core.req.bits.kill
//   io.core.debug.nack := io.core.nack.valid
//   io.core.debug.cache_nack := io.core.nack.cache_nack
//   io.core.debug.cache_resp_tag := resp_tag
//   io.core.debug.cache_not_ready := !io.core.req.ready

//   for (i <- 0 until max_num_inflight)
//   {
//      io.core.debug.ld_req_slot(i).valid := inflight_load_buffer(i).valid
//      io.core.debug.ld_req_slot(i).killed := inflight_load_buffer(i).was_killed
//      io.core.debug.ld_req_slot(i).uop := inflight_load_buffer(i).out_uop
//   }

   //------------------------------------------------------------

}

}
