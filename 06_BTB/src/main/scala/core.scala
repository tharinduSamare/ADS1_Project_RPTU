// ADS I Class Project
// Pipelined RISC-V Core with Hazard Detetcion and Resolution
//
// Chair of Electronic Design Automation, RPTU in Kaiserslautern
// File created on 05/21/2024 by Andro Mazmishvili (@Andrew8846)

/*
The goal of this task is to equip the pipelined 5-stage 32-bit RISC-V core from the previous task with a ForwardingUnit_inst unit that takes care of hazard detetction and hazard resolution.
The functionality is the same as in task 4, but the core should now also be able to also process instructions with operands depending on the outcome of a previous instruction without stalling.

In addition to the pipelined design from task 4, you need to implement the following modules and functionality:

    Hazard Detection and Forwarding:
        Forwarding Unit: Determines if and from where data should be forwarded to resolve hazards. 
                         Resolves data hazards by ForwardingUnit_inst the correct values from later pipeline stages to earlier ones.
                         - Inputs: Register identifiers from the ID, EX, MEM, and WB stages.
                         - Outputs: Forwarding select signals (forwardA and forwardB) indicating where to forward the values from.

        The ForwardingUnit_inst logic utilizes multiplexers to select the correct operand values based on ForwardingUnit_inst decisions.

Make sure that data hazards (dependencies between instructions in the pipeline) are detected and resolved without stalling the pipeline. For additional information, you can revise the ADS I lecture slides (6-25ff).

Note this design only represents a simplified RISC-V pipeline. The structure could be equipped with further instructions and extension to support a real RISC-V ISA.
*/

package core_tile

import chisel3._
import chisel3.experimental.ChiselEnum
import chisel3.util._
import chisel3.util.experimental.loadMemoryFromFile
// import core_tile.opcodeT.{U_type => U_type}


// -----------------------------------------
// Global Definitions and Data Types
// -----------------------------------------

object uopc extends ChiselEnum {

  val isADD   = Value(0x01.U)
  val isSUB   = Value(0x02.U)
  val isXOR   = Value(0x03.U)
  val isOR    = Value(0x04.U)
  val isAND   = Value(0x05.U)
  val isSLL   = Value(0x06.U)
  val isSRL   = Value(0x07.U)
  val isSRA   = Value(0x08.U)
  val isSLT   = Value(0x09.U)
  val isSLTU  = Value(0x0A.U)

  val isADDI  = Value(0x10.U)
  val isSLTI  = Value(0x11.U)
  val isSLTIU = Value(0x12.U)
  val isXORI  = Value(0x13.U)
  val isORI   = Value(0x14.U)
  val isANDI  = Value(0x15.U)
  val isSLLI  = Value(0x16.U)
  val isSRLI  = Value(0x17.U)
  val isSRAI  = Value(0x18.U)

  val isBEQ   = Value(0x20.U)
  val isBNE   = Value(0x21.U)
  val isBLT   = Value(0x22.U)
  val isBLTU  = Value(0x23.U)
  val isBGE   = Value(0x24.U)
  val isBGEU  = Value(0x25.U)

  val isJAL   = Value(0x30.U)
  val isJALR  = Value(0x31.U)

  val isLW    = Value(0x40.U)
  val isSW    = Value(0x50.U)

  val invalid = Value(0xFF.U)
}

object opcodeT extends  ChiselEnum {
    val L_type  = Value("b0000011".U) // LOAD (I_type)
    val I_type  = Value("b0010011".U)
    val AU_type = Value("b0010111".U) // AUIPC (U_type)
    val S_type  = Value("b0100011".U)
    val R_type  = Value("b0110011".U)
    val U_type  = Value("b0110111".U)
    val B_type  = Value("b1100011".U)
    val JR_type = Value("b1100111".U) // JALR (I_type)
    val J_type  = Value("b1101111".U)
}

object branchT extends ChiselEnum {
    val BEQ  = Value("b000".U)
    val BNE  = Value("b001".U)
    val BLT  = Value("b100".U)
    val BGE  = Value("b101".U)
    val BLTU = Value("b110".U)
    val BGEU = Value("b111".U)
}

object aluOpAMux extends  ChiselEnum { // ForwardingUnit_inst mux for ALU opB
    val opA_id, AluResult_mem, AluResult_wb = Value
}
object aluOpBMux extends  ChiselEnum { // ForwardingUnit_inst mux for ALU opA
    val opB_id, AluResult_mem, AluResult_wb = Value
}
object  aluOpBImmMux extends ChiselEnum {
    val forwardMuxB, imme = Value
}

import uopc._
import aluOpAMux._
import aluOpBMux._


// -----------------------------------------
// Register File
// -----------------------------------------

class regFileReadReq extends Bundle {
    val addr  = Input(UInt(5.W))
}

class regFileReadResp extends Bundle {
    val data  = Output(UInt(32.W))
}

class regFileWriteReq extends Bundle {
    val addr  = Input(UInt(5.W))
    val data  = Input(UInt(32.W))
    val wr_en = Input(Bool())
}

class RegFile extends Module {
  val io = IO(new Bundle {
    val req_1  = new regFileReadReq
    val resp_1 = new regFileReadResp
    val req_2  = new regFileReadReq
    val resp_2 = new regFileReadResp
    val req_3  = new regFileWriteReq
})

  val RegFile_inst = Mem(32, UInt(32.W))
  RegFile_inst(0) := 0.U                           // hard-wired zero for x0

  when(io.req_3.wr_en){
    when(io.req_3.addr =/= 0.U){
      RegFile_inst(io.req_3.addr) := io.req_3.data
    }
  }

  io.resp_1.data := Mux((io.req_1.addr === 0.U), 0.U, (Mux((io.req_1.addr === io.req_3.addr), io.req_3.data, RegFile_inst(io.req_1.addr))))
  io.resp_2.data := Mux((io.req_2.addr === 0.U), 0.U, (Mux((io.req_2.addr === io.req_3.addr), io.req_3.data, RegFile_inst(io.req_2.addr))))

}

class ControlUnit extends  Module {
    val io = IO(new Bundle {
        val instr = Input(UInt(32.W))
        val wrEn = Output(UInt(1.W)) // Register file write back enable
        val uop  = Output(uopc())
        val ALUSrc = Output(aluOpBImmMux()) // ALU srcB Mux controller
        val memRd = Output(UInt(1.W))
        val memWr = Output(UInt(1.W))
        val memtoReg = Output(UInt(1.W))
    })

    val (opcode, opcode_cast3)  = opcodeT.safe(io.instr(6, 0))
    assert(opcode_cast3, "Opcode must be a valid one, got %x.", io.instr(6,0))
    val funct3  = io.instr(14, 12)

    // R-Type
    val funct7  = io.instr(31, 25)
    
    io.uop := invalid
    switch(opcode){
        is(opcodeT.R_type){
            when(funct3 === "b000".U){
                when(funct7 === "b0000000".U){
                    io.uop := isADD
                }.elsewhen(funct7 === "b0100000".U){
                    io.uop := isSUB
                }.otherwise{
                    io.uop := invalid
                }
            }.elsewhen(funct3 === "b100".U){
                when(funct7 === "b0000000".U){
                    io.uop := isXOR
                }.otherwise{
                    io.uop := invalid
                }
            }.elsewhen(funct3 === "b110".U){
                when(funct7 === "b0000000".U){
                    io.uop := isOR
                }.otherwise{
                    io.uop := invalid
                }
            }.elsewhen(funct3 === "b111".U){
                when(funct7 === "b0000000".U){
                    io.uop := isAND
                }.otherwise{
                    io.uop := invalid
                }
            }.elsewhen(funct3 === "b001".U){
                when(funct7 === "b0000000".U){
                    io.uop := isSLL
                }.otherwise{
                    io.uop := invalid
                }
            }.elsewhen(funct3 === "b101".U){
                when(funct7 === "b0000000".U){
                    io.uop := isSRL
                }.elsewhen(funct7 === "b0100000".U){
                    io.uop := isSRA
                }.otherwise{
                    io.uop := invalid
                }
            }.elsewhen(funct3 === "b010".U){
                when(funct7 === "b0000000".U){
                    io.uop := isSLT
                }.otherwise{
                    io.uop := invalid
                }
            }.elsewhen(funct3 === "b011".U){
                when(funct7 === "b0000000".U){
                    io.uop := isSLTU
                }.otherwise{
                    io.uop := invalid
                }
            }.otherwise{
                io.uop := invalid
            }
        }
        is(opcodeT.I_type){
            when(funct3 === "b000".U){
            io.uop := isADDI
            }.otherwise{
            io.uop := invalid
            }
        }
        is(opcodeT.J_type){ io.uop := isJAL  }
        is(opcodeT.JR_type){ io.uop := isJALR }
        is(opcodeT.B_type){
            io.uop := invalid
            switch (funct3){
                is("b000".U){ io.uop := isBEQ}
                is("b001".U){ io.uop := isBNE}
                is("b100".U){ io.uop := isBLT}
                is("b101".U){ io.uop := isBGE}
                is("b110".U){ io.uop := isBLTU}
                is("b111".U){ io.uop := isBGEU}
            }
        }
    }

    val isALUOp = Wire(UInt(1.W))
    val isJump = Wire(UInt(1.W))
    val isImme = Wire(UInt(1.W))

    isALUOp := (opcode === opcodeT.R_type)
    isJump  := ((opcode === opcodeT.J_type) || (opcode === opcodeT.JR_type))
    isImme  := (opcode === opcodeT.I_type)

    io.wrEn := (isALUOp | isJump | isImme)
    io.ALUSrc := Mux((isImme === 1.U), aluOpBImmMux.imme, aluOpBImmMux.forwardMuxB)
    io.memRd := (opcode === opcodeT.L_type)
    io.memWr := (opcode === opcodeT.S_type)
    io.memtoReg := (opcode === opcodeT.L_type)
}

class BranchCheck extends Module{
    val io = IO(new Bundle{
        val instr = Input(UInt(32.W))
        val PC = Input(UInt(32.W))
        val imme = Input(UInt(32.W))
        val operandA = Input(UInt(32.W))
        val operandB = Input(UInt(32.W))
        val PCSrc = Output(UInt(1.W))
        val PC_JB = Output(UInt(32.W))
    })

    val (opcode, opcode_cast4) = opcodeT.safe(io.instr(6,0))
    val (branch_func3, func3_cast1) = branchT.safe(io.instr(14,12))
    assert(opcode_cast4, "Opcode must be a valid one, got %x.", io.instr(6,0))
    assert(func3_cast1, "Opcode must be a valid one, got %x.", io.instr(14,12))

    val branch_condition = Wire(UInt(1.W))

    branch_condition := 0.U
    switch(branch_func3){
        is(branchT.BEQ) {branch_condition := (io.operandA === io.operandB)}
        is(branchT.BNE) {branch_condition := (io.operandA =/= io.operandB)}
        is(branchT.BLT) {branch_condition := (io.operandA.asSInt < io.operandB.asSInt)}
        is(branchT.BGE) {branch_condition := (io.operandA.asSInt >= io.operandB.asSInt)}
        is(branchT.BLTU){branch_condition := (io.operandA.asUInt < io.operandB.asUInt)}
        is(branchT.BGEU){branch_condition := (io.operandA.asUInt >= io.operandB.asUInt)}
    }

    io.PCSrc := ((opcode === opcodeT.J_type) || (opcode === opcodeT.JR_type) || ((opcode === opcodeT.B_type) && (branch_condition === 1.U)))

    io.PC_JB := io.PC + (io.imme << 1.U)
}


class ALU extends Module {
    val io = IO(new Bundle {
        val uop = Input(uopc())
        val operandA = Input(UInt(32.W))
        val operandB = Input(UInt(32.W))
        val aluResult = Output(UInt(32.W))
    })

    when((io.uop === isADD) || (io.uop === isADDI)) {                           
      io.aluResult := io.operandA + io.operandB 
    }.elsewhen(io.uop === isSUB) {  
      io.aluResult := io.operandA - io.operandB 
    }.elsewhen((io.uop === isXOR) || (io.uop === isXORI)) {  
      io.aluResult := io.operandA ^ io.operandB 
    }.elsewhen((io.uop === isOR) || (io.uop === isORI)) {  
      io.aluResult := io.operandA | io.operandB 
    }.elsewhen((io.uop === isAND) || (io.uop === isANDI)) {  
      io.aluResult := io.operandA & io.operandB 
    }.elsewhen((io.uop === isSLL) || (io.uop === isSLLI)) {  
      io.aluResult := io.operandA << io.operandB(4, 0) 
    }.elsewhen((io.uop === isSRL) || (io.uop === isSRLI)) {  
      io.aluResult := io.operandA >> io.operandB(4, 0) 
    }.elsewhen((io.uop === isSRA) || (io.uop === isSRAI)) {  
      io.aluResult := io.operandA >> io.operandB(4, 0)          // automatic sign extension, if SInt datatype is used
    }.elsewhen((io.uop === isSLT) || (io.uop === isSLTI)) {  
      io.aluResult := Mux(io.operandA < io.operandB, 1.U, 0.U)  // automatic sign extension, if SInt datatype is used
    }.elsewhen((io.uop === isSLTU) || (io.uop === isSLTIU)) {  
      io.aluResult := Mux(io.operandA < io.operandB, 1.U, 0.U)
    }.otherwise{
      io.aluResult := "h_FFFF_FFFF".U // = 2^32 - 1; self-defined encoding for invalid operation, value is unlikely to be reached in a regular arithmetic operation
    }

    val zero = (io.operandA === io.operandB) // this is not being used. Brach logic is in the decode stage as a seperate module called "BranchCheck"

}

class ImmediateGen extends Module {
    val io = IO(new Bundle {
        val instr = Input(UInt(32.W))
        val imme = Output(UInt(32.W))
    })

    val (opcode, opcode_cast1) = opcodeT.safe(io.instr(6,0))
    assert(opcode_cast1, "Opcode must be a valid one, got %x.", io.instr(6,0))

    val I_imme = Cat(Fill(20, io.instr(31)), io.instr(31,20))
    val S_imme = Cat(Fill(20, io.instr(31)), io.instr(31,25), io.instr(11,7))
    val B_imme = Cat(Fill(20, io.instr(31)), io.instr(7), io.instr(30,25), io.instr(11,8), 0.U)
    val U_imme = Cat(io.instr(31,12), 0.U)
    val J_imme = Cat(Fill(12, io.instr(31)), io.instr(19,12), io.instr(20), io.instr(30,25), io.instr(24,21), 0.U)

    io.imme := 0.U // default case
    switch(opcode){
        is(opcodeT.I_type)  {io.imme := I_imme}
        is(opcodeT.S_type)  {io.imme := S_imme}
        is(opcodeT.B_type)  {io.imme := B_imme}
        is(opcodeT.U_type)  {io.imme := U_imme}
        is(opcodeT.J_type)  {io.imme := J_imme}
        is(opcodeT.AU_type) {io.imme := U_imme} // AUIPC
        is(opcodeT.JR_type) {io.imme := I_imme} // JALR
        is(opcodeT.L_type)  {io.imme := I_imme} // LOAD
    }
}

class ForwardingUnit extends Module {
    val io = IO(new Bundle {
        // What inputs and / or outputs does the ForwardingUnit_inst unit need?

        // from decode stage
        val rs1_id          = Input(UInt(5.W))
        val rs2_id          = Input(UInt(5.W))
        val uop_id          = Input(uopc())

        // forwarded signals
        val rd_mem          = Input(UInt(5.W))
        val wrEn_mem        = Input(UInt(1.W))
        val rd_wb           = Input(UInt(5.W))
        val wrEn_wb         = Input(UInt(1.W))

        //  alu input mux controllers
        val aluOpA_ctrl = Output(aluOpAMux())
        val aluOpB_ctrl = Output(aluOpBMux())
    })


    /*
        Hazard detetction logic:
        Which pipeline stages are affected and how can a potential hazard be detetced there?
    */
    val rs1_mem_hazard = Wire(UInt(1.W))
    val rs2_mem_hazard = Wire(UInt(1.W))
    val rs1_wb_hazard = Wire(UInt(1.W))
    val rs2_wb_hazard = Wire(UInt(1.W))

    rs1_mem_hazard := ((io.uop_id =/= invalid) && (io.rs1_id === io.rd_mem) && (io.wrEn_mem === 1.U))
    rs1_wb_hazard := ((io.uop_id =/= invalid) && (io.rs1_id === io.rd_wb) && (io.wrEn_wb === 1.U))
    
    rs2_mem_hazard := ((io.uop_id =/= invalid) && (io.uop_id =/= isADDI) && (io.rs2_id === io.rd_mem) && (io.wrEn_mem === 1.U))
    rs2_wb_hazard := ((io.uop_id =/= invalid) && (io.uop_id =/= isADDI) && (io.rs2_id === io.rd_wb) && (io.wrEn_wb === 1.U))


    /*
        Forwarding Selection:
        Select the appropriate value to forward from one stage to another based on the hazard checks.
    */
    // operandA mux
    when (rs1_mem_hazard === 1.U){
        io.aluOpA_ctrl := aluOpAMux.AluResult_mem
    }
    .elsewhen(rs1_wb_hazard === 1.U){
        io.aluOpA_ctrl := aluOpAMux.AluResult_wb
    }
    .otherwise{
        io.aluOpA_ctrl := aluOpAMux.opA_id
    }

    // operandB mux
    when (rs2_mem_hazard === 1.U){
        io.aluOpB_ctrl := aluOpBMux.AluResult_mem
    }
    .elsewhen(rs2_wb_hazard === 1.U){
        io.aluOpB_ctrl := aluOpBMux.AluResult_wb
    }
    .otherwise{
        io.aluOpB_ctrl := aluOpBMux.opB_id
    }

}

class HazardDetectionUnit extends  Module {
    val io = IO(new Bundle {
        val instr = Input(UInt(32.W))
        val ex_RD = Input(UInt(32.W))
        val ex_memRd = Input(UInt(1.W))
        val id_stall = Output(UInt(1.W))
        val if_stall = Output(UInt(1.W))
        val pcWrite = Output(UInt(1.W))
    })

    val (opcode, opcode_cast2) = opcodeT.safe(io.instr(6,0))
    assert(opcode_cast2, "Opcode must be a valid one, got %x.", io.instr(6,0))
    val id_rs1 = io.instr(19,15)
    val id_rs2 = io.instr(24,20)

    val check_rs1 = Wire(UInt(1.W))
    when((opcode === opcodeT.R_type) || (opcode === opcodeT.I_type) || (opcode === opcodeT.S_type) || (opcode === opcodeT.B_type) || (opcode === opcodeT.L_type) || (opcode === opcodeT.JR_type)){check_rs1 := 1.U}
    .otherwise{check_rs1 := 0.U}
    
    val check_rs2 = Wire(UInt(1.W))
    when((opcode === opcodeT.R_type) || (opcode === opcodeT.S_type) || (opcode === opcodeT.B_type) || (opcode === opcodeT.L_type) || (opcode === opcodeT.JR_type)){check_rs2 := 1.U}
    .otherwise{check_rs2 := 0.U}

    when((io.ex_memRd === 1.U) && ((check_rs1 === 1.U) && (io.ex_memRd === id_rs1)) || ((check_rs2 === 1.U) && (io.ex_memRd === id_rs2))){
        io.if_stall := 1.U
        io.id_stall := 1.U
        io.pcWrite := 0.U
    }
    .otherwise{
        io.if_stall := 0.U
        io.id_stall := 0.U
        io.pcWrite := 1.U
    }
}


// -----------------------------------------
// Fetch Stage
// -----------------------------------------

class IF (BinaryFile: String) extends Module {
    val io = IO(new Bundle {
        val instr = Output(UInt(32.W))
        val pc = Output(UInt(32.W))
        val PC_JB = Input(UInt(32.W))
        val PCSrc = Input(UInt(1.W))
        val PCWrite = Input(UInt(1.W))
    })

    val IMem = Mem(4096, UInt(32.W))
    loadMemoryFromFile(IMem, BinaryFile)

    val PC = RegInit(0.U(32.W))
    
    io.instr := IMem(PC>>2.U)
    io.pc := PC

    // Update PC
    when(io.PCWrite === 1.U){
        PC := Mux((io.PCSrc === 0.U), (PC+4.U), io.PC_JB)
    }
}


// -----------------------------------------
// Decode Stage
// -----------------------------------------

class ID extends Module {
    val io = IO(new Bundle {
        val regFileReq_A  = Flipped(new regFileReadReq) 
        val regFileResp_A = Flipped(new regFileReadResp) 
        val regFileReq_B  = Flipped(new regFileReadReq) 
        val regFileResp_B = Flipped(new regFileReadResp) 
        val instr         = Input(UInt(32.W))
        val rd            = Output(UInt(5.W))
        val rs1           = Output(UInt(5.W))
        val rs2           = Output(UInt(5.W))
        val imme          = Output(UInt(32.W))
        val operandA      = Output(UInt(32.W))
        val operandB      = Output(UInt(32.W))
    })

    val opcode  = io.instr(6, 0)
    io.rd      := io.instr(11, 7)
    val funct3  = io.instr(14, 12)
    val rs1     = io.instr(19, 15)

    // R-Type
    val funct7  = io.instr(31, 25)
    val rs2     = io.instr(24, 20)

    val immediateGen_inst = Module(new ImmediateGen)

    immediateGen_inst.io.instr := io.instr
    io.imme := immediateGen_inst.io.imme

    // Operands
    io.regFileReq_A.addr := rs1
    io.regFileReq_B.addr := rs2

    io.operandA := io.regFileResp_A.data
    io.operandB := io.regFileResp_B.data

    io.rs1     := rs1
    io.rs2     := rs2
}

// -----------------------------------------
// Execute Stage
// -----------------------------------------

class EX extends Module {
    val io = IO(new Bundle {
        val uop       = Input(uopc())
        val ALUSrc    = Input(aluOpBImmMux())
        val operandA  = Input(UInt(32.W))
        val operandB  = Input(UInt(32.W))
        val imme      = Input(UInt(32.W))
        val aluResult = Output(UInt(32.W))
    })

    val operandA = io.operandA
    val operandB = Mux((io.ALUSrc === aluOpBImmMux.imme), io.imme, io.operandB)
    val uop      = io.uop

    val alu = Module (new ALU)
    alu.io.uop := io.uop
    alu.io.operandA := operandA
    alu.io.operandB := operandB
    io.aluResult := alu.io.aluResult
}

// -----------------------------------------
// Memory Stage
// -----------------------------------------

class MEM extends Module {
    val io = IO(new Bundle {
        val addr = Input(UInt(32.W))
        val writeData = Input(UInt(32.W))
        val memRd = Input(UInt(1.W))
        val memWr = Input(UInt(1.W))
        val readData = Output(UInt(32.W))
    })

    val DMem = Mem(4096, UInt(32.W))

    when(io.memWr === 1.U){
        DMem.write(io.addr, io.writeData)
    }

    when(io.memRd === 1.U){ io.readData := DMem.read(io.addr) }
    .otherwise{             io.readData := 0.U }

}

// -----------------------------------------
// Writeback Stage
// -----------------------------------------

class WB extends Module {
    val io = IO(new Bundle {
        val regFileReq = Flipped(new regFileWriteReq) 
        val rd         = Input(UInt(5.W))
        val aluResult  = Input(UInt(32.W))
        val memData    = Input(UInt(32.W))
        val memtoReg   = Input(UInt(1.W))
        val wrEn      = Input(UInt(1.W))
        val check_res  = Output(UInt(32.W))
    })

    io.regFileReq.addr  := io.rd
    io.regFileReq.data  := Mux((io.memtoReg === 1.U), io.memData, io.aluResult)
    io.regFileReq.wr_en := io.wrEn
    //  io.regFileReq.wr_en := io.aluResult =/= "h_FFFF_FFFF".U  // could depend on the current uopc, if ISA is extendet beyond R-type and I-type instructions

    io.check_res := io.aluResult

}


// -----------------------------------------
// IF-Barrier
// -----------------------------------------

class IFBarrier extends Module {
    val io = IO(new Bundle {
        val if_stall = Input(UInt(1.W))
        val inInstr  = Input(UInt(32.W))
        val inPC     = Input(UInt(32.W))
        val outInstr = Output(UInt(32.W))
        val outPC    = Output(UInt(32.W))
    })

    val instrReg = RegInit(0.U(32.W))
    val pcReg = RegInit(0.U(32.W))

    when(io.if_stall === 0.U){
        instrReg := io.inInstr
        pcReg    := io.inPC
    }

    io.outInstr := instrReg
    io.outPC    := pcReg

}


// -----------------------------------------
// ID-Barrier
// -----------------------------------------

class IDBarrier extends Module {
    val io = IO(new Bundle {
        val inUOP       = Input(uopc())
        val inRS1       = Input(UInt(5.W))
        val inRS2       = Input(UInt(5.W))
        val inOperandA  = Input(UInt(32.W))
        val inOperandB  = Input(UInt(32.W))
        val inImme      = Input(UInt(32.W))
        val inAluSrc    = Input(aluOpBImmMux())
        val inMemRd     = Input(UInt(1.W))
        val inMemWr     = Input(UInt(1.W))
        val inMemtoReg  = Input(UInt(1.W))
        val inRD        = Input(UInt(5.W))
        val inWrEn      = Input(UInt(1.W))
        val outUOP      = Output(uopc())
        val outRS1      = Output(UInt(5.W))
        val outRS2      = Output(UInt(5.W))
        val outOperandA = Output(UInt(32.W))
        val outOperandB = Output(UInt(32.W))
        val outImme     = Output(UInt(32.W))
        val outAluSrc   = Output(aluOpBImmMux())
        val outMemRd    = Output(UInt(1.W))
        val outMemWr    = Output(UInt(1.W))
        val outMemtoReg = Output(UInt(1.W))
        val outRD       = Output(UInt(5.W))
        val outWrEn     = Output(UInt(1.W))
    })

    io.outUOP := RegNext(io.inUOP, 0.U)
    io.outRD  := RegNext(io.inRD, 0.U)
    io.outRS1 := RegNext(io.inRS1, 0.U)
    io.outRS2 := RegNext(io.inRS2, 0.U)
    io.outOperandA := RegNext(io.inOperandA, 0.U)
    io.outOperandB := RegNext(io.inOperandB, 0.U)
    io.outImme  := RegNext(io.inImme, 0.U)
    io.outAluSrc:= RegNext(io.inAluSrc, 0.U)
    io.outWrEn  := RegNext(io.inWrEn, 0.U)
    io.outMemRd := RegNext(io.inMemRd, 0.U)
    io.outMemWr := RegNext(io.inMemWr, 0.U)
    io.outMemtoReg := RegNext(io.inMemtoReg, 0.U)
}


// -----------------------------------------
// EX-Barrier
// -----------------------------------------

class EXBarrier extends Module {
    val io = IO(new Bundle {
        val inAluResult  = Input(UInt(32.W))
        val inRD         = Input(UInt(5.W))
        val inMemWrData  = Input(UInt(32.W))
        val inMemRd      = Input(UInt(1.W))
        val inMemWr      = Input(UInt(1.W))
        val inMemtoReg   = Input(UInt(1.W))
        val inWrEn       = Input(UInt(1.W))
        val outAluResult = Output(UInt(32.W))
        val outRD        = Output(UInt(5.W))
        val outMemWrData = Output(UInt(32.W))
        val outMemRd     = Output(UInt(1.W))
        val outMemWr     = Output(UInt(1.W))
        val outMemtoReg  = Output(UInt(1.W))
        val outWrEn      = Output(UInt(1.W))
    })

    io.outAluResult := RegNext(io.inAluResult, 0.U)
    io.outRD        := RegNext(io.inRD, 0.U)
    io.outMemWrData := RegNext(io.inMemWrData, 0.U)
    io.outMemRd     := RegNext(io.inMemRd, 0.U)
    io.outMemWr     := RegNext(io.inMemWr, 0.U)
    io.outMemtoReg  := RegNext(io.inMemtoReg, 0.U)
    io.outWrEn      := RegNext(io.inWrEn, 0.U)
}


// -----------------------------------------
// MEM-Barrier
// -----------------------------------------

class MEMBarrier extends Module {
    val io = IO(new Bundle {
        val inAluResult  = Input(UInt(32.W))
        val inMemData    = Input(UInt(32.W))
        val inMemtoReg   = Input(UInt(1.W))
        val inRD         = Input(UInt(5.W))
        val inWrEn       = Input(UInt(1.W))
        val outAluResult = Output(UInt(32.W))
        val outMemData   = Output(UInt(32.W))
        val outMemtoReg  = Output(UInt(1.W))
        val outRD        = Output(UInt(5.W))
        val outWrEn      = Output(UInt(1.W))
    })

    io.outAluResult := RegNext(io.inAluResult, 0.U)
    io.outMemData   := RegNext(io.inMemData, 0.U)
    io.outMemtoReg  := RegNext(io.inMemtoReg, 0.U)
    io.outRD        := RegNext(io.inRD, 0.U)
    io.outWrEn      := RegNext(io.inWrEn, 0.U)

}


// -----------------------------------------
// WB-Barrier
// -----------------------------------------

class WBBarrier extends Module {
    val io = IO(new Bundle {
        val inCheckRes   = Input(UInt(32.W))
        val outCheckRes  = Output(UInt(32.W))
    })

    val check_res   = RegInit(0.U(32.W))

    io.outCheckRes := check_res
    check_res      := io.inCheckRes
}


// -----------------------------------------
// Main Class
// -----------------------------------------

class PipelinedRV32Icore (BinaryFile: String) extends Module {
    val io = IO(new Bundle {
        val check_res = Output(UInt(32.W))
    })

    // Pipeline Registers
    val IFBarrier  = Module(new IFBarrier)
    val IDBarrier  = Module(new IDBarrier)
    val EXBarrier  = Module(new EXBarrier)
    val MEMBarrier = Module(new MEMBarrier)
    val WBBarrier  = Module(new WBBarrier)

    // Pipeline Stages
    val IF  = Module(new IF(BinaryFile))
    val ID  = Module(new ID)
    val EX  = Module(new EX)
    val MEM = Module(new MEM)
    val WB  = Module(new WB)

    val ForwardingUnit_inst = Module(new ForwardingUnit)
    val RegFile_inst = Module(new RegFile)
    val HazardDetectionUnit_inst = Module(new HazardDetectionUnit)
    val ControlUnit_inst = Module(new ControlUnit)
    val BranchCheck_inst = Module(new BranchCheck)

    IF.io.PCWrite   := HazardDetectionUnit_inst.io.pcWrite
    IF.io.PCSrc     := BranchCheck_inst.io.PCSrc
    IF.io.PC_JB     := BranchCheck_inst.io.PC_JB

    IFBarrier.io.inInstr  := IF.io.instr
    IFBarrier.io.inPC     := IF.io.pc
    IFBarrier.io.if_stall := HazardDetectionUnit_inst.io.if_stall

    HazardDetectionUnit_inst.io.instr       := IFBarrier.io.outInstr
    HazardDetectionUnit_inst.io.ex_RD       := IDBarrier.io.outRD
    HazardDetectionUnit_inst.io.ex_memRd    := IDBarrier.io.outMemRd
    
    ControlUnit_inst.io.instr := IFBarrier.io.outInstr

    BranchCheck_inst.io.instr       := IFBarrier.io.outInstr
    BranchCheck_inst.io.PC          := IFBarrier.io.outPC
    BranchCheck_inst.io.imme        := ID.io.imme
    BranchCheck_inst.io.operandA    := ID.io.operandA
    BranchCheck_inst.io.operandB    := ID.io.operandB
    
    ID.io.instr               := IFBarrier.io.outInstr
    ID.io.regFileReq_A        <> RegFile_inst.io.req_1
    ID.io.regFileReq_B        <> RegFile_inst.io.req_2
    ID.io.regFileResp_A       <> RegFile_inst.io.resp_1
    ID.io.regFileResp_B       <> RegFile_inst.io.resp_2
    
    IDBarrier.io.inRS1        := ID.io.rs1
    IDBarrier.io.inRS2        := ID.io.rs2
    IDBarrier.io.inOperandA   := ID.io.operandA
    IDBarrier.io.inOperandB   := ID.io.operandB
    IDBarrier.io.inImme       := ID.io.imme
    IDBarrier.io.inRD         := ID.io.rd
    IDBarrier.io.inWrEn       := Mux((HazardDetectionUnit_inst.io.id_stall === 1.U), 0.U, ControlUnit_inst.io.wrEn)
    IDBarrier.io.inUOP        := Mux((HazardDetectionUnit_inst.io.id_stall === 1.U), uopc.invalid, ControlUnit_inst.io.uop)
    IDBarrier.io.inAluSrc     := Mux((HazardDetectionUnit_inst.io.id_stall === 1.U), aluOpBImmMux.forwardMuxB, ControlUnit_inst.io.ALUSrc)
    IDBarrier.io.inMemRd      := Mux((HazardDetectionUnit_inst.io.id_stall === 1.U), 0.U, ControlUnit_inst.io.memRd)
    IDBarrier.io.inMemWr      := Mux((HazardDetectionUnit_inst.io.id_stall === 1.U), 0.U, ControlUnit_inst.io.memWr)
    IDBarrier.io.inMemtoReg   := Mux((HazardDetectionUnit_inst.io.id_stall === 1.U), 0.U, ControlUnit_inst.io.memtoReg)

    ForwardingUnit_inst.io.rs1_id   := IDBarrier.io.outRS1
    ForwardingUnit_inst.io.rs2_id   := IDBarrier.io.outRS2
    ForwardingUnit_inst.io.uop_id   := IDBarrier.io.outUOP
    ForwardingUnit_inst.io.rd_mem   := EXBarrier.io.outRD
    ForwardingUnit_inst.io.wrEn_mem := EXBarrier.io.outWrEn
    ForwardingUnit_inst.io.rd_wb    := MEMBarrier.io.outRD
    ForwardingUnit_inst.io.wrEn_wb  := MEMBarrier.io.outWrEn

    EX.io.uop       := IDBarrier.io.outUOP
    EX.io.ALUSrc    := IDBarrier.io.outAluSrc
    EX.io.imme      := IDBarrier.io.outImme
    EX.io.operandA  := IDBarrier.io.outOperandA // default case
    EX.io.operandB  := IDBarrier.io.outOperandB // default case
    switch(ForwardingUnit_inst.io.aluOpA_ctrl){
        is(aluOpAMux.opA_id)        {EX.io.operandA := IDBarrier.io.outOperandA}
        is(aluOpAMux.AluResult_mem) {EX.io.operandA := EXBarrier.io.outAluResult}
        is(aluOpAMux.AluResult_wb)  {EX.io.operandA := MEMBarrier.io.outAluResult}
    }
    switch(ForwardingUnit_inst.io.aluOpB_ctrl){
        is(aluOpBMux.opB_id)        {EX.io.operandB := IDBarrier.io.outOperandB}
        is(aluOpBMux.AluResult_mem) {EX.io.operandB := EXBarrier.io.outAluResult}
        is(aluOpBMux.AluResult_wb)  {EX.io.operandB := MEMBarrier.io.outAluResult}
    }

    EXBarrier.io.inAluResult  := EX.io.aluResult
    EXBarrier.io.inRD         := IDBarrier.io.outRD
    EXBarrier.io.inMemWrData  := IDBarrier.io.outOperandB
    EXBarrier.io.inMemRd      := IDBarrier.io.outMemRd
    EXBarrier.io.inMemWr      := IDBarrier.io.outMemWr
    EXBarrier.io.inMemtoReg   := IDBarrier.io.outMemtoReg
    EXBarrier.io.inWrEn       := IDBarrier.io.outWrEn

    MEM.io.addr         := EXBarrier.io.outAluResult
    MEM.io.memRd        := EXBarrier.io.outMemRd
    MEM.io.memWr        := EXBarrier.io.outMemWr
    MEM.io.writeData    := EXBarrier.io.outMemWrData

    MEMBarrier.io.inAluResult := EXBarrier.io.outAluResult
    MEMBarrier.io.inMemData   := MEM.io.readData
    MEMBarrier.io.inRD        := EXBarrier.io.outRD
    MEMBarrier.io.inWrEn      := EXBarrier.io.outWrEn
    MEMBarrier.io.inMemtoReg  := EXBarrier.io.outMemtoReg

    WB.io.rd            := MEMBarrier.io.outRD
    WB.io.aluResult     := MEMBarrier.io.outAluResult
    WB.io.memData       := MEMBarrier.io.outMemData
    WB.io.memtoReg      := MEMBarrier.io.outMemtoReg
    WB.io.wrEn          := MEMBarrier.io.outWrEn
    WB.io.regFileReq    <> RegFile_inst.io.req_3

    WBBarrier.io.inCheckRes   := WB.io.check_res

    io.check_res              := WBBarrier.io.outCheckRes

}

