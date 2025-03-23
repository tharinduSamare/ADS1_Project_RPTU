package core_tile

import chisel3._
import chisel3.experimental.ChiselEnum
import chisel3.util._
import chisel3.util.experimental.loadMemoryFromFile
// import core_tile.opcodeT.{U_type => U_type}


// -----------------------------------------
// Global Definitions and Data Types
// -----------------------------------------

import ALUOpT._
import aluOpAMux._
import aluOpBMux._

class ControlUnit extends  Module {
    val io = IO(new Bundle {
        val instr = Input(UInt(32.W))
        val wrEn = Output(UInt(1.W)) // Register file write back enable
        val ALUOp  = Output(ALUOpT())
        val ALUSrcA  = Output(aluOpAPCMux()) // ALU srcA Mux controller
        val ALUSrcB = Output(aluOpBImmMux()) // ALU srcB Mux controller
        val memRd = Output(UInt(1.W))
        val memWr = Output(UInt(1.W))
        val memtoReg = Output(UInt(1.W))
    })

    val (opcode, opcode_cast3)  = opcodeT.safe(io.instr(6, 0))
    assert(opcode_cast3, "Opcode must be a valid one, got 0x%x.", io.instr(6,0))
    val funct3  = io.instr(14, 12)

    // R-Type
    val funct7  = io.instr(31, 25)
    
    io.ALUOp := invalid //default case
    switch(opcode){
        is(opcodeT.R_type){
            when     ((funct3 === "b000".U) && (funct7 === "b0000000".U)) {io.ALUOp := isADD}
            .elsewhen((funct3 === "b000".U) && (funct7 === "b0100000".U)) {io.ALUOp := isSUB}
            .elsewhen((funct3 === "b100".U) && (funct7 === "b0000000".U)) {io.ALUOp := isXOR}
            .elsewhen((funct3 === "b110".U) && (funct7 === "b0000000".U)) {io.ALUOp := isOR}
            .elsewhen((funct3 === "b111".U) && (funct7 === "b0000000".U)) {io.ALUOp := isAND}
            .elsewhen((funct3 === "b010".U) && (funct7 === "b0000000".U)) {io.ALUOp := isSLT}
            .elsewhen((funct3 === "b011".U) && (funct7 === "b0000000".U)) {io.ALUOp := isSLTU}
            .elsewhen((funct3 === "b001".U) && (funct7 === "b0000000".U)) {io.ALUOp := isSLL}
            .elsewhen((funct3 === "b101".U) && (funct7 === "b0000000".U)) {io.ALUOp := isSRL}
            .elsewhen((funct3 === "b101".U) && (funct7 === "b0100000".U)) {io.ALUOp := isSRA}
            .otherwise {io.ALUOp := invalid}
        }
        is(opcodeT.I_type){
            when     ((funct3 === "b000".U)                             ) {io.ALUOp := isADD}
            .elsewhen((funct3 === "b100".U)                             ) {io.ALUOp := isXOR}
            .elsewhen((funct3 === "b110".U)                             ) {io.ALUOp := isOR}
            .elsewhen((funct3 === "b111".U)                             ) {io.ALUOp := isAND}
            .elsewhen((funct3 === "b010".U)                             ) {io.ALUOp := isSLT}
            .elsewhen((funct3 === "b011".U)                             ) {io.ALUOp := isSLTU}
            .elsewhen((funct3 === "b001".U) && (funct7 === "b0000000".U)) {io.ALUOp := isSLL}
            .elsewhen((funct3 === "b101".U) && (funct7 === "b0000000".U)) {io.ALUOp := isSRL}
            .elsewhen((funct3 === "b101".U) && (funct7 === "b0100000".U)) {io.ALUOp := isSRA}
            .otherwise {io.ALUOp := invalid}
        }
        is(opcodeT.L_type) { io.ALUOp := isADD} // MemAddr = rs1 + imme
        is(opcodeT.S_type) { io.ALUOp := isADD} // MemAddr = rs1 + imme
        is(opcodeT.U_type) { io.ALUOp := isPASSB} // rd -> Imm
        is(opcodeT.AU_type){ io.ALUOp := isADD} // rd -> PC + Imme
        is(opcodeT.J_type) { io.ALUOp := isADD} // rd -> PC + 4
        is(opcodeT.JR_type){ io.ALUOp := isADD } // rd -> PC + 4
        is(opcodeT.B_type) { io.ALUOp := invalid} // branch unit handles this operation
    }

    val isPCRelative = Wire(UInt(1.W))
    val isJump = Wire(UInt(1.W))
    isJump := (opcode === opcodeT.J_type) || (opcode === opcodeT.JR_type) 
    isPCRelative  := (isJump === 1.U) || (opcode === opcodeT.AU_type)

    io.wrEn := (opcode === opcodeT.R_type) || (opcode === opcodeT.I_type) || (opcode === opcodeT.U_type) || (opcode === opcodeT.AU_type) || (opcode === opcodeT.L_type) || (isJump === 1.U)
    io.ALUSrcA := Mux((isPCRelative === 1.U), aluOpAPCMux.PC, aluOpAPCMux.forwardMuxA)
    io.ALUSrcB := Mux((opcode === opcodeT.I_type), aluOpBImmMux.imme, (Mux((isJump === 1.U), aluOpBImmMux.plus4, aluOpBImmMux.forwardMuxB)))
    io.memRd := (opcode === opcodeT.L_type)
    io.memWr := (opcode === opcodeT.S_type)
    io.memtoReg := (opcode === opcodeT.L_type)
}
