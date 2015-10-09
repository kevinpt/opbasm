# PicoBlaze simulator

# Copyright © 2015 Kevin Thibedeau
# (kevin 'period' thibedeau 'at' gmail 'punto' com)
#
# Permission is hereby granted, free of charge, to any person obtaining a
# copy of this software and associated documentation files (the "Software"),
# to deal in the Software without restriction, including without limitation
# the rights to use, copy, modify, merge, publish, distribute, sublicense,
# and/or sell copies of the Software, and to permit persons to whom the
# Software is furnished to do so, subject to the following conditions:
#
# The above copyright notice and this permission notice shall be included in
# all copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
# FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
# DEALINGS IN THE SOFTWARE.

## This module implements a rudimentary PicoBlaze-3 and -6 simulator primarily meant
## to run validation teats against the Opbasm macro libray. It supports the full
## instruction set with the exception of the interrupt instructions (ENABLE/DISABLE,
## RETURNI) and HWBUILD on PB6.

## Hardware differences:
## * Overflowing or underflowing the call stack causes termination
## * Scratchpad is limited to 64 bytes

import strutils, sequtils, tables, unsigned, times, parseopt2, json, sets, math, re, os
import engineering


proc readMemFile(fname: string): ref seq[int32] =
  ## Read the contents of a MEM file into a seq
  var r = new seq[int32]
  try:
    r[] = toSeq(fname.lines).filterIt(not it.startsWith("@")).mapIt(int32, it.parseHexInt.int32)
  except IOError:
    r[] = @[]

  result = r


proc extractSymbols(fname: string): Table[int32, string] =
  var
    s = initTable[int32, string]()

  let symRe = re(r"\s+[*]?\s+(\w+)\s+([0-9A-F]{3})\s+([\w.]+)")

  for ln in fname.lines:
    if ln =~ symRe:
      s[matches[1].parseHexInt.int32] = "$# ($#)" % [matches[0], matches[2]]
      #echo matches[0], "  ", matches[1], "  ",  matches[2]

  result = s


type 
  CommandOptions = tuple[memFile: string,
                        logFile: string,
                        instLimit:int,
                        verbose: bool,
                        trace: bool,
                        quiet: bool,
                        jsonInput: string,
                        jsonOutput: bool,
                        usePB3: bool,
                        usePB6: bool]

type
  simTermination = enum
    termNormal, termInstLimit, termInvalidOpcode, termUnsupportedOpcode, termInvalidAddress,
    termStackOverflow, termStackUnderflow, termInvalidScratchpad

type
  # Base class for peripheral objects
  Peripheral = ref object of RootObj
    name       : string
    writePorts*: seq[uint8]
    readPorts* : seq[uint8]

  # Receive characters through a port and write to stdout
  ConsolePeriph = ref object of Peripheral
    charBuf*: string
    charLog*: seq[string]

  # Terminate the simulation when a port is written to    
  QuitPeriph = ref object of Peripheral
  
  # Copy output port values to the input port
  LoopbackPeriph = ref object of Peripheral

  # Access program memory as a 16-bit ROM
  ROMPeriph = ref object of Peripheral


  
type ProcState = object
  pc: int32
  progc: uint32
  z: bool
  c: bool
  callStack: seq[int32]
  activeBank: range[0..1]
  regs: array[0..1, array[0..15, uint8]]
  portsIn: array[0..255, uint8]
  portsOut: array[0..255, uint8]
  kports: array[0..15, uint8]
  scratchpad: array[0..63, uint8]
  rom: ref seq[int32]
  symbolTable: Table[int32, string]
  totalInsts: int
  termination: simTermination
  executed: HashSet[int32]
  peripherals: seq[Peripheral]
  periphWriteIndex: Table[int, ref seq[Peripheral]]
  periphReadIndex : Table[int, ref seq[Peripheral]]
  periphIndex     : Table[string, Peripheral]




#proc initConsolePeriph(writePorts: seq[uint8]): ConsolePeriph =
#  result = ConsolePeriph(writePorts: writePorts, charBuf: "")

proc newQuitPeriph(name: string, writePorts: seq[uint8]): QuitPeriph =
  new result
  result.name = name
  result.writePorts = writePorts

proc newConsolePeriph(name: string, writePorts: seq[uint8]): ConsolePeriph =
  new result
  result.name = name
  result.writePorts = writePorts
  result.charBuf = ""
  result.charLog = @[]

proc newLoopbackPeriph(name: string, writePorts: seq[uint8]): LoopbackPeriph =
  new result
  result.name = name
  result.writePorts = writePorts

proc newROMPeriph(name: string, writePorts: seq[uint8], readPorts: seq[uint8]): ROMPeriph =
  new result
  result.name = name
  result.writePorts = writePorts
  result.readPorts = readPorts


method init(periph: Peripheral, state: ref ProcState, quiet: bool) =
  discard
  
method portWrite(periph: Peripheral, port: uint8, state: ref ProcState, quiet: bool): bool =
  result = true

method portRead(periph: Peripheral, port: uint8, state: ref ProcState, quiet: bool): bool =
  result = true

method activateInterrupt(periph: Peripheral, state: ref ProcState, quiet: bool) =
  discard

  
method portWrite(periph: ConsolePeriph, port: uint8, state: ref ProcState, quiet: bool): bool =
  let ch = chr(state.portsOut[port])
  
  if ch == '\l':
    if not quiet: echo "> " & periph.charBuf
    periph.charLog.add(periph.charBuf)
    periph.charBuf = ""
  else:
    #echo "### CHAR: ", ch
    periph.charBuf.add(ch)
    
  result = true
 
  
method portWrite(periph: QuitPeriph, port: uint8, state: ref ProcState, quiet: bool): bool =
  if not quiet: echo "Quitting simulation"
  result = false


method portWrite(periph: LoopbackPeriph, port: uint8, state: ref ProcState, quiet: bool): bool =
  #echo "#### LOOPBACK: ", toHex(port.int, 2)
  state.portsIn[port] = state.portsOut[port]
  result = true


method portWrite(periph: ROMPeriph, port: uint8, state: ref ProcState, quiet: bool): bool =
  let address : int32 = state.portsOut[periph.writePorts[0]].int32 shl 8 + state.portsOut[periph.writePorts[1]].int32
  if address < len(state.rom[]):
    let data = state.rom[][address]
    state.portsIn[periph.readPorts[0]] = (data shr 8).uint8
    state.portsIn[periph.readPorts[1]] = (data and 0xFF).uint8
    result = true
  else:
    state.termination = termInvalidAddress
    if not quiet: echo "ERROR: Invalid access to address: 0x", toHex(address, 5)
    result = false



proc getSymbol(state: ref ProcState, address: int32): string =
  if state.symbolTable.hasKey(address):
    result = state.symbolTable[address]
  else:
    result = ""



# Instruction templates

template reportInst(i:expr): expr =
  if show_trace:
    echo toHex(state.pc, 3), " ", toHex(w, 5), ' '.repeat(len(state.callStack)*2 + 1), i

template do_badOp(): expr =
  reportInst("ERROR: Invalid opcode")
  state.termination = termInvalidOpcode
  break
  
template do_unsupportedOp(): expr =
  reportInst("ERROR: Unsupported opcode")
  state.termination = termUnsupportedOpcode
  break

template do_terminate(): expr =
  echo "Terminate at address: ", toHex(state.pc, 4)
  break



# PicoBlaze instructions
# ======================
template do_load(n:expr): expr =
  reportInst("Load  s" & toHex(x,1) & " = 0x" & toHex(n.int,2))
  state.regs[state.activeBank][x] = n


template do_and(n:expr): expr =
  reportInst("And  s" & toHex(x,1) & " & 0x" & toHex(n.int,2))
  state.regs[state.activeBank][x] = state.regs[state.activeBank][x] and n
  state.z = state.regs[state.activeBank][x] == 0
  state.c = false

template do_test(n:expr): expr =
  reportInst("Test  s" & toHex(x,1) & " & 0x" & toHex(n.int,2))
  let r: uint8 = state.regs[state.activeBank][x] and n
  state.z = r == 0
  # Compute parity in C
  var p: uint8 = r xor (r shr 1)
  p = p xor (p shr 2)
  p = p xor (p shr 4)
  state.c = (p and 1) != 0

  
template do_testcy(n:expr): expr =
  reportInst("Testcy  s" & toHex(x,1) & " & 0x" & toHex(n.int,2))
  let r: uint8 = state.regs[state.activeBank][x] and n
  state.z = state.z and (r == 0)
  # Compute parity in C
  var p: uint8 = r xor (r shr 1)
  p = p xor (p shr 2)
  p = p xor (p shr 4)
  p = p xor (if state.c: 1 else: 0)
  state.c = (p and 1) != 0
  
template do_or(n:expr): expr =
  reportInst("Or  s" & toHex(x,1) & " | 0x" & toHex(n.int,2))
  state.regs[state.activeBank][x] = state.regs[state.activeBank][x] or n
  state.z = state.regs[state.activeBank][x] == 0
  state.c = false

template do_xor(n:expr): expr =
  reportInst("Xor  s" & toHex(x,1) & " ^ 0x" & toHex(n.int,2))
  state.regs[state.activeBank][x] = state.regs[state.activeBank][x] xor n
  state.z = state.regs[state.activeBank][x] == 0
  state.c = false


template do_add(n:expr): expr =
  reportInst("Add  s" & toHex(x,1) & " + 0x" & toHex(n.int,2))
  let r: uint16 = state.regs[state.activeBank][x].uint16 + n.uint16
  state.regs[state.activeBank][x] = r.uint8
  state.z = state.regs[state.activeBank][x] == 0
  state.c = (r and 0x100'u16) != 0'u16

template do_addcy(n:expr): expr =
  reportInst("Addcy  s" & toHex(x,1) & " + 0x" & toHex(n.int,2))
  let r: uint16 = state.regs[state.activeBank][x].uint16 + n.uint16 + (if state.c: 1 else: 0)
  state.regs[state.activeBank][x] = r.uint8
  state.z = state.z and (state.regs[state.activeBank][x] == 0)
  state.c = (r and 0x100'u16) != 0'u16

template do_sub(n:expr): expr =
  reportInst("Sub  s" & toHex(x,1) & " - 0x" & toHex(n.int,2))
  let r: uint16 = state.regs[state.activeBank][x].uint16 - n.uint16
  state.regs[state.activeBank][x] = r.uint8
  state.z = state.regs[state.activeBank][x] == 0
  state.c = (r and 0x100'u16) != 0'u16

template do_subcy(n:expr): expr =
  reportInst("Subcy  s" & toHex(x,1) & " - 0x" & toHex(n.int,2))
  let r: uint16 = state.regs[state.activeBank][x].uint16 - n.uint16 - (if state.c: 1 else: 0)
  state.regs[state.activeBank][x] = r.uint8
  state.z = state.z and (state.regs[state.activeBank][x] == 0)
  state.c = (r and 0x100'u16) != 0'u16

template do_compare(n:expr): expr =
  reportInst("Compare  s" & toHex(x,1) & " - 0x" & toHex(n.int,2))
  let r: uint16 = state.regs[state.activeBank][x].uint16 - n.uint16
  #echo "@@@ Compare: ", state.regs[state.activeBank][x].uint16, " ", n.uint16, " -> ", r
  state.z = (r and 0xFF) == 0
  state.c = (r and 0x100'u16) != 0'u16

template do_comparecy(n:expr): expr =
  reportInst("Comparecy  s" & toHex(x,1) & " - 0x" & toHex(n.int,2))
  let r: uint16 = state.regs[state.activeBank][x].uint16 - n.uint16 - (if state.c: 1 else: 0)
  state.z = state.z and ((r and 0xFF) == 0)
  state.c = (r and 0x100'u16) != 0'u16

  
  
template do_sl0(): expr =
  reportInst("Sl0  << s" & toHex(x,1) & " & '0'")
  var r: uint16 = state.regs[state.activeBank][x]
  r = r shl 1
  state.regs[state.activeBank][x] = r.uint8
  state.z = state.regs[state.activeBank][x] == 0
  state.c = (r and 0x100'u16) != 0'u16

template do_sl1(): expr =
  reportInst("Sl1  << s" & toHex(x,1) & " & '1'")
  var r: uint16 = state.regs[state.activeBank][x]
  r = (r shl 1) or 0x01
  state.regs[state.activeBank][x] = r.uint8
  state.z = false
  state.c = (r and 0x100'u16) != 0'u16  

template do_slx(): expr =
  reportInst("Slx  << s" & toHex(x,1) & " & s" & toHex(x,1) & "[0]")
  var r: uint16 = state.regs[state.activeBank][x]
  r = (r shl 1) or (r and 0x01)
  state.regs[state.activeBank][x] = r.uint8
  state.z = state.regs[state.activeBank][x] == 0
  state.c = (r and 0x100'u16) != 0'u16

template do_sla(): expr =
  reportInst("Sla  << s" & toHex(x,1) & " & C")
  var r: uint16 = state.regs[state.activeBank][x]
  r = (r shl 1) or (if state.c: 1 else: 0)
  state.regs[state.activeBank][x] = r.uint8
  state.z = state.regs[state.activeBank][x] == 0
  state.c = (r and 0x100'u16) != 0'u16


template do_sr0(): expr =
  reportInst("Sr0  >> '0' & s" & toHex(x,1))
  var r: uint16 = state.regs[state.activeBank][x]
  state.c = (r and 0x01) != 0'u16
  r = r shr 1
  state.regs[state.activeBank][x] = r.uint8
  state.z = state.regs[state.activeBank][x] == 0

template do_sr1(): expr =
  reportInst("Sr1  >> '1' & s" & toHex(x,1))
  var r: uint16 = state.regs[state.activeBank][x]
  state.c = (r and 0x01) != 0'u16
  r = (r shr 1) or 0x80
  state.regs[state.activeBank][x] = r.uint8
  state.z = false

template do_srx(): expr =
  reportInst("Srx  >> s" & toHex(x,1) & "[0] & s" & toHex(x,1))
  var r: uint16 = state.regs[state.activeBank][x]
  state.c = (r and 0x01) != 0'u16
  r = (r shr 1) or (r and 0x80)
  state.regs[state.activeBank][x] = r.uint8
  state.z = state.regs[state.activeBank][x] == 0

template do_sra(): expr =
  reportInst("Sra  >> C & s" & toHex(x,1))
  var r: uint16 = state.regs[state.activeBank][x]
  let new_c = (r and 0x01) != 0'u16
  r = (r shr 1) or (if state.c: 0x80 else: 0)
  state.regs[state.activeBank][x] = r.uint8
  state.z = state.regs[state.activeBank][x] == 0
  state.c = new_c

  
template do_rl(): expr =
  reportInst("Rl  s" & toHex(x,1))
  var r: uint16 = state.regs[state.activeBank][x]
  r = (r shl 1) or (r shr 7)
  state.regs[state.activeBank][x] = r.uint8
  state.z = state.regs[state.activeBank][x] == 0
  state.c = (r and 0x100'u16) != 0'u16

template do_rr(): expr =
  reportInst("Rr  s" & toHex(x,1))
  var r: uint16 = state.regs[state.activeBank][x]
  state.c = (r and 0x01) != 0'u16
  r = (r shr 1) or (r shl 7)
  state.regs[state.activeBank][x] = r.uint8
  state.z = state.regs[state.activeBank][x] == 0
  
template do_regbank(): expr =
  state.activeBank = (w and 0x01)
  reportInst("Regbank  " & (if state.activeBank == 0: "A" else: "B"))


template do_input(n:expr): expr =
  reportInst("Input  s" & toHex(x,1) & " = port[" & toHex(n.int,2) & "]")

  var success = true

  # Call the read peripherals to let them alter the input port value
  if state.periphReadIndex.hasKey(n.int):
    for periph in state.periphReadIndex[n.int][]:
      success = success and periph.portRead(n, state, options.quiet)
  if not success: # Stop simulation loop
    break

  state.regs[state.activeBank][x] = state.portsIn[n]

template do_output(n:expr): expr =
  reportInst("Output  port[" & toHex(n.int,2) & "] = s" & toHex(x,1))
  state.portsOut[n] = state.regs[state.activeBank][x]
  
  var success = true

  # Notify the write peripherals of the new output port value
  if state.periphWriteIndex.hasKey(n.int):
    for periph in state.periphWriteIndex[n.int][]:
      success = success and periph.portWrite(n, state, options.quiet)
  if not success: # Stop simulation loop
    break
  

template do_outputk(): expr =
  reportInst("Outputk  kport[" & toHex(w and 0x0F,1) & "] = " & toHex((w shr 4) and 0xFF, 2))
  state.kports[w and 0x0F] = ((w shr 4) and 0xFF).uint8



template do_store(n:expr): expr =
  reportInst("Store  s" & toHex(x,1) & " = sp[" & toHex(n.int,2) & "]")
  if n.int < len(state.scratchpad):
    state.scratchpad[n] = state.regs[state.activeBank][x]
  else:
    state.termination = termInvalidScratchpad
    do_terminate()

template do_fetch(n:expr): expr =
  reportInst("Fetch  sp[" & toHex(n.int,2) & "] = s" & toHex(x,1))
  if n.int < len(state.scratchpad):
    state.regs[state.activeBank][x] = state.scratchpad[n]
  else:
    state.termination = termInvalidScratchpad
    do_terminate()


template do_jump(): expr =
  reportInst("Jump  " & state.getSymbol(address))
  next_pc = address

template do_jump_z(): expr =
  reportInst("Jump Z  " & $state.z & "  " & state.getSymbol(address))
  if state.z: next_pc = address

template do_jump_nz(): expr =
  reportInst("Jump NZ  " & $(not state.z) & "  " & state.getSymbol(address))
  if not state.z: next_pc = address

template do_jump_c(): expr =
  reportInst("Jump C  " & $state.c & "  " & state.getSymbol(address))
  if state.c: next_pc = address

template do_jump_nc(): expr =
  reportInst("Jump NC  " & $(not state.c) & "  " & state.getSymbol(address))
  if not state.c: next_pc = address

template do_jump_at(): expr =
  reportInst("Jump@")
  next_pc = ((state.regs[state.activeBank][x].int32 shl 8) or state.regs[state.activeBank][y].int32) and 0xFFF


template do_call_push(n: expr): expr =
  if len(state.callStack) < 30: # Max levels on KCPSM6
    state.callStack.add(n)
  else:
    state.termination = termStackOverflow
    do_terminate()
  
template do_call(): expr =
  reportInst("Call  " & state.getSymbol(address))
  next_pc = address
  do_call_push(state.pc+1)

template do_call_z(): expr =
  reportInst("Call Z  " & $state.z & "  " & state.getSymbol(address))
  if state.z:
    next_pc = address
    do_call_push(state.pc+1)

template do_call_nz(): expr =
  reportInst("Call NZ  " & $(not state.z) & "  " & state.getSymbol(address))
  if not state.z:
    next_pc = address
    do_call_push(state.pc+1)

template do_call_c(): expr =
  reportInst("Call C  " & $state.c & "  " & state.getSymbol(address))
  if state.c:
    next_pc = address
    do_call_push(state.pc+1)

template do_call_nc(): expr =
  reportInst("Call NC  " & $(not state.c) & "  " & state.getSymbol(address))
  if not state.c:
    next_pc = address
    do_call_push(state.pc+1)

template do_call_at(): expr =
  reportInst("Call@")
  next_pc = ((state.regs[state.activeBank][x].int32 shl 8) or state.regs[state.activeBank][y].int32) and 0xFFF
  do_call_push(state.pc+1)



template do_popCall(): expr =
  if len(state.callStack) > 0:
    next_pc = state.callStack.pop()
  else:
    state.termination = termStackUnderflow
    do_terminate()

template do_return(): expr =
  reportInst("Return")
  do_popCall()
  
template do_return_z(): expr =
  reportInst("Return Z  " & $state.z)
  if state.z: do_popCall()

template do_return_nz(): expr =
  reportInst("Return NZ  " & $(not state.z))
  if not state.z: do_popCall()

template do_return_c(): expr =
  reportInst("Return C  " & $state.c)
  if state.c: do_popCall()

template do_return_nc(): expr =
  reportInst("Return NC  " & $(not state.c))
  if not state.c: do_popCall()

  
template do_load_and_return(): expr =
  reportInst("Load&return  s" & toHex(x, 1) & " = " & toHex(imm.int, 2))
  state.regs[state.activeBank][x] = imm
  do_popCall()

template do_star(): expr =
  reportInst("Star s$# ($#) = s$# ($#)" % [toHex(x,1), (if state.activeBank == 0: "B" else: "A"),
                                          toHex(y,1), (if state.activeBank == 0: "A" else: "B")])
  state.regs[1 - state.activeBank][x] = state.regs[state.activeBank][y]




proc newProcState(periphs: openarray[Peripheral], jsonInput: array[0..255, uint8]): ref ProcState =
  var state: ref ProcState
  new state

  state.pc = 0
  state.termination = termNormal
  state.executed = initSet[int32](1024)
  state.callStack = @[]
  state.portsIn = jsonInput
  state.peripherals = @periphs
  
  
  # Index which peripherals are on each in/out port
  state.periphWriteIndex = initTable[int, ref seq[Peripheral]]()
  state.periphReadIndex  = initTable[int, ref seq[Peripheral]]()
  state.periphIndex = initTable[string, Peripheral]()
  
  for periph in state.peripherals:
    for port in periph.writePorts:
      if not state.periphWriteIndex.hasKey(port.int):
        state.periphWriteIndex[port.int] = new seq[Peripheral]
        state.periphWriteIndex[port.int][] = @[]

      state.periphWriteIndex[port.int][].add(periph)

    for port in periph.readPorts:
      if not state.periphReadIndex.hasKey(port.int):
        state.periphReadIndex[port.int] = new seq[Peripheral]
        state.periphReadIndex[port.int][] = @[]

      state.periphReadIndex[port.int][].add(periph)

    state.periphIndex[periph.name] = periph

  result = state


template simulateLoop(): expr =
  ## Main simulation loop
  #  Terminates normally when an OUTPUT instruction targeting quitPort executes 
    
  state.rom = rom
  state.symbolTable = symbolTable
  
  for i in 0..<options.instLimit:

    # Fetch next instruction
    if state.pc < len(state.rom[]):
      w = state.rom[state.pc]
    else:
      state.termination = termInvalidAddress
      do_terminate()

    # Decode common fields
    opc = (w shr 12) and 0xFF
    x = (w shr 8) and 0xF
    y = (w shr 4) and 0xF
    imm = w and 0xFF
    
    next_pc = state.pc + 1
    state.totalInsts += 1
    state.executed.incl(state.pc)

    if options.usePB6:
      address = w and 0xFFF

      # Decode PB6 opcodes
      case opc
      of 0x00: do_load(state.regs[state.activeBank][y])
      of 0x01: do_load(imm)
      of 0x02: do_and(state.regs[state.activeBank][y])
      of 0x03: do_and(imm)
      of 0x04: do_or(state.regs[state.activeBank][y])
      of 0x05: do_or(imm)
      of 0x06: do_xor(state.regs[state.activeBank][y])
      of 0x07: do_xor(imm)
      
      of 0x10: do_add(state.regs[state.activeBank][y])
      of 0x11: do_add(imm)
      of 0x12: do_addcy(state.regs[state.activeBank][y])
      of 0x13: do_addcy(imm)
      
      of 0x18: do_sub(state.regs[state.activeBank][y])
      of 0x19: do_sub(imm)
      of 0x1A: do_subcy(state.regs[state.activeBank][y])
      of 0x1B: do_subcy(imm)
      
      of 0x0C: do_test(state.regs[state.activeBank][y])
      of 0x0D: do_test(imm)
      of 0x0E: do_testcy(state.regs[state.activeBank][y])
      of 0x0F: do_testcy(imm)
      of 0x1C: do_compare(state.regs[state.activeBank][y])
      of 0x1D: do_compare(imm)
      of 0x1E: do_comparecy(state.regs[state.activeBank][y])
      of 0x1F: do_comparecy(imm)
      
      of 0x14:
        let sc = w and 0xFF'i32
        case sc
        of 0x06: do_sl0()
        of 0x07: do_sl1()
        of 0x04: do_slx()
        of 0x00: do_sla()
        of 0x02: do_rl()
        of 0x0E: do_sr0()
        of 0x0F: do_sr1()
        of 0x0A: do_srx()
        of 0x08: do_sra()
        of 0x0C: do_rr()
        of 0x80: do_unsupportedOp() # HWBUILD
        else: do_badOp

      of 0x37: do_regbank()
      
      of 0x08: do_input(state.regs[state.activeBank][y])
      of 0x09: do_input(imm)
      of 0x2C: do_output(state.regs[state.activeBank][y])
      of 0x2D: do_output(imm)
      of 0x2B: do_outputk()
      
      of 0x2E: do_store(state.regs[state.activeBank][y])
      of 0x2F: do_store(imm)
      of 0x0A: do_fetch(state.regs[state.activeBank][y])
      of 0x0B: do_fetch(imm)
      
      of 0x28: do_unsupportedOp()  # DISABLE / ENABLE
      of 0x29: do_unsupportedOp()  # RETURNI
      
      of 0x22: do_jump()
      of 0x32: do_jump_z()
      of 0x36: do_jump_nz()
      of 0x3A: do_jump_c()
      of 0x3E: do_jump_nc()
      of 0x26: do_jump_at()
      
      of 0x20: do_call()
      of 0x30: do_call_z()
      of 0x34: do_call_nz()
      of 0x38: do_call_c()
      of 0x3C: do_call_nc()
      of 0x24: do_call_at()
      
      of 0x25: do_return()
      of 0x31: do_return_z()
      of 0x35: do_return_nz()
      of 0x39: do_return_c()
      of 0x3D: do_return_nc()
      
      of 0x21: do_load_and_return()
      
      of 0x16: do_star()
      
      else: do_badOp

      next_pc = next_pc and 0xFFF
      
      
    else:
      address = w and 0x3FF
      # Decode PB3 opcodes
      case opc
      of 0x01: do_load(state.regs[state.activeBank][y])
      of 0x00: do_load(imm)
      of 0x0B: do_and(state.regs[state.activeBank][y])
      of 0x0A: do_and(imm)
      of 0x0D: do_or(state.regs[state.activeBank][y])
      of 0x0C: do_or(imm)
      of 0x0F: do_xor(state.regs[state.activeBank][y])
      of 0x0E: do_xor(imm)
      
      of 0x19: do_add(state.regs[state.activeBank][y])
      of 0x18: do_add(imm)
      of 0x1B: do_addcy(state.regs[state.activeBank][y])
      of 0x1A: do_addcy(imm)
      
      of 0x1D: do_sub(state.regs[state.activeBank][y])
      of 0x1C: do_sub(imm)
      of 0x1F: do_subcy(state.regs[state.activeBank][y])
      of 0x1E: do_subcy(imm)
      
      of 0x13: do_test(state.regs[state.activeBank][y])
      of 0x12: do_test(imm)
      of 0x15: do_compare(state.regs[state.activeBank][y])
      of 0x14: do_compare(imm)
      
      of 0x20:
        let sc = w and 0xFF'i32
        case sc
        of 0x06: do_sl0()
        of 0x07: do_sl1()
        of 0x04: do_slx()
        of 0x00: do_sla()
        of 0x02: do_rl()
        of 0x0E: do_sr0()
        of 0x0F: do_sr1()
        of 0x0A: do_srx()
        of 0x08: do_sra()
        of 0x0C: do_rr()
        of 0x80: do_unsupportedOp() # HWBUILD
        else: do_badOp

      of 0x05: do_input(state.regs[state.activeBank][y])
      of 0x04: do_input(imm)
      of 0x2D: do_output(state.regs[state.activeBank][y])
      of 0x2C: do_output(imm)
      #of 0x2B: do_outputk()
      
      of 0x2F: do_store(state.regs[state.activeBank][y])
      of 0x2E: do_store(imm)
      of 0x07: do_fetch(state.regs[state.activeBank][y])
      of 0x06: do_fetch(imm)
      
      of 0x3C: do_unsupportedOp()  # DISABLE / ENABLE
      of 0x38: do_unsupportedOp()  # RETURNI
      
      of 0x34: do_jump()
      of 0x35:
        let cc = (w shr 10) and 0x03
        case cc
        of 0x00: do_jump_z()
        of 0x01: do_jump_nz()
        of 0x02: do_jump_c()
        of 0x03: do_jump_nc()
      
      of 0x30: do_call()
      of 0x31:
        let cc = (w shr 10) and 0x03
        case cc
        of 0x00: do_call_z()
        of 0x01: do_call_nz()
        of 0x02: do_call_c()
        of 0x03: do_call_nc()
      
      of 0x2A: do_return()
      of 0x2B:
        let cc = (w shr 10) and 0x03
        case cc
        of 0x00: do_return_z()
        of 0x01: do_return_nz()
        of 0x02: do_return_c()
        of 0x03: do_return_nc()
      
      else: do_badOp

      next_pc = next_pc and 0x3FF

    
    state.pc = next_pc

    if not show_trace and not options.quiet and (state.totalInsts mod 1_000_000 == 0):
      stdout.write(".")
      stdout.flushFile()

  if not show_trace and not options.quiet: echo ""

  if state.totalInsts == options.instLimit:
    state.termination = termInstLimit


proc simulate(state: ref ProcState, rom: ref seq[int32], options: CommandOptions, symbolTable: Table[int32, string]) =
  var
    w, opc, x, y, address, next_pc: int32
    imm: uint8
  
  # Instantiate two versions of the simulation loop
  # This avoids testing the trace setting on every instruction and instead uses
  # templates to expand two variants of the simulation loop.
  if options.trace:
    const show_trace = true
    simulateLoop
  else:
    const show_trace = false
    simulateLoop
  

const usageString = """
Open PicoBlaze simulator
Usage: opbsim [OPTIONS]

Options:
  -m:MEM_FILE --mem:MEM_FILE    Input mem file
  -i:JSON_IN  --input:JSON_IN   JSON input string
  --log:LOG_FILE                Log file with symbol table
  -L:NUM        --limit:NUM     Limit to NUM instructions executed
  -v            --verbose       Verbose output
  -t            --trace         Print execution trace
  -q            --quiet         Quiet output
  -j            --json          JSON report [forces quiet too]
  --pb3                         Simulate PicoBlase-3 code
  --pb6                         Simulate PicoBlaze-6 code [default]
"""



proc main() =

  var options : CommandOptions
  
  options.instLimit = 100_000_000
  
  # Parse command line
  for kind, key, val in getopt():
    case kind
    of cmdShortOption, cmdLongOption:
      case key
      of "h"           : echo usageString; return
      of "mem", "m"  : options.memFile = val
      of "limit", "L"  : options.instLimit = parseInt(val)
      of "log",        : options.logFile = val
      of "verbose", "v": options.verbose = true
      of "trace", "t"  : options.trace = true
      of "quiet", "q"  : options.quiet = true
      of "input", "i"  : options.jsonInput = val
      of "json", "j"   : options.jsonOutput = true
      of "pb3"         : options.usePB3 = true
      of "pb6"         : options.usePB6 = true
      else             : discard
    else: discard
    
  if options.memFile == "" or options.memFile == nil: echo usageString; return
  if options.usePB3 and options.usePB6:
    echo "Invalid options: Select only one PicoBlaze architecture"
  if not (options.usePB3 or options.usePB6): options.usePB6 = true


  if options.jsonOutput:
    options.verbose = false
    options.trace = false
    options.quiet = true
  
  var jsonInput : array[0..255, uint8]
  
  if options.jsonInput != "" and options.jsonInput != nil:
    let jobj = parseJson(options.jsonInput)
    #echo "### ", $jobj.kind
    assert jobj.kind == JObject
    #echo "JSON: ", $jobj
    
    for f in jobj.pairs:
      var offset = parseHexInt(f.key)
      
      for i in f.val:
        if offset > jsonInput.high: break
        
        jsonInput[offset] = i.getNum.uint8
        offset.inc
    
    #echo "Input: ", $(@jsonInput)
  

  if not options.quiet:
    echo "PicoBlaze simulator"
    echo "Running in ", (if options.usePB6: "PicoBlaze-6" else: "PicoBlaze-3"), " mode"
    echo "Input: ", options.memFile
  
  var rom: ref seq[int32] = readMemFile(options.memFile)
  if len(rom[]) == 0: echo "ERROR: Invalid MEM file"; quit(1)
  
  if not options.quiet:
    echo "Read $# words\n" % [$len(rom[])]

  var symbolTable: Table[int32, string]

  # Look for log file if one wasn't provided in arguments
  if options.logFile == "" or options.logFile == nil:
    let elems = splitFile(options.memFile)
    let logFile = elems.dir / (elems.name & ".log")
    if fileExists(logFile):
      options.logFile = logFile

  if options.logFile != "" and options.logFile != nil:
    symbolTable = extractSymbols(options.logFile)
    if not options.quiet:
      echo "Found $# symbols in $#\n" % [$len(symbolTable), options.logFile]
  else:
    symbolTable = initTable[int32, string]()


  # Instantiate the peripherals
  const quitPort: uint8 = 0xFF
  let con = newConsolePeriph("console", @[0xFE'u8])
  let periphs = [con,
                newQuitPeriph("quit", @[quitPort]),
                newLoopbackPeriph("loopback", toSeq(0x00'u8 .. 0x0F'u8)),
                newROMPeriph("ROM", @[0xFA'u8, 0xFB'u8], @[0xFA'u8, 0xFB'u8])
                ]

  var state = newProcState(periphs, jsonInput)

  # Run the simulator
  let t1 = cpuTime()
  state.simulate(rom, options, symbolTable)
  let t2 = cpuTime()
  
  let cpuRuntime = t2 - t1
  
  if options.verbose:
    # Report results
    echo "\n     A    B"
    for i in 0..15:
      echo "s$1 = $2   $3" % [toHex(i,1), toHex(state.regs[0][i].int32,2), toHex(state.regs[1][i].int32,2)]

    echo "\nScratchpad:"
    for i in 0..<len(state.scratchpad) /% 16:
      echo toHex(i*16, 2), " : ", state.scratchpad[i*16..<(i+1)*16].mapIt(string, toHex(it.int32,2)).join(" ")

    echo "\nOut ports:"
    for i in 0..<len(state.portsOut) /% 16:
      echo toHex(i*16, 2), " : ", state.portsOut[i*16..<(i+1)*16].mapIt(string, toHex(it.int32,2)).join(" ")

  # Determine the number of valid instructions in the ROM
  # Assume the last instruction is a default_jump or uninitialized 0's
  let nonInst = state.rom[state.rom[].high]
  var instCount = 0
  for i in 0..state.rom[].high:
    if state.rom[i] != nonInst:
      instCount = instCount + 1
      
  if not options.quiet:
    let simTime = state.totalInsts.float * 2.0 / 50.0e6
    echo "\nExecuted $1 instructions ($2 @ 50 MHz, CPU time = $3, $4x realtime)" %
        [$state.totalInsts, formatSI(simTime, "s", 0), formatSI(cpuRuntime, "s", 0), formatFloat(simTime / cpuRuntime, ffDecimal, 2)]
    echo "$1 instructions visited of $2 total ($3%)" % [$state.executed.len, $instCount, $round(state.executed.len / instCount * 100.0)]
    echo "$1 testbench error(s)" % [$state.portsOut[quitPort]]
    
    
    case state.termination
    of termNormal:            echo "Normal termination"
    of termInvalidOpcode:     echo "UNEXPECTED TERMINATION: Invalid opcode"
    of termInvalidAddress:    echo "UNEXPECTED TERMINATION: Invalid address"
    of termUnsupportedOpcode: echo "UNEXPECTED TERMINATION: Unsupported opcode"
    of termInstLimit:         echo "UNEXPECTED TERMINATION: Reached instruction limit"
    of termInvalidScratchpad: echo "UNEXPECTED TERMINATION: Invalid scratchpad address"
    of termStackUnderflow:    echo "UNEXPECTED TERMINATION: PC stack underflow"
    of termStackOverflow:     echo "UNEXPECTED TERMINATION: PC stack overflow"
    else:                     echo "UNEXPECTED TERMINATION: UNKNOWN"
    
    
  if options.jsonOutput:
    let
      jsonData = newJObject()

    jsonData["regs_a"] = %(state.regs[0].mapIt(JsonNode, %(it.int)))
    jsonData["regs_b"] = %(state.regs[1].mapIt(JsonNode, %(it.int)))
    jsonData["scratchpad"] = %(state.scratchpad.mapIt(JsonNode, %(it.int)))
    jsonData["ports_in"] = %(state.portsIn.mapIt(JsonNode, %(it.int)))
    jsonData["ports_out"] = %(state.portsOut.mapIt(JsonNode, %(it.int)))
    jsonData["total_insts"] = %state.totalInsts
    jsonData["cpu_runtime"] = %cpuRuntime
    jsonData["executed"] = %state.executed.len
    jsonData["inst_count"] = %instCount
    jsonData["termination"] = %($state.termination)
    
    #let con: ConsolePeriph = state.periphIndex["console"]
    jsonData["console"] = %con.charLog.join("\n")

    echo($(jsonData))

  let returnCode = (if state.termination == termNormal and state.portsOut[quitPort] == 0: 0 else: 1)
  quit(returnCode)

  
when isMainModule:
  main()
