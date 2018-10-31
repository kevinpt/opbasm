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
## to run validation tests against the Opbasm macro library. It supports the full
## instruction set for both architectures.

## Hardware differences:
## * Overflowing or underflowing the call stack causes termination

import strutils, sequtils, tables, times, parseopt2, json, sets, math, nre, os, options
import engineering

proc getVersion(fname: string): string {.compileTime.} =
  for ln in staticRead(fname).splitLines:
    if ln.startsWith("__version__"):
      return ln.split('=')[1].strip().strip(chars={'\''})
  return "??"

const buildVersion = getVersion("../opbasm/opbasm.py")


proc readMemFile(fname: string): ref seq[int32] =
  ## Read the contents of a MEM file into a seq
  var r = new seq[int32]
  try:
    r[] = sequtils.toSeq(lines(fname)).filterIt(not it.startsWith("@")).mapIt(it.parseHexInt.int32)
  except IOError:
    r[] = @[]

  result = r



proc extractSymbols(fname: string): Table[int32, string] =
  ## Read the symbol definitions from a log file
  var s = initTable[int32, string]()

  let symRe = nre.re(r"\s+[*]?\s+(\w+)\s+([0-9A-F]{3})\s+([\w.]+)")

  for ln in fname.lines:
    let m = ln.match(symRe)
    if isSome(m):
      s[m.get.captures[1].parseHexInt.int32] = "$# ($#)" % [m.get.captures[0], m.get.captures[2]]

  result = s



type 
  CommandOptions = tuple
    memFile: string
    logFile: string
    scratchSize: int
    instLimit: int
    verbose: bool
    trace: bool
    quiet: bool
    jsonInput: string
    jsonOutput: bool
    listPeriphs : bool
    usePB3: bool
    usePB6: bool
    version: bool

type
  simTermination = enum
    termNormal, termInstLimit, termInvalidOpcode, termUnsupportedOpcode, termInvalidAddress,
    termStackOverflow, termStackUnderflow, termInvalidScratchpad

type RomInfo = tuple
  data        : seq[int32]
  symbolTable : Table[int32, string]


type ExecFlags = tuple
  z : bool
  c : bool
  activeBank : range[0..1]

type
  # Base class for peripheral objects
  Peripheral = ref object of RootObj
    name       : string
    writePorts*: seq[uint8]
    readPorts* : seq[uint8]

  
type ProcState = object
  pc: int32
  
  curFlags   : ExecFlags
  savedFlags : ExecFlags
  
  intFlag   : bool
  intActive : bool
  intVec    : int32
  
  callStack: seq[int32]
  regs: array[0..1, array[0..15, uint8]]
  portsIn: array[0..255, uint8]
  portsOut: array[0..255, uint8]
  kports: array[0..15, uint8]
  scratchpad: array[0..255, uint8]
  scratchMask: uint8

  rom: ref RomInfo
  hwbuild : uint8
  
  totalInsts: int
  termination: simTermination
  executed: HashSet[int32]
  
  peripherals: seq[Peripheral]
  periphWriteIndex: Table[int, ref seq[Peripheral]]
  periphReadIndex : Table[int, ref seq[Peripheral]]
  periphIndex     : Table[string, Peripheral]



proc getSymbol(state: ref ProcState, address: int32): string =
  ## Lookup the name of a symbol associated with an address.
  ## Returns an empty string if no symbol is found.
  if state.rom.symbolTable.hasKey(address):
    result = state.rom.symbolTable[address]
  else:
    result = ""



#### Peripheral base methods ####
#################################
method init(periph: Peripheral, state: ref ProcState, quiet: bool) {.base.} =
  discard
  
method portWrite(periph: Peripheral, port: uint8, state: ref ProcState, quiet: bool): bool {.base.} =
  result = true

method portRead(periph: Peripheral, port: uint8, state: ref ProcState, quiet: bool): bool {.base.} =
  result = true

method activateInterrupt(periph: Peripheral, state: ref ProcState, quiet: bool) {.base.} =
  if state.intActive:
    state.intFlag = true


#### Console peripheral ####
############################
type
  # Receive characters through a port and write to stdout
  ConsolePeriph = ref object of Peripheral
    charBuf*: string
    charLog*: seq[string]

proc newConsolePeriph(name: string, writePorts: seq[uint8]): ConsolePeriph =
  new result
  result.name = name
  result.writePorts = writePorts
  result.charBuf = ""
  result.charLog = @[]

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


#### Quit peripheral ####
#########################
type
  # Terminate the simulation when a port is written to
  QuitPeriph = ref object of Peripheral

proc newQuitPeriph(name: string, writePorts: seq[uint8]): QuitPeriph =
  new result
  result.name = name
  result.writePorts = writePorts

method portWrite(periph: QuitPeriph, port: uint8, state: ref ProcState, quiet: bool): bool =
  if not quiet: echo "Quitting simulation"
  result = false


#### Loopback peripheral ####
#############################
type
  # Copy output port values to the input port
  LoopbackPeriph = ref object of Peripheral

proc newLoopbackPeriph(name: string, writePorts: seq[uint8]): LoopbackPeriph =
  new result
  result.name = name
  result.writePorts = writePorts

method portWrite(periph: LoopbackPeriph, port: uint8, state: ref ProcState, quiet: bool): bool =
  #echo "#### LOOPBACK: ", toHex(port.int, 2)
  state.portsIn[port] = state.portsOut[port]
  result = true


#### ROM peripheral ####
########################
type
  # Access program memory as a 16-bit ROM
  ROMPeriph = ref object of Peripheral

proc newROMPeriph(name: string, writePorts: seq[uint8], readPorts: seq[uint8]): ROMPeriph =
  new result
  result.name = name
  result.writePorts = writePorts
  result.readPorts = readPorts

method portWrite(periph: ROMPeriph, port: uint8, state: ref ProcState, quiet: bool): bool =
  let address : int32 = state.portsOut[periph.writePorts[0]].int32 shl 8 + state.portsOut[periph.writePorts[1]].int32
  if address < len(state.rom.data):
    let data = state.rom.data[address]
    state.portsIn[periph.readPorts[0]] = (data shr 8).uint8
    state.portsIn[periph.readPorts[1]] = (data and 0xFF).uint8
    result = true
  else:
    state.termination = termInvalidAddress
    if not quiet: echo "ERROR: Invalid access to address: 0x", toHex(address, 5)
    result = false


#### Interrupt peripheral ####
##############################
type
  # Generate an interrupt when written to
  InterruptPeriph = ref object of Peripheral

proc newInterruptPeriph(name: string, writePorts: seq[uint8]): InterruptPeriph =
  new result
  result.name = name
  result.writePorts = writePorts

method portWrite(periph: InterruptPeriph, port: uint8, state: ref ProcState, quiet: bool): bool =
  periph.activateInterrupt(state, quiet);
  result = true


#### Counter peripheral ####
############################
type  
  # Count instruction cycles
  CounterPeriph = ref object of Peripheral
    startTime: int

proc newCounterPeriph(name: string, writePorts: seq[uint8], readPorts: seq[uint8]): CounterPeriph =
  new result
  result.name = name
  result.writePorts = writePorts
  result.readPorts = readPorts

method portWrite(periph: CounterPeriph, port: uint8, state: ref ProcState, quiet: bool): bool =
  let value = state.portsOut[periph.writePorts[0]]

  if value == 1: # Start timer
    periph.startTime = state.totalInsts

  elif value == 0: # End timer
    let cycles = state.totalInsts - periph.startTime
    state.portsIn[periph.readPorts[0]] = (cycles and 0xFF).uint8
    state.portsIn[periph.readPorts[1]] = ((cycles shr 8) and 0xFF).uint8
    state.portsIn[periph.readPorts[2]] = ((cycles shr 16) and 0xFF).uint8
    state.portsIn[periph.readPorts[3]] = ((cycles shr 24) and 0xFF).uint8
    periph.startTime = 0

  result = true




#### Instruction templates ####
###############################

template reportInst(i:untyped): untyped =
  if show_trace:
    echo toHex(state.pc, 3), " ", toHex(w, 5), ' '.repeat(len(state.callStack)*2 + 1), i

template do_badOp(): untyped =
  reportInst("ERROR: Invalid opcode")
  state.termination = termInvalidOpcode
  break
  
template do_unsupportedOp(): untyped =
  reportInst("ERROR: Unsupported opcode")
  state.termination = termUnsupportedOpcode
  break

template do_terminate(): untyped =
  echo "Terminate at address: ", toHex(state.pc, 4)
  break



# PicoBlaze instructions
# ======================
template do_load(n:untyped): untyped =
  reportInst("Load  s" & toHex(x,1) & " = 0x" & toHex(n.int,2))
  state.regs[state.curFlags.activeBank][x] = n


template do_and(n:untyped): untyped =
  reportInst("And  s" & toHex(x,1) & " & 0x" & toHex(n.int,2))
  state.regs[state.curFlags.activeBank][x] = state.regs[state.curFlags.activeBank][x] and n
  state.curFlags.z = state.regs[state.curFlags.activeBank][x] == 0
  state.curFlags.c = false

template do_test(n:untyped): untyped =
  reportInst("Test  s" & toHex(x,1) & " & 0x" & toHex(n.int,2))
  let r: uint8 = state.regs[state.curFlags.activeBank][x] and n
  state.curFlags.z = r == 0
  # Compute parity in C
  var p: uint8 = r xor (r shr 1)
  p = p xor (p shr 2)
  p = p xor (p shr 4)
  state.curFlags.c = (p and 1) != 0

  
template do_testcy(n:untyped): untyped =
  reportInst("Testcy  s" & toHex(x,1) & " & 0x" & toHex(n.int,2))
  let r: uint8 = state.regs[state.curFlags.activeBank][x] and n
  state.curFlags.z = state.curFlags.z and (r == 0)
  # Compute parity in C
  var p: uint8 = r xor (r shr 1)
  p = p xor (p shr 2)
  p = p xor (p shr 4)
  p = p xor (if state.curFlags.c: 1 else: 0)
  state.curFlags.c = (p and 1) != 0
  
template do_or(n:untyped): untyped =
  reportInst("Or  s" & toHex(x,1) & " | 0x" & toHex(n.int,2))
  state.regs[state.curFlags.activeBank][x] = state.regs[state.curFlags.activeBank][x] or n
  state.curFlags.z = state.regs[state.curFlags.activeBank][x] == 0
  state.curFlags.c = false

template do_xor(n:untyped): untyped =
  reportInst("Xor  s" & toHex(x,1) & " ^ 0x" & toHex(n.int,2))
  state.regs[state.curFlags.activeBank][x] = state.regs[state.curFlags.activeBank][x] xor n
  state.curFlags.z = state.regs[state.curFlags.activeBank][x] == 0
  state.curFlags.c = false


template do_add(n:untyped): untyped =
  reportInst("Add  s" & toHex(x,1) & " + 0x" & toHex(n.int,2))
  let r: uint16 = state.regs[state.curFlags.activeBank][x].uint16 + n.uint16
  state.regs[state.curFlags.activeBank][x] = r.uint8
  state.curFlags.z = state.regs[state.curFlags.activeBank][x] == 0
  state.curFlags.c = (r and 0x100'u16) != 0'u16

template do_addcy(n:untyped): untyped =
  reportInst("Addcy  s" & toHex(x,1) & " + 0x" & toHex(n.int,2))
  let r: uint16 = state.regs[state.curFlags.activeBank][x].uint16 + n.uint16 + (if state.curFlags.c: 1 else: 0)
  state.regs[state.curFlags.activeBank][x] = r.uint8
  state.curFlags.z = state.curFlags.z and (state.regs[state.curFlags.activeBank][x] == 0)
  state.curFlags.c = (r and 0x100'u16) != 0'u16

template do_sub(n:untyped): untyped =
  reportInst("Sub  s" & toHex(x,1) & " - 0x" & toHex(n.int,2))
  let r: uint16 = state.regs[state.curFlags.activeBank][x].uint16 - n.uint16
  state.regs[state.curFlags.activeBank][x] = r.uint8
  state.curFlags.z = state.regs[state.curFlags.activeBank][x] == 0
  state.curFlags.c = (r and 0x100'u16) != 0'u16

template do_subcy(n:untyped): untyped =
  reportInst("Subcy  s" & toHex(x,1) & " - 0x" & toHex(n.int,2))
  let r: uint16 = state.regs[state.curFlags.activeBank][x].uint16 - n.uint16 - (if state.curFlags.c: 1 else: 0)
  state.regs[state.curFlags.activeBank][x] = r.uint8
  state.curFlags.z = state.curFlags.z and (state.regs[state.curFlags.activeBank][x] == 0)
  state.curFlags.c = (r and 0x100'u16) != 0'u16

template do_compare(n:untyped): untyped =
  reportInst("Compare  s" & toHex(x,1) & "(0x" & toHex(state.regs[state.curFlags.activeBank][x].int,2) & ") - 0x" & toHex(n.int,2))
  let r: uint16 = state.regs[state.curFlags.activeBank][x].uint16 - n.uint16
  #echo "@@@ Compare: ", state.regs[state.curFlags.activeBank][x].uint16, " ", n.uint16, " -> ", r
  state.curFlags.z = (r and 0xFF) == 0
  state.curFlags.c = (r and 0x100'u16) != 0'u16

template do_comparecy(n:untyped): untyped =
  reportInst("Comparecy  s" & toHex(x,1) & " - 0x" & toHex(n.int,2))
  let r: uint16 = state.regs[state.curFlags.activeBank][x].uint16 - n.uint16 - (if state.curFlags.c: 1 else: 0)
  state.curFlags.z = state.curFlags.z and ((r and 0xFF) == 0)
  state.curFlags.c = (r and 0x100'u16) != 0'u16

  
  
template do_sl0(): untyped =
  reportInst("Sl0  << s" & toHex(x,1) & " & '0'")
  var r: uint16 = state.regs[state.curFlags.activeBank][x]
  r = r shl 1
  state.regs[state.curFlags.activeBank][x] = r.uint8
  state.curFlags.z = state.regs[state.curFlags.activeBank][x] == 0
  state.curFlags.c = (r and 0x100'u16) != 0'u16

template do_sl1(): untyped =
  reportInst("Sl1  << s" & toHex(x,1) & " & '1'")
  var r: uint16 = state.regs[state.curFlags.activeBank][x]
  r = (r shl 1) or 0x01
  state.regs[state.curFlags.activeBank][x] = r.uint8
  state.curFlags.z = false
  state.curFlags.c = (r and 0x100'u16) != 0'u16  

template do_slx(): untyped =
  reportInst("Slx  << s" & toHex(x,1) & " & s" & toHex(x,1) & "[0]")
  var r: uint16 = state.regs[state.curFlags.activeBank][x]
  r = (r shl 1) or (r and 0x01)
  state.regs[state.curFlags.activeBank][x] = r.uint8
  state.curFlags.z = state.regs[state.curFlags.activeBank][x] == 0
  state.curFlags.c = (r and 0x100'u16) != 0'u16

template do_sla(): untyped =
  reportInst("Sla  << s" & toHex(x,1) & " & C")
  var r: uint16 = state.regs[state.curFlags.activeBank][x]
  r = (r shl 1) or (if state.curFlags.c: 1 else: 0)
  state.regs[state.curFlags.activeBank][x] = r.uint8
  state.curFlags.z = state.regs[state.curFlags.activeBank][x] == 0
  state.curFlags.c = (r and 0x100'u16) != 0'u16


template do_sr0(): untyped =
  reportInst("Sr0  >> '0' & s" & toHex(x,1))
  var r: uint16 = state.regs[state.curFlags.activeBank][x]
  state.curFlags.c = (r and 0x01) != 0'u16
  r = r shr 1
  state.regs[state.curFlags.activeBank][x] = r.uint8
  state.curFlags.z = state.regs[state.curFlags.activeBank][x] == 0

template do_sr1(): untyped =
  reportInst("Sr1  >> '1' & s" & toHex(x,1))
  var r: uint16 = state.regs[state.curFlags.activeBank][x]
  state.curFlags.c = (r and 0x01) != 0'u16
  r = (r shr 1) or 0x80
  state.regs[state.curFlags.activeBank][x] = r.uint8
  state.curFlags.z = false

template do_srx(): untyped =
  reportInst("Srx  >> s" & toHex(x,1) & "[0] & s" & toHex(x,1))
  var r: uint16 = state.regs[state.curFlags.activeBank][x]
  state.curFlags.c = (r and 0x01) != 0'u16
  r = (r shr 1) or (r and 0x80)
  state.regs[state.curFlags.activeBank][x] = r.uint8
  state.curFlags.z = state.regs[state.curFlags.activeBank][x] == 0

template do_sra(): untyped =
  reportInst("Sra  >> C & s" & toHex(x,1))
  var r: uint16 = state.regs[state.curFlags.activeBank][x]
  let new_c = (r and 0x01) != 0'u16
  r = (r shr 1) or (if state.curFlags.c: 0x80 else: 0)
  state.regs[state.curFlags.activeBank][x] = r.uint8
  state.curFlags.z = state.regs[state.curFlags.activeBank][x] == 0
  state.curFlags.c = new_c

  
template do_rl(): untyped =
  reportInst("Rl  s" & toHex(x,1))
  var r: uint16 = state.regs[state.curFlags.activeBank][x]
  r = (r shl 1) or (r shr 7)
  state.regs[state.curFlags.activeBank][x] = r.uint8
  state.curFlags.z = state.regs[state.curFlags.activeBank][x] == 0
  state.curFlags.c = (r and 0x100'u16) != 0'u16

template do_rr(): untyped =
  reportInst("Rr  s" & toHex(x,1))
  var r: uint16 = state.regs[state.curFlags.activeBank][x]
  state.curFlags.c = (r and 0x01) != 0'u16
  r = (r shr 1) or (r shl 7)
  state.regs[state.curFlags.activeBank][x] = r.uint8
  state.curFlags.z = state.regs[state.curFlags.activeBank][x] == 0
  
template do_regbank(): untyped =
  state.curFlags.activeBank = (w and 0x01)
  reportInst("Regbank  " & (if state.curFlags.activeBank == 0: "A" else: "B"))


template do_input(n:untyped): untyped =
  reportInst("Input  s" & toHex(x,1) & " = port[" & toHex(n.int,2) & "]")

  var success = true

  # Call the read peripherals to let them alter the input port value
  if state.periphReadIndex.hasKey(n.int):
    for periph in state.periphReadIndex[n.int][]:
      success = success and periph.portRead(n, state, cmdOpts.quiet)
  if not success: # Stop simulation loop
    break

  state.regs[state.curFlags.activeBank][x] = state.portsIn[n]

template do_output(n:untyped): untyped =
  reportInst("Output  port[" & toHex(n.int,2) & "] = s" & toHex(x,1))
  state.portsOut[n] = state.regs[state.curFlags.activeBank][x]
  
  var success = true

  # Notify the write peripherals of the new output port value
  if state.periphWriteIndex.hasKey(n.int):
    for periph in state.periphWriteIndex[n.int][]:
      success = success and periph.portWrite(n, state, cmdOpts.quiet)
  if not success: # Stop simulation loop
    break
  

template do_outputk(): untyped =
  reportInst("Outputk  kport[" & toHex(w and 0x0F,1) & "] = " & toHex((w shr 4) and 0xFF, 2))
  state.kports[w and 0x0F] = ((w shr 4) and 0xFF).uint8



template do_store(n:untyped): untyped =
  reportInst("Store  sp[" & toHex(n.int,2) & "] = s" & toHex(x,1))
  state.scratchpad[(n and state.scratchMask).uint8] = state.regs[state.curFlags.activeBank][x]

template do_fetch(n:untyped): untyped =
  reportInst("Fetch  s" & toHex(x,1) & " = sp[" & toHex(n.int,2) & "]  (" & toHex(state.scratchpad[(n and state.scratchMask).uint8].int,2) & ")")
  state.regs[state.curFlags.activeBank][x] = state.scratchpad[(n and state.scratchMask).uint8]


template do_jump(): untyped =
  reportInst("Jump  " & state.getSymbol(address))
  next_pc = address

template do_jump_z(): untyped =
  reportInst("Jump Z  " & $state.curFlags.z & "  " & state.getSymbol(address))
  if state.curFlags.z: next_pc = address

template do_jump_nz(): untyped =
  reportInst("Jump NZ  " & $(not state.curFlags.z) & "  " & state.getSymbol(address))
  if not state.curFlags.z: next_pc = address

template do_jump_c(): untyped =
  reportInst("Jump C  " & $state.curFlags.c & "  " & state.getSymbol(address))
  if state.curFlags.c: next_pc = address

template do_jump_nc(): untyped =
  reportInst("Jump NC  " & $(not state.curFlags.c) & "  " & state.getSymbol(address))
  if not state.curFlags.c: next_pc = address

template do_jump_at(): untyped =
  reportInst("Jump@")
  next_pc = ((state.regs[state.curFlags.activeBank][x].int32 shl 8) or state.regs[state.curFlags.activeBank][y].int32) and 0xFFF


template do_call_push(n: untyped): untyped =
  if len(state.callStack) < 30: # Max levels on KCPSM6
    state.callStack.add(n)
  else:
    state.termination = termStackOverflow
    do_terminate()
  
template do_call(): untyped =
  reportInst("Call  " & state.getSymbol(address))
  next_pc = address
  do_call_push(state.pc+1)

template do_call_z(): untyped =
  reportInst("Call Z  " & $state.curFlags.z & "  " & state.getSymbol(address))
  if state.curFlags.z:
    next_pc = address
    do_call_push(state.pc+1)

template do_call_nz(): untyped =
  reportInst("Call NZ  " & $(not state.curFlags.z) & "  " & state.getSymbol(address))
  if not state.curFlags.z:
    next_pc = address
    do_call_push(state.pc+1)

template do_call_c(): untyped =
  reportInst("Call C  " & $state.curFlags.c & "  " & state.getSymbol(address))
  if state.curFlags.c:
    next_pc = address
    do_call_push(state.pc+1)

template do_call_nc(): untyped =
  reportInst("Call NC  " & $(not state.curFlags.c) & "  " & state.getSymbol(address))
  if not state.curFlags.c:
    next_pc = address
    do_call_push(state.pc+1)

template do_call_at(): untyped =
  reportInst("Call@")
  next_pc = ((state.regs[state.curFlags.activeBank][x].int32 shl 8) or state.regs[state.curFlags.activeBank][y].int32) and 0xFFF
  do_call_push(state.pc+1)



template do_popCall(): untyped =
  if len(state.callStack) > 0:
    next_pc = state.callStack.pop()
  else:
    state.termination = termStackUnderflow
    do_terminate()

template do_return(): untyped =
  reportInst("Return")
  do_popCall()
  
template do_return_z(): untyped =
  reportInst("Return Z  " & $state.curFlags.z)
  if state.curFlags.z: do_popCall()

template do_return_nz(): untyped =
  reportInst("Return NZ  " & $(not state.curFlags.z))
  if not state.curFlags.z: do_popCall()

template do_return_c(): untyped =
  reportInst("Return C  " & $state.curFlags.c)
  if state.curFlags.c: do_popCall()

template do_return_nc(): untyped =
  reportInst("Return NC  " & $(not state.curFlags.c))
  if not state.curFlags.c: do_popCall()


template do_returni(): untyped =
  reportInst("ReturnI")
  
  # Restore processor flags from before the interrupt
  state.curFlags = state.savedFlags
  state.intActive = (if imm == 1: true else: false)
  do_popCall()

template do_enable_disable(): untyped =
  if imm == 1:
    reportInst("Enable")
    state.intActive = true
  else:
    reportInst("Disable")
    state.intActive = false

  
template do_load_and_return(): untyped =
  reportInst("Load&return  s" & toHex(x, 1) & " = " & toHex(imm.int, 2))
  state.regs[state.curFlags.activeBank][x] = imm
  do_popCall()

template do_star(): untyped =
  reportInst("Star s$# ($#) = s$# ($#)" % [toHex(x,1), (if state.curFlags.activeBank == 0: "B" else: "A"),
                                          toHex(y,1), (if state.curFlags.activeBank == 0: "A" else: "B")])
  state.regs[1 - state.curFlags.activeBank][x] = state.regs[state.curFlags.activeBank][y]

template do_hwbuild(): untyped =
  reportInst("Hwbuild s$# = $#" % [toHex(x,1), toHex(state.hwbuild.int32,2)])
  state.regs[state.curFlags.activeBank][x] = state.hwbuild
  state.curFlags.c = true
  state.curFlags.z = state.regs[state.curFlags.activeBank][x] == 0




proc newProcState(periphs: openarray[Peripheral], jsonInput: array[0..255, uint8], scratchSize: int,
                  intVec: int32 = 0x3FF, hwbuild: uint8 = 0): ref ProcState =
  ## Initialize a procState object
  var state: ref ProcState
  new state

  state.pc = 0
  state.termination = termNormal
  state.executed = initSet[int32](1024)
  state.callStack = @[]
  state.portsIn = jsonInput
  state.peripherals = @periphs
  
  state.scratchMask = (scratchSize - 1).uint8
  
  state.intActive = true
  state.intFlag = false
  state.intVec = intVec
  
  state.hwbuild = hwbuild
  
  
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


template simulateLoop(): untyped =
  ## Main simulation loop.
  ## Terminates normally when an OUTPUT instruction targeting quitPort executes.
    
  state.rom = romData # FIXME: relocate?
  
  for i in 0..<cmdOpts.instLimit:

    # Fetch next instruction
    if state.intFlag:
      state.intActive = false
      state.intFlag = false
      # Save current flags
      state.savedFlags = state.curFlags
      do_call_push(state.pc)
      state.pc = state.intVec
      
      
    if state.pc < len(state.rom.data):
      w = state.rom.data[state.pc]
    else:
      state.termination = termInvalidAddress
      do_terminate()

    # Decode common fields
    opc = (w shr 12) and 0xFF
    x = (w shr 8) and 0xF
    y = (w shr 4) and 0xF
    imm = (uint8) w and 0xFF
    
    next_pc = state.pc + 1
    state.totalInsts += 1
    state.executed.incl(state.pc)

    if cmdOpts.usePB6:
      address = w and 0xFFF # Let memory wrap at 4K

      # Decode PB6 opcodes
      case opc
      of 0x00: do_load(state.regs[state.curFlags.activeBank][y])
      of 0x01: do_load(imm)
      of 0x02: do_and(state.regs[state.curFlags.activeBank][y])
      of 0x03: do_and(imm)
      of 0x04: do_or(state.regs[state.curFlags.activeBank][y])
      of 0x05: do_or(imm)
      of 0x06: do_xor(state.regs[state.curFlags.activeBank][y])
      of 0x07: do_xor(imm)
      
      of 0x10: do_add(state.regs[state.curFlags.activeBank][y])
      of 0x11: do_add(imm)
      of 0x12: do_addcy(state.regs[state.curFlags.activeBank][y])
      of 0x13: do_addcy(imm)
      
      of 0x18: do_sub(state.regs[state.curFlags.activeBank][y])
      of 0x19: do_sub(imm)
      of 0x1A: do_subcy(state.regs[state.curFlags.activeBank][y])
      of 0x1B: do_subcy(imm)
      
      of 0x0C: do_test(state.regs[state.curFlags.activeBank][y])
      of 0x0D: do_test(imm)
      of 0x0E: do_testcy(state.regs[state.curFlags.activeBank][y])
      of 0x0F: do_testcy(imm)
      of 0x1C: do_compare(state.regs[state.curFlags.activeBank][y])
      of 0x1D: do_compare(imm)
      of 0x1E: do_comparecy(state.regs[state.curFlags.activeBank][y])
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
        of 0x80: do_hwbuild() # HWBUILD
        else: do_badOp

      of 0x37: do_regbank()
      
      of 0x08: do_input(state.regs[state.curFlags.activeBank][y])
      of 0x09: do_input(imm)
      of 0x2C: do_output(state.regs[state.curFlags.activeBank][y])
      of 0x2D: do_output(imm)
      of 0x2B: do_outputk()
      
      of 0x2E: do_store(state.regs[state.curFlags.activeBank][y])
      of 0x2F: do_store(imm)
      of 0x0A: do_fetch(state.regs[state.curFlags.activeBank][y])
      of 0x0B: do_fetch(imm)
      
      of 0x28: do_enable_disable()
      of 0x29: do_returni()
      
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
      
      
    else: # PB3
      address = w and 0x3FF # Let memory wrap at 1K
      # Decode PB3 opcodes
      case opc
      of 0x01: do_load(state.regs[state.curFlags.activeBank][y])
      of 0x00: do_load(imm)
      of 0x0B: do_and(state.regs[state.curFlags.activeBank][y])
      of 0x0A: do_and(imm)
      of 0x0D: do_or(state.regs[state.curFlags.activeBank][y])
      of 0x0C: do_or(imm)
      of 0x0F: do_xor(state.regs[state.curFlags.activeBank][y])
      of 0x0E: do_xor(imm)
      
      of 0x19: do_add(state.regs[state.curFlags.activeBank][y])
      of 0x18: do_add(imm)
      of 0x1B: do_addcy(state.regs[state.curFlags.activeBank][y])
      of 0x1A: do_addcy(imm)
      
      of 0x1D: do_sub(state.regs[state.curFlags.activeBank][y])
      of 0x1C: do_sub(imm)
      of 0x1F: do_subcy(state.regs[state.curFlags.activeBank][y])
      of 0x1E: do_subcy(imm)
      
      of 0x13: do_test(state.regs[state.curFlags.activeBank][y])
      of 0x12: do_test(imm)
      of 0x15: do_compare(state.regs[state.curFlags.activeBank][y])
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

      of 0x05: do_input(state.regs[state.curFlags.activeBank][y])
      of 0x04: do_input(imm)
      of 0x2D: do_output(state.regs[state.curFlags.activeBank][y])
      of 0x2C: do_output(imm)
      #of 0x2B: do_outputk()
      
      of 0x2F: do_store(state.regs[state.curFlags.activeBank][y])
      of 0x2E: do_store(imm)
      of 0x07: do_fetch(state.regs[state.curFlags.activeBank][y])
      of 0x06: do_fetch(imm)
      
      of 0x3C: do_enable_disable()
      of 0x38: do_returni()
      
      of 0x34: do_jump()
      of 0x35:
        let cc = (w shr 10) and 0x03
        case cc
        of 0x00: do_jump_z()
        of 0x01: do_jump_nz()
        of 0x02: do_jump_c()
        of 0x03: do_jump_nc()
        else: do_badOp
      
      of 0x30: do_call()
      of 0x31:
        let cc = (w shr 10) and 0x03
        case cc
        of 0x00: do_call_z()
        of 0x01: do_call_nz()
        of 0x02: do_call_c()
        of 0x03: do_call_nc()
        else: do_badOp
      
      of 0x2A: do_return()
      of 0x2B:
        let cc = (w shr 10) and 0x03
        case cc
        of 0x00: do_return_z()
        of 0x01: do_return_nz()
        of 0x02: do_return_c()
        of 0x03: do_return_nc()
        else: do_badOp
      
      else: do_badOp

      next_pc = next_pc and 0x3FF

    
    state.pc = next_pc

    if not show_trace and not cmdOpts.quiet and (state.totalInsts mod 1_000_000 == 0):
      stdout.write(".")
      stdout.flushFile()

  if not show_trace and not cmdOpts.quiet: echo ""

  if state.totalInsts == cmdOpts.instLimit:
    state.termination = termInstLimit


proc simulate(state: ref ProcState, romData: ref RomInfo, cmdOpts: CommandOptions) =
  ## Run a simulation with a program in ROM
  var
    w, opc, x, y, address, next_pc: int32
    imm: uint8
  
  # Instantiate two versions of the simulation loop
  # This avoids testing the trace setting on every instruction and instead uses
  # templates to expand two variants of the simulation loop.
  if cmdOpts.trace:
    const show_trace = true
    simulateLoop
  else:
    const show_trace = false
    simulateLoop


proc newRomInfo(memFile: string, logFile: string): ref RomInfo =
  ## Create a new RomInfo tuple with data read from a mem file and optional symbols
  new result
  result.data = readMemFile(memFile)[]
  
  if logFile != nil and logFile != "":
    result.symbolTable = extractSymbols(logFile)
  else:
    result.symbolTable = initTable[int32, string]()


proc countInstructions(state: ref ProcState): int =
  # Determine the number of valid instructions in the ROM
  # Assume the last instruction is a default_jump or uninitialized 0's
  # FIXME: This breaks if last instruction is the interrupt vector
  let nonInst = state.rom.data[state.rom.data.high]
  var instCount = 0
  for i in 0..state.rom.data.high:
    if state.rom.data[i] != nonInst:
      instCount = instCount + 1
  result = instCount

const usageString = """
Open PicoBlaze simulator
Usage: opbsim [OPTIONS]

Options:
  -m:MEM_FILE --mem:MEM_FILE        Input mem file
  -i:JSON_IN  --input:JSON_IN       JSON input string
  --log:LOG_FILE                    Log file with symbol table
  -s:NUM        --scratch-size:NUM  Set scratchpad memory size
  -L:NUM        --limit:NUM         Limit to NUM instructions executed
  -v            --verbose           Verbose output
  -t            --trace             Print execution trace
  -q            --quiet             Quiet output
  -j            --json              JSON report [forces quiet too]
  -p            --list-periphs      Print peripheral information
  --pb3                             Simulate PicoBlaze-3 code
  --pb6                             Simulate PicoBlaze-6 code [default]
  --version                         Report version information
"""


proc parseCommandLine() : CommandOptions =
  var cmdOpts : CommandOptions
  
  cmdOpts.instLimit = 100_000_000
  cmdOpts.scratchSize = 64;
  
  # Parse command line
  for kind, key, val in getopt():
    case kind
    of cmdShortOption, cmdLongOption:
      case key
      of "h"           : echo usageString; quit(0)
      of "mem", "m"    : cmdOpts.memFile = val
      of "scratch-size", "s" : cmdOpts.scratchSize = parseInt(val)
      of "limit", "L"  : cmdOpts.instLimit = parseInt(val)
      of "log",        : cmdOpts.logFile = val
      of "verbose", "v": cmdOpts.verbose = true
      of "trace", "t"  : cmdOpts.trace = true
      of "quiet", "q"  : cmdOpts.quiet = true
      of "input", "i"  : cmdOpts.jsonInput = val
      of "json", "j"   : cmdOpts.jsonOutput = true
      of "list-periphs", "p": cmdOpts.listPeriphs = true
      of "pb3"         : cmdOpts.usePB3 = true
      of "pb6"         : cmdOpts.usePB6 = true
      of "version"     : cmdOpts.version = true
      else             : discard
    else: discard
    
  if cmdOpts.version: echo "OPBSIM version ", buildVersion; quit(0)

  if cmdOpts.memFile == "" or cmdOpts.memFile == nil: echo usageString; quit(1)
  if cmdOpts.usePB3 and cmdOpts.usePB6:
    echo "Invalid options: Select only one PicoBlaze architecture"
    quit(1)
  if not (cmdOpts.usePB3 or cmdOpts.usePB6): cmdOpts.usePB6 = true
  
  var scratchSizes : seq[int]
  if cmdOpts.usePB6:
    scratchSizes = @[64,128,256]
  else:
    scratchSizes = @[64]

  if not (cmdOpts.scratchSize in scratchSizes):
    echo "Invalid scratchpad size. Must be ", scratchSizes.mapIt($it).join(", "), "."
    quit(1)
  

  if cmdOpts.jsonOutput:
    cmdOpts.verbose = false
    cmdOpts.trace = false
    cmdOpts.quiet = true

  return cmdOpts



proc main() =
  var cmdOpts = parseCommandLine()
  
  var jsonInput : array[0..255, uint8]
  
  if cmdOpts.jsonInput != "" and cmdOpts.jsonInput != nil:
    # Initialize input data
    let jobj = parseJson(cmdOpts.jsonInput)
    assert jobj.kind == JObject
    
    for f in jobj.pairs:
      var offset = parseHexInt(f.key)
      
      for i in f.val:
        if offset > jsonInput.high: break
        
        jsonInput[offset] = i.getNum.uint8
        offset.inc


  if not cmdOpts.quiet:
    echo "PicoBlaze simulator"
    echo "Running in ", (if cmdOpts.usePB6: "PicoBlaze-6" else: "PicoBlaze-3"), " mode"
    echo "Scratchpad size: ", cmdOpts.scratchSize
    echo "Input: ", cmdOpts.memFile


  # Look for log file if one wasn't provided in arguments
  if cmdOpts.logFile == "" or cmdOpts.logFile == nil:
    let elems = splitFile(cmdOpts.memFile)
    let logFile = elems.dir / (elems.name & ".log")
    if fileExists(logFile):
      cmdOpts.logFile = logFile

  var romData = newRomInfo(cmdOpts.memFile, cmdOpts.logFile)

  if len(romData.data) == 0: echo "ERROR: Invalid MEM file"; quit(1)

  if not cmdOpts.quiet:
    echo "Read $# words\n" % [$len(romData.data)]

    if cmdOpts.logFile != nil and cmdOpts.logFile != "":
      echo "Found $# symbols in $#\n" % [$len(romData.symbolTable), cmdOpts.logFile]


  # Instantiate the peripherals
  const quitPort: uint8 = 0xFF
  let con = newConsolePeriph("Console", @[0xFE'u8])
  let periphs = [con,
                newQuitPeriph("Quit", @[quitPort]),
                newLoopbackPeriph("Loopback", toSeq(0x00'u8 .. 0x0F'u8)),
                newROMPeriph("ROM", @[0xFA'u8, 0xFB'u8], @[0xFA'u8, 0xFB'u8]),
                newInterruptPeriph("IntGen", @[0xFC'u8]),
                newCounterPeriph("Counter", @[0xF0'u8], @[0xF0'u8, 0xF1'u8, 0xF2'u8, 0xF3'u8,])
                ]

  var state = newProcState(periphs, jsonInput, cmdOpts.scratchSize)

  # Run the simulator
  let t1 = cpuTime()
  state.simulate(romData, cmdOpts)
  let t2 = cpuTime()
  
  let cpuRuntime = t2 - t1
  
  if cmdOpts.verbose:
    # Report results
    echo "\n     A    B"
    for i in 0..15:
      echo "s$1 = $2   $3" % [toHex(i,1), toHex(state.regs[0][i].int32,2), toHex(state.regs[1][i].int32,2)]

    echo "\nScratchpad:"
    for i in 0..<cmdOpts.scratchSize /% 16:
      echo toHex(i*16, 2), " : ", state.scratchpad[i*16..<(i+1)*16].mapIt(toHex(it.int32,2)).join(" ")

    echo "\nOut ports:"
    for i in 0..<len(state.portsOut) /% 16:
      echo toHex(i*16, 2), " : ", state.portsOut[i*16..<(i+1)*16].mapIt(toHex(it.int32,2)).join(" ")

  let instCount = countInstructions(state)
      
  if not cmdOpts.quiet:
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
    
    if cmdOpts.listPeriphs:
      echo "\nPeripherals:"
      
      # Get the longest peripheral name
      var maxLen = 0
      for p in state.peripherals:
        if len(p.name) > maxLen: maxLen = len(p.name)
        
      for p in state.peripherals:
        echo "  " & p.name & ' '.repeat(maxLen - len(p.name)) &
             " W: " & p.writePorts.mapIt(toHex(it.int32,2)).join(",") &
             "  R: " & p.readPorts.mapIt(toHex(it.int32,2)).join(",")
    
    
  if cmdOpts.jsonOutput:
    let
      jsonData = newJObject()

    jsonData["regs_a"] = %(state.regs[0].mapIt(%(it.int)))
    jsonData["regs_b"] = %(state.regs[1].mapIt(%(it.int)))
    jsonData["scratchpad"] = %(state.scratchpad[0..<cmdOpts.scratchSize].mapIt(%(it.int)))
    jsonData["ports_in"] = %(state.portsIn.mapIt(%(it.int)))
    jsonData["ports_out"] = %(state.portsOut.mapIt(%(it.int)))
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
