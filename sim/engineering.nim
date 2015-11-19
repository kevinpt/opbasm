# Convert numbers to engineering and SI units

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

import strutils, sequtils, tables, math


type  engPair = tuple[num: float, exp: int]
   
proc toEng(f: float): engPair =

  let logVal = log10(abs(f))
  var exp = if classify(logVal) != fcNegInf: logVal.int else: 1
  
  if exp < -24:
    exp = -24
  
  var r : engPair
  
  r.exp = exp - exp mod 3
  
  if r.exp <= 0 and f < (10.0.pow(r.exp.float)-0.00000001):
    r.exp -= 3
    
  r.num = f / 10.0.pow(r.exp.float)
  
  result = r

  
proc formatEng*(f: float, fracDigits: Natural = 1): string =
  let ep = toEng(f)
  var digits: string
  if fracDigits > 0:
    digits = formatFloat(ep.num, ffDecimal, fracDigits)
  else:
    digits = $ep.num.round
    
  result = digits & "e" & $ep.exp


const siPrefixes = {
  21:  "Y",
  18:  "Z",
  15:  "E",
  12:  "P",
  9:   "G",
  6:   "M",
  3:   "k",
  0:   "",
  -3:  "m",
  -6:  "u",
  -9:  "n",
  -12: "p",
  -15: "f",
  -18: "a",
  -21: "z",
  -24: "y"
}.toTable

proc formatSI*(f: float, units: string, fracDigits: Natural = 1, unitSep = " "): string =
  let ep = toEng(f)
  var digits: string
  if fracDigits > 0:
    digits = formatFloat(ep.num, ffDecimal, fracDigits)
  else:
    digits = $ep.num.round

  var strExp: string
  if siPrefixes.hasKey(ep.exp):
    strExp = unitSep & siPrefixes[ep.exp] & units
  else:
    strExp = "e" & $ep.exp
  
  result = digits & strExp
  
when isMainModule:
  #var x = newSIEng(42.0e-5, "s")
  
  echo "Result: ", formatEng(42.0e-5)
  echo "toSI: ", formatSI(32.0e-10, "F")
  
  
