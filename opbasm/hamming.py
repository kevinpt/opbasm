#!/usr/bin/python
# -*- coding: utf-8 -*-

'''Hamming and SECDED encoding routines
'''

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



import math
from functools import reduce


def split_bits(n, num_bits):
  '''Convert integer to a list of bits (MSB-first)
  n (int)
      The number to convert to bits.
  num_bits (int)
      The number of bits in the result.
  Returns a list of ints representing each bit in n.
  '''
  bits = [0] * num_bits
  for i in range(num_bits-1, -1, -1):
    bits[i] = n & 0x01
    n >>= 1

  return bits


def join_bits(bits):
  '''Convert an array of bits (MSB first) to an integer word
  bits (sequence of ints)
      The bits to be merged into an integer
  Returns an int representing the bits contained in the bits parameter.
  '''
  word = 0
  for b in bits:
    word = (word << 1) | b

  return word


def hamming_message_size( data_size ):
  '''Calculate total message size from the data size'''
  psize = int(math.log(data_size, 2)) + 1
  if (2**psize - 1) - psize < data_size:
    psize += 1

  return data_size + psize

def hamming_parity_size( message_size ):
  '''Calculate parity size from the total message size'''
  return int(math.ceil(math.log(message_size + 1, 2)))

def hamming_data_size( message_size ):
  '''Calculate data size from the total message size'''
  return message_size - hamming_parity_size(message_size)


def secded_message_size( data_size ):
  '''Calculate total SECDED message size from the data size'''
  return hamming_message_size(data_size) + 1

def secded_parity_size( message_size ):
  '''Calculate SECDED parity size from the total message size'''
  return hamming_parity_size(message_size) + 1

def secded_data_size( message_size ):
  '''Calculate SECDED data size from the total message size'''
  return message_size - secded_parity_size(message_size)



def hamming_interleave( data, parity_bits ):
  '''Combine separate data and parity bits into a message with interleaved parity
  
  data        : array of data bits
  parity_bits : array of parity_bits. Use all 0's when encoding
  
  Returns full message with interleaved bits
  '''
  message_size = len(data) + len(parity_bits)
  assert len(data) == hamming_data_size(message_size), 'Data and parity size mismatch'

  msg = [0 for _ in range(message_size)]
  parity_ix = 0
  data_ix = 0
  for i in range(1,len(msg)+1):
    if 2**int(math.ceil(math.log(i,2))) == i: # This is a power of 2 and will be a parity bit
      msg[i-1] = parity_bits[parity_ix]
      parity_ix += 1
    else:
      msg[i-1] = data[data_ix]
      data_ix += 1

  return msg


def hamming_parity( message ):
  '''Generate Hamming parity bits from an interleaved message
  This is the core routine of the package that determines which bits of a
  message to XOR together. It is employed for both encoding and decoding
  When encoding, the message should have all zeroes interleaved for the
  parity bits. The result is the parity to be used by a decoder.
  When decoding, the previously generated parity bits are interleaved and
  the result is a syndrome that can be used for error detection and
  correction.
  
  Returns the computed parity bits
  '''
  result = [0 for _ in range(hamming_parity_size(len(message)))]
  result_ix = 0
  for i in range(1, len(message)+1):
    if 2**int(math.ceil(math.log(i,2))) == i: # This is a power of 2
      count = i
      parity_bit = 0
      for d in range(i, len(message)+1):
        if count > 0:
          parity_bit ^= message[d-1]
        elif count == 1 - i:
          count = i + 1

        count -= 1

      result[result_ix] = parity_bit
      result_ix += 1

  return result

def hamming_encode( data ):
  '''Generate parity for the data'''
  parity_bits = [0 for _ in range(hamming_parity_size(hamming_message_size(len(data))))]
  parity_bits = hamming_parity(hamming_interleave(data, parity_bits))

  return parity_bits


def secded_encode( data ):
  '''Generate SECDED parity for the data'''
  parity_bits = hamming_encode(data)

  overall = reduce(lambda a,b: a ^ b, data) ^ reduce(lambda a,b: a ^ b, parity_bits)

  parity_bits.append(overall)

  return parity_bits

def secded_encode_num( data, bits ):
  '''Generate SECDED parity from an integer values
  
  data : Integer data to computer SECDED parity on
  bits : Number of bits in the data_format
  
  Returns an integer representing the SECDED parity'''
  adata = [int(b) for b in reversed(split_bits(data, bits))]
  sparity = secded_encode(adata)
  return join_bits(reversed(sparity))

