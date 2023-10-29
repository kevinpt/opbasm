#!/usr/bin/python
# -*- coding: utf-8 -*-

'''Color formatting
'''

# Copyright © 2014 Kevin Thibedeau
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



try:
    import colorama
    colorama.init()

    from colorama import Fore, Back, Style

except ImportError:

    def note(t): return t
    def success(t): return t
    def warn(t): return t
    def error(t): return t

else:
    import os
    _no_color = os.getenv('NO_COLOR', 'false')
    _no_color = True if _no_color.lower() in ['1', 'true', 't', 'y', 'yes'] else False

    def stdout_redirected():
        return os.fstat(0) != os.fstat(1)

    _redir_stdout = stdout_redirected()



    def colorize(t, code):
        if _no_color or _redir_stdout:
            return t

        return ''.join([code, t, Style.RESET_ALL])

    def note(t):
        return colorize(t, Fore.MAGENTA)

    def success(t):
        return colorize(t, Fore.GREEN)

    def warn(t):
        return colorize(t, Fore.YELLOW + Style.BRIGHT)

    def error(t):
        return colorize(t, Fore.RED + Style.BRIGHT)


if __name__ == '__main__':
    print('Colorized text:\n')
    print('note("foobar")    : ' + note('foobar'))
    print('success("foobar") : ' + success('foobar'))
    print('warn("foobar")    : ' + warn('foobar'))
    print('error("foobar")   : ' + error('foobar'))

