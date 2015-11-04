;:::::::::: Configure test environment ::::::::::
namereg sD, ERRORS ; Error count
namereg sE, TEMP   ; Temp register
namereg sF, SP     ; Stack pointer

constant portConsole, FE
constant portQuit, FF
constant portISR, FC

mem16(portROM, 0xFA, 0xFB)

define(`assert_true', `if($1,`',`add ERRORS, 01
addcy ERRORS, 00
$2')')

define(`assert_false', `if($1,`add ERRORS, 01
addcy ERRORS, 00
$2')')

define(`assert_z',  `ifne(`add ERRORS, 01
addcy ERRORS, 00')')
define(`assert_nz', `ifeq(`add ERRORS, 01
addcy ERRORS, 00')')
define(`assert_c',  `ifge(`add ERRORS, 01
addcy ERRORS, 00')')
define(`assert_nc', `iflt(`add ERRORS, 01
addcy ERRORS, 00')')

use_tempreg(TEMP)
use_stack(SP, 0x3F)
load ERRORS, 00

jump init_done

;:::::::::: Finish testcase ::::::::::

terminate: output ERRORS, portQuit ; End normally
jump terminate

default_jump exec_error
exec_error: jump exec_error ; End by exceeding instruction limit

init_done:
