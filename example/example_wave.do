onerror {resume}
quietly WaveActivateNextPane {} 0
add wave -noupdate /example/clk
add wave -noupdate /example/reset
add wave -noupdate /example/sim_done
add wave -noupdate -divider {Picoblaze Port}
add wave -noupdate /example/bram_enable
add wave -noupdate -radix hexadecimal /example/address
add wave -noupdate -radix hexadecimal /example/instruction
add wave -noupdate /example/pb6/kcpsm6_status
add wave -noupdate -color Red /example/pb6/kcpsm6_opcode
add wave -noupdate /example/write_strobe
add wave -noupdate /example/read_strobe
add wave -noupdate /example/k_write_strobe
add wave -noupdate -radix hexadecimal /example/port_id
add wave -noupdate -radix hexadecimal /example/in_port
add wave -noupdate -radix hexadecimal /example/out_port
add wave -noupdate /example/interrupt_ack
add wave -noupdate /example/sleep
add wave -noupdate -divider {Second ROM port}
add wave -noupdate -radix hexadecimal /example/address2
add wave -noupdate -radix hexadecimal /example/instruction2
add wave -noupdate -divider Registers
add wave -noupdate -radix hexadecimal /example/pb6/sim_sF
add wave -noupdate -radix hexadecimal /example/pb6/sim_sE
add wave -noupdate -radix hexadecimal /example/pb6/sim_sD
add wave -noupdate -radix hexadecimal /example/pb6/sim_sC
add wave -noupdate -radix hexadecimal /example/pb6/sim_sB
add wave -noupdate -radix hexadecimal /example/pb6/sim_sA
add wave -noupdate -radix hexadecimal /example/pb6/sim_s9
add wave -noupdate -radix hexadecimal /example/pb6/sim_s8
add wave -noupdate -radix hexadecimal /example/pb6/sim_s7
add wave -noupdate -radix hexadecimal /example/pb6/sim_s6
add wave -noupdate -radix hexadecimal /example/pb6/sim_s5
add wave -noupdate -radix hexadecimal /example/pb6/sim_s4
add wave -noupdate -radix hexadecimal /example/pb6/sim_s3
add wave -noupdate -radix hexadecimal /example/pb6/sim_s2
add wave -noupdate -radix hexadecimal /example/pb6/sim_s1
add wave -noupdate -radix hexadecimal /example/pb6/sim_s0
add wave -noupdate -divider Stack
add wave -noupdate -radix hexadecimal /example/pb6/sim_spm3F
add wave -noupdate -radix hexadecimal /example/pb6/sim_spm3E
add wave -noupdate -radix hexadecimal /example/pb6/sim_spm3D
add wave -noupdate -radix hexadecimal /example/pb6/sim_spm3C
add wave -noupdate -radix hexadecimal /example/pb6/sim_spm3B
add wave -noupdate -radix hexadecimal /example/pb6/sim_spm3A
add wave -noupdate -radix hexadecimal /example/pb6/sim_spm39
add wave -noupdate -radix hexadecimal /example/pb6/sim_spm38
add wave -noupdate -radix hexadecimal /example/pb6/sim_spm37
add wave -noupdate -radix hexadecimal /example/pb6/sim_spm36
add wave -noupdate -radix hexadecimal /example/pb6/sim_spm35
add wave -noupdate -radix hexadecimal /example/pb6/sim_spm34
add wave -noupdate -radix hexadecimal /example/pb6/sim_spm33
add wave -noupdate -radix hexadecimal /example/pb6/sim_spm32
add wave -noupdate -radix hexadecimal /example/pb6/sim_spm31
add wave -noupdate -radix hexadecimal /example/pb6/sim_spm30
TreeUpdate [SetDefaultTree]
WaveRestoreCursors {{Cursor 1} {678608 ps} 0}
quietly wave cursor active 1
configure wave -namecolwidth 164
configure wave -valuecolwidth 100
configure wave -justifyvalue left
configure wave -signalnamewidth 0
configure wave -snapdistance 10
configure wave -datasetprefix 0
configure wave -rowmargin 4
configure wave -childrowmargin 2
configure wave -gridoffset 0
configure wave -gridperiod 1
configure wave -griddelta 40
configure wave -timeline 0
configure wave -timelineunits ns
update
WaveRestoreZoom {0 ps} {9529984 ps}
