namespace eval ::fs {
    lappend auto_path [file dirname [info script]]/XilinxTclStore/pkgIndex.tcl
}


proc ::fs::mem {} {
    variable mems [get_cells -hier -filter "PRIMITIVE_GROUP	== BMEM"]

    set net_width_port_a [list ]
    set net_width_port_b [list ]
    set mem_type [list ]
    set mem_name [list ]
    set bus_width [list ]
    set mem_depth [list ]

    foreach m $mems {
        
        set n [llength [get_nets -filter "TYPE == SIGNAL" -of_objects [get_pins -filter "BUS_NAME == DIADI"  -of [get_cells $m] ] ]]
        if {[llength $n] == 0} {
            lappend net_width_port_a "-"
        } else {
            lappend net_width_port_a $n
        }

        set n [llength [get_nets -filter "TYPE == SIGNAL" -of_objects [get_pins -filter "BUS_NAME == DIBDI"  -of [get_cells $m] ] ]]
        if {[llength $n] == 0} {
            lappend net_width_port_b "-"
        } else {
            lappend net_width_port_b $n
        }

        lappend mem_type [get_property REF_NAME $m]
        lappend mem_name [get_property NAME     $m]

        puts $m
        
        if {[get_property PRIMITIVE_SUBGROUP [get_cells $m]] eq "fifo"} {
            lappend bus_width [lindex [get_property BUS_WIDTH [get_nets -filter "TYPE == SIGNAL" -of_objects [get_pins -filter {BUS_NAME == DI}  -of [get_cells $m] ] ] ] 0]
            lappend mem_depth "?"
        }

        if {[get_property PRIMITIVE_SUBGROUP [get_cells $m]] eq "bram"} {
            lappend bus_width [lindex [get_property BUS_WIDTH [get_nets -filter "TYPE == SIGNAL" -of_objects [get_pins -regexp -filter {BUS_NAME =~ DI.*DI}  -of [get_cells $m] ] ] ] 0]
            lappend mem_depth [expr 2**[lindex [get_property BUS_WIDTH [get_nets -filter "TYPE == SIGNAL" -of_objects [get_pins -regexp -filter {BUS_NAME =~ ADDRA.*}  -of [get_cells $m] ] ] ] 0]]
        }
    }

    set tbl [::tclapp::xilinx::designutils::prettyTable {TABLICHKA DLYA ANDRUHI}]
    $tbl header [list Num BUS_WIDTH DINA DINB DEPTH TYPE  NAME]

    for {set i 0} {$i < [llength $mems]} {incr i} {

        $tbl addrow [list $i [lindex $bus_width $i] [lindex $net_width_port_a $i] [lindex $net_width_port_b $i] [lindex $mem_depth $i] [lindex $mem_type $i] [lindex $mem_name $i]]
    }

    $tbl print

}


proc ::fs::get_fmax {} {
    variable fmax
    
    set slack [get_property SLACK [ get_timing_paths ]]
    set clock [get_property STARTPOINT_CLOCK [ get_timing_paths ]]
    set period [get_property PERIOD [get_clocks $clock]]

    set fmax [format %.3f [expr 1.0/($period - $slack) * 1000.0]]
   
    return -code ok "Max frequency of design is $fmax MHz"
}

proc ::fs::get_nets_from_report {args} {

    set error 0
    set r_high_fanout_nets 0

    while {[llength $args]} {
        set flag [::fs::lshift args]
        
        switch -exact -- $flag {
            
            -rhfn -
            -report_high_fanout_nets {
                set opt_args [split [::fs::lshift args] " "]
                set fanout   [lindex $opt_args 0]
                set pattern  [lindex $opt_args 1]
                set max_nets [lindex $opt_args 2]
                
                set r_high_fanout_nets 1
            }




            default { 
                if {[string match "-*" $flag]} {
                    puts " ERROR - option '$flag' is not a valid option."
                    incr error
                } else {
                    puts "ERROR - option '$flag' is not a valid option."
                    incr error
                }
            }
        }
    }

    if {$error} {
        return -code error {Oops, something is not correct}
    }
    if {$r_high_fanout_nets} {
        
        set r [report_high_fanout_nets -return_string -fanout_greater_than $fanout -max_nets $max_nets]
        set ls [split $r \n]
        
        set net_name   [list ]
        set net_fanout [list ]
        set net_driver [list ]

        foreach l $ls {
            if { [regexp {\|\s*([^\|\s*]*)\s*\|\s*(\d*)\s*\|\s+?(\w*)\s*\|} $l a g1 g2 g3] } {
                lappend net_name   $g1
                lappend net_fanout $g2
                lappend net_driver $g3
            }
        }
        
        set i -1
        foreach net $net_name {
            incr i     

            if {[string match $pattern $net]} {
                if {[string match RAM* [lindex $net_driver $i]]} {
                    set_property MAX_FANOUT_MODE MACRO [get_nets $net]
                    set_property MAX_FANOUT $fanout [get_nets $net]
                } elseif   {[string match BUFG* [lindex $net_driver $i]]} {
                    continue
                } else {
                    set_property MAX_FANOUT $fanout [get_nets $net]
                }
            }
        }

        return -code ok "\[FS::INFO\] ===> Save design before continue"
    }


}

proc ::fs::lshift listVar {
    upvar 1 $listVar L
    set r [lindex $L 0]
    set L [lreplace $L [set L 0] 0]
    return $r
}



