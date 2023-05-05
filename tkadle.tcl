#!/bin/sh
# TCL ignores the next line -*- tcl -*- \
exec wish "$0" -- "$@"

###############################################################################
# tkadle -- Single file source code program (Pronounced Teakaddle) originally
#           named as a "AsciiDoc List Editor" for creation and modification
#           of AsciiDoc list text files. For ease of deployment, tcllib and
#           other external packages are avoided.
################################################################################
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License along
# with this program; if not, write to the Free Software Foundation, Inc.,
# 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
###############################################################################

# Presence of TCL OO classes necessitates at least version 8.6
if {[catch {package require Tcl 8.6}]} {
    puts stderr "tkadle requires Tcl 8.6 or higher."
    puts stderr "Detected Tcl version: [info tclversion]"
    exit 1
}

set commandError 0
set commandFile ""
set paradigm "LINELIST"
for {set argIndex 0} {$argIndex < $argc} {incr argIndex} {
    switch -exact [lindex $argv $argIndex] {
        "-syn" {
            incr argIndex
            if {$paradigm eq "LINELIST"} {
                set paradigm [string toupper [lindex $argv $argIndex]]
            } else {
                set commandError 1
            }
        }
        default {
            if {$commandFile eq ""} {
                set commandFile [lindex $argv $argIndex]
            } else {
                set commandError 1
            }
        }
    }
}

# Handle syntax option via file extension
switch -exact -- $paradigm {
    "ADOC" {set paradigm "ASCIIDOC"}
    "MD"   {set paradigm "MARKDOWN"}
}

if {$paradigm ni [list "ASCIIDOC" "LINELIST" "MARKDOWN"]} {
    puts stderr "Invalid syntax option detected"
    set commandError 1
}

if {$commandError || ($commandFile eq "")} {
    puts stderr "Usage: tkadle.tcl \[-syn asciidoc|adoc|markdown|md\] filename"
    exit
}

#----------------------------------------------------------------------------
# HexadecIcon is a square image showing one hexadecimal per pixel (Grayscale)
#----------------------------------------------------------------------------
set IconInstances 0
oo::class create HexadecIcon {
    variable BaseSide
    variable ImageName
    constructor {Brightness colors} {
        global IconInstances
        incr IconInstances
        set ImageName BaseImage$IconInstances
        set BaseSide [string length [lindex $Brightness 0]]
        image create photo $ImageName -height [llength $Brightness] -width $BaseSide
        $ImageName blank
        set y 0
        set rgb [split [string toupper $colors] {}]
        foreach theRow $Brightness {
            set rowLength [string length $theRow]
            for {set x 0} {$x < $rowLength} {incr x} {
                set value [scan [string index $theRow $x] %x]
                foreach {i c} {0 R 1 G 2 B} {
                    if {$c in $rgb} {
                        set v$i $value
                    } else {
                        set v$i 0
                    }
                }
                $ImageName put [format #%x%x%x $v0 $v1 $v2] -to $x $y
            }
            if {$x != $BaseSide} {
                break
            }
            incr y
        }
        if {($x != $BaseSide) || ($y != $BaseSide)} {
            error "HexadecIcon input must be a geometric square of hexadecimal glyphs."
        }
        return
    }

    method getSize {sidePx} {
        set factor [gcd $sidePx $BaseSide]
        image create photo zoomed
        zoomed copy $ImageName -zoom [expr {$sidePx / $factor}]

        image create photo sampled
        sampled copy zoomed -subsample [expr {$BaseSide / $factor}]
        return sampled
    }

    method map {lo hi result} {
        scan $lo$lo %2x decLo
        scan $hi$hi %2x decHi
        scan $result$result %2x decResult
        for {set y 0} {$y < $BaseSide} {incr y} {
            for {set x 0} {$x < $BaseSide} {incr x} {
                set colors [split [$ImageName get $x $y]]
                for {set i 0} {$i < 3} {incr i} {
                    set value [lindex $colors $i]
                    if {($decLo <= $value) && ($value <= $decHi)} {
                        set value $decResult
                    }
                    set c$i $value
                }
                $ImageName put [format #%02x%02x%02x $c0 $c1 $c2] -to $x $y
            }
        }
        return
    }
}

#----------------------------------------------------------------------------
# LineItemList is a base class where each list item has no children
#----------------------------------------------------------------------------
oo::class create LineItemList {
    variable Constant
    constructor {} {
        global Prefs
        set Constant(descriptionsMax) 0
        set Constant(indentMin) -1
        set Constant(legend) "LineList"
        set Constant(logo) [list "\u2261" black red]
        set Constant(structured) false
        # Default values follow this line:
        set Prefs([my attName descriptions]) 0
        set Prefs([my attName indent]) 0
        set Prefs([my attName struct]) ""
        return
    }

    method attName {attribute} {
        return [string cat $Constant(legend) "_" $attribute]
    }

    method getConstant {key} {
        return $Constant($key)
    }

    method html {output} {
        if {$output eq ""} {
            error "No filename provided for html markup of unformatted list"
        }
        set fp [open $output w]
            puts $fp "<html>"
            puts $fp [format "<!-- %s -->" [SaveComment $output]]
            puts $fp [format "<title>%s</title>" [file tail $output]]
            puts $fp "<body>"
            my HTMLChildren $fp {}
            puts $fp "</body>\n</html>"
        close $fp
        return
    }

    method HTMLChildren {fp parentID} {
        foreach marking [.f.tvList children $parentID] {
            puts $fp [format "    <p>%s</p>" [.f.tvList item $marking -text]]
            my HTMLChildren $fp $marking
        }
        return
    }

    method labelLogo {lblWidget} {
        foreach i {"text" "foreground" "background"} j $Constant(logo) {
            $lblWidget configure -$i $j
        }
        return
    }

    method load {textFile} {
        set fp [open $textFile r]
            set data [split [read $fp] \n]
        close $fp

        if {[lindex $data end] eq ""} {
            set data [lreplace $data end end]
        }

        foreach line $data {
            Treeview::itemCache [.f.tvList insert {} end -text $line]
        }
        return 0
    }

    method save {textFile} {
        if {$textFile ne ""} {
            set fp [open $textFile w]
                my SaveChildren $fp {}
            close $fp
        }
        return
    }

    method SaveChildren {fp parentID} {
        foreach saving [.f.tvList children $parentID] {
            puts $fp [format "%s" [.f.tvList item $saving -text]]
            my SaveChildren $fp $saving
        }
        return
    }

    method structErrorMsg {} {
        return "Invalid structure item."
    }

    method structHelpMsg {} {
        return "No structure exists for this list format."
    }

    method validStruct {hierCode} {
        return false
    }
}

#----------------------------------------------------------------------------
# Asciidoc implementation of LineItemList methods
#----------------------------------------------------------------------------
oo::class create ADocFormat {
    superclass LineItemList
    variable Constant
    constructor {} {
        global Prefs
        set Constant(descriptionsMax) 3
        set Constant(indentMin) 0
        set Constant(legend) "AsciiDoc"
        set Constant(logo) [list "AD" white #1F8197]
        set Constant(structured) true
        # Default values follow this line:
        set Prefs([my attName descriptions]) 0
        set Prefs([my attName indent]) 4
        set Prefs([my attName struct]) "*"
        return
    }

    method AdocCommentLines {lastSkip lineOfText} {
        set result 0
        if {$lineOfText eq ""} {
            set result 1
        } elseif {[regexp {^////} $lineOfText]} {
            if {$lastSkip == 10000} {
                set result 1
            } else {
                set result 10000
            }
        } elseif {[regexp {^//} $lineOfText]} {
            set result 1
        }
        return $result
    }

    method GetDescToken {trimmed} {
        foreach token {"::::" ":::" "::" ";;"} {
            set foundDesc [string first $token $trimmed 0]
            if {$foundDesc >= 0} {
                return $token
            }
        }
        return ""
    }

    method GetOrderToken {trimmed gap1} {
        set token [string range $trimmed 0 [expr {$gap1 - 1}]]
        foreach i [split $token {}] {
            if {$i ne [string index $token 0]} {
                return ""
            }
        }
        return $token
    }

    method html {output} {
        if {$output eq ""} {
            error "No filename provided for html markup of asciidoc list"
        }
        exec asciidoc -o $output $ListFileIO::filename
        return
    }

    method load {textFile} {
        global Prefs
        set errorAt 0
        set prefStruct ""
        set levelTag [list]
        set skipLines 0
        set fp [open $textFile r]
        for {set lineNumber 1} {[gets $fp line] >= 0} {incr lineNumber} {
            set trimmed [string trimleft $line]
            set skipLines [my AdocCommentLines $skipLines $trimmed]
            if {$skipLines} {
                continue
            }

            set foundLevel -1
            set foundType "+"
            set gap1 [string first " " $trimmed]
            if {$gap1 > 0} {
                ###############################################################
                #        |<--spaces-->|<-----trimmed------>|
                # $line: ______________???? alpha beta gamma
                #  gap1: ------------------^
                # token:           -->|    |<--
                ###############################################################
                set token [my GetOrderToken $trimmed $gap1]
                if {$token ne ""} {
                    set foundLevel [lsearch -exact $levelTag $token]
                    if {$foundLevel == -1} {
                        switch -exact -- [string index $token end] {
                            "." {set foundType "."}
                            "*" {set foundType "*"}
                            "-" {set foundType "*"}
                        }
                        if {$foundType != "+"} {
                            set foundLevel [llength $levelTag]
                            set atStruct [string index $prefStruct $foundLevel]
                            if {($atStruct eq $foundType) || ($atStruct eq "")} {
                                lappend levelTag $token
                                if {$atStruct eq ""} {
                                    append prefStruct $foundType
                                }
                            }
                        }
                    }

                    if {$foundLevel >= 0} {
                        set listItem [string range $trimmed [expr {$gap1 + 1}] end]
                    }
                }
            }

            if {$foundLevel == -1} {
                ###############################################################
                #        |<--spaces-->|<----trimmed----->|
                # $line: ______________alpha beta::: gamma
                #        foundDesc---------------^
                # token:                     -->|   |<--
                ###############################################################
                set token [my GetDescToken $trimmed]
                if {$token ne ""} {
                    set foundLevel [lsearch -exact $levelTag $token]
                    if {$foundLevel == -1} {
                        set foundType ":"
                        set foundLevel [llength $levelTag]
                        set atStruct [string index $prefStruct $foundLevel]
                        if {($atStruct eq $foundType) || ($atStruct eq "")} {
                            lappend levelTag $token
                            if {$atStruct eq ""} {
                                append prefStruct $foundType
                            }
                        }
                    }

                    if {$foundLevel >= 0} {
                        set foundDesc [string first $token $trimmed 0]
                        set listItem [string range $trimmed 0 [expr {$foundDesc - 1}]] 
                    }
                }
            }

            if {$foundLevel >= 0} {
                set foundType [string index $prefStruct $foundLevel]
                set levelTag [lrange $levelTag 0 $foundLevel]
            }

            if {$foundType eq "+"} {
                if {![Treeview::loadAppend $trimmed]} {
                    set errorAt $lineNumber
                    break
                }
            } else {
                if {[string index $prefStruct $foundLevel] != $foundType} {
                    set errorAt $lineNumber
                    break
                }
                Treeview::loadInsert $foundLevel $listItem
            }
        }
        close $fp
        if {!$errorAt} {
            set descriptions 0
            while {[string index $prefStruct 0] eq ":"} {
                incr descriptions
                set prefStruct [string range $prefStruct 1 end]
            }
            if {$prefStruct eq ""} {
                set prefStruct "*"
            }
            set Pref([my attName descriptions]) $descriptions
            set Pref([my attName struct]) [Preferences::shortStruct $prefStruct]
        }
        return $errorAt
    }

    method save {textFile} {
        if {$textFile ne ""} {
            set fp [open $textFile w]
                puts $fp [SaveComment "//"]
                puts $fp ""
                my SaveChildren $fp {} 0
            close $fp
        }
        return
    }

    method SaveChildren {fp parentID depth} {
        global Prefs
        set indent [string repeat " " $Prefs([my attName indent])]
        set depthdent [string repeat $indent $depth]
        if {$depth < $Prefs([my attName descriptions])} {
            incr depth
            set glyphs [string repeat ":" [expr {$depth + 1}]]
            foreach saving [.f.tvList children $parentID] {
                puts $fp [format "%s%s" [.f.tvList item $saving -text] $glyphs]
                my SaveChildren $fp $saving $depth
            }
        } else {
            set adjusted [expr {$depth - $Prefs([my attName descriptions])}]
            set glyph [string index $Prefs([my attName struct]) $adjusted]
            if {$glyph eq ""} {
                set glyph [string index $Prefs([my attName struct]) end]
            }
            incr depth
            set glyphs [string repeat $glyph $depth]
            foreach saving [.f.tvList children $parentID] {
                puts $fp [format "%s%s %s" $depthdent $glyphs [.f.tvList item $saving -text]]
                my SaveChildren $fp $saving $depth
            }
        }
        return
    }

    method structErrorMsg {} {
        return "Invalid structure item: A valid item is \"*\" or \".\""
    }

    method structHelpMsg {} {
        set codes {
            "\"*\" \u2192 unordered list type"
            "\".\" \u2192 ordered list type"
            " "
            "Example: \".*\" is ordered list items above unordered lists"
            "Note: checklist icons require unordered list"
        }
        return [join $codes \n]
    }

    method validStruct {hierCode} {
        return [regexp {^[*.]+$} $hierCode]
    }
}

#----------------------------------------------------------------------------
# MarkDown implementation of LineItemList methods
#----------------------------------------------------------------------------
oo::class create MDFormat {
    superclass LineItemList
    variable Constant
    constructor {} {
        global Prefs
        set Constant(descriptionsMax) 0
        set Constant(indentMin) 1
        set Constant(legend) "MarkDown"
        set Constant(logo) [list "M\u2193" black white]
        set Constant(structured) true
        # Default values follow this line:
        set Prefs([my attName descriptions]) 0
        set Prefs([my attName indent]) 4
        set Prefs([my attName struct]) "*"
        return
    }

    method GetListToken {line gap1} {
        set token [string range $line 0 [expr {$gap1 - 1}]]
        if {[string index $token end] eq "."} {
            if {[regexp {^(\s*\d*)\.$} $token]} {
                regsub -all {\d} $token "" token
            }
        }
        if {[string trimleft $token] in [list "." "+" "-" "*"]} {
            return $token
        }
        return ""
    }

    method html {output} {
        if {$output eq ""} {
            error "No filename provided for html markup of markdown list"
        }
        exec markdown $ListFileIO::filename > $output
        return
    }

    method load {textFile} {
        global Prefs
        set errorAt 0
        set prefStruct ""
        set levelTag [list]
        set fp [open $textFile r]
        for {set lineNumber 1} {[gets $fp line] >= 0} {incr lineNumber} {
            set trimmed [string trimleft $line]
            if {$trimmed eq ""} {
                continue
            }
            set ws [expr {[string length $line] - [string length $trimmed]}]

            set foundType "&"
            set foundLevel -1
            set gap1 [string first " " $trimmed]
            if {$gap1 > 0} {
                ###############################################################
                #        |<--spaces-->|<-----trimmed------>|
                # $line: ______________???? alpha beta gamma
                #  gap1: ------------------^
                #        |<----token----->|
                ###############################################################
                incr gap1 $ws
                set token [my GetListToken $line $gap1]
                if {$token ne ""} {
                    set foundLevel [lsearch -exact $levelTag $token]
                    if {$foundLevel == -1} {
                        set foundType [string index $token end]
                        set foundLevel [llength $levelTag]
                        set atStruct [string index $prefStruct $foundLevel]
                        if {$atStruct eq $foundType || $atStruct eq ""} {
                            lappend levelTag $token
                            if {$atStruct eq ""} {
                                append prefStruct $foundType
                            }
                        }
                    }

                    if {$foundLevel >= 0} {
                        set listItem [string range $line [expr {$gap1 + 1}] end]
                    }
                }
            }

            if {$foundLevel >= 0} {
                set foundType [string index $prefStruct $foundLevel]
                set levelTag [lrange $levelTag 0 $foundLevel]
            }

            if {$foundType eq "&"} {
                if {![Treeview::loadAppend $trimmed]} {
                    set errorAt $lineNumber
                    break
                }
            } else {
                if {[string index $prefStruct $foundLevel] != $foundType} {
                    set errorAt $lineNumber
                    break
                }
                Treeview::loadInsert $foundLevel $listItem
            }
        }
        close $fp

        if {!$errorAt} {
            if {$prefStruct eq ""} {
                set prefStruct "*"
            }
            set Pref([my attName struct]) [Preferences::shortStruct $prefStruct]
        }
        return $errorAt
    }

    method save {textFile} {
        if {$textFile ne ""} {
            set fp [open $textFile w]
            my SaveChildren $fp {} 0
            close $fp
        }
        return
    }

    method SaveChildren {fp parentID depth} {
        global Prefs
        set indent [string repeat " " $Prefs([my attName indent])]
        set depthdent [string repeat $indent $depth]
        set glyph [string index $Prefs([my attName struct]) $depth]
        if {$glyph eq ""} {
            set glyph [string index $Prefs([my attName struct]) end]
        }
        incr depth
        set counter 1
        foreach saving [.f.tvList children $parentID] {
            if {$glyph eq "."} {
                puts $fp [format "%s%s%s %s" $depthdent $counter $glyph [.f.tvList item $saving -text]]
            } else {
                puts $fp [format "%s%s %s" $depthdent $glyph [.f.tvList item $saving -text]]
            }
            my SaveChildren $fp $saving $depth
            incr counter
        }
        return
    }

    method structErrorMsg {} {
        return "Invalid structure item: A valid item is \"*\", \"+\", \"-\" or \".\""
    }

    method structHelpMsg {} {
        set codes {
            "\"*\" \u2192 unordered list type"
            "\"+\" \u2192 unordered list type"
            "\"-\" \u2192 unordered list type"
            "\".\" \u2192 ordered list type"
            " "
            "Example: \".*\" is ordered list items above unordered lists"
        }
        return [join $codes \n]
    }

    method validStruct {hierCode} {
        return [regexp {^[*+-.]+$} $hierCode]
    }
}

#----------------------------------------------------------------------------
# Maintain list of deleted items to be trashed at program's end
#----------------------------------------------------------------------------
namespace eval Deleted {
    proc archiveItems {itemList} {
        foreach i $itemList {
            set itemText [.f.tvList item $i -text]
            if {$itemText ne ""} {
                .f.lbDEL insert end $itemText
                .f.lbDEL see end
            }
            archiveItems [.f.tvList children $i]
        }
        return
    }

    proc clear {} {
        .f.lbDEL delete 0 end
        return
    }

    proc endKey {} {
        .f.lbDEL selection clear 0 end
        selectPrev
        return
    }

    proc gui {} {
        listbox .f.lbDEL -yscrollcommand {.f.sList set}
        .f.lbDEL configure -background black -foreground white -selectmode extended
        scrollbar .f.sDEL -command {.f.lbDEL yview}
    }

    proc hide {} {
        pack forget .f.lbDEL
        pack forget .f.sDEL
        return
    }

    proc homeKey {} {
        .f.lbDEL selection clear 0 end
        selectNext
        return
    }

    proc returnItem {} {
        Gui::mode "LIST"
        foreach i [.f.lbDEL curselection] {
            set newItem [Treeview::itemCache [.f.tvList insert {} end -text [.f.lbDEL get $i]]]
            .f.tvList selection set $newItem
            .f.tvList see [.f.tvList selection]
        }
        Treeview::focusOn $newItem
        return
    }

    proc selectNext {} {
        Navigation::SelectLBItem .f.lbDEL 1
        return
    }

    proc selectPrev {} {
        Navigation::SelectLBItem .f.lbDEL -1
        return
    }

    proc show {} {
        pack .f.lbDEL -side left -expand yes -fill both
        .f.lbDEL configure -yscrollcommand {.f.sDEL set}
        update
        if {[.f.sDEL get] != {0.0 1.0}} {
            pack .f.sDEL -side right -fill y
        }
        return
    }

    proc status {} {
        set selected [.f.lbDEL curselection]
        set prefix ""
        if {[llength $selected] > 1} {
            set prefix "[llength $selected] of "
        } elseif {$selected ne {}} {
            set idx [expr {1 + [lindex $selected 0]}]
            set prefix [format "%s of " [StatusBar::Suffixed $idx]]
        }
        set suffix ""
        if {$prefix ne ""} {
            set suffix "       Press <Return> to append selected item to list"
        }
        StatusBar::show [string cat $prefix "[.f.lbDEL size] deleted item(s)" $suffix]
        return
    }
}

#----------------------------------------------------------------------------
# Functionality to reformat list content as HTML paragraphs.
#----------------------------------------------------------------------------
namespace eval Export {
    proc FormatNonPara {nodeID selected} {
        global Prefs
        set text [FormatPara $nodeID $selected]
        foreach {val start end} { "Bold"      "<b>" "</b>"
                                  "Italics"   "<i>" "</i>"
                                  "Underline" "<u>" "</u>" } {
            if {$Prefs(exportNonPara$val)} {
                set text [string cat $start $text $end]
            }
        }
        return [string cat "    <br><div>" $text "</div><br>"]
    }

    proc FormatPara {nodeID selected} {
        global Prefs
        set sentence [.f.tvList item $nodeID -text]
        set tr [string trimright $sentence]
        if {$tr ne ""} {
            if {$Prefs(exportPeriods)} {
                if {[string index $tr end] ne "."} {
                    set sentence [string cat $tr "."]
                }
            }
            if {$Prefs(exportSentCap)} {
                set sentence [string toupper $sentence 0]
            }
            if {[lsearch -exact $selected $nodeID] ne -1} {
                set needle [format "\"background-color:%s\"" \
                        $Prefs(exportHilight)]
                set sentence "<span style=$needle>$sentence</span>"
            }
        }
        return [string cat "        " $sentence]
    }

    proc gui {} {
        global Prefs
        labelframe .f.export -text "Export" -padx 2 -pady 2

        labelframe .f.export.par -pady 2
        pack .f.export.par -side top -fill x
        Export::GuiPara .f.export.par

        labelframe .f.export.non -padx 2 -pady 2
        pack .f.export.non -side top -fill x
        Export::GuiNonPara .f.export.non

        frame .f.export.hilite -padx 2 -pady 2
        button .f.export.hilite.btn -text "Set HTML highlight color..." \
                -command "SetHighlightColor .f.export.hilite.lbl exportHilight"
        label .f.export.hilite.lbl -text "Sample highlight text" \
                -background $Prefs(exportHilight)
        grid .f.export.hilite.btn .f.export.hilite.lbl -padx 2 -sticky nsew
        pack .f.export.hilite -side top -fill x

        frame .f.export.buts
        ttk::button .f.export.buts.exp -text "Export" -default active -command Export::SaveHTML
        ttk::button .f.export.buts.can -text "Cancel" -default normal -command {event generate . <Escape>}
        grid .f.export.buts.exp .f.export.buts.can
        grid columnconfigure .f.export.buts 0 -weight 1 -uniform a
        grid columnconfigure .f.export.buts 1 -weight 1 -uniform a
        grid columnconfigure .f.export 2 -weight 1
        pack .f.export.buts -side bottom -fill x

        frame .f.export.sep -relief groove -borderwidth 2 -width 2 -height 2
        pack .f.export.sep -side bottom -fill x -pady 3

        Export::LFrameToggle .f.export.non exportNonParagraph
        Export::LFrameToggle .f.export.par exportParagraph

        return
    }

    proc GuiNonPara {w} {
        global Prefs
        set ancestry { "Bold"      "Add bold style"
                       "Italics"   "Add italics style"
                       "Underline" "Add underlined style" }

        checkbutton $w.cb -text "Non-Paragraph items" \
                -variable Prefs(exportNonParagraph) \
                -command "Export::LFrameToggle $w exportNonParagraph"
        $w configure -labelwidget $w.cb

        label $w.legend -text "Description: Each list item not designated paragraph content will appear on its own line."
        pack $w.legend -side top -anchor w
        foreach {val legend} $ancestry {
            checkbutton $w.exportNonPara$val -text $legend \
                    -variable Prefs(exportNonPara$val)
            pack $w.exportNonPara$val -side top -anchor w
        }

        return
    }

    proc GuiPara {w} {
        global Prefs
        set contents { "parentless" "Leaf items"
                       "parented"   "Single ancestor and leaf items" }
        set styles { "block"  "Block"
                     "indent" "Indented" }

        checkbutton $w.cb -text "Paragraph items" \
                -variable Prefs(exportParagraph) \
                -command "Export::LFrameToggle $w exportParagraph"
        $w configure -labelwidget $w.cb

        label $w.legend -text "Description: Exported sibling leaf nodes (And optionally one ancestor) are catenated."
        grid $w.legend -row 0 -column 0 -columnspan 2 -sticky w
        labelframe $w.content -text "Content" -padx 2 -pady 2
        grid $w.content -row 1 -column 0 -sticky w -padx 2
        foreach {val legend} $contents {
            radiobutton $w.content.$val -text $legend -value $val \
                    -variable Prefs(exportParaContent)
            pack $w.content.$val -side top -anchor w
        }
        labelframe $w.style -text "Style" -padx 2 -pady 2
        grid $w.style -row 1 -column 1 -sticky w -padx 2
        foreach {val legend} $styles {
            radiobutton $w.style.$val -text $legend -value $val \
                    -variable Prefs(exportParaStyle)
            pack $w.style.$val -side top -anchor w
        }
        checkbutton $w.end -variable Prefs(exportPeriods) \
                -text "Ensure every exported item ends with period."
        grid $w.end -row 2 -column 0 -columnspan 2 -sticky w
        checkbutton $w.cap -variable Prefs(exportSentCap) \
                -text "Ensure sentence capitalization."
        grid $w.cap -row 3 -column 0 -columnspan 2 -sticky w
        return
    }

    proc hide {} {
        pack forget .f.export
        return
    }

    proc LFrameToggle {w enable} {
        global Prefs
        foreach child [winfo children $w] {
            if {[llength [winfo children $child]]} {
                Export::LFrameToggle $child $enable
            } elseif {$child != "$w.cb"} {
                if {$Prefs($enable)} {
                    $child configure -state normal
                } else {
                    $child configure -state disabled
                }
            }
        }
        return
    }

    proc SaveHTML {} {
        global Prefs
        set targetFile [file rootname $ListFileIO::filename].html
        set fp [open $targetFile w]
            puts $fp "<html><head>"
            puts $fp "    <meta charset=\"utf-8\">"
            puts $fp [format "<!-- %s -->" [SaveComment $targetFile]]
            puts $fp [format "<title>%s</title>" [file tail $targetFile]]
            puts $fp "<style type=\"text/css\">"
            if {$Prefs(exportParaStyle) eq "indent"} {
                puts $fp "    p {"
                puts $fp "        text-indent: 0.5in;"
                puts $fp "        margin-bottom: 0;"
                puts $fp "        margin-top: 0;"
                puts $fp "    }"
            }
            puts $fp "</style>"
            puts $fp "</head>"
            puts $fp "<body>"

            if {$Prefs(exportNonParagraph) || $Prefs(exportParagraph)} {
                Export::SaveHTMLChildren $fp {} [.f.tvList selection]
            } else {
                puts $fp "<p>NOTICE: No content selected for export</p>"
            }

            puts $fp "</body>\n</html>"
        close $fp
        Preferences::save
        exec xdg-open $targetFile
        return
    }

    proc SaveHTMLChildren {fp parentID selected} {
        global Prefs
        set descendents [.f.tvList children $parentID]
        if {[llength $descendents] == 0} {
            return
        }

        set state "NONPARAGRAPH"
        foreach saving $descendents {
            set further [.f.tvList children $saving]
            set moreDepth [llength $further]

            switch -exact -- $state {
                "NONPARAGRAPH" {
                    if {$moreDepth} {
                        SaveHTMLNonParagraph $fp $saving $further $selected
                    } else {
                        if {$Prefs(exportParagraph)} {
                            set topicSentence ""
                            if {$Prefs(exportParaContent) eq "parented"} {
                                append topicSentence [Export::FormatPara $parentID $selected]
                            }
                            puts $fp [format "    <p>%s" $topicSentence]
                            puts $fp [Export::FormatPara $saving $selected]
                        }
                        set state "PARAGRAPH"
                    }
                }
                "PARAGRAPH" {
                    if {$moreDepth} {
                        if {$Prefs(exportParagraph)} {
                            puts $fp "    </p>"
                        }
                        set state "NONPARAGRAPH"
                        SaveHTMLNonParagraph $fp $saving $further $selected
                    } elseif {$Prefs(exportParagraph)} {
                        puts $fp [Export::FormatPara $saving $selected]
                    }
                }
            }
        }
        if {($state == "PARAGRAPH") && ($Prefs(exportParagraph))} {
            puts $fp "    </p>"
        }
        return
    }

    proc SaveHTMLNonParagraph {fp parentID further selected} {
        global Prefs
        if {$Prefs(exportNonParagraph)} {
            set printSeparate true
            foreach node $further {
                if {[llength [.f.tvList children $node]] == 0} {
                    set printSeparate false
                    break
                }
            }
            if {$printSeparate || ($Prefs(exportParaContent) eq "parentless")} {
                puts $fp [FormatNonPara $parentID $selected]
            }
        }
        Export::SaveHTMLChildren $fp $parentID $selected
        return
    }

    proc show {} {
        pack .f.export -side left -expand yes -fill both
        return
    }
}

#----------------------------------------------------------------------------
# Nested list folding operations.
#----------------------------------------------------------------------------
namespace eval Folding {
    variable oo
    set oo 10000

    proc FirstClosed {parentItem} {
        set result $Folding::oo
        foreach node [.f.tvList children $parentItem] {
            if {[.f.tvList item $node -open]} {
                set deeper [FirstClosed $node]
                if {$result > $deeper} {
                    set result [expr {$deeper + 1}]
                }
            } else {
                set result 1
                break
            }
        }
        return $result
    }

    proc LastOpen {parentItem} {
        set result 0
        foreach node [.f.tvList children $parentItem] {
            if {[.f.tvList item $node -open]} {
                set deeper [expr {[LastOpen $node] + 1}]
                if {$result < $deeper} {
                    set result $deeper
                }
            }
        }
        return $result
    }

    proc levelClose {} {
        set levels [LastOpen {}]
        OpenLevels $levels {}
        return
    }

    proc levelOpen {} {
        set levels [FirstClosed {}]
        if {$levels < $Folding::oo} {
            OpenLevels [expr {$levels + 1}] {}
        }
        return
    }

    proc OpenLevels {opening parentItem} {
        if {$opening > 1} {
            foreach i [.f.tvList children $parentItem] {
                .f.tvList item $i -open true
                OpenLevels [expr {$opening - 1}] $i
            }
        } else {
            foreach closing [.f.tvList children $parentItem] {
                OpenLevels 0 $closing
                .f.tvList item $closing -open false
            }
        }
        return
    }
}

#----------------------------------------------------------------------------
# Reconfigure GUI appearance on demand.
#----------------------------------------------------------------------------
namespace eval Gui {
    variable current

    proc escapeKey {} {
        variable current
        if {$current eq "EDIT"} {
            ChangeText $Treeview::buffer(edit)
        } elseif {$current eq "EXPORT"} {
            toggleExport
        } elseif {$current eq "LIST"} {
            .f.tvList selection set {}
        } elseif {$current eq "PREFERENCES"} {
            toggleOptions
        } elseif {$current eq "REMOVED"} {
            toggleRemoved
        } else {
            Gui::mode "LIST"
            Search::Hide
        }
        return
    }

    proc listMode {{operation ""}} {
        variable current
        if {$current ne "LIST"} {
            return false
        }
        if {$operation ne ""} {
            $operation
        }
        return true
    }

    proc mode {newMode} {
        variable current
        if {$current eq $newMode} {
            return
        }
        if {$newMode ne "EXPORT"} {
            Export::hide
        }
        if {$newMode ne "LIST" && $newMode ne "EDIT"} {
            pack forget .f.tvList
            pack forget .f.sList
            Search::Hide
        }
        if {$newMode ne "REMOVED"} {
            Deleted::hide
        }
        if {$newMode ne "PREFERENCES"} {
            Preferences::hide
        }
        if {$newMode ne "EDIT"} {
            grid remove .lfEdit
        }
        switch -exact -- $newMode {
            EXPORT {
                Export::show
            }
            LIST {
                pack .f.tvList -side left -expand yes -fill both
                set lohi [.f.sList get]
                ScrollbarOnDemand .f.sList [lindex $lohi 0] [lindex $lohi 1]
            }
            PREFERENCES {
                Preferences::show
                Preferences::status
            }
            REMOVED {
                Deleted::show
                Deleted::status
            }
            EDIT {
                grid .lfEdit -row 2 -column 0 -sticky ew
                StatusBar::show "Edit Item"
            }
        }
        set current $newMode
        return
    }

    proc nonEditMode {{operation ""}} {
        variable current
        if {$current eq "EDIT"} {
            return false
        }
        if {$operation ne ""} {
            $operation
        }
        return true
    }

    proc toggleExport {} {
        variable current
        if {$current eq "EXPORT"} {
            Gui::mode "LIST"
            Treeview::focusOn [.f.tvList selection]
            StatusBar::show
            Preferences::save
        } elseif {$current eq "LIST"} {
            Gui::mode "EXPORT"
        }
        return
    }

    proc toggleOptions {} {
        variable current
        if {$current eq "PREFERENCES"} {
            Gui::mode "LIST"
            Treeview::focusOn [.f.tvList selection]
            StatusBar::show
            Preferences::save
        } elseif {$current eq "LIST"} {
            Gui::mode "PREFERENCES"
        }
        return
    }

    proc toggleRemoved {} {
        variable current
        if {$current eq "REMOVED"} {
            Gui::mode "LIST"
            Treeview::focusOn [.f.tvList selection]
            StatusBar::show
        } elseif {$current eq "LIST"} {
            Gui::mode "REMOVED"
        }
        return
    }
}

#----------------------------------------------------------------------------
# Help tips management and presentation
#----------------------------------------------------------------------------
namespace eval Help {
    variable helpTip
    array set helpTip [list]

    proc bindTip {widget tag script tip} {
        variable helpTip
        bind $widget $tag $script
        set helpTip($tag) $tip
        if {[array get helpTip helping] eq ""} {
            set helpTip(helping) $tag
        }
        return
    }

    proc dialog {} {
        variable helpTip
        catch {destroy .help}
        toplevel .help
        wm title .help "tkadle Help"

        button .help.bClose -text Close -command {destroy .help}
        pack .help.bClose -side bottom -anchor e
        text .help.t -width 56 -height 30 -yscrollcommand ".help.s set"
        pack .help.t -expand 1 -fill both -side left -anchor nw
        scrollbar .help.s -command ".help.t yview"
        pack .help.s -fill y -side right -anchor ne

        .help.t insert end "\"key\": effect\n"
        .help.t insert end "-------------\n"

        set contents [list]
        foreach sym [array names helpTip] {
            if {$sym ne "helping"} {
                set keystr [Keystroke4 $sym]
                lappend contents [string cat "\"" $keystr "\": " $helpTip($sym) "\n"]
            }
        }
        foreach line [lsort $contents] {
            .help.t insert end $line
        }

        .help.t insert end "\n"
        set arrowKeys {"Note: arrow key labels shown above"
                       "----------------------------------"
                       " \"\u2192\" is labeled \"Right\""
                       " \"\u2191\" is labeled \"Up\""
                       " \"\u2190\" is labeled \"Left\""
                       " \"\u2193\" is labeled \"Down\""}
        .help.t insert end [join $arrowKeys "\n"]
        focus .help.bClose
        return
    }

    proc Keystroke4 {ksymbol} {
        while {[string index $ksymbol 0  ] == "<" && \
               [string index $ksymbol end] == ">"} {
            set ksymbol [string range $ksymbol 1 end-1]
        }

        set keyBindings {, comma . period < less ? question > greater
            \{ braceleft \} braceright \[ bracketleft \] bracketright | bar
            \\ backslash + plus = equal _ underscore - minus \( parenleft
            \) parenright * asterisk & ampersand ^ asciicircum % percent
            \$ dollar # numbersign @ at ! exclam : colon ; semicolon}

        foreach {key sym} $keyBindings {
            if {$sym eq $ksymbol} {
                return $key
            }
        }

        return $ksymbol
    }
}

#----------------------------------------------------------------------------
# Variables and procs for list file input/output operations regarding
#----------------------------------------------------------------------------
namespace eval ListFileIO {
    variable filename ""

    proc loadFile {textFile} {
        global openFile
        variable filename
        Treeview::clear

        set errorLine 0
        if {[file exists $textFile]} {
            set errorLine [$openFile load $textFile]
            set successText ""
        } else {
            set chan [open $textFile w+]
            close $chan
            set successText "File \"$textFile\" not found, created empty file."
        }

        if {$errorLine} {
            error [format "File $textFile load error on line $errorLine"]
            StatusBar::show "File load error line $errorLine"
            Treeview::clear
        } else {
            set filename $textFile
            Gui::mode "LIST"
            if {$successText eq ""} {
                StatusBar::show
            } else {
                StatusBar::show $successText
            }
        }
        zeroize
        return
    }

    proc nameLocation {} {
        variable filename
        if {$filename eq ""} {
            return ""
        }
        return [string cat [file tail $filename] " - " [file dirname $filename]]
    }

    # Saving is delayed until idle time to prevent HD thrashing at every change
    # such as CTRL+arrow shifting an item from end of the list to the other.
    proc saveFile {{delay 5000}} {
        global delayID
        if {[writable]} {
            after cancel $delayID
            set delayID [after $delay ListFileIO::SaveNow]
            SetWindowTitle true
            StatusBar::show
        }
        return
    }

    proc SaveNow {} {
        global delayID openFile
        variable filename

        set delayID "None"
        set tmpFilename [string cat $filename ".tmp"]
        if {[catch {$openFile save $tmpFilename} err]} {
            tk_messageBox -icon error -type ok -message "Error" \
                -detail "File not saved.\n\nListFileIO: $err"
        } else {
            file copy -force $tmpFilename $filename
            SetWindowTitle false
            StatusBar::show
        }

        if {[file exists $tmpFilename]} {
            file delete $tmpFilename
        }
        return
    }

    proc writable {} {
        variable filename
        return [file writable $filename]
    }
}

#----------------------------------------------------------------------------
# Widget selection front ends accessed by key presses.
#----------------------------------------------------------------------------
namespace eval Navigation {
    proc endKey {} {
        global Gui::current
        switch -exact -- $Gui::current {
            LIST    {.f.tvList selection set {}; SelectDefaultItem prev}
            REMOVED {Deleted::endKey}
        }
        return
    }

    proc homeKey {} {
        global Gui::current
        switch -exact -- $Gui::current {
            LIST    {.f.tvList selection set {}; SelectDefaultItem next}
            REMOVED {Deleted::homeKey}
        }
        return
    }

    #------------------------------------------------------------------------
    # If no treeview item is selected then make a logical choice.
    #------------------------------------------------------------------------
    proc SelectDefaultItem {{tvdir ""}} {
        set selected [.f.tvList selection]
        if {($selected eq {}) && ([array size Treeview::ItemID] > 0)} {
            switch -exact -- $tvdir {
                prev {set tvdir next}
                next {set tvdir prev}
            }
            for {set i 0} {[info exists Treeview::ItemID($i)] == 0} {incr i} {}
            set result $Treeview::ItemID($i)
            while {[.f.tvList parent $result] != {}} {
                set result [.f.tvList parent $result]
            }
            if {$tvdir ne ""} {
                while {[.f.tvList $tvdir $result] != {}} {
                    set result [.f.tvList $tvdir $result]
                }
            }
            Treeview::focusOn $result
        }
        return
    }

    #------------------------------------------------------------------------
    # If no listbox item is selected then make a logical choice.
    #------------------------------------------------------------------------
    proc SelectLBItem {widgetID offset} {
        set selected [llength [$widgetID curselection]]
        if {$selected == 0} {
            if {$offset == 1} {
                set currentID 0
            } else {
                set currentID end
            }
        } elseif {$selected == 1} {
            set currentID [lindex [$widgetID curselection] 0]
        } else {
            return
        }
        $widgetID selection clear 0 end
        $widgetID selection set $currentID $currentID
        $widgetID activate $currentID
        $widgetID see $currentID
        focus $widgetID
        return
    }

    proc selectNext {} {
        global Gui::current
        switch -exact -- $Gui::current {
            LIST    {SelectDefaultItem next}
            REMOVED {Deleted::selectNext}
        }
        return
    }
    proc selectPrev {} {
        global Gui::current
        switch -exact -- $Gui::current {
            LIST    {SelectDefaultItem prev}
            REMOVED {Deleted::selectPrev}
        }
        return
    }
}

#----------------------------------------------------------------------------
# tkadle Preferences viewing and assignment
#----------------------------------------------------------------------------
namespace eval Preferences {
    proc changedColor {} {
        global Prefs
        if {$Prefs(changedShow)} {
            .f.tvList tag configure modded -background $Prefs(changedHilight)
        } else {
            .f.tvList tag configure modded -background ""
        }
        return
    }

    proc gui {} {
        global openFile

        frame .f.fPrefs
        ttk::notebook .f.fPrefs.nbPrefs
        .f.fPrefs.nbPrefs add [GUIFrame .f.fPrefs.nbPrefs] -text "GUI"
        if {[$openFile getConstant structured]} {
            .f.fPrefs.nbPrefs add [StylesFrame .f.fPrefs.nbPrefs] -text "Syntax"
        }
        grid .f.fPrefs.nbPrefs -sticky news -padx 2 -pady 2
        grid rowconfigure .f.fPrefs 0 -weight 1
        grid columnconfigure .f.fPrefs 0 -weight 1
        return
    }

    proc GUIFrame {parent} {
        global Prefs
        set result [frame $parent.gui -padx 2 -pady 2]

        labelframe $result.nav -text "Navigation key options" -padx 2 -pady 2
        label $result.nav.arrows -anchor w -background grey95\
                -text "Arrow keys enabled: \u2191\u2190\u2193\u2192"
        pack $result.nav.arrows -side top -fill x
        foreach {val legend caption} {khjl vi "Editor movement keys \"khjl\""} {
            checkbutton $result.nav.$val -text "$legend ($caption)" \
                    -anchor w -variable Prefs($val) -command {Preferences::NavKeys}
            pack $result.nav.$val -side top -fill x
        }
        pack $result.nav -side top -fill x -pady 4

        labelframe $result.sort -text "Arrangement key sort options" -padx 2 -pady 2
        set sortLegend { ascii      "Default (Unicode code-point collation order)" \
                         nocase     "Case-insensitive" \
                         dictionary "Case-insensitive and integer values" }
        foreach {name legend} $sortLegend {
            radiobutton $result.sort.$name -text $legend -value $name \
                    -anchor w -variable Prefs(sort)
            pack $result.sort.$name -side top -fill x
        }
        pack $result.sort -side top -fill x -pady 4

        labelframe $result.changed -padx 2 -pady 2
        checkbutton $result.changed.cb -text "Highlight changed list items" \
                -variable Prefs(changedShow) \
                -command "Preferences::changedColor"
        $result.changed configure -labelwidget $result.changed.cb
        button $result.changed.btn -text "Set changed highlight color..." \
                -command "SetHighlightColor $result.changed.lbl changedHilight"
        label $result.changed.lbl -text "Sample highlight item" \
                -background $Prefs(changedHilight)
        grid $result.changed.btn $result.changed.lbl -padx 2 -sticky nsew
        pack $result.changed -side top -fill x -pady 4

        frame $result.offsetY
        label $result.offsetY.lbl -text "tkadle application window vertical offset (Pixels): "
        spinbox $result.offsetY.sp -from 0 -to 200 -width 4 -validate key \
                -vcmd {string is integer %P} -textvariable Prefs(guiOffsetY) \
                -justify right
        pack $result.offsetY.lbl -side left
        pack $result.offsetY.sp -side left -fill x
        pack $result.offsetY -side top -fill x

        return $result
    }

    proc hide {} {
        pack forget .f.fPrefs
        return
    }

    proc load {} {
        global ConfigFile Prefs openFile
        set Prefs(changedHilight) yellow
        set Prefs(changedShow) 1
        set Prefs(exportHilight) #BFB
        set Prefs(exportNonParaBold) 0
        set Prefs(exportNonParagraph) 1
        set Prefs(exportNonParaItalics) 1
        set Prefs(exportNonParaUnderline) 0
        set Prefs(exportParaContent) parentless
        set Prefs(exportParagraph) 1
        set Prefs(exportParaStyle) indent
        set Prefs(exportPeriods) 1
        set Prefs(exportSentCap) 1
        set Prefs(geometry) 640x480
        set Prefs(guiOffsetY) 27
        set Prefs(insertDest) after
        set Prefs(khjl) 0
        set Prefs(sort) dictionary

        if {[file isfile $ConfigFile]} {
            set fp [open $ConfigFile r]
            set data [split [read $fp] \n]
            foreach i $data {
                set config [split $i " "]
                if {[lindex $config 0] ne ""} {
                    set Prefs([lindex $config 0]) [lindex $config 1]
                }
            }
            close $fp
        } else {
            set fp [open $ConfigFile w]
            puts $fp "0"
            close $fp
        }

        if {[$openFile getConstant structured]} {
            set struct [$openFile attName struct]
            set Prefs($struct) [shortStruct $Prefs($struct)]

            set indent [$openFile attName indent]
            if {$Prefs($indent) < [$openFile getConstant indentMin]} {
                set Prefs($indent) [$openFile getConstant indentMin]
            }

            set descriptions [$openFile attName descriptions]
            if {$Prefs($descriptions) > [$openFile getConstant descriptionsMax]} {
                set Prefs($descriptions) [$openFile getConstant descriptionsMax]
            }
        }
        Preferences::NavKeys

        return
    }

    # Other potental navigation options for the future: arrow keys, wasd...
    proc NavKeys {} {
        global Prefs
        foreach i {khjl} {
            set newKeys [split $i {}]
            if {$Prefs($i)} {
                bind . [lindex $newKeys 0] {event generate . <Up>}
                bind . [lindex $newKeys 1] {event generate . <Left>}
                bind . [lindex $newKeys 2] {event generate . <Down>}
                bind . [lindex $newKeys 3] {event generate . <Right>}
            } else {
                foreach j $newKeys {
                    bind . $j {}
                }
            }
        }
        return
    }

    proc reverseInsert {} {
        global Prefs
        if {$Prefs(insertDest) eq "after"} {
            set Prefs(insertDest) "before"
        } else {
            set Prefs(insertDest) "after"
        }
        Preferences::save
        StatusBar::show
        return
    }

    proc save {} {
        global ConfigFile Prefs
        changedColor
        set fp [open $ConfigFile w]
        foreach i [array names Prefs] {
            puts $fp "$i $Prefs($i)"
        }
        close $fp
        return
    }

    proc shortStruct {str} {
        while {[string index $str end] eq [string index $str end-1]} {
            set str [string replace $str end end]
        }
        return $str
    }

    proc show {} {
        pack .f.fPrefs -side left -expand yes -fill both
        return
    }

    proc ShowStructCodes {} {
        global openFile
        tk_messageBox -icon info -type ok -message "List Type Identifiers" \
                -detail [$openFile structHelpMsg]
        return
    }

    proc status {} {
        StatusBar::show "Options"
        return
    }

    proc StructError {} {
        global openFile
        tk_messageBox -icon error -type ok -message "Invalid List Structure" \
            -detail [$openFile structErrorMsg]
        return
    }

    proc StylesFrame {parent} {
        global openFile Prefs

        set result [frame $parent.duo -padx 2 -pady 2]

        ttk::labelframe $result.cfg -text "List File Configuration"
        label $result.cfg.lbl -text "Description Level(s)"
        spinbox $result.cfg.sb -from 0 -to 3 -justify right -width 2 \
                -command {set Prefs([$openFile attName descriptions]) %s}
        grid $result.cfg.lbl -row 0 -column 0 -sticky e
        grid $result.cfg.sb -row 0 -column 1 -sticky w
        if {[$openFile getConstant descriptionsMax] > 0} {
            $result.cfg.sb set $Prefs([$openFile attName descriptions])
        } else {
            $result.cfg.lbl configure -state disabled
            $result.cfg.sb configure -state disabled
        }

        label $result.cfg.id -text "List Type(s)" -anchor e
        entry $result.cfg.code -textvariable Prefs([$openFile attName struct]) \
                -vcmd {$openFile validStruct %P} -validate key \
                -invcmd Preferences::StructError -font "Courier 12"
        button $result.cfg.help -text "Help" -command Preferences::ShowStructCodes
        grid $result.cfg.id -row 1 -column 0 -sticky e
        grid $result.cfg.code -row 1 -column 1 -sticky w -columnspan 2
        grid $result.cfg.help -row 1 -column 3 -sticky w -padx 2
        if {![$openFile getConstant structured]} {
            $result.cfg.id configure -state disabled
            $result.cfg.code configure -state disabled
        }

        label $result.cfg.lbl2 -text "Indentation spaces" -anchor e
        spinbox $result.cfg.sb2 -from [$openFile getConstant indentMin] -to 80 \
                -width 2 -command {set Prefs([$openFile attName indent]) %s} \
                -justify right
        grid $result.cfg.lbl2 -row 2 -column 0 -sticky e
        grid $result.cfg.sb2 -row 2 -column 1 -sticky w
        $result.cfg.sb2 set $Prefs([$openFile attName indent])
        pack $result.cfg -side top -fill x -ipadx 2 -ipady 2

        return $result
    }
}

#----------------------------------------------------------------------------
# Find text widget and result handling
#----------------------------------------------------------------------------
namespace eval Search {
    variable Status false

    proc FindEveryItem {needle parentID} {
        foreach searching [.f.tvList children $parentID] {
            if {[string first $needle [.f.tvList item $searching -text]] != -1} {
                .f.tvList see $searching
                .f.tvList selection add $searching
            }
            FindEveryItem $needle $searching
        }
        return
    }

    proc gui {} {
        frame .search
        label .search.lbl -text "Search for:"
        entry .search.target -textvariable Treeview::buffer(find)
        .search.target icursor end
        pack .search.lbl -side left -padx 3
        pack .search.target -side left -fill x -pady 3
        bind .search.target <Return> {Search::SelectFinds}
        Search::Show
        Search::Hide
        return
    }

    proc Hide {} {
        grid remove .search
        return
    }

    proc SelectFinds {} {
        .f.tvList selection set {}
        if {$Treeview::buffer(find) ne ""} {
            FindEveryItem $Treeview::buffer(find) {}
        }
        StatusBar::show
        return
    }

    proc Show {} {
        grid .search -row 0 -column 0 -sticky ew
        return
    }

    proc toggle {} {
        variable Status
        if {[set Status [expr {!$Status}]]} {
            Search::Show
        } else {
            Search::Hide
        }
        return
    }
}

#----------------------------------------------------------------------------
# Logical grouping of list edits for using selected treeview items.
#----------------------------------------------------------------------------
namespace eval Selection {
    proc allItems {} {
        set allTreeItems [list]
        foreach {index value} [array get Treeview::ItemID] {
            lappend allTreeItems $value
        }
        .f.tvList selection set $allTreeItems
        return
    }

    proc arrange {} {
        global Prefs

        set selected [SingleItem sorting]
        if {$selected eq {}} {
            tk_messageBox -message "Need 1 item selected for arrangement."
        } else {
            set parentID [.f.tvList parent $selected]
            foreach node [.f.tvList children $parentID] {
                set data [.f.tvList item $node -text]
                append data " " [.f.tvList index $node]
                lappend orig $data
            }

            set upward [lsort -increasing -$Prefs(sort) $orig]
            set downward [lreverse $upward]
            if {[ListEqual $orig $upward]} {
                set arranged $downward
                set statusMsg "Siblings arranged in descending order"
            } elseif {([llength $orig] > 2) && [ListEqual $orig $downward]} {
                set arranged [lshuffle $orig]
                set statusMsg "Siblings arranged in random order"
            } else {
                set arranged $upward
                set statusMsg "Siblings arranged in ascending order"
            }

            set oldChildren [.f.tvList children $parentID]
            set limit [llength $orig]
            for {set to 0} {$to < $limit} {incr to} {
                set data [lindex $arranged $to]
                set lastSpace [string last " " $data end]
                set from [expr {int([string range $data $lastSpace end])}]
                set newIndex($to) $from
            }

            for {set i 0} {$i < $limit} {incr i} {
                lappend newChildren [lindex $oldChildren $newIndex($i)]
            }
            .f.tvList children $parentID $newChildren
            ListFileIO::saveFile
            StatusBar::show $statusMsg
        }
        return
    }

    proc boxes {} {
        set selected [.f.tvList selection]
        if {$selected eq {}} {
            tk_messageBox -message "No item selected for removal."
        } else {
            foreach node $selected {
                set oldText [.f.tvList item $node -text]
                set past4 [string range $oldText 4 end]
                switch -exact [string range $oldText 0 3] {
                    "\[x\] " { set newBox $past4 }
                    "\[ \] " { set newBox [string cat "\[x\] " $past4  ] }
                    default  { set newBox [string cat "\[ \] " $oldText] }
                }
                .f.tvList item $node -text $newBox
            }
            .f.tvList tag add modded $selected
            ListFileIO::saveFile
        }
        return
    }

    proc copy {} {
        set selected [.f.tvList selection]
        if {$selected eq {}} {
            StatusBar::show "No item(s) selected for copy to clipboard."
            return false
        } else {
            set result ""
            foreach widgetID $selected {
                if {$result ne ""} {
                    append result "\n"
                }
                append result [.f.tvList item $widgetID -text]
            }
            clipboard clear -displayof .
            clipboard append -displayof . $result
        }
        return true
    }

    proc cut {} {
        if {[copy]} {
            delete
        }
        return
    }

    proc delete {} {
        global stats
        set selected [.f.tvList selection]
        if {$selected eq {}} {
            tk_messageBox -message "No item selected for removal."
        } else {
            set children 0
            foreach node $selected {
                incr children [llength [.f.tvList children $node]]
            }
            if {$children > 0} {
                set response [tk_messageBox -icon question -type yesno \
                        -message "Also delete all child nodes?" \
                        -detail "Select \"Yes\" to remove attachments"]
                if {$response eq "no"} {
                    return
                }
            }

            Deleted::archiveItems $selected
            .f.tvList delete $selected
            foreach i [array names Treeview::ItemID] {
                if {[.f.tvList exists $Treeview::ItemID($i)] == 0} {
                    array unset Treeview::ItemID $i
                    incr stats(removed)
                }
            }
            ListFileIO::saveFile
        }
        return
    }

    proc duplicate {} {
        set selected [SingleItem duplication]
        if {$selected ne {}} {
            set parentID [.f.tvList parent $selected]
            set nextIndex [expr {1 + [.f.tvList index $selected]}]
            set newItem [Treeview::itemCache [.f.tvList insert $parentID $nextIndex \
                    -text [.f.tvList item $selected -text]]]
            DuplicateChildren $selected $newItem
            ListFileIO::saveFile
        }
        return
    }

    proc DuplicateChildren {src dst} {
        set counter 0
        foreach duping [.f.tvList children $src] {
            set newItem [Treeview::itemCache [.f.tvList insert $dst $counter \
                    -text [.f.tvList item $duping -text]]]
            DuplicateChildren $duping $newItem
            incr counter
        }
        return
    }

    proc edit {} {
        set selected [SingleItem edit]
        if {$selected ne {}} {
            Gui::mode "EDIT"
            set Treeview::buffer(edit) [.f.tvList item $selected -text]
            .lfEdit.t replace 1.0 end $Treeview::buffer(edit)
            .f.tvList item $selected -text "<!-- UNDER CONSTRUCTION -->"
            update
            .f.tvList see $selected
            .f.tvList state disabled
            .lfEdit.t mark set insert end
            after idle focus -force .lfEdit.t
        }
        return
    }

    proc insert {{initialText ""}} {
        global Prefs

        if {[.f.tvList selection] eq {}} {
            if {$Prefs(insertDest) eq "before"} {
                NewTreeItem {} 0
            } else {
                NewTreeItem {} end
            }
        } else {
            set selected [SingleItem addition]
            if {$selected ne {}} {
                set newIndex [.f.tvList index $selected]
                if {$Prefs(insertDest) eq "after"} {
                    incr newIndex
                }
                NewTreeItem [.f.tvList parent $selected] $newIndex
            }
        }

        set selected [.f.tvList selection]
        if {[llength $selected] == 1 && $initialText ne ""} {
            ChangeText $initialText
        }
        return
    }

    proc join {} {
        set selected [SingleItem join]
        if {$selected ne {}} {
            set nextItem [.f.tvList next $selected]
            if {$nextItem eq {}} { return }
            set nextChildren [.f.tvList children $nextItem]
            if {[llength $nextChildren] != 0} {
                StatusBar::show "Error: Join item has > 0 children."
                return
            }
            set oldText [.f.tvList item $selected -text]
            set newText [.f.tvList item $nextItem -text]
            .f.tvList item $selected -text [string cat $oldText " " $newText]
            .f.tvList selection set $nextItem
            Selection::delete

            .f.tvList selection set $selected
            .f.tvList tag add modded $selected
            ListFileIO::saveFile
        }
        return
    }

    proc ListEqual {lhs rhs} {
        if {[llength $lhs] != [llength $rhs]} {
            return false
        }

        set lsize [llength $lhs]
        for {set i 0} {$i < $lsize} {incr i} {
            if {[lindex $lhs $i] ne [lindex $rhs $i]} {
                return false
            }
        }

        return true
    }

    proc NewTreeItem {parentIdx positionIdx} {
        set newItem [Treeview::itemCache [.f.tvList insert $parentIdx $positionIdx]]
        .f.tvList selection set $newItem
        edit
        .f.tvList see [.f.tvList selection]
        return
    }

    proc shiftDown {} {
        VerticalMove 1
        event generate .f.tvList <Up>
        return
    }

    proc shiftLeft {} {
        set selected [SingleItem promotion]
        if {$selected ne {}} {
            set selParent [.f.tvList parent $selected]
            if {$selParent ne {}} {
                set nextParent [.f.tvList parent $selParent]
                set nextIndex [expr {1 + [.f.tvList index $selParent]}]
                .f.tvList move $selected $nextParent $nextIndex
                .f.tvList see $selected
                .f.tvList tag add modded $selected
                ListFileIO::saveFile
            }
        }
        return
    }

    proc shiftRight {} {
        set selected [SingleItem demotion]
        if {$selected ne {}} {
            set commonParent [.f.tvList parent $selected]
            set prevIndex [.f.tvList prev $selected]
            while {[.f.tvList parent $prevIndex] ne $commonParent} {
                set prevIndex [.f.tvList prev $prevIndex]
                if {$prevIndex eq {}} {
                    break
                }
            }
            if {$prevIndex ne {}} {
                .f.tvList move $selected $prevIndex end
                .f.tvList see $selected
                .f.tvList tag add modded $selected
                ListFileIO::saveFile
            }
        }
        return
    }

    proc shiftUp {} {
        VerticalMove -1
        event generate .f.tvList <Down>
        return
    }

    proc SingleItem {legend} {
        set selected [.f.tvList selection]
        if {[llength $selected] == 1} {
            return $selected
        } 
        StatusBar::show "Single item selection necessary for $legend"
        return {}
    }

    proc split {} {
        set selected [SingleItem edit]
        if {$selected ne {}} {
            set text1 [.lfEdit.t get 1.0 insert]
            set text2 [.lfEdit.t get insert end]
            ChangeText $text1
            .f.tvList selection set $selected
            .f.tvList tag add modded $selected
            Selection::insert $text2
        }
        return
    }

    proc VerticalMove {offset} {
        set selected [SingleItem movement]
        if {$selected ne {}} {
            set nextIndex [expr {[.f.tvList index $selected] + $offset}]
            set tvSiblings [llength [.f.tvList children [.f.tvList parent $selected]]]
            if {0 <= $nextIndex && $nextIndex < $tvSiblings} {
                .f.tvList move $selected [.f.tvList parent $selected] $nextIndex
                .f.tvList tag add modded $selected
                ListFileIO::saveFile
            }
        }
        return
    }

    proc web {} {
        set selected [SingleItem web]
        if {$selected ne {}} {
            set itemList [regexp -all -inline {\S+} [.f.tvList item $selected -text]]
            foreach prefix {"http://" "https://"} {
                foreach token $itemList {
                    if {[string first $prefix $token] == 0} {
                        exec xdg-open $token &
                        return
                    }
                }
            }
        }
        return
    }
}

#----------------------------------------------------------------------------
# Generate text of status bar located at bottom of GUI.
#----------------------------------------------------------------------------
namespace eval StatusBar {
    variable Changed
    proc RenumberTv {target parentID atCount} {
        variable Changed
        set theCount $atCount
        foreach searching [.f.tvList children $parentID] {
            incr theCount
            if {$target eq $searching} {
                set Treeview::buffer(selection) $theCount
            }
            if {[.f.tvList tag has modded $searching]} {
                incr Changed
            }
            set theCount [RenumberTv $target $searching $theCount]
        }
        return $theCount
    }

    proc show {{statusMsg "default"}} {
        global delayID Prefs stats
        variable Changed
        set Changed 0
        if {$statusMsg eq "default"} {
            set selected [.f.tvList selection]
            if {[llength $selected] == 1} {
                set Treeview::buffer(selection) 0
                RenumberTv $selected {} 0
                set prefix [format "%s" [Suffixed $Treeview::buffer(selection)]]
            } else {
                RenumberTv {} {} 0
                if {$selected eq {}} {
                    set prefix "-"
                } else {
                    set prefix [format "%d" [llength $selected]]
                }
            }

            if {$Treeview::buffer(find) ne ""} {
                set found 0
                set needle $Treeview::buffer(find)
                foreach i $selected {
                    set hay [.f.tvList item $i -text]
                    if {[string first $needle $hay] != -1} {
                        incr found
                    }
                }
                if {($found == 0) || ($found < [llength $selected])} {
                    set Treeview::buffer(find) ""
                }
            }

            set selections "selected: $prefix of [array size Treeview::ItemID]"
            if {$Prefs(insertDest) eq "before"} {
                set ins "INS\u2191"
            } else {
                set ins "INS\u2193"
            }
            set gauges [list $selections $ins]
            lappend gauges [format "\u2212%d +%d \u0394%d" \
                    $stats(removed) $stats(added) $Changed]

            if {$Treeview::buffer(find) ne {}} {
                lappend gauges "find: \"$Treeview::buffer(find)\""
            }

            set statusMsg [join $gauges "      "]
        }

        if {![ListFileIO::writable]} {
            set fileBG red
            set fileText "Read-Only"
        } elseif {[after info] != {}} {
            set fileBG yellow
            set fileText "Modified"
        } else {
            set fileBG green
            if {$Treeview::ItemsCreated == 0} {
                set fileText "Created"
            } elseif {$stats(removed) + $stats(added) + $Changed == 0} {
                set fileText "Loaded"
            } else {
                set fileText "Saved"
            }
        }

        .statusbar.text configure -text [string cat " " $statusMsg]
        .statusbar.file configure -text $fileText -bg $fileBG

        pack .statusbar.badge -side left -ipadx 2
        return
    }

    proc Suffixed {number} {
        set of100 [expr {$number % 100}]
        if {(11 <= $of100) && ($of100 <= 13)} {
            set congruence 0
        } else {
            set congruence [expr {$of100 % 10}]
        }
        #                      0  1  2  3  4  5  6  7  8  9
        return $number[lindex {th st nd rd th th th th th th} $congruence]
    }
}

#----------------------------------------------------------------------------
# Treeview related functions
#----------------------------------------------------------------------------
namespace eval Treeview {
    variable buffer
    variable ItemID
    variable ItemsCreated 0
    variable parentID
    variable previousItem {}
    array set parentID {0 {}}

    proc clear {} {
        variable ItemID
        variable ItemsCreated
        variable buffer
        array unset ItemID
        set ItemsCreated 0
        set buffer(edit) ""
        set buffer(find) ""
        .f.tvList delete [.f.tvList children {}]
        Deleted::clear
        return
    }

    proc focusOn {widgetID} {
        focus .f.tvList
        if {$widgetID ne {}} {
            .f.tvList focus $widgetID
            .f.tvList selection set $widgetID
            .f.tvList see $widgetID
        }
        return
    }

    proc itemCache {treeviewItem} {
        global stats
        variable ItemID
        variable ItemsCreated
        incr stats(added)
        incr ItemsCreated
        return [set ItemID($ItemsCreated) $treeviewItem]
    }

    proc itemEnd {} {
        variable ItemID
        set ids [::tcl::mathfunc::max {*}[array names ItemID]]
        return $ItemID([lindex $ids end])
    }

    proc loadAppend {newText} {
        variable previousItem
        if {$previousItem eq {}} {
            return false
        }
        set TvItemText [.f.tvList item $previousItem -text]
        append TvItemText " " $newText
        .f.tvList item $previousItem -text $TvItemText
        return true
    }

    proc loadInsert {foundLevel listItem} {
        variable parentID
        variable previousItem
        Treeview::itemCache [.f.tvList insert $parentID($foundLevel) end -text $listItem]
        set previousItem [Treeview::itemEnd]
        set parentID([expr {$foundLevel + 1}]) $previousItem
        return
    }
}

#----------------------------------------------------------------------------
# Actions performed when text box is properly closed.
#----------------------------------------------------------------------------
proc ChangeText {newText} {
    set selIndex [.f.tvList selection]
    if {[.lfEdit.t edit modified]} {
        regsub -all {[\r\n]} $newText "" contentNew
        if {$contentNew ne $Treeview::buffer(edit)} {
            .f.tvList tag add modded $selIndex
        }
        .f.tvList item $selIndex -text $contentNew
        ListFileIO::saveFile
    }

    .f.tvList selection toggle $selIndex
    .lfEdit.t replace 1.0 end ""
    .f.tvList state !disabled
    Gui::mode "LIST"
    Treeview::focusOn $selIndex
    StatusBar::show
    return
}

#----------------------------------------------------------------------------
# End of tkadle execution.
#----------------------------------------------------------------------------
proc Endtkadle {} {
    global Prefs
    if {$Prefs(geometry) ne [winfo geometry .]} {
        set Prefs(geometry) [winfo geometry .]
        Preferences::save
    }
    ListFileIO::saveFile 0
    destroy .
}

#----------------------------------------------------------------------------
# Generic greatest common denominator proc.
#----------------------------------------------------------------------------
proc gcd {u v} {
    if {$u} {
        return [gcd [expr $v % $u] $u]
    }
    return $v
}

#----------------------------------------------------------------------------
# Returns a random shuffle of a list.
#----------------------------------------------------------------------------
proc lshuffle theList {
    for {set limit [llength $theList]} {$limit > 0} {} {
        set index [expr {int($limit * rand())}]
        set chosenValue [lindex $theList $index]
        incr limit -1
        lset theList $index [lindex $theList $limit]
        lset theList $limit $chosenValue
    }
    return $theList
}

#----------------------------------------------------------------------------
# Paste lines from clipboard as separate items.
#----------------------------------------------------------------------------
proc PasteItems {} {
    set selected [.f.tvList selection]
    set selects [llength $selected]
    set clipped [split [string trimright [clipboard get]] \n]
    set clips [llength $clipped]
    if {$selects == 0} {
        foreach i $clipped {
            set newItem [Treeview::itemCache [.f.tvList insert {} end]]
            .f.tvList item $newItem -text $i
        }
        set message "$clips item(s) added to end of list."
    } elseif {$selects == 1} {
        if {$clips == 0} {
            StatusBar::show "Clipboard is empty. No paste occurred."
            return
        } elseif {$clips == 1} {
            .f.tvList item $selected -text [lindex $clipped 0]
            set message "One item replaced with clipboard content."
        } else {
            set parent [lindex $selected 0]
            foreach i $clipped {
                set newItem [Treeview::itemCache [.f.tvList insert $parent end]]
                .f.tvList item $newItem -text $i
            }
            .f.tvList item $parent -open true
            .f.tvList see $parent
            set message "$clips items added beneath selected item"
        }
    } else {
        StatusBar::show "Too many selections for paste destination."
        return
    }
    ListFileIO::saveFile
    StatusBar::show $message
    return
}

#----------------------------------------------------------------------------
# Deliver, if possible, a web browser preview of the session list.
#----------------------------------------------------------------------------
proc PreviewList {} {
    global openFile
    set targetFile [file rootname $ListFileIO::filename].html
    ListFileIO::saveFile 0
    $openFile html $targetFile
    exec xdg-open $targetFile
    return
}

#----------------------------------------------------------------------------
# String of information to be included in export files when possible.
#----------------------------------------------------------------------------
proc SaveComment {comment} {
    global tcl_platform
    append comment " tkadle"
    foreach i {tclversion hostname} {
        append comment " $i:[info $i]"
    }
    foreach i {os user} {
        append comment " $i:$tcl_platform($i)"
    }
    append comment [clock format [clock seconds] -format " %Y-%m-%dT%H:%M:%S"]
    return $comment
}

#----------------------------------------------------------------------------
# Cosmetic handling of scrollbar appearance.
#----------------------------------------------------------------------------
proc ScrollbarOnDemand {sb lo hi} {
    if {$lo <= 0.0 && 1.0 <= $hi} {
        pack forget .f.sList
    } else {
        pack .f.sList -side right -fill y
    }
    $sb set $lo $hi
    return
}

#----------------------------------------------------------------------------
# Use color chooser to assign a color variable.
#----------------------------------------------------------------------------
proc SetHighlightColor {widgt colorVar} {
    global Prefs
    set color [tk_chooseColor -title "Choose highlight color" \
            -initialcolor $Prefs($colorVar)]
    if {$color ne ""} {
        set Prefs($colorVar) $color
        $widgt configure -background $color
    }
    return
}

#----------------------------------------------------------------------------
# Set tkadle window title using opened file name, if available.
#----------------------------------------------------------------------------
proc SetWindowTitle {dirty} {
    global savedIcon openedIcon changedIcon updatedList

    set fname [ListFileIO::nameLocation]
    if {$fname ne ""} {
        append fname " - "
    }

    if {$dirty} {
        set updatedList true
        wm iconphoto . -default [$changedIcon getSize 32]
        wm title . [string cat "*" $fname "tkadle"]
    } elseif {$updatedList} {
        wm iconphoto . -default [$savedIcon getSize 32]
        wm title . [string cat $fname "tkadle"]
    } else {
        wm iconphoto . -default [$openedIcon getSize 32]
        wm title . [string cat $fname "tkadle"]
    }
    return
}

#----------------------------------------------------------------------------
# Experimental handling of <Tab> in the list gui.
#----------------------------------------------------------------------------
proc TabDown {} {
    event generate . <Down>
    return
}

#----------------------------------------------------------------------------
# Reset session statistics in the manner of a trip odometer.
#----------------------------------------------------------------------------
proc zeroize {{show true}} {
    global stats
    set stats(added) 0
    set stats(removed) 0
    if {$show} {
        .f.tvList tag remove modded
        StatusBar::show
    }
    return
}

#----------------------------------------------------------------------------
# Commands
#----------------------------------------------------------------------------

package require Tk
set ConfigFile "~/.tkadle"
set delayID None
zeroize false

switch -exact -- $paradigm {
    "ASCIIDOC" {set openFile [  ADocFormat new]}
    "LINELIST" {set openFile [LineItemList new]}
    "MARKDOWN" {set openFile [    MDFormat new]}
    default    {
        puts "Usage: tkadle.tcl \[-syn asciidoc|adoc|markdown|md\] filename"
        exit
    }
}
Preferences::load

frame .f

Search::gui

ttk::treeview .f.tvList -show tree -yscrollcommand [list ScrollbarOnDemand .f.sList]
scrollbar .f.sList -command {.f.tvList yview}
pack .f.tvList -side left -expand yes -fill both
grid .f -row 1 -column 0 -sticky nsew

Deleted::gui
Export::gui
Preferences::gui
Preferences::changedColor

labelframe .lfEdit -text "Selection Editor" -padx 3 -pady 1
text .lfEdit.t -height 3 -yscrollcommand {.lfEdit.sEdit set} -wrap word
scrollbar .lfEdit.sEdit -command {.lfEdit.t yview}
pack .lfEdit.t -side left -fill x -expand yes
pack .lfEdit.sEdit -side right -fill y
bind .lfEdit.t <F1> help::dialog
bind .lfEdit.t <Return> {ChangeText [.lfEdit.t get 1.0 end]}

frame .statusbar
label .statusbar.badge -relief sunken -borderwidth 2 -font {{Times Roman} 0 bold}
label .statusbar.text -relief sunken -borderwidth 2 -anchor w
label .statusbar.file -relief sunken -borderwidth 2 -anchor e -text "Loaded"
pack .statusbar.badge -side left -ipadx 2
pack .statusbar.file -side right
pack .statusbar.text -side right -expand yes -fill both
grid .statusbar -row 3 -column 0 -sticky ew

grid rowconfigure . 1 -weight 1
grid columnconfigure . 0 -weight 1

# Abandoned attempt to change appearance of gui:
#ttk::setTheme "clam"
#set suffix [winfo class .f.tvList]
#ttk::style configure styleCustom.$suffix -background black
#ttk::style configure styleCustom.$suffix -foreground green
#.f.tvList configure -style "styleCustom.$suffix"

set Gui::current "LIST"
Treeview::clear
$openFile labelLogo .statusbar.badge

set priorGeo [split $Prefs(geometry) +x]
set offsetY [expr {[lindex $priorGeo end] - $Prefs(guiOffsetY)}]
if {$offsetY < 0} {
    set offsetY 0
}
set newGeo [join [lrange $priorGeo 0 1] "x"]
set newGeo [join [list $newGeo [lindex $priorGeo 2] $offsetY] "+"]
wm geometry . $newGeo
ListFileIO::loadFile [file normalize $commandFile]

set IconHexMap {
        00000000000000000000000000000000
        00111111111111111111111111111100
        01111111111111111111111111111110
        011FFFFFF11111111111111FFFFFF110
        011FFFFFF11111111111111FFFFFF110
        011FF1111111111111111111111FF110
        011FF1111111111111111111111FF110
        011FF1111111111111111111111FF110
        011FF1148884111111114888411FF110
        011FF118EEE8111111118EEE811FF110
        011FF1118EEE81111118EEE8111FF110
        011FF11118EEE811118EEE81111FF110
        011FF111118EEE8118EEE811111FF110
        011FF1111118EEE88EEE8111111FF110
        011FF111111118EEEE811111111FF110
        011FF111111114EEEE411111111FF110
        011FF111111114EEEE411111111FF110
        011FF111111118EEEE811111111FF110
        011FF1111118EEE88EEE8111111FF110
        011FF111118EEE8118EEE811111FF110
        011FF11118EEE811118EEE81111FF110
        011FF1118EEE81111118EEE8111FF110
        011FF118EEE8111111118EEE811FF110
        011FF1148884111111114888411FF110
        011FF1111111111111111111111FF110
        011FF1111111111111111111111FF110
        011FF1111111111111111111111FF110
        011FFFFFF11111111111111FFFFFF110
        011FFFFFF11111111111111FFFFFF110
        01111111111111111111111111111110
        00111111111111111111111111111100
        00000000000000000000000000000000
}
if {[ListFileIO::writable]} {
    set savedIcon [HexadecIcon new $IconHexMap G]
    set openedIcon [HexadecIcon new $IconHexMap G]
} else {
    set savedIcon [HexadecIcon new $IconHexMap R]
    set openedIcon [HexadecIcon new $IconHexMap R]
}
$openedIcon map "E" "F" "2"
$openedIcon map "1" "1" "F"
set changedIcon [HexadecIcon new $IconHexMap RG]
$changedIcon map "E" "E" "1"
set updatedList false
SetWindowTitle false

#------------------------------------------------------------------------
# Events & binds...virtual event help topics pending implementation clue
#------------------------------------------------------------------------
event add <<SelectAll>> <Control-a>

Help::bindTip . <F1>         Help::dialog "Show help dialog"
Help::bindTip . <Escape>     Gui::escapeKey "Cancel an edit or selection"
Help::bindTip . <Home>       {Gui::nonEditMode Navigation::homeKey} "Select first list item"
Help::bindTip . <End>        {Gui::nonEditMode Navigation::endKey} "Select last list item"
Help::bindTip . +            {Gui::nonEditMode Folding::levelOpen} "Expand one complete level of nodes"
Help::bindTip . -            {Gui::nonEditMode Folding::levelClose} "Close one complete level of nodes"
Help::bindTip . <Control-e>  {Gui::nonEditMode Gui::toggleExport} "Show/Hide export options"
Help::bindTip . <Control-p>  {Gui::nonEditMode PreviewList} "Preview list in web browser"
Help::bindTip . <Control-q>  {Gui::nonEditMode Endtkadle} "Exit tkadle"
Help::bindTip . <Control-d>  {Gui::nonEditMode Gui::toggleRemoved} "Show/Hide list of deleted items"
Help::bindTip . <Control-i>  {Gui::nonEditMode Preferences::reverseInsert} "Reverse insert direction"
Help::bindTip . <Control-o>  {Gui::nonEditMode Gui::toggleOptions} "Show/Hide tkadle options"
bind . <Up>                  {Gui::nonEditMode Navigation::selectPrev}
bind . <Down>                {Gui::nonEditMode Navigation::selectNext}
bind . <Tab>                 {Gui::nonEditMode TabDown}
Help::bindTip .f.lbDEL <Return> Deleted::returnItem "Return archived item to list"

Help::bindTip . <Control-f>               {Gui::listMode Search::toggle} "Show/Hide search entry area"
Help::bindTip . <Insert>                  {Gui::listMode Selection::insert} "Insert new item after selected list item"
Help::bindTip .f.tvList <Return>          {Gui::listMode Selection::edit} "Edit selected list item"
Help::bindTip .f.tvList <Delete>          {Gui::listMode Selection::delete} "Remove selected item"
Help::bindTip .f.tvList a                 {Gui::listMode Selection::arrange} "Arrangements sort cycle: upward, downward, random"
Help::bindTip .f.tvList b                 {Gui::listMode Selection::boxes} "Box options cycle: none, empty, checked"
Help::bindTip .f.tvList z                 {Gui::listMode zeroize} "Reset session statistics to zero"
Help::bindTip .f.tvList =                 {Gui::listMode Selection::duplicate} "Duplicate selected item"
Help::bindTip .f.tvList <Control-w>       {Gui::listMode Selection::web} "Web (Internet) access http address"
Help::bindTip .f.tvList <Control-greater> {Gui::listMode Selection::join} "Join selected item with next item"
Help::bindTip .lfEdit.t <Control-less>    {Selection::split} "Split edit item cursor to 2 items"

Help::bindTip .f.tvList <Shift-Up>        {Gui::listMode Selection::shiftUp} "Move selected item up one position"
Help::bindTip .f.tvList <Shift-Down>      {Gui::listMode Selection::shiftDown} "Move selected item down one position"
Help::bindTip .f.tvList <Shift-Left>      {Gui::listMode Selection::shiftLeft} "Promote selected item"
Help::bindTip .f.tvList <Shift-Right>     {Gui::listMode Selection::shiftRight} "Demote selected item"

bind . <<Copy>>                           {Gui::listMode Selection::copy}
bind . <<Cut>>                            {Gui::listMode Selection::cut}
bind . <<SelectAll>>                      {Gui::listMode Selection::allItems}
bind . <<Paste>>                          {Gui::listMode PasteItems}

bind .f.tvList <<TreeviewSelect>> StatusBar::show
bind .f.lbDEL <<ListboxSelect>>   Deleted::status

bind .statusbar.badge <Enter> { StatusBar::show "Interpreting Syntax: [$openFile getConstant legend]"}
bind .statusbar.badge <Leave> { StatusBar::show }

############################################################################
#  hotkeys: abcdefghijklmnopqrstuvwxyz
# assigned: abCCCC vCvvv  CCCC   CCC z
#    Types: (a)rrange, (b)ox, (v)i navigation, (C)ontrol-? (z)eroize
############################################################################
