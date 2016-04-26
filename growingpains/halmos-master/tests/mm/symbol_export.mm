$( test that symbols are exported (to symbol_import.mm) $)
$( Halmos deviates from the metamath specification in that $[ filename $] $)
$( is not a textual substitution. Including a file with $[ $] causes only $)
$( the global symbols to be included (so $v, $f, $d, and $e are ignored). $)
$c |- num 0 S $.
$v x $.
a.num.0 $a num 0 $.
num.x $f num x $.
a.num.succ $a num S x $.
thm.one $p num S 0 $= a.num.0 a.num.succ $.
thm.succ2 $p num S S x $= num.x a.num.succ a.num.succ $.
