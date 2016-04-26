$(
	Propositional Logic
		6 Nov 2015

	Propositional variables Prop
	Well-formed formulas wff
$)

$( Propositional variables $)
$c Prop $.

$( Well-formed formulas $)
$c wff $.

$( Valuations $)
$c Val $.

$( 
	syntax
$)

$c ( ) -> ! $.

$( if P is a proposition, then P is well-formed. $)
${
	$v P $.
	Prop.P $f Prop P $.
	wff.P $a wff P $.
$}

${
	$v P Q $.
	wff.P $f wff P $.
	wff.Q $f wff Q $.
	wff.T $a wff T $.
	wff.F $a wff F $.
	wff.not $a wff ! ( P ) $.
	wff.imp $a wff ( P -> Q ) $.
$}

$( axioms $)
${
	$v P Q R $.
	wff.P $f wff P $.
	wff.Q $f wff Q $.
	wff.R $f wff R $.
	ax.lukasiewicz1 $a
	|- ( P -> ( Q -> R ) ) $.
	ax.lukasiewicz2 $a 
	|- ( ( P -> ( Q -> R ) ) -> ( ( P -> Q ) -> ( P -> R ) ) ) $.
	ax.lukasiewicz3 $a
	|- ( ( ! ( P ) -> ! ( Q ) ) -> ( Q -> P ) ) $.
$}

$( inference $)
${
	$v P Q $.
	wff.P $f wff P $.
	wff.Q $f wff Q $.
	$( modus ponens $)
	${
		maj $e |- ( P -> Q ) $.
		min $e |- P $.
		ax.mp $a |- Q $.
	$}
$}


