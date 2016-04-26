$( Propositional logic $)

$c ( $.
$c ) $.
$c -> $.
$c -. $.
$c wff $.
$c |- $.

$v ph ps ch th ta $.
wph $f wff ph $.
wps $f wff ps $.
wch $f wff ch $.
wth $f wff th $.
wta $f wff ta $.
$( well-formedness $)
wn $a wff -. ph $.	
wi $a wff ( ph -> ps ) $.


$( axiom of simplification $)
${
	ax-1 $a |- ( ph -> ( ps -> ph ) ) $.
$}

$( Frege's axiom $)
${
	ax-2 $a 
	|- ( ( ph -> ( ps -> ch ) ) -> ( ( ph -> ps ) -> ( ph -> ch ) ) ) 
	$.
$}

$( axiom of transposition $)
${
	ax-3 $a ( -. ph -> -. ps ) -> ( ps -> ph ) $.
$}

$( modus ponens $) 
${
	min $e |- ph $.
	maj $e |- ( ph -> ps ) $.
	ax-mp $a |- ps $.
$}

$( logical implication $)

$( double modus ponens $) 
${ 
	mp2b.1 $e |- ph $.
	mp2b.2 $e |- ( ph -> ps ) $.
	mp2b.3 $e |- ( ps -> ch ) $.
	mp2b $p |- ch $=
	wps wch wph wps mp2b.1 mp2b.2 ax-mp mp2b.3 ax-mp $.
$}

${ $( adding an antecedent $)
	a1i.1 $e |- ph $.
	a1i $p |- ( ps -> ph ) $=
	wph
	wps wph wi
	a1i.1
	wph wps ax-1
	ax-mp
	$.
$}

${ $( modus ponens and antecedent addition $)
	mp1i.1 $e |- ph $.
	mp1i.2 $e |- ( ph -> ps ) $.
	mp1i $p |- ( ch -> ps ) $=
	wps
	wch
	$( |- ps $)
	wph wps mp1i.1 mp1i.2 ax-mp
	a1i
	$.
$}

${ $( distribution $)
	a2i.1 $e |- ( ph -> ( ps -> ch ) ) $.
	a2i $p |- ( ( ph -> ps ) -> ( ph -> ch ) ) $=
	wph wps wch wi wi
	wph wps wi wph wch wi wi
	a2i.1
	wph wps wch ax-2
	ax-mp
	$.
$}

${ $( common antecedent addition in implication $)
	imim2i.1 $e |- ( ph -> ps ) $.
	imim2i $p |- ( ( ch -> ph ) -> ( ch -> ps ) ) $=
	wch wph wps
	wph wps wi wch imim2i.1 a1i
	a2i
	$.
$}

${ $( modus ponens deduction $)
	mdp.1 $e |- ( ph -> ps ) $.
	mdp.2 $e |- ( ph -> ( ps -> ch ) ) $.
	mdp $p |- ( ph -> ch ) $=
	wph wps wi
	wph wch wi
	mdp.1
	wph wps wch mdp.2 a2i
	ax-mp
	$.
$}

${ $( The principle of syllogism $)
	syl.1 $e |- ( ph -> ps ) $.
	syl.2 $e |- ( ps -> ch ) $.
	syl $p |- ( ph -> ch ) $=
	wph wps wch
	syl.1

	wps wch wi
	wph
	syl.2
	a1i

	mdp
	$.
$}

${ $( nested modus ponens $)
	mpi.1 $e |- ps $.
	mpi.2 $e |- ( ph -> ( ps -> ch ) ) $.
	mpi $p |- ( ph -> ch ) $=
	wph wps wi
	wph wch wi

	wps wph
	mpi.1
	a1i $( |- ( ph -> ps ) $)

	wph wps wch
	mpi.2
	a2i $( |- ( ( ph -> ps ) -> ( ps -> ch ) ) $)

	ax-mp
	$.
$}

${ $( double modus ponens $)
	mp2.1 $e |- ph $.
	mp2.2 $e |- ps $.
	mp2.3 $e |- ( ph -> ( ps -> ch ) ) $.
	mp2 $p |- ch $=
	wph wch
	mp2.1

	wph wps wch
	mp2.2
	mp2.3
	mpi $( |- ( ph -> ch ) $)
	ax-mp
	$.
$}

${ $( double syllogism inference $)
	3syl.1 $e |- ( ph -> ps ) $.
	3syl.2 $e |- ( ps -> ch ) $.
	3syl.3 $e |- ( ch -> th ) $.
	3syl $p |- ( ph -> th ) $=
	wph wch wth
	wph wps wch 3syl.1 3syl.2 syl 
	3syl.3 syl $.
$}

${ $( Principle of identity $)
	id $p |- ( ph -> ph ) $=
	wph wps wph wi wi
	wph wph wi

	wph wps
	ax-1 $( |- ( ph -> ( ps -> ph ) $)

	wph wps wph wi wph wi wi
	wph wps wph wi wi wph wph wi wi

	wph
	wps wph wi
	ax-1 $( |- ( ph -> ( ( ps -> ph ) -> ph ) ) $)

	wph
	wps wph wi
	wph
	ax-2 $( |- ( ph -> ( ( ps -> ph ) -> ph ) ) -> 
			( ( ph -> ( ps -> ph ) ) -> ( ph -> ph ) ) $)
	ax-mp $( |- ( ( ph -> ( ps -> ph ) ) -> ( ph -> ph ) ) $)
	ax-mp $.
$}

${ $( principle of identity with antecedent $)
	idd $p |- ( ph -> ( ps -> ps ) ) $=
	wps wps wi
	wph
	wps
	id
	a1i
	$.
$}

${ $( deduction introducing an embedded antecedent $)
	a1d.1 $e |- ( ph -> ps ) $.
	a1d $p |- ( ph -> ( ch -> ps ) ) $=
	wph wps wch wps wi
	a1d.1
	wps wch ax-1 $( |- ( ps -> ( ch -> ps ) ) $)
	syl
	$.
$}

${ $( embedded antecedent distribution $)
	a2d.1 $e |- ( ph -> ( ps -> ( ch -> th ) ) ) $.
	a2d $p |- ( ph -> ( ( ps -> ch ) -> ( ps -> th ) ) ) $=
	wph wps wch wth wi wi wps wch wi wps wth wi wi 
	a2d.1
	wps wch wth
	ax-2
	syl
	$.
$}

${ $( add two antecedents $)
	a1ii.1 $e |- ch $.
	a1ii.2 $p |- ( ph -> ( ps -> ch ) ) $=
	wps wch wi
	wph
	wch wps a1ii.1 a1i 
	a1i $.
$}

${ $( syllogism inference with commutation of antecedents $)
	sylcom.1 $e |- ( ph -> ( ps -> ch ) ) $.
	sylcom.2 $e |- ( ps -> ( ch -> th ) ) $.
	sylcom $p |- ( ph -> ( ps -> th ) ) $=
	wph wps wch wi wps wth wi
	sylcom.1
	wps wch wth
	sylcom.2
	a2i
	syl $.
$}

${ $( syllogism inference with commuted antecedents $)
	syl5com.1 $e |- ( ph -> ps ) $.
	syl5com.2 $e |- ( ch -> ( ps -> th ) ) $.
	syl5com $p |- ( ph -> ( ch -> th ) ) $=
	wph wch wps wi wch wth wi
	wph wps wch
	syl5com.1
	a1d $( |- ( ph -> ( ch -> ps ) ) $)
	wch wps wth
	syl5com.2
	a2i
	syl
$.	
$}

${ $( antecedents swap (commutation) in implication $)
	com12.1 $e |- ( ph -> ( ps -> ch ) ) $.
	com12 $p |- ( ps -> ( ph -> ch ) ) $=
	wps wph wps wi wph wch wi
	wps wph
	ax-1 $( |- ( ps -> ( ph -> ps ) ) $)
	wph wps wch
	com12.1
	a2i $( |- ( ( ph -> ps ) -> ( ph -> ch ) ) $)
	syl
$.
$}

${ $( a syllogism rule of inference (embedded weakening) $)
	syl5.1 $e |- ( ph -> ps ) $.
	syl5.2 $e |- ( ch -> ( ps -> th ) ) $.
	syl5 $p |- ( ch -> ( ph -> th ) ) $=
	wph wch wth
	wph wps wch wth
	syl5.1 syl5.2 syl5com $( |- ( ph -> ( ch -> th ) ) $)
	com12 $.
$}

${ $( a syllogism rule of inference (strengthening) $)
	syl6.1 $e |- ( ph -> ( ps -> ch ) ) $.
	syl6.2 $e |- ( ch -> th ) $.
	syl6 $p |- ( ph -> ( ps -> th ) ) $=
	wph wps wch wi wps wth wi
	syl6.1
	wps wch wth
	wch wth wi wps
	syl6.2 a1i $( |- ( ps -> ( ch -> th ) ) $)
	a2i $( |- ( ( ps -> ch ) -> ( ps -> th ) ) $)
	syl $.
$}

${ $( combine syl5 and syl6 $)
	syl56.1 $e |- ( ph -> ps ) $.
	syl56.2 $e |- ( ch -> ( ps -> th ) ) $.
	syl56.3 $e |- ( th -> ta ) $.
	syl56 $p |- ( ch -> ( ph -> ta ) ) $=
	wch wph wth wta
	wph wps wch wth
	syl56.1 syl56.2
	syl5 $( |- ( ch -> ( ph -> th ) ) $)
	syl56.3
	syl6 $.
$}

${ $( syllogism inference with commuted antecedents $)
	syl6com.1 $e |- ( ph -> ( ps -> ch ) ) $.
	syl6com.2 $e |- ( ch -> th ) $.
	syl6com $p |- ( ps -> ( ph -> th ) ) $=
	wph wps wth
	wph wps wch wth
	syl6com.1 syl6com.2
	syl6 $( ( ph -> ( ps -> th ) ) $)
	com12 $.
$}

${ $( modus ponens inference with commutation of antecedents $)
	mpcom.1 $e |- ( ps -> ph ) $.
	mpcom.2 $e |- ( ph -> ( ps -> ch ) ) $.
	mpcom $p |- ( ps -> ch ) $=
	wps wph wi wps wch wi
	mpcom.1
	wps wph wch
	wph wps wch
	mpcom.2 com12 $( |- ( ps -> ( ph -> ch ) ) $)
	a2i $( |- ( ( ps -> ph ) -> ( ps -> ch ) ) $)
	ax-mp $.
$}

${ $( syllogism inference with common nested antecedent $)
	syli.1 $e |- ( ps -> ( ph -> ch ) ) $.
	syli.2 $e |- ( ch -> ( ph -> th ) ) $.
	syli $p |- ( ps -> ( ph -> th ) ) $=
	wps wph wch wth
	syli.1
	wch wph wth
	syli.2
	com12 $( |- ( ph -> ( ch -> th ) ) $)
	sylcom $.
$}

${ $( double antecedent replacement $)
	syl2im.1 $e |- ( ph -> ps ) $.
	syl2im.2 $e |- ( ch -> th ) $.
	syl2im.3 $e |- ( ps -> ( th -> ta ) ) $.
	syl2im $p |- ( ph -> ( ch -> ta ) ) $=
	wph wps wch wta wi
	syl2im.1
	wch wth wps wta
	syl2im.2
	syl2im.3
	syl5 $( |- ( ps -> ( ch -> ta ) ) $)
	syl $.
$}

${ $( assertion - like a closed form of modus ponens $)
	pm2.27 $p |- ( ph -> ( ( ph -> ps ) -> ps ) ) $=
	wph wps wi wph wps
	wph wps wi
	id $( ( ( ph -> ps ) -> ( ph -> ps ) ) $)
	com12 $.
$}

${ $( nested modus ponens deduction $)
	mpdd.1 $e |- ( ph -> ( ps -> ch ) ) $.
	mpdd.2 $e |- ( ph -> ( ps -> ( ch -> th ) ) ) $.
	mpdd $p |- ( ph -> ( ps -> th ) ) $=
	wph wps wch wi wps wth wi
	mpdd.1
	wph wps wch wth
	mpdd.2 a2d $( |- ( ph -> ( ( ps -> ch ) -> ( ps -> th ) ) ) $)
	mdp  $.
$}

${ $( nested modus ponens deduction $)
	mpid.1 $e |- ( ph -> ch ) $.
	mpid.2 $e |- ( ph -> ( ps -> ( ch -> th ) ) ) $.
	mpid $p |- ( ph -> ( ps -> th ) ) $=
	wph wps wch wth
	wph wch wps
	mpid.1 a1d $( ph -> ( ps -> ch ) $)
	mpid.2 mpdd $.
$}	

${ $( nested modus ponens deduction $)
	mpdi.1 $e |- ( ps -> ch ) $.
	mpdi.2 $e |- ( ph -> ( ps -> ( ch -> th ) ) ) $.
	mpdi $p |- ( ph -> ( ps -> th ) ) $=
	wph wps wch wth
	wps wch wi wph
	mpdi.1 a1i $( |- ph -> ( ps -> ch ) $)
	mpdi.2
	mpdd $.
$}

${ $( doubly nested modus ponens inference $)
	mpii.1 $e |- ch $.
	mpii.2 $e |- ( ph -> ( ps -> ( ch -> th ) ) ) $.
	mpii $p |- ( ph -> ( ps -> th ) ) $=
	wph wps wch wth
	wch wps
	mpii.1 a1i $( ( ps -> ch ) $)
	mpii.2
	mpdi $.
$}

${ $( syllogism deduction $)
	syld.1 $e |- ( ph -> ( ps -> ch ) ) $.
	syld.2 $e |- ( ph -> ( ch -> th ) ) $.
	syld $p |- ( ph -> ( ps -> th ) ) $=
	wph wps wch wth
	syld.1
	wph wch wth wi wps
	syld.2
	a1d $( |- ( ph -> ( ps -> ( ch -> th ) ) ) $)
	mpdd $.
$}

${ $( double modus ponens deduction $)
	mp2d.1 $e |- ( ph -> ps ) $.
	mp2d.2 $e |- ( ph -> ch ) $.
	mp2d.3 $e |- ( ph -> ( ps -> ( ch -> th ) ) ) $.
	mp2d $p |- ( ph -> th ) $=
	wph wps wth
	mp2d.1
	wph wps wch wth
	wph wch wps
	mp2d.2 a1d $( |- ( ph -> ( ps -> ch ) ) $)
	mp2d.3 mpdd $( ( ph -> ( ps -> th ) ) $)
	mdp $.
$}



