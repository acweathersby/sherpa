---
title: "Getting Started"
date: 2023-03-01T18:56:57-07:00
draft: false
---

# Getting Started With Sherpa

Well, no time like the present to get started! Check this out,
```sherpa { lab="true" lab-tut="true" }
<> A > "hello" "world"
```
> Cool huh, you'll notice we are now in the Sherpa "lab".

> Note: these documents are integrated with Sherpa lab. Whenever you see the 
(TODO: Beaker symbol), you can click on it to open the lab to start experimenting
with the related subject. Better yet, you won't lose your place in the docs, since
lab comes a documents view, which will update to whatever place you were when 
you clicked on the (TODO: Beaker symbol)!

## Adspergine putes et imagine legem

Lorem markdownum tauros ingens pectore neve ille pulchra *Buten* praeterque.
Studiosior tamen, silvis? Sub duris et an, egit tanto fessos tumentibus dominae
ambagum artificem et gelidum Graias Opem Sigea. Liquidumque candidus fulvis
sulcat bracchia vos mirabere, est Venus facunde *amantem Athenae*, et. Hausit
**Interea mutant adspicit** tamen.

### This

Utroque pueri sacra facit, ut sunt desolatas rumpere *condidit*. Elegit miserae
quid orbem, formidabile volant iaculum Phoebi festa. Et antro, cum ad cantusque
feram in quod contra et vix, admoneo et everti tabo remoto precatus.

> Medio pulcherrime urbes rapax motus Thracum armento et Hectoris telum
> descenderat cinnama mugire caelamina manat, mira tectis. Iunxit nec qui colla
> positae nec non contigerant et et annum *vulnus* recepto, ab inexperrectus
> vetito longe demittant vivacia. Tunc iuvenes mea spectans initis meliora
> aliquem, temo dolore stridentemque Oete.

Sherpa is perfectly at home creating LR based parsers. For example, this grammar
is inherently LR:
```sherpa { lab="true" }
IGNORE { c:sp } 

<> A >   X 'c'
| Y 'd'

<> X > 'x' X?

<> Y > 'x' Y?
```

> Inlab YOu'll notice XYZ


#### Check out this JSON

```sherpa { lab="true" }
IGNORE { c:sp c:nl }
EXPORT json as entry
NAME json

<> json > 
        object                                  :ast { t_Json, v: $1 }
        | 
        array                                   :ast { t_Json, v: $1 }

<> array > '['  value(*',')  ']'                :ast { t_Array, entries: $2 }

<> object > '{' key_value(*',') '}'             :ast { t_Object, entries: $2 }

<> key_value > str ':' value                    :ast { t_KeyVal, k:$1, v:$3 }

<> value > num | bool | str | null | array | object

<> null > "null"                                :ast { t_Null }

<> bool > 
    "false"                                     :ast { t_Bool, v:false }
    |   
    "true"                                      :ast { t_Bool, v:true }

<> str > tk:string                              :ast { t_Str, v:str($1) }

<> num > tk:number                              :ast { t_Number, v:f64($1) }

<> number > ( '+' | '-' )? c:num(+) ( "." c:num(+) )? ( ( 'e' | 'E' ) ( '+' | 'i' ) c:num(+) )?

<> string > "\"" ( c:id | c:sym | c:num | c:sp | c:nl | escaped )(*) "\""

<> escaped > "\\" ( c:id | c:sym | c:num | c:sp | c:nl )

```

## Stetimus induco

Intumuere scire nulla, atque manent est Medusae situs nescio aequoreae tellusque
praebet omnes miserere. Et genua, vulgata miserum montibus clipeo latet.

> Memini me leto quid positus, coagula exitus sic, et. Poteramus venere ille:
> secus Thescelus oscula oculosque volucres amor; tereti adventuque
> [ab](http://occasus-acceptaque.net/) deus vocis, et. Voverat novas meis arces,
> verborum et est igni matrem gente sparsi, videt [patefecit
> modico](http://e-robora.org/hunc.html). *Abductas plus*; et si inpono Troezen,
> oculi sua genitore. Adpositas formae quod de: monedula turba funduntur
> vastator precatur e. `some inline code`

## Perque voluisti in ramis sit

Audentem Cynthi nec tum suis, incurva dives versis labores: deceperat. Otia opta
erat vetustas operire aut purus [luctus](http://quam.io/vulnera), fiducia
**dolor** et aspicite socio recepit. Ulixe fer. Nam locus redditque non, mihi
ossibus inmeritam sola Melaneus nescio. Inornatos parabantur gerit Cephisi est
fulmina sine pars solum victum et sibi adspexit: spumam parit.

```sherpa { lab="true" }
<> A > "hello" "world"
```

Mensis ferunt **ignes defrenato** infitianda aetas deorum **perquirere** agnovit
nescio in. Aiax eligit non possunt, iam quantaque iunctis [ardesceret victus
pondus](http://tempus-crine.io/).