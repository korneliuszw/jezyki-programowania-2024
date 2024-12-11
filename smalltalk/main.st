"TROJKAT ROWNORAMIENNY (5+1+9)%5=0 I WYŚRODKUJ (6+7+11)%2=0"
Object subclass: #Wielokat
	instanceVariableNames: 'wierzcholki nazwa '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'JezykiProgramowania'!

!Wielokat methodsFor: 'initialize-release'!

initialize: liczbaWierzcholkow name: nowaNazwa
	"konstruktor obiektu - wielokata"

	nazwa:=nowaNazwa.
	wierzcholki:=Array new: liczbaWierzcholkow.
	wierzcholki at: 1 put: 0@0.! !

!Wielokat methodsFor: 'accessing'!

nazwa
	"podaje nazwe wielokata"

	^nazwa!
	
wierzcholki
    "Zwraca tablicę wierzchołków"
    
    ^wierzcholki!
    
nazwa: nowa_nazwa
	"ustawia nowa nazwe wielokata"

	nazwa:=nowa_nazwa! !

!Wielokat methodsFor: 'output'!

wyswietlWierzcholki
    
    Transcript show: 'Wierzchołki ', self nazwa, ':'; cr.
    1 to: wierzcholki size do: [ :i |
        Transcript show: 'Wierzcholek ', i printString, ': '; show: (wierzcholki at: i) printString; cr.
    ].! "ten ! konczy dana funkcje" 

wyswietlPole

    Transcript show: 'Pole figury ', self nazwa, ':', self pole printString; cr.! 
    "magicznie znajduje przez dziedziczenie o ktore pole chodzi"

drukuj 
    self wyswietlPole.
    self wyswietlWierzcholki.!
! " a ten ! konczy blok output"

!Wielokat methodsFor: 'arithmetic'!
obroc: angle
    " Obraca wielokat o podany kat w stopniach wokół (0,0) "
    " shouldRound - określa czy współrzędne mają być zaokrąglone "
    (angle > 0 and: [angle <= 90])
        ifTrue: [
            wierzcholki := wierzcholki collect: [ :point |
                | radians cos sin newX newY |
                radians := angle negated degreesToRadians.
                cos := radians cos.
                sin := radians sin.
                "
                x' = x*cos(θ) - y*sin(θ)
                y' = x*sin(θ) + y*cos(θ)
                "
                newX := (point x * cos) - (point y * sin).
                newY := (point x * sin) + (point y * cos).
                
                Point x: newX y: newY
            ].
        ]
        ifFalse: [
            Transcript show: 'Kąt musi być między 0 a 90 stopni'; cr.
        ].
!

zaokraglij_wierzcholki 
    "zaokrągla współrzędne wierzchołków do 2 miejsc po przecinku"
    wierzcholki := wierzcholki collect: [ :point |
        Point x: (point x roundTo: 0.01) y: (point y roundTo: 0.01)
    ].
!
! "ten ! konczy blok arithmetic"




Wielokat subclass: #Kwadrat
	instanceVariableNames: 'bok'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'JezykiProgramowania'!


!Kwadrat methodsFor: 'arithmetic'!

+ figura
	"dodaj 2 figury w sensie pola"

	| p |

	p:=(self pole + figura pole).
	^(Kwadrat new) initialize: p sqrt! 
	
wysrodkuj
    | przesun |
    przesun := bok / 2 . "trzeba wszystkie wspolrzedne przesunac o -(bok/2)"
    1 to: wierzcholki size do: [ :i |
        | punkt |
        punkt := wierzcholki at: i.
        wierzcholki at: i put: ((punkt x - przesun) @ (punkt y - przesun)).
    ] !  !

!Kwadrat methodsFor: 'actions'!

pole
	"licz pole kwadratu"

	^(bok) squared! !
	
!Kwadrat methodsFor: 'initialize-release'!

initialize: dl
	"tworz kwadrat o podanym boku"

	super initialize: 4 name: 'Kwadrat'.
	bok := dl.
	wierzcholki at: 2 put: bok@0.
	wierzcholki at: 3 put: bok@bok.
	wierzcholki at: 4 put: 0@bok.! !
	
Wielokat subclass: #TrojkatRownoramienny
    instanceVariableNames: 'bok wysokosc'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'JezykiProgramowania'!
	
!TrojkatRownoramienny methodsFor: 'initialize-release'!

initialize: a wysokosc: h
    super initialize: 3 name:'TrojkatRownoramienny'.
    bok := a.
    wysokosc := h.
    wierzcholki at: 2 put: bok@0.
    wierzcholki at: 3 put: (bok/2)@ wysokosc.! !
	
!TrojkatRownoramienny methodsFor: 'arithmetic'!
+ figura
    |p2 nowy_bok nowa_wysokosc|
    p2 := self pole + figura pole.
    "musi być skala jak a do h"
    "a1h2=a2h1"
    "a2h2/2=P2"
    "z tego h2=sqrt(2*P2h1/a1)"
    "wiec a2=2*P2/h2"
    nowa_wysokosc := (2*p2*wysokosc/bok) sqrt.
    nowy_bok := 2*p2/nowa_wysokosc.
    ^(TrojkatRownoramienny new) initialize: nowy_bok wysokosc:nowa_wysokosc! 
    
wysrodkuj
    | przesunx ramie  wysokosc2 przesuny kawalek_ramienia|
    przesunx := bok / 2 . "trzeba wszystkie wspolrzedne x przesunac o -(bok/2), a na y skomplikowane"
    ramie := ((wysokosc squared) + ((bok/2) squared)) sqrt. "pitagoras do ramienia"
    "Transcript show: ramie printString;cr."
    wysokosc2 := (2* self pole / ramie). "ile wynosi druga wysokosc"
    "Transcript show: wysokosc2 printString;cr."
    kawalek_ramienia := (((bok) squared) - (wysokosc2) squared) sqrt. "podstawa sredniego trojkata"
    "Transcript show: kawalek_ramienia printString;cr."
    przesuny := ((kawalek_ramienia*bok)/(2*wysokosc2))."podobienstwo malego i sredniego trojkata"
    "Transcript show: przesuny printString;cr."
    1 to: wierzcholki size do: [ :i |
        | punkt |
        punkt := wierzcholki at: i.
        wierzcholki at: i put: ((punkt x - przesunx) @ (punkt y - przesuny)).
    ] !  !

!TrojkatRownoramienny methodsFor: 'actions'!
pole 
    ^(bok*wysokosc/2) ! !

"Wypisy"
Transcript show: '* Wypisane za pomocą komunikatu drukuj *'; cr.
Transcript show: '============================='; cr.
Transcript show: 'Zad 1 - Trójkąt równoramienny'; cr; cr.
trojkat1 := TrojkatRownoramienny new initialize: 6 wysokosc: 4.
trojkat1 drukuj.
trojkat2 := TrojkatRownoramienny new initialize: 6 wysokosc: 4.
Transcript show: '> Pole dwóch dodanych trójkątów:'; cr.
(trojkat1 + trojkat2) zaokraglij_wierzcholki drukuj.
Transcript show: '============================='; cr.
Transcript show: 'Zad 2 - Wyśrodkowanie'; cr; cr.
kwadrat1 := Kwadrat new initialize: 4.
kwadrat1 drukuj.
kwadrat1 wysrodkuj.
Transcript show: '> Po wyśrodkowaniu:' ;cr.
kwadrat1 drukuj.
Transcript show: '----------'; cr.
trojkat3 := TrojkatRownoramienny new initialize: 6 wysokosc: 4.
trojkat3 drukuj.
trojkat3 wysrodkuj.
Transcript show: '> Po wyśrodkowaniu:'; cr.
trojkat3 zaokraglij_wierzcholki drukuj.
Transcript show: '============================='; cr.
Transcript show: 'Zad 4 - Obracanie'; cr; cr.
kwadrat2 := Kwadrat new initialize: 4.
kwadrat2 wysrodkuj.
kwadrat2 drukuj.
kwadrat2 obroc: 45.
Transcript show: '> Po obrocie wyśrodkowanego kwadratu o 45 stopni:'; cr.
kwadrat2 zaokraglij_wierzcholki drukuj.
Transcript show: '----------'; cr.
trojkat4 := TrojkatRownoramienny new initialize: 6 wysokosc: 4.
trojkat4 drukuj.
trojkat4 obroc: 90.
trojkat4 obroc: 90.
Transcript show: '> Po obrocie o 180 (90*2) stopni:'; cr.
trojkat4 zaokraglij_wierzcholki drukuj.
trojkat4 obroc: 90.
trojkat4 obroc: 90.
Transcript show: '> Po obrocie o 360 (90*4) stopni:'; cr.
trojkat4 zaokraglij_wierzcholki drukuj.
Transcript show: '============================='; cr.