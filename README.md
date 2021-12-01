# Analiza podatkov s programom R - 2021/22

Repozitorij za projekt pri predmetu APPR v študijskem letu 2021/22. 

## Analiza smuči in rezultatov alpskega smučanja

V projektu bom analizirala rezultate tekem klasičnih disciplin alpskega smučanja (slalom, veleslalom, superveleslalom, smuk) v sezoni 2020/2021 in pri tem poskušala ugotoviti, katere smuči so najboljše (glede na snežno podlago, temperaturo, disciplino, po državah - katere smuči največ uporabljajo v posamezni državi ...). Zaradi velikega števila tekem in podobnosti bom analizirala le moške tekme. 

Glavni vir je [podatkovna baza FIS-a](https://www.fis-ski.com/DB/alpine-skiing/calendar-results.html?eventselection=results&place=&sectorcode=AL&seasoncode=2021&categorycode=WC&disciplinecode=&gendercode=M&racedate=&racecodex=&nationcode=&seasonmonth=X-2021&saveselection=-1&seasonselection=). Ker so podatki tu zbrani v pdf datotekah, sem jih najprej s posebnim programom pretvorila v obliko `.csv`, nato pa uvozila v R.

Poleg podrobne analize smuči v sezoni 2020/21 bom analizirala tudi dobitnike velikih in malih kristalnih globusov ter tako primerjala zmagovalce po državah in smučeh. Te podatke sem našla na spletni strani [AlpineSkiDataBase](https://ski-db.com/db/stats/overall_m_gc.php) in bodo v obliki `.html`.

#### Tabele:
Tabele z rezultati posamezne tekme:
* `rank` - uvrstitev na posamezni tekmi
* `FIS_code` - FIS koda tekmovalca
* `ime` - ime in priimek tekmovalca
* `YB` - datum rojstva tekmovalca
* `NSA_code` - kratica države (SLO, SUI, AUT, ...)
* `time` - čas dosežen na posamezni tekmi (če gre za tehnično disciplino, je to skupen čas)
* `diff` - časovna razlika do 1. mesta
* `ski` - proizvajalec smuči, ki jih uporablja tekmovalec

Tabela s podatki o smučarjih:
* `FIS_code`
* `ime`
* `ski`
* `YB`
* `NSA_code`
* stolpci za rank vsake posamezne tekme v sezoni 2020/2021
* skunpni ranking po disciplini
* skupni ranking

Tabela s podatki o vremenu na vski posamezni tekmi:
* `T_1` - temperatura v prvem teku (povprečje temperature na startu in cilju)
* `T_2` - temperatura v drugem teku
* `vreme_1` - podatek o vremenu v prvem teku (sončno, oblačno, ...)
* `vreme_2` - podatek o vremenu v drugem teku
* `sneg_1` - struktura snega v prvem teku
* `sneg_2` - struktura snega v drugem teku

Tabela s smučmi - po disciplinah in glede na temperaturo in sneg (točke in FIS točke)

Tabela z zmagovalci po kristalnih globusih globusih:
* `leto`
* `SL` - dobitnik malega kristalnega globusa v slalomu
* `GS` - dobitnik malega kristalnega globusa v veleslalomu
* `SG` - dobitnik malega kristalnega globusa v superveleslalomu
* `DH` - dobitnik malega kristalnega globusa v smuku
* `overall` - zmagovalec skupnega seštevka; dobitnik velikega kristalnega globusa

Ta zadnja tabela bo sprva za potrebe analize zmagovalcev po državah vsebovala imena smučarjev, ki pa jim bom kasneje raje priredila smuči, da bom lahko tako analizirala še smuči skozi čas.

## Program

Glavni program in poročilo se nahajata v datoteki `projekt.Rmd`.
Ko ga prevedemo, se izvedejo programi, ki ustrezajo drugi, tretji in četrti fazi projekta:

* obdelava, uvoz in čiščenje podatkov: `uvoz/uvoz.r`
* analiza in vizualizacija podatkov: `vizualizacija/vizualizacija.r`
* napredna analiza podatkov: `analiza/analiza.r`

Vnaprej pripravljene funkcije se nahajajo v datotekah v mapi `lib/`.
Potrebne knjižnice so v datoteki `lib/libraries.r`
Podatkovni viri so v mapi `podatki/`.
Zemljevidi v obliki SHP, ki jih program pobere,
se shranijo v mapo `../zemljevidi/` (torej izven mape projekta).
