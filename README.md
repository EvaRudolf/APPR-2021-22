# Analiza podatkov s programom R - 2021/22

Repozitorij za projekt pri predmetu APPR v študijskem letu 2021/22. 

## Analiza smuči in rezultatov alpskega smučanja

V projektu bom analizirala rezultate tekem klasičnih disciplin alpskega smučanja (slalom, veleslalom, superveleslalom, smuk) v sezoni 2020/2021 in pri tem poskušala ugotoviti, katere smuči so najboljše (glede na snežno podlago, temperaturo in disciplino). Zaradi velikega števila tekem in podobnosti bom analizirala le moške tekme v sezoni 2020/21.

Glavni vir je [podatkovna baza FIS-a](https://www.fis-ski.com/DB/alpine-skiing/calendar-results.html?eventselection=results&place=&sectorcode=AL&seasoncode=2021&categorycode=WC&disciplinecode=&gendercode=M&racedate=&racecodex=&nationcode=&seasonmonth=X-2021&saveselection=-1&seasonselection=). Ker so podatki tu zbrani v `pdf` datotekah, sem jih najprej s posebnim programom pretvorila v obliko `.csv`, nato pa uvozila v R.

Poleg podrobne analize smuči v sezoni 2020/21 bom analizirala tudi dobitnike velikih kristalnih globusov ter tako primerjala zmagovalce po državah. Te podatke sem našla na spletni strani [AlpineSkiDataBase](https://ski-db.com/db/stats/overall_m_gc.php) in so v obliki `.html`.

#### Tabele:
Manjše tabele s podatki o rezultatih in vremenu bob združila v eno veliko tabelo (`REZULTATI.VREME`), ki bo vsebovala naslednje stolpce:
* `Rank` -- uvrstitev na posamezni tekmi
* `Name` -- ime in priimek tekmovalca
* `YB` -- leto rojstva tekmovalca
* `NSA` -- kratica države (national ski association)
* `Ski` -- proizvajalec smuči, ki jih uporablja tekmovalec
* `venue` -- prizorišče tekme
* `Discciplina`
* `temperatura`
* `vreme`
* `sneg`
* `tocke_30` -- točke, ki jih prejme tekmovalce za uvrstitev med 30 na posamezni tekmi 

Tabela s podatki o smučarjih (`smucarji`), kjer je vsak smučar v tabeli naveden samo enkrat:
* `Name` -- ime in priimek tekmovalca
* `ski`-- proizvajalec smuči, ki jih uporablja tekmovalec
* `YB`-- leto rojstva tekmovalca
* `NSA`-- kratica države (national ski association)
* `tocke.DH` -- seštevek točk, doseženih v smuku
* `tocke.SG` -- seštevek točk, doseženih v superveleslalomu
* `tocke.GS` -- seštevek točk, doseženih v veleslalomu
* `tocke.SL` -- seštevek točk, doseženih v slalomu

Dve tabeli z zmagovalci po kristalnih globusih (ena za moške: `zmagovalci` in ena za ženske: `zmagovalke`). Obe imata stolpce:
* `Season` -- sezona oz. leto
* `Winner`-- ime in priimek zmagovalca/zmagovalke
* `NSA`-- kratica države (national ski association)


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
