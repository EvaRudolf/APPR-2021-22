# Analiza podatkov s programom R - 2021/22

Repozitorij za projekt pri predmetu APPR v študijskem letu 2021/22. 

## Analiza smuči in rezultatov alpskega smučanja

V projektu bom analizirala rezultate tekem klasičnih disciplin alpskega smučanja (slalom, veleslalom, superveleslalom, smuk) v sezoni 2020/2021 in pri tem poskušala ugotoviti, katere smuči so najboljše. Zaradi velikega števila tekem in podobnosti bom analizirala le moške tekme. 
Smuči bom analizirala glede na odziv na snežno podlago, temperaturo, disciplino, po državah (katere smuči največ uporabljajo v posamezni državi) ...
Glavni vir je podatkovna baza FIS-a[https://www.fis-ski.com/DB/alpine-skiing/calendar-results.html?eventselection=results&place=&sectorcode=AL&seasoncode=2021&categorycode=WC&disciplinecode=&gendercode=M&racedate=&racecodex=&nationcode=&seasonmonth=X-2021&saveselection=-1&seasonselection=]. Ker so podatki tu zbrani v pdf datotekah, sem jih najprej s posebnim programom pretvorila v obliko '.csv', nato pa uvozila v R. 
Poleg podrobne analize smuči v sezoni 2020/21 bom analizirala tudi dobitnike velikih in malih kristalnih globusov ter tako primerjala zmagovalce po državah in smučeh. Te podatke sem našla na spletni strani AlpineSkiDataBase[https://ski-db.com/db/stats/overall_m_gc.php].
Tabele:
Tabele z rezultati posamezne tekme
* rank
* FIS_code
* ime
* datum_rojstva
* time
* diff
* smuči
Tabela s odatki o smučarjih
* FIS_code
* ime
* smuči
* datum_rojstva
* NSA_code
* stolpci za rank vsake posamezne tekme v sezoni 2020/2021
* skunpni ranking po disciplini
* skupni ranking
Tabela s podatki o vremenu na vski posamezni tekmi
* T_1
* T_2
* vreme_1
* vreme_2
* sneg_1
* sneg_2
Tabela s smučmi - po disciplinah in glede na temperaturo in sneg (točke in FIS točke)
Tabela z zmagovalci po globusih
* leto
* slalom
* veleslalom
* superveleslalom
* smuk
* skupno
Ta zadnja tabela bo sprva vsebovala imena smučarjev, nato pa jih bom imenom raje priredila smuči, da bom lahko analizirala še smuči skozi čas.

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
