  *------------------------------------------------------------------------------*
* (TITLE TO BE DETERMINED)
* Appendix on Textual Classification
* Prepared by:
* Andre aassumpcao
* aassumpcao@unc.edu
*------------------------------------------------------------------------------*
set more off

*------------------------------------------------------------------------------*
* README:
*
* This third script produces the analysis in the Annex on service order classi-
* fication. We use a program co-author Assumpcao wrote for Stata to classify the
* service orders and produce quality measures of classification.
*
* Replication note: you need to install textfind, latab and outtable to run this
* do-file and produce the tex tables here.
*
*------------------------------------------------------------------------------*

********************************************************************************
******************** Replace for your own working directory ********************
********************************************************************************
global folder = "/Users/aassumpcao/OneDrive - University of North Carolina " ///
  + "at Chapel Hill/Documents/Research/2012 Discretion and Corruption/" ///
  + "discretioncorruption"

cd "$folder/"

********************************************************************************
********************************* textfind *************************************
********************************************************************************
use soData, clear

*** Define keywords that match SO to public purchases
local purchase = ///
  `""aquisi" "execu" "equipame" "ve[íi]culo" "despesa" "aplica[çc]" "'     + ///
  `""medicamento(.)*peaf" "compra" "recurso(.)*financ" "'                  + ///
  `""unidade(.)*m[óo]ve(.)*sa[úu]de" "pnate" "transporte(.)*escola" "'     + ///
  `""desenv(.)*ensino" "kit" "siafi" "implementa[çc]" "adquir" "'          + ///
  `""pme(.)*2004" "aparelhamento""'

*** Define keywords that match SO to public works
local works = ///
  `""co(ns|sn)tru" "obra" "implant" "infra(.)*estrut" "amplia" "'          + ///
  `""abasteci(.)*d(.)*[áa]gua" "reforma" "'                                + ///
  `""(melhoria|adequa)+(.)*(f[íi]sica|escolar|habitac|sanit[áa]ria)+" "'   + ///
  `""esgot" "adutora|dessaliniz|reservat[óo]" "sanit[áa]ri[ao]" "'         + ///
  `""poço" "aperfei[çc]oa" "saneamento" "res[íi]duo(.)*s[óo]lido" "'       + ///
  `""conclus[ãa]o""'

*** Run textfind for procurement both SO description
textfind soDescription, key(`purchase') nocase or tag(purchases) notable

*** Print matrix of results
mat2txt, mat(key) saving("$folder/article/tables/taba2")

*** Run textfind for public works both on transfer description
textfind transferDescription, key(`purchase') nocase or tag(tpurchases) notable

*** Print matrix of results
mat2txt, mat(key) saving("$folder/article/tables/taba3")

*** Run textfind for procurement both SO description
textfind soDescription, key(`works') but("psf") nocase or tag(works) notable

*** Print matrix of results
mat2txt, mat(key) saving("$folder/article/tables/taba4")

*** Run textfind for public works both on transfer description
textfind transferDescription, key(`works') but("psf") nocase or tag(tworks) ///
  notable

*** Print matrix of results
mat2txt, mat(key) saving("$folder/article/tables/taba5")

********************************************************************************
******************************** Quality tests *********************************
********************************************************************************
*** Code to produce tex tables below
la var purchases  "Purchases by SO description"
la var tpurchases "Purchases by Grant description"
la var works      "Works by SO description"
la var tworks     "Works by Grant description"
la de label       0 "No" 1 "Yes"
la val *purchases *works label

/** Test measures
gen pp1 = (purchases == 1 & tpurchases == 0 & works != 1 ///
  & transferDescription != ".")
gen pp2 = (purchases == 0 & tpurchases == 1 & works != 1 ///
  & transferDescription != ".")
gen wp1 = (works == 1 & tworks == 0 & transferDescription != ".")
gen wp2 = (works == 0 & tworks == 1 & transferDescription != ".")*/

*** Correction for missing values
replace tpurchases = . if transferDescription == "."
replace tworks     = . if transferDescription == "."

*** Save to file
save soData_tagged, replace

exit
