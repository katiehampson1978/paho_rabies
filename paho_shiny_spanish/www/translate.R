# Translate between Spanish/ Portuguese, replacing accented characters with normal characters

Sys.setlocale("LC_ALL","C") # First set system to recognize locale

language = function(accented){
  corrected = gsub('\213', "a", accented, ignore.case=TRUE) # a, ~ tilda e.g. SAo Paulo, MaranhAo
  corrected = gsub('\203', "E", corrected, ignore.case=TRUE) # E, acute e.g. MURCIELAGO
  corrected = gsub('\211', "a", corrected, ignore.case=TRUE) # a, hat ^ e.g. GoiAnia
  corrected = gsub('\231', "o", corrected, ignore.case=TRUE) # o, hat ^ e.g. RondOnia
  corrected = gsub('\227', "o", corrected, ignore.case=TRUE) # o, acute e.g. San CristOobal
  corrected = gsub('\234', "u", corrected, ignore.case=TRUE) # u, acute accent e.g. PerU
  corrected = gsub('\237', "u", corrected, ignore.case=TRUE) # u, umlout : e.g. General GUemes
  corrected = gsub('\222', "i", corrected, ignore.case=TRUE) # i, acute accent, e.g. HaitI, ParaIba
  corrected = gsub('\216', "e", corrected, ignore.case=TRUE) # e, e.g. MExico
  corrected = gsub('\226', "n", corrected, ignore.case=TRUE) # n, ~ tilda e.g. Elias PiNa, MonseNor Nouel
  corrected = gsub('\204', "N", corrected, ignore.case=TRUE) # N, ~ tilda CAPS, e.g. Nuflo de Chavez
  corrected = gsub('\215', "c", corrected, ignore.case=TRUE) # c, 5, e.g. PaCo do Lumiar, CasanCao

  corrected = gsub('\341', "a", corrected, ignore.case=TRUE) # a, acute, e.g. MichoacAn de Ocampo
  corrected = gsub('\351', "e", corrected, ignore.case=TRUE) # e, acute, e.g. MExico
  corrected = gsub('\363', "o", corrected, ignore.case=TRUE) # o, acute, e.g. Nuevo LeOn
  corrected = gsub('\355', "i", corrected, ignore.case=TRUE) # i, acute, e.g. San Luis PotosI

  gsub('\207', "a", corrected, ignore.case=TRUE) # a, acute accent e.g. CanadA, ParanA, GoiAs
}

#language(country$UnidMaior)



