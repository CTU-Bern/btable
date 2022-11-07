// the 'make.do' file is automatically created by 'github' package.
// execute the code below to generate the package installation files.
// DO NOT FORGET to update the version of the package, if changed!
// for more information visit http://github.com/haghish/github


*run test
cd "test"

do "00_masterfile.do"


*generate make file 
cd ".."

make btable, replace toc pkg version(1.1.0)                           ///
     license("Academic Free License v3.0")                                   ///
     author("Lukas Bütikofer")                                              ///
     affiliation("CTU Bern")                                                 ///
     email("lukas.buetikofer@ctu.unibe.ch")                                  ///
     url("https://github.com/CTU-Bern/btable")                               ///
     title("btable")                                                         ///
     description("Summary tables")                                           ///
     install("btable.ado;btable.sthlp;btable_format.ado;btable_format.sthlp;RMST.ado;RMST.sthlp;wmwTest.ado;wmwTest.sthlp;empty_line.ado;empty_line.sthlp") ///
     ancillary("")                                                         
