

numero_pagamenti=100

nodoChiediElencoFlussiRendicontazione=f'nodoChiediElencoFlussiRendicontazione-{numero_pagamenti}'

nodoChiediFlussoRendicontazione=f'''<?xml version="1.0" encoding="UTF-8" standalone="yes">
                                  <FlussoRiversamento xmlns="http://www.digitpa.gov.it/schemas/2011/Pagamenti/">
                                      <versioneOggetto>1.0</versioneOggetto>
                                      <identificativoFlusso>2021-07-27SELBIT2B-S003014897</identificativoFlusso>
                                      <dataOraFlusso>2021-07-27T12:00:07</dataOraFlusso>
                                      <identificativoUnivocoRegolamento>Bonifico SEPA-03268-A0EDT</identificativoUnivocoRegolamento>
                                      <dataRegolamento>2021-07-27</dataRegolamento>
                                      <istitutoMittente>
                                          <identificativoUnivocoMittente>
                                              <tipoIdentificativoUnivoco>B</tipoIdentificativoUnivoco>
                                              <codiceIdentificativoUnivoco>SELBIT2B</codiceIdentificativoUnivoco>
                                          </identificativoUnivocoMittente>
                                          <denominazioneMittente>Banca Sella</denominazioneMittente>
                                      </istitutoMittente>
                                      <istitutoRicevente>
                                          <identificativoUnivocoRicevente>
                                              <tipoIdentificativoUnivoco>G</tipoIdentificativoUnivoco>
                                              <codiceIdentificativoUnivoco>77777777777</codiceIdentificativoUnivoco>
                                          </identificativoUnivocoRicevente>
                                          <denominazioneRicevente>COMUNE DI MILANO</denominazioneRicevente>
                                      </istitutoRicevente>
                                      <numeroTotalePagamenti>{numero_pagamenti}</numeroTotalePagamenti>
                                      <importoTotalePagamenti>60.00</importoTotalePagamenti>
                                      <datiSingoliPagamenti>
                                          <identificativoUnivocoVersamento>02030267565002997</identificativoUnivocoVersamento>
                                          <identificativoUnivocoRiscossione>63e54a6b51694167ad30dafa0cb3a3c7</identificativoUnivocoRiscossione>
                                          <indiceDatiSingoloPagamento>2</indiceDatiSingoloPagamento>
                                          <singoloImportoPagato>20.00</singoloImportoPagato>
                                          <codiceEsitoSingoloPagamento>0</codiceEsitoSingoloPagamento>
                                          <dataEsitoSingoloPagamento>2021-07-26</dataEsitoSingoloPagamento>
                                      </datiSingoliPagamenti>
                                      <datiSingoliPagamenti>
                                          <identificativoUnivocoVersamento>02030267565002999</identificativoUnivocoVersamento>
                                          <identificativoUnivocoRiscossione>4613dac4329c4aa1b2b3a17af66ec4aa</identificativoUnivocoRiscossione>
                                          <indiceDatiSingoloPagamento>2</indiceDatiSingoloPagamento>
                                          <singoloImportoPagato>20.00</singoloImportoPagato>
                                          <codiceEsitoSingoloPagamento>0</codiceEsitoSingoloPagamento>
                                          <dataEsitoSingoloPagamento>2021-07-26</dataEsitoSingoloPagamento>
                                      </datiSingoliPagamenti>
                                      <datiSingoliPagamenti>
                                          <identificativoUnivocoVersamento>02030267565002998</identificativoUnivocoVersamento>
                                          <identificativoUnivocoRiscossione>a16f227ae78d4738bb7e62b210229106</identificativoUnivocoRiscossione>
                                          <indiceDatiSingoloPagamento>2</indiceDatiSingoloPagamento>
                                          <singoloImportoPagato>20.00</singoloImportoPagato>
                                          <codiceEsitoSingoloPagamento>0</codiceEsitoSingoloPagamento>
                                          <dataEsitoSingoloPagamento>2021-07-26</dataEsitoSingoloPagamento>
                                      </datiSingoliPagamenti>
                                  </FlussoRiversamento>'''