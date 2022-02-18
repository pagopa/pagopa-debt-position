

numero_pagamenti=100

nodoChiediElencoFlussiRendicontazione=f'''<?xml version="1.0" encoding="UTF-8" standalone="no"?>
                                            <soapenv:Envelope xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:soapenv="http://schemas.xmlsoap.org/soap/envelope/" xmlns:ppt="http://ws.pagamenti.telematici.gov/" xmlns:tns="http://NodoPagamentiSPC.spcoop.gov.it/servizi/PagamentiTelematiciRPT" xmlns:ppthead="http://ws.pagamenti.telematici.gov/ppthead">
                                                <soapenv:Body>
                                                    <ppt:nodoChiediElencoFlussiRendicontazioneRisposta>
                                                        <elencoFlussiRendicontazione>
                                                            <totRestituiti>2</totRestituiti>
                                                            <idRendicontazione>
                                                                <identificativoFlusso>2022-01-12PPAYITR1XXX-S003101841</identificativoFlusso>
                                                                <dataOraFlusso>2022-01-12T00:31:05</dataOraFlusso>
                                                            </idRendicontazione>
                                                            <idRendicontazione>
                                                                <identificativoFlusso>2022-01-20PPAYITR1XXX-S003037399</identificativoFlusso>
                                                                <dataOraFlusso>2022-01-20T00:30:39</dataOraFlusso>
                                                            </idRendicontazione>
                                                        </elencoFlussiRendicontazione>
                                                    </ppt:nodoChiediElencoFlussiRendicontazioneRisposta>
                                                </soapenv:Body>
                                            </soapenv:Envelope>'''

nodoChiediFlussoRendicontazione=f'''<?xml version="1.0" encoding="UTF-8" standalone="no"?>
                                    <soapenv:Envelope xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:soapenv="http://schemas.xmlsoap.org/soap/envelope/" xmlns:ppt="http://ws.pagamenti.telematici.gov/" xmlns:tns="http://NodoPagamentiSPC.spcoop.gov.it/servizi/PagamentiTelematiciRPT" xmlns:ppthead="http://ws.pagamenti.telematici.gov/ppthead">
                                        <soapenv:Body>
                                            <ppt:nodoChiediFlussoRendicontazioneRisposta>
                                                <xmlRendicontazione>PD94bWwgdmVyc2lvbj0iMS4wIiBlbmNvZGluZz0iVVRGLTgiIHN0YW5kYWxvbmU9InllcyI/Pgo8Rmx1c3NvUml2ZXJzYW1lbnRvIHhtbG5zPSJodHRwOi8vd3d3LmRpZ2l0cGEuZ292Lml0L3NjaGVtYXMvMjAxMS9QYWdhbWVudGkvIj4KICAgIDx2ZXJzaW9uZU9nZ2V0dG8+MS4wPC92ZXJzaW9uZU9nZ2V0dG8+CiAgICA8aWRlbnRpZmljYXRpdm9GbHVzc28+MjAyMi0wMi0wMVNFTEJJVDJCLVMwMDMwMzkxMzk8L2lkZW50aWZpY2F0aXZvRmx1c3NvPgogICAgPGRhdGFPcmFGbHVzc28+MjAyMi0wMi0wMVQwMDozMDo0NTwvZGF0YU9yYUZsdXNzbz4KICAgIDxpZGVudGlmaWNhdGl2b1VuaXZvY29SZWdvbGFtZW50bz5Cb25pZmljbyBTRVBBLTAzMjY4LTAxMTk5MjUwMTU4PC9pZGVudGlmaWNhdGl2b1VuaXZvY29SZWdvbGFtZW50bz4KICAgIDxkYXRhUmVnb2xhbWVudG8+MjAyMi0wMi0wMTwvZGF0YVJlZ29sYW1lbnRvPgogICAgPGlzdGl0dXRvTWl0dGVudGU+CiAgICAgICAgPGlkZW50aWZpY2F0aXZvVW5pdm9jb01pdHRlbnRlPgogICAgICAgICAgICA8dGlwb0lkZW50aWZpY2F0aXZvVW5pdm9jbz5CPC90aXBvSWRlbnRpZmljYXRpdm9Vbml2b2NvPgogICAgICAgICAgICA8Y29kaWNlSWRlbnRpZmljYXRpdm9Vbml2b2NvPlNFTEJJVDJCPC9jb2RpY2VJZGVudGlmaWNhdGl2b1VuaXZvY28+CiAgICAgICAgPC9pZGVudGlmaWNhdGl2b1VuaXZvY29NaXR0ZW50ZT4KICAgICAgICA8ZGVub21pbmF6aW9uZU1pdHRlbnRlPkJhbmNhIFNlbGxhPC9kZW5vbWluYXppb25lTWl0dGVudGU+CiAgICA8L2lzdGl0dXRvTWl0dGVudGU+CiAgICA8aXN0aXR1dG9SaWNldmVudGU+CiAgICAgICAgPGlkZW50aWZpY2F0aXZvVW5pdm9jb1JpY2V2ZW50ZT4KICAgICAgICAgICAgPHRpcG9JZGVudGlmaWNhdGl2b1VuaXZvY28+RzwvdGlwb0lkZW50aWZpY2F0aXZvVW5pdm9jbz4KICAgICAgICAgICAgPGNvZGljZUlkZW50aWZpY2F0aXZvVW5pdm9jbz4wMTE5OTI1MDE1ODwvY29kaWNlSWRlbnRpZmljYXRpdm9Vbml2b2NvPgogICAgICAgIDwvaWRlbnRpZmljYXRpdm9Vbml2b2NvUmljZXZlbnRlPgogICAgICAgIDxkZW5vbWluYXppb25lUmljZXZlbnRlPkNPTVVORSBESSBNSUxBTk88L2Rlbm9taW5hemlvbmVSaWNldmVudGU+CiAgICA8L2lzdGl0dXRvUmljZXZlbnRlPgogICAgPG51bWVyb1RvdGFsZVBhZ2FtZW50aT4xPC9udW1lcm9Ub3RhbGVQYWdhbWVudGk+CiAgICA8aW1wb3J0b1RvdGFsZVBhZ2FtZW50aT4zMi4wNTwvaW1wb3J0b1RvdGFsZVBhZ2FtZW50aT4KICAgIDxkYXRpU2luZ29saVBhZ2FtZW50aT4KICAgICAgICA8aWRlbnRpZmljYXRpdm9Vbml2b2NvVmVyc2FtZW50bz43MjEwODgyMzk5NjEyMTU8L2lkZW50aWZpY2F0aXZvVW5pdm9jb1ZlcnNhbWVudG8+CiAgICAgICAgPGlkZW50aWZpY2F0aXZvVW5pdm9jb1Jpc2Nvc3Npb25lPjRkNWUxNTVlYWE5MjRmYmRiZjI4YjIxYjJjNmVkYzgzPC9pZGVudGlmaWNhdGl2b1VuaXZvY29SaXNjb3NzaW9uZT4KICAgICAgICA8aW5kaWNlRGF0aVNpbmdvbG9QYWdhbWVudG8+MTwvaW5kaWNlRGF0aVNpbmdvbG9QYWdhbWVudG8+CiAgICAgICAgPHNpbmdvbG9JbXBvcnRvUGFnYXRvPjMyLjA1PC9zaW5nb2xvSW1wb3J0b1BhZ2F0bz4KICAgICAgICA8Y29kaWNlRXNpdG9TaW5nb2xvUGFnYW1lbnRvPjA8L2NvZGljZUVzaXRvU2luZ29sb1BhZ2FtZW50bz4KICAgICAgICA8ZGF0YUVzaXRvU2luZ29sb1BhZ2FtZW50bz4yMDIyLTAxLTMxPC9kYXRhRXNpdG9TaW5nb2xvUGFnYW1lbnRvPgogICAgPC9kYXRpU2luZ29saVBhZ2FtZW50aT4KPC9GbHVzc29SaXZlcnNhbWVudG8+Cg==</xmlRendicontazione>
                                            </ppt:nodoChiediFlussoRendicontazioneRisposta>
                                        </soapenv:Body>
                                    </soapenv:Envelope>'''